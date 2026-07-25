#!/usr/bin/env python3
"""Run OpenSTA for synthesized clash-hash modules."""

from __future__ import annotations

import argparse
import getpass
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
BUILD_DIR = PROJECT_ROOT / "build"
CLASH_TARGETS_FILE = PROJECT_ROOT / "clash.json"
VHDL_TARGETS_FILE = PROJECT_ROOT / "vhdl.json"

DOCKER_NAMESPACE = "clash-hash"
DOCKER_PROJECT = "sta"
DOCKER_TAG = "latest"
DOCKER_WORK_ROOT = "/workspace/clash-hash"
DOCKER_IMAGE = f"{DOCKER_NAMESPACE}/{DOCKER_PROJECT}:{DOCKER_TAG}"
STA_CLOCK_PERIOD_NS = 5.0


class StaError(RuntimeError):
    """Raised when STA preparation or execution fails."""


def _print(msg: str) -> None:
    """Print a message flushed immediately."""
    print(msg, flush=True)


def _process_library(process: str) -> Path:
    """Return the Liberty path for the given process, ensuring it exists."""
    if process == "nangate45":
        lib_path = PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"
    else:
        raise StaError(f"Unsupported process '{process}'. Supported: nangate45")

    if not lib_path.exists():
        raise StaError(
            f"Library file not found: {lib_path}. "
            "Run 'cd lib/nangate45 && ./download.sh' to install the library."
        )

    _print(f"Using {process} library: {lib_path}")
    return lib_path


def _validate_environment() -> None:
    """Ensure required tools are available before running STA."""
    yosys = shutil.which("yosys")
    if yosys is None:
        raise StaError("yosys not found. Run inside the 'nix develop' environment or install yosys.")

    version = subprocess.run([yosys, "--version"], text=True, capture_output=True, check=False)
    first_line = version.stdout.splitlines()[0] if version.stdout else "yosys"
    _print(f"✓ Yosys found: {first_line}")

    if shutil.which("sta") is None:
        raise StaError("OpenSTA binary 'sta' not found. Ensure it is installed in the environment.")


def load_simple_aliases(path: Path, required: bool = False) -> dict[str, str]:
    """Load simple target aliases from JSON file."""
    if not path.is_file():
        if required:
            sys.exit(f"error: targets file missing at {path}")
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"error: could not parse {path}: {exc}")
    if not isinstance(data, dict):
        sys.exit(f"error: targets file {path} must contain a JSON object")
    return {str(k): str(v) for k, v in data.items()}


def load_vhdl_targets(path: Path) -> dict[str, dict]:
    """Load VHDL targets from JSON file."""
    if not path.is_file():
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"error: could not parse {path}: {exc}")
    if not isinstance(data, dict):
        sys.exit(f"error: vhdl targets file {path} must contain a JSON object")
    targets: dict[str, dict] = {}
    for name, entry in data.items():
        if not isinstance(entry, dict):
            sys.exit(f"error: vhdl target '{name}' must be an object")
        top = entry.get("top")
        if not isinstance(top, str) or not top:
            sys.exit(f"error: vhdl target '{name}' missing 'top' string")
        targets[str(name)] = {
            "top": top,
            "dir": entry.get("dir"),
        }
    return targets


def _resolve_netlist_path(target: str) -> tuple[Path, str, Path | None]:
    """Resolve target alias and return (netlist_path, top_module_name, clash_sdc_path)."""
    # Check VHDL targets first
    vhdl_targets = load_vhdl_targets(VHDL_TARGETS_FILE)
    if target in vhdl_targets:
        entry = vhdl_targets[target]
        top = entry["top"]
        dir_name = entry.get("dir") or target
        label_name = f"vhdl_{dir_name}"
        netlist = BUILD_DIR / "synth" / label_name / "netlist" / f"{top}.mapped.v"
        return (netlist, top, None)

    # Check Clash targets (with alias support)
    clash_aliases = load_simple_aliases(CLASH_TARGETS_FILE, required=False)
    label = clash_aliases.get(target, target)

    # Try to find the manifest to get the top module name
    try:
        from pathlib import Path as PathType

        clash_roots = [PROJECT_ROOT / "systemverilog", PROJECT_ROOT / "verilog"]
        manifest_path = None
        manifest_root = None
        for root in clash_roots:
            candidate = root / label / "clash-manifest.json"
            if candidate.is_file():
                manifest_path = candidate
                manifest_root = root
                break
        if manifest_path is not None and manifest_root is not None:
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            top = manifest.get("top_component", {}).get("name")
            if top:
                netlist = BUILD_DIR / "synth" / label / "netlist" / f"{top}.mapped.v"
                # Check for Clash-generated SDC file
                clash_sdc = manifest_root / label / f"{top}.sdc"
                if clash_sdc.exists():
                    return (netlist, top, clash_sdc)
                return (netlist, top, None)
    except Exception:
        pass

    # Fallback: assume target IS the top module name
    netlist = BUILD_DIR / "synth" / target / "netlist" / f"{target}.mapped.v"
    return (netlist, target, None)


def _resolve_output_label(target: str) -> str:
    """Return the stable bench label for the target."""
    vhdl_targets = load_vhdl_targets(VHDL_TARGETS_FILE)
    if target in vhdl_targets:
        dir_name = vhdl_targets[target].get("dir") or target
        return f"vhdl_{dir_name}"
    clash_aliases = load_simple_aliases(CLASH_TARGETS_FILE, required=False)
    return clash_aliases.get(target, target)


def _clock_period_text() -> str:
    return f"{STA_CLOCK_PERIOD_NS:.3f}"


def _normalise_sdc_clock_period(text: str) -> str:
    period = _clock_period_text()
    half_period = f"{STA_CLOCK_PERIOD_NS / 2:.3f}"
    text = re.sub(r"-period\s+[0-9.]+", f"-period {period}", text)
    text = re.sub(r"-waveform\s+\{[0-9.]+\s+[0-9.]+\}", f"-waveform {{0.000 {half_period}}}", text)
    return text


def _auto_synthesize(target: str) -> bool:
    """Attempt to auto-synthesize the target if netlist is missing."""
    _print(f"Netlist not found, attempting to synthesize '{target}' automatically...")
    synth_script = SCRIPT_DIR / "synth.py"
    if not synth_script.exists():
        return False

    result = subprocess.run(
        [sys.executable, str(synth_script), target],
        cwd=PROJECT_ROOT,
    )
    return result.returncode == 0


def _setup_paths(process: str, target: str) -> dict[str, Path]:
    """Compute key paths for STA execution and create required directories."""
    # Resolve the netlist path from target (handles aliases and different formats)
    netlist, top_module, clash_sdc = _resolve_netlist_path(target)

    if not netlist.exists():
        # Try auto-synthesis
        if not _auto_synthesize(target):
            raise StaError(
                f"Netlist file not found: {netlist}. "
                f"Run 'nix run .#synth -- {target}' first."
            )
        # Re-resolve after synthesis to pick up SDC file
        netlist, top_module, clash_sdc = _resolve_netlist_path(target)
        if not netlist.exists():
            raise StaError(f"Auto-synthesis failed for target '{target}'")

    # Use Clash-generated SDC if available, otherwise create a default one
    sdc = BUILD_DIR / "sta" / f"{top_module}.sdc"
    sdc.parent.mkdir(parents=True, exist_ok=True)

    # Always regenerate SDC to avoid stale constraints
    if clash_sdc and clash_sdc.exists():
        # Use Clash-generated SDC file
        _print(f"Using Clash-generated SDC: {clash_sdc}")
        sdc.write_text(_normalise_sdc_clock_period(clash_sdc.read_text(encoding="utf-8")), encoding="utf-8")
    else:
        # Create a basic SDC file
        # Use CLK for Clash (uppercase) or clk for VHDL (lowercase)
        clock_name = "CLK" if clash_sdc is not None or "vhdl" not in str(netlist) else "clk"
        sdc.write_text(
            f"# Auto-generated SDC for clash-hash STA\n"
            f"# Clock: {clock_name}\n"
            f"create_clock -name {clock_name} -period {_clock_period_text()} [get_ports {clock_name}]\n"
            f"set_input_delay -clock {clock_name} 0.0 [all_inputs]\n"
            f"set_output_delay -clock {clock_name} 0.0 [all_outputs]\n"
        )
        _print(f"Generated SDC file: {sdc} (clock={clock_name})")

    output_dir = BUILD_DIR / "sta" / _resolve_output_label(target) / top_module
    (output_dir / "reports" / "timing").mkdir(parents=True, exist_ok=True)
    (output_dir / "logs").mkdir(parents=True, exist_ok=True)

    return {
        "netlist": netlist,
        "sdc": sdc,
        "output": output_dir,
        "top_module": top_module,  # Store resolved top module name
    }


def _show_configuration(process: str, module: str, lib_path: Path, paths: dict[str, Path]) -> None:
    """Print a summary of the STA run configuration."""
    _print("clash-hash Static Timing Analysis Configuration:")
    _print(f"  Process: {process}")
    _print(f"  Liberty File: {lib_path}")
    _print(f"  Netlist File: {paths['netlist']}")
    _print(f"  SDC File: {paths['sdc']}")
    _print(f"  Top Module: {module}")
    _print(f"  Output Dir: {paths['output']}")
    _print("")


def _ensure_docker_image() -> bool:
    """Ensure the STA Docker image exists, building it if necessary."""
    if shutil.which("docker") is None:
        return False

    # Check if image exists
    result = subprocess.run(
        ["docker", "image", "inspect", DOCKER_IMAGE],
        capture_output=True,
        text=True,
    )
    if result.returncode == 0:
        return True

    # Build the image
    docker_dir = PROJECT_ROOT / "docker"
    if not docker_dir.exists():
        _print("Docker directory not found, cannot build image")
        return False

    _print(f"Building Docker image {DOCKER_IMAGE}...")
    _print("This may take several minutes on first run...")
    result = subprocess.run(
        ["docker", "build", "--progress=plain", "-t", DOCKER_IMAGE, "."],
        cwd=docker_dir,
    )
    return result.returncode == 0


def _sta_container_name() -> str:
    """Compute the STA container name."""
    return f"clash-hash-sta_{getpass.getuser()}"


def _ensure_docker_container() -> bool:
    """Ensure the STA container exists; create it if missing."""
    container = _sta_container_name()

    # Check if container exists
    result = subprocess.run(
        ["docker", "container", "inspect", container],
        capture_output=True,
        text=True,
    )
    if result.returncode == 0:
        return True

    # Create container
    host_root = PROJECT_ROOT.resolve()
    result = subprocess.run(
        [
            "docker",
            "container",
            "run",
            "-itd",
            "--name",
            container,
            "--volume",
            f"{host_root}:{DOCKER_WORK_ROOT}",
            "--workdir",
            DOCKER_WORK_ROOT,
            DOCKER_IMAGE,
            "/bin/bash",
        ],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def _start_docker_container() -> bool:
    """Start the STA container (safe if already running)."""
    container = _sta_container_name()
    result = subprocess.run(
        ["docker", "start", container],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def _run_sta_in_container(target: str, process: str) -> int:
    """Execute the STA script inside the Docker container."""
    container = _sta_container_name()
    inner = f"cd {DOCKER_WORK_ROOT} && python3 ./scripts/sta.py --no-docker {target} --process {process}"
    result = subprocess.run(
        ["docker", "exec", container, "bash", "-lc", inner],
        cwd=PROJECT_ROOT,
    )
    return result.returncode


def _run_sta(process: str, lib_path: Path, paths: dict[str, Path]) -> int:
    """Run OpenSTA and capture its output to the STA log."""
    top_module = paths["top_module"]
    env = os.environ.copy()
    env.update(
        {
            "STA_LIBERTY_FILE": str(lib_path),
            "STA_NETLIST_FILE": str(paths["netlist"]),
            "STA_SDC_FILE": str(paths["sdc"]),
            "STA_OUTPUT_DIR_FULL": str(paths["output"]),
            "STA_TOP_MODULE": top_module,
        }
    )

    log_path = paths["output"] / "logs" / "sta.log"
    tcl_script = SCRIPT_DIR / "tcl" / "sta_run_reports.tcl"

    if not tcl_script.exists():
        raise StaError(f"TCL script not found: {tcl_script}")

    _print("Running OpenSTA...")

    with log_path.open("w", encoding="utf-8") as log_file:
        process_obj = subprocess.Popen(
            ["sta", "-no_init", "-exit", str(tcl_script)],
            cwd=str(PROJECT_ROOT),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        assert process_obj.stdout is not None
        for line in process_obj.stdout:
            sys.stdout.write(line)
            log_file.write(line)
        return_code = process_obj.wait()

    if return_code != 0:
        raise StaError(f"OpenSTA exited with status {return_code}. See log: {log_path}")

    _print("✓ Static Timing Analysis completed successfully!")
    _print(f"Reports generated in: {paths['output'] / 'reports'}")
    _print(f"Log: {log_path}")

    # Print summary metrics if available
    summary_file = paths["output"] / "reports" / "summary.rpt"
    if summary_file.exists():
        _print("")
        _print("Timing Summary:")
        _print("=" * 50)
        summary_content = summary_file.read_text()
        for line in summary_content.strip().split('\n'):
            _print(f"  {line}")
        _print("=" * 50)

    return return_code


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Run OpenSTA for a synthesized module. "
        "Target can be an alias from clash.json or vhdl.json, or a module name."
    )
    parser.add_argument(
        "target",
        help="Alias in clash.json or vhdl.json, or module name",
    )
    parser.add_argument(
        "--process",
        default="nangate45",
        help="Technology process (default: nangate45)",
    )
    parser.add_argument(
        "--no-docker",
        action="store_true",
        help="Run OpenSTA directly on host (requires OpenSTA installed)",
    )
    args = parser.parse_args(argv)

    # If not running in Docker mode and OpenSTA is not available, try Docker
    if not args.no_docker:
        if shutil.which("docker") is None:
            _print("Docker not found. Attempting to run OpenSTA directly on host...")
            _print("Note: This requires OpenSTA to be installed manually.")
            args.no_docker = True
        else:
            # Use Docker mode
            _print("clash-hash Static Timing Analysis (Docker mode)")
            _print(f"  Target: {args.target}")
            _print(f"  Process: {args.process}")

            # Ensure Docker infrastructure is ready
            if not _ensure_docker_image():
                print("✗ Failed to prepare Docker image")
                return 1

            if not _ensure_docker_container():
                print("✗ Failed to prepare Docker container")
                return 1

            if not _start_docker_container():
                print("✗ Failed to start Docker container")
                return 1

            # Run STA inside container
            return _run_sta_in_container(args.target, args.process)

    # Direct execution mode (inside Docker or on host with OpenSTA)
    try:
        _print("clash-hash Static Timing Analysis Starting...")
        _print(f"  Process: {args.process}")
        _print(f"  Target: {args.target}")

        lib_path = _process_library(args.process)
        _validate_environment()
        paths = _setup_paths(args.process, args.target)
        top_module = paths["top_module"]
        _print(f"  Resolved top module: {top_module}")
        _show_configuration(args.process, top_module, lib_path, paths)
        return _run_sta(args.process, lib_path, paths)
    except StaError as exc:
        print(f"✗ {exc}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
