#!/usr/bin/env python3
"""
Minimal Yosys wrapper that synthesizes either:
  * Clash-generated Verilog targets (via clash-manifest.json), or
  * Plain VHDL designs described in vhdl.json and stored under ./vhdl

Verilog usage (existing behaviour):
    python3 scripts/synth.py Hash.Stateful4.topEntity

VHDL usage (new):
    python3 scripts/synth.py <alias-in-vhdl.json>
"""

from __future__ import annotations

import argparse
import json
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
VERILOG_ROOT = PROJECT_ROOT / "verilog"
VHDL_ROOT = PROJECT_ROOT / "vhdl"
CLASH_TARGETS_FILE = PROJECT_ROOT / "clash.json"
VHDL_TARGETS_FILE = PROJECT_ROOT / "vhdl.json"
GHDL_WORK_BASE = PROJECT_ROOT / "build" / "ghdl"
SV_BASE = PROJECT_ROOT / "build" / "sv"
EXTERNAL_VHDL_REPO = PROJECT_ROOT.parent / "keccak-vhdl"
DEFAULT_OUTPUT_ROOT = PROJECT_ROOT / "build" / "synth"
DEFAULT_LIBERTY = PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"


def load_simple_aliases(path: Path, required: bool = False) -> dict[str, str]:
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
        files = entry.get("files")
        if not isinstance(top, str) or not top:
            sys.exit(f"error: vhdl target '{name}' missing 'top' string")
        if not isinstance(files, list) or not files:
            sys.exit(f"error: vhdl target '{name}' requires non-empty 'files' list")
        if not all(isinstance(f, str) for f in files):
            sys.exit(f"error: vhdl target '{name}' has non-string 'files' entries")
        targets[str(name)] = {
            "top": top,
            "files": files,
            "dir": entry.get("dir"),
        }
    return targets


def _auto_generate_verilog(module_name: str) -> bool:
    """Attempt to auto-generate Verilog from Clash source."""
    print(f"[synth] Verilog not found, generating from Clash: {module_name}", flush=True)

    # Convert module name to file path: Hash.NonPipelined.topEntity -> src/Hash/NonPipelined.hs
    parts = module_name.split(".")
    if parts[-1] == "topEntity":
        parts = parts[:-1]  # Remove topEntity suffix
    file_path = PROJECT_ROOT / "src" / "/".join(parts[:-1]) / f"{parts[-1]}.hs"

    if not file_path.exists():
        print(f"[synth] Source file not found: {file_path}", flush=True)
        return False

    # Use: stack exec clash -- --verilog <file.hs>
    result = subprocess.run(
        ["stack", "exec", "clash", "--", "--verilog", str(file_path)],
        cwd=PROJECT_ROOT,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        print(f"[synth] Failed to generate Verilog:", flush=True)
        print(result.stdout, flush=True)
        print(result.stderr, flush=True)
        return False

    print(f"[synth] ✓ Verilog generated successfully", flush=True)
    return True


def load_manifest(arg: str) -> tuple[Path, dict]:
    manifest_path = VERILOG_ROOT / arg / "clash-manifest.json"

    # Auto-generate if missing
    if not manifest_path.is_file():
        # Try to resolve alias first
        clash_aliases = load_simple_aliases(CLASH_TARGETS_FILE, required=False)
        module_name = clash_aliases.get(arg, arg)

        if not _auto_generate_verilog(module_name):
            sys.exit(f"error: could not generate Verilog for {arg}")

        # Check again after generation
        if not manifest_path.is_file():
            sys.exit(f"error: manifest not found at {manifest_path} even after generation")

    try:
        data = json.loads(manifest_path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"error: failed to read manifest {manifest_path}: {exc}")
    return manifest_path, data


def collect_verilog_files(manifest_path: Path, manifest: dict) -> list[Path]:
    files: list[Path] = []
    for entry in manifest.get("files", []):
        name = entry.get("name")
        if isinstance(name, str) and name.endswith(".v"):
            files.append((manifest_path.parent / name).resolve())
    if not files:
        sys.exit("error: manifest lists no .v files")
    return files


def collect_vhdl_files(base_dir: Path, file_list: list[str]) -> list[Path]:
    files: list[Path] = []
    for fname in file_list:
        path = (base_dir / fname).resolve()
        if not path.is_file():
            sys.exit(f"error: VHDL file not found: {path}")
        files.append(path)
    if not files:
        sys.exit("error: no VHDL files specified")
    return files


def yosys_with_ghdl(script: str) -> int:
    yosys_cmd = ["yosys", "-m", "ghdl", "-p", script]
    result = subprocess.run(yosys_cmd, cwd=PROJECT_ROOT, capture_output=True, text=True, check=False)
    if result.returncode == 0:
        return 0
    if "Can't load module `./ghdl'" not in result.stderr and "Can't load module `ghdl'" not in result.stderr:
        sys.stderr.write(result.stdout + result.stderr)
        return result.returncode

    if not EXTERNAL_VHDL_REPO.is_dir():
        sys.stderr.write(result.stdout + result.stderr)
        return result.returncode

    external_cmd = [
        "nix",
        "develop",
        str(EXTERNAL_VHDL_REPO),
        "--command",
        "yosys",
        "-m",
        "ghdl",
        "-p",
        script,
    ]
    result = subprocess.run(external_cmd, cwd=PROJECT_ROOT, capture_output=True, text=True, check=False)
    if result.returncode != 0:
        sys.stderr.write(result.stdout + result.stderr)
    return result.returncode


def convert_vhdl_to_sv(name: str, top: str, vhdl_files: list[Path]) -> Path:
    work_dir = (GHDL_WORK_BASE / name).resolve()
    out_dir = (SV_BASE / name).resolve()
    work_dir.mkdir(parents=True, exist_ok=True)
    out_dir.mkdir(parents=True, exist_ok=True)

    out_file = out_dir / f"{top}.sv"
    file_args = [str(p) for p in vhdl_files]

    ghdl_parts = [
        "ghdl",
        "--std=93c",
        "--ieee=synopsys",
        f"--workdir={work_dir}",
        *file_args,
        "-e",
        top,
    ]
    ghdl_cmd = " ".join(shlex.quote(part) for part in ghdl_parts)
    out_file_q = shlex.quote(str(out_file))
    top_q = shlex.quote(top)

    yosys_script = "; ".join(
        [
            ghdl_cmd,
            f"hierarchy -check -top {top_q}",
            "proc; opt; memory; opt",
            f"write_verilog -sv {out_file_q}",
        ]
    )

    print(f"[synth] Converting {name} VHDL → SystemVerilog...", flush=True)
    rc = yosys_with_ghdl(yosys_script)
    if rc != 0 or not out_file.exists():
        sys.exit(f"error: failed to convert VHDL for {name}")

    return out_file


def build_yosys_commands(verilog_files: list[Path], top: str, netlist_path: Path, liberty: Path) -> list[str]:
    def read_step(path: Path) -> str:
        quoted = shlex.quote(str(path))
        if path.suffix.lower() == ".sv":
            return f"read_verilog -sv {quoted}"
        return f"read_verilog {quoted}"

    read_cmds = [read_step(p) for p in verilog_files]
    liberty_q = shlex.quote(str(liberty))
    netlist_q = shlex.quote(str(netlist_path))
    top_q = shlex.quote(top)

    commands = [
        *read_cmds,
        f"hierarchy -check -top {top_q}",
        "proc",
        "opt",
        "techmap",
        "opt",
        f"dfflibmap -liberty {liberty_q}",
        f"abc -liberty {liberty_q}",
        "clean",
        f"write_verilog -noattr {netlist_q}",
        f"stat -top {top_q} -liberty {liberty_q}",
    ]
    return commands


def run_yosys(commands: list[str]) -> subprocess.CompletedProcess[str]:
    script = "; ".join(commands)
    return subprocess.run(
        ["yosys", "-p", script],
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        check=False,
    )


def summarise(output: str) -> None:
    cell_line = None
    seq_line = None
    area_line = None
    cpu_line = None
    for raw in output.splitlines():
        line = raw.strip()
        if line.startswith("Number of cells:"):
            cell_line = line
        if "sequential elements" in line:
            seq_line = line
        if line.startswith("Chip area"):
            area_line = line
        if line.startswith("CPU: user"):
            cpu_line = line
    if cell_line:
        print(f"  {cell_line}")
    if seq_line:
        print(f"  {seq_line}")
    if area_line:
        print(f"  {area_line}")
    if cpu_line:
        print(f"  {cpu_line}")


def ensure_liberty() -> None:
    if not DEFAULT_LIBERTY.is_file():
        sys.exit(f"error: liberty file not found at {DEFAULT_LIBERTY}")


def run_clash_target(label: str) -> None:
    ensure_liberty()
    manifest_path, manifest = load_manifest(label)
    top = manifest.get("top_component", {}).get("name")
    if not top:
        sys.exit("error: manifest missing top_component.name")

    verilog_files = collect_verilog_files(manifest_path, manifest)

    label_name = manifest_path.parent.name
    out_root = DEFAULT_OUTPUT_ROOT / label_name
    netlist_dir = out_root / "netlist"
    report_dir = out_root / "reports"
    netlist_dir.mkdir(parents=True, exist_ok=True)
    report_dir.mkdir(parents=True, exist_ok=True)

    netlist_path = netlist_dir / f"{top}.mapped.v"
    report_path = report_dir / "yosys.log"

    commands = build_yosys_commands(verilog_files, top, netlist_path, DEFAULT_LIBERTY)

    print(f"[synth] {label_name} → top={top}")
    result = run_yosys(commands)
    output = f"{result.stdout}{result.stderr}"
    report_path.write_text(output, encoding="utf-8")

    if result.returncode != 0:
        sys.exit(f"error: yosys exited with code {result.returncode}\n{output}")

    print(f"  ✓ netlist: {netlist_path.relative_to(PROJECT_ROOT)}")
    print(f"  ↳ report : {report_path.relative_to(PROJECT_ROOT)}")
    summarise(output)


def run_vhdl_target(name: str, entry: dict) -> None:
    ensure_liberty()
    dir_name = entry.get("dir") or name
    base_dir = (VHDL_ROOT / dir_name).resolve()
    if not base_dir.is_dir():
        sys.exit(f"error: VHDL directory not found: {base_dir}")

    vhdl_files = collect_vhdl_files(base_dir, entry["files"])
    top = entry["top"]

    sv_file = convert_vhdl_to_sv(name, top, vhdl_files)

    label_name = f"vhdl_{dir_name}"
    out_root = DEFAULT_OUTPUT_ROOT / label_name
    netlist_dir = out_root / "netlist"
    report_dir = out_root / "reports"
    netlist_dir.mkdir(parents=True, exist_ok=True)
    report_dir.mkdir(parents=True, exist_ok=True)

    netlist_path = netlist_dir / f"{top}.mapped.v"
    report_path = report_dir / "yosys.log"

    commands = build_yosys_commands([sv_file], top, netlist_path, DEFAULT_LIBERTY)

    print(f"[synth] {name} (VHDL) → top={top}")
    result = run_yosys(commands)
    output = f"{result.stdout}{result.stderr}"
    report_path.write_text(output, encoding="utf-8")

    if result.returncode != 0:
        sys.exit(f"error: yosys exited with code {result.returncode}\n{output}")

    print(f"  ✓ netlist: {netlist_path.relative_to(PROJECT_ROOT)}")
    print(f"  ↳ report : {report_path.relative_to(PROJECT_ROOT)}")
    summarise(output)


def main(argv: list[str]) -> None:
    parser = argparse.ArgumentParser(description="Synthesize a Clash (Verilog) or VHDL target with Yosys.")
    parser.add_argument(
        "target",
        help="Alias in clash.json or vhdl.json, or raw verilog directory name under ./verilog",
    )
    args = parser.parse_args(argv)

    clash_aliases = load_simple_aliases(CLASH_TARGETS_FILE, required=True)
    vhdl_targets = load_vhdl_targets(VHDL_TARGETS_FILE)

    target = args.target
    if target in vhdl_targets:
        run_vhdl_target(target, vhdl_targets[target])
        return

    label = clash_aliases.get(target, target)
    run_clash_target(label)


if __name__ == "__main__":
    main(sys.argv[1:])
