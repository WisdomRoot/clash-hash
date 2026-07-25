#!/usr/bin/env python3
"""
Minimal Yosys wrapper that synthesizes either:
  * Clash-generated SystemVerilog/Verilog targets (via clash-manifest.json), or
  * Plain VHDL designs described in vhdl.json and stored under ./vhdl

SystemVerilog usage (existing behaviour; also emits Verilog):
    python3 scripts/synth.py Hash.Stateful4.topEntity

VHDL usage (new):
    python3 scripts/synth.py <alias-in-vhdl.json>
"""

from __future__ import annotations

import argparse
import json
import re
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SYSTEMVERILOG_ROOT = PROJECT_ROOT / "systemverilog"
VERILOG_ROOT = PROJECT_ROOT / "verilog"
CLASH_HDL_ROOTS = [SYSTEMVERILOG_ROOT, VERILOG_ROOT]
VHDL_ROOT = PROJECT_ROOT / "vhdl"
CLASH_TARGETS_FILE = PROJECT_ROOT / "clash.json"
VHDL_TARGETS_FILE = PROJECT_ROOT / "vhdl.json"
GHDL_WORK_BASE = PROJECT_ROOT / "build" / "ghdl"
SV_BASE = PROJECT_ROOT / "build" / "sv"
EXTERNAL_VHDL_REPO = PROJECT_ROOT.parent / "keccak-vhdl"
DEFAULT_OUTPUT_ROOT = PROJECT_ROOT / "build" / "synth"
DEFAULT_LIBERTY = PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"
ABC_CONSTRAINTS = DEFAULT_OUTPUT_ROOT / "abc.constr"
ABC_DELAY_PS = 5_000


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


def _parse_clash_target(label: str) -> tuple[str, str | None]:
    suffix = ".topEntity"
    if label.endswith(suffix):
        return label[: -len(suffix)], None
    parts = label.split(".")
    if parts and parts[-1] and parts[-1][0].islower():
        return ".".join(parts[:-1]), label
    return label, None


def _module_source_path(module_name: str) -> Path | None:
    source = PROJECT_ROOT / "src" / Path(*module_name.split(".")).with_suffix(".hs")
    return source if source.is_file() else None


def _main_is_name(main_is: str | None) -> str | None:
    if main_is is None:
        return None
    return main_is.split(".")[-1]


def _run_clash_codegen(flag: str, module_name: str, main_is: str | None) -> tuple[bool, str]:
    source = _module_source_path(module_name)
    if source is None:
        cmd = ["stack", "exec", "clash", "--", flag, module_name]
    else:
        cmd = ["stack", "exec", "clash", "--", flag, "-isrc", str(source.relative_to(PROJECT_ROOT))]
    if main_is:
        cmd += ["-main-is", _main_is_name(main_is)]
    result = subprocess.run(
        cmd,
        cwd=PROJECT_ROOT,
        capture_output=True,
        text=True,
    )
    output = f"{result.stdout}{result.stderr}"
    if result.returncode == 0:
        return True, output

    if "Relocation target for PAGE21 out of range" in output:
        print("[synth] Clash hit GHC relocation bug; retrying once...", flush=True)
        result = subprocess.run(
            cmd,
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True,
        )
        output = f"{result.stdout}{result.stderr}"
        if result.returncode == 0:
            return True, output

    return False, output


def _auto_generate_systemverilog(label: str) -> bool:
    """Attempt to auto-generate SystemVerilog from Clash source."""
    module_name, main_is = _parse_clash_target(label)
    print(f"[synth] SystemVerilog not found, generating from Clash: {label}", flush=True)

    ok, output = _run_clash_codegen("--systemverilog", module_name, main_is)
    if not ok:
        print(f"[synth] Failed to generate SystemVerilog:", flush=True)
        print(output, flush=True)
        return False

    print(f"[synth] ✓ SystemVerilog generated successfully", flush=True)
    return True


def _auto_generate_verilog(label: str) -> bool:
    """Attempt to auto-generate Verilog from Clash source (fallback)."""
    module_name, main_is = _parse_clash_target(label)
    print(f"[synth] Verilog not found, generating from Clash: {label}", flush=True)

    ok, output = _run_clash_codegen("--verilog", module_name, main_is)
    if not ok:
        print(f"[synth] Failed to generate Verilog:", flush=True)
        print(output, flush=True)
        return False

    print(f"[synth] ✓ Verilog generated successfully", flush=True)
    return True


def _find_manifest_path(arg: str) -> Path | None:
    for root in CLASH_HDL_ROOTS:
        candidate = root / arg / "clash-manifest.json"
        if candidate.is_file():
            return candidate
    return None


def _find_manifest_in_root(root: Path, arg: str) -> Path | None:
    candidate = root / arg / "clash-manifest.json"
    return candidate if candidate.is_file() else None


def _ensure_clash_outputs(label: str) -> None:
    sv_manifest = _find_manifest_in_root(SYSTEMVERILOG_ROOT, label)
    if sv_manifest is None:
        if not _auto_generate_systemverilog(label):
            sys.exit(f"error: could not generate SystemVerilog for {label}")

    v_manifest = _find_manifest_in_root(VERILOG_ROOT, label)
    if v_manifest is None:
        if not _auto_generate_verilog(label):
            sys.exit(f"error: could not generate Verilog for {label}")


def load_manifest(arg: str) -> tuple[Path, dict]:
    manifest_path = _find_manifest_in_root(SYSTEMVERILOG_ROOT, arg)

    # Auto-generate SystemVerilog if missing
    if manifest_path is None:
        clash_aliases = load_simple_aliases(CLASH_TARGETS_FILE, required=False)
        module_name = clash_aliases.get(arg, arg)

        if not _auto_generate_systemverilog(module_name):
            sys.exit(f"error: could not generate SystemVerilog for {arg}")

        manifest_path = _find_manifest_in_root(SYSTEMVERILOG_ROOT, arg)

    # Fallback to Verilog if SystemVerilog still missing
    if manifest_path is None:
        manifest_path = _find_manifest_in_root(VERILOG_ROOT, arg)
        if manifest_path is None:
            searched = ", ".join(str(root / arg / "clash-manifest.json") for root in CLASH_HDL_ROOTS)
            sys.exit(f"error: manifest not found after generation (searched: {searched})")

    try:
        data = json.loads(manifest_path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"error: failed to read manifest {manifest_path}: {exc}")
    return manifest_path, data


def verilog_files_from_manifest(manifest_path: Path, manifest: dict) -> list[Path]:
    files: list[Path] = []
    for entry in manifest.get("files", []):
        name = entry.get("name")
        if isinstance(name, str) and name.lower().endswith((".v", ".sv")):
            files.append((manifest_path.parent / name).resolve())
    if not files:
        sys.exit(f"error: manifest at {manifest_path} lists no Verilog/SystemVerilog files")
    return files


_PACKED_2D_RE = re.compile(r"\blogic\s+(?:signed\s+)?\[(?P<outer>[^\]]+)\]\s*\[(?P<inner>[^\]]+)\]")
_UNPACKED_ARG_RE = re.compile(
    r"\b[A-Za-z_][A-Za-z0-9_]*\s+(?:\[[^\]]+\]\s+)?[A-Za-z_][A-Za-z0-9_]*\s*\[[^\]]+\]"
)


def _collapse_single_bit_packed_dims(text: str) -> str:
    def repl(match: re.Match[str]) -> str:
        outer = match.group("outer")
        inner = match.group("inner").replace(" ", "")
        prefix = match.group(0).split("[", 1)[0].rstrip()
        if inner in ("0:0", "0"):
            return f"{prefix} [{outer}]"
        return match.group(0)

    return _PACKED_2D_RE.sub(repl, text)


def _strip_unused_packed2d_functions(text: str, other_text: str) -> str:
    lines = text.splitlines()
    out_lines: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()
        if stripped.startswith("function"):
            header_lines = [line]
            j = i
            if ");" not in line:
                j = i + 1
                while j < len(lines):
                    header_lines.append(lines[j])
                    if ");" in lines[j]:
                        break
                    j += 1
            header = " ".join(h.strip() for h in header_lines)
            m_name = re.search(
                r"function\s+automatic\s+.*?\b([A-Za-z_][A-Za-z0-9_]*)\s*\(",
                header,
            )
            func_name = m_name.group(1) if m_name else None

            packed2d = _PACKED_2D_RE.findall(header)
            has_packed2d = bool(packed2d)
            has_non_single = any(inner.replace(" ", "") not in ("0:0", "0") for _, inner in packed2d)
            args_text = ""
            m_args = re.search(r"\((.*)\)", header)
            if m_args:
                args_text = m_args.group(1)
            has_unpacked_arg = _UNPACKED_ARG_RE.search(args_text) is not None

            if (has_packed2d and has_non_single and func_name) or (has_unpacked_arg and func_name):
                used = re.search(rf"\b{re.escape(func_name)}\b", other_text) is not None
                if used:
                    sys.exit(
                        "error: Yosys can't parse some SystemVerilog function signatures "
                        "(packed multi-dimensional arrays or unpacked array arguments). "
                        f"Function '{func_name}' is referenced. "
                        "Consider synthesizing from Verilog output or add an SV-to-Verilog conversion step."
                    )
                # Skip entire function block
                i = j + 1
                while i < len(lines) and "endfunction" not in lines[i]:
                    i += 1
                if i < len(lines):
                    i += 1
                continue

        out_lines.append(line)
        i += 1

    return "\n".join(out_lines) + ("\n" if text.endswith("\n") else "")


def sanitize_systemverilog_files(files: list[Path], label: str) -> list[Path]:
    out_root = (PROJECT_ROOT / "build" / "sv_yosys" / label).resolve()
    out_root.mkdir(parents=True, exist_ok=True)

    other_text_parts: list[str] = []
    for path in files:
        if path.suffix.lower() == ".sv" and not path.name.endswith("_types.sv"):
            other_text_parts.append(path.read_text(encoding="utf-8"))
    other_text = "\n".join(other_text_parts)

    sanitized: list[Path] = []
    for path in files:
        out_path = out_root / path.name
        if path.suffix.lower() != ".sv":
            shutil.copy2(path, out_path)
            sanitized.append(out_path)
            continue

        text = path.read_text(encoding="utf-8")
        if path.name.endswith("_types.sv"):
            text = _strip_unused_packed2d_functions(text, other_text)
        text = _collapse_single_bit_packed_dims(text)
        out_path.write_text(text, encoding="utf-8")
        sanitized.append(out_path)

    return sanitized


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
            f"write_verilog -sv {out_file_q}",
        ]
    )

    print(f"[synth] Converting {name} VHDL → SystemVerilog...", flush=True)
    rc = yosys_with_ghdl(yosys_script)
    if rc != 0 or not out_file.exists():
        sys.exit(f"error: failed to convert VHDL for {name}")

    return out_file


def write_abc_constraints(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        "\n".join(
            [
                "set_driving_cell BUF_X4",
                "set_load 10.0",
                "",
            ]
        ),
        encoding="utf-8",
    )


def build_yosys_commands(
    verilog_files: list[Path],
    top: str,
    netlist_path: Path,
    liberty: Path,
    abc_constraints: Path,
) -> list[str]:
    def read_step(path: Path) -> str:
        quoted = shlex.quote(str(path))
        if path.suffix.lower() == ".sv":
            return f"read_verilog -sv {quoted}"
        return f"read_verilog {quoted}"

    read_cmds = [read_step(p) for p in verilog_files]
    liberty_q = shlex.quote(str(liberty))
    netlist_q = shlex.quote(str(netlist_path))
    abc_constraints_q = shlex.quote(str(abc_constraints))
    top_q = shlex.quote(top)

    commands = [
        *read_cmds,
        f"hierarchy -check -top {top_q}",
        "proc",
        "opt",
        "techmap",
        f"dfflibmap -liberty {liberty_q}",
        f"abc -D {ABC_DELAY_PS} -constr {abc_constraints_q} -liberty {liberty_q}",
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
    _ensure_clash_outputs(label)
    manifest_path, manifest = load_manifest(label)
    using_systemverilog = SYSTEMVERILOG_ROOT in manifest_path.parents
    top = manifest.get("top_component", {}).get("name")
    if not top:
        sys.exit("error: manifest missing top_component.name")

    seen_files: set[Path] = set()
    verilog_files: list[Path] = []

    def add_files(paths: list[Path]) -> None:
        for path in paths:
            resolved = path.resolve()
            if resolved not in seen_files:
                seen_files.add(resolved)
                verilog_files.append(resolved)

    if using_systemverilog:
        verilog_manifest = _find_manifest_in_root(VERILOG_ROOT, label)
        if verilog_manifest is None:
            _auto_generate_verilog(label)
            verilog_manifest = _find_manifest_in_root(VERILOG_ROOT, label)

        if verilog_manifest is not None:
            print("[synth] SystemVerilog generated; using Verilog for Yosys compatibility.", flush=True)
            manifest_path = verilog_manifest
            try:
                manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            except Exception as exc:
                sys.exit(f"error: failed to read manifest {manifest_path}: {exc}")
            using_systemverilog = False

    add_files(verilog_files_from_manifest(manifest_path, manifest))

    dep_entries = manifest.get("dependencies", {})
    if isinstance(dep_entries, dict):
        transitive = dep_entries.get("transitive", [])
        if isinstance(transitive, list):
            for dep in transitive:
                if not isinstance(dep, str):
                    continue
                dep_manifest_path, dep_manifest = load_manifest(dep)
                add_files(verilog_files_from_manifest(dep_manifest_path, dep_manifest))

    label_name = manifest_path.parent.name
    if any(path.suffix.lower() == ".sv" and SYSTEMVERILOG_ROOT in path.parents for path in verilog_files):
        verilog_files = sanitize_systemverilog_files(verilog_files, label_name)

    out_root = DEFAULT_OUTPUT_ROOT / label_name
    netlist_dir = out_root / "netlist"
    report_dir = out_root / "reports"
    netlist_dir.mkdir(parents=True, exist_ok=True)
    report_dir.mkdir(parents=True, exist_ok=True)

    netlist_path = netlist_dir / f"{top}.mapped.v"
    report_path = report_dir / "yosys.log"

    write_abc_constraints(ABC_CONSTRAINTS)
    commands = build_yosys_commands(verilog_files, top, netlist_path, DEFAULT_LIBERTY, ABC_CONSTRAINTS)

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

    write_abc_constraints(ABC_CONSTRAINTS)
    commands = build_yosys_commands([sv_file], top, netlist_path, DEFAULT_LIBERTY, ABC_CONSTRAINTS)

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
    parser = argparse.ArgumentParser(description="Synthesize a Clash (SystemVerilog) or VHDL target with Yosys.")
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
