#!/usr/bin/env python3
"""
Minimal Yosys wrapper that synthesizes a single Clash output directory.

Usage:
    python3 scripts/synth.py Hash.Stateful4.topEntity

This resolves to verilog/<arg>/clash-manifest.json, reads only the .v files
listed in that manifest, uses top_component.name as the top module, and runs a
simple Yosys flow. Outputs land in build/synth/<arg>/netlist and reports.
"""

from __future__ import annotations

import argparse
import json
import shlex
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
VERILOG_ROOT = PROJECT_ROOT / "verilog"
DEFAULT_OUTPUT_ROOT = PROJECT_ROOT / "build" / "synth"
DEFAULT_LIBERTY = PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"


def load_manifest(arg: str) -> tuple[Path, dict]:
    manifest_path = VERILOG_ROOT / arg / "clash-manifest.json"
    if not manifest_path.is_file():
        sys.exit(f"error: manifest not found at {manifest_path}")
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


def build_yosys_commands(verilog_files: list[Path], top: str, netlist_path: Path, liberty: Path) -> list[str]:
    quoted_files = [shlex.quote(str(p)) for p in verilog_files]
    liberty_q = shlex.quote(str(liberty))
    netlist_q = shlex.quote(str(netlist_path))
    top_q = shlex.quote(top)

    # Intentionally simple pass sequence.
    commands = [
        *(f"read_verilog {vf}" for vf in quoted_files),
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


def main(argv: list[str]) -> None:
    parser = argparse.ArgumentParser(description="Synthesize a Clash manifest target with Yosys.")
    parser.add_argument(
        "target",
        help="Name of the directory under ./verilog (e.g., Hash.Stateful4.topEntity)",
    )
    args = parser.parse_args(argv)

    manifest_path, manifest = load_manifest(args.target)
    top = manifest.get("top_component", {}).get("name")
    if not top:
        sys.exit("error: manifest missing top_component.name")

    if not DEFAULT_LIBERTY.is_file():
        sys.exit(f"error: liberty file not found at {DEFAULT_LIBERTY}")

    verilog_files = collect_verilog_files(manifest_path, manifest)

    label = manifest_path.parent.name
    out_root = DEFAULT_OUTPUT_ROOT / label
    netlist_dir = out_root / "netlist"
    report_dir = out_root / "reports"
    netlist_dir.mkdir(parents=True, exist_ok=True)
    report_dir.mkdir(parents=True, exist_ok=True)

    netlist_path = netlist_dir / f"{top}.mapped.v"
    report_path = report_dir / "yosys.log"

    commands = build_yosys_commands(verilog_files, top, netlist_path, DEFAULT_LIBERTY)

    print(f"[synth] {label} → top={top}")
    result = run_yosys(commands)
    output = f"{result.stdout}{result.stderr}"
    report_path.write_text(output, encoding="utf-8")

    if result.returncode != 0:
        sys.exit(f"error: yosys exited with code {result.returncode}\n{output}")

    print(f"  ✓ netlist: {netlist_path.relative_to(PROJECT_ROOT)}")
    print(f"  ↳ report : {report_path.relative_to(PROJECT_ROOT)}")
    summarise(output)


if __name__ == "__main__":
    main(sys.argv[1:])
