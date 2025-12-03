#!/usr/bin/env python3
"""
Manifest-driven bench script (single target only).

Given a target name (directory under ./verilog), it will:
  - Read verilog/<target>/clash-manifest.json
  - Synthesise the target using scripts/synth.py
  - Parse the target's yosys.log to report total and per-module area/seq%

No dependency synthesis is performed. Run inside `nix develop` so yosys is on PATH.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
VERILOG_ROOT = PROJECT_ROOT / "verilog"


def fmt2(value):
    """Format float to two decimals; return 'N/A' for None."""
    if value is None:
        return "N/A"
    return f"{value:.2f}"


def fmt_area(value):
    """Format area with three decimals; return 'N/A' for None."""
    if value is None:
        return "N/A"
    return f"{value:.3f}"


def fmt_mem(value):
    """Format memory in MB with two decimals; return 'N/A' for None."""
    if value is None:
        return "N/A"
    return f"{value:.2f}"


def run_cmd(cmd, label, timeout=3600):
    print(f"[bench] {label}...", file=sys.stderr)
    result = subprocess.run(
        cmd,
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        timeout=timeout,
    )
    if result.returncode != 0:
        print(result.stdout + result.stderr, file=sys.stderr)
        sys.exit(f"[bench] ERROR: {label} failed (exit {result.returncode})")
    return result.stdout + result.stderr


def parse_report(label: str) -> str | None:
    report = PROJECT_ROOT / "build" / "synth" / label / "reports" / "yosys.log"
    if not report.is_file():
        return None
    return report.read_text(encoding="utf-8")


def parse_synth_output(text: str):
    """Extract cpu time, chip area, sequential area/%, and per-module area+seq."""
    cpu = None
    mem = None
    area = None
    seq_area = None
    seq_pct = None
    module_info: dict[str, tuple[float | None, float | None, float | None]] = {}

    # CPU: user 12.53s system 0.63s (take the last one)
    m_all = list(re.finditer(r"CPU:\s*user\s*([0-9.]+)s", text))
    if m_all:
        cpu = float(m_all[-1].group(1))

    # MEM: 3293.16 MB peak (take the last one)
    m_all = list(re.finditer(r"MEM:\s*([0-9.]+) MB", text))
    if m_all:
        mem = float(m_all[-1].group(1))

    # Stream through lines to associate area + seq with modules
    current_mod: str | None = None
    for raw in text.splitlines():
        line = raw.strip()
        m_top = re.match(r"Chip area for top module '\\?([^']+)':\s*([0-9.]+)", line)
        m_mod = re.match(r"Chip area for module '\\?([^']+)':\s*([0-9.]+)", line)
        if m_top:
            name, val = m_top.group(1), float(m_top.group(2))
            area = val
            module_info[name] = (val, None, None)
            current_mod = name
            continue
        if m_mod:
            name, val = m_mod.group(1), float(m_mod.group(2))
            module_info[name] = (val, None, None)
            current_mod = name
            continue

        m_seq = re.match(
            r"of which used for sequential elements:\s*([0-9.]+)\s*\(([0-9.]+)%\)",
            line,
        )
        if m_seq and current_mod:
            sa, sp = float(m_seq.group(1)), float(m_seq.group(2))
            area_val, _, _ = module_info.get(current_mod, (None, None, None))
            module_info[current_mod] = (area_val, sa, sp)
            # If this is the top module, also populate top seq
            if current_mod and area is not None and current_mod in module_info:
                seq_area = sa
                seq_pct = sp

    # Fallback: if top area not found but any area exists, use last generic chip area
    if area is None:
        m_all = list(re.finditer(r"Chip area[^:]*:\s*([0-9.]+)", text))
        if m_all:
            area = float(m_all[-1].group(1))

    # If seq missing, try last seq line globally
    if seq_area is None or seq_pct is None:
        m_all = list(
            re.finditer(r"of which used for sequential elements:\s*([0-9.]+)\s*\(([0-9.]+)%\)", text)
        )
        if m_all:
            seq_area = float(m_all[-1].group(1))
            seq_pct = float(m_all[-1].group(2))

    return cpu, mem, area, seq_area, seq_pct, module_info


def parse_clash_timings(text: str):
    """Extract GHC+Clash load time and per-top compile time from Clash output."""
    load = None
    top_compile = None

    m = re.search(r"GHC\+Clash: Loading modules cumulatively took ([0-9.]+)s", text)
    if m:
        load = float(m.group(1))

    # Prefer the last "Clash: Compiling <...> took Xs" line
    m_all = list(re.finditer(r"Clash: Compiling .* took ([0-9.]+)s", text))
    if m_all:
        top_compile = float(m_all[-1].group(1))

    return load, top_compile


def load_manifest(label: str) -> dict:
    path = VERILOG_ROOT / label / "clash-manifest.json"
    if not path.is_file():
        sys.exit(f"[bench] ERROR: manifest not found at {path}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"[bench] ERROR: could not read manifest {path}: {exc}")


def module_from_label(label: str) -> str:
    """Infer Clash module name from verilog directory label.

    Convention: <Module>.topEntity → <Module>
    """
    suffix = ".topEntity"
    return label[: -len(suffix)] if label.endswith(suffix) else label


def synth_label(label: str) -> str:
    return " ".join(
        [
            "nix",
            "develop",
            "--command",
            "python3",
            "scripts/synth.py",
            label,
        ]
    )


def run_synth(label: str):
    run_cmd(
        [
            "nix",
            "develop",
            "--command",
            "python3",
            "scripts/synth.py",
            label,
        ],
        f"Synth {label}",
    )
    report_text = parse_report(label)
    if report_text is None:
        sys.exit(f"[bench] ERROR: missing report for {label}")
    return parse_synth_output(report_text)


def bench(target_label: str):
    module_name = module_from_label(target_label)

    # Rebuild only this package so Clash sees fresh sources without a full stack build
    run_cmd(["stack", "build", "clash-hash:lib"], "stack build clash-hash:lib")

    # (Re)generate Verilog for the target and capture Clash timings
    clash_output = run_cmd(
        [
            "stack",
            "exec",
            "clash",
            "--",
            "--verilog",
            module_name,
        ],
        f"Verilog gen for {module_name}",
    )

    load_time, compile_time = parse_clash_timings(clash_output)

    manifest = load_manifest(target_label)
    cpu, mem, area, seq_area, seq_pct, modules = run_synth(target_label)

    if modules:
        print("\n[bench] Module areas (from stat):")
        header = f"{'module':<45} {'area (µm²)':>14} {'seq area (µm²)':>16} {'seq %':>8}"
        print("  " + header)
        print("  " + "-" * len(header))
        for mod, (a, sa, sp) in sorted(modules.items()):
            row = f"{mod:<45} {fmt_area(a):>14} {fmt_area(sa):>16} {fmt2(sp):>8}%"
            print("  " + row)

    print(
        "\n[bench] Time/Mem: load {0}s | compile {1}s | synth {2}s | mem {3} MB".format(
            fmt2(load_time), fmt2(compile_time), fmt2(cpu), fmt_mem(mem)
        )
    )

    # Final chip summary intentionally omitted; top module appears in the table.


def main():
    parser = argparse.ArgumentParser(description="Manifest-driven synthesis benchmark")
    parser.add_argument("target", help="Directory name under ./verilog (e.g., Hash.Stateful4.topEntity)")
    args = parser.parse_args()

    bench(args.target)


if __name__ == "__main__":
    main()
