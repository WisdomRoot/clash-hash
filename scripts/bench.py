#!/usr/bin/env python3
"""
Manifest-driven bench script (single target only).

Given a target name (directory under ./systemverilog or ./verilog), it will:
  - Read systemverilog/<target>/clash-manifest.json (fallback to verilog/<target>)
  - Synthesise the target using scripts/synth.py
  - Parse the target's yosys.log to report total and per-module area/seq%
  - Run static timing analysis using scripts/sta.py

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
SYSTEMVERILOG_ROOT = PROJECT_ROOT / "systemverilog"
VERILOG_ROOT = PROJECT_ROOT / "verilog"
CLASH_HDL_ROOTS = [SYSTEMVERILOG_ROOT, VERILOG_ROOT]
CLASH_TARGETS_FILE = PROJECT_ROOT / "clash.json"
VHDL_TARGETS_FILE = PROJECT_ROOT / "vhdl.json"


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
    result = subprocess.run(
        cmd,
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        timeout=timeout,
    )
    if result.returncode != 0:
        output = result.stdout + result.stderr
        if "Relocation target for PAGE21 out of range" in output:
            result = subprocess.run(
                cmd,
                cwd=PROJECT_ROOT,
                text=True,
                capture_output=True,
                timeout=timeout,
            )
            output = result.stdout + result.stderr
        if result.returncode != 0:
            print(output, file=sys.stderr)
            sys.exit(f"[bench] ERROR: {label} failed (exit {result.returncode})")
        return output
    return result.stdout + result.stderr


def output_label(target: str) -> str:
    if target in VHDL_TARGETS:
        dir_name = VHDL_TARGETS[target].get("dir") or target
        return f"vhdl_{dir_name}"
    return target


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
    path = None
    for root in CLASH_HDL_ROOTS:
        candidate = root / label / "clash-manifest.json"
        if candidate.is_file():
            path = candidate
            break
    if path is None:
        searched = ", ".join(str(root / label / "clash-manifest.json") for root in CLASH_HDL_ROOTS)
        sys.exit(f"[bench] ERROR: manifest not found (searched: {searched})")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"[bench] ERROR: could not read manifest {path}: {exc}")


def parse_clash_target(label: str) -> tuple[str, str | None]:
    """Return (module_name, main_is) for a Clash target label.

    - <Module>.topEntity → (Module, None)
    - <Module>.<func>    → (Module, Module.func) when func starts lowercase
    - <Module>           → (Module, None)
    """
    suffix = ".topEntity"
    if label.endswith(suffix):
        return label[: -len(suffix)], None
    parts = label.split(".")
    if parts and parts[-1] and parts[-1][0].islower():
        return ".".join(parts[:-1]), label
    return label, None


def load_aliases(path: Path, required: bool = False) -> dict[str, str]:
    if not path.is_file():
        if required:
            sys.exit(f"[bench] ERROR: targets file missing at {path}")
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"[bench] ERROR: could not parse {path}: {exc}")
    if not isinstance(data, dict):
        sys.exit(f"[bench] ERROR: targets file {path} must contain a JSON object")
    return {str(k): str(v) for k, v in data.items()}


ALIASES = load_aliases(CLASH_TARGETS_FILE, required=True)
def load_vhdl_targets(path: Path) -> dict[str, dict]:
    if not path.is_file():
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"[bench] ERROR: could not parse {path}: {exc}")
    if not isinstance(data, dict):
        sys.exit(f"[bench] ERROR: vhdl targets file {path} must contain a JSON object")
    targets: dict[str, dict] = {}
    for name, entry in data.items():
        if not isinstance(entry, dict):
            sys.exit(f"[bench] ERROR: vhdl target '{name}' must be an object")
        targets[str(name)] = entry
    return targets


VHDL_TARGETS = load_vhdl_targets(VHDL_TARGETS_FILE)


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


def run_synth(target: str):
    synth_target = target
    if target not in VHDL_TARGETS:
        synth_target = ALIASES.get(target, target)
    run_cmd(
        [
            "nix",
            "develop",
            "--command",
            "python3",
            "scripts/synth.py",
            target,
        ],
        f"Synth {target}",
    )
    report_text = parse_report(output_label(synth_target))
    if report_text is None:
        sys.exit(f"[bench] ERROR: missing report for {output_label(synth_target)}")
    return parse_synth_output(report_text)


def resolve_sta_summary_path(target: str) -> Path:
    sta_target = target
    if target not in VHDL_TARGETS:
        sta_target = ALIASES.get(target, target)
    manifest = load_manifest(output_label(sta_target))
    top = manifest.get("top_component", {}).get("name")
    if not isinstance(top, str) or not top:
        sys.exit(f"[bench] ERROR: could not resolve top module for STA summary of {sta_target}")
    return PROJECT_ROOT / "build" / "sta" / top / "reports" / "summary.rpt"


def parse_sta_summary(text: str) -> dict[str, str]:
    fields: dict[str, str] = {}
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line.startswith("Design Type:"):
            continue
        if ":" in line:
            key, value = line.split(":", 1)
            fields[key.strip()] = value.strip()
    return fields


def run_sta(target: str):
    result = subprocess.run(
        [
            sys.executable,
            "scripts/sta.py",
            target,
        ],
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        timeout=3600,
    )
    output = result.stdout + result.stderr
    if result.returncode != 0:
        print(output, file=sys.stderr)
        sys.exit(f"[bench] ERROR: STA for {target} failed (exit {result.returncode})")

    summary_path = resolve_sta_summary_path(target)
    if not summary_path.is_file():
        sys.exit(f"[bench] ERROR: missing STA summary at {summary_path}")

    return parse_sta_summary(summary_path.read_text(encoding="utf-8"))


def bench(target_label: str):
    requested_target = target_label
    if requested_target in VHDL_TARGETS:
        cpu, mem, area, seq_area, seq_pct, modules = run_synth(requested_target)
        sta = run_sta(requested_target)
        critical_path = sta.get("Critical Path") or sta.get("Combinational Delay") or "N/A"
        wns = sta.get("WNS (max)", "N/A")
        tns = sta.get("TNS (max)", "N/A")
        worst_slack = sta.get("Worst Slack", "N/A")
        print(requested_target)
        print_metric("area", fmt_area(area), "um^2")
        print_metric("critical path", *split_value_unit(critical_path))
        print_metric("wns", *split_value_unit(wns))
        print_metric("tns", *split_value_unit(tns))
        print_metric("worst slack", *split_value_unit(worst_slack))
        return

    resolved_target = ALIASES.get(requested_target, requested_target)
    module_name, main_is = parse_clash_target(resolved_target)

    # Rebuild only this package so Clash sees fresh sources without a full stack build
    run_cmd(["stack", "build", "clash-hash:lib"], "stack build clash-hash:lib")

    # (Re)generate SystemVerilog for the target and capture Clash timings
    clash_cmd = ["stack", "exec", "clash", "--", "--systemverilog", module_name]
    if main_is:
        clash_cmd += ["-main-is", main_is]
    clash_output = run_cmd(clash_cmd, f"SystemVerilog gen for {module_name}")
    # Also emit Verilog for tool compatibility
    verilog_cmd = ["stack", "exec", "clash", "--", "--verilog", module_name]
    if main_is:
        verilog_cmd += ["-main-is", main_is]
    run_cmd(verilog_cmd, f"Verilog gen for {module_name}")

    load_time, compile_time = parse_clash_timings(clash_output)

    cpu, mem, area, seq_area, seq_pct, modules = run_synth(requested_target)

    sta = run_sta(requested_target)
    critical_path = sta.get("Critical Path") or sta.get("Combinational Delay") or "N/A"
    wns = sta.get("WNS (max)", "N/A")
    tns = sta.get("TNS (max)", "N/A")
    worst_slack = sta.get("Worst Slack", "N/A")
    print(requested_target)
    print_metric("area", fmt_area(area), "um^2")
    print_metric("critical path", *split_value_unit(critical_path))
    print_metric("wns", *split_value_unit(wns))
    print_metric("tns", *split_value_unit(tns))
    print_metric("worst slack", *split_value_unit(worst_slack))


def split_value_unit(text: str) -> tuple[str, str]:
    parts = text.split(None, 1)
    if len(parts) == 2:
        return parts[0], parts[1]
    if len(parts) == 1:
        return parts[0], ""
    return "N/A", ""


def print_metric(label: str, value: str, unit: str) -> None:
    print(f"  {label:<15} {value:>12} {unit}")

    # Final chip summary intentionally omitted; top module appears in the table.


def main():
    parser = argparse.ArgumentParser(description="Manifest-driven synthesis benchmark + STA")
    parser.add_argument("target", help="Directory name under ./verilog (e.g., Hash.Stateful4.topEntity)")
    args = parser.parse_args()

    bench(args.target)


if __name__ == "__main__":
    main()
