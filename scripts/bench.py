#!/usr/bin/env python3
"""Benchmark script to collect synthesis metrics for Keccak variants."""

import argparse
import re
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]

# Map subcommand to variant details (module names for Clash invocation)
VARIANTS = {
    "F200": {
        "name": "KeccakF200",
        "perm_module": "Permutation.KeccakF200",
        "top_module": "Hash.KeccakF200",
    },
    "F400": {
        "name": "KeccakF400",
        "perm_module": "Permutation.KeccakF400",
        "top_module": "Hash.KeccakF400",
    },
    "F800": {
        "name": "KeccakF800",
        "perm_module": "Permutation.KeccakF800",
        "top_module": "Hash.KeccakF800",
    },
    "SHA3": {
        "name": "KeccakF1600",
        "perm_module": "Permutation.KeccakF1600",
        "top_module": "Hash.KeccakF1600",
    },
    "S4": {
        "name": "Stateful4",
        "perm_module": "Permutation.KeccakF1600",  # reuse 1600 perm
        "top_module": "Sponge.Stateful4",
    },
}


def run_command(cmd, description, timeout=3600):
    """Run a command and return stdout."""
    print(f"[bench] {description}...", file=sys.stderr)
    try:
        result = subprocess.run(
            cmd,
            cwd=PROJECT_ROOT,
            text=True,
            capture_output=True,
            timeout=timeout,
        )
        if result.returncode != 0:
            print(f"[bench] ERROR: {description} failed with exit code {result.returncode}", file=sys.stderr)
            print(f"[bench] Run this command to see the error:", file=sys.stderr)
            print(f"  {' '.join(cmd)}", file=sys.stderr)
            sys.exit(1)
        return result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        print(f"[bench] ERROR: {description} timed out after {timeout}s", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"[bench] ERROR: {description} failed: {e}", file=sys.stderr)
        sys.exit(1)


def parse_verilog_gen_time(output, entity_name):
    """Parse Clash output for 'Compiling <entity> took X.XXXs'."""
    pattern = rf"Compiling {re.escape(entity_name)} took ([0-9.]+)s"
    match = re.search(pattern, output)
    if match:
        return float(match.group(1))
    print(f"[bench] WARNING: Could not find Verilog gen time for {entity_name}", file=sys.stderr)
    return None


def parse_synth_output(output):
    """Parse synth output for CPU time, chip area, and sequential percentage."""
    cpu_time = None
    chip_area = None
    seq_pct = None

    # CPU user  : 1.06s
    cpu_match = re.search(r"CPU user\s*:\s*([0-9.]+)s", output)
    if cpu_match:
        cpu_time = float(cpu_match.group(1))

    # Chip area : 1948.450
    area_match = re.search(r"Chip area\s*:\s*([0-9.]+)", output)
    if area_match:
        chip_area = float(area_match.group(1))

    # of which used for sequential elements: 1452.360000 (27.55%)
    seq_match = re.search(r"of which used for sequential elements:.*\(([0-9.]+)%\)", output)
    if seq_match:
        seq_pct = float(seq_match.group(1))

    return cpu_time, chip_area, seq_pct


def benchmark_variant(variant_key):
    """Run benchmark for a variant and collect all 6 metrics."""
    variant = VARIANTS[variant_key]
    name = variant["name"]

    # 0. Clean stale builds
    run_command(
        ["stack", "clean"],
        "Cleaning stale builds",
    )

    # 1. Verilog generation for Permutation
    perm_verilog_output = run_command(
        ["stack", "exec", "clash", "--", "--verilog", f"{variant['perm_module']}"] ,
        f"Verilog gen for {variant['perm_module']}.topEntity",
    )
    perm_vlog_time = parse_verilog_gen_time(perm_verilog_output, f"{variant['perm_module']}.topEntity")

    # 2. Verilog generation for Top
    top_verilog_output = run_command(
        ["stack", "exec", "clash", "--", "--verilog", f"{variant['top_module']}"] ,
        f"Verilog gen for {variant['top_module']}.topEntity",
    )
    top_vlog_time = parse_verilog_gen_time(top_verilog_output, f"{variant['top_module']}.topEntity")

    # 3. Synthesis for Permutation
    perm_synth_output = run_command(
        ["nix", "develop", "--command", "synth", f"{variant['perm_module']}.topEntity"],
        f"Synthesis for {variant['perm_module']}.topEntity",
    )
    perm_cpu, perm_area, _ = parse_synth_output(perm_synth_output)

    # 4. Synthesis for Top
    top_synth_output = run_command(
        ["nix", "develop", "--command", "synth", f"{variant['top_module']}.topEntity"],
        f"Synthesis for {variant['top_module']}.topEntity",
    )
    top_cpu, top_area, seq_pct = parse_synth_output(top_synth_output)

    # Calculate total synthesis time
    total_cpu = None
    if perm_cpu is not None and top_cpu is not None:
        total_cpu = perm_cpu + top_cpu

    # Print results
    print(f"\n[bench] Results for {name}:", file=sys.stderr)
    print(f"Perm Verilog gen:  {perm_vlog_time}s", file=sys.stderr)
    print(f"Top Verilog gen:   {top_vlog_time}s", file=sys.stderr)
    print(f"Perm synth time:   {perm_cpu}s", file=sys.stderr)
    print(f"Top synth time:    {top_cpu}s", file=sys.stderr)
    print(f"Total synth time:  {total_cpu}s", file=sys.stderr)
    print(f"Perm area:         {perm_area} µm²", file=sys.stderr)
    print(f"Total area:        {top_area} µm² ({seq_pct}% sequential)", file=sys.stderr)

    # Format for ATTEMPTS.md table
    vlog_str = f"{perm_vlog_time}s / {top_vlog_time}s"
    synth_str = f"{total_cpu:.2f}s ({perm_cpu}s / {top_cpu}s)"
    area_str = f"{perm_area} (0%) / {top_area} ({seq_pct}%)"

    table_row = f"| **Reintroduce Vec** | {vlog_str} | {synth_str} | {area_str} |"

    print(f"\n[bench] ATTEMPTS.md table row:", file=sys.stderr)
    print(table_row)


def main():
    parser = argparse.ArgumentParser(description="Benchmark Keccak variants")
    parser.add_argument(
        "variant",
        choices=list(VARIANTS.keys()),
        help="Variant to benchmark (F200, F400, F800, SHA3)",
    )
    args = parser.parse_args()

    benchmark_variant(args.variant)


if __name__ == "__main__":
    main()
