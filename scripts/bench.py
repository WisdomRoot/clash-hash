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
import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path

from bench_cache import cache_stage_reusable, compute_stage_plan, load_cache, save_cache

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SYSTEMVERILOG_ROOT = PROJECT_ROOT / "systemverilog"
VERILOG_ROOT = PROJECT_ROOT / "verilog"
CLASH_HDL_ROOTS = [SYSTEMVERILOG_ROOT, VERILOG_ROOT]
CLASH_TARGETS_FILE = PROJECT_ROOT / "clash.json"
VHDL_TARGETS_FILE = PROJECT_ROOT / "vhdl.json"
CACHE_ROOT = PROJECT_ROOT / "build" / "cache"
LIBERTY_FILE = PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"


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


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_text(text: str) -> str:
    return sha256_bytes(text.encode("utf-8"))


def sha256_file(path: Path) -> str:
    if not path.is_file():
        return f"missing:{path}"
    return sha256_bytes(path.read_bytes())


def hash_paths(paths: list[Path]) -> str:
    payload = []
    for path in sorted(paths, key=lambda p: str(p)):
        payload.append(f"{path}:{sha256_file(path)}")
    return sha256_text("\n".join(payload))


def tool_version(cmd: list[str]) -> str:
    try:
        result = subprocess.run(
            cmd,
            cwd=PROJECT_ROOT,
            text=True,
            capture_output=True,
            timeout=30,
        )
    except Exception:
        return f"missing:{' '.join(cmd)}"
    output = (result.stdout + result.stderr).strip()
    if result.returncode != 0:
        return f"error:{' '.join(cmd)}:{output}"
    first = output.splitlines()[0] if output else "unknown"
    return first


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


def cache_file_for(target: str) -> Path:
    return CACHE_ROOT / f"{target}.json"


def unique_paths(paths: list[Path]) -> list[Path]:
    seen: set[Path] = set()
    unique: list[Path] = []
    for path in paths:
        resolved = path.resolve()
        if resolved not in seen:
            seen.add(resolved)
            unique.append(resolved)
    return unique


def load_manifest_path(path: Path) -> dict:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        sys.exit(f"[bench] ERROR: could not read manifest {path}: {exc}")


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
    return load_manifest_path(path)


def verilog_files_from_manifest(manifest_path: Path, manifest: dict) -> list[Path]:
    files: list[Path] = []
    for entry in manifest.get("files", []):
        name = entry.get("name")
        if isinstance(name, str) and name.lower().endswith((".v", ".sv")):
            files.append((manifest_path.parent / name).resolve())
    return files


def manifest_artifact_paths(manifest_path: Path) -> list[Path]:
    artifacts = [manifest_path]
    if not manifest_path.is_file():
        return artifacts
    manifest = load_manifest_path(manifest_path)
    for entry in manifest.get("files", []):
        name = entry.get("name")
        if isinstance(name, str) and name:
            artifacts.append((manifest_path.parent / name).resolve())
    return artifacts


def collect_stack_build_artifacts() -> list[Path]:
    patterns = [
        ".stack-work/dist/*/ghc-*/build/libHSclash-hash*.a",
        ".stack-work/dist/*/ghc-*/build/libHSclash-hash*.so",
        ".stack-work/dist/*/ghc-*/build/libHSclash-hash*.dylib",
        ".stack-work/dist/*/ghc-*/package.conf.inplace/clash-hash-*.conf",
    ]
    paths: list[Path] = []
    for pattern in patterns:
        paths.extend(PROJECT_ROOT.glob(pattern))
    unique = unique_paths(paths)
    if not unique:
        sys.exit("[bench] ERROR: could not find Stack build artifacts for HDL cache key")
    return sorted(unique)


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


def resolve_target_label(target: str) -> str:
    if target in VHDL_TARGETS:
        return target
    return ALIASES.get(target, target)


def load_top_module(target: str) -> str:
    if target in VHDL_TARGETS:
        entry = VHDL_TARGETS[target]
        top = entry.get("top")
        if not isinstance(top, str) or not top:
            sys.exit(f"[bench] ERROR: could not resolve VHDL top for {target}")
        return top

    resolved = resolve_target_label(target)
    manifest = load_manifest(output_label(resolved))
    top = manifest.get("top_component", {}).get("name")
    if not isinstance(top, str) or not top:
        sys.exit(f"[bench] ERROR: could not resolve top module for {resolved}")
    return top


def collect_synth_input_files(target: str) -> list[Path]:
    if target in VHDL_TARGETS:
        entry = VHDL_TARGETS[target]
        dir_name = entry.get("dir") or target
        files = entry.get("files")
        if isinstance(files, list) and all(isinstance(f, str) for f in files):
            return [PROJECT_ROOT / "vhdl" / dir_name / f for f in files]
        return []

    resolved = resolve_target_label(target)
    manifest_path = VERILOG_ROOT / output_label(resolved) / "clash-manifest.json"
    if not manifest_path.is_file():
        manifest_path = SYSTEMVERILOG_ROOT / output_label(resolved) / "clash-manifest.json"
    if not manifest_path.is_file():
        sys.exit(f"[bench] ERROR: missing manifest for synth inputs of {resolved}")

    seen: set[Path] = set()
    paths: list[Path] = []

    def add_manifest(manifest_label: str, path: Path | None = None) -> None:
        manifest_file = path
        if manifest_file is None:
            manifest_file = VERILOG_ROOT / manifest_label / "clash-manifest.json"
            if not manifest_file.is_file():
                manifest_file = SYSTEMVERILOG_ROOT / manifest_label / "clash-manifest.json"
        if manifest_file is None or not manifest_file.is_file():
            sys.exit(f"[bench] ERROR: missing dependency manifest for {manifest_label}")
        manifest = load_manifest_path(manifest_file)
        for file_path in verilog_files_from_manifest(manifest_file, manifest):
            resolved_path = file_path.resolve()
            if resolved_path not in seen:
                seen.add(resolved_path)
                paths.append(resolved_path)
        dep_entries = manifest.get("dependencies", {})
        if isinstance(dep_entries, dict):
            transitive = dep_entries.get("transitive", [])
            if isinstance(transitive, list):
                for dep in transitive:
                    if isinstance(dep, str):
                        add_manifest(dep)

    add_manifest(output_label(resolved), manifest_path)
    return sorted(paths)


def hdl_stage_current(target: str, module_name: str | None = None, main_is: str | None = None) -> dict:
    if target in VHDL_TARGETS:
        entry = VHDL_TARGETS[target]
        key = sha256_text(
            json.dumps(
                {
                    "target": target,
                    "kind": "vhdl",
                    "entry": entry,
                },
                sort_keys=True,
            )
        )
        return {
            "key": key,
            "artifacts": [str(VHDL_TARGETS_FILE.resolve())],
        }

    resolved = resolve_target_label(target)
    label = output_label(resolved)
    sv_manifest = SYSTEMVERILOG_ROOT / label / "clash-manifest.json"
    v_manifest = VERILOG_ROOT / label / "clash-manifest.json"
    artifacts = unique_paths(manifest_artifact_paths(sv_manifest) + manifest_artifact_paths(v_manifest))
    key = sha256_text(
        json.dumps(
            {
                "target": target,
                "resolved": resolved,
                "module": module_name,
                "main_is": main_is,
                "backends": ["systemverilog", "verilog"],
                "stack": hash_paths(collect_stack_build_artifacts()),
            },
            sort_keys=True,
        )
    )
    return {
        "key": key,
        "artifacts": [str(path) for path in artifacts],
    }


def run_hdl(target: str, module_name: str, main_is: str | None) -> None:
    clash_cmd = ["stack", "exec", "clash", "--", "--systemverilog", module_name]
    if main_is:
        clash_cmd += ["-main-is", main_is]
    run_cmd(clash_cmd, f"SystemVerilog gen for {module_name}")

    verilog_cmd = ["stack", "exec", "clash", "--", "--verilog", module_name]
    if main_is:
        verilog_cmd += ["-main-is", main_is]
    run_cmd(verilog_cmd, f"Verilog gen for {module_name}")


def synth_stage_current(target: str, top: str) -> dict:
    label = output_label(resolve_target_label(target))
    report = PROJECT_ROOT / "build" / "synth" / label / "reports" / "yosys.log"
    netlist = PROJECT_ROOT / "build" / "synth" / label / "netlist" / f"{top}.mapped.v"
    inputs = collect_synth_input_files(target) + [PROJECT_ROOT / "scripts" / "synth.py", LIBERTY_FILE]
    key = sha256_text(
        json.dumps(
            {
                "target": target,
                "label": label,
                "top": top,
                "yosys": tool_version(["yosys", "--version"]),
                "inputs": hash_paths(inputs),
            },
            sort_keys=True,
        )
    )
    return {
        "key": key,
        "artifacts": [str(netlist), str(report)],
    }


def sta_input_sdc_path(target: str, top: str) -> Path:
    if target in VHDL_TARGETS:
        generated = PROJECT_ROOT / "build" / "sta" / f"{top}.sdc"
        return generated
    resolved = resolve_target_label(target)
    clash_sdc = SYSTEMVERILOG_ROOT / output_label(resolved) / f"{top}.sdc"
    if clash_sdc.exists():
        return clash_sdc
    generated = PROJECT_ROOT / "build" / "sta" / f"{top}.sdc"
    return generated


def sta_stage_current(target: str, top: str) -> dict:
    label = output_label(resolve_target_label(target))
    netlist = PROJECT_ROOT / "build" / "synth" / label / "netlist" / f"{top}.mapped.v"
    summary = PROJECT_ROOT / "build" / "sta" / top / "reports" / "summary.rpt"
    sdc = sta_input_sdc_path(target, top)
    tcl_files = sorted((PROJECT_ROOT / "scripts" / "tcl").glob("*.tcl"))
    inputs = [netlist, sdc, PROJECT_ROOT / "scripts" / "sta.py", LIBERTY_FILE, *tcl_files]
    key = sha256_text(
        json.dumps(
            {
                "target": target,
                "label": label,
                "top": top,
                "sta": tool_version(["sta", "-version"]),
                "inputs": hash_paths(inputs),
            },
            sort_keys=True,
        )
    )
    return {
        "key": key,
        "artifacts": [str(summary)],
    }


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
        cache_path = cache_file_for(requested_target)
        cache = load_cache(cache_path)
        top = load_top_module(requested_target)
        hdl_current = hdl_stage_current(requested_target)
        synth_current = synth_stage_current(requested_target, top)
        sta_current = sta_stage_current(requested_target, top)
        plan = compute_stage_plan(
            {
                "stages": {
                    "hdl": hdl_current,
                    "synth": synth_current,
                    "sta": sta_current,
                }
            },
            cache,
        )

        if plan["synth"] == "run":
            cpu, mem, area, seq_area, seq_pct, modules = run_synth(requested_target)
        else:
            cpu, mem, area, seq_area, seq_pct, modules = parse_synth_output(
                parse_report(output_label(resolve_target_label(requested_target))) or ""
            )
        synth_current = synth_stage_current(requested_target, top)
        sta_current = sta_stage_current(requested_target, top)

        if plan["sta"] == "run":
            sta = run_sta(requested_target)
        else:
            sta = parse_sta_summary(resolve_sta_summary_path(requested_target).read_text(encoding="utf-8"))
        sta_current = sta_stage_current(requested_target, top)

        save_cache(
            cache_path,
            {
                "target": requested_target,
                "top": top,
                "stages": {
                    "hdl": {**hdl_current, "success": True},
                    "synth": {**synth_current, "success": True},
                    "sta": {**sta_current, "success": True},
                },
            },
        )

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

    cache_path = cache_file_for(requested_target)
    cache = load_cache(cache_path)
    hdl_current = hdl_stage_current(requested_target, module_name, main_is)
    cached_hdl = cache.get("stages", {}).get("hdl") if isinstance(cache, dict) else None
    if not cache_stage_reusable(hdl_current, cached_hdl):
        run_hdl(requested_target, module_name, main_is)
    hdl_current = hdl_stage_current(requested_target, module_name, main_is)

    top = load_top_module(requested_target)
    synth_current = synth_stage_current(requested_target, top)
    sta_current = sta_stage_current(requested_target, top)
    plan = compute_stage_plan(
        {
            "stages": {
                "hdl": hdl_current,
                "synth": synth_current,
                "sta": sta_current,
            }
        },
        cache,
    )

    if plan["synth"] == "run":
        cpu, mem, area, seq_area, seq_pct, modules = run_synth(requested_target)
    else:
        cpu, mem, area, seq_area, seq_pct, modules = parse_synth_output(
            parse_report(output_label(resolve_target_label(requested_target))) or ""
        )

    synth_current = synth_stage_current(requested_target, top)
    sta_current = sta_stage_current(requested_target, top)
    if plan["sta"] == "run":
        sta = run_sta(requested_target)
    else:
        sta = parse_sta_summary(resolve_sta_summary_path(requested_target).read_text(encoding="utf-8"))
    sta_current = sta_stage_current(requested_target, top)

    save_cache(
        cache_path,
        {
            "target": requested_target,
            "top": top,
            "stages": {
                "hdl": {**hdl_current, "success": True},
                "synth": {**synth_current, "success": True},
                "sta": {**sta_current, "success": True},
            },
        },
    )

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


def main():
    parser = argparse.ArgumentParser(description="Manifest-driven synthesis benchmark + STA")
    parser.add_argument("target", help="Directory name under ./verilog (e.g., Hash.Stateful4.topEntity)")
    args = parser.parse_args()

    bench(args.target)


if __name__ == "__main__":
    main()
