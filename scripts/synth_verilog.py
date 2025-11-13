#!/usr/bin/env python3
"""Project-specific helper to synthesise Clash-generated Verilog with Yosys."""

from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

PROJECT_ROOT = Path(__file__).resolve().parents[1]
VERILOG_ROOT = PROJECT_ROOT / "verilog"
DEFAULT_OUTPUT_ROOT = PROJECT_ROOT / "build" / "synth"
LIBERTY_CANDIDATES = [
    PROJECT_ROOT / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib",
    PROJECT_ROOT / "synthesis" / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib",
    PROJECT_ROOT / "lib" / "NangateOpenCellLibrary_typical.lib",
]
ABC_SCRIPT_CANDIDATES = [
    PROJECT_ROOT / "scripts" / "abc_fast.script",
]
MODULE_PATTERN = re.compile(r"^\s*module\s+([A-Za-z_][\w$]*)\b")


@dataclass(frozen=True)
class SynthTarget:
    label: str
    verilog: Path
    top: str
    manifest: Path | None = None

    def output_root(self, base: Path) -> Path:
        return base / self.label


@dataclass(frozen=True)
class SynthConfig:
    liberty: Path
    output_root: Path
    abc_script: Path | None
    dry_run: bool


def ensure_tool_available(name: str) -> None:
    if shutil.which(name) is None:  # type: ignore[name-defined]
        sys.exit(f"error: required tool '{name}' not found in PATH")


def resolve_liberty(explicit: str | None) -> Path:
    if explicit:
        path = Path(explicit).expanduser()
        if not path.is_file():
            sys.exit(f"error: liberty file '{explicit}' not found")
        return path

    for candidate in LIBERTY_CANDIDATES:
        if candidate.is_file():
            return candidate

    env_path = os.getenv("NANGATE45_LIB")
    if env_path and Path(env_path).is_file():
        return Path(env_path)

    sys.exit("error: liberty file not found; pass --liberty or set NANGATE45_LIB")


def resolve_abc_script() -> Path | None:
    for candidate in ABC_SCRIPT_CANDIDATES:
        if candidate.is_file():
            return candidate
    env = os.getenv("ABC_SCRIPT")
    if env and Path(env).is_file():
        return Path(env)
    return None


def discover_targets(arguments: Sequence[str], explicit_top: str | None) -> list[SynthTarget]:
    if not arguments:
        # Default to every Clash build folder under verilog/ that actually contains HDL artefacts.
        candidates = [
            p
            for p in VERILOG_ROOT.iterdir()
            if p.is_dir()
            and ((p / "clash-manifest.json").is_file() or any(p.glob("*.v")))
        ]
        if not candidates:
            sys.exit("error: no synthesis targets provided and none discovered in ./verilog")
        return [target_from_directory(path) for path in sorted(candidates)]

    targets: list[SynthTarget] = []
    for raw in arguments:
        path = Path(raw)
        if not path.exists():
            as_verilog_dir = VERILOG_ROOT / raw
            if as_verilog_dir.exists():
                path = as_verilog_dir
        if path.is_dir():
            targets.append(target_from_directory(path))
            continue
        if path.suffix == ".json":
            targets.append(target_from_manifest(path))
            continue
        if path.suffix == ".v":
            top = explicit_top or infer_top_from_verilog(path)
            if top is None:
                sys.exit(f"error: could not infer top module for {path}; use --top")
            label = path.stem
            targets.append(SynthTarget(label=label, verilog=path.resolve(), top=top))
            continue
        sys.exit(f"error: unsupported target '{raw}'")

    if explicit_top and len(targets) != 1:
        sys.exit("--top can only be supplied when targeting a single Verilog file")

    return targets


def target_from_directory(path: Path) -> SynthTarget:
    manifest = path / "clash-manifest.json"
    if manifest.is_file():
        return target_from_manifest(manifest)

    verilog_files = list(path.glob("*.v"))
    if not verilog_files:
        sys.exit(f"error: directory '{path}' does not contain any .v sources")
    if len(verilog_files) > 1:
        sys.exit(f"error: directory '{path}' has multiple .v files; specify one explicitly")

    verilog = verilog_files[0]
    top = infer_top_from_verilog(verilog)
    if top is None:
        sys.exit(f"error: could not infer top module for {verilog}")
    return SynthTarget(label=path.name, verilog=verilog.resolve(), top=top)


def target_from_manifest(manifest_path: Path) -> SynthTarget:
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except OSError as exc:
        sys.exit(f"error: failed to read manifest {manifest_path}: {exc}")
    except json.JSONDecodeError as exc:
        sys.exit(f"error: manifest {manifest_path} is not valid JSON: {exc}")

    base_dir = manifest_path.parent
    component_name = manifest.get("top_component", {}).get("name")
    verilog_candidates: list[Path] = []

    for entry in manifest.get("files", []):
        name = entry.get("name")
        if isinstance(name, str) and name.endswith(".v"):
            verilog_candidates.append(base_dir / name)

    if not verilog_candidates:
        verilog_candidates = list(base_dir.glob("*.v"))

    if not verilog_candidates:
        sys.exit(f"error: no Verilog sources listed in manifest {manifest_path}")

    if len(verilog_candidates) > 1:
        preferred = [p for p in verilog_candidates if component_name and p.stem == component_name]
        if preferred:
            verilog = preferred[0]
        else:
            sys.exit(
                f"error: manifest {manifest_path} refers to multiple .v files; "
                "specify one explicitly"
            )
    else:
        verilog = verilog_candidates[0]

    top = component_name or infer_top_from_verilog(verilog)
    if top is None:
        sys.exit(f"error: unable to determine top module for manifest {manifest_path}")

    label = base_dir.name
    return SynthTarget(label=label, verilog=verilog.resolve(), top=str(top), manifest=manifest_path)


def infer_top_from_verilog(verilog: Path) -> str | None:
    try:
        with verilog.open("r", encoding="utf-8") as handle:
            for line in handle:
                match = MODULE_PATTERN.match(line)
                if match:
                    return match.group(1)
    except OSError:
        return None
    return None


def build_yosys_commands(target: SynthTarget, conf: SynthConfig, netlist_path: Path) -> list[str]:
    liberty = shlex.quote(str(conf.liberty))
    verilog = shlex.quote(str(target.verilog))
    top = shlex.quote(target.top)
    netlist = shlex.quote(str(netlist_path))

    base_commands = [
        f"read_verilog {verilog}",
        f"hierarchy -check -top {top}",
        f"synth -top {top}",
        "opt -purge",
        f"dfflibmap -liberty {liberty}",
    ]

    if conf.abc_script:
        script = shlex.quote(str(conf.abc_script))
        base_commands.append(f"abc -liberty {liberty} -script {script}")
    else:
        base_commands.append(f"abc -liberty {liberty}")

    base_commands.extend(
        [
            "clean",
            f"write_verilog -noattr {netlist}",
            f"stat -liberty {liberty}",
        ]
    )

    return base_commands


def run_yosys(commands: Iterable[str]) -> subprocess.CompletedProcess[str]:
    yosys_script = "; ".join(commands)
    cmd = ["yosys", "-p", yosys_script]
    return subprocess.run(
        cmd,
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        check=False,
    )


def summarise_output(text: str) -> tuple[str | None, float | None]:
    cell_summary = None
    chip_area = None
    for line in reversed(text.splitlines()):
        if chip_area is None and "Chip area for module" in line:
            parts = line.split(":")
            if len(parts) == 2:
                value = parts[1].strip()
                try:
                    chip_area = float(value)
                except ValueError:
                    chip_area = None
        if cell_summary is None and line.strip().startswith("Number of cells:"):
            cell_summary = line.strip()
        if cell_summary and chip_area is not None:
            break
    return cell_summary, chip_area


def synthesise_target(target: SynthTarget, conf: SynthConfig) -> None:
    workspace = target.output_root(conf.output_root)
    netlist_dir = workspace / "netlist"
    report_dir = workspace / "reports"
    netlist_dir.mkdir(parents=True, exist_ok=True)
    report_dir.mkdir(parents=True, exist_ok=True)

    netlist_path = netlist_dir / f"{target.top}.mapped.v"
    report_path = report_dir / "yosys.log"

    commands = build_yosys_commands(target, conf, netlist_path)

    print(f"[synth] {target.label} → top={target.top}")
    if conf.dry_run:
        print("  (dry-run) yosys -p")
        for command in commands:
            print(f"    {command}")
        return

    result = run_yosys(commands)
    output = f"{result.stdout}{result.stderr}"
    report_path.write_text(output, encoding="utf-8")

    if result.returncode != 0:
        print(output)
        sys.exit(f"error: yosys exited with code {result.returncode} for target {target.label}")

    cell_line, area_value = summarise_output(output)
    rel_netlist = netlist_path.relative_to(PROJECT_ROOT)
    rel_report = report_path.relative_to(PROJECT_ROOT)

    print(f"  ✓ netlist: {rel_netlist}")
    print(f"  ↳ report : {rel_report}")
    if cell_line:
        print(f"  {cell_line}")
    if area_value is not None:
        print(f"  Chip area : {area_value:.3f}")
    print()


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run Yosys on Clash-generated Verilog artefacts.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "targets",
        nargs="*",
        help="Targets to synthesise: directory under ./verilog, manifest.json, or .v file",
    )
    parser.add_argument(
        "--top",
        help="Override the top module (only valid with a single explicit Verilog file).",
    )
    parser.add_argument(
        "--liberty",
        help="Path to the liberty file.",
    )
    parser.add_argument(
        "--out",
        default=str(DEFAULT_OUTPUT_ROOT),
        help="Directory to store synthesis outputs.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the Yosys commands without running them.",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str]) -> None:
    args = parse_args(argv)

    liberty = resolve_liberty(args.liberty) if not args.dry_run else Path(args.liberty or "NangateOpenCellLibrary_typical.lib")
    abc_script = resolve_abc_script()
    output_root = Path(args.out).expanduser().resolve()

    conf = SynthConfig(
        liberty=liberty,
        output_root=output_root,
        abc_script=abc_script,
        dry_run=args.dry_run,
    )

    if not conf.dry_run:
        ensure_tool_available("yosys")

    targets = discover_targets(args.targets, args.top)
    for target in targets:
        synthesise_target(target, conf)


if __name__ == "__main__":
    import shutil  # placed here to keep import ordering near usage

    main(sys.argv[1:])
