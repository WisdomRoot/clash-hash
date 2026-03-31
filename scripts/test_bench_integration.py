#!/usr/bin/env python3
"""Integration-style tests for bench cache orchestration."""

from __future__ import annotations

import importlib.util
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Could not import {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


SCRIPTS_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS_DIR))
bench = load_module("bench_under_test", SCRIPTS_DIR / "bench.py")


class BenchIntegrationTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.target = "Demo"
        self.label = "Component.Demo"
        self.top = "dut"
        self.stack_artifact = self.root / ".stack-work" / "dist" / "ghc-9.6.6" / "build" / "libHSclash-hash-test.a"
        self.stack_artifact.parent.mkdir(parents=True, exist_ok=True)
        self.stack_artifact.write_text("stack", encoding="utf-8")
        self.liberty = self.root / "lib" / "nangate45" / "NangateOpenCellLibrary_typical.lib"
        self.liberty.parent.mkdir(parents=True, exist_ok=True)
        self.liberty.write_text("lib", encoding="utf-8")

        self.calls = {"hdl": 0, "synth": 0, "sta": 0, "stack": 0}

        self.patchers = [
            mock.patch.object(bench, "PROJECT_ROOT", self.root),
            mock.patch.object(bench, "SYSTEMVERILOG_ROOT", self.root / "systemverilog"),
            mock.patch.object(bench, "VERILOG_ROOT", self.root / "verilog"),
            mock.patch.object(bench, "CLASH_HDL_ROOTS", [self.root / "systemverilog", self.root / "verilog"]),
            mock.patch.object(bench, "CACHE_ROOT", self.root / "build" / "cache"),
            mock.patch.object(bench, "LIBERTY_FILE", self.liberty),
            mock.patch.object(bench, "ALIASES", {self.target: self.label}),
            mock.patch.object(bench, "VHDL_TARGETS", {}),
            mock.patch.object(bench, "collect_stack_build_artifacts", side_effect=lambda: [self.stack_artifact]),
            mock.patch.object(bench, "run_cmd", side_effect=self.fake_run_cmd),
            mock.patch.object(bench, "run_hdl", side_effect=self.fake_run_hdl),
            mock.patch.object(bench, "run_synth", side_effect=self.fake_run_synth),
            mock.patch.object(bench, "run_sta", side_effect=self.fake_run_sta),
        ]
        for patcher in self.patchers:
            patcher.start()

    def tearDown(self):
        for patcher in reversed(self.patchers):
            patcher.stop()
        self.tmp.cleanup()

    def fake_run_cmd(self, cmd, label, timeout=3600):
        if cmd[:3] == ["stack", "build", "clash-hash:lib"]:
            self.calls["stack"] += 1
        return ""

    def write_manifest(self, root: Path, ext: str, top_file: str) -> None:
        out_dir = root / self.label
        out_dir.mkdir(parents=True, exist_ok=True)
        (out_dir / top_file).write_text(f"// {top_file}\n", encoding="utf-8")
        (out_dir / f"{self.top}.sdc").write_text("create_clock -name CLK -period 10.0 [get_ports CLK]\n", encoding="utf-8")
        manifest = {
            "top_component": {"name": self.top},
            "files": [{"name": top_file}],
            "dependencies": {"transitive": []},
        }
        (out_dir / "clash-manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    def fake_run_hdl(self, target, module_name, main_is):
        self.calls["hdl"] += 1
        self.write_manifest(self.root / "systemverilog", "sv", f"{self.top}.sv")
        self.write_manifest(self.root / "verilog", "v", f"{self.top}.v")

    def synth_log_text(self) -> str:
        return (
            f"Chip area for top module '{self.top}': 123.456\n"
            "of which used for sequential elements: 10.000 (8.10%)\n"
            "CPU: user 1.23s system 0.11s\n"
            "MEM: 321.00 MB peak\n"
        )

    def fake_run_synth(self, target):
        self.calls["synth"] += 1
        out_dir = self.root / "build" / "synth" / self.label
        (out_dir / "netlist").mkdir(parents=True, exist_ok=True)
        (out_dir / "reports").mkdir(parents=True, exist_ok=True)
        (out_dir / "netlist" / f"{self.top}.mapped.v").write_text("// netlist\n", encoding="utf-8")
        (out_dir / "reports" / "yosys.log").write_text(self.synth_log_text(), encoding="utf-8")
        return bench.parse_synth_output(self.synth_log_text())

    def fake_run_sta(self, target):
        self.calls["sta"] += 1
        summary = (
            "Critical Path: 7.89 ns\n"
            "WNS (max): 1.230 ns\n"
            "TNS (max): 4.560 ns\n"
            "Worst Slack: 1.23 ns\n"
        )
        out_dir = self.root / "build" / "bench-sta" / self.label / "reports"
        out_dir.mkdir(parents=True, exist_ok=True)
        (out_dir / "summary.rpt").write_text(summary, encoding="utf-8")
        return bench.parse_sta_summary(summary)

    def run_bench(self) -> str:
        output = io.StringIO()
        with redirect_stdout(output):
            bench.bench(self.target)
        return output.getvalue()

    def test_bench_cache_lifecycle(self):
        first = self.run_bench()
        self.assertIn("build           run", first)
        self.assertIn("hdl             run", first)
        self.assertIn("synth           run", first)
        self.assertIn("sta             run", first)
        self.assertEqual(self.calls["stack"], 1)
        self.assertEqual(self.calls["hdl"], 1)
        self.assertEqual(self.calls["synth"], 1)
        self.assertEqual(self.calls["sta"], 1)

        second = self.run_bench()
        self.assertIn("build           run", second)
        self.assertIn("hdl             cached", second)
        self.assertIn("synth           cached", second)
        self.assertIn("sta             cached", second)
        self.assertEqual(self.calls["stack"], 2)
        self.assertEqual(self.calls["hdl"], 1)
        self.assertEqual(self.calls["synth"], 1)
        self.assertEqual(self.calls["sta"], 1)

        synth_report = self.root / "build" / "synth" / self.label / "reports" / "yosys.log"
        synth_report.unlink()

        third = self.run_bench()
        self.assertIn("build           run", third)
        self.assertIn("hdl             cached", third)
        self.assertIn("synth           run", third)
        self.assertIn("sta             run", third)
        self.assertEqual(self.calls["stack"], 3)
        self.assertEqual(self.calls["hdl"], 1)
        self.assertEqual(self.calls["synth"], 2)
        self.assertEqual(self.calls["sta"], 2)


if __name__ == "__main__":
    unittest.main()
