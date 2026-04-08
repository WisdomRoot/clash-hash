#!/usr/bin/env python3
"""Tests for STA output path resolution."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
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
sta = load_module("sta_under_test", SCRIPTS_DIR / "sta.py")


class StaPathTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.patches = [
            mock.patch.object(sta, "PROJECT_ROOT", self.root),
            mock.patch.object(sta, "BUILD_DIR", self.root / "build"),
            mock.patch.object(sta, "CLASH_TARGETS_FILE", self.root / "clash.json"),
            mock.patch.object(sta, "VHDL_TARGETS_FILE", self.root / "vhdl.json"),
        ]
        for patcher in self.patches:
            patcher.start()
        manifest_dir = self.root / "systemverilog" / "Component.G"
        manifest_dir.mkdir(parents=True, exist_ok=True)
        (manifest_dir / "dut.sv").write_text("// dut\n", encoding="utf-8")
        (manifest_dir / "dut.sdc").write_text("create_clock -name CLK -period 10.0 [get_ports CLK]\n", encoding="utf-8")
        (self.root / "build" / "synth" / "Component.G" / "netlist").mkdir(parents=True, exist_ok=True)
        (self.root / "build" / "synth" / "Component.G" / "netlist" / "dut.mapped.v").write_text("// netlist\n", encoding="utf-8")
        (manifest_dir / "clash-manifest.json").write_text(
            '{"top_component":{"name":"dut"},"files":[{"name":"dut.sv"}],"dependencies":{"transitive":[]}}',
            encoding="utf-8",
        )
        (self.root / "clash.json").write_text('{"G":"Component.G"}', encoding="utf-8")

    def tearDown(self):
        for patcher in reversed(self.patches):
            patcher.stop()
        self.tmp.cleanup()

    def test_output_dir_is_target_specific(self):
        out = sta._setup_paths("nangate45", "G")
        self.assertEqual(out["output"], self.root / "build" / "sta" / "Component.G" / "dut")
        self.assertTrue((self.root / "build" / "sta" / "Component.G" / "dut" / "reports" / "timing").is_dir())
        self.assertTrue((self.root / "build" / "sta" / "Component.G" / "dut" / "logs").is_dir())

    def test_clash_sdc_clock_period_is_normalised(self):
        out = sta._setup_paths("nangate45", "G")
        self.assertIn("-period 5.000", out["sdc"].read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
