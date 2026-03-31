#!/usr/bin/env python3
"""TDD tests for planned bench caching behavior.

Run with:

    python3 scripts/test_bench_cache.py
"""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Could not import {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


cache_mod = load_module("bench_cache_under_test", Path(__file__).with_name("bench_cache.py"))


class BenchCacheBase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def require_fn(self, name: str):
        fn = getattr(cache_mod, name, None)
        self.assertIsNotNone(
            fn,
            f"scripts/bench_cache.py is expected to provide {name}",
        )
        return fn

    def artifact(self, rel: str, exists: bool = True) -> str:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        if exists:
            path.write_text(rel, encoding="utf-8")
        return str(path)

    def current_state(self, target: str = "SN-O24-L4", top: str = "dut"):
        return {
            "target": target,
            "top": top,
            "stages": {
                "hdl": {
                    "key": "hdl-key-v1",
                    "artifacts": [
                        self.artifact(f"systemverilog/{target}/clash-manifest.json"),
                        self.artifact(f"systemverilog/{target}/{top}.sv"),
                    ],
                },
                "synth": {
                    "key": "synth-key-v1",
                    "artifacts": [
                        self.artifact(f"build/synth/{target}/netlist/{top}.mapped.v"),
                        self.artifact(f"build/synth/{target}/reports/yosys.log"),
                    ],
                },
                "sta": {
                    "key": "sta-key-v1",
                    "artifacts": [
                        self.artifact(f"build/sta/{target}/{top}/reports/summary.rpt"),
                    ],
                },
            },
        }

    def cache_state(self):
        return {
            "target": "SN-O24-L4",
            "top": "dut",
            "stages": {
                "hdl": {"key": "hdl-key-v1", "success": True},
                "synth": {"key": "synth-key-v1", "success": True},
                "sta": {"key": "sta-key-v1", "success": True},
            },
        }

    def compute_plan(self, current, cache):
        fn = self.require_fn("compute_stage_plan")
        return fn(current, cache)

    def normalize_cache(self, cache):
        fn = self.require_fn("normalize_cache")
        return fn(cache)

    def normalize_current(self, current):
        fn = self.require_fn("normalize_current")
        return fn(current)

    def stage_artifacts_valid(self, stage):
        fn = self.require_fn("stage_artifacts_valid")
        return fn(stage)

    def stage_metadata_valid(self, stage):
        fn = self.require_fn("stage_metadata_valid")
        return fn(stage)

    def cache_stage_reusable(self, current_stage, cached_stage):
        fn = self.require_fn("cache_stage_reusable")
        return fn(current_stage, cached_stage)

    def load_cache(self, path: Path):
        fn = self.require_fn("load_cache")
        return fn(path)

    def save_cache(self, path: Path, data):
        fn = self.require_fn("save_cache")
        return fn(path, data)

    def assert_plan(self, plan, hdl: str, synth: str, sta: str):
        self.assertEqual(plan["hdl"], hdl)
        self.assertEqual(plan["synth"], synth)
        self.assertEqual(plan["sta"], sta)


class BenchCachePlanTests(BenchCacheBase):
    def test_no_cache_means_all_run(self):
        plan = self.compute_plan(self.current_state(), None)
        self.assert_plan(plan, "run", "run", "run")

    def test_all_matching_keys_and_artifacts_means_all_cached(self):
        plan = self.compute_plan(self.current_state(), self.cache_state())
        self.assert_plan(plan, "cached", "cached", "cached")

    def test_hdl_key_change_invalidates_all_downstream(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["hdl"]["key"] = "hdl-key-v2"
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "run", "run", "run")

    def test_synth_key_change_invalidates_synth_and_sta_only(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["synth"]["key"] = "synth-key-v2"
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_sta_key_change_invalidates_sta_only(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["sta"]["key"] = "sta-key-v2"
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_missing_hdl_artifact_invalidates_everything(self):
        current = self.current_state()
        cache = self.cache_state()
        Path(current["stages"]["hdl"]["artifacts"][0]).unlink()
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "run", "run", "run")

    def test_missing_synth_artifact_invalidates_synth_and_sta(self):
        current = self.current_state()
        cache = self.cache_state()
        Path(current["stages"]["synth"]["artifacts"][0]).unlink()
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_missing_sta_artifact_invalidates_sta_only(self):
        current = self.current_state()
        cache = self.cache_state()
        Path(current["stages"]["sta"]["artifacts"][0]).unlink()
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_failed_cached_hdl_stage_is_not_reused(self):
        current = self.current_state()
        cache = self.cache_state()
        cache["stages"]["hdl"]["success"] = False
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "run", "run", "run")

    def test_failed_cached_synth_stage_is_not_reused(self):
        current = self.current_state()
        cache = self.cache_state()
        cache["stages"]["synth"]["success"] = False
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_failed_cached_sta_stage_is_not_reused(self):
        current = self.current_state()
        cache = self.cache_state()
        cache["stages"]["sta"]["success"] = False
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_missing_stage_entry_invalidates_that_stage_and_downstream(self):
        current = self.current_state()
        cache = self.cache_state()
        del cache["stages"]["synth"]
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_missing_stage_key_invalidates_that_stage_and_downstream(self):
        current = self.current_state()
        cache = self.cache_state()
        del cache["stages"]["sta"]["key"]
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_empty_artifact_list_invalidates_stage_and_downstream(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["synth"]["artifacts"] = []
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_directory_instead_of_file_invalidates_stage_and_downstream(self):
        current = self.current_state()
        cache = self.cache_state()
        p = Path(current["stages"]["sta"]["artifacts"][0])
        p.unlink()
        p.mkdir()
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_partial_cache_with_only_hdl_reuses_only_hdl(self):
        current = self.current_state()
        cache = {"stages": {"hdl": {"key": "hdl-key-v1", "success": True}}}
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "run", "run")

    def test_cache_with_unknown_fields_is_ignored_safely(self):
        current = self.current_state()
        cache = self.cache_state()
        cache["junk"] = {"hello": "world"}
        cache["stages"]["hdl"]["extra"] = 123
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "cached")

    def test_non_dict_stage_entry_is_treated_as_missing(self):
        current = self.current_state()
        cache = self.cache_state()
        cache["stages"]["sta"] = "bad"
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "run")

    def test_stage_dag_monotonicity_for_hdl_run(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["hdl"]["key"] = "hdl-key-v2"
        plan = self.compute_plan(current, cache)
        self.assertEqual(plan["hdl"], "run")
        self.assertEqual(plan["synth"], "run")
        self.assertEqual(plan["sta"], "run")

    def test_stage_dag_monotonicity_for_synth_run(self):
        current = self.current_state()
        cache = self.cache_state()
        current["stages"]["synth"]["key"] = "synth-key-v2"
        plan = self.compute_plan(current, cache)
        self.assertEqual(plan["synth"], "run")
        self.assertEqual(plan["sta"], "run")


class BenchCacheStoreTests(BenchCacheBase):
    def test_save_then_load_roundtrips(self):
        path = self.root / "build" / "cache" / "SN-O24-L4.json"
        data = self.cache_state()
        self.save_cache(path, data)
        loaded = self.load_cache(path)
        self.assertEqual(loaded, data)

    def test_save_cache_is_atomic_and_leaves_no_tmp_file(self):
        path = self.root / "build" / "cache" / "SN-O24-L4.json"
        data = self.cache_state()
        self.save_cache(path, data)
        self.assertTrue(path.is_file())
        self.assertFalse(path.with_name(f"{path.name}.tmp").exists())

    def test_load_missing_cache_returns_none(self):
        path = self.root / "build" / "cache" / "missing.json"
        loaded = self.load_cache(path)
        self.assertIsNone(loaded)

    def test_load_malformed_json_returns_none(self):
        path = self.root / "build" / "cache" / "broken.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("{not valid json", encoding="utf-8")
        loaded = self.load_cache(path)
        self.assertIsNone(loaded)

    def test_load_non_object_json_returns_none(self):
        path = self.root / "build" / "cache" / "bad.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(["not", "an", "object"]), encoding="utf-8")
        loaded = self.load_cache(path)
        self.assertIsNone(loaded)


class BenchCacheHelperTests(BenchCacheBase):
    def test_normalize_cache_none_creates_empty_stage_dicts(self):
        cache = self.normalize_cache(None)
        self.assertEqual(set(cache["stages"].keys()), {"hdl", "synth", "sta"})
        self.assertEqual(cache["stages"]["hdl"], {})

    def test_normalize_cache_preserves_unknown_top_level_fields(self):
        cache = self.normalize_cache({"foo": 1, "stages": {"hdl": {"key": "x"}}})
        self.assertEqual(cache["foo"], 1)
        self.assertEqual(cache["stages"]["hdl"]["key"], "x")
        self.assertEqual(cache["stages"]["synth"], {})

    def test_normalize_current_none_creates_empty_stage_dicts(self):
        current = self.normalize_current(None)
        self.assertEqual(set(current["stages"].keys()), {"hdl", "synth", "sta"})
        self.assertEqual(current["stages"]["sta"], {})

    def test_stage_artifacts_valid_accepts_existing_files(self):
        stage = {"artifacts": [self.artifact("a"), self.artifact("b")]}
        self.assertTrue(self.stage_artifacts_valid(stage))

    def test_stage_artifacts_valid_rejects_missing_file(self):
        stage = {"artifacts": [str(self.root / "missing")]}
        self.assertFalse(self.stage_artifacts_valid(stage))

    def test_stage_artifacts_valid_rejects_directory(self):
        p = self.root / "dir"
        p.mkdir(parents=True)
        stage = {"artifacts": [str(p)]}
        self.assertFalse(self.stage_artifacts_valid(stage))

    def test_stage_metadata_valid_requires_nonempty_string_key(self):
        self.assertTrue(self.stage_metadata_valid({"key": "x"}))
        self.assertFalse(self.stage_metadata_valid({"key": ""}))
        self.assertFalse(self.stage_metadata_valid({"key": None}))
        self.assertFalse(self.stage_metadata_valid({}))

    def test_cache_stage_reusable_true_on_match_success_and_artifacts(self):
        current = {"key": "k1", "artifacts": [self.artifact("ok")]}
        cached = {"key": "k1", "success": True}
        self.assertTrue(self.cache_stage_reusable(current, cached))

    def test_cache_stage_reusable_false_when_key_mismatch(self):
        current = {"key": "k1", "artifacts": [self.artifact("ok")]}
        cached = {"key": "k2", "success": True}
        self.assertFalse(self.cache_stage_reusable(current, cached))

    def test_cache_stage_reusable_false_when_success_missing(self):
        current = {"key": "k1", "artifacts": [self.artifact("ok")]}
        cached = {"key": "k1"}
        self.assertFalse(self.cache_stage_reusable(current, cached))


class BenchCacheLifecycleTests(BenchCacheBase):
    def test_full_lifecycle_no_cache_then_cached_second_run(self):
        current = self.current_state()
        cache_path = self.root / "build" / "cache" / "SN-O24-L4.json"

        first_plan = self.compute_plan(current, None)
        self.assert_plan(first_plan, "run", "run", "run")

        cache = self.cache_state()
        self.save_cache(cache_path, cache)
        loaded = self.load_cache(cache_path)

        second_plan = self.compute_plan(current, loaded)
        self.assert_plan(second_plan, "cached", "cached", "cached")

    def test_lifecycle_synth_change_after_cached_run(self):
        current = self.current_state()
        cache_path = self.root / "build" / "cache" / "SN-O24-L4.json"

        self.save_cache(cache_path, self.cache_state())
        loaded = self.load_cache(cache_path)
        current["stages"]["synth"]["key"] = "synth-key-v2"

        plan = self.compute_plan(current, loaded)
        self.assert_plan(plan, "cached", "run", "run")

    def test_vhdl_like_target_shape_uses_same_stage_dag(self):
        current = self.current_state(target="vhdl_Foo", top="FooTop")
        cache = {
            "target": "vhdl_Foo",
            "top": "FooTop",
            "stages": {
                "hdl": {"key": "hdl-key-v1", "success": True},
                "synth": {"key": "synth-key-v1", "success": True},
                "sta": {"key": "sta-key-v1", "success": True},
            },
        }
        plan = self.compute_plan(current, cache)
        self.assert_plan(plan, "cached", "cached", "cached")


if __name__ == "__main__":
    unittest.main()
