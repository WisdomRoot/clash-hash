#!/usr/bin/env python3
"""Bench cache support.

Pure cache planning and cache-file helpers for `scripts/bench.py`.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

STAGES = ("hdl", "synth", "sta")


def load_cache(path: Path) -> dict[str, Any] | None:
    """Load a cache file.

    Returns `None` if the file is missing or malformed.
    """
    try:
        text = path.read_text(encoding="utf-8")
    except FileNotFoundError:
        return None
    except OSError:
        return None

    try:
        data = json.loads(text)
    except Exception:
        return None

    return data if isinstance(data, dict) else None


def save_cache(path: Path, data: dict[str, Any]) -> None:
    """Persist a cache file, creating parent directories as needed."""
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(f"{path.name}.tmp")
    tmp.write_text(json.dumps(data, indent=2, sort_keys=True), encoding="utf-8")
    tmp.replace(path)


def normalize_cache(cache: dict[str, Any] | None) -> dict[str, Any]:
    """Return a normalized cache shape.

    Unknown top-level fields are preserved. Missing or malformed stage entries are
    replaced with empty dicts so planner logic can treat them uniformly.
    """
    if not isinstance(cache, dict):
        return {"stages": {stage: {} for stage in STAGES}}

    result = dict(cache)
    stages_in = cache.get("stages")
    stages_out: dict[str, dict[str, Any]] = {}
    for stage in STAGES:
        if isinstance(stages_in, dict) and isinstance(stages_in.get(stage), dict):
            stages_out[stage] = dict(stages_in[stage])
        else:
            stages_out[stage] = {}
    result["stages"] = stages_out
    return result


def normalize_current(current: dict[str, Any] | None) -> dict[str, Any]:
    """Return a normalized current-state shape."""
    if not isinstance(current, dict):
        return {"stages": {stage: {} for stage in STAGES}}

    result = dict(current)
    stages_in = current.get("stages")
    stages_out: dict[str, dict[str, Any]] = {}
    for stage in STAGES:
        if isinstance(stages_in, dict) and isinstance(stages_in.get(stage), dict):
            stages_out[stage] = dict(stages_in[stage])
        else:
            stages_out[stage] = {}
    result["stages"] = stages_out
    return result


def stage_artifacts_valid(stage: dict[str, Any] | None) -> bool:
    if not isinstance(stage, dict):
        return False
    artifacts = stage.get("artifacts")
    if not isinstance(artifacts, list) or len(artifacts) == 0:
        return False
    for artifact in artifacts:
        if not isinstance(artifact, str) or not artifact:
            return False
        path = Path(artifact)
        if not path.is_file():
            return False
    return True


def stage_metadata_valid(stage: dict[str, Any] | None) -> bool:
    if not isinstance(stage, dict):
        return False
    key = stage.get("key")
    return isinstance(key, str) and bool(key)


def cache_stage_reusable(current_stage: dict[str, Any] | None, cached_stage: Any) -> bool:
    if not isinstance(current_stage, dict):
        return False
    if not isinstance(cached_stage, dict):
        return False
    if not stage_metadata_valid(current_stage):
        return False
    if not stage_metadata_valid(cached_stage):
        return False
    current_key = current_stage["key"]
    cached_key = cached_stage["key"]
    if cached_stage.get("success") is not True:
        return False
    if current_key != cached_key:
        return False
    return stage_artifacts_valid(current_stage)


def compute_stage_plan(current: dict[str, Any], cache: dict[str, Any] | None) -> dict[str, str]:
    """Compute whether each stage should run or can be reused from cache.

    The returned dict has keys `hdl`, `synth`, `sta`, each with value:
    - `run`
    - `cached`
    """
    current_norm = normalize_current(current)
    cache_norm = normalize_cache(cache)
    current_stages = current_norm["stages"]
    cached_stages = cache_norm["stages"]

    plan: dict[str, str] = {}
    invalidate_downstream = False
    for stage_name in STAGES:
        current_stage = current_stages.get(stage_name)
        cached_stage = cached_stages.get(stage_name)
        reusable = (not invalidate_downstream) and cache_stage_reusable(current_stage, cached_stage)
        if reusable:
            plan[stage_name] = "cached"
        else:
            plan[stage_name] = "run"
            invalidate_downstream = True

    return plan
