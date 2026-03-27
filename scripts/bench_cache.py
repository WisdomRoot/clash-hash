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
    path.write_text(json.dumps(data, indent=2, sort_keys=True), encoding="utf-8")


def _stage_artifacts_exist(stage: dict[str, Any] | None) -> bool:
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


def _cache_stage_reusable(current_stage: dict[str, Any] | None, cached_stage: Any) -> bool:
    if not isinstance(current_stage, dict):
        return False
    if not isinstance(cached_stage, dict):
        return False
    current_key = current_stage.get("key")
    cached_key = cached_stage.get("key")
    if not isinstance(current_key, str) or not current_key:
        return False
    if not isinstance(cached_key, str) or not cached_key:
        return False
    if cached_stage.get("success") is not True:
        return False
    if current_key != cached_key:
        return False
    return _stage_artifacts_exist(current_stage)


def compute_stage_plan(current: dict[str, Any], cache: dict[str, Any] | None) -> dict[str, str]:
    """Compute whether each stage should run or can be reused from cache.

    The returned dict has keys `hdl`, `synth`, `sta`, each with value:
    - `run`
    - `cached`
    """
    current_stages = current.get("stages") if isinstance(current, dict) else None
    cached_stages = cache.get("stages") if isinstance(cache, dict) else None
    if not isinstance(current_stages, dict):
        return {stage: "run" for stage in STAGES}
    if not isinstance(cached_stages, dict):
        return {stage: "run" for stage in STAGES}

    plan: dict[str, str] = {}
    invalidate_downstream = False
    for stage_name in STAGES:
        current_stage = current_stages.get(stage_name)
        cached_stage = cached_stages.get(stage_name)
        reusable = (not invalidate_downstream) and _cache_stage_reusable(current_stage, cached_stage)
        if reusable:
            plan[stage_name] = "cached"
        else:
            plan[stage_name] = "run"
            invalidate_downstream = True

    return plan
