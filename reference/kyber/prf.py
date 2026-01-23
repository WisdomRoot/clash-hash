#!/usr/bin/env python3
"""
Minimal external reference for PRF using kyber-py (FIPS 203, Section 4.3).

Input: 34 raw bytes on stdin:
  - eta (1 byte, value 2 or 3)
  - s   (32 bytes)
  - b   (1 byte)

Output: 64 * eta raw bytes on stdout.
"""

import os
import sys

# Get kyber-py path from environment, with fallback
kyber_py_path = os.environ.get("KYBER_PY_PATH", "/Users/banacorn/work/kyber-py/src")
sys.path.insert(0, kyber_py_path)

from kyber_py.ml_kem.ml_kem import ML_KEM


def prf(eta: int, s: bytes, b: bytes) -> bytes:
    return ML_KEM._prf(eta, s, b)


def main() -> None:
    data = sys.stdin.buffer.read(34)
    if len(data) != 34:
        raise SystemExit("Expected 34 bytes on stdin: eta (1) + s (32) + b (1).")

    eta = data[0]
    if eta not in (2, 3):
        raise SystemExit("eta must be 2 or 3.")

    s = data[1:33]
    b = data[33:34]

    out = prf(eta, s, b)
    sys.stdout.buffer.write(out)


if __name__ == "__main__":
    main()
