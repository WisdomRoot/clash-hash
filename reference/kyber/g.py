#!/usr/bin/env python3
"""
External reference for G (SHA3-512 split) using kyber-py (FIPS 203).

G: B* → B^32 × B^32

Input: Variable-length raw bytes on stdin (typically 33 bytes: d (32) + k (1))

Output: 64 raw bytes on stdout (rho (32) + sigma (32))
"""

import os
import sys

# Get kyber-py path from environment, with fallback
kyber_py_path = os.environ.get("KYBER_PY_PATH", "/Users/banacorn/work/kyber-py/src")
sys.path.insert(0, kyber_py_path)

from kyber_py.ml_kem.ml_kem import ML_KEM


def g(input_bytes: bytes) -> bytes:
    """
    G: B* → B^32 × B^32
    SHA3-512, split into two 32-byte outputs.
    """
    rho, sigma = ML_KEM._G(input_bytes)
    return rho + sigma


def main() -> None:
    data = sys.stdin.buffer.read()
    if len(data) == 0:
        raise SystemExit("Expected input bytes on stdin.")

    out = g(data)
    sys.stdout.buffer.write(out)


if __name__ == "__main__":
    main()
