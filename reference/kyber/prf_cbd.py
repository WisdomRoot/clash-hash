#!/usr/bin/env python3
"""
External reference for SamplePolyCBD ∘ PRF using kyber-py (FIPS 203).

Input: 34 raw bytes on stdin:
  - eta (1 byte, value 2 or 3)
  - s   (32 bytes)
  - b   (1 byte)

Output: 512 raw bytes on stdout (256 coefficients, uint16 little-endian).
"""

import os
import struct
import sys

# Get kyber-py path from environment, with fallback
kyber_py_path = os.environ.get("KYBER_PY_PATH", "/Users/banacorn/work/kyber-py/src")
sys.path.insert(0, kyber_py_path)

from kyber_py.ml_kem.ml_kem import ML_KEM
from kyber_py.polynomials.polynomials import PolynomialRing


def prf_cbd(eta: int, s: bytes, b: bytes) -> bytes:
    prf_out = ML_KEM._prf(eta, s, b)
    poly = PolynomialRing().cbd(prf_out, eta)
    return b"".join(struct.pack("<H", c) for c in poly.coeffs)


def main() -> None:
    data = sys.stdin.buffer.read(34)
    if len(data) != 34:
        raise SystemExit("Expected 34 bytes on stdin: eta (1) + s (32) + b (1).")

    eta = data[0]
    if eta not in (2, 3):
        raise SystemExit("eta must be 2 or 3.")

    s = data[1:33]
    b = data[33:34]

    out = prf_cbd(eta, s, b)
    sys.stdout.buffer.write(out)


if __name__ == "__main__":
    main()
