#!/usr/bin/env python3
"""
Minimal external reference for SampleNTT using kyber-py.

Implements FIPS 203 Algorithm 6 (SampleNTT).
Input: 34 raw bytes on stdin (32-byte rho + i + j)
Output: 512 raw bytes on stdout (256 coefficients × 2 bytes, little-endian)
"""
import sys
import os
from hashlib import shake_128

# Get kyber-py path from environment, with fallback
kyber_py_path = os.environ.get('KYBER_PY_PATH', '/Users/banacorn/work/kyber-py/src')
sys.path.insert(0, kyber_py_path)

from kyber_py.polynomials.polynomials import PolynomialRing

# Read raw 34 bytes from stdin
input_bytes = sys.stdin.buffer.read(34)

# SHAKE-128 XOF (840 bytes as per kyber-py spec)
xof_bytes = shake_128(input_bytes).digest(840)

# SampleNTT: Rejection sampling to get 256 coefficients
R = PolynomialRing()
poly_ntt = R.ntt_sample(xof_bytes)

# Output raw 512 bytes (256 coeffs × 2 bytes little-endian)
for coeff in poly_ntt.coeffs:
    sys.stdout.buffer.write(coeff.to_bytes(2, 'little'))
