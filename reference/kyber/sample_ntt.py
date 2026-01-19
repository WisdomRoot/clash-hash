#!/usr/bin/env python3
"""
Minimal external reference for SampleNTT using kyber-py.

Implements FIPS 203 Algorithm 6 (SampleNTT).
Input: 34 raw bytes on stdin (32-byte rho + i + j)
Output: 384 raw bytes on stdout (256 coefficients × 12 bits, packed)
"""
import sys
import os
from hashlib import shake_128

# Get kyber-py path from environment, with fallback
kyber_py_path = os.environ.get('KYBER_PY_PATH', '/Users/banacorn/work/kyber-py/src')
sys.path.insert(0, kyber_py_path)

from kyber_py.polynomials.polynomials import PolynomialRing


def shake128_xof(input_bytes):
    """
    Apply SHAKE-128 XOF to input bytes.
    Returns 840 bytes as per kyber-py spec for SampleNTT.
    """
    return shake_128(input_bytes).digest(840)


def sample_ntt(input_bytes):
    """
    Perform SampleNTT: rejection sampling to get 256 coefficients.

    Args:
        input_bytes: 34 bytes (32-byte rho + i + j)

    Returns:
        384 bytes (256 coefficients × 12 bits, packed)
    """
    # SHAKE-128 XOF (840 bytes)
    xof_bytes = shake128_xof(input_bytes)

    # SampleNTT: Rejection sampling to get 256 coefficients
    R = PolynomialRing()
    poly_ntt = R.ntt_sample(xof_bytes)

    # Pack 256 coefficients as 12-bit values into 384 bytes (128 pairs × 3 bytes)
    result = bytearray()
    for i in range(0, 256, 2):
        c0 = poly_ntt.coeffs[i]
        c1 = poly_ntt.coeffs[i + 1]

        # Pack two 12-bit coefficients into 3 bytes
        byte0 = c0 & 0xFF                           # Lower 8 bits of c0
        byte1 = ((c0 >> 8) & 0x0F) | ((c1 & 0x0F) << 4)  # Upper 4 bits of c0, lower 4 bits of c1
        byte2 = (c1 >> 4) & 0xFF                    # Upper 8 bits of c1

        result.extend([byte0, byte1, byte2])

    return bytes(result)


def main():
    """Main entry point for stdin/stdout usage."""
    # Read raw 34 bytes from stdin
    input_bytes = sys.stdin.buffer.read(34)

    # Perform SampleNTT
    output_bytes = sample_ntt(input_bytes)

    # Write to stdout
    sys.stdout.buffer.write(output_bytes)


if __name__ == "__main__":
    main()
