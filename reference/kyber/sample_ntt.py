#!/usr/bin/env python3
"""
Minimal external reference for SampleNTT using kyber-py.

Implements FIPS 203 Algorithm 6 (SampleNTT).
Input: 34 raw bytes on stdin (32-byte rho + i + j)
Output: JSON on stdout with packed coefficients and validity pattern.
"""
import sys
import os
import json
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


def compute_validity_pattern(xof_bytes):
    """
    Compute validity pattern for rejection sampling (< 3329).

    Returns a list of booleans indicating whether each candidate was accepted,
    extended with False entries to the next full XOF block boundary (112 candidates).
    This ensures the hardware simulation has a complete last block to process.
    """
    validity_pattern = []
    valid_count = 0
    i = 0

    # Extract 12-bit coefficients per FIPS 203 Algorithm 6
    # Every 3 bytes -> 2 candidates
    while valid_count < 256:
        b0, b1, b2 = xof_bytes[i], xof_bytes[i + 1], xof_bytes[i + 2]

        # d1 = b0 + 256*(b1 mod 16)
        d1 = b0 + 256 * (b1 & 0x0F)
        validity_pattern.append(d1 < 3329)
        if d1 < 3329:
            valid_count += 1

        if valid_count < 256:
            # d2 = floor(b1/16) + 16*b2
            d2 = (b1 >> 4) + 16 * b2
            validity_pattern.append(d2 < 3329)
            if d2 < 3329:
                valid_count += 1

        i += 3

    # Extend to the next full XOF block boundary (112 candidates per block).
    # The hardware always processes complete squeeze blocks, so the reference model
    # must predict a full block's worth of candidates after the 256th valid one.
    block_size = 112
    current_len = len(validity_pattern)
    next_boundary = ((current_len + block_size - 1) // block_size) * block_size
    max_candidates = (len(xof_bytes) // 3) * 2
    next_boundary = min(next_boundary, max_candidates)
    extra = next_boundary - current_len
    if extra > 0:
        validity_pattern.extend([False] * extra)

    return validity_pattern


def sample_ntt(input_bytes):
    """
    Perform SampleNTT: rejection sampling to get 256 coefficients.

    Args:
        input_bytes: 34 bytes (32-byte rho + i + j)

    Returns:
        (packed_bytes, validity_pattern)
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
        byte0 = c0 & 0xFF  # Lower 8 bits of c0
        byte1 = ((c0 >> 8) & 0x0F) | ((c1 & 0x0F) << 4)  # Upper 4 bits of c0, lower 4 bits of c1
        byte2 = (c1 >> 4) & 0xFF  # Upper 8 bits of c1

        result.extend([byte0, byte1, byte2])

    validity_pattern = compute_validity_pattern(xof_bytes)

    return bytes(result), validity_pattern


def main():
    """Main entry point for stdin/stdout usage."""
    # Read raw 34 bytes from stdin
    input_bytes = sys.stdin.buffer.read(34)

    # Perform SampleNTT
    output_bytes, validity_pattern = sample_ntt(input_bytes)

    # Write JSON to stdout
    payload = {
        "packed": list(output_bytes),
        "validity": validity_pattern,
    }
    print(json.dumps(payload))


if __name__ == "__main__":
    main()
