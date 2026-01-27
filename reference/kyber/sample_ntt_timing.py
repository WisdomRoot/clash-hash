#!/usr/bin/env python3
"""
Returns timing info for SampleNTT: which coefficients pass rejection sampling.
Input: 34 bytes on stdin
Output: JSON with validity pattern

This script implements the same coefficient extraction as FIPS 203 Algorithm 6,
allowing us to predict exactly how many squeeze blocks the hardware will need.
"""
import sys
import json
from hashlib import shake_128


def get_timing_info(input_bytes):
    """
    Compute timing information for SampleNTT hardware simulation.

    Returns the validity pattern (which coefficients pass rejection < 3329),
    which determines exactly how many squeeze blocks and cycles are needed.
    """
    # SHAKE-128 XOF - 840 bytes covers worst case (5 squeeze blocks)
    # Each squeeze block produces 168 bytes = 112 coefficients
    xof = shake_128(input_bytes).digest(840)

    validity_pattern = []
    valid_count = 0
    i = 0

    # Extract 12-bit coefficients per FIPS 203 Algorithm 6
    # Every 3 bytes → 2 coefficients
    while valid_count < 256:
        b0, b1, b2 = xof[i], xof[i + 1], xof[i + 2]

        # d1 = b0 + 256*(b1 mod 16)
        d1 = b0 + 256 * (b1 % 16)
        validity_pattern.append(d1 < 3329)
        if d1 < 3329:
            valid_count += 1

        if valid_count < 256:
            # d2 = floor(b1/16) + 16*b2
            d2 = (b1 // 16) + 16 * b2
            validity_pattern.append(d2 < 3329)
            if d2 < 3329:
                valid_count += 1

        i += 3

    return validity_pattern


if __name__ == "__main__":
    input_bytes = sys.stdin.buffer.read(34)
    validity_pattern = get_timing_info(input_bytes)
    print(json.dumps(validity_pattern))
