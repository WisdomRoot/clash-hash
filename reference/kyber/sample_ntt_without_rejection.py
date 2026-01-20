#!/usr/bin/env python3
"""
Temporary development CLI for SHAKE128 testing.

Usage: python3 sample_ntt_without_rejection.py <68-char-hex-string>
Output: First 3 bytes of SHAKE128 output (raw bytes to stdout)
"""
import sys
import os

# Ensure we can import sample_ntt from the same directory
script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, script_dir)

from sample_ntt import shake128_xof, sample_ntt

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 sample_ntt_without_rejection.py <68-char-hex-string>", file=sys.stderr)
        print("Example: python3 sample_ntt_without_rejection.py " + "00" * 34, file=sys.stderr)
        sys.exit(1)

    hex_string = sys.argv[1]

    # Validate hex string length (68 chars = 34 bytes)
    if len(hex_string) != 68:
        print(f"Error: Expected 68 hex characters (34 bytes), got {len(hex_string)}", file=sys.stderr)
        sys.exit(1)

    # Convert hex string to bytes
    try:
        input_bytes = bytes.fromhex(hex_string)
    except ValueError as e:
        print(f"Error: Invalid hex string: {e}", file=sys.stderr)
        sys.exit(1)

    # Get SHAKE128 output (reusing function from sample_ntt.py)
    xof_bytes = shake128_xof(input_bytes)

    # Extract first 10 12-bit chunks from raw SHAKE128 (no rejection sampling)
    raw_chunks = []
    for i in range(15):  # Need 15 bytes for 10 chunks (3 bytes per 2 chunks)
        if i * 3 + 2 >= len(xof_bytes):
            break
        b0 = xof_bytes[i * 3]
        b1 = xof_bytes[i * 3 + 1]
        b2 = xof_bytes[i * 3 + 2]

        # Extract two 12-bit values (little-endian, as per Kyber spec)
        d1 = b0 + 256 * (b1 & 0x0F)
        d2 = (b1 >> 4) + 16 * b2
        raw_chunks.extend([d1, d2])

        if len(raw_chunks) >= 10:
            break

    # Get first 10 coefficients from reference implementation (with rejection sampling)
    output_bytes = sample_ntt(input_bytes)

    # Unpack first 10 coefficients from packed output
    sampled_coeffs = []
    for i in range(15):  # Need up to 15 bytes for 10 coefficients
        if i * 3 + 2 >= len(output_bytes):
            break
        b0 = output_bytes[i * 3]
        b1 = output_bytes[i * 3 + 1]
        b2 = output_bytes[i * 3 + 2]

        c0 = b0 + 256 * (b1 & 0x0F)
        c1 = (b1 >> 4) + 16 * b2
        sampled_coeffs.extend([c0, c1])

        if len(sampled_coeffs) >= 10:
            break

    # Print comparison
    print("SHAKE128 raw (first 10 12-bit chunks):")
    print(" ".join(f"{c:4d}" for c in raw_chunks[:10]))
    print()
    print("After rejection sampling (first 10 coefficients < 3329):")
    print(" ".join(f"{c:4d}" for c in sampled_coeffs[:10]))
    print()
    print("Rejection sampling visualization:")
    for i, raw in enumerate(raw_chunks[:10]):
        if i < len(sampled_coeffs):
            if raw < 3329:
                status = "✓ kept"
            else:
                status = "✗ rejected (>= 3329)"
            print(f"  Position {i}: {raw:4d} -> {status}")
        else:
            break

if __name__ == "__main__":
    main()
