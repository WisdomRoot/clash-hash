#!/usr/bin/env python3
"""
Compute the M(n) Markov chain family, its stationary distribution, and emit rate.

Why this script exists
----------------------
SampleNTT lookahead variants are modeled with a finite-state Markov chain M(n),
where n is the number of screened candidates per screening step:
  - M(4): 6x6
  - M(6): 8x8
  - M(8): 10x10
  - in general: M(n) has size (n+2) x (n+2)

This script is the canonical calculator for:
  1) Prn(k) binomial probabilities
  2) M(n) construction
  3) row-stochastic validation
  4) stationary distribution pi (pi M = pi)
  5) emit-success rate r_n
  6) M(n)^t and distribution evolution mu_{t+1} = mu_t M(n)

Base probability model
----------------------
Given candidate-valid probability p and q = 1-p:

  Prn(k) = C(n, k) * p^k * q^(n-k),   k = 0..n

The default p matches ML-KEM rejection probability usage in this repo:
  p = 3329 / 4096

M(n) construction (row-stochastic)
----------------------------------
Let size = n+2, rows/cols indexed from 0:

  row 0 = [Prn(0), Prn(2), Prn(1), Prn(3), Prn(4), ..., Prn(n), 0]
  row 1 = same as row 0
  row 2 = [0, Prn(1), Prn(0), Prn(2), Prn(3), ..., Prn(n)]
  row 3 = same as row 2

Lower rows are deterministic shift rows:
  row 4 -> col 1
  row 5 -> col 3
  row r (>=6) -> col (r-2)

For n=6 this reproduces the exact 8x8 matrix discussed in design notes.

Stationary distribution and emit rate
-------------------------------------
The stationary row vector pi satisfies:
  pi M = pi
  sum(pi) = 1

By convention used in this repo for this chain family, per-cycle successful emit
rate is:
  r_n = 1 - pi[0] - pi[2]

This script prints r_n and E[cycles per emit] = 1 / r_n.

Examples
--------
  python3 scripts/markov_emit_rate.py --n 6 --print-pr
  python3 scripts/markov_emit_rate.py --n 8 --print-matrix
  python3 scripts/markov_emit_rate.py --n 6 --power 20
  python3 scripts/markov_emit_rate.py --n 6 --evolve 50 --init 1,0,0,0,0,0,0,0
"""

from __future__ import annotations

import argparse
import math
from fractions import Fraction
from typing import Iterable, List


Matrix = List[List[float]]
Vector = List[float]


def parse_probability(text: str) -> float:
    text = text.strip()
    if "/" in text:
        return float(Fraction(text))
    return float(text)


def pr(n: int, k: int, p: float) -> float:
    q = 1.0 - p
    return math.comb(n, k) * (p**k) * (q ** (n - k))


def build_m(n: int, p: float) -> Matrix:
    if n < 4:
        raise ValueError("n must be >= 4")

    size = n + 2
    prn = [pr(n, k, p) for k in range(n + 1)]
    m: Matrix = [[0.0 for _ in range(size)] for _ in range(size)]

    # row 0 / row 1:
    # [Prn(0), Prn(2), Prn(1), Prn(3), Prn(4), ..., Prn(n), 0]
    # The [0,2,1,3,...] ordering is intentional and part of the model definition.
    row0 = [0.0] * size
    row0[0] = prn[0]
    row0[1] = prn[2]
    row0[2] = prn[1]
    row0[3] = prn[3]
    for k in range(4, n + 1):
        row0[k] = prn[k]
    row0[size - 1] = 0.0
    m[0] = row0.copy()
    m[1] = row0.copy()

    # row 2 / row 3:
    # [0, Prn(1), Prn(0), Prn(2), Prn(3), ..., Prn(n)]
    row2 = [0.0] * size
    row2[0] = 0.0
    row2[1] = prn[1]
    row2[2] = prn[0]
    row2[3] = prn[2]
    for k in range(3, n + 1):
        row2[k + 1] = prn[k]
    m[2] = row2.copy()
    m[3] = row2.copy()

    # Deterministic lower rows.
    # row 4 -> col 1
    # row 5 -> col 3
    # row r>=6 -> col (r-2)
    m[4][1] = 1.0
    m[5][3] = 1.0
    for r in range(6, size):
        m[r][r - 2] = 1.0

    return m


def solve_linear_system(a: Matrix, b: Vector) -> Vector:
    n = len(a)
    aug = [row[:] + [b_i] for row, b_i in zip(a, b)]

    # Forward elimination with partial pivoting.
    for col in range(n):
        pivot_row = max(range(col, n), key=lambda r: abs(aug[r][col]))
        pivot_val = aug[pivot_row][col]
        if abs(pivot_val) < 1e-15:
            raise ValueError("singular matrix while solving linear system")
        if pivot_row != col:
            aug[col], aug[pivot_row] = aug[pivot_row], aug[col]

        # Normalize pivot row.
        inv = 1.0 / aug[col][col]
        for j in range(col, n + 1):
            aug[col][j] *= inv

        # Eliminate below.
        for r in range(col + 1, n):
            factor = aug[r][col]
            if factor == 0.0:
                continue
            for j in range(col, n + 1):
                aug[r][j] -= factor * aug[col][j]

    # Back substitution.
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        acc = aug[i][n]
        for j in range(i + 1, n):
            acc -= aug[i][j] * x[j]
        x[i] = acc / aug[i][i]

    return x


def stationary_distribution(pmat: Matrix) -> Vector:
    n = len(pmat)
    # Solve (P^T - I) * x = 0 with last row replaced by sum(x)=1.
    # This computes row-stationary pi such that pi P = pi.
    a: Matrix = [
        [pmat[row][col] - (1.0 if row == col else 0.0) for row in range(n)]
        for col in range(n)
    ]
    a[-1] = [1.0] * n
    b: Vector = [0.0] * n
    b[-1] = 1.0
    x = solve_linear_system(a, b)
    # Numerical cleanup.
    x = [0.0 if abs(v) < 1e-15 else v for v in x]
    s = sum(x)
    return [v / s for v in x]


def matmul(a: Matrix, b: Matrix) -> Matrix:
    n = len(a)
    out = [[0.0] * n for _ in range(n)]
    for i in range(n):
        for k in range(n):
            aik = a[i][k]
            if aik == 0.0:
                continue
            row_bk = b[k]
            row_out = out[i]
            for j in range(n):
                row_out[j] += aik * row_bk[j]
    return out


def matpow(m: Matrix, t: int) -> Matrix:
    n = len(m)
    out = [[0.0] * n for _ in range(n)]
    for i in range(n):
        out[i][i] = 1.0
    base = [row[:] for row in m]
    e = t
    while e > 0:
        if e & 1:
            out = matmul(out, base)
        base = matmul(base, base)
        e >>= 1
    return out


def evolve(mu: Vector, pmat: Matrix, steps: int) -> Vector:
    cur = mu[:]
    n = len(pmat)
    for _ in range(steps):
        nxt = [0.0] * n
        for i in range(n):
            mi = cur[i]
            if mi == 0.0:
                continue
            for j in range(n):
                nxt[j] += mi * pmat[i][j]
        cur = nxt
    return cur


def format_vec(v: Iterable[float], digits: int = 12) -> str:
    return "[" + ", ".join(f"{x:.{digits}f}" for x in v) + "]"


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Compute M(n), stationary pi, and emit rate for SampleNTT Markov model"
    )
    parser.add_argument("--n", type=int, default=6, help="M(n) parameter (default: 6)")
    parser.add_argument(
        "--p",
        type=str,
        default="3329/4096",
        help="success probability p, float or fraction (default: 3329/4096)",
    )
    parser.add_argument("--print-pr", action="store_true", help="print Prn(k)")
    parser.add_argument("--print-matrix", action="store_true", help="print M(n)")
    parser.add_argument("--power", type=int, default=None, help="print M(n)^t for t>=0")
    parser.add_argument(
        "--evolve",
        type=int,
        default=None,
        help="evolve default initial distribution for t steps (mu_{t+1}=mu_t M)",
    )
    parser.add_argument(
        "--init",
        type=str,
        default=None,
        help="comma-separated initial distribution for --evolve; default is state0 one-hot",
    )
    args = parser.parse_args()

    p = parse_probability(args.p)
    n = args.n
    size = n + 2

    prn = [pr(n, k, p) for k in range(n + 1)]
    m = build_m(n, p)

    # Validate row stochastic.
    row_sums = [sum(row) for row in m]
    max_row_err = max(abs(s - 1.0) for s in row_sums)

    pi = stationary_distribution(m)
    emit_rate = 1.0 - pi[0] - pi[2]

    print(f"n = {n}")
    print(f"p = {p:.15f}")
    print(f"q = {1.0 - p:.15f}")
    print(f"size = {size} x {size}")
    print(f"max row-sum error = {max_row_err:.3e}")
    print(f"stationary pi = {format_vec(pi)}")
    print(f"emit rate r_n = 1 - pi[0] - pi[2] = {emit_rate:.15f}")
    print(f"E[cycles per emit] = {1.0 / emit_rate:.15f}")

    if args.print_pr:
        print(f"Pr{n}(k):")
        for k, pk in enumerate(prn):
            print(f"  k={k}: {pk:.18f}")

    if args.print_matrix:
        print(f"M({n}):")
        for row in m:
            print("  " + format_vec(row, digits=18))

    if args.power is not None:
        if args.power < 0:
            raise ValueError("--power must be >= 0")
        mp = matpow(m, args.power)
        print(f"M({n})^{args.power}:")
        for row in mp:
            print("  " + format_vec(row, digits=18))

    if args.evolve is not None:
        t = args.evolve
        if t < 0:
            raise ValueError("--evolve must be >= 0")
        if args.init is None:
            mu0 = [0.0] * size
            mu0[0] = 1.0
        else:
            raw = [float(x.strip()) for x in args.init.split(",") if x.strip() != ""]
            if len(raw) != size:
                raise ValueError(f"--init must have {size} entries for M({n})")
            s = sum(raw)
            if s == 0.0:
                raise ValueError("--init must not sum to zero")
            mu0 = [x / s for x in raw]
        mut = evolve(mu0, m, t)
        print(f"mu0 = {format_vec(mu0)}")
        print(f"mu_{t} = {format_vec(mut)}")


if __name__ == "__main__":
    main()
