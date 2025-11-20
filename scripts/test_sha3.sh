#!/bin/bash
# Test SHA3-256 core using Clash behavioral simulation
# DO NOT use synthesis tools for functional verification

set -e

echo "[test] Running SHA3-256 testbench in Clash simulation..."
echo "[test] This tests the design behavior WITHOUT synthesizing to HDL"
echo ""

# Run testbench in clashi for 100 cycles
stack exec clashi <<EOF
:m +KeccakF1600Testbench
putStrLn "Sampling testBench for 100 cycles..."
sampleN 100 testBench
:q
EOF

echo ""
echo "[test] Note: The testbench is incomplete and always returns False"
echo "[test] Next steps:"
echo "  1. Implement multi-cycle AXI output collection"
echo "  2. Add digest comparison logic"
echo "  3. Set done=True when verification passes"
