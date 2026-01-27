#include <cstdlib>

#include <verilated.h>

#include "VSHAKE_128_NonPipelined.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSHAKE_128_NonPipelined *top = new VSHAKE_128_NonPipelined;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

