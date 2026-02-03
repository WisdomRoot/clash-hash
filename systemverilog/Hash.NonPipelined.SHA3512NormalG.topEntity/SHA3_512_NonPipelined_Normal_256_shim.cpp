#include <cstdlib>

#include <verilated.h>

#include "VSHA3_512_NonPipelined_Normal_256.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSHA3_512_NonPipelined_Normal_256 *top = new VSHA3_512_NonPipelined_Normal_256;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

