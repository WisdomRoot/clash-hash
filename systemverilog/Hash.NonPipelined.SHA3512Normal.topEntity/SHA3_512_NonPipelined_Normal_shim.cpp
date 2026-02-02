#include <cstdlib>

#include <verilated.h>

#include "VSHA3_512_NonPipelined_Normal.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSHA3_512_NonPipelined_Normal *top = new VSHA3_512_NonPipelined_Normal;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

