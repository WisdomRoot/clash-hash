#include <cstdlib>

#include <verilated.h>

#include "VSHA3_256_NonPipelined.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSHA3_256_NonPipelined *top = new VSHA3_256_NonPipelined;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

