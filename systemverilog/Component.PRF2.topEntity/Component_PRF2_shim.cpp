#include <cstdlib>

#include <verilated.h>

#include "VComponent_PRF2.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_PRF2 *top = new VComponent_PRF2;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

