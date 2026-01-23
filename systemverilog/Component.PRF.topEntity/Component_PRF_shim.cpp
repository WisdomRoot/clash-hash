#include <cstdlib>

#include <verilated.h>

#include "VComponent_PRF.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_PRF *top = new VComponent_PRF;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

