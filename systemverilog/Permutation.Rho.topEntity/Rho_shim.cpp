#include <cstdlib>

#include <verilated.h>

#include "VRho.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VRho *top = new VRho;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

