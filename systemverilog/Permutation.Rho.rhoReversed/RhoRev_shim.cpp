#include <cstdlib>

#include <verilated.h>

#include "VRhoRev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VRhoRev *top = new VRhoRev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

