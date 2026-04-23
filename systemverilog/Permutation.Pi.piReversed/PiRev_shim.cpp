#include <cstdlib>

#include <verilated.h>

#include "VPiRev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VPiRev *top = new VPiRev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

