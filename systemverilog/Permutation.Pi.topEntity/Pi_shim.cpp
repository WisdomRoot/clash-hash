#include <cstdlib>

#include <verilated.h>

#include "VPi.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VPi *top = new VPi;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

