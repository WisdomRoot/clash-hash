#include <cstdlib>

#include <verilated.h>

#include "VComponent_Dev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_Dev *top = new VComponent_Dev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

