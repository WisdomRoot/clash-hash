#include <cstdlib>

#include <verilated.h>

#include "VComponent_SampleNTT.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_SampleNTT *top = new VComponent_SampleNTT;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

