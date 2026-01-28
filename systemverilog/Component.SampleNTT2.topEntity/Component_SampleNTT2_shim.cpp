#include <cstdlib>

#include <verilated.h>

#include "VComponent_SampleNTT2.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_SampleNTT2 *top = new VComponent_SampleNTT2;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

