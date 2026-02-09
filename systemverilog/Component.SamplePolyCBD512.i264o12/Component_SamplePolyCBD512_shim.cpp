#include <cstdlib>

#include <verilated.h>

#include "VComponent_SamplePolyCBD512.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VComponent_SamplePolyCBD512 *top = new VComponent_SamplePolyCBD512;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

