#include <cstdlib>

#include <verilated.h>

#include "VTheta.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VTheta *top = new VTheta;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

