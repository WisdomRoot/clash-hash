#include <cstdlib>

#include <verilated.h>

#include "VChiRev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VChiRev *top = new VChiRev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

