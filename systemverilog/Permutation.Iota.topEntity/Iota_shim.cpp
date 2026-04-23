#include <cstdlib>

#include <verilated.h>

#include "VIota.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VIota *top = new VIota;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

