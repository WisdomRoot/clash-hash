#include <cstdlib>

#include <verilated.h>

#include "VIotaRev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VIotaRev *top = new VIotaRev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

