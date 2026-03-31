#include <cstdlib>

#include <verilated.h>

#include "VKeccakF1600_PermRev.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VKeccakF1600_PermRev *top = new VKeccakF1600_PermRev;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

