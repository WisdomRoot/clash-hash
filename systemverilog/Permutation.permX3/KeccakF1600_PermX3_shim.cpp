#include <cstdlib>

#include <verilated.h>

#include "VKeccakF1600_PermX3.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VKeccakF1600_PermX3 *top = new VKeccakF1600_PermX3;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

