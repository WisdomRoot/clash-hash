#include <cstdlib>

#include <verilated.h>

#include "VKeccakF1600_PermSeq.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VKeccakF1600_PermSeq *top = new VKeccakF1600_PermSeq;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

