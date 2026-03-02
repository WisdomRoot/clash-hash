#include <cstdlib>

#include <verilated.h>

#include "VtopEntity.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VtopEntity *top = new VtopEntity;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

