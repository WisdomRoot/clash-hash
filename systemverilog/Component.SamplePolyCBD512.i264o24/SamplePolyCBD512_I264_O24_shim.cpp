#include <cstdlib>

#include <verilated.h>

#include "VSamplePolyCBD512_I264_O24.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSamplePolyCBD512_I264_O24 *top = new VSamplePolyCBD512_I264_O24;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

