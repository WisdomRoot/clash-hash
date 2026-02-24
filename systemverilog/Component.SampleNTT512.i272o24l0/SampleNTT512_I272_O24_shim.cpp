#include <cstdlib>

#include <verilated.h>

#include "VSampleNTT512_I272_O24.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSampleNTT512_I272_O24 *top = new VSampleNTT512_I272_O24;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

