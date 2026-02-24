#include <cstdlib>

#include <verilated.h>

#include "VSN512_I272_O24_L1.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VSN512_I272_O24_L1 *top = new VSN512_I272_O24_L1;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

