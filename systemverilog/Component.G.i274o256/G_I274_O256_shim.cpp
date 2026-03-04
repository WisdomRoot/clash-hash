#include <cstdlib>

#include <verilated.h>

#include "VG_I274_O256.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VG_I274_O256 *top = new VG_I274_O256;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

