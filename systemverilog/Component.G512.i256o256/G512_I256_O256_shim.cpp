#include <cstdlib>

#include <verilated.h>

#include "VG512_I256_O256.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VG512_I256_O256 *top = new VG512_I256_O256;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

