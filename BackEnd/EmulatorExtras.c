#include "HsFFI.h"
#include "Emulator_stub.h"

extern void __stginit_legalzmemulatorzm1zi0_Emulator();


int emulator_init() {
  int argc = 1;
  char *arg0 = "emulator";
  char **argv = { &arg0 };
  
  hs_init(&argc, &argv);
  hs_add_root(__stginit_legalzmemulatorzm1zi0_Emulator);
  
  return 1;
}
