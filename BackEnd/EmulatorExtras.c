#include "HsFFI.h"
#include "Emulator_stub.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

extern void __stginit_legalzmemulatorzm1zi0_Emulator();


int emulator_init() {
  int profiling = 0;
  
  char *profiling_variable = getenv("LEGAL_EMULATOR_PROFILE");
  if(profiling_variable && !strcasecmp(profiling_variable, "yes")) {
    printf("Profiling, hopefully.\n");
    profiling = 1;
  }
  
  if(profiling) {
    int argc = 3;
    char *arg0 = "emulator";
    char *arg1 = "+RTS";
    char *arg2 = "-hy";
    char **argv = malloc(4 * sizeof(char *));
    argv[0] = arg0;
    argv[1] = arg1;
    argv[2] = arg2;
    argv[3] = NULL;
    int argc_modified = argc;
    char **argv_modified = argv;
    
    hs_init(&argc_modified, &argv_modified);
    
    free(argv);
  } else {
    int argc = 1;
    char *arg0 = "emulator";
    char **argv = malloc(2 * sizeof(char *));
    argv[0] = arg0;
    argv[1] = NULL;
    int argc_modified = argc;
    char **argv_modified = argv;
    
    hs_init(&argc_modified, &argv_modified);
    
    free(argv);
  }
  
  hs_add_root(__stginit_legalzmemulatorzm1zi0_Emulator);
  
  return 1;
}
