#include "HsFFI.h"
#include "Rts.h"
#include "RtsOpts.h"
#include "Emulator_stub.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

extern void __stginit_legalzmemulatorzm1zi0_Emulator();


const RtsOptsEnabledEnum rtsOptsEnabled = RTS_OPTS_ENABLED;



int emulator_init() {
  int profiling = 0;
  
  char *profiling_variable = getenv("LEGAL_EMULATOR_PROFILE");
  if(profiling_variable && !strcasecmp(profiling_variable, "yes")) {
    profiling = 1;
  }
  
  if(profiling) {
    int argc = 4;
    char *arg0 = "emulator";
    char *arg1 = "+RTS";
    char *arg2 = "-P";
    char *arg3 = "-hy";
    char **argv = malloc(5 * sizeof(char *));
    argv[0] = arg0;
    argv[1] = arg1;
    argv[2] = arg2;
    argv[3] = arg3;
    argv[4] = NULL;
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


void emulator_terminate() {
  hs_exit();
}
