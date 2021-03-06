#include "HsFFI.h"
#include "Rts.h"
#include "RtsOpts.h"
#include "Emulator_stub.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

extern void __stginit_legalzmemulatorzm1zi0_Emulator();

#ifndef RTS_OPTS_ENABLED
#define RTS_OPTS_ENABLED RtsOptsAll
#endif

const RtsOptsEnabledEnum rtsOptsEnabled = RTS_OPTS_ENABLED;


int argc;
char **argv;


int emulator_init() {
  int profiling = 0;
  
  char *profiling_variable = getenv("LEGAL_EMULATOR_PROFILE");
  if(profiling_variable && !strcasecmp(profiling_variable, "yes")) {
    profiling = 1;
  }
  
  if(profiling) {
    argc = 7;
    char *arg0 = "emulator";
    char *arg1 = "+RTS";
    char *arg2 = "-xc";
    char *arg3 = "-pa";
    char *arg4 = "-hr";
    char *arg5 = "-sprofile.txt";
    char *arg6 = "-i0.01";
    argv = malloc(8 * sizeof(char *));
    argv[0] = arg0;
    argv[1] = arg1;
    argv[2] = arg2;
    argv[3] = arg3;
    argv[4] = arg4;
    argv[5] = arg5;
    argv[6] = arg6;
    argv[7] = NULL;
    int argc_modified = argc;
    char **argv_modified = argv;
    
    hs_init(&argc_modified, &argv_modified);
  } else {
    argc = 1;
    char *arg0 = "emulator";
    argv = malloc(2 * sizeof(char *));
    argv[0] = arg0;
    argv[1] = NULL;
    int argc_modified = argc;
    char **argv_modified = argv;
    
    hs_init(&argc_modified, &argv_modified);
  }
  
  hs_add_root(__stginit_legalzmemulatorzm1zi0_Emulator);
  
  return 1;
}


void emulator_terminate() {
  hs_exit();
}
