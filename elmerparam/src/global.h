#ifndef _GLOBAL_H_
#define _GLOBAL_H_

#include <float.h>
#include "config.h"
#include "dynarray.h"

#define FALSE 0
#define TRUE 1
#define MAXLINESIZE 512
#define MAXFILESIZE 512
#define DEFAULT_FUN DBL_MAX
#define PKG_NAME "ElmerParam: "

#ifndef DISABLE_MATC
    char *mtc_domath(char *);
    void mtc_init(FILE * input, FILE * output, FILE * error);

#   define MTC_DOMATH(cmd) mtc_domath(cmd)
#   define MTC_INIT(p) {\
        char command[MAXLINESIZE];\
        int i;\
    \
        mtc_init(NULL, stdout, stderr);\
        strcpy(command, "format( 12, \"rowform\")");\
        mtc_domath(command);\
        for (i = 0; i < da_n(p->xr); i++) {\
            sprintf(command, "R(%d) = %e", i, dr_get(p->xr,i));\
            mtc_domath(command);\
        }\
        for (i = 0; i < da_n(p->xi); i++) {\
            sprintf(command, "I(%d) = %d", i, di_get(p->xi,i));\
            mtc_domath(command);\
        }\
        printf("MATC library was activated!\n");\
        p->usematc = TRUE;\
    }
#else
static void *nop() { return NULL; }
#   define MTC_DOMATH(cmd) nop()
#   define MTC_INIT(p) {\
        fprintf(stderr, "WARNING: This version of ElmerParam was compiled "\
                        "without MATC library!\n");\
        p->usematc = FALSE;\
    }
#endif

typedef struct {
    daint_t *xi;
    dareal_t *xr;
    dareal_t *fun;

    int info, usematc, isfun;

    int taglen;
    char tag[MAXLINESIZE];

    char cmdfile[MAXFILESIZE];
    int lnr;                    /* Line number in the command file. */
} param_t;

#endif                          /* _GLOBAL_H_ */
