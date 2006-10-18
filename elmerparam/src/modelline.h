#ifndef _MODELLINE_H_
#define _MODELLINE_H_

#include "global.h"

typedef enum { ML_LITERAL, ML_WHITESPACE, ML_PARAM } mltype_t;


/* Used to represent both ML_LITERAL:s and ML_WHIESPACE:s.  ML_WHITESPACE is
 * treated exactly the same way as ML_LITERAL when printing, but when reading,
 * it's length is insignificant; whitespace up to next non-whitespace character
 * is skipped. */

typedef struct {
    char s[MAXLINESIZE];
    int len;
} ml_literal_t;

typedef struct {
    char type;   /* I, R, P, T or O */
    daint_t *index;
    int len;     /* Vector length; 0 for "all", -1 for error during parsing.  */
    int column;
} ml_param_t;

typedef struct ml_node_t {
    mltype_t type;
    union {
        ml_literal_t l;
        ml_param_t p;
    } u;
    struct ml_node_t *next;
} ml_node_t;

typedef struct {
    ml_node_t *line;    /* Linked list of nodes representing one line. */

    /* The following components are used for error messages.  */
    char *fname;
    int lnr;
} modelline_t;

modelline_t *ml_parse(const char *line, const char *fname, int lnr);
void ml_print(modelline_t *ml, FILE *fd, const param_t *p);
void ml_read(modelline_t *ml, FILE *fd, param_t *p);
void ml_kill(modelline_t *ml);

#endif                          /* _MODELLINE_H_ */
