/*
 *
 * huti_main.c - HUTIter libarary auxiliary routines
 *
 * $Id: huti_main.c,v 1.2 2005/05/04 20:18:42 vierinen Exp $
 *
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "huti_defs.h"


#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* Global HUTI variables */

int huti_num_of_procs;

/*
 *
 * HUTI_Init - Initialize HUTI environment
 *
 */
void HUTI_Init()
{
  char *evname;
  static int huti_init_done = FALSE;

  if (huti_init_done)
    return;

  /* Read environment variables */

  if ((evname = getenv(NUMBER_OF_PROCESSORS)) == NULL)
    huti_num_of_procs = 1;
  else
    if ((huti_num_of_procs = atoi(evname)) == 0) {
#if 0
      (void) HUTI_Errexit("Environment variable ",NUMBER_OF_PROCESSORS,
		   " has an illegal value ", evname, NULL);
#endif
  }

  huti_init_done = TRUE;
}

/*
 *
 *
 *
 */

void HUTI_Exit()
{
}

/*
 *
 * HUTI_Errexit - Print error messages and exit. There can be several
 *                messages as an argument.
 *
 */

#if 0
HUTI_Errexit(va_alist)
     va_dcl
{
  va_list ap;
  char *buffer[MAX_ERRMSGS];
  int msgno;

  msgno = 0;
  va_start( ap );
  while ( (buffer[msgno] = va_arg(ap, char *)) != NULL )
    fprintf(stderr, "%s", buffer[msgno++]);
  fprintf(stderr, "\n");
  fflush(stderr);
  va_end( ap );

  exit(1);
}
#endif

/*
 *
 *
 *
 */
