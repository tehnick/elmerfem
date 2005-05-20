/*******************************************************************************
 *
 *       ELMER, A Computational Fluid Dynamics Program.
 *
 *       Copyright 1st April 1995 - , Center for Scientific Computing,
 *                                    Finland.
 *
 *       All rights reserved. No part of this program may be used,
 *       reproduced or transmitted in any form or by any means
 *       without the written permission of CSC.
 *
 ******************************************************************************/

/*******************************************************************************
 *
 * Provide system time / memory usage.
 *
 *******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02101 Espoo, Finland
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 26 Sep 1995
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 ******************************************************************************/

#include "../config.h"

#ifndef WIN32 // posix compliant...

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#include <sys/time.h>
#include <sys/resource.h>

static struct rusage usage;

static struct timeval tp;
static struct timezone tzp;

#ifndef HAVE_F_ETIME
float FC_FUNC(etime,ETIME)(tt)
float tt[2];
{
   int who;
   struct rusage used;
   who = 0;
   getrusage(who,&used);
   tt[0] = used.ru_utime.tv_sec+((used.ru_utime.tv_usec)/1000000.);
   tt[1] = used.ru_stime.tv_sec+((used.ru_stime.tv_usec)/1000000.);
   return(tt[0]+tt[1]);
}
#endif

#ifndef HAVE_F_FLUSH
void FC_FUNC(flush,FLUSH) (int n)
{
  // might as well flush a toilet...?
}
#endif

double FC_FUNC(cputime,CPUTIME) ()
{
  getrusage( RUSAGE_SELF, &usage );
  return (double) usage.ru_utime.tv_sec + usage.ru_utime.tv_usec*1.0e-6;
}

double FC_FUNC(realtime,REALTIME) () 
{
  gettimeofday( &tp,&tzp );
  return (double) tp.tv_sec + tp.tv_usec*1.0e-6;
}

double FC_FUNC(cpumemory,CPUMEMORY) ()
{ 
  getrusage( RUSAGE_SELF, &usage );
  return (double) 1.0 * usage.ru_maxrss;
}

#else // windoze territory...

#include <sys/types.h>
#include <time.h>

__declspec(dllexport) double __stdcall REALTIME( )
{
  return clock() / (double)CLOCKS_PER_SEC;
}

__declspec(dllexport) double __stdcall CPUTIME( )
{
  return clock() / (double)CLOCKS_PER_SEC;
}

__declspec(dllexport) double __stdcall CPUMEMORY( )
{
  return 0.0;
}

#endif // WIN32
