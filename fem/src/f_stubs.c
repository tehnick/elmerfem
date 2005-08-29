/*
  All kinds of stubs etc that cover up if something is missing from fortran.
 */
#include "../config.h"


#include <sys/types.h>




#if defined(WIN32) | defined(MINGW32)

#else 

#include <sys/resource.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/param.h>

#endif



#ifndef HAVE_F_ETIME

#if defined(WIN32) | defined(MINGW32)
float STDCALLBULL FC_FUNC(etime,ETIME)(tt)
float tt[2];
{
  return(.0);
}
#else
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
#endif // win32
#endif // etime_defined

#ifndef HAVE_F_FLUSH
void STDCALLBULL FC_FUNC(flush,FLUSH) (int n)
{
  /*  might as well flush a toilet...? */
}
#endif
