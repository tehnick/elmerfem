/*
  All kinds of stubs etc that cover up if something is missing from fortran.
 */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "../config.h"

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
  /*  might as well flush a toilet...? */
}
#endif

/* 
   #ifndef HAVE_MPI  
   
   These are always used for the non-mpi version.
 */
void FC_FUNC_(mpi_init,MPI_INIT) 
     (int *p) { *p = 0; }
void FC_FUNC_(mpi_comm_size,MPI_COMM_SIZE) 
     (int *a, int *b, int *c) { *b = 1; *c = 0;}
void FC_FUNC_(mpi_comm_rank,MPI_COMM_RANK) 
     (int *a, int *b, int *c) { *b = 0; *c = 0;}
void FC_FUNC_(mpi_recv,MPI_RECV) 
     (int *a,int *b,int *c,int *d,int *e,int *f,int *g,int *h) {}
void FC_FUNC_(mpi_send,MPI_SEND)
     (int *a,int *b,int *c,int *d,int *e,int *f,int *g) {}
void FC_FUNC_(mpi_bcast,MPI_BCAST) () {}
void FC_FUNC_(mpi_barrier,MPI_BARRIER)
     (int *a,int *b) {}
void FC_FUNC_(mpi_finalize,MPI_FINALIZE)
     (int *a) {}
void FC_FUNC_(mpi_dup_fn,MPI_DUP_FN) () {}
void FC_FUNC_(mpi_null_copy_fn,MPI_NULL_COPY_FN) () {}
void FC_FUNC_(mpi_buffer_detach,MPI_BUFFER_DETACH) () {}
void FC_FUNC_(mpi_bsend,MPI_BSEND) () {}
void FC_FUNC_(mpi_null_delete_fn,MPI_NULL_DELETE_FN) () {}
void FC_FUNC_(mpi_buffer_attach,MPI_BUFFER_ATTACH) () {}

/* parpack */
void FC_FUNC(pdneupd,PDNEUPD) () {}
void FC_FUNC(pdseupd,PDSEUPD) () {}
void FC_FUNC(pdsaupd,PDSAUPD) () {}
void FC_FUNC(pdnaupd,PDNAUPD) () {}

/* #endif  */
