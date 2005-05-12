
/* 
 * These definitions are only to resolve mpi- and parpack routines,
 * if for some reason the libraries are not present. Should perhaps give
 * warning (or error?) if actually called?
 *
 * juhav: modified to use FC_FUNC{_} macros to determine function names, so
 * this will most likely be screwed before it is tested on different platforms
 */
#include "../config.h"

#ifndef HAVE_MPI 
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
#endif 

#ifndef HAVE_PARPACK
void FC_FUNC(pdneupd,PDNEUPD) () {}
void FC_FUNC(pdseupd,PDSEUPD) () {}
void FC_FUNC(pdsaupd,PDSAUPD) () {}
void FC_FUNC(pdnaupd,PDNAUPD) () {}
#endif
