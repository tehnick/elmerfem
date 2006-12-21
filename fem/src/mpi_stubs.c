#include "../config.h"

void STDCALLBULL FC_FUNC_(mpi_init,MPI_INIT) 
     (int *p) { *p = 0; }
void STDCALLBULL FC_FUNC_(mpi_comm_size,MPI_COMM_SIZE) 
     (int *a, int *b, int *c) { *b = 1; *c = 0;}
void STDCALLBULL FC_FUNC_(mpi_comm_rank,MPI_COMM_RANK) 
     (int *a, int *b, int *c) { *b = 0; *c = 0;}
void STDCALLBULL FC_FUNC_(mpi_recv,MPI_RECV) 
     (int *a,int *b,int *c,int *d,int *e,int *f,int *g,int *h) {}
void STDCALLBULL FC_FUNC_(mpi_send,MPI_SEND)
     (int *a,int *b,int *c,int *d,int *e,int *f,int *g) {}
void STDCALLBULL FC_FUNC_(mpi_bcast,MPI_BCAST) () {}
void STDCALLBULL FC_FUNC_(mpi_barrier,MPI_BARRIER)
     (int *a,int *b) {}
void STDCALLBULL FC_FUNC_(mpi_finalize,MPI_FINALIZE)
     (int *a) {}
void STDCALLBULL FC_FUNC_(mpi_dup_fn,MPI_DUP_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_null_copy_fn,MPI_NULL_COPY_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_buffer_detach,MPI_BUFFER_DETACH) ( void *buf, int *i, int *ierr ) {}
void STDCALLBULL FC_FUNC_(mpi_bsend,MPI_BSEND) (void *a, void *b, void *c, void *d, void *e, void *f, void *g ) {}
void STDCALLBULL FC_FUNC_(mpi_null_delete_fn,MPI_NULL_DELETE_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_buffer_attach,MPI_BUFFER_ATTACH) ( void *buf, int *i, int *ierr ) {}
void STDCALLBULL FC_FUNC_(mpi_allreduce,MPI_ALLREDUCE) () {}
void STDCALLBULL FC_FUNC_(mpi_wtime,MPI_WTIME) () {}
void STDCALLBULL FC_FUNC_(mpi_wtick,MPI_WTICK) () {}
void STDCALLBULL FC_FUNC_(pmpi_wtime,PMPI_WTIME) () {}
void STDCALLBULL FC_FUNC_(pmpi_wtick,PMPI_WTICK) () {}

void STDCALLBULL FC_FUNC_(mpi_type_null_copy_fn,MPI_TYPE_NULL_COPY_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_comm_dup_fn,MPI_COMM_DUP_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_type_null_delete_fn,MPI_TYPE_NULL_DELETE_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_win_dup_fn,MPI_WIN_DUP_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_win_null_copy_fn,MPI_WIL_NULL_COPY_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_type_dup_fn,MPI_TYPE_DUP_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_win_null_delete_fn,MPI_WIN_NULL_DELETE_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_comm_null_delete_fn,MPI_COMM_NULL_DELETE_FN) () {}
void STDCALLBULL FC_FUNC_(mpi_comm_null_copy_fn,MPI_COMM_NULL_DELETE_FN) () {}

void STDCALLBULL MPI_Type_free() {}
void STDCALLBULL MPI_Startall() {}
void STDCALLBULL MPI_Gather() {}
void STDCALLBULL MPI_Alltoall() {}
void STDCALLBULL MPI_Address() {}
void STDCALLBULL MPI_Allreduce() {}
void STDCALLBULL MPI_Comm_size() {}
void STDCALLBULL MPI_Recv() {}
void STDCALLBULL MPI_Isend() {}
void STDCALLBULL MPI_Allgather() {}
void STDCALLBULL MPI_Abort() {}
void STDCALLBULL MPI_Waitall() {}
void STDCALLBULL MPI_Wait() {}
void STDCALLBULL MPI_Comm_rank() {}
void STDCALLBULL MPI_Type_struct() {}
void STDCALLBULL MPI_Barrier() {}
void STDCALLBULL MPI_Allgatherv() {}
void STDCALLBULL MPI_Irecv() {}
void STDCALLBULL MPI_Bcast() {}
void STDCALLBULL MPI_Reduce() {}
void STDCALLBULL MPI_Request_free() {}
void STDCALLBULL MPI_Scatter() {}
void STDCALLBULL MPI_Type_commit() {}
void STDCALLBULL MPI_Recv_init() {}
void STDCALLBULL MPI_Wtime() {}
void STDCALLBULL MPI_Send_init() {}


/* parpack */
void STDCALLBULL FC_FUNC(pdneupd,PDNEUPD) ( void *a, void *b, void *c, void *d, void *e, void *f, void *g, void *a8, void *b9, void *c10, void *d11, void *e12, void *f13, void *g14, void *a15, void *b16, void *c17, void *d18, void *e19, void *a20, void *b21, void *c22, void *d23, void *e24, void *f25, void *g26, void *g27, void *g28,void *g29 ) {}
void STDCALLBULL FC_FUNC(pdseupd,PDSEUPD) ( void *a, void *b, void *c, void *d, void *e, void *f, void *g, void *a8, void *b9, void *c10, void *d11, void *e12, void *f13, void *g14, void *a15, void *b16, void *c17, void *d18, void *e19, void *a20, void *b21, void *c22, void *d23, void *e24, void *f25, void *g26  ) {}
void STDCALLBULL FC_FUNC(pdsaupd,PDSAUPD) ( void *a, void *b, void *c, void *d, void *e, void *f, void *g, void *a8, void *b9, void *c10, void *d11, void *e12, void *f13, void *g14, void *a15, void *b16, void *c17, void *d18, void *e19 ) {}
void STDCALLBULL FC_FUNC(pdnaupd,PDNAUPD) ( void *a, void *b, void *c, void *d, void *e, void *f, void *g, void *a8, void *b9, void *c10, void *d11, void *e12, void *f13, void *g14, void *a15, void *b16, void *c17, void *d18, void *e19 ) {}
