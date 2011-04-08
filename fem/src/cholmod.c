#include "../config.h"

#ifdef HAVE_CHOLMOD
#include "cholmod.h"

typedef struct {
  cholmod_common c;
  cholmod_sparse a;
  cholmod_factor *l;
} cholmod;


cholmod STDCALLBULL *FC_FUNC_(cholmod_ffactorize,CHOLMOD_FFACTORIZE)(int *n,int *rows,int *cols,double *vals)
{
  static double *y;
  int i,j,nz,ok,*p,*q;
  double *xx,*bx;

  cholmod *handle;

  handle = (cholmod *)calloc(sizeof(cholmod),1);
  cholmod_start(&handle->c);

  handle->a.nrow=*n;
  handle->a.ncol=*n;
  handle->a.p=rows;
  handle->a.i=cols;
  handle->a.x=vals;
  handle->a.packed=1;
  handle->a.sorted=1;
  handle->a.stype=-1;
  handle->a.nzmax=rows[*n];
  handle->a.xtype=CHOLMOD_REAL;

  handle->l=cholmod_analyze(&handle->a,&handle->c);
  cholmod_factorize(&handle->a,handle->l,&handle->c);

  return handle;
}

void STDCALLBULL FC_FUNC_(cholmod_fsolve,CHOLMOD_FSOLVE)(cholmod **handle,int *n,double *x, double *b)
{
  double *xx,*bb;
  int i;
  cholmod_dense *dx, *db;

  db = cholmod_zeros((*handle)->a.nrow, 1, (*handle)->a.xtype, &(*handle)->c);
  bb=db->x;
  for(i=0;i<*n;i++) bb[i]=b[i];

  dx=cholmod_solve(CHOLMOD_A,(*handle)->l,db,&(*handle)->c);
  xx=dx->x;
  for(i=0;i<*n;i++) x[i]=xx[i];

  cholmod_free_dense(&dx, &(*handle)->c);
  cholmod_free_dense(&db, &(*handle)->c);
}


void STDCALLBULL FC_FUNC_(cholmod_ffree,CHOLMOD_FFREE)(cholmod **handle)
{
  cholmod_free_factor(&(*handle)->l,&(*handle)->c);
  cholmod_finish(&(*handle)->c);
  free(*handle);
}
#endif
