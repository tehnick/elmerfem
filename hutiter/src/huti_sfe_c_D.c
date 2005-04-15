 #include <stdlib.h>
 #include <stdio.h>
 #include "huti_defs.h"

extern void HUTI_dCGSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dCGSSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dBICGSTABSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dTFQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dGMRESSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_dBICGSTAB_2SOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern int huti_num_of_procs;

extern void huti_ddummy_pcondfun_ (void *u, void *v, int *ipar);



extern void ddot_ (int *N, void *x, int *xind, void *y, int *yind);
extern void ddot_ (int *N, void *x, int *xind, void *y, int *yind);
extern void dnrm2_ (int *N, void *x, int *xind);
void huti_d_cg_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dcgsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_tfqmr_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dtfqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_cgs_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dcgssolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_qmr_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_bicgstab_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dbicgstabsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_gmres_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dgmressolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_d_bicgstab_2_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_ddummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_ddummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = ddot_;
  if (*((int *) normfun) == 0)
    normfun = dnrm2_;




  huti_dbicgstab_2solv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
