 #include <stdlib.h>
 #include <stdio.h>
 #include "huti_defs.h"

extern void HUTI_zCGSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zCGSSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zBICGSTABSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zTFQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zGMRESSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_zBICGSTAB_2SOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern int huti_num_of_procs;

extern void huti_zdummy_pcondfun_ (void *u, void *v, int *ipar);



extern void zdotu_ (int *N, void *x, int *xind, void *y, int *yind);
extern void zdotc_ (int *N, void *x, int *xind, void *y, int *yind);
extern void dznrm2_ (int *N, void *x, int *xind);
void huti_z_cg_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zcgsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_tfqmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_ztfqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_cgs_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zcgssolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_qmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_bicgstab_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zbicgstabsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_gmres_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotc_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zgmressolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_z_bicgstab_2_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;




  huti_zbicgstab_2solv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
