 #include <stdlib.h>
 #include <stdio.h>
 #include "huti_defs.h"

extern void HUTI_cCGSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cCGSSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cBICGSTABSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cTFQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cGMRESSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_cBICGSTAB_2SOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern int huti_num_of_procs;

extern void huti_cdummy_pcondfun_ (void *u, void *v, int *ipar);



extern void cdotu_ (int *N, void *x, int *xind, void *y, int *yind);
extern void cdotc_ (int *N, void *x, int *xind, void *y, int *yind);
extern void scnrm2_ (int *N, void *x, int *xind);
void huti_c_cg_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_ccgsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_tfqmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_ctfqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_cgs_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_ccgssolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_qmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_cqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_bicgstab_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_cbicgstabsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_gmres_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotc_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_cgmressolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_c_bicgstab_2_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_cdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_cdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = cdotu_;
  if (*((int *) normfun) == 0)
    normfun = scnrm2_;




  huti_cbicgstab_2solv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
