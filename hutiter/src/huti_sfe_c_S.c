 #include <stdlib.h>
 #include <stdio.h>
 #include "huti_defs.h"

extern void HUTI_sCGSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sCGSSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sBICGSTABSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sTFQMRSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sGMRESSOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void HUTI_sBICGSTAB_2SOLV_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern int huti_num_of_procs;

extern void huti_sdummy_pcondfun_ (void *u, void *v, int *ipar);



extern void sdot_ (int *N, void *x, int *xind, void *y, int *yind);
extern void sdot_ (int *N, void *x, int *xind, void *y, int *yind);
extern void snrm2_ (int *N, void *x, int *xind);
void huti_s_cg_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_scgsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_tfqmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_stfqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_cgs_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_scgssolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_qmr_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_sqmrsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_bicgstab_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_sbicgstabsolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_gmres_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_sgmressolv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
void huti_s_bicgstab_2_ ( void *xvec, void *rhsvec,

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
    pcondrsubr = huti_sdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_sdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = sdot_;
  if (*((int *) normfun) == 0)
    normfun = snrm2_;




  huti_sbicgstab_2solv_ ( &HUTI_NDIM, &HUTI_WRKDIM, xvec, rhsvec,

                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
