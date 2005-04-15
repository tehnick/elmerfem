
/*

 Internal definitions for HUTIter library

 $Id: huti_intdefs.h,v 1.5 2000/10/11 11:12:12 jpr Exp $

 */

#define AUTOC "!!! This file is generated automatically, do not edit !!!"
#define _CONCAT(x,y)x/**/y

#if defined(AIX) || defined(IRIX) || defined(DUNIX) || defined(UNICOS) || defined(WIN32) || defined(LINUX_PC)

# ifdef S_PRE
#  define PRECISIONC s
#  define PRECISION_COMMENT Single precision
#  define F_PRECISION_TYPE real
#  define C_PRECISION_TYPE float
#  define NORMFUN_PREC_TYPE real
#  if defined(AIX)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##s##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##s##bn2
#    define PRECD_DUMMY_PCONDFUN huti_sdummy_pcondfun
#    define PRECD_BLAS_DOTPRODFUN sdot
#    define PRECD_BLAS_DOTPRODFUN_N sdot
#    define PRECD_BLAS_NORMFUN snrm2
#  endif
#  if defined(WIN32)
#    define MAKE_SUBRN(bn1,bn2)bn1##S##bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1##S##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_SDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN SDOT
#    define PRECD_BLAS_DOTPRODFUN_N SDOT
#    define PRECD_BLAS_NORMFUN SNRM2
#  endif
#  if defined(IRIX) || defined(DUNIX) || defined(LINUX_PC)
#    define PRECD_DUMMY_PCONDFUN huti_sdummy_pcondfun_
#    define PRECD_BLAS_DOTPRODFUN sdot_
#    define PRECD_BLAS_DOTPRODFUN_N sdot_
#    define PRECD_BLAS_NORMFUN snrm2_
#  endif
#  if defined(LINUX_PC)
#    define MAKE_SUBRN(bn1,bn2)bn1##s##bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1##s##bn2##_
#  endif
#  if defined(IRIX)
#    define MAKE_SUBRN(bn1,bn2)bn1/**/s/**/bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1/**/s/**/bn2/**/_
#  endif
#  ifdef DUNIX
#    define MAKE_SUBRN( bn1, bn2 ) bn1/**/s/**/bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1/**/s/**/bn2/**/_
#  endif
#  ifdef UNICOS
#    define MAKE_SUBRN( bn1, bn2 ) bn1##S##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##S##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_SDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN SDOT
#    define PRECD_BLAS_DOTPRODFUN_N SDOT
#    define PRECD_BLAS_NORMFUN SNRM2
#  endif
#  if defined(AIX) || defined(IRIX) || defined(DUNIX) || defined(WIN32) || defined(LINUX_PC)
#    define LAPACK_LU sgetrf
#    define LAPACK_SOLVE sgetrs
#  endif
#  if defined(UNICOS)
#    define LAPACK_LU sgetrf
#    define LAPACK_SOLVE sgetrs
#  endif
# endif

# ifdef D_PRE
#  define PRECISIONC d
#  define PRECISION_COMMENT Double precision
#  define F_PRECISION_TYPE double precision
#  define C_PRECISION_TYPE double
#  define NORMFUN_PREC_TYPE double precision
#  if defined(AIX)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##d##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##d##bn2
#    define PRECD_DUMMY_PCONDFUN huti_ddummy_pcondfun
#    define PRECD_BLAS_DOTPRODFUN ddot
#    define PRECD_BLAS_DOTPRODFUN_N ddot
#    define PRECD_BLAS_NORMFUN dnrm2
#  endif
#  if defined(WIN32)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##D##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##D##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_DDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN DDOT
#    define PRECD_BLAS_DOTPRODFUN_N DDOT
#    define PRECD_BLAS_NORMFUN DNRM2
#  endif
#  if defined(IRIX) || defined(DUNIX) || defined(LINUX_PC)
#    define PRECD_DUMMY_PCONDFUN huti_ddummy_pcondfun_
#    define PRECD_BLAS_DOTPRODFUN ddot_
#    define PRECD_BLAS_DOTPRODFUN_N ddot_
#    define PRECD_BLAS_NORMFUN dnrm2_
#  endif
#  if defined(LINUX_PC)
#    define MAKE_SUBRN(bn1,bn2)bn1##d##bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1##d##bn2##_
#  endif
#  if defined(IRIX)
#    define MAKE_SUBRN(bn1,bn2)bn1/**/d/**/bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1/**/d/**/bn2/**/_
#  endif
#  ifdef DUNIX
#    define MAKE_SUBRN( bn1, bn2 ) bn1/**/d/**/bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1/**/d/**/bn2/**/_
#  endif
#  ifdef UNICOS
#    define MAKE_SUBRN( bn1, bn2 ) bn1##D##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##D##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_DDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN SDOT
#    define PRECD_BLAS_DOTPRODFUN_N SDOT
#    define PRECD_BLAS_NORMFUN SNRM2
#  endif
#  if defined(AIX) || defined(IRIX) || defined(DUNIX) || defined(WIN32) || defined(LINUX_PC)
#    define LAPACK_LU dgetrf
#    define LAPACK_SOLVE dgetrs
#  endif
#  if defined(UNICOS)
#    define LAPACK_LU sgetrf
#    define LAPACK_SOLVE sgetrs
#  endif
# endif

# ifdef C_PRE
#  define PRECISIONC c
#  define PRECISION_COMMENT Complex
#  define F_PRECISION_TYPE complex
#  define C_PRECISION_TYPE float
#  define NORMFUN_PREC_TYPE real
#  if defined(AIX)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##c##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##c##bn2
#    define PRECD_DUMMY_PCONDFUN huti_cdummy_pcondfun
#    define PRECD_BLAS_DOTPRODFUN cdotu
#    define PRECD_BLAS_DOTPRODFUN_N cdotc
#    define PRECD_BLAS_NORMFUN scnrm2
#  endif
#  if defined(WIN32)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##C##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##C##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_CDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN CDOTU
#    define PRECD_BLAS_DOTPRODFUN_N CDOTC
#    define PRECD_BLAS_NORMFUN SCNRM2
#  endif
#  if defined(IRIX) || defined(DUNIX) || defined(LINUX_PC)
#    define PRECD_DUMMY_PCONDFUN huti_cdummy_pcondfun_
#    define PRECD_BLAS_DOTPRODFUN cdotu_
#    define PRECD_BLAS_DOTPRODFUN_N cdotc_
#    define PRECD_BLAS_NORMFUN scnrm2_
#  endif
#  if defined(LINUX_PC)
#    define MAKE_SUBRN(bn1,bn2)bn1##c##bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1##c##bn2##_
#  endif
#  if defined(IRIX)
#    define MAKE_SUBRN(bn1,bn2)bn1/**/c/**/bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1/**/c/**/bn2/**/_
#  endif
#  ifdef DUNIX
#    define MAKE_SUBRN( bn1, bn2 ) bn1/**/c/**/bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1/**/c/**/bn2/**/
#  endif
#  ifdef UNICOS
#    define MAKE_SUBRN( bn1, bn2 ) bn1##C##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##C##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_CDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN CDOTU
#    define PRECD_BLAS_DOTPRODFUN_N CDOTC
#    define PRECD_BLAS_NORMFUN SCNRM2
#  endif
#  if defined(AIX) || defined(IRIX) || defined(DUNIX) || defined(WIN32) || defined(LINUX_PC)
#    define LAPACK_LU cgetrf
#    define LAPACK_SOLVE cgetrs
#  endif
#  if defined(UNICOS)
#    define LAPACK_LU cgetrf
#    define LAPACK_SOLVE cgetrs
#  endif
# endif

# ifdef Z_PRE
#  define PRECISIONC z
#  define PRECISION_COMMENT Double complex
#  define F_PRECISION_TYPE double complex
#  define C_PRECISION_TYPE double
#  define NORMFUN_PREC_TYPE double precision
#  if defined(AIX)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##z##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##z##bn2
#    define PRECD_DUMMY_PCONDFUN huti_zdummy_pcondfun
#    define PRECD_BLAS_DOTPRODFUN zdotu
#    define PRECD_BLAS_DOTPRODFUN_N zdotc
#    define PRECD_BLAS_NORMFUN dznrm2
#  endif
#  if defined(WIN32)
#    define MAKE_SUBRN( bn1, bn2 ) bn1##Z##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##Z##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_ZDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN ZDOTU
#    define PRECD_BLAS_DOTPRODFUN_N ZDOTC
#    define PRECD_BLAS_NORMFUN DZNRM2
#  endif
#  if defined(IRIX) || defined(DUNIX) || defined(LINUX_PC)
#    define PRECD_DUMMY_PCONDFUN huti_zdummy_pcondfun_
#    define PRECD_BLAS_DOTPRODFUN zdotu_
#    define PRECD_BLAS_DOTPRODFUN_N zdotc_
#    define PRECD_BLAS_NORMFUN dznrm2_
#  endif
#  if defined(LINUX_PC)
#    define MAKE_SUBRN(bn1,bn2)bn1##z##bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1##z##bn2##_
#  endif
#  if defined(IRIX)
#    define MAKE_SUBRN(bn1,bn2)bn1/**/z/**/bn2
#    define MAKE_F_SUBRN(bn1,bn2)bn1/**/z/**/bn2/**/_
#  endif
#  ifdef DUNIX
#    define MAKE_SUBRN( bn1, bn2 ) bn1/**/z/**/bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1/**/z/**/bn2/**/_
#  endif
#  ifdef UNICOS
#    define MAKE_SUBRN( bn1, bn2 ) bn1##Z##bn2
#    define MAKE_F_SUBRN( bn1, bn2 ) bn1##Z##bn2
#    define PRECD_DUMMY_PCONDFUN HUTI_ZDUMMY_PCONDFUN
#    define PRECD_BLAS_DOTPRODFUN CDOTU
#    define PRECD_BLAS_DOTPRODFUN_N CDOTC
#    define PRECD_BLAS_NORMFUN SCNRM2
#  endif
#  if defined(AIX) || defined(IRIX) || defined(DUNIX) || defined(WIN32) || defined(LINUX_PC)
#    define LAPACK_LU zgetrf
#    define LAPACK_SOLVE zgetrs
#  endif
#  if defined(UNICOS)
#    define LAPACK_LU cgetrf
#    define LAPACK_SOLVE cgetrs
#  endif
# endif

# if defined(AIX) || defined(IRIX) || defined(DUNIX)
#   define MAKE_INCLUDE( nsign, inc, incfile )nsign/**/inc incfile
#   define MAKE_DEFINE( nsign, def, dname, dvalue )nsign/**/def dname dvalue
# endif
# if defined(UNICOS) || defined(WIN32) || defined(LINUX_PC)
#   define MAKE_INCLUDE(nsign,inc,incfile)nsign##inc incfile
#   define MAKE_DEFINE(nsign,def,dname,dvalue)nsign##def dname dvalue
# endif

#endif

