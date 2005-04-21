dnl @synopsis ACX_PROG_AR([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl 
dnl look for AR
dnl
dnl ELMER_PREFIX
dnl
dnl @version $Id: acx_blas.m4,v 1.1.1.1 2005/04/15 10:31:18 vierinen Exp $
dnl @author juha.vierinen@csc.fi

dnl
dnl Check for ar.
dnl
AC_DEFUN([ACX_PROG_AR],
[if test -z "$AR"; then
  AR=ar
fi
AC_SUBST(AR)

if test -z "$ARFLAGS"; then
  ARFLAGS="rc"
fi
AC_SUBST(ARFLAGS)
])


dnl  
dnl Elmer-prefix is the canonical place where to build elmer. 
dnl It might be even possible that everything might be installed here as well?
dnl another option could be to look for libs in the normal prefix?
dnl
AC_DEFUN([ACX_ELMER_PREFIX], [
AC_PREREQ(2.50)
acx_eio_ok=no

AC_ARG_WITH([elmer_prefix],
	[AC_HELP_STRING([--with-elmer-prefix=<dir>], [Specify where to look for libraries needed by elmer])])
])dnl ACX_ELMER_PREFIX

dnl
dnl @synopsis ACX_HUTI([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for hut iterative solver library. 
dnl
AC_DEFUN([ACX_HUTI], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
acx_huti_ok=no

AC_ARG_WITH(huti,
	[AC_HELP_STRING([--with-huti=<lib>], [use HUTI library <lib>])])
case $with_huti in
	yes | "") ;;
	no) acx_huti_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) HUTI_LIBS="$with_huti" ;;
	*) HUTI_LIBS="-l$with_huti" ;;
esac

# Get fortran linker names of BLAS functions to check for.
AC_FC_FUNC(huti_d_gmres)
AC_FC_FUNC(huti_d_cgs)

acx_huti_save_LIBS="$LIBS"
LIBS="$BLAS_LIBS $LAPACK_LIBS $LIBS $FCLIBS $FLIBS"

# First, check HUTI_LIBS environment variable
if test $acx_huti_ok = no; then
if test "x$HUTI_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$HUTI_LIBS $LIBS"
	AC_MSG_CHECKING([for $huti_d_gmres in $HUTI_LIBS])
	AC_TRY_LINK_FUNC($huti_d_gmres, [acx_huti_ok=yes], [HUTI_LIBS=""])
	AC_MSG_RESULT($acx_huti_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic HUTI library?
if test $acx_huti_ok = no; then
	AC_CHECK_LIB(huti, $huti_d_gmres, [acx_huti_ok=yes; HUTI_LIBS="-lhuti"])
fi

AC_SUBST(HUTI_LIBS)

LIBS="$acx_huti_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_huti_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_HUTI,1,[Define if you have a HUTI library.]),[$1])
        :
else
        acx_huti_ok=no
        $2
fi
])dnl ACX_HUTI




dnl
dnl @synopsis ACX_EIO([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for eio library
dnl
AC_DEFUN([ACX_EIO], [
AC_PREREQ(2.50)
acx_eio_ok=no

AC_ARG_WITH(eio,
	[AC_HELP_STRING([--with-eio=<lib>], [use EIO library <lib>])])
case $with_eio in
	yes | "") ;;
	no) acx_eio_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) HUTI_LIBS="$with_eio" ;;
	*) HUTI_LIBS="-l$with_eio" ;;
esac

# Get fortran linker names of EIO functions to check for.
AC_FC_FUNC(eio_init)

acx_eio_save_LIBS="$LIBS"

# First, check EIO_LIBS environment variable
if test $acx_eio_ok = no; then
if test "x$EIO_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$EIO_LIBS $LIBS"
	AC_MSG_CHECKING([for $eio_init in $EIO_LIBS])
	AC_TRY_LINK_FUNC($eio_init, [acx_eio_ok=yes], [EIO_LIBS=""])
	AC_MSG_RESULT($acx_eio_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic EIO library?
if test $acx_eio_ok = no; then
	AC_CHECK_LIB(eio, $eio_init, [acx_eio_ok=yes; EIO_LIBS="-leio"])
fi

AC_SUBST(EIO_LIBS)

LIBS="$acx_eio_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_eio_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_EIO,1,[Define if you have a EIO library.]),[$1])
        :
else
        acx_eio_ok=no
        $2
fi
])dnl ACX_EIO


dnl
dnl @synopsis ACX_ARPACK([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for ARPACK library
dnl
AC_DEFUN([ACX_ARPACK], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
acx_huti_ok=no

AC_MSG_CHECKING([whether you want ARPACK functions])
AC_ARG_WITH(arpack,
    [AC_HELP_STRING([--with-arpack=PATH],
                    [enable ARPACK functions (specify path to lib)])],
    [ arpack_lib="-larpack -L${withval}"],
    [ arpack_lib="" ]
)
if test -n "${arpack_lib}"; then
    arpack_do_tools="arpack"
    AC_MSG_RESULT([yes])
else
    arpack_do_tools=""
    AC_MSG_RESULT([no])
fi
AC_SUBST(arpack_lib)
AC_SUBST(arpack_do_tools)
])dnl ACX_ARPACK
