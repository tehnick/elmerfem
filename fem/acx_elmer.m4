dnl 
dnl Elmer specific M4sh macros 
dnl
dnl @version $Id: acx_elmer.m4,v 1.2 2005/04/29 08:04:53 vierinen Exp $
dnl @author juha.vierinen@csc.fi
dnl

dnl
dnl @synopsis ACX_PROG_AR([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl 
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
dnl @synopsis ACX_EIOF([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for eio fortran library
dnl
AC_DEFUN([ACX_EIOF], [
AC_PREREQ(2.50)
acx_eiof_ok=no

AC_ARG_WITH(eiof,
	[AC_HELP_STRING([--with-eiof=<lib>], [use EIOF library <lib>])])
case $with_eiof in
	yes | "") ;;
	no) acx_eiof_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) EIOF_LIBS="$with_eiof" ;;
	*) EIOF_LIBS="-l$with_eiof" ;;
esac

# Get fortran linker names of EIO functions to check for.
AC_FC_FUNC(eio_init)

acx_eiof_save_LIBS="$LIBS"

LIBS="$LIBS"

# First, check EIO_LIBS environment variable
if test $acx_eiof_ok = no; then
if test "x$EIOF_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$EIOF_LIBS $LIBS"
	AC_MSG_CHECKING([for $eio_init in $EIOF_LIBS])
	AC_TRY_LINK_FUNC($eio_init, [acx_eiof_ok=yes], [EIOF_LIBS=""])
	AC_MSG_RESULT($acx_eiof_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic EIO library?
if test "$acx_eiof_ok" = no; then
	AC_CHECK_LIB(eiof, $eio_init, [acx_eiof_ok=yes; EIOF_LIBS="-leiof"])
fi

AC_SUBST(EIOF_LIBS)

LIBS="$acx_eiof_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_eiof_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_EIOF,1,[Define if you have a EIOF library.]),[$1])
        :
else
        acx_eiof_ok=no
        $2
fi
])dnl ACX_EIO



dnl
dnl @synopsis ACX_EIOC([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for eio library
dnl
AC_DEFUN([ACX_EIOC], 
[
AC_PREREQ(2.50)
acx_eioc_ok=no

AC_ARG_WITH(eioc,
	[AC_HELP_STRING([--with-eioc=<lib>], [use EIOC library <lib>])])
case $with_eioc in
	yes | "") ;;
	no) acx_eioc_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) EIOC_LIBS="$with_eioc" ;;
	*) EIOC_LIBS="-l$with_eioc" ;;
esac

acx_eioc_save_LIBS="$LIBS"

LIBS="-leioc $LIBS"

# First, check EIO_LIBS environment variable
if test $acx_eioc_ok = no; then
if test "x$EIOC_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$LIBS $EIOC_LIBS"
	AC_MSG_CHECKING([for eio_init in $EIOC_LIBS])
	AC_TRY_LINK_FUNC(eio_init, [acx_eioc_ok=yes], [EIOC_LIBS=""])
	AC_MSG_RESULT($acx_eioc_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic EIO library?
if test "$acx_eioc_ok" = no; then
	AC_CHECK_LIB(eioc, eio_init, [acx_eioc_ok=yes; EIOC_LIBS="-leioc"])
fi

AC_SUBST(EIOC_LIBS)

LIBS="$acx_eioc_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_eioc_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_EIOC,1,[Define if you have a EIOC library.]),[$1])
        :
else
        acx_eioc_ok=no
        $2
fi
])dnl ACX_EIOC


dnl
dnl @synopsis ACX_ARPACK([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for ARPACK library
dnl
AC_DEFUN([ACX_ARPACK], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
acx_arpack_ok=no

AC_ARG_WITH(arpack,
	[AC_HELP_STRING([--with-arpack=<lib>], [Specify location of ARPACK])])
case $with_arpack in
	yes | "") ;;
	no) acx_arpack_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) ARPACK_LIBS="$with_arpack" ;;
	*) ARPACK_LIBS="-l$with_arpack" ;;
esac

# Get fortran linker names of ARPACK functions to check for.
AC_FC_FUNC(dseupd)

acx_arpack_save_LIBS="$LIBS"

LIBS="$BLAS_LIBS $LAPACK_LIBS $LIBS $FCLIBS $FLIBS"

# First, check ARPACK_LIBS environment variable
if test $acx_arpack_ok = no; then
if test "x$ARPACK_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$ARPACK_LIBS $LIBS"
	AC_MSG_CHECKING([for $dseupd in $ARPACK_LIBS])
	AC_TRY_LINK_FUNC($dseupd, [acx_arpack_ok=yes], [ARPACK_LIBS=""])
	AC_MSG_RESULT($acx_arpack_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic ARPACK library?
if test $acx_arpack_ok = no; then
	AC_CHECK_LIB(arpack, $dseupd, [acx_arpack_ok=yes; ARPACK_LIBS="-larpack"])
fi

AC_SUBST(ARPACK_LIBS)

LIBS="$acx_arpack_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_arpack_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_ARPACK,1,[Define if you have a ARPACK library.]),[$1])
        :
else
        acx_arpack_ok=no
        $2
fi
])dnl ACX_ARPACK



dnl
dnl @synopsis ACX_UMFPACK([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for UMFPACK library
dnl
AC_DEFUN([ACX_UMFPACK], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
acx_umfpack_ok=no

AC_ARG_WITH(umfpack,
	[AC_HELP_STRING([--with-umfpack=<lib>], [Specify location of UMFPACK])])
case $with_umfpack in
	yes | "") ;;
	no) acx_umfpack_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) UMFPACK_LIBS="$with_umfpack" ;;
	*) UMFPACK_LIBS="-l$with_umfpack" ;;
esac

# Get fortran linker names of UMFPACK functions to check for.
AC_FC_FUNC(umf4def)

acx_umfpack_save_LIBS="$LIBS"

LIBS="$BLAS_LIBS $LAPACK_LIBS $LIBS $FCLIBS $FLIBS"

# First, check UMFPACK_LIBS environment variable
if test $acx_umfpack_ok = no; then
if test "x$UMFPACK_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$UMFPACK_LIBS $LIBS"
	AC_MSG_CHECKING([for $umf4def in $UMFPACK_LIBS])
	AC_TRY_LINK_FUNC($umf4def, [acx_umfpack_ok=yes], [UMFPACK_LIBS=""])
	AC_MSG_RESULT($acx_umfpack_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic UMFPACK library?
if test $acx_umfpack_ok = no; then
	AC_CHECK_LIB(umfpack, $umf4def, [acx_umfpack_ok=yes; UMFPACK_LIBS="-lumfpack -lamd"],,[-lamd])
fi

AC_SUBST(UMFPACK_LIBS)

LIBS="$acx_umfpack_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_umfpack_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_UMFPACK,1,[Define if you have a UMFPACK library.]),[$1])
        :
else
        acx_umfpack_ok=no
        $2
fi
])dnl ACX_UMFPACK


dnl
dnl @synopsis ACX_MATC([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for matc library
dnl
AC_DEFUN([ACX_MATC], [
AC_PREREQ(2.50)
acx_matc_ok=no

AC_ARG_WITH(matc,
	[AC_HELP_STRING([--with-matc=<lib>], [use MATC library <lib>])])
case $with_matc in
	yes | "") ;;
	no) acx_matc_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) MATC_LIBS="$with_matc" ;;
	*) MATC_LIBS="-l$with_matc" ;;
esac

acx_matc_save_LIBS="$LIBS"

# First, check EIO_LIBS environment variable
if test $acx_matc_ok = no; then
if test "x$MATC_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$MATC_LIBS $LIBS"
	AC_MSG_CHECKING([for mtc_init in $MATC_LIBS])
	AC_TRY_LINK_FUNC(mtc_init, [acx_matc_ok=yes], [MATC_LIBS=""])
	AC_MSG_RESULT($acx_matc_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic MATC library?
if test "$acx_matc_ok" = no; then
	AC_CHECK_LIB(matc, mtc_init, [acx_matc_ok=yes; MATC_LIBS="-lmatc"],,[-lm])
fi

AC_SUBST(MATC_LIBS)

LIBS="$acx_matc_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_matc_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_MATC,1,[Define if you have a MATC library.]),[$1])
        :
else
        acx_matc_ok=no
        $2
fi
])dnl ACX_MATC
