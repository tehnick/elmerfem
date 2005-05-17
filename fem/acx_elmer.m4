dnl 
dnl Elmer specific M4sh macros 
dnl
dnl @version $Id: acx_elmer.m4,v 1.18 2005/05/16 12:18:51 vierinen Exp $
dnl @author juha.vierinen@csc.fi 5/2005
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

AC_LANG_PUSH(C++)

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
AC_LANG_POP(C++)
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

AC_LANG_PUSH(C++)
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
AC_LANG_POP(C++)
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
dnl @synopsis ACX_PARPACK([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Look for PARPACK library
dnl
AC_DEFUN([ACX_PARPACK], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
acx_parpack_ok=no

AC_ARG_WITH(parpack,
	[AC_HELP_STRING([--with-parpack=<lib>], [Specify location of PARPACK])])
case $with_parpack in
	yes | "") ;;
	no) acx_parpack_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) PARPACK_LIBS="$with_parpack" ;;
	*) PARPACK_LIBS="-l$with_parpack" ;;
esac

# Get fortran linker names of PARPACK functions to check for.
AC_FC_FUNC(pdneupd)

acx_parpack_save_LIBS="$LIBS"

LIBS="$BLAS_LIBS $LAPACK_LIBS $LIBS $FCLIBS $FLIBS"

# First, check PARPACK_LIBS environment variable
if test $acx_parpack_ok = no; then
if test "x$PARPACK_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$PARPACK_LIBS $LIBS"
	AC_MSG_CHECKING([for $pdneupd in $PARPACK_LIBS])
	AC_TRY_LINK_FUNC($pdneupd, [acx_parpack_ok=yes], [PARPACK_LIBS=""])
	AC_MSG_RESULT($acx_parpack_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic PARPACK library?
if test $acx_parpack_ok = no; then
	AC_CHECK_LIB(parpack, $pdneupd, [acx_parpack_ok=yes; PARPACK_LIBS="-lparpack"])
fi

AC_SUBST(PARPACK_LIBS)

LIBS="$acx_parpack_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_parpack_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_PARPACK,1,[Define if you have a PARPACK library.]),[$1])
        :
else
        acx_parpack_ok=no
        $2
fi
])dnl ACX_PARPACK



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
dnl look for the std c libraries:
dnl
dnl stdc++ (gnu)
dnl Cstd   (sun)
dnl C      (aix)
dnl 
dnl
AC_DEFUN([ACX_CHECK_STDCXXLIB],
[
AC_REQUIRE([AC_PROG_CXX])
acx_check_stdcxxlib_save_LIBS=$LIBS
acx_stdcxxlib_ok=no

AC_ARG_WITH(stdcxxlib,
	[AC_HELP_STRING([--with-stdcxxlib=<lib>], [use std c++ library <lib>])])

case $with_stdcxxlib in
	yes | "") ;;
	no) acx_stdcxxlib_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) STDCXX_LIBS="$with_stdcxxlib" ;;
	*) STDCXX_LIBS="-l$with_stdcxxlib" ;;
esac

if test x$STDCXX_LIBS != x; then
	save_LIBS="$LIBS"; LIBS="$STDCXX_LIBS $LIBS"
	AC_MSG_CHECKING([for main in $STDCXX_LIBS])
	AC_TRY_LINK_FUNC(main, [acx_stdcxx_lib_ok=yes], [STDCXX_LIBS=""])
	AC_MSG_RESULT($acx_stdcxx_lib_ok)
	LIBS="$save_LIBS"
fi

dnl check for stdc++
if test $acx_stdcxxlib_ok = no; then
	AC_CHECK_LIB(stdc++, main,[
				   STDCXX_LIBS="-lstdc++"
			           acx_stdcxxlib_ok=yes
                                  ])			
fi

dnl check for stdc++
if test $acx_stdcxxlib_ok = no; then
	AC_CHECK_LIB(Cstd, main,[
				   STDCXX_LIBS="-lCstd"
			           acx_stdcxxlib_ok=yes
                                  ])
fi

dnl check for stdc++
if test $acx_stdcxxlib_ok = no; then
	AC_CHECK_LIB(C, main,[
				   STDCXX_LIBS="-lC"
			           acx_stdcxxlib_ok=yes
                                  ])
fi

if test $acx_stdcxxlib_ok = no; then
	AC_MSG_ERROR([Couldn't find std c++ library that is needed for linking.])
fi

LIBS=$acx_check_stdcxxlib_save_LIBS
])

dnl find out the flags that enable 64 bit compilation
AC_DEFUN([ACX_CHECK_B64FLAGS],
[
AC_PREREQ([2.50])
AC_REQUIRE([AC_PROG_FC])
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_CXX])

AC_ARG_WITH(64bits,
	[AC_HELP_STRING([--with-64bits=yes(/no)], [Try to compile using 64 bits (default)])])


if test -z "$host"; then
  host=unknown
fi
canonical_host_type=$host
if test "$host" = unknown; then
  AC_MSG_ERROR([unknown system type, your build will most likely be screwed. quitting.])
fi

dnl by default, use no flags at all
B64FLAGS=
B64CFLAGS=
B64FCFLAGS=
B64F77FLAGS=
B64CXXFLAGS=

AC_MSG_CHECKING([for 64 bit compilation flags])

if test "$with_64bits" = no; then
   AC_MSG_RESULT(not even going to try)
else
   AC_MSG_RESULT([let's see what happens])

case "$canonical_host_type" in
  *-*-386bsd* | *-*-openbsd* | *-*-netbsd*)
  ;;
  *-*-freebsd*)
  ;;
  alpha*-dec-osf*)
  ;;
  *-*-darwin*)
  ;;
  *-*-cygwin* | *-*-mingw*)
  ;;
  *-*-linux* | *-*-gnu*)
        dnl -M64
	case "$FC" in
  	  ifort | ifc)
 		;;
	  g95 | gfortran)
		;;
	  *)
		;;
        esac
  ;;
  rs6000-ibm-aix* | powerpc-ibm-aix*)
  ;;
  hppa*-hp-hpux*)
  ;;
  *-sgi-*)
  ;;
  sparc-sun-sunos4*)
  ;;
  sparc-sun-solaris2* | i386-pc-solaris2*)
        B64FLAGS="-xtarget=native64 -KPIC"
	SUN_64BIT_FLAGS=$B64FLAGS
	case "$FC" in 
	  mpf* | f*)
		B64FCFLAGS=$SUN_64BIT_FLAGS
	  ;;
 	esac

	case "$F77" in
	  mpf* | f*)
		B64FFLAGS=$SUN_64BIT_FLAGS
	  ;;
	esac
	
	case "$CC" in
	  mpcc | mp*)
	        B64CFLAGS=$SUN_64BIT_FLAGS
	  ;;
 	esac
	
	case "$CXX" in 
	  mpCC | CC)
		B64CXXFLAGS=$SUN_64BIT_FLAGS
	  ;;
	esac
  ;;
esac

if test "$with_64bits" != no; then
        AC_MSG_CHECKING([for 64 bit CFLAGS])
        AC_MSG_RESULT($B64CFLAGS)
	CFLAGS="$CFLAGS $B64CFLAGS"

        AC_MSG_CHECKING([for 64 bit FCFLAGS])
        AC_MSG_RESULT($B64FCFLAGS)
	FCFLAGS="$FCFLAGS $B64FCFLAGS"

        AC_MSG_CHECKING([for 64 bit CXXFLAGS])
        AC_MSG_RESULT($B64CXXFLAGS)
	CXXFLAGS="$CXXFLAGS $B64CXXFLAGS"

        AC_MSG_CHECKING([for 64 bit FFLAGS])
        AC_MSG_RESULT($B64FFLAGS)
	FFLAGS="$FFLAGS $B64FFLAGS"
fi

fi

dnl let's see if it works...
AC_CHECK_SIZEOF(void*)
case "$ac_cv_sizeof_voidp" in
  "8")
    AC_DEFINE(ARCH_64_BITS, 1,[64 bit arch.]) 
  ;;
  "4")
    AC_DEFINE(ARCH_32_BITS, 1,[32 bit arch.]) 
  ;;
esac

if test "$with_64bits" != no; then
   AC_MSG_CHECKING(to see if we got 64 bits)

   if test "$ac_cv_sizeof_voidp" -ne 8; then
      AC_MSG_RESULT([nope]) 
   else
      AC_MSG_RESULT([oh yes]) 
   fi
fi

AC_SUBST(B64FLAGS)
])dnl ACX_CHECK_B64FLAGS

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

dnl
dnl We really need the old style cpp for preprocessing fortran.
dnl 
AC_DEFUN([ACX_PROG_TRADITIONAL_CPP], [
# sun mpcc -E leaves nasty # comment that chokes the fortran compiler, so we have to hope
# that ye olde cpp is present.
AC_CHECK_PROG(TRADITIONAL_CPP, cpp, yes, no)
if test "$TRADITIONAL_CPP" = yes; then
     CPP=cpp 
else
     AC_MSG_ERROR([Traditional cpp not found, just have to exit for for now.])
fi
AC_SUBST(CPP)
])



AC_DEFUN([ACX_FC_CHAR_MANGLING], [
AC_PREREQ(2.50)
AC_REQUIRE(AC_FC_FUNC)
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])

ac_cv_[]_AC_LANG_ABBREV[]_char_mangling="unknown"
AC_FC_FUNC(barf)
acx_fc_char_param_barf=$barf

AC_MSG_CHECKING([for fortran char mangling scheme])

AC_LANG_PUSH(C)
AC_COMPILE_IFELSE([
void $acx_fc_char_param_barf(char *name, int *l2, int *l3)
{
  printf("%d",*l2);
}],
[
   mv conftest.$ac_objext cfortran_test.$ac_objext
],
[
   ac_cv_[]_AC_LANG_ABBREV[]_char_mangling="unknown"
])
AC_LANG_POP(C)
AC_LANG_PUSH(Fortran)
AC_COMPILE_IFELSE
([    program foobar
        character(len=7) :: str
        str='my ass'
        str(7:7)=char(0)
        call barf(str,42)
     end program foobar
 ]
,
 [
   $FC -o conftest$ac_exeext cfortran_test.$ac_objext $conftest$ac_objext 
   AC_TRY_COMMAND(./conftest$ac_exeext)
   AC_MSG_RESULT($2)
 ]
,
 [
 AC_MSG_RESULT("unknown")
 ])
AC_LANG_POP(Fortran)
])
