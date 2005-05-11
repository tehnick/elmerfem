dnl @synopsis ACX_CHECK_TCL/TK([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
dnl
dnl A really simple check for some tcl/tk combinations, please
dnl send me a better one: juha.vierinen[]@[]csc.fi
dnl
AC_DEFUN([ACX_CHECK_TCL], [
AC_PREREQ(2.50)
acx_tcl_ok=no

AC_ARG_WITH(tcl,
	[AC_HELP_STRING([--with-tcl=<lib>], [use Tcl library <lib>])])
case $with_tcl in
	yes | "") ;;
	no) acx_tcl_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) TCL_LIBS="$with_tcl" ;;
	*) TCL_LIBS="-l$with_tcl" ;;
esac

acx_tcl_save_LIBS="$LIBS"

LIBS="$LIBS"

# First, check TCL_LIBS environment variable
if test $acx_tcl_ok = no; then
if test "x$TCL_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$TCL_LIBS $LIBS"
	AC_MSG_CHECKING([for Tcl_MainLoop in $TCL_LIBS])
	AC_TRY_LINK_FUNC(Tcl_MainLoop, [acx_tcl_ok=yes], [TCL_LIBS=""])
	AC_MSG_RESULT($acx_tcl_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic TCL library?
if test "$acx_tcl_ok" = no; then
	AC_CHECK_LIB(tcl, Tcl_MainLoop, [acx_tcl_ok=yes; TCL_LIBS="-ltcl"])
fi

AC_SUBST(TCL_LIBS)

LIBS="$acx_eioc_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_tcl_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_TCL,1,[Define if you have a Tcl library.]),[$1])
        :
else
        acx_tcl_ok=no
        $2
fi
])dnl ACX_CHECK_TCL

AC_DEFUN([ACX_CHECK_TK], [
AC_PREREQ(2.50)
acx_tk_ok=no

AC_ARG_WITH(tk,
	[AC_HELP_STRING([--with-tk=<lib>], [use Tk library <lib>])])
case $with_tk in
	yes | "") ;;
	no) acx_tk_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) TK_LIBS="$with_tk" ;;
	*) TK_LIBS="-l$with_tk" ;;
esac

acx_eioc_save_LIBS="$LIBS"

LIBS="$LIBS"

# First, check TK_LIBS environment variable
if test $acx_tk_ok = no; then
if test "x$TK_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$TK_LIBS $LIBS"
	AC_MSG_CHECKING([for Tk_Init in $TK_LIBS])
	AC_TRY_LINK_FUNC(Tk_Init, [acx_tk_ok=yes], [TK_LIBS=""])
	AC_MSG_RESULT($acx_tk_ok)
	LIBS="$save_LIBS"
fi
fi

# Generic TK library?
if test "$acx_tk_ok" = no; then
	AC_CHECK_LIB(tk, Tk_Init, [acx_tk_ok=yes; TK_LIBS="-ltk"])
fi

AC_SUBST(TK_LIBS)

LIBS="$acx_tk_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_tk_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_TK,1,[Define if you have the Tk library.]),[$1])
        :
else
        acx_tk_ok=no
        $2
fi
])dnl ACX_CHECK_TK
