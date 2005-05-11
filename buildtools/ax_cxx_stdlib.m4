dnl @synopsis AX_CXX_STDLIB([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Search for the standard c++ library 
dnl
dnl juha.vierinen[]@[]csc.fi 
dnl

AC_DEFUN([AX_CXX_STDLIB], [
AC_REQUIRE([AC_PROG_CXX])

ax_cxx_stdlib_ok=no

#
#
AC_ARG_WITH([stdlib],
            [AC_HELP_STRING([--with-stdlib],
                            [The standard c++ library.])])

case $with_stdlib in
	yes | "") ;;
	-* | */* | *.a | *.so | *.so.* | *.o) STDCXX_LIBS="$with_stdlib" ;;
	*) STDCXX_LIBS="-l$with_stdlib" ;;
esac
   
   if test X"$GL_LIBS" != X; then
	save_LIBS="$LIBS"; LIBS="$GL_LIBS $LIBS"
	AC_MSG_CHECKING([for glBegin in $GL_LIBS])
	AC_TRY_LINK_FUNC(glBegin, [ax_gl_ok=yes], [GL_LIBS=""])
	AC_MSG_RESULT($ax_gl_ok)
	LIBS="$save_LIBS"
   fi
fi

if test "$ax_gl_ok" = no; then

   AC_LANG_PUSH(C)

   AX_LANG_COMPILER_MS
   if test X$ax_compiler_ms = Xno; then
     GL_CFLAGS="${PTHREAD_CFLAGS}"
     GL_LIBS="${PTHREAD_LIBS} -lm"
   fi

  #
  # Use x_includes and x_libraries if they have been set (presumably by
  # AC_PATH_X).
  #
  if test "X$no_x" != "Xyes"; then
    if test -n "$x_includes"; then
      GL_CFLAGS="-I${x_includes} ${GL_CFLAGS}"
    fi
    if test -n "$x_libraries"; then
      GL_LIBS="-L${x_libraries} -lX11 ${GL_LIBS}"
    fi
  fi

  AC_CHECK_HEADERS([windows.h])

  AC_CACHE_CHECK([for OpenGL library], [ax_cv_check_gl_libgl],
  [ax_cv_check_gl_libgl="no"
  ax_save_CPPFLAGS="${CPPFLAGS}"
  CPPFLAGS="${GL_CFLAGS} ${CPPFLAGS}"
  ax_save_LIBS="${LIBS}"
  LIBS=""
  ax_check_libs="-lopengl32 -lGL"
  for ax_lib in ${ax_check_libs}; do
    if test X$ax_compiler_ms = Xyes; then
      ax_try_lib=`echo $ax_lib | sed -e 's/^-l//' -e 's/$/.lib/'`
    else
      ax_try_lib="${ax_lib}"
    fi
    LIBS="${ax_try_lib} ${GL_LIBS} ${ax_save_LIBS}"
    AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[
# if HAVE_WINDOWS_H && defined(_WIN32)
#   include <windows.h>
# endif
# include <GL/gl.h>]],
                     [[glBegin(0)]])],
    [ax_cv_check_gl_libgl="${ax_try_lib}"; break])
  done
  LIBS=${ax_save_LIBS}
  CPPFLAGS=${ax_save_CPPFLAGS}])

  if test "X${ax_cv_check_gl_libgl}" = "Xno"; then
    no_gl="yes"
    GL_CFLAGS=""
    GL_LIBS=""
  else
    GL_LIBS="${ax_cv_check_gl_libgl} ${GL_LIBS}"
  fi
  AC_LANG_POP(C)
fi
AC_SUBST([GL_CFLAGS])
AC_SUBST([GL_LIBS])

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$no_gl" != xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_OPENGL,1,[Define if you have a OpenGL library.]),[$1])
        :
else
        no_gl=yes
        $2
fi
])
