dnl 
dnl The big one.  
dnl 
dnl Preq. libs:
dnl eio-export_dynamic -shared -o libSolver.so ElmerSolver.o -L. -L$(LIBDIR) 
dnl
dnl -lfem -lhuti -lsparse -lumfpack -lamd -lmatc -leioF -lstdc++ -larpack -llapack -lblas -ldl -lf95 -lgcc -lgcc -lg2c
dnl
dnl Optional libs:
dnl -support different blas implementations
dnl 
dnl The plan is to first try automake+autoconf. 
dnl If that doesn't work out, just use autoconf for snooping 
dnl libraries and leave the rest to good old makefile.
dnl
dnl Things to find out:
dnl
dnl prefix
dnl platform
dnl f90,f77,cc,cpp,c++,ld,objext,shobjext,cp_cmd,rm_cmd
dnl cflags,fflags,ldflags
dnl

dnl
dnl Check host type and other basic things
dnl
AC_INIT(src/Solver.f90)
AC_PREREQ(2.57)

AC_CANONICAL_HOST
AC_CANONICAL_TARGET

if test -z "$host"; then
  host=unknown
fi
canonical_host_type=$host
if test "$host" = unknown; then
  AC_MSG_ERROR([configuring for unknown system type, your build will most likely be screwed.])
fi
AC_SUBST(canonical_host_type)

AC_LANG_COMPILER
AC_PROG_RANLIB
sinclude(acx_elmer.m4)
ACX_PROG_AR

AC_ISC_POSIX
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_HEADER_STDC
AC_C_BIGENDIAN

AC_PROG_CC
AC_PROG_CPP

AC_CHECK_FUNCS([gettimeofday mkdir sqrt ])
AC_CHECK_HEADERS([stdlib.h strings.h sys/param.h sys/time.h])
AC_FUNC_MALLOC
AC_HEADER_TIME
AC_STDC_HEADERS

AC_EXEEXT
AC_OBJEXT

dnl look for default fortran compiler (should we also define the standard?)
dnl others to look for: F77, 
AC_PROG_FC
dnl ([g95 f90 f77 xlf77 cf77 fort77 f90 xlf90])
AC_LANG_PREPROC(Fortran 90)
AC_F77_LIBRARY_LDFLAGS
AC_FC_LIBRARY_LDFLAGS
AC_F77_DUMMY_MAIN
AC_FC_DUMMY_MAIN
AC_ISC_POSIX

dnl create macros in config.h for obfuscating fortran-type function symbols
AC_F77_WRAPPERS
AC_FC_WRAPPERS

dnl setup name mangling
case "$ac_cv_f77_mangling" in
  "upper case, no underscore, no extra underscore" | \
  "upper case, no underscores")
    AC_DEFINE(ELMER_LINKTYP, 2, "Mangling: upper case, no underscore, no extra underscore") ;;
  "lower case, no underscore, no extra underscore" | \
  "lower case, no underscores" )
    AC_DEFINE(ELMER_LINKTYP, 3, "Mangling: lower case, no underscore, no extra underscore") ;;
  "lower case, underscore, no extra underscore" | \
  "lower case, single underscores")
    AC_DEFINE(ELMER_LINKTYP, 1, "Mangling: lower case, underscore, no extra underscore") ;;
  "lower case, underscore, extra underscore" | \
  "lower case, double underscores")
    AC_DEFINE(ELMER_LINKTYP, 1, "Mangling: lower case, underscore, extra underscore")
    # We aren't set up for f2c-style double underscoring.
    if test "$G77" = yes; then
      AC_MSG_WARN([
*** Not using option -fno-second-underscore with g95, sticking with canonical naming conventions.
])
dnl      FFLAGS="-fno-second-underscore $FFLAGS"
dnl      FCFLAGS="-fno-second-underscore $FCFLAGS"
    else
      AC_MSG_WARN([
*** Your Fortran compiler looks like f2c.
*** You can't currently link xdl_view routines with Fortran compiled with it.
*** You could use g77 with option `'-fno-second-underscore' instead.])
    fi ;;
  *)
    AC_MSG_ERROR([Unknown sort of Fortran name mangling]) ;;
esac

dnl 
dnl Checks for libraries:
dnl - blas
dnl - lapack
dnl - arpack
dnl - umfpack
dnl - hutiter
dnl - eio
dnl - matc

dnl
dnl Elmer prefix is an influential argument that is used 
dnl when searching for libraries in abnormal places.
dnl 
ACX_ELMER_PREFIX
dnl 
dnl Also, we'll look for libs and includes in the installation prefix
dnl 
LIBS="$LIBS -L$prefix/lib"
CFLAGS="$CFLAGS -I$prefix/include"

sinclude(acx_lapack.m4)
sinclude(acx_blas.m4)
ACX_BLAS([], [AC_MSG_ERROR([BLAS was not found!])])
ACX_LAPACK([], [AC_MSG_ERROR([LAPACK was not found!])])

dnl requires BLAS & LAPACK
ACX_HUTI([], [AC_MSG_ERROR([libhuti wasn't found.])])

dnl umfpack
# disable it for not since it is not used yet: need to check for "NO"
# AC_CHECK_UMFPACK(4.3.0) # at least umfpack 4.3
dnl AM_CONDITIONAL(HAVE_UMFPACK, test "x$umfpack_libs" != "x" )

dnl lapack, blas, g2c
dnl AC_CHECK_UMFPACK
dnl AM_CONDITIONAL(HAVE_UMFPACK, test "x$umfpack_libs" != "x" )


dnl look for our own libs.
dnl ACX_HUTI
dnl ACX_EIO
dnl ACX_MATC

LIBS="$BLAS_LIBS $LAPACK_LIBS $HUTI_LIBS $LIBS $FCLIBS $FLIBS"

dnl  
dnl OS-specific stuff
dnl first the canonical values and then the exceptions
dnl
DLFCN_DIR=
CPICFLAG=-fPIC
CXXPICFLAG=-fPIC
FPICFLAG=-fPIC
SHLEXT=so
SHLLIB='$(SHLEXT)'
SHLBIN=
SHLEXT_VER='$(SHLEXT).$(version)'
SHLLIB_VER='$(SHLLIB).$(version)'
SHLBIN_VER='$(SHLBIN).$(version)'
SHLLINKEXT=
LIBEXT=a
SH_LD='$(CXX)'
SH_LDFLAGS=-shared
DL_LD='$(SH_LD)'
DL_LDFLAGS='$(SH_LDFLAGS)'
MKOCTFILE_DL_LDFLAGS='$(DL_LDFLAGS)'
SONAME_FLAGS=
RLD_FLAG=
NO_OCT_FILE_STRIP=false
TEMPLATE_AR='$(AR)'
TEMPLATE_ARFLAGS="$ARFLAGS"
library_path_var=LD_LIBRARY_PATH
FCPPFLAGS=''
CP='cp'
RM='rm -f'
MV='mv'
LIBSOLVER_DEPS=$LIBS

dnl Should we use canonical host name or something more coarse grained?
ELMER_OSNAME="LINUX_PC"
dnl ELMER_CHARMANGLING = "char *"

case "$canonical_host_type" in
  *-*-386bsd* | *-*-openbsd* | *-*-netbsd*)
    SH_LD=ld
    SH_LDFLAGS=-Bshareable
  ;;
  *-*-freebsd*)
    SH_LD='$(CC)'
    SH_LDFLAGS=-shared -Wl,-x
  ;;
  alpha*-dec-osf*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    SH_LDFLAGS="-shared -Wl,-expect_unresolved -Wl,'*'"
    RLD_FLAG='-Wl,-rpath -Wl,$(octlibdir)'
  ;;
  *-*-darwin*)
    DL_LDFLAGS='-bundle -bundle_loader $(TOPDIR)/src/octave $(LDFLAGS)'
    MKOCTFILE_DL_LDFLAGS='-bundle -bundle_loader $(bindir)/octave-$(version)$(EXEEXT)'
    SH_LDFLAGS='-dynamiclib -single_module $(LDFLAGS)'
    CXXPICFLAG=
    CPICFLAG=
    FPICFLAG=
    SHLEXT=dylib 
    SHLLIB='$(SHLEXT)'
    SHLEXT_VER='$(version).$(SHLEXT)'
    SHLLIB_VER='$(version).$(SHLLIB)'
    NO_OCT_FILE_STRIP=true
    SONAME_FLAGS='-install_name $(octlibdir)/$@'
    library_path_var=DYLD_LIBRARY_PATH	
  ;;
  *-*-cygwin* | *-*-mingw*)
    CXXPICFLAG=
    CPICFLAG=
    FPICFLAG=
    SHLEXT=dll
    SHLLIB=dll.a
    SHLBIN=dll
    SH_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import"
    SHLLINKEXT=.dll
    SONAME_FLAGS='-Wl,--out-implib=$@.a'
    library_path_var=PATH
  ;;
  *-*-linux* | *-*-gnu*)
    MKOCTFILE_DL_LDFLAGS="-shared -Wl,-Bsymbolic"
    SONAME_FLAGS='-Wl,-soname -Wl,$@'
    RLD_FLAG='-Wl,-rpath -Wl,$(octlibdir)'
    FCPPFLAGS='-traditional-cpp'
  ;;
  i[[3456]]86-*-sco3.2v5*)
    SONAME_FLAGS='-Wl,-h -Wl,$@'
    RLD_FLAG=
    SH_LDFLAGS=-G
  ;;
  rs6000-ibm-aix* | powerpc-ibm-aix*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    DLFCN_DIR=dlfcn
  ;;
  hppa*-hp-hpux*)
    if test "$ac_cv_f77_compiler_gnu" = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=+Z
    fi
    SHLEXT=sl
    SH_LDFLAGS="-shared -fPIC"
    RLD_FLAG='-Wl,+b -Wl,$(octlibdir)'
  ;;
  *-sgi-*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    RLD_FLAG='-rpath $(octlibdir)'
  ;;
  sparc-sun-sunos4*)
    if test "$ac_cv_f77_compiler_gnu" = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=-PIC
    fi
    SH_LD=ld
    SH_LDFLAGS="-assert nodefinitions"
    RLD_FLAG='-L$(octlibdir)'
  ;;
  sparc-sun-solaris2* | i386-pc-solaris2*)
    if test "$ac_cv_f77_compiler_gnu" = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=-KPIC
    fi
    if test "$GCC" = yes; then
      CPICFLAG=-fPIC
    else
      CPICFLAG=-KPIC
    fi
    if test "$GXX" = yes; then
      CXXPICFLAG=-fPIC
      SH_LDFLAGS=-shared
    else
      CXXPICFLAG=-KPIC
      SH_LDFLAGS=-G
    fi
    RLD_FLAG='-R $(octlibdir)'
    # Template closures in archive libraries need a different mechanism.
    if test "$GXX" = yes; then
      true
    else
      TEMPLATE_AR='$(CXX)'
      TEMPLATE_ARFLAGS="-xar -o"
    fi
  ;;
esac

dnl
dnl Compiler specific stuff
dnl
dnl Known compilers:
dnl  f77/f90/f95: generic compiler names
dnl  g77: GNU Fortran 77 compiler
dnl  gfortran: putative GNU Fortran 95+ compiler (in progress)
dnl  fort77: native F77 compiler under HP-UX (and some older Crays)
dnl  frt: Fujitsu F77 compiler
dnl  pgf77/pgf90/pgf95: Portland Group F77/F90/F95 compilers
dnl  xlf/xlf90/xlf95: IBM (AIX) F77/F90/F95 compilers
dnl  lf95: Lahey-Fujitsu F95 compiler
dnl  fl32: Microsoft Fortran 77 "PowerStation" compiler
dnl  af77: Apogee F77 compiler for Intergraph hardware running CLIX
dnl  epcf90: "Edinburgh Portable Compiler" F90
dnl  fort: Compaq (now HP) Fortran 90/95 compiler for Tru64 and Linux/Alpha
dnl  ifc: Intel Fortran 95 compiler for Linux/x86
dnl  efc: Intel Fortran 95 compiler for IA64
dnl
case "$FC" in
  gfortran | g95)
    FCPPFLAGS="$FCPPFLAGS -DDLLEXPORT=\"!\""
  ;;
esac

if $use_rpath; then
  if test -n "$enable_rpath_arg"; then
    RLD_FLAG="$enable_rpath_arg"
  fi
else
  RLD_FLAG=""
fi

AC_MSG_RESULT([defining FPICFLAG to be $FPICFLAG])
AC_MSG_RESULT([defining CPICFLAG to be $CPICFLAG])
AC_MSG_RESULT([defining CXXPICFLAG to be $CXXPICFLAG])
AC_MSG_RESULT([defining SHLEXT to be $SHLEXT])
AC_MSG_RESULT([defining SHLLIB to be $SHLLIB])
AC_MSG_RESULT([defining SHLBIN to be $SHLBIN])
AC_MSG_RESULT([defining SHLEXT_VER to be $SHLEXT_VER])
AC_MSG_RESULT([defining SHLLIB_VER to be $SHLLIB_VER])
AC_MSG_RESULT([defining SHLBIN_VER to be $SHLBIN_VER])
AC_MSG_RESULT([defining SHLLINKEXT to be $SHLLINKEXT])
AC_MSG_RESULT([defining DLFCN_DIR to be $DLFCN_DIR])
AC_MSG_RESULT([defining SH_LD to be $SH_LD])
AC_MSG_RESULT([defining SH_LDFLAGS to be $SH_LDFLAGS])
AC_MSG_RESULT([defining DL_LD to be $DL_LD])
AC_MSG_RESULT([defining DL_LDFLAGS to be $DL_LDFLAGS])
AC_MSG_RESULT([defining MKOCTFILE_DL_LDFLAGS to be $MKOCTFILE_DL_LDFLAGS])
AC_MSG_RESULT([defining SONAME_FLAGS to be $SONAME_FLAGS])
AC_MSG_RESULT([defining NO_OCT_FILE_STRIP to be $NO_OCT_FILE_STRIP])
AC_MSG_RESULT([defining RLD_FLAG to be $RLD_FLAG])
AC_MSG_RESULT([defining TEMPLATE_AR to be $TEMPLATE_AR])
AC_MSG_RESULT([defining TEMPLATE_ARFLAGS to be $TEMPLATE_ARFLAGS])
AC_MSG_RESULT([defining library_path_var to be $library_path_var])
AC_MSG_RESULT([defining FCPPFLAGS to be $FCPPFLAGS])
AC_MSG_RESULT([defining LIBSOLVER_DEPS to be $LIBSOLVER_DEPS])
AC_SUBST(FPICFLAG)
AC_SUBST(CPICFLAG)
AC_SUBST(CXXPICFLAG)
AC_SUBST(SHLEXT)
AC_SUBST(SHLLIB)
AC_SUBST(SHLBIN)
AC_SUBST(SHLEXT_VER)
AC_SUBST(SHLLIB_VER)
AC_SUBST(SHLBIN_VER)
AC_SUBST(SHLLINKEXT)
AC_SUBST(DLFCN_DIR)
AC_SUBST(SH_LD)
AC_SUBST(SH_LDFLAGS)
AC_SUBST(DL_LD)
AC_SUBST(DL_LDFLAGS)
AC_SUBST(MKOCTFILE_DL_LDFLAGS)
AC_SUBST(SONAME_FLAGS)
AC_SUBST(NO_OCT_FILE_STRIP)
AC_SUBST(RLD_FLAG)
AC_SUBST(TEMPLATE_AR)
AC_SUBST(TEMPLATE_ARFLAGS)
AC_SUBST(library_path_var)
AC_SUBST(FCPPFLAGS)
AC_SUBST(LIBEXT)
AC_SUBST(LIBSOLVER_DEPS)
AC_SUBST(RM)
AC_SUBST(CP)
AC_SUBST(MV)
AC_SUBST(LIBS)

AC_CONFIG_HEADER(config.h)

AC_OUTPUT([
Makefile
src/Makefile]
)
