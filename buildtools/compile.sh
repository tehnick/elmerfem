#!/bin/bash
#
# Always compile into tmp
#

EPREFIX=/tmp/`whoami`/elmer
modules="umfpack matc mathlibs eio hutiter fem"

#
# if anything resembling a help flag is given, we print help 
#
if test "`echo $* |grep "\-h"`" != ""; then 
    printf "\n"
    printf " -----------------------\n"
    printf " Elmer build script v0.1\n"
    printf " -----------------------\n"
    printf "\n"
    printf "Fetches elmer from cvs into: ./build-HOST-DATE\n"
    printf "and compiles it into directory under: /tmp/username/build-HOST-DATE\n"
    printf "\n"
    printf "DATE is replaced with the current date and HOST with hostname.\n"
    printf "It is possible to do several builds at the same time,\n"
    printf "\n"
    printf "If ELMER_USER is unset, whoami (in the future anonymous) will be used to\n"
    printf "fetch the source.\n"
    printf "\n"
    exit
fi

if test "$ELMER_USER" != ""; then
    printf "Fetching source from CVS using CVSROOT=%s\n" "$ELMER_USER@corona.csc.fi:/home/csc/vierinen/cvsroot"
    export CVSROOT=$ELMER_USER@corona.csc.fi:/home/csc/vierinen/cvsroot
else
    printf "Fetching source from CVS using CVSROOT=%s\n" `whoami`@corona.csc.fi:/home/csc/vierinen/cvsroot
    export CVSROOT=`whoami`@corona.csc.fi:/home/csc/vierinen/cvsroot
fi
export CVS_RSH="ssh"

# uset ELMER_HOME and remove it from ld_library_path to avoid confusion
if test "$ELMER_HOME" != ""; then
    printf "Variable ELMER_HOME, will be cleared for the duration of the build.\n"
    printf "Also, \$ELMER_HOME/lib will be removed from LD_LIBRARY_PATH.\n"
    OLD_ELMER_HOME=$ELMER_HOME
    unset ELMER_HOME
    OLD_LD_LIBRARY_PATH=$LD_LIBRRARY_PATH
    LD_LIBRARY_PATH=`echo $LD_LIBRARY_PATH | sed -e "s,$ELMER_HOME/lib[/]*,,g"`
    export LD_LIBRARY_PATH

    printf "LD_LIBRARY_PATH is now: %s\n" $LD_LIBRARY_PATH
    printf "Original values will be restored after the compilation.\n"
fi

if test "$ELMER_LIB" != ""; then
    OLD_ELMER_LIB=$ELMER_LIB
    unset ELMER_LIB
fi

if test "$1" = "clean"; then
    # remove all temp files
    printf "Removing all temp files from %s\n" $EPREFIX
    rm -Rf $EPREFIX
    exit
fi

if test "$NPROCS" = ""; then
    NPROCS=1
fi
printf "Using %s processors for compiling\n" $NPROCS

datestr=`date '+%Y-%m-%d-%H-%M-%S'`
tmpdir=build.`hostname`.$datestr
mkdir -p $tmpdir
tmpname=build.`hostname`.$datestr

TESTPREFIX=$EPREFIX/$tmpname
rm -Rf $TESTPREFIX
printf "Using %s as build name\n" $tmpname

topdir=`pwd`
cd $tmpdir
cvs co $modules

for m in $modules; do
    cd $m
    if test "$USE_OWN_MATHLIBS" = yes; then
	./configure --prefix=$TESTPREFIX $CONFFLAGS --with-blas=$TESTPREFIX/lib/libblas.a --with-lapack=$TESTPREFIX/lib/liblapack.a
    else
	./configure --prefix=$TESTPREFIX $CONFFLAGS 
    fi
    gmake -j$NPROCS
    make install
    if test "$m" = fem; then
	make check
    fi
    cd ..
done

cd $topdir

# reset ELMER* variables if necessary
if test "$OLD_ELMER_HOME" != ""; then
    printf "Restoring old ELMER_HOME and LD_LIBRARY_PATH\n"
    export ELMER_HOME=$OLD_ELMER_HOME
    export LD_LIBRARY_PATH=$OLD_LD_LIBRRARY_PATH
fi

if test "$ELMER_LIB" != ""; then
    printf "Restoring old ELMER_LIB\n"
    export ELMER_LIB=$OLD_ELMER_LIB
fi
