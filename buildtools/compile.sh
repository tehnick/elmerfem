#!/bin/bash
#
# Always compile into tmp
#

EPREFIX=/tmp/`whoami`/elmer
modules="umfpack matc mathlibs eio hutiter fem"
export CVSROOT="vierinen@corona.csc.fi:/home/csc/vierinen/cvsroot"
export CVS_RSH="ssh"

if test "$1" = "clean"; then
    # remove all temp files
    rm -Rf $EPREFIX
    # remove all modules
    rm -Rf $modules
fi

if test "$NPROCS" = ""; then
    NPROCS=1
fi
datestr=`date '+%Y-%m-%d-%H-%M-%S'`
tmpdir=build.`hostname`.$datestr
mkdir -p $tmpdir
tmpname=build.`hostname`.$datestr

TESTPREFIX=$EPREFIX/$tmpname
rm -Rf $TESTPREFIX

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

