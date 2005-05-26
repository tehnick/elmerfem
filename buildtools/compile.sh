#!/bin/bash
#
# Always compile into tmp
#
EPREFIX=/tmp/`whoami`/elmer
modules="matc mathlibs eio hutiter fem"
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

tmpdir=build.`hostname``date '+%Y%M%S'`
mkdir -p $tmpdir
tmpname=`hostname``date '+%Y%M%S'`

TESTPREFIX=$EPREFIX/$tmpname
rm -Rf $TESTPREFIX

topdir=`pwd`
cd $tmpdir
cvs co $modules

for m in $modules; do
    cd $m
    ./configure --prefix=$TESTPREFIX $CONFFLAGS
    gmake -j$NPROCS
    make install
    if test "$m" = fem; then
	make check
    fi
    cd ..
    rm -Rf $m
done

cd $topdir
# rm -Rf $tmpdir
