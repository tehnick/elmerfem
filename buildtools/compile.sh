#!/bin/bash
#
# Always compile into tmp
#
EPREFIX=/tmp/`whoami`/elmer
modules="matc mathlibs eio hutiter fem"
export CVSROOT="vierinen@corona.csc.fi:/home/csc/vierinen/cvsroot"
export CVS_RSH="ssh"

if test "$1" = "clean"; then
    rm -Rf $EPREFIX
fi

if test "$NPROCS" = ""; then
    NPROCS=1
fi

tmpdir=`mktemp -d build.XXXXXX`
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
done

cd $topdir
rm -Rf $tmpdir
