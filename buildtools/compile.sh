#!/bin/bash

modules="matc mathlibs eio hutiter fem"

TESTPREFIX=/tmp/vierinen/elmer
rm -Rf $TESTPREFIX

export CVSROOT="vierinen@corona.csc.fi:/home/csc/vierinen/cvsroot"
export CVS_RSH="ssh"

rm -Rf $modules
cvs co $modules

for m in $modules; do
    cd $m
    ./configure --prefix=$TESTPREFIX
    gmake -j42
    make install
    if test "$m" = fem; then
	make check
    fi
    cd ..
done