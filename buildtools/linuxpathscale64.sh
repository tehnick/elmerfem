#!/bin/sh 

export CC=pathcc
export CXX=pathCC
export F77=pathf90
export FC=pathf90
export ELMER_MODULES="umfpack matc post mathlibs eio hutiter fem"

sh compile.sh
