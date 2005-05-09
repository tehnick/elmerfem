#!/bin/bash
#
# shell script that compiles and tests elmer on two different compilers
#
#################
#
# Try out g95 
#
export TESTPREFIX=/tmp/elmer/g95
export TESTNAME=g95
export CC=gcc
export CXX=g++
export FC=g95
export F77=g77
export CFLAGS="-O"
export FCFLAGS="-O"
export FFFLAGS="-O"
export UMFPACK_LIBS="/home/vierinen/lib/libumfpack.a /home/vierinen/lib/libamd.a"
export BLAS_LIBS="/home/vierinen/lib/libf77blas.a /home/vierinen/lib/libatlas.a"
export LAPACK_LIBS="/home/vierinen/lib/liblapack.a"

sh compile.sh

export ELMER_HOME=$TESTPREFIX
echo "running tests"
export LD_LIBRARY_PATH=.:$TESTPREFIX/lib
export PATH=$TESTPREFIX/bin:$PATH
cd elmer-tests ; ./runtests.g95 


