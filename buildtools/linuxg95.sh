#!/bin/sh 

export CC=gcc
export CXX=g++
export F77=g77
export FC=g95
export ELMER_MODULES="umfpack meshgen2d elmergrid matc post mathlibs eio front hutiter fem post front"
export USE_OWN_MATHLIBS="yes"

sh compile.sh

