#!/bin/sh 

export CC=gcc
export CXX=g++
export F77=g77
export FC=g95
export LDFLAGS="-L/usr/lib/gcc-lib/i686-pc-cygwin/3.3.3/ -lgcc"

sh compile.sh
