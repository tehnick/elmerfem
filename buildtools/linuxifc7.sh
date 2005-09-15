#!/bin/sh 

export CC=gcc
export CXX=g++
export F77=ifc
export FC=ifc

source /opt/intel/compiler70/ia32/bin/ifcvars.sh

sh compile.sh
