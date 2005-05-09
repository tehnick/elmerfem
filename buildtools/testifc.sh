#!/bin/bash
#################
#
# Try out ifort
#
export TESTPREFIX=/tmp/elmer/ifc
export TESTNAME=ifc
export FC=ifort
export F77=ifort
export UMFPACK_LIBS="/home/vierinen/lib/libumfpack.a /home/vierinen/lib/libamd.a"
export BLAS_LIBS="/home/vierinen/lib/libf77blas.a /home/vierinen/lib/libatlas.a"
export LAPACK_LIBS="/home/vierinen/lib/liblapack.a"

sh compile.sh

export ELMER_HOME=$TESTPREFIX
echo "running tests"
export LD_LIBRARY_PATH=.:$TESTPREFIX/lib:/opt/intel_fc_80/lib/
export PATH=$TESTPREFIX/bin:$PATH
cd elmer-tests ; ./runtests.ifc 

