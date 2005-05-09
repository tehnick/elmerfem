#!/bin/bash
#################
#
# Try out ifort
#
export TESTPREFIX=/tmp/elmer/ifc
export TESTNAME=ifc
export FC=ifort
sh compile.sh

export ELMER_HOME=$TESTPREFIX
echo "running tests"
export OLD_LD_PATH=$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=.:$TESTPREFIX
cd elmer-tests ; ./runtests > tests.$TESTNAME ; cd ..

export LD_LIBRARY_PATH=$OLD_LD_PATH
