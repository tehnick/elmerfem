#!/bin/sh

#
# Kludge: copying umfpack to dest dir.
#
mkdir -p $(TESTPREFIX)/lib; cp libumfpack.a libamd.a $(TESTPREFIX)/lib
./banner -w 30 "MATC"
cd matc; ./configure --prefix=$TESTPREFIX; make clean ; make ; make install ; cd ..
./banner -w 30 "EIO"
cd eio; ./configure --prefix=$TESTPREFIX; make clean ; make ; make install ; cd ..
./banner -w 30 "HUTI"
cd hutiter; ./configure --prefix=$TESTPREFIX ; make clean ; make ; make install ; cd ..
./banner -w 30 "GRID"
cd elmergrid; ./configure --prefix=$TESTPREFIX ; make clean ; make ; make install ; cd ..
./banner -w 30 "MESHGEN"
cd meshgen2d; ./configure --prefix=$TESTPREFIX ; make clean ; make ; make install ; cd ..
./banner -w 30 "FEM"
cd fem; ./configure --prefix=$TESTPREFIX ; make clean ; make ; make install ; cd ..
