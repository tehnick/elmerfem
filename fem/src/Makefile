#*******************************************************************************
# Elmer Makefile.in 
#
# 1. build objects using dependencies from make.inc.
# 2. create libsolver.so using objects.
# 3. build individual solvers as dynamically linked libraries.
# 
# mpi_stubs, solver, binaries, odd binaries 
# 
# LIB	   = libfem.a
#
# make ext variables look better in makefile-mode than autoconf replacements
OBJ_EXT=.o
SHL_EXT=.so
LIB_EXT=.a
EXE_EXT=

include make.bin

all:	libfem$(LIB_EXT) libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT) $(BINARIES)

include make.inc

.SUFFIXES: .f90 .so

# a bit kludgy way to do the cpp, but hopefully portable enough
.f90.o:
	cp $< $*.fpp
	gcc -E -traditional-cpp -DDLLEXPORT="!" -g -O2 -I/tmp/testelmer//include $*.fpp > $*.F90
	g95 -g -c $*.F90

# a bit kludgy way to do the cpp, but hopefully portable enough
.f.o:
	cp $< $*.fpp
	gcc -E -traditional-cpp -DDLLEXPORT="!" -g -O2 -I/tmp/testelmer//include $*.fpp > $*.FOR
	g77 -g -O2 -c $*.FOR

# mpi stubs and load.c
.c.o:
	$(CC) -c -O  $<

# static fem lib
libfem$(LIB_EXT): $(OBJS)
	rm -f $@
	$(AR) rc $@ $(OBJS)
	ranlib $@

# Shared base lib (libs in the right place!)
libsolver$(SHL_EXT): libfem$(LIB_EXT) ElmerSolver$(OBJ_EXT)
	rm -f $@
	$(CXX) -shared -Wl,-soname -Wl,$@ -o $@ ElmerSolver$(OBJ_EXT) -lfem -ldl -lblas -llapack -larpack -lumfpack -lamd -lhuti -lmatc -leio -lmatc -lstdc++  -L/tmp/testelmer//lib  -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/ -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0 -L/lib// -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/// -L/usr/lib// -lf95 -lm  -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/../../.. -lfrtbegin -lg2c -lm -lgcc_s -L.

# The executable
ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT) libsolver$(SHL_EXT)
	g95 -lsolver -o ElmerSolver$(EXE_EXT) -L. Solver$(OBJ_EXT)

# build seperate solvers as shared libs
.f90.so:
	g95 -g -c $*.f90
	$(CXX) -shared -Wl,-soname -Wl,$@ -o $@ $*$(OBJ_EXT)  -lsolver -L.

install:
	@echo "Installing stuff to $(prefix),/tmp/testelmer/"
	mkdir -p /tmp/testelmer//lib
	mkdir -p /tmp/testelmer//include
	mkdir -p /tmp/testelmer//bin
	cp ElmerSolver$(EXEEXT) /tmp/testelmer//bin
	cp *$(SHL_EXT) /tmp/testelmer//lib
	cp *.mod /tmp/testelmer//include

clean:
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so \#*

##################
# Cellar
#
# libfem.a obsolete?
#
