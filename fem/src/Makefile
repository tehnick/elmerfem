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

# define $(SOLVER) and $(BINARIES)
include make.bin

all:	libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT)
# $(SOLVER) $(BINARIES)$(BINARIES_ODD)
# libfem$(LIB_EXT)

# define $(OBJS) that go into libsolver.so
include make.inc

.SUFFIXES: .f90

# a bit kludgy way to do the cpp, but hopefully portable enough
.f90.o:
	cp $< $*.fpp
	gcc -E -traditional-cpp -DDLLEXPORT="!" -g -O2 -I/tmp/testelmer/include $*.fpp > $*.F90
	gfortran -g -c $*.F90

# a bit kludgy way to do the cpp, but hopefully portable enough
.f.o:
	cp $< $*.fpp
	gcc -E -traditional-cpp -DDLLEXPORT="!" -g -O2 -I/tmp/testelmer/include $*.fpp > $*.F
	g77 -g -O2 -c $*.F

# Shared base lib
libsolver$(SHL_EXT): $(OBJS)
	rm -f $@
	$(CXX) -shared -Wl,-soname -Wl,$@ -o $@ -lblas -llapack -lhuti  -L/tmp/testelmer/lib  -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/ -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0 -L/lib// -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/// -L/usr/lib// -lf95 -lm  -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/../../.. -lfrtbegin -lg2c -lm -lgcc_s $(OBJS)

# The executable
ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT)
	gfortran -g -lblas -llapack -lhuti  -L/tmp/testelmer/lib  -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/ -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0 -L/lib// -L/home/vierinen/src/g95-install/bin/../lib/gcc-lib/i686-pc-linux-gnu/4.0.0/// -L/usr/lib// -lf95 -lm  -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/../../.. -lfrtbegin -lg2c -lm -lgcc_s -lsolver -o ElmerSolver$(EXE_EXT) -L. Solver$(OBJ_EXT)

install:
	@echo "Install stuff"

clean:
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so \#*

##################
# Cellar
#
# libfem.a obsolete?
#
# libfem$(LIB_EXT): $(OBJS)
#	rm -f $@
#	$(AR) rc $@ $(OBJS)
#	ranlib $@
