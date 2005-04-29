SUBDIRS=blas lapack arpack

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
OBJ_EXT=.@OBJEXT@
SHL_EXT=.@SHLEXT@
LIB_EXT=.@LIBEXT@
EXE_EXT=@EXEEXT@

include make.bin

all:	libfem$(LIB_EXT) libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT) $(BINARIES)

include make.inc

SUFFIXES: .f90 .so

# a bit kludgy way to do the cpp, but hopefully portable enough
.f90.o:
	@CP@ $< $*.fpp
	@CPP@ @FCPPFLAGS@ @CFLAGS@ $*.fpp > $*.F90
	@FC@ @FCFLAGS@ -c $*.F90

# a bit kludgy way to do the cpp, but hopefully portable enough
.f.o:
	@CP@ $< $*.fpp
	@CPP@ @FCPPFLAGS@ @CFLAGS@ $*.fpp > $*.FOR
	@F77@ @FFLAGS@ -c $*.FOR

# mpi stubs and load.c
.c.o:
	$(CC) -c -O  $<

# static fem lib
libfem$(LIB_EXT): $(OBJS)
	@RM@ $@
	@TEMPLATE_AR@ @TEMPLATE_ARFLAGS@ $@ $(OBJS)
	@RANLIB@ $@

# Shared base lib (libs in the right place!)
libsolver$(SHL_EXT): libfem$(LIB_EXT) ElmerSolver$(OBJ_EXT)
	@RM@ $@
	@SH_LD@ @SH_LDFLAGS@ @SONAME_FLAGS@ -o $@ ElmerSolver$(OBJ_EXT) @ELMER_FEM_DEPENDENCIES@ -lfem @LIBS@ -L.

# The executable
ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT) libsolver$(SHL_EXT)
	@FC@ -lsolver -o ElmerSolver$(EXE_EXT) -L. Solver$(OBJ_EXT)

# build seperate solvers as shared libs
.f90.@SHLEXT@:
	@FC@ @FCFLAGS@ -c $*.f90
	@SH_LD@ @SH_LDFLAGS@ @SONAME_FLAGS@ -o $@ $*$(OBJ_EXT)  -lsolver -L.

install:
	@echo "Installing stuff to $(prefix),@prefix@"
	@MKDIR@ @prefix@/lib
	@MKDIR@ @prefix@/include
	@MKDIR@ @prefix@/bin
	@CP@ ElmerSolver$(EXEEXT) @prefix@/bin
	@CP@ *$(SHL_EXT) @prefix@/lib
	@CP@ *.mod @prefix@/include
	@CP@ elements.def @prefix@/lib
	@CP@ SOLVER.KEYWORDS @prefix@/lib

clean:
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so \#*

##################
# Cellar
#
# libfem.a obsolete?
#
