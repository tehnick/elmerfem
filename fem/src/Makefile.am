SUBDIRS=arpack parpack view3d viewaxis

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

# libfem$(LIB_EXT)
all:	 libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT) $(BINARIES) $(BINARIES_ODD) ViewFactors$(EXE_EXT) GebhardtFactors$(EXE_EXT)

include make.inc

SUFFIXES: .f90 .so

##########################
# Rules for building stuff
#
# a bit kludgy way to do the cpp, but hopefully portable enough
# 
.f90.@OBJEXT@:
	@CP@ $< $*.fpp
	@CPP@ @FCPPFLAGS@ @CFLAGS@ $*.fpp > $*.F90
	@FC@ @FCFLAGS@ -c $*.F90

# a bit kludgy way to do the cpp, but hopefully portable enough
.f.@OBJEXT@:
	@CP@ $< $*.fpp
	@CPP@ @FCPPFLAGS@ @CFLAGS@ $*.fpp > $*.FOR
	@F77@ @FFLAGS@ -c $*.FOR

# mpi stubs and load.c
#.c.@OBJEXT@:
#	$(CC) -c -O  $<

# build seperate solvers as shared libs
.f90.@SHLEXT@:
	@CP@ $< $*.fpp
	@CPP@ @FCPPFLAGS@ @CFLAGS@ $*.fpp > $*.F90
	@FC@ @FCFLAGS@ -c $*.F90
	@SH_LD@ @SH_LDFLAGS@ @SH_LDTOFLAGS@ @SONAME_FLAGS@ -o $@ $*$(OBJ_EXT)  -lsolver -L. @EXTRA_LIBS@

###############################
# libsolver.so
#
libsolver$(SHL_EXT): ElmerSolver$(OBJ_EXT) $(OBJS) arpack parpack
	@RM@ $@
	@SH_LD@ @SH_LDFLAGS@ @SONAME_FLAGS@ -o $@ ElmerSolver$(OBJ_EXT) $(OBJS) @LIBS@ -L. 

#########################
# The executables
#
ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT) libsolver$(SHL_EXT)
	@SH_LD2@ @SH_LDTOFLAGS@ -lsolver -o ElmerSolver$(EXE_EXT) -L. Solver$(OBJ_EXT) @EXTRA_LIBS@

ViewFactors$(EXE_EXT): libsolver$(SHL_EXT) ViewFactors$(OBJ_EXT)
	@SH_LD2@ @SH_LDTOFLAGS@ -lsolver -o ViewFactors$(EXE_EXT) -L. ViewFactors$(OBJ_EXT) viewaxis/libviewaxis.a view3d/libview3d.a  @EXTRA_LIBS@

GebhardtFactors$(EXE_EXT): libsolver$(SHL_EXT) GebhardtFactors$(OBJ_EXT)
	@SH_LD2@ @SH_LDTOFLAGS@ -lsolver -o GebhardtFactors$(EXE_EXT) -L. GebhardtFactors$(OBJ_EXT) viewaxis/libviewaxis.a view3d/libview3d.a  @EXTRA_LIBS@

################################33
# Local installation rules
#
install-exec-local:
	@echo "Installing stuff to $(prefix),@prefix@"
	@MKDIR@ @prefix@/lib
	@MKDIR@ @prefix@/include
	@MKDIR@ @prefix@/bin
	@CP@ ElmerSolver$(EXEEXT) @prefix@/bin
	@CP@ ElmerSolver$(EXEEXT) @prefix@/bin/Solver$(EXEEXT)
	@CP@ ViewFactors$(EXEEXT) @prefix@/bin
	@CP@ GebhardtFactors$(EXEEXT) @prefix@/bin
	@CP@ *$(SHL_EXT) @prefix@/lib
	@CP@ *.mod @prefix@/include
	@CP@ elements.def @prefix@/lib
	@CP@ SOLVER.KEYWORDS @prefix@/lib

clean-local:
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so \#*

####################################
# Automake knows how to do this
#
bin_PROGRAMS = SC2Elmer 
SC2Elmer_SOURCES = SC2Elmer.c


# static fem lib
# libfem$(LIB_EXT): $(OBJS)
#	@RM@ $@
#	@TEMPLATE_AR@ @TEMPLATE_ARFLAGS@ $@ $(OBJS) 
#	@RANLIB@ $@

