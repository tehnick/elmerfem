SUBDIRS=elmergrid meshgen2d arpack parpack view3d viewaxis

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

# dependencies
include make.bin
# automatic object dependencies for .f90 files
include make.dep

# attach to automake all target
all-local:   mpif libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT) $(BINARIES) $(BINARIES_ODD) ViewFactors$(EXE_EXT) GebhardtFactors$(EXE_EXT)

SUFFIXES: .f90 .so .F95

.f90.F95:
	@CPP@ @FCPPFLAGS@ $< > $*.F95

.F95.@OBJEXT@:
	@FC@ @FCFLAGS@ -c $<

libsolver$(SHL_EXT): $(SOLVEROBJS) arpack parpack 
	@RM@ $@
	@SH_LD2@ @SH_LDFLAGS@ @B64FLAGS@ @SONAME_FLAGS@ -o $@ $(SOLVEROBJS) @SOLVER_LIBS@ -L.

.@OBJEXT@.@SHLEXT@: 
	@SH_LD@ @SH_LDFLAGS@ @SH_LINKING_TO_FLAGS@ @B64FLAGS@ @SONAME_FLAGS@ -o $@ $*$(OBJ_EXT) -L. -lsolver @EXTRA_LIBS@

$(BINARIES) $(BINARIES_ODD): libsolver$(SHL_EXT)

$(SOLVEROBJS): mpif

ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT) libsolver$(SHL_EXT) 
	@SH_LD@ @SH_LINKING_TO_FLAGS@ @B64FLAGS@ -o ElmerSolver$(EXE_EXT) Solver$(OBJ_EXT) -L. -lsolver @EXTRA_LIBS@ 

ViewFactors$(EXE_EXT): libsolver$(SHL_EXT) ViewFactors$(OBJ_EXT)  
	@SH_LD@ @SH_LINKING_TO_FLAGS@ @B64FLAGS@ -o ViewFactors$(EXE_EXT) ViewFactors$(OBJ_EXT) -L. -lsolver @EXTRA_LIBS@ viewaxis/libviewaxis.a view3d/libview3d.a  

GebhardtFactors$(EXE_EXT): libsolver$(SHL_EXT) GebhardtFactors$(OBJ_EXT)  
	@SH_LD@ @SH_LINKING_TO_FLAGS@ @B64FLAGS@ -o GebhardtFactors$(EXE_EXT) GebhardtFactors$(OBJ_EXT) -L. -lsolver @EXTRA_LIBS@ viewaxis/libviewaxis.a view3d/libview3d.a  

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
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so \#* mpif mpif.h

###############################
# Automake knows how to do this 
#
bin_PROGRAMS = SC2Elmer 
SC2Elmer_SOURCES = SC2Elmer.c

###############################
# If no mpif.h found, but mpi still seems kosher, then use a stub mpif.h
if USE_LOCAL_MPIF_H
mpif:
	@CP@ mpif_stub.h mpif.h
	@CP@ mpif_stub.h mpif
else 
mpif:
	@CP@ mpif_stub.h mpif
endif


# all source files that automake doesn't know about in this directory.
EXTRA_DIST = \
	Acoustics.f90 \
	Adaptive.f90 \
	AdvectionDiffusion.f90 \
	ApertureAmplitude.f90 \
	AplacExport.f90 \
	ArtificialCompressibility.f90 \
	BandMatrix.f90 \
	BandwidthOptimize.f90 \
	CoordinateSystems.f90 \
	CPUTime.c \
	CRSMatrix.f90 \
	DCRComplexSolve.f90 \
	DefUtils.f90 \
	Differentials.f90 \
	DiffuseConvectiveAnisotropic.f90 \
	DiffuseConvectiveGeneralAnisotropic.f90 \
	DirectSolve.f90 \
	DissipativeHelmholtz.f90 \
	EigenSolve.f90 \
	ElasticSolve.f90 \
	ElectricForce.f90 \
	Electrokinetics.f90 \
	ElementDescription.f90 \
	ElementUtils.f90 \
	EliminateDirichlet.f90 \
	EliminatePeriodic.f90 \
	ElmerSolver.f90 \
	EnergyRelease.f90 \
	FDiffusion3D.f90 \
	FDiffusion.f90 \
	FindOptimum.f90 \
	FlowSolve.f90 \
	FluidicForce.f90 \
	FreeSurface.f90 \
	FreeSurfaceReduced.f90 \
	FreeSurfaceSolver.f90 \
	GebhardtFactors.f90 \
	GeneralUtils.f90 \
	HashTable.f90 \
	HeatSolve.f90 \
	HelmholtzBEM.f90 \
	HelmholtzSolve.f90 \
	huti_fdefs.h \
	IMeshToMesh.f90 \
	Integration.f90 \
	Interpolation.f90 \
	IrrotationalVelocity.f90 \
	IterSolve.f90 \
	KESolver.f90 \
	LinearAlgebra.f90 \
	Lists.f90 \
	Load.c \
	LUDecomposition.f90 \
	MagneticSolve.f90 \
	MagneticW1Solve.f90 \
	MainUtils.f90 \
	MaterialModels.f90 \
	MaxwellAxiS.f90 \
	Maxwell.f90 \
	MaxwellGeneral.f90 \
	MEMLumping.f90 \
	MEMReynolds.f90 \
	MEMStatElecReduced.f90 \
	MEMUtilities.f90 \
	MeshSolve.f90 \
	MeshUtils.f90 \
	Messages.f90 \
	MGPrec.f90 \
	ModelDescription.f90 \
	mpif_linux.h \
	mpif_stub.h \
	stubut.c \
	Multigrid.f90 \
	NavierStokesCylindrical.f90 \
	NavierStokes.f90 \
	NavierStokesGeneral.f90 \
	ParallelEigenSolve.f90 \
	ParallelUtils.f90 \
	PElementBase.f90 \
	PElementMaps.f90 \
	PhaseChange.f90 \
	PoissonBEM.f90 \
	PoissonBoltzmannSolve.f90 \
	Radiation.f90 \
	RadiationFactors.f90 \
	RateOfChange.f90 \
	ReloadInput.f90 \
	ResultToPost.f90 \
	ResultToResult.f90 \
	ReynoldsEquation.f90 \
	ReynoldsUtilities.f90 \
	RigidBodyReduction.f90 \
	SaveData.f90 \
	ShearWave.f90 \
	Smitc.f90 \
	SolveBand.f90 \
	solve_cmplx.f90 \
	solve.f90 \
	solve_real.f90 \
	Solver.f90 \
	SolverUtils.f90 \
	SolveSBand.f90 \
	SParIterComm.f90 \
	SParIterGlobals.f90 \
	SParIterPrecond.f90 \
	SParIterSolver.f90 \
	SparseMatrix.f90 \
	StatCurrentSolve.f90 \
	StatElecReduced.f90 \
	StatElecSolve.f90 \
	StatMagSolve.f90 \
	StreamSolver.f90 \
	Stress.f90 \
	StressGeneral.f90 \
	StressSolve.f90 \
	TimeIntegrate.f90 \
	TransportEquation.f90 \
	Types.f90 \
	ViewFactors.f90 \
	Walls.f90 
