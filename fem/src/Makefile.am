SUBDIRS=arpack parpack view3d viewaxis

OBJ_EXT=.$(OBJEXT)
SHL_EXT=.$(SHLEXT)
LIB_EXT=.$(LIBEXT)
EXE_EXT=$(EXEEXT)

include make.bin
include make.dep

NORMAL_TARGETS=mpif libsolver$(SHL_EXT) ElmerSolver$(EXE_EXT) $(BINARIES) $(BINARIES_ODD) ViewFactors$(EXE_EXT) GebhardtFactors$(EXE_EXT)
MPI_TARGETS=libsolver_mpi$(SHL_EXT) ElmerSolver_mpi$(EXE_EXT)

if USE_MPI
ELMER_TARGETS=$(NORMAL_TARGETS) $(MPI_TARGETS)
else
ELMER_TARGETS=$(NORMAL_TARGETS)
endif

all-local: $(ELMER_TARGETS)

SUFFIXES: .f90 .so .src

.src.f90:
	$(CPP) $(FCPPFLAGS) $< > $*.f90

.f90.$(OBJEXT):
	$(FC) $(FCFLAGS) -c $<

libsolver$(SHL_EXT): $(SOLVEROBJS) arpack stubut.$(OBJEXT)
	$(RM) $@
	$(SH_LD2) $(SH_LDFLAGS) $(B64FLAGS) $(LDFLAGS) -o $@ $(SOLVEROBJS) stubut.$(OBJEXT) $(SOLVER_LIBS) -L.

.$(OBJEXT).$(SHLEXT): 
	$(SH_LD) $(SH_LDFLAGS) $(SH_LINKING_TO_FLAGS) $(B64FLAGS) -o $@ $*$(OBJ_EXT) $(EXTRA_LIBS)

$(BINARIES) $(BINARIES_ODD): libsolver$(SHL_EXT)

$(SOLVEROBJS): mpif

ElmerSolver_mpi$(EXE_EXT): Solver$(OBJ_EXT) libsolver_mpi$(SHL_EXT)
	$(SH_LD) $(SH_LINKING_TO_FLAGS) $(B64FLAGS) $(LDFLAGS) -o ElmerSolver_mpi$(EXE_EXT) Solver$(OBJ_EXT) -L. -lsolver_mpi $(EXTRA_LIBS)

libsolver_mpi$(SHL_EXT): $(SOLVEROBJS) arpack parpack
	$(RM) $@
	$(SH_LD2) $(SH_LDFLAGS) $(B64FLAGS) $(LDFLAGS) -o $@ $(SOLVEROBJS) $(SOLVER_LIBS) $(MPI_LIBS) -L.

ElmerSolver$(EXE_EXT): Solver$(OBJ_EXT) libsolver$(SHL_EXT) 
	$(SH_LD) $(SH_LINKING_TO_FLAGS) $(B64FLAGS) $(LDFLAGS) -o ElmerSolver$(EXE_EXT) Solver$(OBJ_EXT) -L. -lsolver $(EXTRA_LIBS)

ViewFactors$(EXE_EXT): libsolver$(SHL_EXT) ViewFactors$(OBJ_EXT)  
	$(SH_LD) $(SH_LINKING_TO_FLAGS) $(B64FLAGS) $(LDFLAGS) -o ViewFactors$(EXE_EXT) ViewFactors$(OBJ_EXT) -L. -lsolver $(EXTRA_LIBS) viewaxis/libviewaxis.a view3d/libview3d.a

GebhardtFactors$(EXE_EXT): libsolver$(SHL_EXT) GebhardtFactors$(OBJ_EXT)  
	$(SH_LD) $(SH_LINKING_TO_FLAGS) $(B64FLAGS) $(LDFLAGS) -o GebhardtFactors$(EXE_EXT) GebhardtFactors$(OBJ_EXT) -L. -lsolver $(EXTRA_LIBS) viewaxis/libviewaxis.a view3d/libview3d.a

ELMER_SOLVER_DATADIR=$(prefix)/share/elmersolver
install-data-local:
	@echo "Instaling solvers to ${ELMER_SOLVER_DATADIR}/lib"

	mkdir -p $(ELMER_SOLVER_DATADIR)/lib
	$(CP) *$(SHL_EXT) $(ELMER_SOLVER_DATADIR)/lib
	$(CP) elements.def $(ELMER_SOLVER_DATADIR)/lib
	$(CP) SOLVER.KEYWORDS $(ELMER_SOLVER_DATADIR)/lib

	@echo "Instaling *.mod to ${ELMER_SOLVER_DATADIR}/include"
	mkdir -p $(ELMER_SOLVER_DATADIR)/include
	$(CP) *.mod $(ELMER_SOLVER_DATADIR)/include

install-exec-local:
	@echo "Installing stuff to ${prefix}/bin"
	$(MKDIR) $(prefix)/bin
	$(CP) ElmerSolver$(EXEEXT) $(prefix)/bin
	$(CP) ElmerSolver$(EXEEXT) $(prefix)/bin/Solver$(EXEEXT)
	$(CP) ViewFactors$(EXEEXT) $(prefix)/bin
	$(CP) GebhardtFactors$(EXEEXT) $(prefix)/bin

clean-local:
	$(RM) $(ALL) *.o *.mod *.F90 *.F *.fpp *~ *.a *.so *.dll \#* mpif mpif.h

bin_PROGRAMS = SC2Elmer 
SC2Elmer_SOURCES = SC2Elmer.c

if USE_LOCAL_MPIF_H
mpif:
	$(CP) mpif_stub.h mpif.h
	$(CP) mpif_stub.h mpif
else 
mpif:
	$(CP) mpif_stub.h mpif
endif

EXTRA_DIST = \
	Acoustics.src \
	Adaptive.src \
	AdvectionDiffusion.src \
	ApertureAmplitude.src \
	AplacExport.src \
	ArtificialCompressibility.src \
	BandMatrix.src \
	BandwidthOptimize.src \
	CoordinateSystems.src \
	CPUTime.c \
	CRSMatrix.src \
	DCRComplexSolve.src \
	DefUtils.src \
	Differentials.src \
	DiffuseConvectiveAnisotropic.src \
	DiffuseConvectiveGeneralAnisotropic.src \
	DirectSolve.src \
	DissipativeHelmholtz.src \
	EigenSolve.src \
	ElasticSolve.src \
	ElectricForce.src \
	Electrokinetics.src \
	ElementDescription.src \
	ElementUtils.src \
	EliminateDirichlet.src \
	EliminatePeriodic.src \
	ElmerSolver.src \
	EnergyRelease.src \
	FDiffusion3D.src \
	FDiffusion.src \
	FindOptimum.src \
	FlowSolve.src \
	FluidicForce.src \
	FreeSurface.src \
	FreeSurfaceReduced.src \
	FreeSurfaceSolver.src \
	GebhardtFactors.src \
	GeneralUtils.src \
	HashTable.src \
	HeatSolve.src \
	HelmholtzBEM.src \
	HelmholtzSolve.src \
	IMeshToMesh.src \
	Integration.src \
	Interpolation.src \
	IrrotationalVelocity.src \
	IterSolve.src \
	KESolver.src \
	LinearAlgebra.src \
	Lists.src \
	Load.c \
	LUDecomposition.src \
	MagneticSolve.src \
	MagneticW1Solve.src \
	MainUtils.src \
	MaterialModels.src \
	MaxwellAxiS.src \
	Maxwell.src \
	MaxwellGeneral.src \
	MEMLumping.src \
	MEMReynolds.src \
	MEMStatElecReduced.src \
	MEMUtilities.src \
	MeshSolve.src \
	MeshUtils.src \
	Messages.src \
	MGPrec.src \
	ModelDescription.src \
	mpif_stub.h \
	stubut.c \
	Multigrid.src \
	NavierStokesCylindrical.src \
	NavierStokes.src \
	NavierStokesGeneral.src \
	ParallelEigenSolve.src \
	ParallelUtils.src \
	PElementBase.src \
	PElementMaps.src \
	PhaseChange.src \
	PoissonBEM.src \
	PoissonBoltzmannSolve.src \
	Radiation.src \
	RadiationFactors.src \
	RateOfChange.src \
	ReloadInput.src \
	ResultOutputSolve.src \
	ResultToPost.src \
	ResultToResult.src \
	ReynoldsEquation.src \
	ReynoldsUtilities.src \
	RigidBodyReduction.src \
	SaveData.src \
	ShearWave.src \
	Smitc.src \
	SolveBand.src \
	solve_cmplx.src \
	solve_real.src \
	Solver.src \
	SolverUtils.src \
	SolveSBand.src \
	SParIterComm.src \
	SParIterGlobals.src \
	SParIterPrecond.src \
	SParIterSolver.src \
	SparseMatrix.src \
	StatCurrentSolve.src \
	StatElecReduced.src \
	StatElecSolve.src \
	StatMagSolve.src \
	StreamSolver.src \
	Stress.src \
	StressGeneral.src \
	StressSolve.src \
	TimeIntegrate.src \
	TransportEquation.src \
	Types.src \
	ViewFactors.src \
	Walls.src \
	elements.def \
	SOLVER.KEYWORDS
