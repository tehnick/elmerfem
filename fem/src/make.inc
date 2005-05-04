OBJS	=	mpi_stubs$(OBJ_EXT) 				\
		NavierStokes$(OBJ_EXT)				\
		NavierStokesGeneral$(OBJ_EXT)	     		\
		NavierStokesCylindrical$(OBJ_EXT)		\
		Lists$(OBJ_EXT)					\
		DiffuseConvectiveAnisotropic$(OBJ_EXT)		\
		DiffuseConvectiveGeneralAnisotropic$(OBJ_EXT)	\
		PElementMaps$(OBJ_EXT)				\
		PElementBase$(OBJ_EXT)				\
		ElementDescription$(OBJ_EXT)			\
		Integration$(OBJ_EXT)				\
		ModelDescription$(OBJ_EXT)			\
		GeneralUtils$(OBJ_EXT)				\
		Stress$(OBJ_EXT)				\
		StressGeneral$(OBJ_EXT)				\
		LinearAlgebra$(OBJ_EXT)		 	        \
		CoordinateSystems$(OBJ_EXT)			\
		CRSMatrix$(OBJ_EXT)				\
		BandMatrix$(OBJ_EXT)				\
		BandwidthOptimize$(OBJ_EXT)			\
		MaterialModels$(OBJ_EXT)			\
		FlowSolve$(OBJ_EXT)				\
		HeatSolve$(OBJ_EXT)				\
		StressSolve$(OBJ_EXT)				\
		DirectSolve$(OBJ_EXT)				\
		IterSolve$(OBJ_EXT)				\
		TimeIntegrate$(OBJ_EXT)				\
		Types$(OBJ_EXT)					\
		SolveBand$(OBJ_EXT)				\
		ElementUtils$(OBJ_EXT)				\
		Radiation$(OBJ_EXT)				\
		Load$(OBJ_EXT)					\
		Differentials$(OBJ_EXT)				\
		FreeSurface$(OBJ_EXT)				\
		Maxwell$(OBJ_EXT)				\
		MaxwellAxiS$(OBJ_EXT)				\
		MaxwellGeneral$(OBJ_EXT)			\
		MagneticSolve$(OBJ_EXT)				\
		Walls$(OBJ_EXT)					\
		SolverUtils$(OBJ_EXT)				\
		SolveSBand$(OBJ_EXT)				\
		CPUTime$(OBJ_EXT)				\
		Interpolation$(OBJ_EXT)				\
		IMeshToMesh$(OBJ_EXT)				\
		MainUtils$(OBJ_EXT)				\
		MeshSolve$(OBJ_EXT)				\
		Adaptive$(OBJ_EXT)				\
		EigenSolve$(OBJ_EXT)				\
		HashTable$(OBJ_EXT)				\
		MeshUtils$(OBJ_EXT)				\
		SParIterGlobals$(OBJ_EXT)			\
		SParIterComm$(OBJ_EXT)				\
		SParIterPrecond$(OBJ_EXT)			\
		SParIterSolver$(OBJ_EXT)			\
		huti_bicgstab_2_D$(OBJ_EXT)			\
		Messages$(OBJ_EXT)				\
		Multigrid$(OBJ_EXT)				\
		ParallelUtils$(OBJ_EXT)                         \
                ParallelEigenSolve$(OBJ_EXT)			\
		solve_cmplx$(OBJ_EXT)				\
		solve_real$(OBJ_EXT)				\
		MGPrec$(OBJ_EXT)				\
		DefUtils$(OBJ_EXT)				\
		LUDecomposition$(OBJ_EXT) 			\
	        RadiationFactors$(OBJ_EXT)                      \
		ResultToResult$(OBJ_EXT)


#
#
#  Dependencies ... !
#
Types$(OBJ_EXT):				\
	Messages$(OBJ_EXT)


ElementDescription$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	Lists$(OBJ_EXT)				\
	PElementBase$(OBJ_EXT)			\
	Integration$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	LUDecomposition$(OBJ_EXT)               \
	LinearAlgebra$(OBJ_EXT)

PElementBase$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	PElementMaps$(OBJ_EXT)			

LUDecomposition$(OBJ_EXT):  			\
	Types$(OBJ_EXT)


Integration$(OBJ_EXT):				\
	Types$(OBJ_EXT)


CRSMatrix$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	GeneralUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)

BandMatrix$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	GeneralUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)

ModelDescription$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	ElementDescription$(OBJ_EXT)		\
	HashTable$(OBJ_EXT)			\
	MeshUtils$(OBJ_EXT)			\
	Lists$(OBJ_EXT)

GeneralUtils$(OBJ_EXT):				\
	Types$(OBJ_EXT)

BandwidthOptimize$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	Lists$(OBJ_EXT)				\
	ElementDescription$(OBJ_EXT)

ElementUtils$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	BandwidthOptimize$(OBJ_EXT)		\
	CRSMatrix$(OBJ_EXT)			\
	BandMatrix$(OBJ_EXT)			\
	Integration$(OBJ_EXT)			\
	Interpolation$(OBJ_EXT)			\
	SParIterGlobals$(OBJ_EXT)		\
	ElementDescription$(OBJ_EXT)

DiffuseConvectiveAnisotropic$(OBJ_EXT):		\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	Differentials$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)

DiffuseConvectiveGeneralAnisotropic$(OBJ_EXT):	\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)			\
	Differentials$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)

NavierStokes$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	MaterialModels$(OBJ_EXT)		\
	Differentials$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)

NavierStokes_C$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

NavierStokesGeneral$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)			\
	Differentials$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)

NavierStokesDiagonalMetric$(OBJ_EXT):		\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)

NavierStokesCylindrical$(OBJ_EXT):		\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)			\
	Differentials$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)

Maxwell$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

MaxwellAxiS$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

MaxwellGeneral$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

Stress$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

StressGeneral$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	Integration$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	GeneralUtils$(OBJ_EXT)

FreeSurface$(OBJ_EXT):				\
	Differentials$(OBJ_EXT)			\
	IterSolve$(OBJ_EXT)			\
	ElementUtils$(OBJ_EXT)

FlowSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Adaptive$(OBJ_EXT)			\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	TimeIntegrate$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	DefUtils$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	FreeSurface$(OBJ_EXT)			\
	Differentials$(OBJ_EXT)			\
	NavierStokes$(OBJ_EXT)			\
	NavierStokesGeneral$(OBJ_EXT)		\
	NavierStokesCylindrical$(OBJ_EXT)	\
	Lists$(OBJ_EXT)

HeatSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Adaptive$(OBJ_EXT)			\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	TimeIntegrate$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	DefUtils$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	Radiation$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	DiffuseConvectiveAnisotropic$(OBJ_EXT)		\
	DiffuseConvectiveGeneralAnisotropic$(OBJ_EXT)	\
	Lists$(OBJ_EXT)

StressSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	TimeIntegrate$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	DefUtils$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	Stress$(OBJ_EXT)			\
	StressGeneral$(OBJ_EXT)			\
	Lists$(OBJ_EXT)

MeshSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	TimeIntegrate$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	DefUtils$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	Stress$(OBJ_EXT)			\
	Lists$(OBJ_EXT)


MagneticSolve$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
	ElementUtils$(OBJ_EXT)			\
	ElementDescription$(OBJ_EXT)		\
	TimeIntegrate$(OBJ_EXT)			\
	GeneralUtils$(OBJ_EXT)			\
	DefUtils$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)		\
	Maxwell$(OBJ_EXT)			\
	MaxwellAxiS$(OBJ_EXT)			\
	MaxwellGeneral$(OBJ_EXT)		\
	Lists$(OBJ_EXT)

DirectSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	BandMatrix$(OBJ_EXT)			\
	CRSMatrix$(OBJ_EXT)
 
IterSolve$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	BandMatrix$(OBJ_EXT)			\
	CRSMatrix$(OBJ_EXT)

Multigrid$(OBJ_EXT):				\
	ParallelUtils$(OBJ_EXT)			\
	DirectSolve$(OBJ_EXT)

MGPrec$(OBJ_EXT):				\
	Multigrid$(OBJ_EXT)

EigenSolve$(OBJ_EXT):				\
	Multigrid$(OBJ_EXT)			\
	CRSMatrix$(OBJ_EXT)

ParallelEigenSolve$(OBJ_EXT):			\
	Multigrid$(OBJ_EXT)			\
	ParallelUtils$(OBJ_EXT)			\
	CRSMatrix$(OBJ_EXT)

SolverUtils$(OBJ_EXT):				\
	ElementUtils$(OBJ_EXT)			\
	TimeIntegrate$(OBJ_EXT)			\
	BandMatrix$(OBJ_EXT)			\
	IterSolve$(OBJ_EXT)			\
	EigenSolve$(OBJ_EXT)			\
	ParallelEigenSolve$(OBJ_EXT)		\
	Multigrid$(OBJ_EXT)			\
	MeshUtils$(OBJ_EXT)			\
	ParallelUtils$(OBJ_EXT)			\
	DirectSolve$(OBJ_EXT)

DefUtils$(OBJ_EXT):				\
	SolverUtils$(OBJ_EXT)

MaterialModels$(OBJ_EXT):			\
	Types$(OBJ_EXT)				\
        ModelDescription$(OBJ_EXT)		\
	ElementDescription$(OBJ_EXT)		\
	CoordinateSystems$(OBJ_EXT)

Lists$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	GeneralUtils$(OBJ_EXT)

IMeshToMesh$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	Interpolation$(OBJ_EXT)

Interpolation$(OBJ_EXT):			\
	ElementDescription$(OBJ_EXT)		\
	Integration$(OBJ_EXT)

ElmerSolver$(OBJ_EXT):				\
	MainUtils$(OBJ_EXT)

Adaptive$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	ElementDescription$(OBJ_EXT)		\
	Radiation$(OBJ_EXT)			\
	SolverUtils$(OBJ_EXT)			\
	Lists$(OBJ_EXT)

MainUtils$(OBJ_EXT):				\
	ModelDescription$(OBJ_EXT)		\
	SolverUtils$(OBJ_EXT)

Radiation$(OBJ_EXT):				\
	Types$(OBJ_EXT)				\
	ElementUtils$(OBJ_EXT)			\
	CoordinateSystems$(OBJ_EXT)

HashTable$(OBJ_EXT):				\
	Lists$(OBJ_EXT)

MeshUtils$(OBJ_EXT):				\
	ElementUtils$(OBJ_EXT)			\
	ParallelUtils$(OBJ_EXT)

ParallelUtils$(OBJ_EXT):			\
	SParIterSolver$(OBJ_EXT)

SParIterSolver$(OBJ_EXT):			\
	SParIterPrecond$(OBJ_EXT)		\
	SParIterComm$(OBJ_EXT)

SParIterPrecond$(OBJ_EXT):			\
	SParIterComm$(OBJ_EXT)

SParIterComm$(OBJ_EXT):				\
	CRSMatrix$(OBJ_EXT)			\
	SParIterGlobals$(OBJ_EXT)

ResultToResult$(OBJ_EXT): 			\
	SParIterComm$(OBJ_EXT)			\
	Types$(OBJ_EXT) \
	Lists$(OBJ_EXT) \
	Integration$(OBJ_EXT) \
	Interpolation$(OBJ_EXT) \
	MainUtils$(OBJ_EXT) \
	CoordinateSystems$(OBJ_EXT) 

MEMReynolds$(SHL_EXT): \
	MEMUtilities$(OBJ_EXT)

MEMStatElecReduced$(SHL_EXT): \
	MEMUtilities$(OBJ_EXT)




