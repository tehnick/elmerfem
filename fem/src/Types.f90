!/******************************************************************************
! *
! *       ELMER, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 - , Center for Scientific Computing,
! *                                    Finland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! *****************************************************************************/
!
!/******************************************************************************
! *
! * Module containing solver type defs ...
! *
! ******************************************************************************
! *
! *                     Authors: Juha Ruokolainen,Jouni Malinen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2302
! *                   EMail: Juha.Ruokolainen@csc.fi,Jouni.Malinen@csc.fi
! *
! *                       Date: 01 Oct 1996
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

MODULE Types
 
   USE Messages

   INTEGER, PARAMETER :: MAX_NAME_LEN = 128

#if defined(ARCH_32_BITS)
   INTEGER, PARAMETER :: AddrInt = SELECTED_INT_KIND(9)
#else
  INTEGER, PARAMETER :: AddrInt = SELECTED_INT_KIND(18)
#endif

   INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12)

   REAL(KIND=dp), PARAMETER :: AEPS = 10 * EPSILON(1.0_dp), &
         PI = 3.1415926535897932384626433832795_dp
!------------------------------------------------------------------------------
  INTEGER, PARAMETER :: MATRIX_CRS  = 1, MATRIX_BAND = 2, MATRIX_SBAND = 3
!------------------------------------------------------------------------------
  INTEGER, PARAMETER :: SOLVER_EXEC_NEVER      = -1, &
                        SOLVER_EXEC_ALWAYS     =  0, &
                        SOLVER_EXEC_AHEAD_ALL  =  1, &
                        SOLVER_EXEC_AHEAD_TIME =  2, &
                        SOLVER_EXEC_AFTER_ALL  =  3, &
                        SOLVER_EXEC_AFTER_TIME =  4
!------------------------------------------------------------------------------
  TYPE Matrix_t
    TYPE(Matrix_t), POINTER :: Child, Parent, JacobiMatrix, EMatrix
    INTEGER :: NumberOfRows

    INTEGER :: Subband, FORMAT, SolveCount
    LOGICAL :: Ordered, Lumped, Symmetric, COMPLEX

    INTEGER(KIND=AddrInt) :: SPMatrix

    INTEGER, POINTER :: Perm(:),InvPerm(:),RowOwner(:)
    INTEGER, POINTER :: GRows(:), GOrder(:)
    INTEGER, POINTER :: Rows(:),Cols(:),Diag(:)

    REAL(KIND=dp), POINTER :: RHS(:),Force(:,:)

    REAL(KIND=dp),  POINTER :: Values(:),ILUValues(:)
    REAL(KIND=dp),  POINTER :: MassValues(:),DampValues(:)

    INTEGER, POINTER :: ILURows(:),ILUCols(:),ILUDiag(:)

!   For Complex systems, not used yet!:
!   -----------------------------------
    COMPLEX(KIND=dp), POINTER :: CRHS(:),CForce(:,:)

    COMPLEX(KIND=dp),  POINTER :: CValues(:),CILUValues(:)
    COMPLEX(KIND=dp),  POINTER :: CMassValues(:),CDampValues(:)

    TYPE(SParIterSolverGlobalD_t), POINTER :: ParMatrix
  END TYPE Matrix_t
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Typedefs for parallel solver 
!------------------------------------------------------------------------------

  TYPE ParEnv_t
     INTEGER                          :: PEs
     INTEGER                          :: MyPE
     LOGICAL                          :: Initialized
     LOGICAL, DIMENSION(:), POINTER   :: Active
     LOGICAL, DIMENSION(:), POINTER   :: IsNeighbour
     LOGICAL, DIMENSION(:), POINTER   :: SendingNB
     INTEGER                          :: NumOfNeighbours
  END TYPE ParEnv_t


  TYPE GlueTableT
     INTEGER, DIMENSION(:), POINTER :: Rows, Cols, Inds, RowOwner
  END TYPE GlueTableT


  TYPE VecIndicesT
     INTEGER, DIMENSION(:), POINTER :: RevInd
  END TYPE VecIndicesT


  TYPE IfVecT
     REAL(KIND=dp), DIMENSION(:), POINTER :: IfVec
  END TYPE IfVecT


  TYPE RHST
     REAL(KIND=dp), DIMENSION(:), POINTER :: RHSvec
     INTEGER, DIMENSION(:), POINTER :: RHSind
  END TYPE RHST


  TYPE DPBufferT
     REAL(KIND=dp), DIMENSION(:), POINTER :: DPBuf
  END TYPE DPBufferT


  TYPE ResBufferT
     REAL(KIND=dp), DIMENSION(:), POINTER :: ResVal
     INTEGER, DIMENSION(:), POINTER :: ResInd
  END TYPE ResBufferT


  TYPE IfLColsT
     INTEGER, DIMENSION(:), POINTER :: IfVec
  END TYPE IfLColsT


  TYPE SplittedMatrixT
     TYPE (Matrix_t), POINTER :: InsideMatrix
     TYPE (Matrix_t), DIMENSION(:), POINTER :: IfMatrix
     TYPE (Matrix_t), DIMENSION(:), POINTER :: NbsIfMatrix
     TYPE (VecIndicesT), DIMENSION(:), POINTER :: VecIndices
     TYPE (IfVecT), DIMENSION(:), POINTER :: IfVecs
     TYPE (IfLColsT), DIMENSION(:), POINTER :: IfLCols
     TYPE (GlueTableT), POINTER :: GlueTable
     TYPE (RHST), DIMENSION(:), POINTER :: RHS
     TYPE (ResBufferT), DIMENSION(:), POINTER :: ResBuf
     REAL(KIND=dp), POINTER :: Work(:,:),TmpXVec(:),TmpRVec(:)
  END TYPE SplittedMatrixT


  TYPE SParIterSolverGlobalD_t
     TYPE (SplittedMatrixT), POINTER :: SplittedMatrix
     TYPE (Matrix_t), POINTER :: Matrix
     TYPE (Nodes_t), POINTER :: Nodes
     TYPE(ParEnv_t) :: ParEnv
     INTEGER :: DOFs, RelaxIters
  END TYPE SParIterSolverGlobalD_t

  TYPE(SParIterSolverGlobalD_t), POINTER :: ParMatrix
DLLEXPORT ParMatrix

!-------------------------------------------------------------------------------

   !
   ! Basis function type
   !
   TYPE BasisFunctions_t 
      INTEGER :: n
      INTEGER, POINTER :: p(:),q(:),r(:)
      REAL(KIND=dp), POINTER :: coeff(:)
   END TYPE BasisFunctions_t


   !
   ! Element type description 
   !
   TYPE ElementType_t
     TYPE(ElementType_t),POINTER :: NextElementType ! this is a list of types

     INTEGER :: ElementCode                         ! numeric code for element

     INTEGER :: BasisFunctionDegree, &              ! linear or quadratic
                NumberOfNodes, &                
                NumberOfEdges, &                
                NumberOfFaces, &                
                DIMENSION                           ! 1=line, 2=surface, 3=volume

     INTEGER :: GaussPoints,GaussPoints2            ! number of gauss points to use

     REAL(KIND=dp) :: StabilizationMK               ! stab.param. depending on
                                                    ! interpolation type

     TYPE(BasisFunctions_t), POINTER :: BasisFunctions(:)
     REAL(KIND=dp), DIMENSION(:), POINTER :: NodeU, NodeV, NodeW
   END TYPE ElementType_t

!------------------------------------------------------------------------------

   TYPE ValueList_t
     TYPE(ValueList_t), POINTER :: Next

     INTEGER :: Model
     INTEGER :: TYPE

     REAL(KIND=dp), POINTER :: TValues(:)
     REAL(KIND=dp), POINTER :: FValues(:,:,:)

     LOGICAL :: LValue
     INTEGER, POINTER :: IValues(:)

#ifdef SGI
     INTEGER :: PROCEDURE
#else
     INTEGER(KIND=AddrInt) :: PROCEDURE
#endif

     CHARACTER(LEN=MAX_NAME_LEN) :: CValue

     INTEGER :: NameLen,DepNameLen
     CHARACTER(LEN=MAX_NAME_LEN) :: Name,DependName
   END TYPE ValueList_t

!------------------------------------------------------------------------------

   TYPE MaterialArray_t
     TYPE(ValueList_t), POINTER :: Values
   END TYPE MaterialArray_t

!------------------------------------------------------------------------------

   TYPE BoundaryConditionArray_t
     INTEGER :: TYPE,Tag
     TYPE(Matrix_t), POINTER :: PMatrix
     TYPE(ValueList_t), POINTER :: Values
   END TYPE BoundaryConditionArray_t

!------------------------------------------------------------------------------

   TYPE InitialConditionArray_t
     INTEGER :: TYPE,Tag
     TYPE(ValueList_t), POINTER :: Values
   END TYPE InitialConditionArray_t

!------------------------------------------------------------------------------

    TYPE BodyForceArray_t
      TYPE(ValueList_t), POINTER :: Values
    END TYPE BodyForceArray_t

!------------------------------------------------------------------------------

    TYPE BoundaryArray_t
      TYPE(ValueList_t), POINTER :: Values
    END TYPE BoundaryArray_t

!------------------------------------------------------------------------------

    TYPE BodyArray_t
      TYPE(ValueList_t), POINTER :: Values
    END TYPE BodyArray_t

!------------------------------------------------------------------------------

    TYPE EquationArray_t
      TYPE(ValueList_t), POINTER :: Values
    END TYPE EquationArray_t

!------------------------------------------------------------------------------

!   TYPE SimulationInfo_t
!     TYPE(ValueList_t), POINTER :: Values
!   END TYPE SimulationInfo_t

!------------------------------------------------------------------------------
   INTEGER, PARAMETER :: Variable_type_scalar    = 0
   INTEGER, PARAMETER :: Variable_type_2vector   = 1
   INTEGER, PARAMETER :: Variable_type_3vector   = 2
   INTEGER, PARAMETER :: Variable_type_2x2tensor = 3
   INTEGER, PARAMETER :: Variable_type_3x3tensor = 4

!  TYPE Variable_Component_t
!     CHARACTER(LEN=MAX_NAME_LEN) :: Name
!     INTEGER :: DOFs, Type
!  END TYPE Variable_Component_t

   TYPE Variable_t
     TYPE(Variable_t), POINTER   :: Next
     INTEGER :: NameLen
     CHARACTER(LEN=MAX_NAME_LEN) :: Name

     TYPE(Solver_t), POINTER :: Solver
     LOGICAL :: Valid, Output
     TYPE(Mesh_t), POINTER :: PrimaryMesh

!    TYPE(Variable_Component_t), POINTER :: Components(:)

     INTEGER :: DOFs
     INTEGER, POINTER          :: Perm(:)
     REAL(KIND=dp)             :: Norm
     COMPLEX(KIND=dp), POINTER :: EigenValues(:),EigenVectors(:,:)
     REAL(KIND=dp),    POINTER :: Values(:),PrevValues(:,:),PValues(:)
   END TYPE Variable_t

!------------------------------------------------------------------------------
   TYPE ListMatrix_t
      INTEGER :: INDEX
      TYPE(ListMatrix_t), POINTER :: Next
   END TYPE ListMatrix_t

   TYPE ListMatrixPointer_t
      INTEGER :: Degree, Level
      TYPE(ListMatrix_t), POINTER :: Head
   END TYPE ListMatrixPointer_t

!------------------------------------------------------------------------------

   TYPE Factors_t 
     INTEGER :: NumberOfFactors, NumberOfImplicitFactors
     INTEGER, POINTER :: Elements(:)
     REAL(KIND=dp), POINTER :: Factors(:)
   END TYPE Factors_t

!-------------------------------------------------------------------------------

   TYPE BoundaryInfo_t
     TYPE(Factors_t) :: ViewFactors,GebhardtFactors
     TYPE(Element_t), POINTER :: Left,Right
     INTEGER :: Constraint,LBody,LElement,RBody,RElement,OutBody
   END TYPE BoundaryInfo_t

!-------------------------------------------------------------------------------

   TYPE Element_t
     TYPE(ElementType_t), POINTER :: TYPE
     INTEGER :: BodyId
     REAL(KIND=dp) :: StabilizationMK,hK,hConvergence

     INTEGER :: Splitted

     TYPE(BoundaryInfo_t),  POINTER :: BoundaryInfo
     INTEGER :: NDOFs, BDOFs, DGDOFs
     INTEGER, DIMENSION(:), POINTER :: &
         NodeIndexes,   EdgeIndexes, FaceIndexes, &
         BubbleIndexes, DGIndexes

     TYPE(PElementDefs_t), POINTER :: PDefs
   END TYPE Element_t

!-------------------------------------------------------------------------------

   TYPE PElementDefs_t
      INTEGER :: P
      INTEGER :: TetraType       ! Type of p tetrahedron={0,1,2}
      LOGICAL :: isEdge          ! Is element an edge or face?
      INTEGER :: GaussPoints     ! Number of gauss points to use when using p elements
      LOGICAL :: pyramidQuadEdge ! Is element an edge of pyramid quad face?
      INTEGER :: localNumber     ! Local number of an edge or face for element on boundary
   END TYPE PElementDefs_t

!-------------------------------------------------------------------------------

   TYPE NeighbourList_t
     INTEGER, DIMENSION(:), POINTER :: Neighbours
   END TYPE NeighbourList_t

!------------------------------------------------------------------------------

   !
   ! Coordinate and vector type definition, coordinate arrays must be allocated
   ! prior to use of variables of this type.
   !
   TYPE Nodes_t
     INTEGER :: NumberOfNodes,TotalNodes
     REAL(KIND=dp), POINTER      :: x(:),y(:),z(:)
     LOGICAL, POINTER               :: INTERFACE(:)
     TYPE(NeighbourList_t),POINTER  :: NeighbourList(:)
     INTEGER, POINTER               :: GlobalNodeNumber(:),Perm(:),INVPerm(:)
     INTEGER :: NumberOfIfNodes
   END TYPE Nodes_t

!------------------------------------------------------------------------------

   TYPE QuadrantPointer_t
     TYPE(Quadrant_t), POINTER :: Quadrant
   END TYPE QuadrantPointer_t

!------------------------------------------------------------------------------

   TYPE Quadrant_t
     INTEGER, DIMENSION(:), POINTER :: Elements
     REAL(KIND=dp) :: SIZE, MinElementSize, BoundingBox(6)
     INTEGER :: NElemsInQuadrant
     TYPE(QuadrantPointer_t), DIMENSION(:), POINTER :: ChildQuadrants
   END TYPE Quadrant_t

!------------------------------------------------------------------------------

   TYPE Projector_t
     TYPE(Projector_t), POINTER :: Next
     TYPE(Mesh_t), POINTER :: Mesh
     TYPE(Matrix_t), POINTER :: Matrix, TMatrix
   END TYPE Projector_t

!------------------------------------------------------------------------------

   TYPE Mesh_t
     CHARACTER(MAX_NAME_LEN) :: Name
     TYPE(Mesh_t), POINTER   :: Next,Parent,Child

     TYPE(Projector_t), POINTER :: Projector
     TYPE(Quadrant_t), POINTER  :: RootQuadrant

     LOGICAL :: Changed, OutputActive, Stabilize
     INTEGER :: SavesDone, AdaptiveDepth
     
     TYPE(Variable_t), POINTER :: Variables

     TYPE(Nodes_t), POINTER :: Nodes
     TYPE(Element_t), DIMENSION(:), POINTER :: Elements, Edges, Faces

     INTEGER :: NumberOfNodes, NumberOfBulkElements, NumberOfEdges, &
                NumberOfFaces, NumberOfBoundaryElements
     INTEGER :: MaxElementNodes, MaxElementDOFs, MaxEdgeDOFs, MaxFaceDOFs, MaxBDOFs
   END TYPE Mesh_t

!------------------------------------------------------------------------------

!   TYPE Constants_t
!     REAL(KIND=dp) :: Gravity(4)
!     REAL(KIND=dp) :: StefanBoltzmann
!   END TYPE Constants_t

!------------------------------------------------------------------------------

    TYPE Solver_t
      TYPE(ValueList_t), POINTER :: Values

      INTEGER :: TimeOrder,DoneTime,Order,NOFEigenValues
      INTEGER(KIND=AddrInt) :: PROCEDURE, LinBeforeProc, LinAfterProc

      REAL(KIND=dp) :: Alpha,Beta,dt

      INTEGER :: SolverExecWhen

      INTEGER :: MultiGridLevel,  MultiGridTotal, MultiGridSweep
      LOGICAL :: MultiGridSolver, MultiGridEqualSplit
      TYPE(Mesh_t), POINTER :: Mesh

      INTEGER :: NumberOfActiveElements
      INTEGER, POINTER :: ActiveElements(:)

      TYPE(Matrix_t),   POINTER :: Matrix
      TYPE(Variable_t), POINTER :: Variable
    END TYPE Solver_t

!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
    TYPE Model_t
!------------------------------------------------------------------------------
!
!     Coodrinate system dimension + type
!
      INTEGER :: DIMENSION, CoordinateSystem
!
!     Model dimensions
!
      INTEGER :: NumberOfBulkElements, &
                 NumberOfNodes,        &
                 NumberOfBoundaryElements
!
!     Simulation input data, that concern the model as a whole
!
      TYPE(ValueList_t), POINTER :: Simulation
!
!     Variables
!
      TYPE(Variable_t), POINTER  :: Variables
!
!     Some physical constants, that will be read from the database or set by
!     other means: gravity direction/intensity and Stefan-Boltzmann constant)
!
      TYPE(ValueList_t), POINTER :: Constants
!
!     Types  of  equations (flow,heat,...) and  some  parameters (for example
!     laminar or turbulent flow or type of convection model for heat equation,
!     etc.)
!
      INTEGER :: NumberOfEquations
      TYPE(EquationArray_t), POINTER :: Equations(:)
!
!     Active bodyforces: (bussinesq approx., heatsource, freele chosen
!     bodyforce...)
!
      INTEGER :: NumberOfBodyForces
      TYPE(BodyForceArray_t), POINTER :: BodyForces(:)
!
!     Initial conditions for field variables
!
      INTEGER :: NumberOfICs
      TYPE(InitialConditionArray_t), POINTER :: ICs(:)
!
!     Boundary conditions
!
      INTEGER :: NumberOfBCs
      TYPE(BoundaryConditionArray_t), POINTER :: BCs(:)
!
!     For free surfaces the curvatures...
!
      INTEGER, POINTER :: FreeSurfaceNodes(:)
      REAL(KIND=dp), POINTER :: BoundaryCurvatures(:)
!
!     Material parameters
!
      INTEGER :: NumberOfMaterials
      TYPE(MaterialArray_t), POINTER :: Materials(:)
!
!     Active bodies, every element has a pointer to a body, body has
!     material,ICs,bodyforces and equations
!
      INTEGER :: NumberOfBodies 
      TYPE(BodyArray_t), POINTER :: Bodies(:)
!
!      Boundary to boundary condition mapping
!
      INTEGER :: NumberOfBoundaries
      INTEGER, POINTER :: BoundaryId(:)
      TYPE(BoundaryArray_t), POINTER :: Boundaries(:)
!
!     Linear equation solvers
!
      INTEGER :: NumberOfSolvers
      TYPE(Solver_t), POINTER :: Solvers(:)
!
!     Node coordinates + info for parallel computations
!
      TYPE(Nodes_t), POINTER :: Nodes
!
!     Max number of nodes in any one element in this model
!
      INTEGER :: MaxElementNodes
!
!     Elements
!
      TYPE(Element_t),DIMENSION(:), POINTER :: Elements
!
!     For reference the current element in process   
!
      TYPE(Element_t), POINTER :: CurrentElement
!
!     These are for internal use,   number of potentially nonzero elements
!     in stiffness and mass matrices (with one dof), and number of nonzero
!     elements in rows of the matrices.
!
      INTEGER :: TotalMatrixElements
      INTEGER, POINTER :: RowNonzeros(:)

      TYPE(Mesh_t), POINTER :: Meshes

      TYPE(Mesh_t),   POINTER :: Mesh
      TYPE(Solver_t), POINTER :: Solver
    END TYPE Model_t

    TYPE(Model_t),  POINTER :: CurrentModel
    TYPE(Matrix_t), POINTER :: GlobalMatrix
!------------------------------------------------------------------------------
END MODULE Types
!------------------------------------------------------------------------------
