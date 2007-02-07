SUBROUTINE StreamSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: StreamSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the StreamFunction of the flow field.
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh, materials, BCs, etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear & nonlinear equation solver options
!
!  REAL(KIND=dp) :: dt,
!     INPUT: Timestep size for time dependent simulations
!
!  LOGICAL :: TransientSimulation
!     INPUT: Steady state or transient simulation
!
!******************************************************************************
  USE SolverUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Matrix_t), POINTER  :: StiffMatrix
  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t), POINTER :: CurrentElement
  TYPE(Variable_t), POINTER :: FlowSol

  CHARACTER(LEN=MAX_NAME_LEN) :: FlowVariableName

  REAL(KIND=dp), POINTER :: StreamFunction(:), ForceVector(:), FlowSolValues(:)
  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), LocalForce(:), LoadVector(:)
  REAL(KIND=dp) :: Norm

  INTEGER, POINTER :: NodeIndexes(:), Perm(:), FlowSolPerm(:)
  INTEGER :: k, t, i, n, DOFs, istat, NSDOFs, FirstNode

  LOGICAL :: AllocationsDone = .FALSE., Found, Shifting, Scaling, StokesStream

  SAVE LocalStiffMatrix, LocalForce, ElementNodes, AllocationsDone, LoadVector

  CALL Info('StreamSolver',' ')
  CALL Info('StreamSolver','----------------------------')
  CALL Info('StreamSolver','STREAMSOLVER')
  CALL Info('StreamSolver','----------------------------')
  CALL Info('StreamSolver',' ')

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------
  IF ( .NOT.ASSOCIATED( Solver % Matrix ) ) RETURN

  StreamFunction => Solver % Variable % Values
  DOFs = Solver % Variable % DOFs
  Perm => Solver % Variable % Perm

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS


!------------------------------------------------------------------------------
! Get initial values ( Default is FlowSolvers 'Flow Solution' )
!------------------------------------------------------------------------------
  FlowVariableName = ListGetString( Solver % Values, 'Stream Function Velocity Variable', Found )
  IF ( .NOT. Found ) THEN
     CALL Info( 'StreamSolver', 'Stream Function Velocity Variable set to Flow Solution' )
     FlowVariableName = 'Flow Solution'
  END IF

  FlowSol => VariableGet( Solver % Mesh % Variables, FlowVariableName )
  IF ( ASSOCIATED( FlowSol ) ) THEN
     FlowSolPerm => FlowSol % Perm
     FlowSolValues => FlowSol % Values
     NSDOFs = FlowSol % DOFs
  ELSE
     CALL Warn( 'StreamSolver', 'No variable for velocity associated.' )
     CALL Warn( 'StreamSolver', 'Quitting execution of StreamSolver.' ) 
     RETURN
  END IF

!------------------------------------------------------------------------------
! Get keyword values
!------------------------------------------------------------------------------
  FirstNode = ListGetInteger( Solver % Values, 'Stream Function First Node', Found )
  IF ( .NOT. Found ) THEN
     CALL Info( 'StreamSolver', 'Stream Function First Node set to 1.' )
     FirstNode = 1
  END IF

  IF ( FirstNode < 1 ) THEN
     CALL Warn( 'StreamSolver', 'Given Stream Function First Node is non-positive.' )
     CALL Info( 'StreamSolver', 'Stream Function First Node set to 1.' )
     FirstNode = 1
  END IF

  n = Solver % Mesh % NumberOfNodes
  IF ( FirstNode > n ) THEN
     CALL Warn( 'StreamSolver', 'Given Stream Function First Node is too big.' )
     WRITE( Message, *) 'Stream Function First Node set to ', n
     CALL Info( 'StreamSolver', Message )
     FirstNode = n
  END IF

  Shifting = ListGetLogical( Solver % Values, 'Stream Function Shifting', Found )
  IF ( .NOT. Found ) THEN
     CALL Info( 'StreamSolver', 'Stream Function Shifting set to .TRUE.' )
     Shifting = .TRUE.
  END IF

  Scaling = ListGetLogical( Solver % Values, 'Stream Function Scaling', Found )
  IF ( .NOT. Found ) THEN
     CALL Info( 'StreamSolver', 'Stream Function Scaling set to .FALSE.' )
     Scaling = .FALSE.
  END IF

  StokesStream = ListGetLogical( Solver % Values, 'Stokes Stream Function', Found )
  IF ( .NOT. Found ) THEN
     IF ( Coordinates == AxisSymmetric ) THEN
        CALL Info( 'StreamSolver', 'Stokes Stream Function set to .TRUE.' )
        StokesStream = .TRUE.
     ELSE
        CALL Info( 'StreamSolver', 'Stokes Stream Function set to .FALSE.' )
        StokesStream = .FALSE.
     END IF
  END IF

  IF ( Coordinates == AxisSymmetric .AND. .NOT. StokesStream ) THEN
     CALL Warn( 'StreamSolver', 'Using normal stream function in axis symmetric case.' )
  ELSE IF ( Coordinates == Cartesian .AND. StokesStream ) THEN
     CALL Warn( 'StreamSolver', 'Using Stokes stream function in cartesian case.' )
  END IF
  
!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays

     ALLOCATE( ElementNodes % x( N ),  &
               ElementNodes % y( N ),  &
               ElementNodes % z( N ),  &
               LocalForce( DOFs*N ),   &
               LoadVector( 2*N ),   &
               LocalStiffMatrix(DOFs*N,DOFs*N ), STAT=istat ) 

     IF ( istat /= 0 ) CALL Fatal( 'PoissonSolve', 'Memory allocation error.' )
     AllocationsDone = .TRUE.
  END IF

!------------------------------------------------------------------------------
! Initialize the system and do the assembly
!------------------------------------------------------------------------------
  CALL InitializeToZero( StiffMatrix, ForceVector )
  
  DO t=1,Solver % NumberOfActiveElements
     CurrentElement => Solver % Mesh % Elements( Solver % ActiveElements(t) )
     Model % CurrentElement => CurrentElement ! may be used be user functions
     n = CurrentElement % Type % NumberOfNodes
     NodeIndexes => CurrentElement % NodeIndexes
     
     ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
     ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
     ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
!------------------------------------------------------------------------------
     LoadVector = 0.0d0
     DO i = 1,n
        k = FlowSolPerm( NodeIndexes(i) )
        k = (k-1) * NSDOFs

        LoadVector( (i-1)*2 +1 ) = -FlowSolValues( k+2 )
        LoadVector( (i-1)*2 +2 ) = FlowSolValues( k+1 )
     END DO
     
!------------------------------------------------------------------------------
!     Get element local matrix and rhs vector
!------------------------------------------------------------------------------
     CALL LocalMatrix(  LocalStiffMatrix, LocalForce, LoadVector, &
          CurrentElement, n, ElementNodes, StokesStream )

!------------------------------------------------------------------------------
!     Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
      CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
           ForceVector, LocalForce, n, DOFs, Perm(NodeIndexes) )

   END DO! <- elements
!------------------------------------------------------------------------------
   CALL FinishAssembly( Solver, ForceVector )

!------------------------------------------------------------------------------
!  Zero the row corresponding to the 'FirstNode':
!------------------------------------------------------------------------------
   k = Perm( FirstNode )
   DO i=StiffMatrix % Rows(k),StiffMatrix % Rows(k+1)-1
      StiffMatrix % Values(i) = 0.0D0
   END DO

   StiffMatrix % Values( StiffMatrix % Diag(k) ) = MAXVAL( StiffMatrix % Values )
   ForceVector(k) = MAXVAL( ForceVector )

!------------------------------------------------------------------------------
!  Solve the system:
!------------------------------------------------------------------------------
   CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
             StreamFunction, Norm, DOFs, Solver )

!------------------------------------------------------------------------------
! Do Shifting and Scaling if needed
!------------------------------------------------------------------------------

  IF ( Shifting ) THEN
     StreamFunction = StreamFunction - MINVAL( StreamFunction )
  END IF

  IF ( Scaling ) THEN
     IF ( MAXVAL( ABS( StreamFunction ) ) < AEPS ) THEN
        CALL Warn( 'StreamSolver', &
             'Maximum absolut value smaller than machine epsilon; cannot scale.' )
     ELSE
        StreamFunction = StreamFunction / MAXVAL( ABS( StreamFunction ) )
     END IF
  END IF
!------------------------------------------------------------------------------

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  StiffMatrix, ForceVector, LoadVector, Element, n, Nodes, StokesStream )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), ForceVector(:)
    REAL(KIND=dp) :: LoadVector(:)
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
    LOGICAL :: StokesStream
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), Load(2)
    REAL(KIND=dp) :: DetJ, U, V, W, S, Radius
    LOGICAL :: Stat
    INTEGER :: t, p, q, dim, NBasis, k
    TYPE(GaussIntegrationPoints_t) :: IntegStuff
!------------------------------------------------------------------------------
    dim = CoordinateSystemDimension()
    StiffMatrix = 0.0d0
    ForceVector = 0.0d0
    Radius = 0.0d0

!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------
    NBasis = n
    IntegStuff = GaussPoints( Element )

    DO t=1,IntegStuff % n
       U = IntegStuff % u(t)
       V = IntegStuff % v(t)
       W = IntegStuff % w(t)
       S = IntegStuff % s(t)
       
!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, U, V, W, DetJ, &
            Basis, dBasisdx, ddBasisddx, .FALSE. )
       s = s * DetJ
       
!------------------------------------------------------------------------------
!      Load at the integration point
!------------------------------------------------------------------------------
       Load(1) = SUM( Basis(1:n) * LoadVector(1:2*n:2) )
       Load(2) = SUM( Basis(1:n) * LoadVector(2:2*n:2) )

!------------------------------------------------------------------------------
!      In StokesStream we also need the radius.
!------------------------------------------------------------------------------
       k = Element % Type % ElementCode / 100
       Radius = SUM( Nodes % x(1:k) * Basis(1:k) )

!------------------------------------------------------------------------------
!      Finally, the elemental matrix & vector
!------------------------------------------------------------------------------       
       DO p=1,NBasis
          DO q=1,NBasis             
             StiffMatrix(p,q) = StiffMatrix(p,q) &
                  + s * SUM( dBasisdx( q,1:dim ) * dBasisdx( p,1:dim ) )
          END DO
       END DO

       IF ( StokesStream ) THEN
          DO p = 1, NBasis
             ForceVector(p) = ForceVector(p) + s * SUM( Load * dBasisdx(p,1:dim) ) * Radius
          END DO
       ELSE
          DO p = 1, NBasis
             ForceVector(p) = ForceVector(p) + s * SUM( Load * dBasisdx(p,1:dim) )
          END DO
       END IF
       
    END DO! <- t eli integraatiopisteet

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE StreamSolver
!------------------------------------------------------------------------------
