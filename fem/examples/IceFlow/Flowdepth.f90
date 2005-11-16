SUBROUTINE FlowdepthSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: FlowdepthSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the Flowdepth equation!
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
  USE DefUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Element_t),POINTER :: Element
  TYPE(ValueList_t), POINTER :: SolverParams

  LOGICAL :: AllocationsDone = .FALSE., Found

  INTEGER :: n, t, istat
  REAL(KIND=dp) :: Norm, Gradient

  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), LOAD(:), FORCE(:)

  SAVE STIFF, LOAD, FORCE, AllocationsDone
!------------------------------------------------------------------------------

  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( .NOT. AllocationsDone ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays
     ALLOCATE( FORCE(N), LOAD(N), STIFF(N,N), STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL Fatal( 'FlowdepthSolve', 'Memory allocation error.' )
     END IF
     AllocationsDone = .TRUE.
  END IF

  ! get Gradient (change of property
  ! with respect to unit-length of
  ! vertical direction
  !----------------------------------
  SolverParams => GetSolverParams()
  Gradient = GetConstReal( SolverParams, &
                      'Gradient',  Found )
  IF (.NOT. Found) THEN
     CALL WARN('FlowdepthSolve', 'No keyword >Gradient< found in section Solver')
     CALL WARN('FlowdepthSolve', 'Assuming value of -1')
     Gradient = -1.0D00
  ELSE
     WRITE(Message,'(A e12.4,A)') 'Gradient of ',Gradient,' applied'
     CALL INFO('FlowdepthSolve', Message,Level=1)
  END IF

  !Initialize the system and do the assembly:
  !------------------------------------------
  CALL DefaultInitialize()

  DO t=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(t)
     n = GetElementNOFNodes()
     CALL LocalMatrix(  STIFF, FORCE, Element, n)
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO

  DO t=1,Solver % Mesh % NUmberOfBoundaryElements
     Element => GetBoundaryElement(t)
     n = GetElementNOFNodes()
     IF ( GetElementFamily() /= 1 ) THEN
        CALL LocalMatrixBC(  STIFF, FORCE, Element, n, Gradient )
        CALL DefaultUpdateEquations( STIFF, FORCE )
     END IF
  END DO

  CALL DefaultFinishAssembly()
  CALL DefaultDirichletBCs()
  Norm = DefaultSolve()

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  STIFF, FORCE, Element, n)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3),DetJ
    LOGICAL :: Stat
    INTEGER :: t, p,q ,dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       DO p=1,n
         DO q=1,n
           STIFF(p,q) = STIFF(p,q) + IP % S(t) * detJ * dBasisdx(q,dim)*dBasisdx(p,dim)
         END DO
       END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBC(  STIFF, FORCE, Element, n, Gradient)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), Gradient
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3), &
                      DetJ,Normal(3)
    LOGICAL :: Stat
    INTEGER :: t, dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
      stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
       IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

      Normal = NormalVector( Element, Nodes, IP % U(t), IP % V(t), .TRUE.)
      FORCE(1:n) = FORCE(1:n) + Gradient * IP % s(t) * DetJ * Normal(dim) * Basis(1:n)
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBC
!------------------------------------------------------------------------------
END SUBROUTINE FlowdepthSolver
!------------------------------------------------------------------------------
