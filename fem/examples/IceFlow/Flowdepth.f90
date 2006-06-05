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
  TYPE(Variable_t), POINTER :: PointerToVariable
  TYPE(Solver_t), POINTER :: PointerToSolver

  LOGICAL :: AllocationsDone = .FALSE., Found, CalcFree = .FALSE.

  INTEGER :: i, n, m, t, istat
  INTEGER, POINTER :: Permutation(:), NumberOfVisits(:)

  REAL(KIND=dp), POINTER :: VariableValues(:), Surface(:), GradSurface1(:),GradSurface2(:)
  REAL(KIND=dp) :: Norm, Gradient,GradSurface(3)

  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), LOAD(:), FORCE(:)
       

  SAVE STIFF, LOAD, FORCE, Surface, GradSurface, AllocationsDone
!------------------------------------------------------------------------------
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values

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

  CalcFree = GetLogical(SolverParams, 'Calc Free Surface', Found)
  IF (.NOT. Found) THEN
     CalcFree = .FALSE.
  ELSE
     CALL INFO('FlowdepthSolve', 'Free surface variable will be calculated', Level=1)
  END IF

  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed  ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays
     M = Model % Mesh % NumberOfNodes
     IF (AllocationsDone) DEALLOCATE(FORCE, LOAD, STIFF,&
          Surface, GradSurface1, GradSurface2, NumberOfVisits)

     ALLOCATE( FORCE(N), LOAD(N), STIFF(N,N),&
          Surface(M), GradSurface1(M), GradSurface2(M), NumberOfVisits(M), STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL Fatal( 'FlowdepthSolve', 'Memory allocation error.' )
     END IF
     
     IF (CalcFree) THEN
     ! Assign Variable for Residual (i.e., heat flux at boundaries)
     !-------------------------------------------------------------
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, PointerToSolver, &
             'FreeSurf', 1, Surface, Permutation )
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, PointerToSolver, &
             'FreeSurfGrad1', 1, GradSurface1, Permutation )
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, PointerToSolver, &
             'FreeSurfGrad2', 1, GradSurface2, Permutation )
     END IF

     AllocationsDone = .TRUE.
  END IF

  Surface = 1.0D00
  GradSurface1 = 2.0D00
  GradSurface2 = 3.0D00


  !Initialize the system and do the assembly:
  !------------------------------------------
  CALL DefaultInitialize()

  ! bulk assembly
  DO t=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(t)
     n = GetElementNOFNodes()
     CALL LocalMatrix(  STIFF, FORCE, Element, n)
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  
  ! vN. conditions
  DO t=1,Solver % Mesh % NUmberOfBoundaryElements
     Element => GetBoundaryElement(t)
     n = GetElementNOFNodes()
     IF ( GetElementFamily() /= 1 ) THEN
        CALL LocalMatrixBC(  STIFF, FORCE, Element, n, Gradient )
        CALL DefaultUpdateEquations( STIFF, FORCE )
     END IF
  END DO
  CALL DefaultFinishAssembly()

  ! Dirichlet 
  CALL DefaultDirichletBCs()
  !Solve the system
  Norm = DefaultSolve()

  IF (Calcfree) THEN   
     Surface = 0.0D00
     GradSurface1 = 0.0D00
     GradSurface2 = 0.0D00
     NumberOfVisits = 0
     DO t=1,Solver % NumberOfActiveElements
        Element => GetActiveElement(t)
        n = GetElementNOFNodes()
        CALL GetSurfaceValue(Model, Surface, GradSurface1, GradSurface2,&
             VariableValues, Permutation, NumberOfVisits, Element, n)
     END DO
     DO i=1,Model % Mesh % NumberOfNodes
        GradSurface1(Permutation(i)) = GradSurface1(Permutation(i))/NumberOfVisits(i)
        GradSurface2(Permutation(i)) = GradSurface2(Permutation(i))/NumberOfVisits(i)
     END DO
  END IF
CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE GetSurfaceValue(Model, Surface, GradSurface1, GradSurface2,&
       VariableValues, Permutation, NumberOfVisits, Element, n)
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: LocalSurf, GradSurface(3), Depth
    INTEGER :: n
    INTEGER, POINTER :: Permutation(:), NumberOfVisits(:)
    REAL(KIND=dp), POINTER :: Surface(:), GradSurface1(:), GradSurface2(:), VariableValues(:)
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3),DetJ, z, U, V, W, SqrtElementMetric
    LOGICAL :: Stat
    INTEGER :: i,j,k,dim
    LOGICAL :: FirstTime
    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )

    dim = CoordinateSystemDimension()
    ! loop over all nodes in Element
    DO i=1,N

       j = Element % NodeIndexes(i) ! get number of node in element in physical space
       NumberOfVisits(j) = NumberOfVisits(j) + 1

       ! get local coordinates of the point i inside the element
       U = Element % Type % NodeU(i)
       V = Element % Type % NodeV(i)
       W = Element % Type % NodeW(i)

       ! get local information on test-functions and derivatives of the point i
       stat = ElementInfo( Element,Nodes,U,V,W,SqrtElementMetric, &
            Basis,dBasisdx,ddBasisddx,.FALSE.,.FALSE. )  
       IF (DIM == 2) THEN 
          z = Model % Nodes % y(j)
       ELSE IF (DIM == 3) THEN
          z = Model % Nodes % z(j)
       ELSE
          CALL FATAL('FlowdepthSolve', 'Flow depth for one-dimensional problem not defined!')
       END IF

       IF (NumberOfVisits(j) == 1) &
            Surface(Permutation(j)) = z + VariableValues(Permutation(j))  
!       DO k=1,n
!          GradSurface1(Permutation(j)) = GradSurface1(Permutation(j)) + dBasisdx(k,1)*VariableValues(Permutation(Element % NodeIndexes(k)))
!       END DO
       GradSurface1(Permutation(j)) = GradSurface1(Permutation(j)) +&
            SUM(dBasisdx(1:N,1)*VariableValues(Permutation(Element % NodeIndexes(1:N))))
       IF (DIM > 2) &
            GradSurface2(Permutation(j)) = GradSurface2(Permutation(j)) +&
            SUM(dBasisdx(1:N,2)*VariableValues(Permutation(Element % NodeIndexes(1:N))))
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE GetSurfaceValue
!------------------------------------------------------------------------------


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


