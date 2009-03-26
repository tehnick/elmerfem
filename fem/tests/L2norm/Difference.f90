SUBROUTINE DifferenceSolver(Model, Solver, dt, TransientSimulation)
!DEC$ATTRIBUTES DLLEXPORT:: DifferenceSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Computes the difference between two scalar fields.
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
  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(ValueList_t), POINTER :: SolverParams
  CHARACTER(LEN=MAX_NAME_LEN) :: SenderName, F1name, F2name, MeshName
  TYPE(Variable_t), POINTER :: F1, F2
  LOGICAL :: Found, AllocationsDone
  INTEGER :: N, t
  TYPE(Element_t), POINTER :: Element
  REAL(KIND=dp), ALLOCATABLE :: F1loc(:), F2loc(:)
  REAL(KIND=dp) :: nrm, nrmloc, nrmf1, nrmf1loc, nrmf2, nrmf2loc
  SAVE F1loc, F2loc, AllocationsDone
!------------------------------------------------------------------------------
  SenderName = 'DifferenceSolve'

  Mesh => GetMesh()

  MeshName = Mesh % Name
  
  WRITE(Message, *) 'Functions are interpolated using mesh: ', TRIM(MeshName)

  CALL Info(SenderName, Message)

  IF(.NOT. AllocationsDone .OR. Solver % Mesh % Changed) THEN
     N = Solver % Mesh % MaxElementDOFs

     IF(AllocationsDone) THEN
        DEALLOCATE(F1loc, F2loc)
     END IF

     ALLOCATE(F1loc(N), F2loc(N))

     AllocationsDone = .TRUE.
  END IF

  SolverParams => GetSolverParams()
  
  F1name = GetString(SolverParams, "F1", Found)
  
  IF(.NOT.Found) THEN
     WRITE(Message, *) 'F1 not found (name unspecified)'
     CALL Fatal(SenderName, Message)
  END IF

  F1 => VariableGet(Solver % Mesh % Variables, F1name)

  IF(.NOT.ASSOCIATED(F1)) THEN
     WRITE(Message, *) 'F1 not found: ', TRIM(F1name)
     CALL Fatal(SenderName, Message)
  ELSE
     WRITE(Message, *) 'F1: ', TRIM(F1name)
     CALL Info(SenderName, Message)     
  END IF

  F2name = GetString(SolverParams, "F2", Found)

  IF(.NOT.Found) THEN
     WRITE(Message, *) 'F2 not found (name unspecified)'
     CALL Fatal(SenderName, Message)
  END IF

  F2 => VariableGet(Solver % Mesh % Variables, F2name)

  IF(.NOT.ASSOCIATED(F2)) THEN
     WRITE(Message, *) 'F2 not found: ', TRIM(F2name)
     CALL Fatal(SenderName, Message)
  ELSE
     WRITE(Message, *) 'F2: ', TRIM(F2name)
     CALL Info(SenderName, Message)     
  END IF

  nrm = 0.0d0
  nrmf1 = 0.0d0
  nrmf2 = 0.0d0

  DO t = 1, GetNofActive()
     Element => GetActiveElement(t)
     n = GetElementNOFNodes()

     CALL GetScalarLocalSolution(F1loc, F1name)
     CALL GetScalarLocalSolution(F2loc, F2name)
     
     CALL Compute(Element, n, F1loc, F2loc, nrmloc, nrmf1loc, nrmf2loc)

     nrm = nrm + nrmloc
     nrmf1 = nrmf1 + nrmf1loc
     nrmf2 = nrmf2 + nrmf2loc
  END DO

  nrm = SQRT(nrm)
  nrmf1 = SQRT(nrmf1)
  nrmf2 = SQRT(nrmf2)

  WRITE(Message, *) '|| F1 - F2 ||_0 =', nrm
  CALL Info(SenderName, Message)
  WRITE(Message, *) '|| F1 ||_0 =', nrmf1
  CALL Info(SenderName, Message)
  WRITE(Message, *) '|| F2 ||_0 =', nrmf2
  CALL Info(SenderName, Message)

CONTAINS

  SUBROUTINE Compute(Element, n, F1, F2, nrm, nrmf1, nrmf2)
    TYPE(Element_t), POINTER :: Element
    INTEGER :: n
    REAL(KIND=dp) :: F1(:), F2(:), nrm, nrmf1, nrmf2

    TYPE(GaussIntegrationPoints_t) :: IP
    REAL(KIND=dp) :: detJ, Basis(n), dBasisdx(n, 3)
    REAL(KIND=dp) :: F1atIP, F2atIP
    INTEGER :: t
    LOGICAL :: stat
    TYPE(Nodes_t) :: Nodes
    SAVE Nodes

    CALL GetElementNodes(Nodes)

    IP = GaussPoints(Element)

    nrm = 0.0d0
    nrmf1 = 0.0d0
    nrmf2 = 0.0d0

    DO t = 1, IP % n
       stat = ElementInfo(Element, Nodes, &
            IP % U(t), IP % V(t), IP % W(t), &
            detJ, Basis, dBasisdx)

       F1atIP = SUM( F1(1:n) * Basis(1:n) )
       F2atIP = SUM( F2(1:n) * Basis(1:n) )

       nrm = nrm + (F1atIP - F2atIP)**2 * IP % s(t) * detJ
       nrmf1 = nrmf1 + F1atIP**2 * IP % s(t) * detJ
       nrmf2 = nrmf2 + F2atIP**2 * IP % s(t) * detJ

    END DO

  END SUBROUTINE Compute

END SUBROUTINE DifferenceSolver
