SUBROUTINE DifferenceSolver( Model, Solver, dt, TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT:: PoissonSolver
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
  TYPE(ValueList_t), POINTER :: SolverParams
  CHARACTER(LEN=MAX_NAME_LEN) :: F1name, F2name
  TYPE(Variable_t), POINTER :: F1, F2
  LOGICAL :: Found, AllocationsDone
  INTEGER :: N, t
  TYPE(Element_t), POINTER :: Element
  REAL(KIND=dp), ALLOCATABLE :: F1local(:), F2local(:)
  REAL(KIND=dp) :: nrm, nrmloc, nrmf1, nrmf1loc, nrmf2, nrmf2loc
  SAVE F1local, F2local, AllocationsDone
!------------------------------------------------------------------------------
  IF(.NOT. AllocationsDone .OR. Solver % Mesh % Changed) THEN
     N = Solver % Mesh % MaxElementDOFs

     IF(AllocationsDone) THEN
        DEALLOCATE(F1local, F2local)
     END IF

     ALLOCATE(F1local(N), F2local(N))

     AllocationsDone = .TRUE.
  END IF

  SolverParams => GetSolverParams()
  
  F1name = GetString(SolverParams, "F1", Found)
  
  IF(.NOT.Found) THEN
     PRINT *, "F1 was not found (name unspecified)"
     RETURN
  END IF

  F1 => VariableGet(Solver % Mesh % Variables, F1name)

  IF(.NOT.ASSOCIATED(F1)) THEN
     PRINT *, "F1 not found: ", TRIM(F1name)
     RETURN
  ELSE
     PRINT *, "Found F1: ", TRIM(F1name)
  END IF

  F2name = GetString(SolverParams, "F2", Found)

  IF(.NOT.Found) THEN
     PRINT *, "F2 was not found (name unspecified)"
     RETURN
  END IF

  F2 => VariableGet(Solver % Mesh % Variables, F2name)

  IF(.NOT.ASSOCIATED(F2)) THEN
     PRINT *, "F2 not found: ", TRIM(F2name)
     RETURN
  ELSE
     PRINT *, "Found F2: ", TRIM(F2name)
  END IF

  nrm = 0.0d0
  nrmf1 = 0.0d0
  nrmf2 = 0.0d0

  DO t = 1, GetNofActive()
     Element => GetActiveElement(t)
     n = GetElementNOFNodes()

     CALL GetScalarLocalSolution(F1local, F1name)
     CALL GetScalarLocalSolution(F2local, F2name)
     
     CALL Compute(Element, n, F1local, F2local, nrmloc, nrmf1loc, nrmf2loc)

     nrm = nrm + nrmloc
     nrmf1 = nrmf1 + nrmf1loc
     nrmf2 = nrmf2 + nrmf2loc
  END DO

  nrm = SQRT(nrm)
  nrmf1 = SQRT(nrmf1)
  nrmf2 = SQRT(nrmf2)

  PRINT *, "|| F1 - F2 ||_0 = ", nrm
  PRINT *, "|| F1 ||_0 = ", nrmf1
  PRINT *, "|| F2 ||_0 = ", nrmf2

CONTAINS

  SUBROUTINE Compute(Element, n, F1local, F2local, nrm, nrmf1, nrmf2)
    TYPE(Element_t), POINTER :: Element
    INTEGER :: n
    REAL(KIND=dp) :: F1local(:), F2local(:), nrm, nrmf1, nrmf2

    TYPE(GaussIntegrationPoints_t) :: IP
    REAL(KIND=dp) :: detJ, Basis(n), dBasisdx(n, 3)
    REAL(KIND=dp) :: F1, F2
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

       F1 = SUM( F1local(1:n) * Basis(1:n) )
       F2 = SUM( F2local(1:n) * Basis(1:n) )

       nrm = nrm + (F1 - F2)**2 * IP % s(t) * detJ
       nrmf1 = nrmf1 + F1**2 * IP % s(t) * detJ
       nrmf2 = nrmf2 + F2**2 * IP % s(t) * detJ

    END DO

  END SUBROUTINE Compute

END SUBROUTINE DifferenceSolver
