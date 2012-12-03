!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
! ******************************************************************************
! *
! *  Authors: Olivier Gagliardini, Ga¨el Durand
! *  Email:   
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: 
! * 
! *****************************************************************************
!> Solver for creating a mask on whether the lower side of an ice sheet/shelf is
!>  grounded or not. +1=grounded,-1=detached, 0=grounding line (=last grounded node)
SUBROUTINE GroundedSolver( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  For the bottom surface, creates and updates a mask which may be equal to -1, 0 or 1

!  GroundedMask = + 1 if grounded
!               = - 1 if floating
!               = 0   if on the grounding line (also grounded)
!
!  Consequently, a node is grounded if GroundedMask >= 0
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
  TYPE(ValueList_t), POINTER :: Material, SolverParams
  TYPE(Variable_t), POINTER :: PointerToVariable, Timevar, TimeStepVar
  TYPE(Nodes_t), SAVE :: Nodes

  CHARACTER(LEN=MAX_NAME_LEN) :: SaveFileName, SaveFileNamePar

  LOGICAL :: AllocationsDone = .FALSE., GotIt, stat, Save_GL = .FALSE., SubroutineVisited = .FALSE., Parallel, Quadratic

  INTEGER :: i, mn, n, t, Nn, istat, DIM, MSum, ZSum
  INTEGER :: timeStep
  INTEGER, POINTER :: Permutation(:)

  REAL(KIND=dp), POINTER :: VariableValues(:)
  REAL(KIND=dp) :: z, toler, time
  REAL(KIND=dp), ALLOCATABLE :: zb(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName = 'GroundedSolver'
       
  SAVE AllocationsDone, DIM, SolverName, zb
  SAVE toler
  SAVE Save_GL, SaveFileName, SaveFileNamePar, SubroutineVisited, Parallel, Quadratic
  !------------------------------------------------------------------------------

  ! Where will be stored the variable GroundedMask
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values

  CALL INFO(SolverName, 'Computing grounded mask from geometry', level=3)

  !--------------------------------------------------------------
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------

  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed ) THEN
    DIM = CoordinateSystemDimension()
    mn = Solver % Mesh % MaxElementNodes

    IF (AllocationsDone) DEALLOCATE(zb)

    ALLOCATE(zb(mn), STAT=istat )
          
    IF ( istat /= 0 ) THEN
      CALL FATAL( SolverName, 'Memory allocation error.' )
    END IF

    AllocationsDone = .TRUE.
    CALL INFO( SolverName, 'Memory allocation done.',Level=1 )

    !---------------------------
    ! Environnement parallele ?
    !---------------------------
    Parallel = .FALSE.
    IF ( ParEnv % PEs > 1 ) Parallel = .TRUE.

    ! quadratic elements or not ??? not used for now
    Quadratic = .FALSE.
    Element => GetActiveElement(1)
    n = GetElementNOFNodes()
    IF ( Element % Type % ElementCode/n == 102 ) Quadratic = .TRUE.

    !---------------------
    ! Save part
    !---------------------
    SolverParams => GetSolverParams()
    Save_GL = GetLogical(SolverParams, 'SaveGL', GotIt)
    IF (Save_GL) THEN
      SaveFileName = GetString(SolverParams, 'Save File Name', GotIt)

    ! Write the header of the file
      OPEN(UNIT=22, File=TRIM(SaveFileName)//".names")
      WRITE(UNIT=22, FMT='("# 1  : Time Step")')
      WRITE(UNIT=22, FMT='("# 2  : Time")')
      WRITE(UNIT=22, FMT='("# 3  : Xg")')
      IF (DIM==3) WRITE(UNIT=22, FMT='("# 4  : Yg")')
      CLOSE(UNIT=22)
    END IF

  END IF
  ! End first time

  ! for saving purpose
  Timevar => VariableGet( Model % Variables, 'Time')
  time = TimeVar % Values(1)

  TimeStepvar => VariableGet( Model % Variables, 'TimeStep')
  timeStep = TimeStepVar % Values(1)

  !--------------------------------------------
  ! First loop, where y > zb+toler GroundedMask = -1, else = 1
  !--------------------------------------------
  DO t = 1, Solver % NumberOfActiveElements
    Element => GetActiveElement(t)
    IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
    n = GetElementNOFNodes()

    SolverParams => GetSolverParams()
    toler = GetConstReal(SolverParams, 'Toler', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No tolerance given for the Grounded Mask.')
    END IF

    Material => GetMaterial( Element )
    zb(1:n) = ListGetReal( Material,'Min ZsBottom',n , & 
                   Element % NodeIndexes, GotIt ) + toler

    CALL GetElementNodes( Nodes )

    DO i = 1, n
      Nn = Permutation(Element % NodeIndexes(i))
      IF (Nn==0) CYCLE

      IF (DIM == 2) THEN
        z = Nodes % y( i )
      ELSE IF (DIM == 3) THEN
        z = Nodes % z( i )
      END IF

      ! Geometrical condition:
      ! if the node is above the bedrock (plus the tolerance)
      ! then its mask is -1 (floating)
      ! otherwise it is 1 (grounded)

      IF (z > zb(i)) THEN
	VariableValues(Nn) = -1.0_dp
      ELSE
	VariableValues(Nn) = 1.0_dp
      END IF

    END DO
  END DO

  ! attribute zeros to the mask, corresponding to the Grounding Line
  ! loop over each element:
  ! if the sum of the element masks is lower than the element number of nodes minus the number of zeros, then each mask equal to 1 is modified to 0

  ! opening the grounding line output files
  IF (Save_GL) THEN

    IF (Parallel) THEN
      DO i=1, MAX_NAME_LEN
        IF ( SaveFileName(i:i) == ' ' ) EXIT
        SaveFileNamePar(i:i) = SaveFileName(i:i)
      END DO
      SaveFileNamePar(i:i) = '.'

      IF ( ParEnv % MyPE < 10 ) THEN
        WRITE( SaveFileNamePar(i+1:), '(i1)' ) ParEnv % MyPE
      ELSE IF ( ParEnv % MyPE < 100 ) THEN
        WRITE( SaveFileNamePar(i+1:), '(i2)' ) ParEnv % MyPE
      ELSE
        WRITE( SaveFileNamePar(i+1:), '(i3)' ) ParEnv % MyPE
      END IF

      IF(SubroutineVisited) THEN 
        OPEN (UNIT=22, FILE=TRIM(SaveFileNamePar),POSITION='APPEND')
      ELSE 
        OPEN (UNIT=22,FILE=TRIM(SaveFileNamePar))
      END IF

    ELSE

      IF (SubroutineVisited) THEN
        OPEN(UNIT=22, File=TRIM(SaveFileName), POSITION='APPEND')
      ELSE
        OPEN(UNIT=22, File=TRIM(SaveFileName))
      END IF

    END IF

  END IF

  !--------------------------------------------
  ! Second loop, to add groundedmask = 0 (grounding line)
  !--------------------------------------------
  DO t = 1, Solver % NumberOfActiveElements

    Element => GetActiveElement(t)
    IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
    n = GetElementNOFNodes()

    CALL GetElementNodes( Nodes )
    MSum = 0
    ZSum = 0

    DO i = 1, n
      Nn = Permutation(Element % NodeIndexes(i))
      IF (Nn==0) CYCLE
      MSum = MSum + VariableValues(Nn)
      IF (VariableValues(Nn) == 0.0_dp) ZSum = ZSum + 1.0_dp
    END DO

    IF (MSum + ZSum < n) THEN
      DO i = 1, n
	Nn = Permutation(Element % NodeIndexes(i))
	IF (Nn==0) CYCLE

	IF (VariableValues(Nn) == 1.0_dp) THEN
	  VariableValues(Nn) = 0.0_dp

	IF (DIM==2) PRINT *, 'Grounding Line, x', Nodes % x( i )
	IF (DIM==3) PRINT *, 'Grounding Line, (x,y)', Nodes % x( i ), Nodes % y( i )

	  ! Write Real Time, X Grounding Line, Y Grounding Line
	  IF (Save_GL) THEN
	    WRITE(UNIT=22, FMT='(I6,2x)', ADVANCE='NO') timeStep
	    WRITE(UNIT=22, FMT='(E14.8,2x)', ADVANCE='NO') time
	    IF (DIM==2) THEN
	      WRITE(UNIT=22, FMT='(E14.8,2x)') Nodes % x(i)
	    ELSE IF (DIM==3) THEN
	      WRITE(UNIT=22, FMT='(E14.8,2x)', ADVANCE='NO') Nodes % x(i)
	      WRITE(UNIT=22, FMT='(E14.8,2x)') Nodes % y(i)
	    END IF
	  END IF	

	END IF
      END DO
    END IF
  END DO

  IF (Save_GL) CLOSE(UNIT=22)
  SubroutineVisited = .TRUE.

  IF ( ParEnv % PEs>1 ) CALL ParallelSumVector( Solver % Matrix, VariableValues, 1 )

  CALL INFO( SolverName , 'Done')

!------------------------------------------------------------------------------
END SUBROUTINE GroundedSolver 
!------------------------------------------------------------------------------

!> for Grounded Mask initialisation purpose
!> same method than above
!> for more explanations, refer to above
SUBROUTINE GroundedSolverInit( Model,Solver,dt,TransientSimulation )
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
  TYPE(ValueList_t), POINTER :: Material, SolverParams
  TYPE(Variable_t), POINTER :: PointerToVariable
  TYPE(Nodes_t), SAVE :: Nodes

  LOGICAL :: stat, FirstTime = .TRUE., GotIt, Quadratic

  INTEGER :: i, mn, n, t, Nn, istat, DIM, MSum, ZSum
  INTEGER, POINTER :: Permutation(:)

  REAL(KIND=dp), POINTER :: VariableValues(:)
  REAL(KIND=dp) :: z, toler
  REAL(KIND=dp), ALLOCATABLE :: zb(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName = 'GroundedSolverInit'

  SAVE DIM, SolverName

  !------------------------------------------------------------------------------

  ! Where will be stored the variable GroundedMask
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values

  !--------------------------------------------------------------
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------

  IF (FirstTime) THEN

    IF (ParEnv % MyPe == 0) THEN
      PRINT *,'-----------------------------------'
      PRINT *,' Initializing Grounded Mask        '
      PRINT *,'-----------------------------------'
    END IF

    FirstTime = .FALSE.
    DIM = CoordinateSystemDimension()

    mn = Solver % Mesh % MaxElementNodes

    ALLOCATE(zb(mn), STAT=istat)   

    IF ( istat /= 0 ) THEN
      CALL FATAL( SolverName, 'Memory allocation error.' )
    END IF
    CALL INFO( SolverName, 'Memory allocation done.',Level=1 )


    SolverParams => GetSolverParams()

    toler = GetConstReal(SolverParams, 'TolerInit', GotIt)
    IF (.NOT.GotIt) THEN
      CALL FATAL(SolverName, 'No tolerance given for the Grounded Mask')
    END IF

    ! quadratic elements or not ??? not used for now
    Quadratic = .FALSE.
    Element => GetActiveElement(1)
    n = GetElementNOFNodes()
    IF ( Element % Type % ElementCode/n == 102 ) Quadratic = .TRUE.

    DO t = 1, Solver % NumberOfActiveElements
      Element => GetActiveElement(t)
      IF (ParEnv % myPe .NE. Element % partIndex) CYCLE

      n = GetElementNOFNodes()

      Material => GetMaterial( Element )
      zb(1:n) = ListGetReal( Material,'Min ZsBottom',n , & 
                   Element % NodeIndexes, GotIt ) + toler

      CALL GetElementNodes( Nodes )

      DO i = 1, n
	Nn = Permutation(Element % NodeIndexes(i))
	IF (Nn==0) CYCLE

	IF (DIM == 2) THEN
	  z = Nodes % y( i )
	ELSE IF (DIM == 3) THEN
	  z = Nodes % z( i )
	END IF

	! Geometrical condition:
	! if the node is above the bedrock (plus the tolerance)
	! then its mask is -1 (floating)
	! otherwise it is 1 (grounded)

	IF (z > zb(i)) THEN
	  VariableValues(Nn) = -1.0_dp
	ELSE
	  VariableValues(Nn) = 1.0_dp
	END IF

      END DO

    END DO

    ! attribute zeros to the mask, corresponding to the Grounding Line
    ! loop over each element:
    ! if the sum of the element masks is lower than the element number of nodes minus the number of zeros, then each mask equal to 1 is modified to 0

    DO t = 1, Solver % NumberOfActiveElements

      Element => GetActiveElement(t)
      IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
      n = GetElementNOFNodes()

      CALL GetElementNodes( Nodes )
      MSum = 0
      ZSum = 0

      DO i = 1, n
	Nn = Permutation(Element % NodeIndexes(i))
	IF (Nn==0) CYCLE

	MSum = MSum + VariableValues(Nn)
	IF (VariableValues(Nn) == 0.0_dp) ZSum = ZSum + 1.0_dp

      END DO

      IF (MSum + ZSum < n) THEN
	DO i = 1, n
	  Nn = Permutation(Element % NodeIndexes(i))
	  IF (Nn==0) CYCLE

	  IF (VariableValues(Nn) == 1.0_dp) THEN
	    VariableValues(Nn) = 0.0_dp

	    IF (DIM==2) PRINT *, 'Initial Grounding Line, x', Nodes % x( i )
	    IF (DIM==3) PRINT *, 'Initial Grounding Line, (x,y)', Nodes % x( i ), Nodes % y( i )

	  END IF
	END DO
      END IF

    END DO

    IF ( ParEnv % PEs>1 ) CALL ParallelSumVector( Solver % Matrix, VariableValues, 1 )

    DEALLOCATE(zb)

    CALL INFO( SolverName , 'Done')
    
  END IF

!------------------------------------------------------------------------------
END SUBROUTINE GroundedSolverInit 
!------------------------------------------------------------------------------


