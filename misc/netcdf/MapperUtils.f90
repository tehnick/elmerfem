!------------------------------------------------------------------------------
! Vili Forsell
! Created: 7.7.2011
! Last Modified: 7.7.2011
!------------------------------------------------------------------------------
! This module contains general functions for GridDataMapper
! - GetElmerNodeValue() ; Simplified access to Elmer Node's coordinates
! - GetElmerMinMax() ; Returns min, or max, value for given Elmer dimension
! - IntWidth() ; Returns the numeric width for an integer value
!     o Mainly used for determining widths for format fields to avoid prefix spaces
! - ListGetStrings() ; Collects an array of strings from SIF defined with names suffixed by running numbers
!     o Used to generalize string input
!------------------------------------------------------------------------------
MODULE MapperUtils

CONTAINS

  !---------------- GetElmerNodeValue() ---------------
  !--- Gets the value of the chosen node from the given dimension (1 = x, 2 = y, 3 = z)
  FUNCTION GetElmerNodeValue( Solver, node, dimE ) RESULT( node_val )
  !----------------------------------------------------
    USE DefUtils, ONLY: Solver_t, dp
    USE Messages, ONLY: Fatal
    IMPLICIT NONE

    TYPE(Solver_t), INTENT(IN) :: Solver
    INTEGER, INTENT(IN) :: node, dimE
    REAL(KIND=dp) :: node_val ! The output
    SELECT CASE (dimE)
      CASE (1)
        node_val = Solver % Mesh % Nodes % x(node)
      CASE (2)
        node_val = Solver % Mesh % Nodes % y(node)
      CASE (3)
        node_val = Solver % Mesh % Nodes % z(node)
      CASE DEFAULT
        CALL Fatal('GridDataMapper','GetElmerNodeValue(): Elmer dimension not found')
        node_val = 0
    END SELECT
     
  END FUNCTION GetElmerNodeValue

  !---------------- GetElmerMinMax() ---------------
  !--- Gets the minimum/maximum (chosen) value of the given dimension (1 = x, 2 = y, 3 = z)
  FUNCTION GetElmerMinMax( Solver, dimE, GET_MIN ) RESULT( node_val )
  !----------------------------------------------------
    USE DefUtils, ONLY: Solver_t, CoordinateSystemDimension, dp
    USE Messages, ONLY: Fatal
    IMPLICIT NONE

    TYPE(Solver_t), INTENT(IN) :: Solver
    INTEGER, INTENT(IN) :: dimE
    LOGICAL, INTENT(IN) :: GET_MIN
    REAL(KIND=dp) :: node_val ! The output

    SELECT CASE (dimE)
      CASE (1)
        IF ( GET_MIN ) THEN 
          node_val = MINVAL(Solver % Mesh % Nodes % x)
        ELSE 
          node_val = MAXVAL(Solver % Mesh % Nodes % x)
        END IF
      CASE (2)
        IF ( GET_MIN ) THEN
          node_val = MINVAL(Solver % Mesh % Nodes % y)
        ELSE 
          node_val = MAXVAL(Solver % Mesh % Nodes % y)
        END IF
      CASE (3)
        IF ( GET_MIN ) THEN
          node_val = MINVAL(Solver % Mesh % Nodes % z)
        ELSE 
          node_val = MAXVAL(Solver % Mesh % Nodes % z)
        END IF
      CASE DEFAULT
        CALL Fatal('GridDataMapper','GetAllElmerNodeValues(): Elmer dimension not found')
        node_val = 0
    END SELECT
     
  END FUNCTION GetElmerMinMax


  !----------------------- IntWidth() --------------
  !--- Finds the width of an integer; ignores sign
  FUNCTION IntWidth( NR ) RESULT( width )
  !-------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NR
    INTEGER :: val
    INTEGER :: width
    INTEGER :: comp

    width = 1
    comp = 10
    val = ABS(NR) + 1 ! Ignores sign; val >= 1

    ! Note that:
    ! 10^0 - 1 = 0 <= 0..9 <= 10 = 10^1 - 1,
    ! 10^1 - 1 = 0 <= 10..99 <= 10 = 10^2 - 1,
    ! 10^2 - 1 = 0 <= 100..999 <= 10 = 10^3 - 1, 
    ! and so forth,
    ! where 10 corresponds to width of the number with 10 based numbers (n == width).
    ! We get: 10^(n-1) <= NR + 1 <= 10^(n)

    ! Width usually small, so logarithmic search is not usually necessary.

    ! P: 10^0 = 1 <= val+1 .AND. width = 1 => For every j; 0 <= j <= i: 10^i <= val+1 .AND. i = 0 .AND. width = i + 1 = 0 + 1 = 1
    DO WHILE (comp < val)
      width = width + 1 ! width = i+1
      comp = 10*comp ! 10*10^(i) = 10^(i+1)
      ! I: For every j; 0 <= j <= i: 10^j <= val+1 .AND. Exists n; i < n: val+1 <= 10^n .AND. width = i+1
    END DO
    ! Q: (For every i; 0 <= i <= n-1: 10^i <= val+1) .AND. val+1 <= 10^n .AND. width = n
    !    => 10^(n-1) <= val+1 .AND. val+1 <= 10^n .AND. width = n

  END FUNCTION IntWidth


  !----------------- ListGetStrings() -----------------
  !--- Gets all strings defined with prefix "Name" and ending with " NR", where NR is a number in an array
  SUBROUTINE ListGetStrings( List,Name,Found,CValues )
  !----------------------------------------------------
    USE DefUtils
    IMPLICIT NONE

    !--- Arguments
    TYPE(ValueList_t), INTENT(IN), POINTER :: List ! A pointer to a list of values
    CHARACTER(LEN=*), INTENT(IN) :: Name ! Name of the SIF variable
    LOGICAL, OPTIONAL, INTENT(INOUT) :: Found ! True, if found
    CHARACTER(LEN=MAX_NAME_LEN), ALLOCATABLE :: CValues(:) ! For returned array data

    !--- Variables
    TYPE(ValueList_t), POINTER :: ptr
    CHARACTER(LEN=MAX_NAME_LEN) :: tmpStr
    LOGICAL :: GotIt
    INTEGER :: loop, amount, alloc_stat
    CHARACTER(LEN=10) :: tmpFormat

    !--- Initializations
    NULLIFY(ptr)
    Found = .FALSE.

    ! Count the amount of defined strings for allocation
    amount = 0
    loop = 1
    GotIt = .TRUE.
    DO WHILE (GotIt)
      ! Tries to ensure that the given integer doesn't have extra spaces before it in char format
      ! Scales until a number with a width of 9 (limited by tmpFormat)
      WRITE(tmpFormat,'(A,I1,A)') '(A,A,I', IntWidth(loop),')'
      WRITE(tmpStr,tmpFormat) TRIM(Name),' ',loop
      ptr => ListFind( List,tmpStr,GotIt )

!      WRITE(*,*) 'TEMP: ', tmpStr

      ! Continues until first name is not found
      IF (GotIt) THEN
        IF (.NOT. ASSOCIATED(ptr)) THEN
          GotIt = .FALSE.
          RETURN
        ELSE
          amount = amount + 1
          loop = loop + 1
        END IF
      END IF
    END DO

!    WRITE (*,*) 'amount: ', amount
    ! TODO: Better error message for when amount of found content is not enough or is too much?
    IF ( amount .LE. 0 ) RETURN ! No strings found

    ! Allocation
    ALLOCATE ( CValues(amount), STAT = alloc_stat )
    IF ( alloc_stat .NE. 0 ) THEN
      CALL Fatal('GridDataMapper','Memory ran out')
    END IF

    ! Getting the strings
    DO loop = 1,amount,1
      WRITE(tmpFormat,'(A,I1,A)') '(A,A,I', IntWidth(loop) ,')'
      WRITE(tmpStr,tmpFormat) TRIM(Name),' ',loop
      CValues(loop) = GetString( List,tmpStr,GotIt )
      IF (.NOT. GotIt) THEN
        CALL Fatal('GridDataMapper','Obtained string did not exist after all')
      END IF
    END DO
   
    Found = .TRUE.

  END SUBROUTINE ListGetStrings 

END MODULE MapperUtils
