!------------------------------------------------------------------------------
! Vili Forsell
! Created: 13.6.2011
! Last Modified: 30.6.2011
!------------------------------------------------------------------------------
! This module contains functions for
! - getting dimensions sizes and NetCDF identifiers; GetAllDimensions()
! - getting data from NetCDF files; GetFromNetCDF()
! - handling NetCDF status errors; G_Error()
!------------------------------------------------------------------------------
MODULE NetCDFGeneralUtils
  USE DefUtils, ONLY: dp, MAX_NAME_LEN
  USE NetCDF
  USE Messages
  IMPLICIT NONE
  LOGICAL, PARAMETER :: DEBUG_UTILS = .FALSE.

  !--- A type for time dimension values
  TYPE TimeType_t
    LOGICAL :: is_defined
    REAL(KIND=dp) :: val ! Exact time value
    INTEGER :: id, & ! Dimension NetCDF id
              len, & ! Dimension length/size
              low, & ! Nearest lower index
              high ! Nearest higher index
    LOGICAL :: doInterpolation ! True, if the exact value is not an integer and, hence, requires interpolation
  END TYPE TimeType_t

  CONTAINS
  
    !------------------ CloseNetCDF() -----------------------
    !--- Closes the given NetCDF file
    SUBROUTINE CloseNetCDF( NCID )
      USE NetCDF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NCID
      INTEGER :: status
      
      status = NF90_CLOSE(NCID)
      IF ( G_ERROR(status,'Failed to close NetCDF file.') ) THEN
        CALL abort()
      END IF
    END SUBROUTINE CloseNetCDF
  
    !------------------ GetDimension() ----------------------
    !--- Takes the NetCDF file identifier and dimension name (as in NetCDF) and gets the id and length of the dimension, or abort otherwise
    SUBROUTINE GetDimension( NCID, DIM_NAME, dim_id, dim_len )
    !--------------------------------------------------------
      USE Messages
      IMPLICIT NONE
      !--- Arguments
      INTEGER, INTENT(IN) :: NCID
      CHARACTER (*), INTENT(IN) :: DIM_NAME
      INTEGER, INTENT(OUT) :: dim_id, dim_len 
      
      !--- Variables
      CHARACTER :: tmp_name ! Temporary name
      INTEGER :: status ! Results and status information from NetCDF
      INTEGER, PARAMETER :: sentinel = -1 ! Default intial value in case of error

      !--- Initializations      
      dim_id = sentinel
      dim_len = sentinel
      
      !--- Get dimension information and check success
      status = NF90_INQ_DIMID(NCID,DIM_NAME,dim_id)
      IF ( .NOT. G_Error(status, 'Dimension identifier could not be found.') ) THEN
        status = NF90_INQUIRE_DIMENSION(NCID,dim_id,tmp_name,dim_len)
        IF ( G_Error(status, 'Dimension could not be inquired.') ) THEN
          dim_id = sentinel
          dim_len = sentinel
          CALL abort()
        END IF
      ELSE
        dim_id = sentinel
        CALL abort()
      END IF
    
    IF ( DEBUG_UTILS ) THEN ! Debug printouts
      WRITE (Message,'(A,A10,A,I5,A,I10,A)') 'Dimension: ', DIM_NAME,' with id ', dim_id, ' and size ', dim_len, ' read correctly'
      CALL Info('GridDataMapper',Message)
    END IF
    END SUBROUTINE GetDimension
   
  
    !------------------ GetAllDimensions() ----------------------
    !--- Takes the NetCDF file and name identifiers (as in NetCDF) and gets the ids and lengths of the dimensions, or abort otherwise
    SUBROUTINE GetAllDimensions( NCID, NAMES, dim_ids, dim_lens )
    !------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER (len = MAX_NAME_LEN), INTENT(IN) :: NAMES(:)
      INTEGER, INTENT(IN) :: NCID
      INTEGER, ALLOCATABLE, INTENT(OUT) :: dim_ids(:),dim_lens(:)
      INTEGER :: alloc_stat, nm

      IF ( (size(NAMES,1) .NE. size(dim_ids)) .AND. (size(dim_ids) .NE. size(dim_lens)) ) THEN
        CALL Fatal('GridDataMapper','GetAllDimensions() input dimensions do not agree!')
      END IF

      ! Allocates the result vectors
      ALLOCATE ( dim_ids(size(NAMES,1)), dim_lens(size(NAMES,1)), STAT = alloc_stat )
      IF ( alloc_stat .NE. 0 ) THEN
        CALL Fatal('GridDataMapper','Memory ran out')
      END IF

      ! Collects the data for each name in order
      DO nm = 1,size(NAMES,1),1
        CALL GetDimension( NCID,NAMES(nm),dim_ids(nm),dim_lens(nm) )
      END DO
!      WRITE(*,*) 'End'
    END SUBROUTINE GetAllDimensions
  
   
    !----------------- GetFromNetCDF() --------------------
    !--- Reads the given variable name and returns the value from NetCDF grid
    !--- TODO: Generalize this!!!!
    FUNCTION GetFromNetCDF( NCID, VAR_ID, LOC, LOC_TIME, TIME, DIM_LENS, accessed, OUT_SIZE ) RESULT( success )
    !------------------------------------------------------
      USE NetCDF
      IMPLICIT NONE

      !--- Arguments
      INTEGER, INTENT(IN) :: DIM_LENS(:), VAR_ID
      INTEGER, INTENT(IN) :: NCID, LOC(:), LOC_TIME
      TYPE(TimeType_t), INTENT(IN) :: TIME
      INTEGER, INTENT(IN) ::  OUT_SIZE(:) ! The sizes of each dimension for the return type
      LOGICAL :: success ! Output: TRUE if all ok

      !--- Variables
      INTEGER :: DIM_COUNT,TOTAL_SIZE
      INTEGER :: alloc_stat ! alloc_stat for allocation status
      INTEGER, ALLOCATABLE :: COUNT_VECTOR(:)
      ! COUNT_VECTOR is the amount of nodes taken
      ! starting from corresponding index vector locations (slabs of data)
      INTEGER, ALLOCATABLE :: locs(:,:) ! First column is left limit, second column is right limit
      INTEGER :: loop, status
      REAL (KIND=dp), ALLOCATABLE, INTENT(INOUT) :: accessed(:) ! Later reshaped to proper dimensions
      
      ! Initializations

      ! Checks if time dimension is taken into account (picks always just one point)
      DIM_COUNT = size(DIM_LENS,1)
      IF (TIME % IS_DEFINED) DIM_COUNT = DIM_COUNT + 1 ! Takes one more dimension for time

      ! The same calculations would be done in any case; uses a little memory to save here
      TOTAL_SIZE = 1
      DO loop = 1,size(OUT_SIZE,1),1
        TOTAL_SIZE = TOTAL_SIZE*OUT_SIZE(loop)
      END DO

      success = .FALSE. ! For checking if all went ok (allows later error recuperation)
      
      ALLOCATE ( accessed(TOTAL_SIZE), COUNT_VECTOR(DIM_COUNT),locs(DIM_COUNT,2), STAT = alloc_stat )
      IF ( alloc_stat .NE. 0 ) THEN
        CALL Fatal('GridDataMapper','Memory ran out')
      END IF
  
      accessed = 0

      ! If has time, then the last dimension is time and is set in count vector and locs
      ! TODO: ADD CHECK: Otherwise size(OUT_SIZE) = size(LOC) = size(DIM_LENS) = DIM_COUNT
      COUNT_VECTOR(1:size(OUT_SIZE)) = OUT_SIZE(:)
      locs(1:size(LOC),1) = LOC(:)
      IF ( TIME % IS_DEFINED ) THEN
        COUNT_VECTOR(DIM_COUNT) = 1
        locs(DIM_COUNT,1) = LOC_TIME
      END IF

      locs(:,2) = locs(:,1) + COUNT_VECTOR(:) - 1 ! Covers the stencil area starting from left indices
      
      ! Checks each dimension range (and, hence, access attempt)
      DO loop = 1,size(DIM_LENS),1
        IF ( (locs(loop,1) .LT. 1) .OR. (DIM_LENS(loop) .LT. locs(loop,2)) ) THEN
          WRITE (*,'(A,/,3(I10),/,3(I10))') 'Locs: ', locs
          WRITE (*,'(A,/,3(I10))') 'Dims: ', DIM_LENS
          CALL Fatal('GridDataMapper','Indexing parameter(s) out of bounds.')
        END IF
      END DO
      
      IF ( TIME % IS_DEFINED ) THEN
        IF ( (locs(DIM_COUNT,1) .LT. 1) .OR. (TIME % LEN .LT. locs(loop,2)) ) THEN
          WRITE (*,'(A,/,3(I10),/,3(I10))') 'Locs: ', locs
          WRITE (*,'(A,/,3(I10))') 'Dims: ', DIM_LENS
          CALL Fatal('GridDataMapper','Indexing parameter(s) out of bounds.')
        END IF
      END IF

      !--- The dimensions and the locations have been read and checked; NetCDF accessing info is a-ok
      
      ! Access variable and take the values
      status = NF90_GET_VAR(NCID,var_id,accessed,locs(:,1),COUNT_VECTOR)
      IF ( G_ERROR(status,'NetCDF variable access failed.') ) THEN
        accessed = 0
        CALL abort()
      END IF
      
  !    outcome = TRANSPOSE(outcome) TODO: Are dimensions a-ok?
      success = .TRUE. ! Successful
  
    END FUNCTION GetFromNetCDF
   


  !----------------- TimeValueToIndex() ---------------
  !--- Takes a NetCDF time value and converts it into an index
  FUNCTION TimeValueToIndex(NCID,TIME_NAME,DIM_ID,DIM_LEN,t_val) RESULT(t_ind)
    IMPLICIT NONE

    !--- Arguments
    CHARACTER(len = MAX_NAME_LEN), INTENT(IN) :: TIME_NAME
    REAL(KIND=dp), INTENT(IN) :: t_val
    INTEGER, INTENT(IN) :: NCID, DIM_ID, DIM_LEN
    REAL(KIND=dp) :: t_ind ! Output

     !--- Variables
    REAL(KIND=dp) :: t_min, t_max, t_tmp1(1), t_tmp2(2), t_diff
    INTEGER :: time_id, status
    INTEGER :: index_scalar(1), count_scalar(1)

    t_ind = -1.0_dp ! Initialization to out of bounds
    index_scalar = 1 ! Initialized to min value
    count_scalar = 2
    time_id = DIM_ID ! Last dimension is time
  
    ! 1) Inquire time variable's id
    status = NF90_INQ_VARID(NCID,TIME_NAME,time_id)
    IF ( G_Error(status,'NetCDF time variable name not found.') ) THEN
      RETURN
    END IF

    ! 2) Get the time range from NetCDF
    status = NF90_GET_VAR(NCID,time_id,t_tmp2,index_scalar,count_scalar)
    IF ( G_Error(status,'First NetCDF time value not found') ) THEN
      RETURN
    END IF
    t_min = t_tmp2(1)
    t_diff = t_tmp2(2) - t_tmp2(1)

    count_scalar = 1
    index_scalar = DIM_LEN ! Pick the last max value
 
    status = NF90_GET_VAR(NCID,time_id,t_tmp1,index_scalar,count_scalar)
    IF ( G_Error(status,'Last NetCDF time value not found') ) THEN
      RETURN
    END IF
    t_max = t_tmp1(1)

    ! Check that input is within range
    IF ( t_val < t_min .OR. t_val > t_max ) THEN
      WRITE (Message,'(A,F7.2,A,F7.2,A,F7.2,A,F7.2)') 'Input value ', t_val, &
              ' is not within range [',t_min,', ',t_max,'] with step', t_diff
      CALL Fatal('GridDataMapper', Message)
    END IF

    ! 3) Use the time range to find the index for the time value (NetCDF variables uniform)
    t_ind = ((t_val - t_min)/t_diff) + 1 ! Uniform grid: just remove the bias and normalize the difference out
    ! No rounding for it is interpolated later on
    WRITE (Message, '(A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2)') 'Time index for given value ', &
                        t_val, ' is ', t_ind, ' over range [', t_min,',',t_max,'] with step ', t_diff
    CALL Info('GridDataMapper', Message)

  END FUNCTION
 
    !-------------------- G_Error() ------------------------
    !----- Checks the status and if failure, prints the error message and returns .TRUE.
    !-------------------------------------------------------
    FUNCTION G_Error( status, msg ) RESULT(erred)
      IMPLICIT NONE
      
      !----- Declarations
      INTEGER, INTENT(IN) :: status ! Status value
      CHARACTER (len = *), INTENT(IN) :: msg ! Error details
      LOGICAL :: erred ! True, if all ok; False otherwise
      
      !----- Checks for errors
      erred = .FALSE.
      IF ( status .NE. NF90_NOERR ) THEN ! Error encountered
        erred = .TRUE.
        CALL Fatal( 'GridDataMapper', msg )
      END IF
    
    END FUNCTION G_Error

END MODULE NetCDFGeneralUtils

