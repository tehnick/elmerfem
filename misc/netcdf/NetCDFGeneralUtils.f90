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
    FUNCTION GetFromNetCDF( NCID, VAR_NAME, outcome, LOC_X, LOC_Y, LOC_TIME, DIM_IDS, DIM_LENS, IS_STENCIL ) RESULT( success )
    !------------------------------------------------------
      USE NetCDF
      IMPLICIT NONE

      !--- Arguments
      CHARACTER (*), INTENT(IN) :: VAR_NAME
      INTEGER, INTENT(IN) :: DIM_IDS(:), DIM_LENS(:)
      INTEGER, INTENT(IN) :: NCID, LOC_X, LOC_Y, LOC_TIME
      LOGICAL, INTENT(IN) :: IS_STENCIL ! True if collects a stencil of the size outcome with the locations defining the lower left corner
      REAL (KIND=dp), INTENT(INOUT) :: outcome(:,:) ! The result defines the dimensionality of the access; check it's square later on
      LOGICAL :: success ! Output: TRUE if all ok

       !--- Variables
      INTEGER :: DIM_COUNT
      INTEGER :: SLAB, NEIGHBOURS, alloc_stat ! alloc_stat for allocation status
      INTEGER, ALLOCATABLE :: COUNT_VECTOR(:)
      INTEGER, ALLOCATABLE :: index_vector(:)
      ! NEIGHBORS is the amount of adjacent nodes taken, COUNT_VECTOR is the amount of nodes taken
      ! starting from corresponding index vector locations (slabs of data)
      INTEGER, ALLOCATABLE :: locs(:,:) ! First column is left limit, second column is right limit
      INTEGER :: d, var_id, status
      REAL (KIND=dp), ALLOCATABLE :: accessed(:,:,:) ! All dimensions have same amount of values 

      
      ! Checks that input makes sense; it should be a square matrix
      IF ( size(outcome,1) .NE. size(outcome,2) ) THEN
        CALL Fatal('GridDataMapper','Result array is not square nor scalar.')
      END IF
        
      IF (.NOT. IS_STENCIL) THEN  ! In the case of getting a centered block
        IF ( mod( (size(outcome,1) - 1), 2 ) .NE. 0 ) THEN
          CALL Fatal('GridDataMapper', 'Result array ill-sized for neighbouring nodes.')
        END IF
      END IF ! In the case of wanting a stencil; any existing result matrix is of good size
  
      !--- Now the input & result matrix "outcome" must be a square matrix with an even amount of adjacent nodes in it
      
      ! Initializations
      DIM_COUNT = size(DIM_IDS,1)
      success = .FALSE. ! For checking if all went ok (allows later error recuperation)
      NEIGHBOURS = (size(outcome,1) - 1)/2
      SLAB = size(outcome,1)
  
      ALLOCATE ( accessed(SLAB,SLAB,1), COUNT_VECTOR(DIM_COUNT),index_vector(DIM_COUNT),locs(DIM_COUNT,2), STAT = alloc_stat )
      IF ( alloc_stat .NE. 0 ) THEN
        CALL Fatal('GridDataMapper','Memory ran out')
      END IF
  
      COUNT_VECTOR = SLAB
      COUNT_VECTOR(3) = 1 ! Takes only over one time value
      accessed = 0
      locs(:,1) = (/ LOC_X, LOC_Y, LOC_TIME /) ! Possibly infinite dimension first
      locs(:,2) = (/ LOC_X, LOC_Y, LOC_TIME /)
  
      IF (.NOT. IS_STENCIL) THEN
        locs(1:2,1) = locs(1:2,1) - NEIGHBOURS ! This column is also used as NetCDF index vector
        locs(1:2,2) = locs(1:2,2) + NEIGHBOURS
      ELSE
        locs(1:2,2) = locs(1:2,2) + SLAB - 1  ! Stencil takes the whole area starting from lower left indices
      END IF
  
      DO d = 1,size(index_vector,1),1
        index_vector(d) = locs(d,1)
      END DO
      
      ! Checks that arrays have been initialized with proper size (should never fail)
      IF ( (size(locs,1) .NE. size(dim_ids)) .OR. (NEIGHBOURS .LT. 0) ) THEN
        CALL Fatal('GridDataMapper','Assumed dimension sizes mismatch')
      END IF
      
      ! Checks each dimension range (and, hence, access attempt)
      DO d = 1,DIM_COUNT,1
      
        IF ( (locs(d,1) .LT. 1) .OR. (dim_lens(d) .LT. locs(d,2)) ) THEN
          WRITE (*,'(A,/,3(I10),/,3(I10))') 'Locs: ', locs
          WRITE (*,'(A,/,3(I10))') 'Dims: ', dim_lens
          CALL Fatal('GridDataMapper','Indexing parameter(s) out of bounds.')
        END IF
      END DO
      
      !--- The dimensions and the locations have been read and checked; NetCDF accessing info is a-ok
      
      ! Find variable to be accessed
      status = NF90_INQ_VARID(NCID,VAR_NAME,var_id)
      IF ( G_Error(status,'NetCDF variable name not found.') ) THEN
        CALL abort()
      ELSE
        ! Access variable and take the values
        status = NF90_GET_VAR(NCID,var_id,accessed,index_vector,COUNT_VECTOR)
        IF ( G_ERROR(status,'NetCDF variable access failed.') ) THEN
          accessed = 0
          CALL abort()
        END IF
      END IF
      
      outcome = accessed(:,:,1) ! Insert the result
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

