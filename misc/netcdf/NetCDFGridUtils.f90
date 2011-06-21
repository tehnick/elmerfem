!------------------------------------------------------------------------------
! Vili Forsell
! Created: 13.6.2011
! Last Modified: 20.6.2011
!------------------------------------------------------------------------------
! Contains tools for
! - getting the essential information on the uniform NetCDF grid; GetNetCDFGridParameters()
! - adjusting the grid parameters on basis of a mask; Focus2DNetCDFGrid()
!------------------------------------------------------------------------------
MODULE NetCDFGridUtils
  USE DefUtils
  USE NetCDF
  USE Messages
  USE NetCDFGeneralUtils, ONLY: G_Error
  IMPLICIT NONE


  CONTAINS

    !------------------ GetNetCDFGridParameters() ----------------------
    !--- Takes the limits for the uniform grid of NetCDF
    !--- (x0,y0) is the lower left corner, (dx,dy) contains the associated step sizes,
    !---  and (nxmax,nymax) are the amounts of steps
    SUBROUTINE GetNetCDFGridParameters( NCID,x0,dx,nmax,DIM_IDS,DIM_LENS )
    !-------------------------------------------------------------------
  
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NCID
      REAL(KIND=dp), INTENT(INOUT) :: x0(2), dx(2)
      INTEGER, INTENT(INOUT) :: nmax(2)
      INTEGER, INTENT(IN) :: DIM_IDS(:),DIM_LENS(:)
      INTEGER :: first_two(2), ind, status, ind_vec(1),count_vec(1)
      REAL(KIND=dp) :: x1(2)
  
      ! Takes the first two values of all grid dimensions to determine the whole grid
      ind_vec = 1
      count_vec = 2
  
      ! Takes the first two values for each dimension, saves the information necessary for reconstructing the grid
      ! Assumes the NetCDF grid is uniform, the indexing of the dimensions enabled via the usual convention of variables with same names
      DO ind = 1,(size(DIM_LENS) - 1),1 ! NOTE! Ignores the time dimension, which is last
  
        first_two = 0
        IF (DIM_LENS(ind) <= 1) THEN
          CALL Fatal('GridDataMapper','Scalar dimension encountered; No obtainable difference')
        END IF
        status = NF90_GET_VAR(NCID,DIM_IDS(ind),first_two,ind_vec,count_vec)
        IF ( G_ERROR(status,'NetCDF dimension values access failed.') ) THEN
          CALL abort()
        END IF
        x0(ind) = first_two(1)
        dx(ind) = first_two(2) - first_two(1)
        nmax(ind) = DIM_LENS(ind)
      END DO
      
    END SUBROUTINE GetNetCDFGridParameters
  
  
    !------------------ FocusNetCDFGrid ----------------------
    !--- Tightens the original bounding box until it touches the masked area
    SUBROUTINE Focus2DNetCDFGrid( NCID,MASK_VAR,MASK_LIMIT,x0,DX,nmax,TIME,DIM_IDS,DIM_LENS )
    !-------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NCID,DIM_IDS(:),DIM_LENS(:)
      INTEGER, INTENT(INOUT) :: nmax(:)
      REAL(KIND=dp), INTENT(INOUT) :: x0(:)
      REAL(KIND=dp), INTENT(IN) :: DX(:)
      REAL(KIND=dp) :: i_bl(2), i_br(2), i_ul(2), i_ur(2) ! Indices for the grid boundaries
      CHARACTER(len = *), INTENT(IN) :: MASK_VAR ! The variable for catching the masking data from NetCDF
      REAL(KIND=dp), INTENT(IN) :: MASK_LIMIT ! The limiting value which decides the mask has been reached
      INTEGER, INTENT(IN) :: TIME ! Used time instance
      
      REAL(KIND=dp), ALLOCATABLE :: scan_horizontal(:), scan_vertical(:) ! Scanning lines of differing dimensions
      INTEGER :: low_limits(2,4), high_limits(2,4),i0(2),i1(2),line,mask_ind, alloc_stat ! The lowest and highest limits for each scanning line
      INTEGER :: status, mask_id, index_vector(3), count_vert(3), count_horiz(3) ! For NetCDF access to the mask
      LOGICAL :: is_vert(4), finished(4) ! True if the scanning line has reached the data mask
  
      status = NF90_INQ_VARID(NCID,MASK_VAR,mask_id)
      IF ( G_Error(status,'NetCDF mask variable name not found.') ) THEN
        CALL abort()
      END IF
  
      index_vector = 1 ! Specialize later on
      ! Counts over one time value
      count_vert = (/ 1,DIM_LENS(2),1 /) ! Vertical scanning line count
      count_horiz = (/ DIM_LENS(1),1,1 /) ! Horizontal scanning line count
      finished = .FALSE.
      is_vert = .FALSE.
      is_vert(1) = .TRUE. ! left scanning line is vertical
      is_vert(3) = .TRUE. ! right scanning line is vertical
      i0(:) = (/1,1/) ! Indices for lower left corner
      i1(:) = nmax(:) ! Indices for upper right corner
  
      ! Allocates the scanning lines ; cannot avoid using at most the dimension sizes amount of memory at some point,
      ! and memory allocation/deallocation to accomodate for changing sizes would be slow, so of constant size.
      ALLOCATE (scan_horizontal(DIM_LENS(1)), scan_vertical(DIM_LENS(2)), STAT = alloc_stat)
      IF ( alloc_stat .NE. 0 ) THEN
        CALL Fatal('GridDataMapper','Scanning line memory allocation failed')
      END IF
  
      ! Collect the four corners of the grid (b - bottom, l - left, u - up, r - right)
      i_bl(:) = i0(:)
      i_br(1) = i1(1)
      i_br(2) = i0(2)
      i_ul(1) = i0(1)
      i_ul(2) = i1(2)
      i_ur(:) = i1(:)
  
      ! Initialize low_limits and high_limits with the input grid points
      ! ORDER of scanning lines: Left, down, right, up.
      low_limits(:,1) = i_bl(:)
      low_limits(:,2) = i_bl(:)
      low_limits(:,3) = i_br(:)
      low_limits(:,4) = i_ul(:)
  
      high_limits(:,1) = i_ul(:)
      high_limits(:,2) = i_br(:)
      high_limits(:,3) = i_ur(:)
      high_limits(:,4) = i_ur(:)
  
      ! Tunes each scanning line at a time to find the suitable rectangular grid region
      DO line = 1,size(finished),1
       
        ! Chooses the right line type, fills it with data, finds the mask, updates the limits of the intersecting lines
        IF ( is_vert(line) ) THEN ! 1) A vertical line
  
          ! Focuses the boundary until it's done
          DO WHILE (.NOT. finished(line) )
  
            ! A) Gather data for scanning
            ! The x and time invariant during the search
            index_vector = (/low_limits(1,line),1,1/)
  
            status = NF90_GET_VAR(NCID,mask_id,scan_vertical,index_vector,count_vert)
            IF ( G_ERROR(status,'NetCDF mask variable access failed.') ) THEN
              CALL abort()
            END IF
  
            ! B) The scan for the mask along y coordinate
            DO mask_ind = 1,dim_lens(2)
              ! If reaches the mask, the current line is finished
              IF ( scan_vertical(mask_ind) .GT. MASK_LIMIT ) THEN
                finished(line) = .TRUE.
                EXIT
              END IF
            END DO
  
            ! C) Every vertical line out of the box handled, tighten the box
            IF ( .NOT. finished(line) ) THEN
              IF ( line .EQ. 1 ) THEN ! Left line moves to right
                low_limits(1,line) = low_limits(1,line) + 1
                high_limits(1,line) = high_limits(1,line) + 1
              ELSE ! Right line moves to left
                low_limits(1,line) = low_limits(1,line) - 1
                high_limits(1,line) = high_limits(1,line) - 1
              END IF
            END IF
  
            ! D) Finishing criteria, if no mask found (left and right vertical lines reach each other)
            IF ( low_limits(1,1) .EQ. low_limits(1,3) ) THEN
              finished(1) = .TRUE.
              finished(3) = .TRUE.
            END IF
          END DO
  
          ! E) Vertical line's x coordinates restrain the intersecting horizontal lines
          IF ( line .EQ. 1 ) THEN ! Left vertical scanning line
            low_limits(1,2) = low_limits(1,line) ! Down horizontal, low
            low_limits(1,4) = low_limits(1,line) ! Up horizontal, low
          ELSE ! Right vertical scanning line
            high_limits(1,2) = low_limits(1,line) ! Down horizontal, high
            high_limits(1,4) = low_limits(1,line) ! Up horizontal, high
          END IF
  
        ELSE ! 2) A horizontal line
  
          ! Focuses the boundary until it's done
          DO WHILE (.NOT. finished(line) )
  
            ! A) Gather data for scanning
            ! The y and time invariant during the search
            index_vector = (/1,low_limits(2,line),1/) 
  
            status = NF90_GET_VAR(NCID,mask_id,scan_horizontal,index_vector,count_horiz)
            IF ( G_ERROR(status,'NetCDF mask variable access failed.') ) THEN
              CALL abort()
            END IF
  
            ! B) The scan for the mask 
            DO mask_ind = 1,dim_lens(1)
              IF ( scan_horizontal(mask_ind) .GT. MASK_LIMIT ) THEN
                finished(line) = .TRUE.
                EXIT
              END IF
            END DO
  
            ! C) Every horizontal line out of the box has been handled
            IF ( .NOT. finished(line) ) THEN
              IF ( line .EQ. 2 ) THEN ! Lower line moves up
                low_limits(2,line) = low_limits(2,line) + 1
                high_limits(2,line) = high_limits(2,line) + 1
              ELSE ! Upper line moves down
                low_limits(2,line) = low_limits(2,line) - 1
                high_limits(2,line) = high_limits(2,line) - 1
              END IF
            END IF
  
  
  
            ! D) Finishing criteria, if no mask found (upper and lower horizontal line reach each other)
            IF ( low_limits(2,2) .EQ. low_limits(2,4) ) THEN
              finished(2) = .TRUE.
              finished(4) = .TRUE.
            END IF
          END DO
  
          ! E) Horizontal line's y coordinates restrain the intersecting vertical lines
          IF ( line .EQ. 2 ) THEN ! Lower horizontal scanning line
            low_limits(2,1) = low_limits(2,line) ! Left vertical, low
            low_limits(2,3) = low_limits(2,line) ! Right vertical, low
          ELSE ! Upper horizontal scanning line
            high_limits(2,1) = low_limits(2,line) ! Left vertical, high
            high_limits(2,3) = low_limits(2,line) ! Right vertical, high
          END IF
  
        END IF
  
      END DO
  
      !---- Finally, updates the values
      i0(:) = low_limits(:,1)
      i1(:) = high_limits(:,4)
      x0(:) = x0(:) + (i0(:) - 1)*DX(:)
      nmax(:) = i1(:) - i0(:) + 1
  
      WRITE (Message,'(A)') '2D grid focusing complete:'
      CALL Info('GridDataMapper', Message)
      WRITE (Message,'(A,2(I5),A,2(I5))') 'Lower left indices: ', i0, '   upper right indices: ', i1
      CALL Info('GridDataMapper', Message)
      WRITE (Message,'(A,2(F14.1),A,2(I5))') 'x0: ', x0, '    nmax: ', nmax
      CALL Info('GridDataMapper', Message)
  
    END SUBROUTINE Focus2DNetCDFGrid

END MODULE NetCDFGridUtils
