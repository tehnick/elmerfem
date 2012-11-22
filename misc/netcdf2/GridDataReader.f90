!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
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
! *****************************************************************************
!
!******************************************************************************
! *
! *  Authors: Peter Råback, Vili Forsell, Juha Ruokolainen
! *  Email:   Juha.Ruokolainen@csc.fi
! *  Web:     http://www.csc.fi/elmer
! *  Address: CSC - IT Center for Science Ltd.
! *           Keilaranta 14
! *           02101 Espoo, Finland 
! *
! *  Original Date: 7.6.2011
! *  Modification Date: 2.12.2011
! *
! ****************************************************************************


MODULE NetCDFInterface
  USE DefUtils
  USE NetCDF
  IMPLICIT NONE

  ! These global variables hide the NetCDF specific Id:s from the calling level.
  !----------------------------------------------------------------------------
  INTEGER :: FileId = -1
  INTEGER :: DimIds(4)
  INTEGER :: VarId          
  INTEGER :: NetCDFStatus
  LOGICAL :: Debug = .FALSE.
  
  SAVE FileId, DimIds, VarId, Debug
  PRIVATE Fileid, DimIds, VarId, Debug, NetCDFStatus

  CONTAINS
    
    !------------------------------------------------------------------------------------
    !> Gathers and initializes all the necessary NetCDF information for picking variables.
    !------------------------------------------------------------------------------------
    SUBROUTINE NetCDFInit(Params, NetDim, DimSize, TimeSize, x0, dx, t0, dt )
      !--------------------------------------------------
      TYPE(ValueList_t), POINTER, INTENT(IN) :: Params
      INTEGER, INTENT(OUT) :: NetDim                  ! Dimension of the NetCDF file
      INTEGER, INTENT(OUT) :: DimSize(:)              ! Lengths for all space dimensions
      INTEGER, INTENT(OUT) :: TimeSize                ! Length for time dimension
      REAL(KIND=dp), INTENT(OUT) :: x0(:)             ! Minimum coordinate values for the NetCDF grid
      REAL(KIND=dp), INTENT(OUT) :: dx(:)             ! Grid resolution for the NetCDF grid     
      REAL(KIND=dp), INTENT(OUT) :: t0                ! Minimum time value
      REAL(KIND=dp), INTENT(OUT) :: dt                ! Time resolution
      !------------------------------------------------------------------------------
      LOGICAL :: Found
      CHARACTER (len=MAX_NAME_LEN) :: CoordName       ! Name of the space or time coordinate
      CHARACTER (len=MAX_NAME_LEN) :: FileName        ! File name for reading the data (of .nc format)
      CHARACTER (len=MAX_NAME_LEN) :: str
      INTEGER :: i, j, k, dimid, size, VarDim, IndVec(1) 
      REAL(KIND=dp) :: FirstTwo(2), LastTwo(2), dx0, dx1, dx2

      ! Opening the NetCDF file  
      !------------------------------------------------------------------------------
      FileName = GetString( Params, "Filename", Found )
      IF ( .NOT. Found ) CALL Fatal('GridDataReader','No > Filename < specified')
      NetCDFstatus = NF90_OPEN(FileName,NF90_NOWRITE,FileId) 
      IF ( NetCDFstatus /= NF90_NOERR ) THEN 
        CALL Fatal( 'GridDataReader', 'NetCDF file could not be opened: '//TRIM(FileName)) 
      END IF
      
      x0 = 0.0_dp
      dx = 0.0_dp
      t0 = 0.0_dp
      dt = 0.0_dp
      DimIds = 0
      DimSize = 0
      TimeSize = 0
      NetDim = 0

      DO i=1,4
        IF( i == 1 ) THEN
          CoordName = GetString( Params, "X Name", Found )
        ELSE IF( i== 2 ) THEN
          CoordName = GetString( Params, "Y Name", Found )
        ELSE IF( i == 3 ) THEN
          CoordName = GetString( Params, "Z Name", Found )
        ELSE IF( i == 4 ) THEN
          CoordName = GetString( Params, "Time Name", Found ) 
        END IF
        
        IF(.NOT. Found ) THEN
          IF( i > 2 ) THEN
            CYCLE
          ELSE
            CALL Fatal('GridDataReader',"Unable to find compulsory coordinate name:"//TRIM(CoordName))
          END IF
        END IF
        IF( i <= 3 ) NetDim = i
        
        ! Get the dimension id
        NetCDFstatus = NF90_INQ_DIMID(FileId,CoordName,dimid)
        IF ( NetCDFstatus /= NF90_NOERR ) THEN 
          CALL Fatal('GridDataReader','Dimension identifier could not be found:'//TRIM(CoordName)) 
        END IF

        ! Get the variable id and ensure that it is 1D only - probably in vain
        NetCDFstatus = NF90_INQ_VARID(FileId,CoordName,varid)
        IF ( NetCDFstatus /= NF90_NOERR ) THEN 
          WRITE(Message,'(A,I0)') 'Variable identifier could not be found: '//TRIM(CoordName)
          CALL Fatal('GridDataReader',Message)
        END IF
        NetCDFstatus = NF90_INQUIRE_VARIABLE(FileId,varid,ndims=vardim)
        IF( VarDim /= 1 ) THEN
          WRITE(Message,'(A,I0)') 'Invalid dimensions for coordinate variable > '&
              //TRIM(CoordName)//' < : ',VarDim
          CALL Warn('GridDataReader',Message)
        END IF
      
        ! Takes the first two values for each dimension and computes the grid resolution from the data.
        ! Assumes the NetCDF grid is uniform, the indexing of the dimensions enabled via the usual convention.
        !----------------------------------------------------------------------------------------------------

        ! Get the size of the coordinate dimension
        NetCDFstatus = NF90_INQUIRE_DIMENSION(FileId,dimid,str,size)
        IF ( NetCDFstatus /= NF90_NOERR ) THEN 
          CALL Fatal('GridDataReader','Dimension could not be inquired.')
        END IF
        IF (size <= 1) THEN
          CALL Fatal('GridDataReader','Scalar dimension encountered; No obtainable difference: '//TRIM(str))
        END IF
        
        WRITE(Message,'(A,I0,A,I0)') 'Found dimension > '&
            //TRIM(CoordName)//' < with id ',dimid,' and size ',size
        CALL Info('GridDataReader',Message, Level=6 )
        
        
        FirstTwo = 0.0_dp
        IndVec = 1
        NetCDFstatus = NF90_GET_VAR(FileId,varid,FirstTwo,IndVec)
        IF ( NetCDFstatus /= NF90_NOERR ) THEN 
          CALL Fatal('GridDataReader','NetCDF dimension values access failed.') 
        END IF
        
        IndVec = size - 1
        NetCDFstatus = NF90_GET_VAR(FileId,varid,LastTwo,IndVec)
        IF ( NetCDFstatus /= NF90_NOERR ) THEN 
          CALL Fatal('GridDataReader','NetCDF dimension values access failed.') 
        END IF

        DimIds(i) = dimid
        
        dx0 = FirstTwo(2)-FirstTwo(1)
        dx1 = LastTwo(2)-LastTwo(1)
        dx2 = (LastTwo(2)-FirstTwo(1))/(size-1)
        
        IF( ABS(dx1-dx0) > 1.0d-3 * ABS(dx0) ) THEN
          CALL Fatal('GridDataReader','NetCDF dimension not uniform (1), code some more...')
        END IF
        IF( ABS(dx2-dx0) > 1.0d-3 * ABS(dx0) ) THEN
          CALL Fatal('GridDataReader','NetCDF dimension not uniform (2), code some more...')
        END IF

        WRITE(Message,'(A,ES12.3)') 'Grid parameter of dimension > '&
            //TRIM(CoordName)//' < is ',dx0
        CALL Info('GridDataReader',Message, Level=6 )

        WRITE(Message,'(A,2ES12.3,A)') 'Range of dimension > '&
            //TRIM(CoordName)//' < is [',FirstTwo(1),LastTwo(2),']'
        CALL Info('GridDataReader',Message, Level=6 )
        
        IF( i <= 3 ) THEN
          DimSize(i) = size
          x0(i) = FirstTwo(1)
          dx(i) = dx0
        ELSE
          TimeSize = size
          t0 = FirstTwo(1)
          dt = dx0
        END IF        

      END DO
      
    END SUBROUTINE NetCDFInit


    !-------------------------------------------------------------------------------
    !> Set NetCDF index of the variable to be mapped
    !-------------------------------------------------------------------------------
    SUBROUTINE NetCDFVariableInit( VarName ) 
      
      CHARACTER(*), INTENT(IN) :: VarName
      !-----------------------------------------------------------------------------      

      VarId = 0
      NetCDFstatus = NF90_INQ_VARID(FileId,VarName,VarId)
      IF ( NetCDFstatus /= NF90_NOERR ) THEN 
        CALL Fatal('GridDataReader','NetCDF variable name not found: '//TRIM(VarName)) 
      END IF
      
      IF(Debug) PRINT *,'NetCDF variable index: ',TRIM(VarName), VarId

    END SUBROUTINE NetCDFVariableInit
    
    
    !----------------------------------------------------------------------------------------------
    !> Reads the given variable and returns data on the desired cell on it from NetCDF grid.
    !> There are two versions since ideally the dimensions of the vectors and are different. 
    !> This is the 2D version. 
    !----------------------------------------------------------------------------------------------
    SUBROUTINE NetCDFDataCell2D( outcome, NetDim, DimIndex, TimeIndex )
      !------------------------------------------------------
      REAL (KIND=dp), INTENT(OUT) :: outcome(:,:,:) 
      INTEGER, INTENT(IN) :: NetDim
      INTEGER, INTENT(IN) :: DimIndex(:)
      INTEGER, INTENT(IN) :: TimeIndex
      !------------------------------------------------------      
      REAL (KIND=dp) :: stencil3D(2,2,1),stencil2D(2,2)
      INTEGER :: i,j,IndexVector3D(3), CountVector3D(3),&
          IndexVector2D(2),CountVector2D(2)
    
      ! Access variable and take the values
      !---------------------------------------------------------------------------------------------
      IF( TimeIndex == 0 ) THEN
        CountVector2D = (/ 2, 2 /)
        IndexVector2D = (/ DimIndex(1), DimIndex(2) /)
        NetCDFstatus = NF90_GET_VAR(FileId,VarId,stencil2D,IndexVector2D,CountVector2D)
        outcome(:,:,1) = stencil2D(:,:)
      ELSE
       CountVector3D = (/ 2, 2, 1 /)
        IndexVector3D = (/ DimIndex(1), DimIndex(2), TimeIndex /)
        NetCDFstatus = NF90_GET_VAR(FileId,VarId,stencil3D,IndexVector3D,CountVector3D)
        outcome(:,:,1) = stencil3D(:,:,1)
      END IF
      
      IF ( NetCDFstatus /= NF90_NOERR ) THEN 
        PRINT *,'FileId:',FileId
        PRINT *,'VarId:',VarId
        IF( TimeIndex == 0 ) THEN
          PRINT *,'IndexVector:',IndexVector2D
          PRINT *,'CountVector:',CountVector2D
        ELSE
          PRINT *,'IndexVector:',IndexVector3D
          PRINT *,'CountVector:',CountVector3D         
        END IF
        CALL Fatal('GridDataReader','NetCDF variable access failed in 2D.')
      END IF

    END SUBROUTINE NetCDFDataCell2D
  

    !----------------------------------------------------------------------------------------------
    !> The 3D version of the previous routine.
    !----------------------------------------------------------------------------------------------
   SUBROUTINE NetCDFDataCell3D( outcome, NetDim, DimIndex, TimeIndex )
      !------------------------------------------------------
      REAL (KIND=dp), INTENT(OUT) :: outcome(:,:,:) 
      INTEGER, INTENT(IN) :: NetDim
      INTEGER, INTENT(IN) :: DimIndex(:)
      INTEGER, INTENT(IN) :: TimeIndex
      !------------------------------------------------------      
      REAL (KIND=dp) :: stencil4D(2,2,2,1),stencil3d(2,2,2)
      INTEGER :: i,j
      INTEGER :: IndexVector3D(3), CountVector3D(3), &
          IndexVector4D(4), CountVector4D(4)
    
      ! Access variable and take the values
      !---------------------------------------------------------------------------------------------
      IF( TimeIndex == 0 ) THEN
        CountVector3D = (/ 2, 2, 2 /)
        IndexVector3D = (/ DimIndex(1), DimIndex(2), DimIndex(3) /)             
        NetCDFstatus = NF90_GET_VAR(FileId,VarId,stencil3D,IndexVector3D,CountVector3D)
        outcome(:,:,:) = stencil3D(:,:,:) 
      ELSE
        CountVector4D = (/ 2, 2, 2, 1 /)
        IndexVector4D = (/ DimIndex(1), DimIndex(2), DimIndex(3), TimeIndex /)
        NetCDFstatus = NF90_GET_VAR(FileId,VarId,stencil4D,IndexVector4D,CountVector4D)
        outcome(:,:,:) = stencil4D(:,:,:,1)
      END IF

       IF ( NetCDFstatus /= NF90_NOERR ) THEN 
        PRINT *,'FileId:',FileId
        PRINT *,'VarId:',VarId
        IF( TimeIndex == 0 ) THEN
          PRINT *,'IndexVector:',IndexVector3D
          PRINT *,'CountVector:',CountVector3D
        ELSE
          PRINT *,'IndexVector:',IndexVector4D
          PRINT *,'CountVector:',CountVector4D         
        END IF
        CALL Fatal('GridDataReader','NetCDF variable access failed in 3D.')
      END IF

    END SUBROUTINE NetCDFDataCell3D
    

    !----------------------------------------------------------------------------------
    ! Closes the active NetCDF file.
    !----------------------------------------------------------------------------------
    SUBROUTINE NetCDFClose( )
      USE NetCDF
      IMPLICIT NONE
      INTEGER :: status
      
      status = NF90_CLOSE(FileId)
      IF ( status /= NF90_NOERR ) THEN ! Error encountered
        CALL Fatal( 'GridDataReader', 'Failed to close NetCDF file' )
      END IF
      
    END SUBROUTINE NetCDFClose
    
  END MODULE NetCDFInterface
  !----------------------------------------------------------------------------------




!------------------------------------------------------------------------------
!> Solver for mapping data from uniform grids into Elmer mesh.
!> Currently netcdf interface has been implemented. 
!------------------------------------------------------------------------------
SUBROUTINE GridDataReader( Model,Solver,dtime,TransientSimulation )
!------------------------------------------------------------------------------

  USE DefUtils
  USE MeshUtils
  USE ElementUtils
  USE NetCDFInterface

  IMPLICIT NONE
 
  !------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dtime
  LOGICAL :: TransientSimulation
  !------------------------------------------------------------------------------
  LOGICAL :: Debug = .FALSE.
  TYPE(Variable_t), POINTER :: FieldVar, PrevFieldVar
  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(Solver_t), POINTER :: PSolver
  TYPE(ValueList_t), POINTER :: Params
  INTEGER :: n,k,node,MeshDim, NetDim,iTime,nTime
  INTEGER, POINTER :: FieldPerm(:)
  REAL(KIND=dp), POINTER :: Field(:)
  REAL(KIND=dp) :: x(3),dx(3),x0(3),x1(3),u1(3),u2(3),dt,t0,val,&
                   Eps(3),Time,x0e(3),x1e(3),pTime,EpsTime,q,r
  
  INTEGER :: DimSize(3),i 
  INTEGER :: TimeSize, IntTimeIndex,tnmax, NoVar
  INTEGER :: status, time_begin,time_end,NoHits,NoMisses
  CHARACTER (len = MAX_NAME_LEN) :: str, VarName, TargetName, &
      CoordSystem, TimeInterpolationMethod
  REAL(KIND=dp) :: Coeff, InterpMultiplier, InterpBias, TimeIndex, acc
  LOGICAL :: Found, IsTime, DoCoordinateTransformation, DoCoordMapping, &
      DoScaling, DoBoundingBox, DoPeriodic 
  INTEGER, POINTER :: CoordMapping(:), PeriodicDir(:)

  ! General initializations
  !------------------------------------------------------------------------------
  
  CALL Info('GridDataReader','-----------------------------------------', Level=5 )
  CALL Info('GridDataReader','Obtaining field(s) from grid NetCDF format',Level=5 )
  CALL ResetTimer('GridDataReader')
  
  !-- Pointer declarations
  PSolver => Solver
  Params => Solver % Values
  Mesh => Solver % Mesh
  MeshDim = Mesh % MeshDim
  
  ! Elmer resolution and coordinate system
  !------------------------------------------------------------------------------  
  CALL InitEpsilon( Params, Eps, EpsTime )
  
  CoordSystem = GetString( Params,'Coordinate Transformation',&
      DoCoordinateTransformation)

  CoordMapping => ListGetIntegerArray( Params,'Coordinate Mapping',&
      DoCoordMapping )
  IF( DoCoordMapping ) THEN
    IF ( SIZE(CoordMapping) /= 3 ) THEN
      WRITE( Message, * ) 'Invalid size of coordinate mapping: ', SIZE(CoordMapping)
      CALL Fatal( 'GridDataReader', Message )
    END IF
    DO i=1,3
      IF( .NOT. ANY( CoordMapping == i ) ) THEN
        WRITE( Message, * ) 'Coordinate mapping should be a permutation of 1,2 and 3'
        CALL Fatal( 'GridDataReader', Message )        
      END IF
    END DO
  END IF

  PeriodicDir => ListGetIntegerArray( Params,'Periodic Directions',&
      DoPeriodic )
 
  !------------------------------------------------------------------------------
  ! Initialize NetCDF data locations, sizes and resolution
  !------------------------------------------------------------------------------
  CALL NetCDFInit(Params, NetDim, DimSize, TimeSize, x0, dx, t0, dt )

  IF( NetDim < 1 .OR. NetDim > 3 ) THEN
    CALL Fatal('GridDataReader','NetCDF dimensions must be either 2 or 3!')
  END IF
  IF (MeshDim > NetDim ) THEN
    CALL Info('GridDataReader','Omitting the extra dimensions of Elmer mesh',Level=6)
  END IF
  x1 = x0 + (DimSize-1) * dx

  
  !------------------------------------------------------------------------------
  ! Optionally map the Elmer mesh so that it coinsides with the NetCDF mesh
  ! This is intended mainly for testing purposes etc.
  !------------------------------------------------------------------------------
  DoScaling = ListGetLogical( Params,'Enable Scaling',Found ) 
  
  DoBoundingBox = ListGetLogical( Params,'Check Bounding Box',Found ) 
  IF( DoScaling ) THEN
    IF( DoCoordinateTransformation .OR. DoCoordMapping ) THEN
      CALL Fatal('GridDataReader','Cannot do scaling and mapping together!')
    ELSE      
      DoBoundingBox = .TRUE.
    END IF
  END IF

  IF( DoBoundingBox ) THEN
    x = 0.0_dp
    x0e = HUGE( x0e )
    x1e = -HUGE( x1e )
    
    DO node=1, Mesh % NumberOfNodes
      IF( ASSOCIATED( FieldPerm ) ) THEN
        k = FieldPerm(node) 
        IF( k == 0 ) CYCLE
      ELSE        
        k = node
      END IF
      
      x(1) = Mesh % Nodes % x(node)
      x(2) = Mesh % Nodes % y(node)
      IF( NetDim == 3 ) x(3) = Mesh % Nodes % z(node)
      
      IF( DoCoordinateTransformation ) THEN
        x = CoordinateTransformation( x, CoordSystem )
      END IF
      
      IF( DoCoordMapping ) THEN
        x = x( CoordMapping(1:NetDim) ) 
      END IF
      
      x0e = MIN( x0e, x )
      x1e = MAX( x1e, x )
    END DO

    DO i=1,NetDim
      WRITE(Message,'(A,I0,A,2ES12.3,A)') 'Initial Elmer coordinate ',i,' range is [',x0e(i),x1e(i),']'
      CALL Info('GridDataReader',Message, Level=6 )
    END DO

    IF( DoScaling ) THEN
      Found = .FALSE.
      DO i=1,NetDim
        q = (x1(i) - x0(i) ) / ( x1e(i) - x0e(i) )
        r = x0(i) - q * x0e(i)
        
        acc = MAX( ABS(r), ABS( q- 1.0) )
        
        ! If the bounding box of the two meshes is the same do nothing
        IF( acc < 1.0d-8 ) CYCLE
        
        Found = .TRUE.
        IF( i == 1 ) THEN
          Mesh % Nodes % x = r + q * Mesh % Nodes % x 
        ELSE IF( i == 2 ) THEN
          Mesh % Nodes % y = r + q * Mesh % Nodes % y
        ELSE        
          Mesh % Nodes % z = r + q * Mesh % Nodes % z 
        END IF
      END DO
      
      IF( Found ) THEN
        x0e(1) = MINVAL(Mesh % Nodes % x)
        x0e(2) = MINVAL(Mesh % Nodes % y)
        x0e(3) = MINVAL(Mesh % Nodes % z)
        x1e(1) = MAXVAL(Mesh % Nodes % x)
        x1e(2) = MAXVAL(Mesh % Nodes % y)
        x1e(3) = MAXVAL(Mesh % Nodes % z)
        
        DO i=1,NetDim
          WRITE(Message,'(A,I0,A,2ES12.3,A)') 'Modified Elmer coordinate ',i,' range is [',x0e(i),x1e(i),']'
          CALL Info('GridDataReader',Message, Level=6 )
        END DO
      END IF
    ELSE
      IF( ALL( x0e >= x0 ) .AND. ALL( x1e <= x1 ) ) THEN
        CALL Info('GridDataReader','Elmer bounding box is within the NetCDF one!',Level=6)
      END IF

    END IF
  END IF



  !--------------------------------------------------------------------------------------
  ! Get the timestep at which interpolation is desired
  ! If the time does not coninside with a timestep in the file, two timesteps are needed.
  !--------------------------------------------------------------------------------------
  IF( TimeSize == 0 ) THEN
    CALL Info('GridDataReader','No time given, using 1st step',Level=6)
    IntTimeIndex = 0
    nTime = 1
    pTime = 1.0_dp
  ELSE
    CALL GetTimePoint(Params, t0, dt, TimeIndex )
    IntTimeIndex = NINT( TimeIndex ) 

    IF( ABS( TimeIndex - IntTimeIndex ) > EpsTime ) THEN
      IntTimeIndex = FLOOR( Time ) 
      nTime = 2
      pTime = 1 + IntTimeIndex - TimeIndex
      WRITE (Message,'(A,ES10.3,A)') 'Given time value ', TimeIndex , ' using time interpolation.'
    ELSE
      nTime = 1
      pTime = 1.0_dp
      WRITE (Message,'(A,ES10.3,A)') 'Given time value ', TimeIndex, ', no time interpolation used.'
    END IF
    CALL Info('GridDataReader',Message,Level=6)
  END IF

  IF( Debug ) THEN
    PRINT *,'nTime',nTime,TimeIndex,pTime
  END IF
  
  !-------------------------------------------------------------------------------
  ! Loop over variables to be mapped. 
  ! If no target variable is given it will be the same name as the primary variable.
  !-------------------------------------------------------------------------------
  NULLIFY( PrevFieldVar )

  NoVar = 0
  DO WHILE (.TRUE.) 
    ! Get NetCDF variable
    !-----------------------------------------------------------------------------
    NoVar = NoVar + 1
    WRITE( str,'(A,I0)') 'Variable ',NoVar
    VarName = GetString( Params,str, Found )
    IF(.NOT. Found ) THEN
      IF( NoVar == 1 ) THEN
        CALL Fatal('GridDataReader','Calling subroutine without > Variable 1 < defined!')
      END IF
      EXIT
    END IF
    CALL NetCDFVariableInit( VarName ) 

    ! Get Elmer variable, if not present create it.
    !-------------------------------------------------------------------------------
    WRITE( str,'(A,I0)') 'Target Variable ',NoVar
    TargetName = GetString( Params,str,Found )
    IF( .NOT. Found ) TargetName = VarName
    FieldVar => VariableGet( Mesh % Variables,TargetName )
    IF( .NOT. ASSOCIATED( FieldVar ) ) THEN
      CALL VariableAddVector( Mesh % Variables,Mesh,PSolver,TargetName,1) 
      FieldVar => VariableGet( Mesh % Variables,TargetName )      
    END IF
    Field => FieldVar % Values
    FieldPerm => FieldVar % Perm

    ! Set a constant background to the field. This can be done a priori
    ! since it does not interfere with the interpolation.
    !----------------------------------------------------------------------
    str = 'Interpolation Offset'
    InterpBias = GetCReal( Params,str,Found ) 
    IF( .NOT. Found ) THEN
      WRITE( str,'(A,I0)') TRIM(str)//' ',NoVar
      InterpBias = GetCReal( Params,str,Found ) 
      IF( .NOT. Found ) InterpBias = 0.0_dp
    END IF

    ! If the target variable is the same, then obviously it is so intentionally 
    ! to combine values from two different data sets.
    !--------------------------------------------------------------------------   
    IF( .NOT. ASSOCIATED( PrevFieldVar, FieldVar ) ) THEN
      Field = InterpBias
    END IF
    PrevFieldVar => FieldVar


    ! Multiply the field with a constant
    ! Here just the constant is obtained, multiplication is done later.
    !----------------------------------------------------------------------
    str = 'Interpolation Multiplier'
    InterpMultiplier = GetCReal( Params,str,Found ) 
    IF( .NOT. Found ) THEN
      WRITE( str,'(A,I0)') TRIM(str)//' ',NoVar
      InterpMultiplier = GetCReal( Params,str,Found ) 
      IF(.NOT. Found ) InterpMultiplier = 1.0_dp
    END IF

    ! Loop over 1 or 2 timesteps
    !----------------------------------------------------------------------
    DO iTime = 1, nTime 
      
      NoHits = 0
      IF( iTime == 2 ) THEN
        TimeIndex = TimeIndex + 1
        pTime = 1.0_dp - pTime
      END IF
      Coeff = InterpMultiplier * pTime 
      
      IF ( IntTimeIndex == 0 ) THEN
        CONTINUE
      ELSE IF ( IntTimeIndex < 1 .OR. IntTimeIndex > TimeSize ) THEN
        WRITE (Message, '(A,I0,A,I0,A)') 'Time value ', IntTimeIndex, ' is out of range (1,',TimeSize, ')'
        CALL Warn('GridDataReader',Message)
      END IF
      
      ! Go through the active nodes and perform interpolation
      !---------------------------------------------------------------------------  
      x = 0.0_dp
      DO node=1, Mesh % NumberOfNodes
        IF( ASSOCIATED( FieldPerm ) ) THEN
          k = FieldPerm(node) 
          IF( k == 0 ) CYCLE
        ELSE        
          k = node
        END IF
          
        ! The Elmer point of interest
        ! Use the leading dimension of NetCDF data - not of Elmer. 
        !-------------------------------------------------------------------------
        x(1) = Mesh % Nodes % x(node)
        x(2) = Mesh % Nodes % y(node)
        IF( NetDim == 3 ) x(3) = Mesh % Nodes % z(node)
        
        ! Coordinate mapping from Elmer (x,y) to the one used by NetCDF.
        !-------------------------------------------------------------------------
        IF( DoCoordinateTransformation ) THEN
          x = CoordinateTransformation( x, CoordSystem )
        END IF

        IF( DoCoordMapping ) THEN
          x = x( CoordMapping(1:NetDim) ) 
        END IF

        IF( DoPeriodic ) THEN
          DO i=1,NetDim
            IF( PeriodicDir(i) > 0 ) THEN
              IF( x(i) > x1(i) ) x(i) = x(i) - (x1(i) - x0(i) )
              IF( x(i) < x0(i) ) x(i) = x(i) + (x1(i) - x0(i) )             
            END IF
          END DO

        END IF

        Found = FDInterpolation(NetDim,x,DimSize,x0,dx,x1,Eps,IntTimeIndex,val) 

        IF( Found ) THEN
          Field(k) = Field(k) + Coeff * val
          NoHits = NoHits + 1
        END IF
        
      END DO
    END DO
  END DO

  NoMisses = Mesh % NumberOfNodes - NoHits
  IF( NoMisses > 0 ) THEN
    PRINT *,'Number of nodes:',Mesh % NumberOfNodes
    PRINT *,'Number of misses:',NoMisses 
  END IF
  
  ! Close the NetCDF file
  !------------------------------------------------------------------------------
  CALL NetCDFClose()

  CALL CheckTimer('GridDataReader',Delete=.TRUE.)
  CALL Info('GridDataReader','All done',Level=5)
  CALL Info('GridDataReader', '-----------------------------------------', Level=5 )

CONTAINS

  !------------------------------------------------------------------------------
  ! Initializes the resolution used in interpolation.
  !------------------------------------------------------------------------------
  SUBROUTINE InitEpsilon( Params, Eps, EpsTime )
    TYPE(ValueList_t), POINTER, INTENT(IN) :: Params
    REAL(KIND=dp), INTENT(INOUT) :: Eps(:), EpsTime
    !-----------------------------------------------------------------
    LOGICAL :: Found
    REAL(KIND=dp) :: eX, eY

    ! Epsilons are the relative tolerances for the amount 
    ! the Elmer grid point misses the bounds of the NetCDF bounding box
    !-------------------------------------------------------------------
    Eps = 0.0_dp
    Eps(1) = GetConstReal(Params, "X Epsilon", Found ) 
    IF ( .NOT. Found ) THEN
      CALL Warn('GridDataReader', 'Keyword > X Epsilon < not given, setting to default eps')
      Eps(1) = EPSILON( Eps(1) )
    END IF
    Eps(2) = GetConstReal(Params, "Y Epsilon", Found ) 
    IF ( .NOT. Found ) THEN
      CALL Info('GridDataReader', 'Keyword > Y Epsilon < not given, setting equal to > X Epsilon <',Level=6)
      Eps(2) = Eps(1)
    END IF
    IF( NetDim == 3 ) THEN
      Eps(3) = GetConstReal(Params, "Z Epsilon", Found ) 
      IF ( .NOT. Found ) THEN
        CALL Info('GridDataReader', 'Keyword > Z Epsilon < not given, setting equal to > X Epsilon <',Level=6)
        Eps(3) = Eps(1)
      END IF
    END IF

    EpsTime = GetConstReal(Params, "Time Epsilon", Found ) 
    IF(.NOT. Found) EpsTime = EPSILON( EpsTime ) 

    IF( Debug ) THEN
      PRINT *,'Eps',Eps,'EpsTime',EpsTime
    END IF

  END SUBROUTINE InitEpsilon
 

  !-------------------------------------------------------------------- 
  ! Initializes the time values
  !--------------------------------------------------------------------
  SUBROUTINE GetTimePoint( Params, t0, dt, TimeIndex )
  !----------------------------------------------------
    TYPE(ValueList_t), POINTER, INTENT(IN) :: Params
    REAL(KIND=dp), INTENT(IN) :: t0, dt
    REAL(KIND=dp), INTENT(OUT) :: TimeIndex
    !-----------------------------------------------------------------------
    LOGICAL :: IsTimeIndex, Found 
    REAL(KIND=dp) :: Time, Coeff
    INTEGER :: VisitedTimes =  0

    SAVE VisitedTimes

    VisitedTimes = VisitedTimes + 1
    IF( GetLogical( Params, "Is Time Counter", Found ) ) THEN
      TimeIndex = VisitedTimes
      RETURN
    END IF
   
    ! Get user-specified time or true physical time
    Time = GetCReal( Params, "Time Point", Found )
    IF( .NOT. Found ) THEN
      Time = GetTime() 
    
      ! Add possible offset in time
      Coeff = GetCReal( Params, "Time Offset", Found )     
      IF( Found ) Time = Time + Coeff
      
      ! Add possible multiplicator in time
      Coeff = GetCReal( Params, "Time Multiplier", Found )     
      IF( Found ) Time = Coeff * Time 
    END IF

    ! Check if time is assumed to be index variable, or true time
    IsTimeIndex = GetLogical( Params, "Is Time Index", Found ) 
    IF ( IsTimeIndex) THEN
      TimeIndex = Time
    ELSE
      TimeIndex =  1.0 + ( Time - t0 ) / dt
    END IF
  END SUBROUTINE GetTimePoint


  !--------------------------------------------------------------
  !> Performs linear interpolation 
  !--------------------------------------------------------------
  FUNCTION LinearInterpolation(x,u1,u2) RESULT(y)
    REAL(KIND=dp), INTENT(IN) :: u1(2),u2(2),x
    REAL(KIND=dp) :: y
    
    y = (((u2(2) - u1(2))/(u2(1) - u1(1)))*(x-u1(1)))+u1(2)
  END FUNCTION LinearInterpolation
  
  !----------------------------------------------------------------------------
  !> Performs bilinear interpolation on a stencil (2x2 matrix of corner values)
  !----------------------------------------------------------------------------
  FUNCTION BiLinearInterpolation(stencil,weights) RESULT(val)
    REAL(KIND=dp), INTENT(IN) :: stencil(:,:), weights(:)
    REAL(KIND=dp) :: val
    
    val = stencil(1,1)*(1-weights(1))*(1-weights(2)) + &
        stencil(2,1)*weights(1)*(1-weights(2)) + &
        stencil(1,2)*(1-weights(1))*weights(2) + &
        stencil(2,2)*weights(1)*weights(2)
    
  END FUNCTION BiLinearInterpolation
  
  !------------------------------------------------------------------------------ 
  !> Performs trilinear interpolation on a stencil (2x2x2 matrix of corner values)
  !------------------------------------------------------------------------------
  FUNCTION TriLinearInterpolation(stencil,weights) RESULT(val)
    REAL(KIND=dp), INTENT(IN) :: stencil(:,:,:), weights(:)
    REAL(KIND=dp) :: val, val1, val2
    
    val1 = BiLinearInterpolation(stencil(:,:,1),weights(1:2)) 
    val2 = BiLinearInterpolation(stencil(:,:,2),weights(1:2)) 
    val = val1*(1-weights(3)) + val2*weights(3)
  END FUNCTION TriLinearInterpolation
      
  !-------------------------------------------------------------------------------
  !> Interpolates one grid point given data on a finite difference stencil.
  !-------------------------------------------------------------------------------
  FUNCTION FDInterpolation(NetDim,x,DimSize,x0,dx,x1,Eps,TimeIndex,val) &
      RESULT( success )
    
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NetDim,DimSize(:)
    INTEGER, INTENT(IN) :: TimeIndex
    REAL(KIND=dp), INTENT(IN) :: x(:),x0(:),dx(:),x1(:),Eps(:)
    REAL(KIND=dp), INTENT(INOUT) :: val ! Final Elmer point and interpolated value 
    LOGICAL :: success
    !----------------------------------------------------------------------------------------
    INTEGER :: i, ind(3)
    REAL(KIND=dp) :: stencil(2,2,2)
    REAL(KIND=dp) :: Weights(3)
    REAL(KIND=dp) :: xi(3), xf(3)

!      WRITE (*,*) 'X: ', X
!      WRITE (*,*) 'X0: ', X0
!      WRITE (*,*) 'DX: ', DX
!      WRITE (*,*) 'NMAX: ', NMAX
!      WRITE (*,*) 'EPS: ', EPS
!      WRITE (*,*) 'TIME: ', TIMEIndex

    
    xf = x
    val = 0.0_dp
    success = .FALSE.
    
    DO i = 1,NetDim
      
      ! Find the (i,j) indices [1,...,max] 
      ! Calculates the normalized difference vector; 
      ! i.e. the distance/indices to a point x from the leftmost points of the FD box.
      !-------------------------------------------------------------------------------
      ind(i) = CEILING( ( xf(i) - x0(i) ) / dx(i) ) 
      
!      PRINT *,'Ind(i): ', i,ind(i),Xf(i),X0(i),Dx(i) 
      
      ! Checks that the estimated index is within the bounding box
      IF( ind(i) < 1 .OR. ind(i) >= DimSize(i) ) THEN
        
        ! If it's smaller than the leftmost index, but within tolerance (Eps), set it to lower bound; and vice versa
        IF( xf(i) <= x0(i) .AND. xf(i) >= x0(i) - Eps(i) ) THEN
          ind(i) = 1
        ELSE IF( xf(i) >= x1(i) .AND. xf(i) <= x1(i) + Eps(i) ) THEN
          ind(i) = DimSize(i) - 1
        ELSE ! The index is too far to be salvaged            
          WRITE (Message, '(A,I0,A,I0,A,F14.3,A)') 'ind(',i,') = ', ind(i), ' from Elmer coordinate ',&
              Xf(i), ' Not in bounding box'
          PRINT *,'Boundint box:',x0(i),x1(i)
          CALL Warn( 'GridDataReader',Message)
          RETURN
        END IF
      END IF
    END DO
    
    ! The value of the estimated NetCDF grid point
    xi(1:NetDim) = x0(1:NetDim) + (ind(1:NetDim)-1) * dx(1:NetDim)
    
    ! Interpolation weights, which are the normalized differences of the estimation 
    ! from lower left corner values. 
    !------------------------------------------------------------------------------
    weights(1:NetDim) = (xf(1:NetDim)-xi(1:NetDim)) / dx(1:NetDim)
    
    IF( NetDim == 2 ) THEN
      CALL NetCDFDataCell2D( stencil, NetDim, Ind, TimeIndex  )     
      val = BiLinearInterpolation(stencil(:,:,1),weights)
    ELSE
      CALL NetCDFDataCell3D( stencil, NetDim, Ind, TimeIndex  )           
      val = TriLinearInterpolation(stencil,weights)
    END IF
    
    success = .TRUE.
    
  END FUNCTION FDInterpolation
  

  !------------------------------------------------------------------------------------
  ! Transforms Elmer input coordinates into the given coordinate system of the netCDF file.
  !------------------------------------------------------------------------------------
  FUNCTION CoordinateTransformation( vec0, CoordSystem ) RESULT( vec1 )
    !--------------------------------------------------------------
    USE DefUtils, ONLY: dp
    USE Messages
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: CoordSystem  ! Some coordinate transformation
    REAL(KIND=dp), INTENT(IN) :: vec0(3)     ! input coordinates
    REAL(KIND=dp) :: vec1(3)                 ! output coordinates
    REAL(KIND=dp) :: vec2(3)  
    REAL(KIND=dp), PARAMETER :: RAD_TO_DEG = 180.0 / PI
    REAL(KIND=dp), PARAMETER :: DEG_TO_RAD = PI / 180.0
    INTEGER :: i
    
    SELECT CASE ( CoordSystem )
      
    CASE ('lat-long')
      CALL Warn('GridDataReader','Applies latitude-longitude coordinate transformation; TODO!')
      vec1 = vec0
      
    CASE ('cylindrical')
      vec1(1) = SQRT( vec0(1)**2 + vec0(2)**2 )
      vec1(2) = RAD_TO_DEG * ATAN2( vec0(2), vec0(1) )
      vec1(3) = vec0(3)
      
    CASE ('none')
      vec1 = vec0

    CASE DEFAULT
      WRITE (Message,'(A)') 'No coordinate transformation applied: Unknown > Coordinate system < :"'// &
          TRIM(CoordSystem) 
      CALL Warn('GridDataReader', Message)
      vec1 = vec0
    END SELECT

  END FUNCTION CoordinateTransformation

    
!------------------------------------------------------------------------------
END SUBROUTINE GridDataReader
!------------------------------------------------------------------------------

