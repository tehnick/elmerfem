!------------------------------------------------------------------------------
! Peter Råback, Vili Forsell
! Created: 7.6.2011
! Last Modified: 28.6.2011
!------------------------------------------------------------------------------
SUBROUTINE GridDataMapper( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  This subroutine saves scalar values to a matrix.
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

!******************************************************************************
! Notes:	o Performs a modification from measurements with uniform grid into Elmer with an unstructured grid
!		o Maintains precipitation energy during the transformation (TODO: formalize, implement, prove)
!		o Remember to see if incremental processing might be possible
!		o Strive for strong exception guarantee (TODO: Revisit this in the end)
!		o Some terminology: "nodes" and "edges" used for grid points and the lines connecting the for grid points and the lines connecting them
!		o Implement Z dimension in data extraction
!******************************************************************************

  USE DefUtils
  USE MeshUtils
  USE ElementUtils
  USE NetCDF
  USE NetCDFInterpolate, ONLY: Interpolate, LinearInterpolation
  USE NetCDFGeneralUtils, ONLY: CloseNetCDF
  USE CustomTimeInterpolation, ONLY: ChooseTimeInterpolation

  IMPLICIT NONE

  !------------------------------------------------------------------------------
  LOGICAL :: DEBUG = .FALSE. ! Shows the basic debug info on grids and dimensions
  LOGICAL :: DEBUG_MORE = .FALSE. ! Shows also debug printouts for each iteration
  !------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
  !------------------------------------------------------------------------------
  ! Local variables
  !------------------------------------------------------------------------------

  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(Solver_t), POINTER :: PSolver
  TYPE(ValueList_t), POINTER :: Params
  INTEGER :: k,node,nmax(2,2),dim, MAX_STEPS
  INTEGER, POINTER :: FieldPerm(:)
  REAL(KIND=dp), POINTER :: Field(:)
  REAL(KIND=dp) :: x(2),dx(2,2),x0(2,2),x1(2,2),u1(2),u2(2),&
                   interp_val,interp_val2,Eps(2,2),Time,x0e(2),x1e(2)
  
!  SAVE VisitedTimes ! Add VisitedTimes for optimization?

  INTEGER, ALLOCATABLE :: dim_ids(:), dim_lens(:) ! Ids and lengths for all dimensions
  INTEGER :: NCID, status, time_begin,time_end
  CHARACTER (len = MAX_NAME_LEN) :: Var_Name, Coord_System, TimeInterpolationMethod
  REAL(KIND=dp) :: InterpMultiplier, InterpBias, GridScales(2,2), GridMove(2,2)
  LOGICAL :: output, tmpBool, ENABLE_SCALING
!  VisitedTimes = VisitedTimes + 1
!  PRINT *,"Visited: ", VisitedTimes

  !------------------------------------------------------------------------------
  ! General initializations
  !------------------------------------------------------------------------------

  CALL Info('GridDataMapper','-----------------------------------------', Level=4 )
  CALL Info('GridDataMapper','Getting field from grid data',Level=4) 

  IF ( TransientSimulation ) THEN
    MAX_STEPS = GetInteger( Model % Simulation, 'TimeStep Intervals' )
  ELSE
    MAX_STEPS = 1 ! Steady state
  END IF

  !-- Pointer declarations
  PSolver => Solver
  Params => Solver % Values
  Mesh => Solver % Mesh
  Field => Solver % Variable % Values  ! This vector will get the field values now
  FieldPerm => Solver % Variable % Perm
  DIM = CoordinateSystemDimension()

  CALL InitNetCDF(Solver, NCID, Var_Name, dim_ids, dim_lens, x0, dx, nmax, Time, Coord_System, TransientSimulation, dt, MAX_STEPS )
  IF (DIM .NE. size(dim_lens,1)) THEN
    CALL Warn('GridDataMapper','NetCDF dimensions do not match the Elmer dimensions')
  END IF
  x1(:,1) = x0(:,1)+(nmax(:,1)-1)*dx(:,1) ! In 3D case opposite points of the cube
  x1(:,2) = x0(:,2)+(nmax(:,2)-1)*dx(:,2) ! If only one dimension, will be 0

  !--- Collects the range of the Elmer mesh bounding box for scaling
  x0e(1) = MINVAL(Mesh % Nodes % x)
  x0e(2) = MINVAL(Mesh % Nodes % y)
  x1e(1) = MAXVAL(Mesh % Nodes % x)
  x1e(2) = MAXVAL(Mesh % Nodes % y)


  !--- Calculates the modifications (by default does nothing)
  GridMove = 0
  GridScales = 1 ! With these initializations: 1*x(:) + 0 = x(:) ; i.e. doesn't modify
  tmpBool = .FALSE.
  ENABLE_SCALING = GetLogical(GetSolverParams(Solver), "Enable Scaling", tmpBool)
  IF ( tmpBool .AND. ENABLE_SCALING ) THEN
    CALL Warn('GridDataMapper','Elmer grid is scaled to match the NetCDF grid')
    ! First the scaling to same size (Eq. a( X1E(1)-X0E(1) ) = (X1(1)-X0(1)) ; ranges over a dimension are same. Solved for a, 1 if equal)
    GridScales(:,1) = (X1(:,1)-X0(:,1))/(X1E(:)-X0E(:)) ! Note: "/" and "*" elementwise operations for arrays in Fortran
    GridScales(:,2) = (X1(:,2)-X0(:,2))/(X1E(:)-X0E(:))

    ! Second the vector to reach X0 from the scaled X0E (wherever it is)
    GridMove(:,1) = X0(:,1) - GridScales(:,1)*X0E(:) ! zero, if equal
    GridMove(:,2) = X0(:,2) - GridScales(:,2)*X0E(:)
  END IF

  !------ Debug printouts -------------------------

  IF (DEBUG) THEN
    PRINT *,'Initial Elmer Grid Bounding Box:'
    PRINT *,'X:',MINVAL(Mesh % Nodes % x), MAXVAL( Mesh % Nodes % x )
    PRINT *,'Y:',MINVAL(Mesh % Nodes % y), MAXVAL( Mesh % Nodes % y )

    PRINT *,'NetCDF (Uniform) Grid Bounding Box 1:'
    PRINT *,'X:',x0(1,1),x1(1,1)
    PRINT *,'Y:',x0(2,1),x1(2,1)
    
    PRINT *,'NetCDF (Uniform) Grid Bounding Box 2 (All 0, if no interpolation):'
    PRINT *,'X:',x0(1,2),x1(1,2)
    PRINT *,'Y:',x0(2,2),x1(2,2)
  END IF
  !------------------------------------------------

  !--- Initializes the interpolation variables
  CALL InitInterpolation( Solver, Eps, TimeInterpolationMethod, InterpMultiplier, InterpBias )

  !------------------------------------------------------------------------------
  ! INTERPOLATION LOOP
  !------------------------------------------------------------------------------

  time_begin = FLOOR(Time)
  time_end = CEILING(Time)
  IF ( time_begin .NE. time_end ) THEN
    WRITE (Message,'(A,F5.2,A)') 'Given time value ', time , ' is not an integer. Using time interpolation.'
    CALL Info('GridDataMapper',Message)
  ELSE
    WRITE (Message,'(A,F5.1,A)') 'Given time value ', time, ' is an integer. No time interpolation used.'
    CALL Info('GridDataMapper',Message)
  END IF

!  Time = GetTimePoint()
!  WRITE (*,*) x0
!  WRITE (*,*) dx
!  WRITE (*,*) nmax
!  WRITE (*,*) x1
!  WRITE (*,*) Eps

  output = .TRUE. ! If true, outputs some iteration information

  ! Go through the active nodes and perform interpolation
  DO node=1, Mesh % NumberOfNodes
    k = FieldPerm(node) 
    IF( k == 0 ) CYCLE
    
    ! The point of interest
    x(1) = Mesh % Nodes % x(node)
    x(2) = Mesh % Nodes % y(node)
    ! NOTE: Collect also other dimension values here, if those are wanted later on

    IF ( FLOOR(Time) .NE. CEILING(Time) ) THEN ! Two time values
      IF ( .NOT. (Interpolate(Solver,NCID,x,Var_Name,dim_ids,dim_lens,x0(:,1),dx(:,1),nmax(:,1),x1(:,1),& 
            GridScales(:,1),GridMove(:,1),Eps(:,1),time_begin,interp_val, Coord_System) .AND. Interpolate(Solver,NCID,x,&
            Var_Name,dim_ids,dim_lens, x0(:,2),dx(:,2),nmax(:,2),x1(:,2),GridScales(:,2),GridMove(:,2),Eps(:,2),&
            time_end,interp_val2, Coord_System)) ) THEN
        CYCLE
      ELSE
        ! Time interpolation on already interpolated space values; save result in interp_val, use original time to weigh
        u1(1) = time_begin
        u1(2) = interp_val
        u2(1) = time_end
        u2(2) = interp_val2
        interp_val = ChooseTimeInterpolation(time,u1,u2,TimeInterpolationMethod,output) ! Chooses the time interpolation method
        ! See: CustomTimeInterpolation.f90
        output = .FALSE.
      END IF
    ELSE
      IF (.NOT. Interpolate(Solver,NCID,x,Var_Name,dim_ids,dim_lens,x0(:,1),dx(:,1),nmax(:,1),x1(:,1), &
                    GridScales(:,1),GridMove(:,1),Eps(:,1),time_begin,interp_val, Coord_System) ) THEN
        CYCLE ! Ignore values for incompatible interpolation
      END IF
    END IF

    !------ Debug printouts -------------------------
    IF (DEBUG_MORE) THEN
       PRINT *,'Interpolation result: ', interp_val
    END IF
    !------------------------------------------------

    Field(k) = InterpMultiplier*interp_val + InterpBias ! Doesn't modify the result by default
  END DO



  !------------------------------------------------------------------------------
  ! Close the NetCDF file
  !------------------------------------------------------------------------------
  CALL CloseNetCDF(NCID)

  !------------------------------------------------------------------------------

  CALL Info('GridDataMapper','All done',Level=4)
  CALL Info('GridDataMapper', '-----------------------------------------', Level=4 )

CONTAINS

  !---------------- InitInterpolation() ---------------
  !--- Initializes the information needed for interpolation
  SUBROUTINE InitInterpolation( Solver, Eps, TimeInterpolationMethod, InterpMultiplier, InterpBias )
    USE DefUtils
    USE Messages
    IMPLICIT NONE

    !--- Parameters
    TYPE(Solver_t), INTENT(IN) :: Solver
    REAL(KIND=dp), INTENT(INOUT) :: Eps(:,:), InterpMultiplier, InterpBias
    CHARACTER(len=MAX_NAME_LEN), INTENT(INOUT) :: TimeInterpolationMethod

    !--- Other variables
    LOGICAL :: tmpBool
    REAL(KIND=dp) :: eX, eY

    ! Epsilons are the relative tolerances for the amount 
    ! the Elmer grid point misses the bounds of the NetCDF bounding box
    Eps = 0
    eX = GetConstReal(Params, "Epsilon X", tmpBool)
    IF ( .NOT. tmpBool ) THEN
       eX = 0
       CALL Warn('GridDataMapper', 'Variable "Epsilon X" not given in Solver Input File. &
 Using zero tolerance for NetCDF grid mismatches with Elmer mesh.')
    END IF
    eY = GetConstReal(Params, "Epsilon Y", tmpBool)
    IF ( .NOT. tmpBool ) THEN
       eY = 0
       CALL Warn('GridDataMapper', 'Variable "Epsilon Y" not given in Solver Input File. &
 Using zero tolerance for NetCDF grid mismatches with Elmer mesh.')
    END IF
    Eps(1,1) = eX * dx(1,1)
    Eps(2,1) = eY * dx(2,1)
    Eps(1,2) = eX * dx(1,2)
    Eps(2,2) = eY * dx(2,2)
  
    !--- Chooses the time interpolation method 
    TimeInterpolationMethod = GetString( GetSolverParams(Solver), "Time Interpolation Method", tmpBool )
    IF ( .NOT. tmpBool ) THEN
      CALL Warn('GridDataMapper', 'SIF variable "Time Interpolation Method" not specified, using default settings.')
      TimeInterpolationMethod = ''
    ELSE
      WRITE (Message,'(A,A)' ) 'Received Time Interpolation Method SIF variable value: ', TimeInterpolationMethod
      CALL Info('GridDataMapper', Message)
    END IF
 
    !--- If absolute time is relative, the value of the time function is added to the interpolated location; otherwise it is multiplied with it
    InterpMultiplier = GetCReal( GetSolverParams(Solver), "Interpolation Multiplier", tmpBool ) ! Multiplies the final interpolation result by given number (relative time)
    IF ( .NOT. tmpBool ) InterpMultiplier = 1.0_dp ! Defaulted to 1, so it doesn't modify the result
  
    InterpBias = GetCReal( GetSolverParams(Solver), "Interpolation Bias", tmpBool ) ! Adds the bias to the final interpolation result (absolute time)
    IF ( .NOT. tmpBool ) InterpBias = 0.0_dp ! Defaulted to 0, so there is no bias on time
  
  END SUBROUTINE InitInterpolation
 
  !---------------- InitTime() ------------------------
  !--- Initializes the time values
  SUBROUTINE InitTime( Solver, T_Name, IS_TRANSIENT, STEP_SIZE, MAX_STEPS, DIM_LENS, time )
  !----------------------------------------------------
    USE NetCDFGeneralUtils, ONLY: TimeValueToIndex
    IMPLICIT NONE
    !--- Input
    TYPE(Solver_t), INTENT(IN) :: Solver
    LOGICAL, INTENT(IN) :: IS_TRANSIENT
    CHARACTER(len=MAX_NAME_LEN), INTENT(IN) :: T_Name
    REAL(KIND=dp), INTENT(IN) :: STEP_SIZE
    INTEGER, INTENT(IN) :: MAX_STEPS,DIM_LENS(:)

    !--- Output
    REAL(KIND=dp) :: Time

    !--- Others
    LOGICAL :: IsTimeIndex, IsUserDefined, Found(4) ! True if SIF definitions found
    REAL(KIND=dp) :: TimeBias ! Biasing for every used Elmer time value/index

    IsUserDefined = GetLogical( GetSolverParams(Solver), "User Defines Time", Found(1) ) ! Set to true, if old definitions are used
    IF ( .NOT. Found(1) ) IsUserDefined = .FALSE.
    IsTimeIndex = GetLogical( GetSolverParams(Solver), "Is Time Index", Found(2) ) ! If true, then the given time value is an index (Default: value)
    IF ( .NOT. Found(2) ) IsTimeIndex = .FALSE. ! Defaulted to False for values are more natural, otherwise set as given
    TimeBias = GetCReal( GetSolverParams(Solver), "NetCDF Starting Time", Found(3) ) ! Index, if IsTimeIndex is true; value otherwise

    !--- Set default values or inform the user of what he did
    IF ( .NOT. Found(3) ) THEN
      IF ( IsTimeIndex ) THEN
        TimeBias = 1.0_dp ! Starts, by default, from index 1
      ELSE
        TimeBias = 0.0_dp ! Starts, by default, without value adjustment
      END IF
    ELSE
      ! Tell the user what is happening
      IF ( IsTimeIndex ) THEN
        WRITE (Message,'(A,F6.2)') 'Input time indices are adjusted (summed) by ', TimeBias
      ELSE
        WRITE (Message,'(A,F6.2)') 'Input time values are adjusted (summed) by ', TimeBias
      END IF
      CALL Info('GridDataMapper',Message)
    END IF

    !--- In transient cases uses Elmer time if not defined otherwise
    IF ( IS_TRANSIENT .AND. (.NOT. IsUserDefined ) ) THEN
      Found(4) = .TRUE.
      WRITE(Message, '(A,A)') 'Simulation is transient and user time input is ignored.',&
      ' (If own time scaling wanted, set SIF variable "User Defines Time" true and use user defined functions)'
      CALL Info('GridDataMapper', Message)

      Time = GetTime() ! Get the time from Elmer

      !--- Bias the given time indices ; time values converted later on
      IF ( IsTimeIndex ) THEN  ! Indexing starts with step size in Elmer, bias it to first index
      
        !--- Check the starting time index is within the NetCDF time range
        IF ( TimeBias .LT. 1.0_dp .OR. TimeBias .GT. DIM_LENS(size(DIM_LENS)) ) THEN
          WRITE (Message, '(A,F6.2,A,I5,A)') 'NetCDF Starting Time index ', TimeBias, &
                              ' does not fit within the NetCDF time index range (1,',DIM_LENS(size(DIM_LENS)), ')'
          CALL Fatal('GridDataMapper', Message)
        END IF

        Time = Time + (TimeBias - STEP_SIZE)

        !--- Gives a fatal error for having set too many iterations
        IF (MAX_STEPS .GT. ((DIM_LENS(size(DIM_LENS)) - (TimeBias - STEP_SIZE))/STEP_SIZE) ) THEN
          WRITE (Message, '(A,I5,A,F10.2,A,F6.2,A)') 'Defined amount of timestep intervals ', MAX_STEPS, &
                          ' is more than ', ((DIM_LENS(size(DIM_LENS)) - (TimeBias - STEP_SIZE))/STEP_SIZE),&
                          ', which is the maximum number of allowed size ', STEP_SIZE ,' steps on the NetCDF grid.'
          CALL Fatal('GridDataMapper',Message)
        END IF
      ELSE
        Time = Time + TimeBias ! Biasing for time value
      END IF

    ELSE 
      !--- For user defined and steady state
      Time = GetCReal( Solver % Values, "Time Point", Found(3) )
    END IF

    IF ( .NOT. Found(4) ) THEN
      WRITE(Message,'(A,I3,A)') 'No time point given; specify it in the Solver Input File with name "Time Point"&
 or enable transient simulation!'
      CALL Fatal('GridDataMapper',Message)
    END IF

    !--- Converts the given time value into an appropriate time index
    IF (.NOT. IsTimeIndex) Time = TimeValueToIndex(NCID,T_Name,dim_ids,dim_lens,Time)

    !--- Final check before letting through
    IF ( Time .LT. 1 .OR. Time .GT. DIM_LENS(size(DIM_LENS)) ) THEN
      WRITE (Message, '(A,F6.2,A,I5,A)') 'Time value ', Time, ' is out of range (1,',DIM_LENS(size(DIM_LENS)), ')'
      CALL Fatal('GridDataMapper',Message)
    END IF
  END SUBROUTINE InitTime

  !----------------- InitNetCDF() ---------------------
  !--- Gathers and initializes all the necessary NetCDF information for picking variables
  SUBROUTINE InitNetCDF(Solver, NCID, var_name, dim_ids, dim_lens, x0, dx, nmax, time,&
                                   Coord_System, IS_TRANSIENT, STEP_SIZE, MAX_STEPS )
  !--------------------------------------------------

    USE NetCDFGridUtils, ONLY: GetNetCDFGridParameters, Focus2DNetCDFGrid 
    USE NetCDFGeneralUtils, ONLY: GetAllDimensions, G_Error, TimeValueToIndex
    USE NetCDF
    USE DefUtils
    IMPLICIT NONE
    TYPE(Solver_t), INTENT(IN) :: Solver
    LOGICAL, INTENT(IN) :: IS_TRANSIENT ! For using Elmer time instead of a variable
    REAL(KIND=dp), INTENT(IN) :: STEP_SIZE
    INTEGER, INTENT(IN) :: MAX_STEPS
  
    ! For x0,dx and nmax: first column for floored time point, second column for ceiled time point
    REAL(KIND=dp), INTENT(OUT) :: x0(2,2),dx(2,2) ! Coordinates for the NetCDF grid
    REAL(KIND=dp) :: Time, TimeMultiplier, TimeBias
    INTEGER, INTENT(OUT) :: NCID, nmax(2,2) !  NCID is the ID of the opened file; time the moment of time to be used
    INTEGER, INTENT(INOUT), ALLOCATABLE :: dim_ids(:), dim_lens(:) ! Ids and lengths for all dimensions

    !------------------------------------------------------------------------------
    ! NetCDF variables
    !------------------------------------------------------------------------------
    
    LOGICAL :: IsTimeValue, IsUserDefined, Found(13) ! True if SIF definitions found
    CHARACTER (len = MAX_NAME_LEN) :: FileName ! File name for reading the data (of .nc format)
    CHARACTER (len = MAX_NAME_LEN) :: X_Name, Y_Name, Z_Name, T_Name, Var_Name, Mask_Name, Coord_System
    REAL(KIND=dp) :: Mask_Limit
    INTEGER :: i, time_val, status ! Status tells whether operations succeed
  
    !------------------------------------------------------------------------------
    ! NetCDF initializations
    !------------------------------------------------------------------------------
  
    x0 = 0.0_dp
    dx = 0.0_dp
    nmax = 0

    !------- Collects the input information from Solver Input File
    FileName = GetString( GetSolverParams(Solver), "File Name", Found(1) )
    X_Name = GetString( GetSolverParams(Solver), "X Name", Found(2) )
    Y_Name = GetString( GetSolverParams(Solver), "Y Name", Found(3) )
    Z_Name = GetString( GetSolverParams(Solver), "Z Name", Found(4) ) ! TODO: Working z dimension
    T_Name = GetString( GetSolverParams(Solver), "Time Name", Found(5) )
  
    ! Following parameters are needed for masking and usual processing
    Var_Name = GetString( GetSolverParams(Solver), "Var Name", Found(6) )
    Mask_Name = GetString( GetSolverParams(Solver), "Mask Variable", Found(7) )
    Mask_Limit = GetConstReal( Solver % Values, "Mask Limit", Found(8) )
    Coord_System = GetString( GetSolverParams(Solver), "Coordinate System", Found(10) ) ! Any input is ok; only valid gives a conversion and error is given otherwise

    IF ( .NOT. Found(10) ) Coord_System = ''
 
    DO i = 1,6,1
      IF ( .NOT. Found(i) .AND. (i .NE. 4) ) THEN ! Checks that the constants have been found successfully
        CALL Fatal('GridDataMapper', &
      "Unable to find a compulsory NetCDF Name Constant (the name of file, x, y, time or variable)")
      END IF
    END DO
  
    ! Opening the NetCDF file  
    status = NF90_OPEN(FileName,NF90_NOWRITE,NCID) ! Read-only
    IF ( G_Error(status, "NetCDF file could not be opened") ) THEN
      CALL abort() ! End execution
    END IF
  
   
    IF ( .NOT. Found(4) ) THEN
      CALL Info('GridDataMapper', "Reading assumes data has only x,y and time dimensions")
      CALL GetAllDimensions(NCID,X_NAME = X_NAME,Y_NAME = Y_NAME,T_NAME = T_NAME, &
            dim_ids = dim_ids, dim_lens = dim_lens)
    ELSE 
      CALL Info('GridDataMapper', "Reading uses all dimensions")
      CALL GetAllDimensions(NCID,X_NAME = X_NAME,Y_NAME = Y_NAME,Z_NAME=Z_NAME, & 
            T_NAME = T_NAME,dim_ids = dim_ids, dim_lens = dim_lens)
    END IF

    CALL InitTime( Solver, T_Name, IS_TRANSIENT, STEP_SIZE, MAX_STEPS, DIM_LENS, time )

    !----------- Get the definining parameters for the NetCDF grid
    ! x0 vector: lower left corner 
    ! dx vector: grid spacings
    ! nmax vector: amounts of steps
    CALL GetNetCDFGridParameters(NCID,x0(:,1),dx(:,1),nmax(:,1),dim_ids,dim_lens) ! Normal grid parameters don't depend on time

    ! If there'll be interpolation, initialize both of the grids usable
    IF ( FLOOR(Time) .NE. CEILING(Time) ) THEN
      x0(:,2) = x0(:,1)
      dx(:,2) = dx(:,1)
      nmax(:,2) = nmax(:,1)
    END IF

    IF ( .NOT. Found(4) ) THEN
      IF ( Found(7) .AND. Found(8) ) THEN
        CALL Info('GridDataMapper','Two dimensional NetCDF grid focusing on basis of the given mask is in effect.')
        time_val = FLOOR(Time)
        CALL Focus2DNetCDFGrid(NCID,Mask_Name,Mask_Limit,x0(:,1),dx(:,1),nmax(:,1),time_val,dim_ids,dim_lens)
        IF ( FLOOR(Time) .NE. CEILING(Time) ) THEN ! Need to interpolate, return two different grid parameters
          x0(:,2) = x0(:,1) ! Copy the same results obtained for all grids
          dx(:,2) = dx(:,1)
          nmax(:,2) = nmax(:,1)
          time_val = CEILING(Time)
          CALL Focus2DNetCDFGrid(NCID,Mask_Name,Mask_Limit,x0(:,2),dx(:,2),nmax(:,2),time_val,dim_ids,dim_lens)
        END IF
      ELSE
        ! No mask used
      END IF
    END IF

  END SUBROUTINE InitNetCDF


!------------------------------------------------------------------------------
END SUBROUTINE GridDataMapper
!------------------------------------------------------------------------------

