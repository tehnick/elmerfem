!------------------------------------------------------------------------------
! Peter Råback, Vili Forsell
! Created: 7.6.2011
! Last Modified: 30.6.2011
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

  USE DefUtils, ONLY: dp, Solver_t, Model_t, Mesh_t,GetInteger, CoordinateSystemDimension, GetSolverParams, &
                      GetLogical, MAX_NAME_LEN
  USE Messages, ONLY: Info, Warn, Message
!  USE MeshUtils
!  USE ElementUtils
  USE NetCDF
  USE NetCDFGridUtils, ONLY: UniformGrid_t
  USE NetCDFInterpolate, ONLY: Interpolate, LinearInterpolation
  USE NetCDFGeneralUtils, ONLY: CloseNetCDF, TimeType_t
  USE CustomTimeInterpolation, ONLY: ChooseTimeInterpolation

  IMPLICIT NONE

  !------------------------------------------------------------------------------
  LOGICAL, PARAMETER :: DEBUG = .FALSE. ! Shows the basic debug info on grids and dimensions
  LOGICAL, PARAMETER :: DEBUG_MORE = .FALSE. ! Shows also debug printouts for each iteration
  !------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
  !------------------------------------------------------------------------------
  ! Local variables
  !------------------------------------------------------------------------------

  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(TimeType_t) :: Time
  TYPE(UniformGrid_t) :: Grids(2)
  INTEGER :: k, node, DIM, MAX_STEPS
  INTEGER, POINTER :: FieldPerm(:)
  REAL(KIND=dp), POINTER :: Field(:)
  REAL(KIND=dp) :: x(2),u1(2),u2(2),interp_val,interp_val2,x0e(2),x1e(2)
  INTEGER, ALLOCATABLE :: dim_ids(:), dim_lens(:) ! Ids and lengths for all dimensions
  INTEGER :: NCID, loop, Var_ID
  CHARACTER (len = MAX_NAME_LEN) :: Coord_System, TimeInterpolationMethod
  REAL(KIND=dp) :: InterpMultiplier, InterpBias
  LOGICAL :: output, tmpBool, ENABLE_SCALING

  !------------------------------------------------------------------------------
  ! General initializations
  !------------------------------------------------------------------------------

  CALL Info('GridDataMapper','-----------------------------------------', Level=4 )
  CALL Info('GridDataMapper','Getting field from grid data',Level=4) 

  Time % val = -1.0_dp
  Time % id = -1
  Time % len = -1
  Time % low = -1
  Time % high = -1
  Time % doInterpolation = .FALSE.
  Time % is_defined = .FALSE.

!  WRITE (*,*) 'val ', Time % val, ' id ', Time % id, ' len ', Time % len,&
! ' low ', Time % low, ' high ', Time % high, ' interp ', Time % doInterpolation

  IF ( TransientSimulation ) THEN
    MAX_STEPS = GetInteger( Model % Simulation, 'TimeStep Intervals' )
  ELSE
    MAX_STEPS = 1 ! Steady state
  END IF

  !-- Pointer declarations
  Mesh => Solver % Mesh
  Field => Solver % Variable % Values  ! This vector will get the field values now
  FieldPerm => Solver % Variable % Perm
  DIM = CoordinateSystemDimension()

  CALL InitNetCDF(Solver, NCID, Var_ID, dim_ids, dim_lens, Grids, Time, TransientSimulation, dt, MAX_STEPS, Coord_System)
  IF (DIM .NE. size(dim_lens,1)) THEN
    CALL Warn('GridDataMapper','NetCDF dimensions do not match the Elmer dimensions')
  END IF

  DO loop = 1,size(Grids,1),1
    Grids(loop) % x1(:) = Grids(loop) % x0(:) +&
     (Grids(loop) % nmax(:)-1) * Grids(loop) % dx(:) ! In 3D case opposite points of the cube; if only one dimension, will be 0
  END DO

  !--- Collects the range of the Elmer mesh bounding box for scaling
  x0e(1) = MINVAL(Mesh % Nodes % x)
  x0e(2) = MINVAL(Mesh % Nodes % y)
  x1e(1) = MAXVAL(Mesh % Nodes % x)
  x1e(2) = MAXVAL(Mesh % Nodes % y)


  !--- Calculates the modifications (by default does nothing)
  tmpBool = .FALSE.
  ENABLE_SCALING = GetLogical(GetSolverParams(Solver), "Enable Scaling", tmpBool)
  IF ( tmpBool .AND. ENABLE_SCALING ) THEN
    CALL Warn('GridDataMapper','Elmer grid is scaled to match the NetCDF grid')

    DO loop = 1,size(Grids,1),1
      ! First the scaling to same size (Eq. a( X1E(1)-X0E(1) ) = (X1(1)-X0(1)) ; ranges over a dimension are same. Solved for a, 1 if equal)
      Grids(loop) % scale(:) = (Grids(loop) % X1(:) - Grids(loop) % X0(:))/(X1E(:)-X0E(:)) ! Note: "/" and "*" elementwise operations for arrays in Fortran
      ! Second the vector to reach X0 from the scaled X0E (wherever it is)
      Grids(loop) % move(:) = Grids(loop) % X0(:) - Grids(loop) % scale(:)*X0E(:) ! zero, if equal
    END DO
  END IF

  !------ Debug printouts -------------------------

  IF (DEBUG) THEN
    PRINT *,'Initial Elmer Grid Bounding Box:'
    PRINT *,'X:',MINVAL(Mesh % Nodes % x), MAXVAL( Mesh % Nodes % x )
    PRINT *,'Y:',MINVAL(Mesh % Nodes % y), MAXVAL( Mesh % Nodes % y )

    DO loop = 1,size(Grids,1),1
      CALL PrintGrid(Grids(loop),loop)
    END DO    
  END IF
  !------------------------------------------------

  !--- Initializes the interpolation variables
  CALL InitInterpolation( Solver, Grids, TimeInterpolationMethod, InterpMultiplier, InterpBias )

  !------------------------------------------------------------------------------
  ! INTERPOLATION LOOP
  !------------------------------------------------------------------------------

  IF ( Time % doInterpolation ) THEN
    WRITE (Message,'(A,F5.2,A)') 'Given time value ', Time % val , ' is not an integer. Using time interpolation.'
    CALL Info('GridDataMapper',Message)
  ELSE
    WRITE (Message,'(A,F5.1,A)') 'Given time value ', Time % val, ' is an integer. No time interpolation used.'
    CALL Info('GridDataMapper',Message)
  END IF

  output = .TRUE. ! If true, outputs some iteration information

  ! Go through the active nodes and perform interpolation
  DO node=1, Mesh % NumberOfNodes
    k = FieldPerm(node) 
    IF( k == 0 ) CYCLE
    
    ! The point of interest
    x(1) = Mesh % Nodes % x(node)
    x(2) = Mesh % Nodes % y(node)
    ! NOTE: Collect also other dimension values here, if those are wanted later on

    IF ( Time % doInterpolation ) THEN ! Two time values
      IF ( .NOT. (Interpolate(Solver,NCID,x,Var_ID,dim_lens, Grids(1),& 
          Time, Time % low,interp_val, Coord_System) .AND. Interpolate(Solver,NCID,x,&
            Var_ID,dim_lens, Grids(2),Time,Time % high,interp_val2, Coord_System)) ) THEN
        CYCLE
      ELSE
        ! Time interpolation on already interpolated space values; save result in interp_val, use original time to weigh
        u1(1) = Time % low
        u1(2) = interp_val
        u2(1) = Time % high
        u2(2) = interp_val2
        interp_val = ChooseTimeInterpolation(Time % val,u1,u2,TimeInterpolationMethod,output) ! Chooses the time interpolation method
        ! See: CustomTimeInterpolation.f90
        output = .FALSE.
      END IF
    ELSE
      IF (.NOT. Interpolate(Solver,NCID,x,Var_ID,dim_lens,Grids(1),&
                Time,Time % low,interp_val, Coord_System) ) THEN
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
  SUBROUTINE InitInterpolation( Solver, Grids, TimeInterpolationMethod, InterpMultiplier, InterpBias )
    USE DefUtils, ONLY: GetSolverParams, MAX_NAME_LEN, GetConstReal, GetString, GetCReal
    USE Messages
    IMPLICIT NONE

    !--- Parameters
    TYPE(Solver_t), INTENT(IN) :: Solver
    TYPE(UniformGrid_t), INTENT(INOUT) :: Grids(:)
    REAL(KIND=dp), INTENT(OUT) :: InterpMultiplier, InterpBias
    CHARACTER(len=MAX_NAME_LEN), INTENT(OUT) :: TimeInterpolationMethod

!    !--- Other variables
    LOGICAL :: tmpBool
    REAL(KIND=dp) :: eps(2)

    eps = 0
    ! Epsilons are the relative tolerances for the amount 
    ! the Elmer grid point misses the bounds of the NetCDF bounding box
    eps(1) = GetConstReal(GetSolverParams(Solver), "Epsilon X", tmpBool)
    IF ( .NOT. tmpBool ) THEN
       eps(1) = 0
       CALL Warn('GridDataMapper', 'Variable "Epsilon X" not given in Solver Input File. &
 Using zero tolerance for NetCDF grid mismatches with Elmer mesh.')
    END IF
    eps(2) = GetConstReal(GetSolverParams(Solver), "Epsilon Y", tmpBool)
    IF ( .NOT. tmpBool ) THEN
       eps(2) = 0
       CALL Warn('GridDataMapper', 'Variable "Epsilon Y" not given in Solver Input File. &
 Using zero tolerance for NetCDF grid mismatches with Elmer mesh.')
    END IF

    Grids(1) % Eps(:) = eps(:) * Grids(1) % dx(:)
    Grids(2) % Eps(:) = eps(:) * Grids(2) % dx(:)
  
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
  SUBROUTINE InitTime( Solver, NCID, T_Name, IS_TRANSIENT, STEP_SIZE, MAX_STEPS, TimeResult )
  !----------------------------------------------------
    USE NetCDFGeneralUtils, ONLY: TimeValueToIndex, GetDimension
    USE DefUtils, ONLY: MAX_NAME_LEN, GetSolverParams, GetLogical, GetCReal, GetTime
    USE Messages, ONLY: Message, Info, Fatal
    IMPLICIT NONE
    !--- Input
    TYPE(Solver_t), INTENT(IN) :: Solver
    INTEGER, INTENT(IN) :: NCID
    LOGICAL, INTENT(IN) :: IS_TRANSIENT
    CHARACTER(len=MAX_NAME_LEN), INTENT(IN) :: T_Name
    REAL(KIND=dp), INTENT(IN) :: STEP_SIZE
    INTEGER, INTENT(IN) :: MAX_STEPS

    !--- Output
    TYPE(TimeType_t), INTENT(OUT) :: TimeResult

    !--- Others
    LOGICAL :: IsTimeIndex, IsUserDefined, Found(4) ! True if SIF definitions found
    REAL(KIND=dp) :: TimeBias, Time ! Biasing for every used Elmer time value/index

    CALL GetDimension(NCID,T_Name, TimeResult % id, TimeResult % len)
!    WRITE(*,*) 'T id ',TimeResult % id,' T len ', TimeResult % len

    IsUserDefined = GetLogical( GetSolverParams(Solver), "User Defines Time", Found(1) ) ! Set to true, if old definitions are used
    IF ( .NOT. Found(1) ) IsUserDefined = .FALSE.
    IsTimeIndex = GetLogical( GetSolverParams(Solver), "Is Time Index", Found(2) ) ! If true, then the given time value is an index (Default: value)
    IF ( .NOT. Found(2) ) IsTimeIndex = .FALSE. ! Defaulted to False for values are more natural, otherwise set as given
    TimeBias = GetCReal( GetSolverParams(Solver), "NetCDF Starting Time", Found(3) ) ! Index, if IsTimeIndex is true; value otherwise

!    WRITE(*,*) 'User def ', IsUserDefined, ' is index ',  IsTimeIndex, ' biased ', TimeBias, ' found ones ', Found

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
        IF ( TimeBias .LT. 1.0_dp .OR. TimeBias .GT. TimeResult % LEN ) THEN
          WRITE (Message, '(A,F6.2,A,I5,A)') 'NetCDF Starting Time index ', TimeBias, &
                              ' does not fit within the NetCDF time index range (1,', TimeResult % LEN,')'
          CALL Fatal('GridDataMapper', Message)
        END IF

        Time = Time + (TimeBias - STEP_SIZE)

        !--- Gives a fatal error for having set too many iterations
        IF (MAX_STEPS .GT. ((TimeResult % LEN - (TimeBias - STEP_SIZE))/STEP_SIZE) ) THEN
          WRITE (Message, '(A,I5,A,F10.2,A,F6.2,A)') 'Defined amount of timestep intervals ', MAX_STEPS, &
                          ' is more than ', ((TimeResult % LEN - (TimeBias - STEP_SIZE))/STEP_SIZE),&
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
    IF (.NOT. IsTimeIndex) Time = TimeValueToIndex(NCID,T_Name,TimeResult % id,TimeResult % LEN,Time)

    !--- Final check before letting through
    IF ( Time .LT. 1 .OR. Time .GT. TimeResult % LEN ) THEN
      WRITE (Message, '(A,F6.2,A,I5,A)') 'Time value ', Time, ' is out of range (1,', TimeResult % LEN, ')'
      CALL Fatal('GridDataMapper',Message)
    END IF

    !--- Sets the rest of the values to the result
    TimeResult % val = Time
    TimeResult % low = FLOOR( Time )
    TimeResult % high = CEILING( Time )
    TimeResult % doInterpolation = (TimeResult % low .NE. TimeResult % high)

  END SUBROUTINE InitTime

  !----------------- InitNetCDF() ---------------------
  !--- Gathers and initializes all the necessary NetCDF information for picking variables
  SUBROUTINE InitNetCDF(Solver, NCID, Var_ID, dim_ids, dim_lens, Grids, Time,&
                                   IS_TRANSIENT, STEP_SIZE, MAX_STEPS, Coord_System )
  !--------------------------------------------------

    USE NetCDFGridUtils, ONLY: PrintGrid, InitGrid, GetNetCDFGridParameters, Focus2DNetCDFGrid 
    USE NetCDFGeneralUtils, ONLY: GetAllDimensions, G_Error, TimeValueToIndex
    USE NetCDF
    USE DefUtils, ONLY: dp, MAX_NAME_LEN, GetSolverParams, GetString, GetConstReal
    USE Messages, ONLY: Fatal, Message
    IMPLICIT NONE
    TYPE(Solver_t), INTENT(IN) :: Solver
    TYPE(TimeType_t), INTENT(INOUT) :: Time
    TYPE(UniformGrid_t), INTENT(INOUT) :: Grids(:)
    LOGICAL, INTENT(IN) :: IS_TRANSIENT ! For using Elmer time instead of a variable
    REAL(KIND=dp), INTENT(IN) :: STEP_SIZE
    INTEGER, INTENT(IN) :: MAX_STEPS
    INTEGER, INTENT(OUT) :: NCID, Var_ID  !  NCID is the ID of the opened file, Var_ID the accessed variable id
    INTEGER, INTENT(INOUT), ALLOCATABLE :: dim_ids(:), dim_lens(:) ! Ids and lengths for all dimensions
    CHARACTER (len = MAX_NAME_LEN), INTENT(OUT) :: Coord_System

    !------------------------------------------------------------------------------
    ! NetCDF variables
    !------------------------------------------------------------------------------
    
    LOGICAL :: Found(8) ! True if SIF definitions found
    CHARACTER (len = MAX_NAME_LEN) :: FileName ! File name for reading the data (of .nc format)
    CHARACTER (len = MAX_NAME_LEN) :: Var_Name, X_Name, Y_Name, Z_Name, T_Name, Mask_Name
    CHARACTER (len = MAX_NAME_LEN), ALLOCATABLE :: Dim_Names(:)
    REAL(KIND=dp) :: Mask_Limit
    INTEGER :: loop, alloc_stat, dim_count, status ! Status tells whether operations succeed
    LOGICAL :: IsTimeDependent
  
    !------------------------------------------------------------------------------
    ! NetCDF initializations
    !------------------------------------------------------------------------------
!    WRITE (*,*) 'NetCDF INIT' 
 
    dim_count = 1 ! Minimum of one dimension required for anything

    !------- Collects the input information from Solver Input File
    FileName = GetString( GetSolverParams(Solver), "File Name", Found(1) )
    Var_Name = GetString( GetSolverParams(Solver), "Var Name", Found(2) )
    X_Name = GetString( GetSolverParams(Solver), "Coordinate 1 Name", Found(3) )
    Y_Name = GetString( GetSolverParams(Solver), "Coordinate 2 Name", Found(4) )
    Z_Name = GetString( GetSolverParams(Solver), "Coordinate 3 Name", Found(5) ) ! TODO: Working z dimension


!    WRITE(*,*) 'count ', dim_count, 'file ', FileName, ' var ', Var_Name, ' x ', X_Name, ' y ', Y_Name, ' z ', Z_Name

    !------- Time needs to be given name to be defined
    T_Name = GetString( GetSolverParams(Solver), "Time Name", IsTimeDependent ) ! If given, time is the last dimension
  
    ! Following parameters are needed for masking and usual processing
    Mask_Name = GetString( GetSolverParams(Solver), "Mask Variable", Found(6) )
    Mask_Limit = GetConstReal( Solver % Values, "Mask Limit", Found(7) )
    Coord_System = GetString( GetSolverParams(Solver), "Coordinate System", Found(8) ) ! Any input is ok; only valid gives a conversion and error is given otherwise

!    WRITE(*,*) 't ',T_Name,' found ', IsTimeDependent,' m name ', Mask_Name, &
!' m limit ', Mask_Limit, ' coords ', Coord_System, ' found ', Found

    IF ( .NOT. Found(8) ) Coord_System = ''
 
    DO loop = 1,3,1
      IF ( .NOT. Found(loop) ) THEN ! Checks that the constants have been found successfully
        CALL Fatal('GridDataMapper', &
      "Unable to find a compulsory NetCDF Name Constant (the name of file, variable or first coordinate)")
      END IF
    END DO
 
    !--- Requires that last found coordinate name limits the input size 
    IF ( Found(5) .AND. (.NOT. Found(4)) ) THEN
      CALL Fatal('GridDataMapper', 'Coordinate 3 was defined without Coordinate 2.')
    END IF

    ! Counts the amount of dimensions
    IF ( Found(4) ) dim_count = 2
    IF ( Found(5) ) dim_count = 3

!    WRITE(*,*) ' dim count finished ', dim_count

    ! Inform about the fate of time
    IF ( IsTimeDependent ) THEN
      CALL Info('GridDataMapper','Time dimension taken into account.')
    END IF

    ! Opening the NetCDF file  
    status = NF90_OPEN(FileName,NF90_NOWRITE,NCID) ! Read-only
    IF ( G_Error(status, "NetCDF file could not be opened") ) THEN
      CALL abort() ! End execution
    END IF
 
    ! Form an array of wanted names for defining the information 
    ALLOCATE (Dim_Names(dim_count), dim_ids(dim_count), dim_lens(dim_count), STAT = alloc_stat)
    IF ( alloc_stat .NE. 0 ) THEN
      CALL Fatal('GridDataMapper','Memory ran out')
    END IF

    Dim_Names = 'none'
    dim_ids = -1
    dim_lens = -1

!    WRITE(*,*) 'Allocations: names ', size(Dim_Names), ' ids ', size(dim_ids),' lens ', size(dim_lens)

    WRITE (Message,'(A,I3,A)') 'Using ',dim_count,' input dimensions.'
    CALL Info('GridDataMapper',Message)

!    WRITE(*,*) 'Inits: names ', Dim_Names, ' ids ', dim_ids, ' lens ', dim_lens

    ! For now, just an opened little loop
    Dim_Names(1) = X_Name
    loop = dim_count - 1
    IF (loop .GT. 0) THEN
      Dim_Names(2) = Y_Name
      loop = loop - 1
    END IF
    IF (loop .GT. 0) THEN
      Dim_Names(3) = Z_Name
      loop = loop - 1
    END IF

!    WRITE(*,*) 'Names: ', Dim_Names, ' loop ', loop
    
!    WRITE(*,*) 'Grid inits'

    DO loop = 1,size(Grids,1),1
!      WRITE (*,*) '('
      CALL InitGrid(Grids(loop), size(Dim_Names,1))
!      CALL PrintGrid(Grids(loop),loop)
!      WRITE (*,*) ')'
    END DO

!    WRITE(*,*) 'Getting dims'
 
    CALL GetAllDimensions(NCID,Dim_Names,dim_ids,dim_lens) ! Gets dimensions on basis of the given names
 
    ! Find variable to be accessed
    status = NF90_INQ_VARID(NCID,Var_Name,Var_ID)
    IF ( G_Error(status,'NetCDF variable name not found.') ) THEN
      CALL abort()
    END IF

!    WRITE(*,*) 'Finished dims'

    !----------- Get the definining parameters for the NetCDF grid
    ! x0 vector: lower left corner 
    ! dx vector: grid spacings
    ! nmax vector: amounts of steps
    CALL GetNetCDFGridParameters(NCID,Grids(1),dim_ids,dim_lens) ! Normal grid parameters don't depend on time

!    WRITE(*,*) 'B'
    ! Initializes time if it is defined
    IF (IsTimeDependent) THEN

!      WRITE(*,*) 'C'
      Time % is_defined = .TRUE.
      CALL InitTime( Solver, NCID, T_Name, IS_TRANSIENT, STEP_SIZE, MAX_STEPS, Time )

      ! If there'll be interpolation, initialize both of the grids usable
      IF ( Time % doInterpolation ) THEN
        Grids(2) % x0(:) = Grids(1) % x0(:)
        Grids(2) % dx(:) = Grids(1) % dx(:)
        Grids(2) % nmax(:) = Grids(1) % nmax(:)
      END IF

      IF ( .NOT. Found(5) ) THEN
        IF ( Found(6) .AND. Found(7) ) THEN
          CALL Info('GridDataMapper','Two dimensional NetCDF grid focusing on basis of the given mask is in effect.')
          CALL Focus2DNetCDFGrid(NCID,Mask_Name,Mask_Limit,Grids(1),Time % low,dim_lens)
          IF ( Time % doInterpolation ) THEN ! Need to interpolate, return two different grid parameters
            
            Grids(2) % x0(1:Grids(2) % DIMS) = Grids(1) % x0(1:Grids(1) % DIMS) ! Copy the same results obtained for all grids
            Grids(2) % dx(1:Grids(2) % DIMS) = Grids(1) % dx(1:Grids(1) % DIMS)
            Grids(2) % nmax(1:Grids(2) % DIMS) = Grids(1) % nmax(1:Grids(1) % DIMS)
            CALL Focus2DNetCDFGrid(NCID,Mask_Name,Mask_Limit,Grids(2),Time % high,dim_lens)
          END IF
        ELSE
          ! No mask used
        END IF
      END IF
    END IF

!    WRITE(*,*) 'D'
  END SUBROUTINE InitNetCDF


!------------------------------------------------------------------------------
END SUBROUTINE GridDataMapper
!------------------------------------------------------------------------------

