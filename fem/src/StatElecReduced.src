!/******************************************************************************
! *
! *       ELMER, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 - , CSC - Scientific Computing Ltd.
! *                                    Finland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! *****************************************************************************/
!
! *****************************************************************************
! *
! *             Module Author: Peter Råback
! *
! *                   Address: CSC - Scenter for Scientific Computing
! *                            Tekniikantie 15a D
! *                            02101 Espoo, Finland
! *                            Tel. +358 0 457 2080
! *                    E-Mail: Peter.Raback@csc.fi
! *
! *                      Date: 04.06.2000
! *
! *               Modified by: Peter Råback
! *
! *      Date of modification: 31.5.2003
! *
! ****************************************************************************/


!------------------------------------------------------------------------------
SUBROUTINE StatElecReduced( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: StatElecReduced
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the Poisson equation for the electric potential and compute the 
!  electric field, flux, energy and capacitance
!  Applicable only to the 1D case.
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh, materials, BCs, etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  DOUBLE PRECISION :: dt,
!     INPUT: Timestep size for time dependent simulations
!
!  LOGICAL :: TransientSimulation
!     INPUT: Steady state or transient simulation
!
!******************************************************************************

  USE Types
  USE Lists
  USE Integration
  USE ElementDescription
  USE Differentials
  USE SolverUtils
  USE ElementUtils
  
  IMPLICIT NONE
!------------------------------------------------------------------------------
 
  TYPE(Model_t) :: Model
  TYPE(Solver_t), TARGET:: Solver
  REAL (KIND=DP) :: dt
  LOGICAL :: TransientSimulation
  
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(Variable_t), POINTER :: DisplaVar, AmplitudeVar, ApertureVar, AnyVar
  TYPE(Nodes_t)   :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement, Parent
  TYPE(Solver_t), POINTER :: PSolver 
  TYPE(ValueList_t), POINTER :: Material

  INTEGER, PARAMETER :: NoPots = 2
  INTEGER :: i,j,k,mat_id,mat_idold,n,t,istat,LocalNodes,pos,&
      NoPositions, NoAmplitudes, DIM, LumpingDegree, PotentialNo, &
      NoElements, tag, ElemDim, ElemDimOld=-1, ElemCorners, AplacElstatMode
  INTEGER, POINTER :: NodeIndexes(:), ForcePerm(:), &
      AmplitudePerm(:), AperturePerm(:)
  
  LOGICAL :: AllocationsDone = .FALSE., gotIt, PullIn, BiasOn, &
      ApertureExists, AmplitudeExists, ElasticCoupling, &
      SubroutineVisited=.FALSE., ScanPosition, LayerExists=.FALSE., &
      HoleCorrection, ComputeSpring, ComputeEnergy, ComputeField, &
      TwoPotentialsExist, FileAppend, SpringDerivatives, SymmetricCapacitor, &
      SideCorrection, ThicknessCorrection, stat, AplacExport=.FALSE.
  LOGICAL, ALLOCATABLE :: NodeComputed(:)

  REAL (KIND=DP) :: PermittivityOfVacuum, Zmax, Zmin, Zcritical, Zpos=0.0, &
      Zposold, ScanRangeMax, ScanRangeMin, ScanMaxAsymmetry, ScanLinear, &
      aa,bb,cc,dd,ee,at,st,CPUTime,s, Capacitance(NoPots), LumpedEnergy(NoPots), &
      LumpedSpring0, MaxPotential(NoPots), LumpedSpringDz, ElasticPos, &
      LumpedSpringDzDz, PullInVoltage, OldLumpedForce, ElasticSpring, &
      PullInScale=1.0, PullInScaleOld, PullInAccuracy, PullInRelative, Work, &
      OldMaxForce, ForceRatio, Alpha, Beta, Gamma, AplacData(10)

  REAL (KIND=DP), POINTER :: ForceVector(:), Field(:), Force(:), Spring(:), &
      Energy(:), Amplitude(:), Aperture(:), Array(:,:), ElemWork(:), &
      SideEnergy(:), SideSpring(:), SideForce(:)
 
  REAL (KIND=DP), ALLOCATABLE ::  Permittivity(:), &
      ElemAmplitude(:,:), ElemAperture(:), HoleSize(:), HoleFraction(:), &
      LayerPermittivity(:), Thickness(:), Density(:), &
      Width(:), LayerThickness(:), EffectiveAperture(:), MaxAmplitudes(:), &
      ElemEnergy(:), ElemForce(:), ElemSpring(:), ElemCharge(:), &
      PotentialDifference(:), LumpedForce(:,:), LumpedCharge(:,:), &
      LumpedSpring(:,:,:), LumpingFactors(:,:), Direction(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, Filename, FilenameNames, &
      HoleType, String1
  CHARACTER :: Too
  
  ! These variables are for efficient manipulation of local scalats data
  INTEGER, PARAMETER :: MaxNoValues = 100
  INTEGER :: NoValues
  REAL (KIND=dp) ::  Values(MaxNoValues) 
  CHARACTER(LEN=MAX_NAME_LEN) :: ValueNames(MaxNoValues), ValueUnits(MaxNoValues)
  LOGICAL :: ValueSaveLocal(MaxNoValues),ValueSaveRes(MaxNoValues)

  
  SAVE ElementNodes, AllocationsDone, Field, Force, Energy, Spring, &
      Permittivity, ElemAmplitude, ElemAperture, Aperture, Amplitude, &
      AmplitudeVar, AmplitudePerm, ApertureVar, AperturePerm, &
      SubroutineVisited, ElemWork, LayerPermittivity, &
      Thickness, Density, LayerThickness, HoleSize, HoleFraction, NoAmplitudes, &
      LumpedForce, LumpedSpring, EffectiveAperture, ElemCharge, &
      ElemEnergy, ElemForce, ElemSpring, MaxAmplitudes, LumpedCharge, &
      PotentialDifference, ComputeEnergy, ComputeSpring, ComputeField, &
      LumpingFactors, NodeComputed, Direction, Width, &
      SideEnergy, SideSpring, SideForce, PullInScale, Zpos, Zcritical

  CALL Info('StatElecReduced','----------------------------------',Level=5)
  CALL Info('StatElecReduced','1-Dimensional electrostatic solver',Level=5)
  CALL Info('StatElecReduced','----------------------------------',Level=5)


!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------
  Force     => Solver % Variable % Values
  ForcePerm => Solver % Variable % Perm
  LocalNodes = Model % NumberOfNodes
  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS

  NULLIFY(AmplitudeVar)
  AmplitudeVar => VariableGet( Model % Variables, 'Amplitude' )
  AmplitudeExists = ASSOCIATED(AmplitudeVar)

  IF(AmplitudeExists) THEN
    NoAmplitudes = AmplitudeVar % DOFs
    IF(NoAmplitudes < 1) NoAmplitudes = 1
    AmplitudePerm  => AmplitudeVar % Perm
    Amplitude => AmplitudeVar % Values
    AmplitudeExists = .TRUE.
  ELSE
    NoAmplitudes = 1
  ENDIF

  NULLIFY(ApertureVar)
  ApertureVar => VariableGet( Model % Variables, 'Aperture' )
  ApertureExists = ASSOCIATED (ApertureVar)
  IF(ApertureExists) THEN
    AperturePerm  => ApertureVar % Perm
    Aperture => ApertureVar % Values
  ENDIF

  ! Which types of corrections should be taken into account?
  HoleCorrection = ListGetLogical( Solver % Values, 'Hole Correction',gotIt )
  SideCorrection = ListGetLogical( Solver % Values, 'Side Correction',gotIt )
  ThicknessCorrection = ListGetLogical( Solver % Values, 'Thickness Correction',gotIt )

!---------------------------------------------------------------------------------------
! Elastic coupling adds the real amplitude of the displacement field into the apertures,
! Pull-in analysis finds a new amplitude so that the amplitude is defined by the 
! electrostatics alone, and BiadOn finds a new amplitude so that the electric 
! forces are the same as those of a linear spring.
!---------------------------------------------------------------------------------------

  ElasticCoupling = ListGetLogical( Solver % Values, 'Elastic Coupling',gotIt )
  BiasOn = ListGetLogical(Solver % Values, 'Bias Voltage',gotIt )
  PullIn = ListGetLogical(Solver % Values, 'Pull In Analysis',gotIt )

  NoPositions = 0
  IF(.NOT. (ElasticCoupling .OR. BiasOn .OR. PullIn)) THEN
    ScanPosition = ListGetLogical(Solver % Values, 'Scan Position',gotIt )
    IF(ScanPosition) THEN
      NoPositions = ListGetInteger( Solver % Values, 'Scan Points',gotIt,minv=2)
      IF(.NOT. gotIt) NoPositions = 20
      
      ScanRangeMin = ListGetConstReal( Solver % Values, 'Scan Range Min',gotIt )
      IF(.NOT. gotIt) ScanRangeMin = -0.5d0    
      
      ScanRangeMax = ListGetConstReal( Solver % Values, 'Scan Range Max',gotIt )
      IF(.NOT. gotIt) ScanRangeMax = 0.5d0
    END IF
  END IF

  EquationName = ListGetString( Solver % Values, 'Equation' )
  
  PermittivityOfVacuum = ListGetConstReal( Model % Constants, &
      'Permittivity Of Vacuum',gotIt )
  IF ( .NOT.gotIt ) PermittivityOfVacuum = 8.8542d-12
    
  SpringDerivatives = ListGetLogical( Solver % Values, 'Spring Derivatives',gotIt )

  AplacExport = .FALSE.
  DO k = 1, Model % NumberOfSolvers
    String1 = ListGetString( Model % Solvers(k) % Values, 'Equation',gotIt )
    IF(TRIM(String1) == 'aplac export') THEN
      AplacExport = .TRUE.
      AplacElstatMode = ListGetInteger( Model % Solvers(k) % Values, 'Electrostatics Mode',gotIt )
      IF(.NOT. GotIt) AplacElstatMode = 1
    END IF
  END DO
  IF(.NOT. AplacExport) THEN
    AplacExport = ListGetLogical( Model % Simulation, 'Aplac Export',gotIt)
  END IF

  LumpingDegree = ListGetInteger( Solver % Values,'Lumping Degree',GotIt)
  IF(.NOT. GotIt) LumpingDegree = 1
  IF(AplacExport) LumpingDegree = 3

  DIM = CoordinateSystemDimension()


!------------------------------------------------------------------------------
!    Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------

  IF ( .NOT. AllocationsDone ) THEN

    N = Model % MaxElementNodes
    
    ALLOCATE( ElementNodes % x(N),   &
        ElementNodes % y(N),   &
        ElementNodes % z(N),   &
        Permittivity(N),       &
        LayerPermittivity(N),       &
        Thickness(N),       &
        Density(N),       &
        LayerThickness(N),       &
        ElemCharge(N),       &
        ElemEnergy(N), &
        ElemForce(N), &
        ElemSpring(N), &
        Width(N), &
        EffectiveAperture(N), &
        MaxAmplitudes(NoAmplitudes), &
        ElemAmplitude(NoAmplitudes,N),       &
        LumpedForce(NoPots,NoAmplitudes), &
        LumpedCharge(NoPots,NoAmplitudes), &
        LumpedSpring(NoPots,NoAmplitudes,NoAmplitudes), &
        PotentialDifference(N), &
        ElemAperture(N),       &
        ElemWork(N),       &
        HoleFraction(N),         &
        HoleSize(N),         &
        Direction(N),       &
        NodeComputed(Model%NumberOfNodes), &
        LumpingFactors(LumpingDegree+1,LumpingDegree+1), &
        STAT=istat )
    
    IF ( istat /= 0 ) THEN
      CALL FATAL('StatElecReduced','Memory allocation error')
    END IF

!------------------------------------------------------------------------------
!      Add electric field to the variable list
!------------------------------------------------------------------------------
    PSolver => Solver
    
    NULLIFY(AnyVar)
    AnyVar => VariableGet( Model % Variables, 'ElectricField' )
    IF(ASSOCIATED (AnyVar)) THEN
      ComputeField = .TRUE.
      Field => AnyVar % Values
    ELSE
      ComputeField = ListGetLogical(Solver % Values, 'Calculate Electric Field',gotIt )
      IF ( ComputeField ) THEN
        ALLOCATE( Field( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'ElectricField', 1, Field, ForcePerm)
      END IF
    END IF

    NULLIFY(AnyVar)
    AnyVar => VariableGet( Model % Variables, 'ElectricEnergy' )
    IF(ASSOCIATED (AnyVar)) THEN
      ComputeEnergy = .TRUE.
      Energy => AnyVar % Values
    ELSE
      ComputeEnergy = ListGetLogical(Solver % Values, 'Calculate Electric Energy',gotIt )
      IF ( ComputeEnergy ) THEN
        ALLOCATE( Energy( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'ElectricEnergy', 1, Energy, ForcePerm)
      END IF
    END IF
    
    NULLIFY(AnyVar)
    AnyVar => VariableGet( Model % Variables, 'ElectricSpring' )
    IF(ASSOCIATED (AnyVar)) THEN
      ComputeSpring = .TRUE.
      Spring => AnyVar % Values
    ELSE
      ComputeSpring = ListGetLogical(Solver % Values, 'Calculate Electric Spring',gotIt )
      IF ( ComputeSpring ) THEN
        ALLOCATE( Spring( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'ElectricSpring', 1, Spring, ForcePerm)
      END IF
    END IF
    
    IF(SideCorrection) THEN
      NULLIFY(AnyVar)
      AnyVar => VariableGet( Model % Variables, 'SideElectricForce' )
      IF(ASSOCIATED (AnyVar)) THEN
        SideForce => AnyVar % Values
      ELSE 
        ALLOCATE( SideForce( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'SideElectricForce', 1, SideForce, ForcePerm)
      END IF

      NULLIFY(AnyVar)
      AnyVar => VariableGet( Model % Variables, 'SideElectricEnergy' )
      IF(ASSOCIATED (AnyVar)) THEN
        SideEnergy => AnyVar % Values
      ELSE IF(ComputeEnergy) THEN
        ALLOCATE( SideEnergy( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'SideElectricEnergy', 1, SideEnergy, ForcePerm)
      END IF

      NULLIFY(AnyVar)
      AnyVar => VariableGet( Model % Variables, 'SideElectricSpring' )
      IF(ASSOCIATED (AnyVar)) THEN
        SideSpring => AnyVar % Values
      ELSE IF(ComputeSpring) THEN
        ALLOCATE( SideSpring( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) CALL Fatal( 'StatElecSolve', 'Memory allocation error.' )     
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            PSolver, 'SideElectricSpring', 1, SideSpring, ForcePerm)
      END IF
    END IF

    AllocationsDone = .TRUE.

  END IF
!------------------------------------------------------------------------------
!    Do some additional initialization, and go for it
!------------------------------------------------------------------------------


  ElasticPos = ListGetConstReal( Model % Simulation,'res: Elastic Displacement',gotIt )

  IF(AmplitudeExists) THEN
    DO j=1,NoAmplitudes
      MaxAmplitudes(j) = MAXVAL(ABS(Amplitude(j::NoAmplitudes)))
    END DO
  ELSE
    MaxAmplitudes(1) = 0.0d0
  END IF

  IF(PullIn .OR. BiasOn) THEN
    PullInScaleOld = PullInScale
    OldMaxForce = MAXVAL(ABS(Force))
    OldLumpedForce = LumpedForce(1,1)+LumpedForce(2,1)
  END IF

  IF(BiasOn) THEN
    ElasticSpring = ListGetConstReal( Model % Simulation,  &
        'res: Elastic Spring',gotIt )
    IF(.NOT. GotIt) ElasticSpring = ListGetConstReal( Model % Simulation,  &
        'res: Elastic Spring 1',gotIt )
    IF(.NOT. GotIt) THEN
      IF (SubroutineVisited .AND. ABS(ElasticPos) > 1.0d-20) THEN
        ElasticSpring = OldLumpedForce / ElasticPos        
      ELSE
        BiasOn = .FALSE.
      END IF
    END IF
  END IF

  IF(PullIn .OR. BiasOn) THEN
    PullInAccuracy = ListGetConstReal( &
        Solver % Values, 'Nonlinear System Convergence Tolerance',gotIt,0.0d0)
    IF(.NOT. GotIt) PullInAccuracy = 1.0d-4

    NoPositions = ListGetInteger( &
        Solver % Values, 'Nonlinear System Max Iterations',gotIt,minv=1)
    IF(.NOT. GotIt) NoPositions = 20
  END IF

  IF(ScanPosition .OR. PullIn .OR. BiasOn) THEN
    Filename = ListGetString(Solver % Values,'Filename',GotIt )
    IF(.NOT. gotIt) Filename = 'elstat.dat'

    FilenameNames = TRIM(Filename) // '.' // TRIM("names")    
    CALL Info('StatElecReduced','Saving results to '//TRIM(Filename),Level=5)

    FileAppend = ListGetLogical(Solver % Values,'File Append',GotIt)
    IF(.NOT. GotIt) FileAppend = .TRUE.
  END IF

  Thickness = 0.0d0
  mat_idold = -1
  Zmax = -1.0d0
  Zmin = 1.0d0

  DO pos=0,NoPositions

    IF(PullIn) THEN
      IF(pos > 0 .OR. SubroutineVisited) THEN
        Zposold = Zpos
        Zpos = Zpos/3.0d0 + (2.0d0/3.0d0)*(LumpedForce(1,1)+LumpedForce(2,1))/ &
            (LumpedSpring(1,1,1)+LumpedSpring(2,1,1))
        PullInRelative = ABS(Zpos/Zcritical)
      END IF
    ELSE IF(BiasOn) THEN
      Zposold = Zpos
      IF(pos > 0 .OR. SubroutineVisited) THEN
        Zpos = Zpos/3.0d0 + (2.0d0/3.0d0)*(LumpedForce(1,1)+LumpedForce(2,1)) / ElasticSpring
      END IF
    ELSE IF(ScanPosition) THEN
      IF(pos > 0) THEN
        ScanLinear = ScanRangeMin + &
            (ScanRangeMax-ScanRangeMin) * (pos-1.0d0)/ (NoPositions-1.0d0)
        Zpos = Zcritical * ScanLinear 
      END IF
    ELSE IF(ElasticCoupling .AND. .NOT. TransientSimulation) THEN
      Zpos = ElasticPos
    ELSE
      Zpos = 0.0d0
    END IF
    
    IF((PullIn .OR. BiasOn) .AND. (pos > 1)) THEN
      IF(PullInAccuracy*ABS(Zpos+Zposold) > 2.0d0*ABS(Zpos-Zposold)) EXIT 
    END IF

    Force = 0.0d0
    IF(ComputeField) Field = 0.0d0
    IF(ComputeEnergy) Energy = 0.0d0
    IF(ComputeSpring) Spring = 0.0d0
    TwoPotentialsExist = .FALSE.
    NodeComputed = .FALSE.

    LumpedEnergy = 0.0d0
    LumpedForce = 0.0d0
    LumpedCharge = 0.0d0
    LumpedSpring = 0.0d0
    LumpedSpringDz = 0.0d0
    LumpedSpringDzDz = 0.0d0
    LumpingFactors = 0.0d0
    MaxPotential = 0.0d0

    Alpha = 1.0d0
    Beta = 1.0d0
    Gamma = 1.0d0


    DO PotentialNo = 1,2

      ElemDim = 0
!------------------------------------------------------------------------------
!    Solve the 1D equation for electric field and energy density
!------------------------------------------------------------------------------
      DO t=1,Solver % Mesh % NumberOfBulkElements + Solver % Mesh % NumberOfBoundaryElements

!------------------------------------------------------------------------------

        CurrentElement => Solver % Mesh % Elements(t)
        ElemCorners = CurrentElement % TYPE % ElementCode / 100

        IF(ElemCorners > 4) THEN
          ElemDim = 3
        ELSE IF(ElemCorners > 2) THEN
          ElemDim = 2
        ELSE
          ElemDim = ElemCorners
        END IF

        IF(ElemDim == 3 .OR. ElemDim == 1) CYCLE

        ! Initialize the table showing the computed bulk and side nodes
        IF(ElemDimOld /= ElemDim) THEN
          NodeComputed = .FALSE.
          ElemDimOld = ElemDim
        END IF

        Model % CurrentElement => Solver % Mesh % Elements(t)

        IF(ElemDim == 2) THEN
          IF ( .NOT. CheckElementEquation( Model, &
              CurrentElement, EquationName ) ) CYCLE

          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
          
          mat_id = ListGetInteger( Model % Bodies(CurrentElement % BodyId) % &
              Values, 'Material', minv=1,maxv=Model % NumberOFMaterials )
        ELSE
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
          i = CurrentElement % BoundaryInfo % Constraint

          gotIt = .FALSE.

          DO k=1, Model % NumberOfBCs
            tag = Model % BCs(k) % Tag
            IF(tag /= i) CYCLE
            
            stat = ListGetLogical(Model % BCs(k) % Values,'Open Side',gotIt)
            IF(stat) EXIT
          END DO

          IF(.NOT. stat) CYCLE

          Parent => CurrentELement % BoundaryInfo % Left
          
          stat = ASSOCIATED( Parent )
          IF ( stat ) stat = stat .AND. ALL(ForcePerm(Parent % NodeIndexes) > 0)
          
          IF(.NOT. stat) THEN
            Parent => CurrentELement % BoundaryInfo % Right
            
            stat = ASSOCIATED( Parent )
            IF ( stat ) stat = stat .AND. ALL(ForcePerm(Parent % NodeIndexes) > 0)
            IF ( .NOT. stat )  CALL Fatal( 'StatElecReduced', &
                'No electrostatics available for specified boundary' )
          END IF
          
          mat_id = ListGetInteger( Model % Bodies(Parent % BodyId) % Values, &
              'Material', minv=1, maxv=Model % NumberOFMaterials )
        END IF
!------------------------------------------------------------------------------

        Material => Model % Materials(mat_id) % Values

        ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
        ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
        ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)

        IF(PotentialNo == 1) THEN
          PotentialDifference(1:n) = ListGetReal( Material, &
              'Potential Difference B',n,NodeIndexes,GotIt )
          IF(GotIt) TwoPotentialsExist = .TRUE.
          PotentialDifference(1:n) = ListGetReal( Material, &
              'Potential Difference',n,NodeIndexes,gotIt )
        ELSE
          PotentialDifference(1:n) = ListGetReal( Material, &
              'Potential Difference B',n,NodeIndexes,GotIt )
        END IF
          
        IF(.NOT. GotIt) CYCLE

        MaxPotential(PotentialNo) = &
            MAX(MaxPotential(PotentialNo),ABS(MAXVAL(PotentialDifference(1:n))))
        
        Permittivity(1:n) = ListGetReal( Model % Materials(mat_id) % Values, &
            'Permittivity',n,NodeIndexes,GotIt)
        IF(.NOT.GotIt) Permittivity = 1.0d0 
        Permittivity(1:n) = Permittivity(1:n) * PermittivityOfVacuum
        
        LayerThickness(1:n) = ListGetReal( Model % Materials(mat_id) % Values, &
            'Layer Thickness',n,NodeIndexes,LayerExists)
        
        IF(LayerExists) THEN
          LayerPermittivity(1:n) = ListGetReal( Model % Materials(mat_id) % Values, &
              'Layer Permittivity',n,NodeIndexes,GotIt)
          IF(.NOT.GotIt) LayerPermittivity = 1.0d0 
          LayerPermittivity(1:n) = LayerPermittivity(1:n) * PermittivityOfVacuum
        END IF
        
        IF(ElemDim == 2 .AND. HoleCorrection) THEN
          HoleType = ListGetString(Material,'Hole Type')
          HoleSize(1:n) = ListGetReal( Material, 'Hole Size', n, NodeIndexes)
          HoleFraction = ListGetReal( Model % Materials(mat_id) % Values, &
              'Hole Fraction',n,NodeIndexes)
        END IF


        IF(ElemDim == 1 .AND. SideCorrection) THEN
          SymmetricCapacitor = ListGetLogical( Model % BCs(k) % Values,&
              'Symmetric Side',gotIt)
          Width(1:n) = ListGetReal(Model % BCs(k) % Values,'Effective Width', &
              n, NodeIndexes)

          Thickness(1:n) = ListGetReal( Model % Materials(mat_id) % Values, &
              'Thickness',n,NodeIndexes,gotIt)

          IF(.NOT. GotIt) THEN
            Thickness(1:n) = ListGetReal(Model % BCs(k) % Values,'Thickness', &
                n, NodeIndexes, gotIt) 
          END IF

          IF(ThicknessCorrection .AND. (.NOT. gotIt)) THEN
            CALL Warn('StatElecReduced','Thickness correction requires thickness')
          END IF
        END IF

        IF(AmplitudeExists) THEN
          DO j=1,NoAmplitudes
            ElemAmplitude(j,1:n) = Amplitude(NoAmplitudes*(AmplitudePerm( NodeIndexes(1:n) ) -1) + j)
          END DO
        ELSE
          ElemAmplitude(1,1:n) = ListGetReal( Material,'Amplitude',n,NodeIndexes,GotIt)
          IF(.NOT.GotIt) ElemAmplitude(1,1:n) = 1.0d0 
          MaxAmplitudes(1) = MAX(MaxAmplitudes(1), MAXVAL(ABS(ElemAmplitude(1,1:n))) ) 
        ENDIF

        IF(ApertureExists) THEN
          ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n) ))
        ELSE
          ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes,GotIt)
          IF(.NOT.GotIt) ElemAperture(1:n) = 1.0d0 
        ENDIF

        IF(.NOT. TransientSimulation) THEN
          ElemAperture(1:n) = ElemAperture(1:n) + Zpos * ElemAmplitude(1,1:n) 
        END IF
        
        IF (pos == 0) THEN
          Zmax = MAX(Zmax, MAXVAL(ElemAmplitude(1,1:n)/ElemAperture(1:n)))
          Zmin = MIN(Zmin, MINVAL(ElemAmplitude(1,1:n)/ElemAperture(1:n)))
        END IF

        Direction(1:n) = ListGetReal(Material,'Direction',n,NodeIndexes,GotIt)
        IF(.NOT. GotIt) Direction(1:n) = 1.0d0

        DO i=1,n
          j = ForcePerm( NodeIndexes(i) )
          
          ! Compute stuff assuming a thin dielectric layer. 
          IF(LayerExists) THEN
            IF(ElemAperture(i) > LayerThickness(i)) THEN
              EffectiveAperture(i) = (ElemAperture(i) - LayerThickness(i)) + &
                  LayerThickness(i)*Permittivity(i)/LayerPermittivity(i)
            ELSE 
              EffectiveAperture(i) = ElemAperture(i) * Permittivity(i) / LayerPermittivity(i)
            END IF
          ELSE
            EffectiveAperture(i) = ElemAperture(i) 
          END IF

          ! For each area element use plate capacitor approximation
          IF(ElemDim == 2) THEN
            IF(HoleCorrection) THEN
              CALL ComputeHoleCorrection(HoleType, HoleSize(i), Thickness(i), &
                  HoleFraction(i), EffectiveAperture(i), Alpha, Beta, Gamma)
            END IF

            ElemForce(i) = -0.5d0 * Beta * (PotentialDifference(i) ** 2.0d0) * &
                Direction(i) * Permittivity(i) /  EffectiveAperture(i)**2.0
            
            ElemEnergy(i) = 0.5d0 * Alpha * (PotentialDifference(i) ** 2.0d0) * &
                Permittivity(i) / EffectiveAperture(i)
            
            ElemCharge(i) = Beta * PotentialDifference(i) * &
                Permittivity(i) /  EffectiveAperture(i)**2.0
            
            ElemSpring(i) = Gamma * (PotentialDifference(i) ** 2.0d0) * &
                Permittivity(i) /  EffectiveAperture(i)**3.0

            IF(.NOT. NodeComputed(j)) THEN
              NodeComputed(j) = .TRUE.
              IF(PotentialNo == 1) THEN
                Force(j) = ElemForce(i) 
                IF(ComputeField) Field(j) = &
                    - Direction(i) * PotentialDifference(i) / EffectiveAperture(i)
                IF(ComputeEnergy) Energy(j) = ElemEnergy(i)
                IF(ComputeSpring) Spring(j) = ElemSpring(i)
              ELSE
                Force(j) = Force(j) + ElemForce(i) 
                IF(ComputeField) Field(j) = Field(j) &
                    - Direction(i) * PotentialDifference(i) / EffectiveAperture(i)
                IF(ComputeEnergy) Energy(j) = Energy(j) + ElemEnergy(i)
                IF(ComputeSpring) Spring(j) = Spring(j) + ElemSpring(i)
              END IF
            END IF
          END IF

          ! For each side element use fringe field approximation
          IF(ElemDim == 1) THEN           
            CALL ComputeSideCorrection(EffectiveAperture(i), Width(i), Thickness(i), &
                SymmetricCapacitor, Alpha, Beta, Gamma)

            ElemForce(i) = -0.5d0 * Beta * (PotentialDifference(i) ** 2.0d0) * &
                Direction(i) * Permittivity(i) /  EffectiveAperture(i)
            
            ElemEnergy(i) = 0.5d0 * Alpha * (PotentialDifference(i) ** 2.0d0) * &
                Permittivity(i) 
            
            ElemCharge(i) = Beta * PotentialDifference(i) * &
                Permittivity(i) /  EffectiveAperture(i)
            
            ElemSpring(i) = Gamma * (PotentialDifference(i) ** 2.0d0) * &
                Permittivity(i) /  EffectiveAperture(i)**2.0

            IF(.NOT. NodeComputed(j)) THEN
              NodeComputed(j) = .TRUE.
              IF(PotentialNo == 1) THEN
                SideForce(j) = ElemForce(i) 
                IF(ComputeEnergy) SideEnergy(j) = ElemEnergy(i)
                IF(ComputeSpring) SideSpring(j) = ElemSpring(i)
              ELSE
                SideForce(j) = SideForce(j) + ElemForce(i) 
                IF(ComputeEnergy) SideEnergy(j) = SideEnergy(j) + ElemEnergy(i)
                IF(ComputeSpring) SideSpring(j) = SideSpring(j) + ElemSpring(i)
              END IF
            END IF
          END IF

        END DO

!#if 0
!        PRINT *,'Elem',t
!        PRINT *,'ElemForce',ElemForce(1:n)
!        PRINT *,'ElemAmplitude',ElemAmplitude(1,1:n)
!        PRINT *,'ElemAperture',ElemAperture(1:n)
!#endif

        CALL LumpedIntegral(n, Model, ElementNodes, CurrentElement, &
            ElemEnergy, ElemForce, ElemSpring, ElemCharge, &
            EffectiveAperture, ElemAmplitude)

      END DO  ! Of Elements

!------------------------------------------------------------------------------
!    Compute the lumped quantities for the electrostatic problem
!------------------------------------------------------------------------------

      IF(ABS(MaxPotential(PotentialNo)) < 1.0d-20) THEN
        CALL Fatal('StatElecReduced','There is no non-zero potential present')
      END IF

      Capacitance(PotentialNo) = 2.0d0 * LumpedEnergy(PotentialNo) / &
          (MaxPotential(PotentialNo) ** 2.0d0)
      
      DO i=1,NoAmplitudes
        LumpedForce(PotentialNo,i)  = LumpedForce(PotentialNo,i) / MaxAmplitudes(i)
        LumpedCharge(PotentialNo,i)  = LumpedCharge(PotentialNo,i) / MaxAmplitudes(i)
        DO j=1,NoAmplitudes
          LumpedSpring(PotentialNo,i,j) = LumpedSpring(PotentialNo,i,j) / &
              (MaxAmplitudes(i) * MaxAmplitudes(j) )
        END DO
      END DO

      IF(.NOT. TwoPotentialsExist) EXIT
     
    END DO! PotentialNo    


    IF (pos == 0 .AND. .NOT. SubroutineVisited) THEN
! .AND. &
!        (ScanPosition .OR. PullIn .OR. BiasOn)) THEN

      LumpedSpring0 = LumpedSpring(1,1,1)+LumpedSpring(2,1,1)
      ScanMaxAsymmetry = 100.0
      IF(Zmin > -Zmax / ScanMaxAsymmetry) THEN
        Zmin = -Zmax / ScanMaxAsymmetry 
      ELSE IF(Zmax < -Zmin / ScanMaxAsymmetry) THEN
        Zmax = -Zmin / ScanMaxAsymmetry
      END IF
      Zmax = 1.0/Zmax
      Zmin = 1.0/Zmin
      IF(Zmax < -Zmin) THEN
        Zcritical = Zmax
        IF(ScanRangeMax > -Zmin/Zmax) ScanRangeMax = -Zmin/Zmax - 0.01
      ELSE 
        Zcritical = Zmin
        IF(ScanRangeMax > -Zmax/Zmin) ScanRangeMax = -Zmax/Zmin - 0.01
      END IF
!    ELSE 
!      aa = Zpos / ((LumpedSpring0/(LumpedSpring(1,1,1)+LumpedSpring(2,1,1))) ** (1.0/3.0) - 1.0)
    END IF

    IF(PullIn .AND. (pos > 0 .OR. SubroutineVisited)) THEN
      Work = ListGetConstReal(Model % Simulation, 'res: Elastic Spring',gotIt )
      IF(gotIt) THEN
        PullInScale = SQRT(ABS(Work/(LumpedSpring(1,1,1)+LumpedSpring(2,1,1)))) * PullInScaleOld
        LumpedForce = PullInScale**2.0 * LumpedForce
        LumpedSpring = PullInScale**2.0 * LumpedSpring
      ELSE
        Work = ListGetConstReal( Model % Simulation, 'res: Elastic Displacement')
        PullInScale = SQRT(ABS(Zpos/Work)) * PullInScaleOld

        LumpedForce = PullInScale**2.0 * LumpedForce
        LumpedSpring = PullInScale**2.0 * LumpedSpring

        IF(SubroutineVisited) THEN
          ForceRatio = SQRT(ABS(OldLumpedForce / LumpedForce(1,1) ))
        END IF
      END IF

      PullInVoltage = PullInScale * MaxPotential(1)

      LumpedCharge = PullInScale * LumpedCharge
      LumpedSpringDz = PullInScale**2.0 * LumpedSpringDz
      LumpedSpringDzDz = PullInScale**2.0 * LumpedSpringDzDz

      Force = PullInScale**2.0 * Force
      IF(ComputeField) Field = PullInScale * Field
      IF(ComputeEnergy) Energy = PullInScale**2.0 * Energy 
      IF(ComputeSpring) Spring = PullInScale**2.0 * Spring

    END IF

    ! The same info may be echoed, printed to external files and
    ! saved for later usage with a prefix 'res:'
    NoValues = 0
    IF(pos > 0) CALL AddToSaveList('Position',Zpos,'(m)')
    CALL AddToSaveList('Critical amplitude',Zcritical,'(m)',.FALSE.,.FALSE.)

    DO PotentialNo=1,2
      IF(PotentialNo == 1) THEN
        Too = ' '
      ELSE
        Too = 'B'
      END IF

      CALL AddToSaveList('Max Potential '//Too,MaxPotential(PotentialNo),'(V)',.FALSE.,.TRUE.)
      CALL AddToSaveList('Capacitance '//Too,Capacitance(PotentialNo),'(C/V)')
      CALL AddToSaveList('Electric energy '//Too,LumpedEnergy(PotentialNo),'(J)')
      
      DO i=1,NoAmplitudes
        WRITE(Message,'(A)') 'Electric Force '//Too
        IF(NoAmplitudes > 1) WRITE(Message,'(A,I1)') TRIM(Message)//' ',i
        CALL AddToSaveList(Message,LumpedForce(PotentialNo,i),'(N)')
      END DO

      DO i=1,NoAmplitudes
        WRITE(Message,'(A)') 'Electric current sensivity '//Too
        IF(NoAmplitudes > 1) WRITE(Message,'(A,I1)') TRIM(Message)//' ',i
        CALL AddToSaveList(Message,LumpedCharge(PotentialNo,i),'(C/m)')
      END DO
      
      DO i=1,NoAmplitudes
        DO j=1,i
          WRITE(Message,'(A)') 'Electric Spring '//Too
          IF(NoAmplitudes > 1) WRITE(Message,'(A,I1,I2)') TRIM(Message)//' ',i,j
          CALL AddToSaveList( Message,LumpedSpring(PotentialNo,i,j),'(N/m)')
        END DO
      END DO

      IF(.NOT. TwoPotentialsExist) EXIT
    END DO

    IF(TwoPotentialsExist) THEN
      IF(NoAmplitudes > 1) THEN
        IF(ABS(LumpedForce(2,1)) < ABS(LumpedForce(2,2))) THEN
          Work = LumpedForce(2,1) / LumpedForce(2,2)
        ELSE
          Work = LumpedForce(2,2) / LumpedForce(2,1)
        END IF
        CALL AddToSaveList('Relative coupling',Work,'')
      END IF
    END IF      

    IF(SpringDerivatives) THEN
      LumpedSpringDz = LumpedSpringDz / MaxAmplitudes(1) ** 3.0d0
      bb =  -3.0d0 * (LumpedSpring(1,1,1)+LumpedSpring(2,1,1)) / LumpedSpringDz
      LumpedSpringDzDz = LumpedSpringDzDz / MaxAmplitudes(1) ** 4.0d0
      cc = SQRT(12.0d0 * ABS(LumpedSpring(1,1,1)+LumpedSpring(2,1,1))) / LumpedSpringDzDz
      CALL AddToSaveList('Lumped DSpring',LumpedSpringDz,'(N/m^2)')
      CALL AddToSaveList('Lumped Spring D1',bb,'(m)',.FALSE.,.FALSE.)
      CALL AddToSaveList('Lumped DDSpring',LumpedSpringDzDz,'(N/m^3)')
      CALL AddToSaveList('Lumped Spring D2',cc,'(m)',.FALSE.,.FALSE.)
    END IF
    
    DO j=0,LumpingDegree
      DO i=0,LumpingDegree
        IF(i==0 .AND. j==0) THEN
          CALL AddToSaveList('Charged area',LumpingFactors(1,1),'m^2',.FALSE.)
        ELSE IF(j==0) THEN
!          LumpingFactors(i+1,1) = LumpingFactors(i+1,1)/LumpingFactors(1,1)
          WRITE(Message,'(A,I1)') 'Lumping factor U^',i
          CALL AddToSaveList(Message,LumpingFactors(i+1,1),'',.FALSE.)
        ELSE 
!          LumpingFactors(i+1,j+1) = (Zcritical**j)*LumpingFactors(i+1,j+1)/LumpingFactors(1,1)
          WRITE(Message,'(A,I1,A,I1)') 'Lumping factor U^',i,'/D^',j
          CALL AddToSaveList(Message,LumpingFactors(i+1,j+1),'')
        END IF
      END DO
    END DO
    
    IF(PullIn) THEN
      CALL AddToSaveList('Pull-In Scale',PullInScale,'')
      CALL AddToSaveList('Pull-In Voltage',PullInVoltage,'(V)')
      CALL AddToSaveList('Pull-In Relative Displacement',PullInRelative,'')
    END IF
    
    IF(pos <= 1) THEN
      IF(NoPositions > 0) THEN
        WRITE(Message,'(A,I3,A)') 'Values after ',pos,' steps' 
        CALL Info('StatElecReduced',Message,Level=5)
      END IF
      DO t=1,NoValues
        WRITE(Message,'(A,T35,ES15.5)') TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
        CALL Info('StatElecReduced',Message,Level=5)
      END DO
    END IF
    
    IF(pos == 1) THEN
      IF(PotentialNo > 1 .OR. (SubroutineVisited .AND. FileAppend)) THEN 
        OPEN (10, FILE=Filename,POSITION='APPEND')
      ELSE 
        OPEN (10,FILE=Filename)
      END IF
    END IF

    IF(pos >= 1) THEN
      DO t=1,NoValues 
        IF(ValueSaveLocal(t)) WRITE(10,'(ES15.5)',ADVANCE='NO') Values(t)
      END DO
      IF(FileAppend) THEN 
        WRITE(10,'(I5)') pos
      ELSE
        WRITE(10,'(A)') ' '
      END IF
    ENDIF

    IF(ScanPosition .AND. pos == 0 ) THEN
      IF(ValueSaveRes(t)) CALL ListAddConstReal( Model % Simulation, &
          'res: '//TRIM(ValueNames(t)), Values(t) )
    END IF
    
    IF(ScanPosition .AND. AplacExport) THEN
      IF(pos == 0) CALL MakeAplacModel()
      IF(pos == 1) CALL MakeAplacModel()
      IF(pos == NoPositions) CALL MakeAplacModel()
    END IF

  END DO

  IF(NoPositions > 0) THEN
    CLOSE(10)
    OPEN (10, FILE=FilenameNames)
    WRITE(10,'(A)') 'Position dependent variables in file '//TRIM(Filename) 
    i = 0
    DO t=1,NoValues
      IF(ValueSaveLocal(t)) THEN
        i = i+1
        WRITE(10,'(I2,T4,A)') i,TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t))
      END IF
    END DO
    WRITE(10,'(A)') 'Other variables and constants'
    DO t=1,NoValues
      IF(.NOT. ValueSaveLocal(t)) THEN
        WRITE(10,'(A,T20,ES15.5)') TRIM(ValueNames(t)),Values(t)
      END IF
    END DO
    CLOSE(10)
  END IF

  IF(pos > 1) THEN
    WRITE(Message,'(A,I3,A)') 'Values after ',pos,' steps' 
    CALL Info('StatElecReduced',Message,Level=5)
    DO t=1,NoValues
      WRITE(Message,'(A,T35,ES15.5)') &
          TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
      CALL Info('StatElecResuced',Message,Level=5)
    END DO
  END IF

  ! Add variabes that may be read by other solvers and 
  ! saved to result matrix. 
  IF(.NOT. ScanPosition) THEN 
    DO t=1,NoValues
      IF(ValueSaveRes(t)) CALL ListAddConstReal( Model % Simulation, &
          'res: '//TRIM(ValueNames(t)), Values(t) )
    END DO
  END IF

  IF(AplacExport) CALL MakeAplacModel()  

  SubroutineVisited = .TRUE.


!------------------------------------------------------------------------------
 

CONTAINS

  SUBROUTINE MakeAplacModel()
    
    INTEGER :: i,j,n,phase
    REAL(KIND=dp), POINTER :: kp(:,:), km(:,:), k0(:,:), e0, f0(:), cc(:,:)
    REAL(KIND=dp) :: phi, p, a, q, preva, f
    LOGICAL :: AplacAllocated = .FALSE.

    SAVE kp, km, k0, f0, cc, AplacAllocated, phase, p
    
    IF(.NOT. AplacAllocated) THEN
      n = NoAmplitudes
      ALLOCATE(kp(n,n), km(n,n), k0(n,n), f0(n), e0, cc(n,5))
      AplacAllocated = .TRUE.
    END IF


    PotentialNo = 1

    IF(.NOT. ScanPosition .OR. pos == 0) THEN

      CALL ListAddInteger( Model % Simulation, 'aplac: elstat mode', 1)
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat area', LumpingFactors(1,1))
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat aeff1', LumpingFactors(2,1))
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat aeff2', LumpingFactors(3,1))
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat deff3', &
          (LumpingFactors(3,1)/LumpingFactors(3,4))**(1.0d0/3.0) )
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat displ', Zpos)
      IF(Thickness(1) > 1.0d-20) THEN        
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat thick', Thickness(1))
      END IF
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat voltage', MaxPotential(1))
      CALL ListAddConstReal( Model % Simulation, 'aplac: elstat capa', Capacitance(1))

      DO i=1,NoAmplitudes
        WRITE(Message,'(A,I1)') 'aplac: elstat charge ',i
        CALL ListAddConstReal( Model % Simulation, Message, LumpedCharge(1,i)) 
        WRITE(Message,'(A,I1)') 'aplac: elstat force ',i
        CALL ListAddConstReal( Model % Simulation, Message, LumpedForce(1,i)) 
        DO j=1,NoAmplitudes
          WRITE(Message,'(A,I1,I1)') 'aplac: elstat spring ',i,j
          CALL ListAddConstReal( Model % Simulation, Message, LumpedSpring(1,i,j)) 
        END DO
      END DO
    END IF

    IF(ScanPosition) THEN 
      IF(pos == 0) THEN
        phase = 1
        e0 = LumpedEnergy(PotentialNo)
        
        phi = MaxPotential(PotentialNo)
        IF(ABS(ScanRangeMax+ScanRangeMin) > 1.0d-4) THEN
          CALL Fatal('StatElecReduced','In fitting Aplac results Range should be symmetric')
        END IF
        p = (ScanRangeMax - ScanRangeMin) / 2.0d0
        DO i=1,NoAmplitudes
          f0(i) = LumpedForce(PotentialNo,i)
          DO j=1,NoAmplitudes
            k0(i,j) = LumpedSpring(PotentialNo,i,j)
          END DO
        END DO
      ELSE IF(pos == 1) THEN
        IF(phase /= 1) CALL Warn('StatElecReduced','Phase should be 1') 
        phase = 2
        DO i=1,NoAmplitudes
          DO j=1,NoAmplitudes
            km(i,j) = LumpedSpring(PotentialNo,i,j)
          END DO
        END DO
      ELSE IF(pos == NoPositions) THEN
        IF(phase /= 2) CALL Warn('StatElecReduced','Phase should be 2') 
        phase = 3
        DO i=1,NoAmplitudes
          DO j=1,NoAmplitudes
            kp(i,j) = LumpedSpring(PotentialNo,i,j)
          END DO
        END DO
      END IF
      
      IF(phase == 3) THEN
        DO i=1,NoAmplitudes
          q = (kp(i,i) + km(i,i)) / k0(i,i)
          preva = 0.0d0
          a = 1.0d0
          j = 0
          DO WHILE(ABS(a-preva) > 1.0d-6)
            j = j + 1
            preva = a
            a = 2.0d0 + LOG(q-(1.0+p)**(a-2.0)) / (LOG(1.0-p)) 
            IF(j > 20) THEN
              CALL Warn('StatElecReduced','Convergence for power a was not obtained')
              EXIT
            END IF
          END DO
          cc(i,1) = a
          
          cc(i,2) = (km(i,i) - (1.0+p)**(a-2.0)*k0(i,i)) / ((1-p)**(a-2.0) - (1+p)**(a-2.0))
          cc(i,2) = cc(i,2) * 2.0 * Zcritical**2.0 / a / (a-1.0) / phi**2.0
          
          cc(i,3) = (km(i,i) - (1.0-p)**(a-2.0)*k0(i,i)) / ((1+p)**(a-2.0) - (1-p)**(a-2.0))
          cc(i,3) = cc(i,3) * 2.0 * Zcritical**2.0 / a / (a-1.0) / phi**2.0
          
          cc(i,4) = f0(i) - (cc(i,2) - cc(i,3)) * phi**2.0 * cc(i,1) / Zcritical / 2.0d0
          cc(i,4) = cc(i,4) * 2.0 * Zcritical / phi**2.0
          
          cc(i,5) = 2.0*e0/phi**2.0 - cc(i,2) - cc(i,3) 
        END DO

        i = 1
        a = cc(i,2) + cc(i,3) + cc(i,5)
        cc(i,2:5) = cc(i,2:5) / a

        ! Using five evnly distributed test points Capacitance is fitted to model 
        ! C=C_0(b_0 + b_1 p + b_i (1+p)^a + b_d (1-p)^a), where (p=d/d_0) and
        ! a=C0, cc(i,5)=b_0, cc(i,4)=b_1, cc(i,3)=b_i, cc(i,2)=b_d, cc(i,1)=a

        CALL ListAddInteger( Model % Simulation, 'aplac: elstat mode', 2)
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat zcrit', Zcritical)
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat aeff0', LumpingFactors(1,1))

        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat capa', a)
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat c1', cc(i,1))      
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat c2', cc(i,2))
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat c3', cc(i,3))
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat c4', cc(i,4))
        CALL ListAddConstReal( Model % Simulation, 'aplac: elstat c5', cc(i,5))
    
        phase = 0
      END IF

    END IF

  END SUBROUTINE MakeAplacModel

!------------------------------------------------------------------------------

  SUBROUTINE AddToSaveList(Name, Value, Unit, savelocal, saveres)

    INTEGER :: n
    CHARACTER(LEN=*) :: Name, Unit
    REAL(KIND=dp) :: Value
    LOGICAL, OPTIONAL :: savelocal,saveres

    n = NoValues
    n = n + 1
    IF(n > MaxNoValues) THEN
      CALL WARN('StatElecReduced','Too little space for the scalars')
      RETURN
    END IF

    Values(n) = Value
    ValueNames(n) = TRIM(Name)
    ValueUnits(n) = TRIM(Unit)
    IF(PRESENT(savelocal)) THEN
      ValueSaveLocal(n) = savelocal
    ELSE 
      ValueSaveLocal(n) = .TRUE.
    END IF
    IF(PRESENT(saveres)) THEN
      ValueSaveRes(n) = saveres
    ELSE 
      ValueSaveRes(n) = .TRUE.
    END IF

    NoValues = n

  END SUBROUTINE AddToSaveList



!------------------------------------------------------------------------------
   SUBROUTINE LumpedIntegral(n, Model, ElementNodes, CurrentElement,  &
       Energy, Force, Spring, Charge, Aperture, Amplitude)

 !------------------------------------------------------------------------------
     INTEGER :: n
     TYPE(Model_t) :: Model
     TYPE(Nodes_t) :: ElementNodes
     TYPE(Element_t), POINTER :: CurrentElement
     REAL(KIND=dp) :: Energy(:), Force(:), Spring(:), &
         Aperture(:), Amplitude(:,:), Charge(:)

!------------------------------------------------------------------------------
     
     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
     REAL(KIND=dp), DIMENSION(:), POINTER :: &
         U_Integ,V_Integ,W_Integ,S_Integ
     REAL(KIND=dp) :: s,ug,vg,wg
     REAL(KIND=dp) :: ddBasisddx(Model % MaxElementNodes,3,3)
     REAL(KIND=dp) :: Basis(Model % MaxElementNodes)
     REAL(KIND=dp) :: dBasisdx(Model % MaxElementNodes,3),SqrtElementMetric
     REAL(KIND=dp) :: dV, Amplitudei, Amplitudej
     INTEGER :: N_Integ, t, tg, ii, jj, i,j,k,l
     LOGICAL :: stat

!------------------------------------------------------------------------------
!    Gauss integration stuff
!------------------------------------------------------------------------------
     IntegStuff = GaussPoints( CurrentElement )
     U_Integ => IntegStuff % u
     V_Integ => IntegStuff % v
     W_Integ => IntegStuff % w
     S_Integ => IntegStuff % s
     N_Integ =  IntegStuff % n

!------------------------------------------------------------------------------
! Loop over Gauss integration points
!------------------------------------------------------------------------------
     DO tg=1,N_Integ

       ug = U_Integ(tg)
       vg = V_Integ(tg)
       wg = W_Integ(tg)

!------------------------------------------------------------------------------
! Need SqrtElementMetric and Basis at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( CurrentElement,ElementNodes,ug,vg,wg, &
           SqrtElementMetric,Basis,dBasisdx,ddBasisddx,.FALSE. )

       s = SqrtElementMetric * S_Integ(tg)
 
       dV = CoordinateSqrtMetric( SUM( ElementNodes % x(1:n) * Basis(1:n)), &
           SUM( ElementNodes % y(1:n) * Basis(1:n)), &
           SUM( ElementNodes % z(1:n) * Basis(1:n)) )
 
! Calculate the function to be integrated at the Gauss point

       LumpedEnergy(PotentialNo) = LumpedEnergy(PotentialNo) + &
           s * dV * SUM( Energy(1:n) * Basis(1:n) )

       DO i=1,NoAmplitudes
         
         Amplitudei = SUM(Amplitude(i,1:n) * Basis(1:n))

         LumpedForce(PotentialNo,i) = LumpedForce(PotentialNo,i) + &
             s * dV * Amplitudei * SUM( Force(1:n) * Basis(1:n) )
         LumpedCharge(PotentialNo,i) = LumpedCharge(PotentialNo,i) + &
             s * dV * Amplitudei * SUM( Charge(1:n) * Basis(1:n) )
         
         DO j=1,NoAmplitudes
           Amplitudej = SUM(Amplitude(j,1:n) * Basis(1:n))
           LumpedSpring(PotentialNo,i,j) = LumpedSpring(PotentialNo,i,j) + &
               s * dV * Amplitudei * Amplitudej * SUM( Spring(1:n) * Basis(1:n) )
         END DO
       END DO

       IF(ElemDim == 2 .AND. SpringDerivatives) THEN
         Amplitudei = SUM(Amplitude(i,1:n) * Basis(1:n))
         LumpedSpringDz = LumpedSpringDz + s * dV * &
             (-3.0d0 * Amplitudei**3.0) * SUM(Spring(1:n) * Basis(1:n) / Aperture(1:n) )
         LumpedSpringDzDz = LumpedSpringDzDz + s * dV * &
             (-3.0d0 * Amplitudei**4.0) * SUM(Spring(1:n) * Basis(1:n) / Aperture(1:n)**2.0 )
       END IF

       IF(ElemDim == 2 .AND. PotentialNo == 1) THEN
         DO k=0,LumpingDegree
           DO l=0,LumpingDegree
             IF(k==0 .AND. l==0) THEN
               LumpingFactors(k+1,l+1) = LumpingFactors(k+1,l+1) + &
                   s * dV * SUM(Basis(1:n)) 
             ELSE IF(k==0) THEN
               LumpingFactors(k+1,l+1) = LumpingFactors(k+1,l+1) + &
                   s * dV * SUM(Basis(1:n) / (Aperture(1:n)**l) )
             ELSE IF(l==0) THEN
               LumpingFactors(k+1,l+1) = LumpingFactors(k+1,l+1) + &
                   s * dV * SUM(Basis(1:n) * (Amplitude(1,1:n)**k) )
             ELSE
               LumpingFactors(k+1,l+1) = LumpingFactors(k+1,l+1) + &
                   s * dV * SUM(Basis(1:n) * (Amplitude(1,1:n)**k) / (Aperture(1:n)**l) )
             END IF
           END DO
         END DO
       END IF

     END DO! of the Gauss integration points
       
!------------------------------------------------------------------------------
   END SUBROUTINE LumpedIntegral
!------------------------------------------------------------------------------


   SUBROUTINE ComputeHoleCorrection(holemodel, r, b, p, d, alpha, beta, gamma)
     ! r=radius, b=hole length, d=aperture, p=hole fraction
     
     CHARACTER(LEN=*) :: holemodel
     REAL(KIND=dp) :: r,d,b,p,alpha,beta,gamma,a,da,dda,c1,c2,dom

     SELECT CASE(holemodel)

       CASE ('slot')
       c1 = 2.3198
       c2 = 0.2284 
 
       CASE ('round')
       c1 = 4.2523d0
       c2 = 0.4133d0
       
       CASE ('square')
       c1 = 3.8434 
       c2 = 0.3148

     CASE DEFAULT 
       alpha = 1.0
       beta = 1.0
       gamma = 1.0

       CALL WARN('ComputeHoleCorrection','Unknown hole type')       

       RETURN
     END SELECT
 
     dom = 1.0d0 + c1*(d/r) + c2* (d/r)**2.0
     a = 1.0 - p * 1.0d0/dom
     da = p * (c1+2.0*c2*(d/r)) / dom**2.0
     dda = p * 2.0 * (c2-2.0* c1**2.0-3.0*c1*c2*(d/r)-3.0* c2**2.0 * (d/r)**2.0) / dom**3.0     
      
     alpha = a
     beta = a - da*(d/r)
     gamma = a - da*(d/r) + 0.5d0*dda*((d/r)**2.0)
     
   END SUBROUTINE ComputeHoleCorrection
   
!------------------------------------------------------------------------------

   SUBROUTINE ComputeSideCorrection(h, a, t, symm, alpha, beta, gamma)
     ! h=aperture, a=width, t=thickness
     ! From APLAC documentation by Timo Veijola     
     ! The result is multiplied appropriately so that the units vanish

     LOGICAL :: symm
     REAL(KIND=dp) :: h, a, t, alpha, beta, gamma, csymm
     REAL(KIND=dp) :: ah, ahh, th, hh, f, fh, fhh, c1th, thc3, c2thc3
     REAL(KIND=dp), PARAMETER :: c1=2.158, c2=0.153, c3=0.231, c4=0.657

     a = a / 2.0d0

     IF(symm) THEN
       csymm = 1.0d0
     ELSE
       csymm = 2.0d0
     END IF

     h = csymm * h
     th = t/h
     hh = h*h

     IF(th < 1.0d-20) THEN
       alpha = (csymm / PI) * ( 1.0 + LOG(2*PI*a/h) )
       beta = csymm / PI 
       gamma = csymm / PI
     ELSE
       c2thc3 = c2+th**c3

       f = LOG(c3*th/c2thc3+1.0)
       fh = -c3**2.0 * c2thc3 / (h*c2thc3**2.0)

       a = c1*th/c2thc3
       ah = -(a/h)*(1-c3*(th**c3)/c2thc3)
       ahh = (ah-a/h)*ah/a - (a/h)*fh

       alpha = (csymm / PI) * ( 1.0 + LOG(2*PI*a/h) + c4*a)
       beta = csymm * (-h / PI) * (-1.0/h + c4*ah/(a+1) )
       gamma = csymm * (hh / PI) * (1/hh + c4*ahh/(a+1) - c4*(ah/(a+1))**2.0 )
     END IF

   END SUBROUTINE ComputeSideCorrection
!------------------------------------------------------------------------------

   
 END SUBROUTINE StatElecReduced
!------------------------------------------------------------------------------
