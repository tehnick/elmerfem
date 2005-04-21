!/*****************************************************************************
! *
! *       Elmer, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 -> , CSC - Scientific Computing Ltd, Findland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! ****************************************************************************/
!
!  Solves the Reynolds Equation that is a dimensinally reduced form of 
!  Navier-Stokes equations in the case of narrow channels. There is a 
!  transient and time-harmonic version of the same equation. In addition, 
!  there are common routines for computing the impedances for holes and 
!  open ends. 
!
! *****************************************************************************
! *
! *                    Author: Peter Råback, Antti Pursula
! *
! *                 Address:  CSC - Scientific Computing Ltd.
! *                           Tekntiikantie 6, P.O. BOX 405
! *                           02101 Espoo, Finland
! *                           Tel. +358 0 457 2080
! *                           EMail: Peter.Raback@csc.fi
! *
! *                       Date: 04 Oct 2000
! *
! *                Modified by: Peter Råback
! *
! *       Date of modification: 6.4.2003
! *
! ****************************************************************************/

!------------------------------------------------------------------------------
! Solver the Harmonic version of the Reynolds equation
!------------------------------------------------------------------------------
SUBROUTINE HarmonicReynoldsSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: HarmonicReynoldsSolver

  USE Types
  USE Lists
  USE Integration
  USE ElementDescription
  USE SolverUtils
  USE ReynoldsUtilities

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement, Parent
  TYPE(Solver_t), POINTER :: PSolver 
  TYPE(ValueList_t), POINTER :: Material
  TYPE(Variable_t), POINTER :: AmplitudeVar, ApertureVar, DampingVar

  INTEGER :: iter, i, j, k, n, t, istat, mat_id, mat_idold, &
      NonlinearIter, NoPerturbations, Perturbation, NoModes, &
      olditer, NoIterations, NoElements, NoNodes, &
      Mode, NoAmplitudes, dofs, AplacFluidMode
  INTEGER, POINTER :: NodeIndexes(:), PressurePerm(:), &
      AmplitudePerm(:), AperturePerm(:)

  LOGICAL :: AllocationsDone = .FALSE., GotIt, &
      stat, EfficientViscosity, Adiabatic, Incompressible, &
      ScanFrequency, Visited = .FALSE., Perturbations, &
      HoleCorrection, SideCorrection, HolesExist, CalculateDamping, &
      ApertureExists, AmplitudeExists, AplacExport 

  REAL(KIND=dp), POINTER :: Pressure(:), PressureDer(:), FilmDamping(:), &
      ForceVector(:), Amplitude(:), Aperture(:), PressurePoint(:)
      
  REAL(KIND=dp) :: Norm, AngularVelocity, &
      Frequency, ReferenceTemperature, ReferencePressure, MeanFreePath, KnudsenNumber, &
      ElasticMass, ElasticSpring, ReImpedanceCorrection, ImImpedanceCorrection, &
      MaxAperture, MinAperture, s, LumpedForce, wmin, wmax, &
      AngularVelocity0, ScanRangeMin, ScanRangeMax, ScanGeometric, WorkReal, &
      HeatRatio, MaxTemperature, Pabs, ViscCorr, OldNorm, TotalArea
  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), &
      LocalMassMatrix(:,:), LocalForce(:), Viscosity(:), &
      Pimag(:), Preal(:), ElemPressure(:), Work(:), &
      ElemAperture(:), ElemAmplitude(:), HoleFraction(:), &
      HoleSize(:), HoleDepth(:), Density(:), &
      ImHoleImpedance(:), ReHoleImpedance(:), MaxAmplitudes(:), &
      Freal(:,:,:), Fimag(:,:,:), PaveReal(:), PaveImag(:), MaxPressures(:), &
      cutoff(:,:), cR0(:,:), cr1(:,:), cL0(:,:), ci1(:,:)
  COMPLEX(KIND=dp), ALLOCATABLE :: Impedance(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, Filename, FilenameNames, &
      HoleType, CompressibilityFlag, String1

  ! These variables are for efficient manipulation of local scalats data
  INTEGER, PARAMETER :: MaxNoValues = 100
  INTEGER :: NoValues
  REAL (KIND=dp) ::  Values(MaxNoValues) 
  CHARACTER(LEN=MAX_NAME_LEN) :: ValueNames(MaxNoValues), ValueUnits(MaxNoValues)
  LOGICAL :: ValueSaveLocal(MaxNoValues),ValueSaveRes(MaxNoValues)
  

  SAVE LocalMassMatrix, LocalStiffMatrix, Work, LocalForce, ElementNodes, &
       Amplitude, Aperture, ElemAperture, ElemAmplitude, Viscosity, &
       Density, AllocationsDone, Pimag, Preal, PressureDer, ElemPressure, &
       HoleFraction, HoleSize, HoleDepth, Impedance, FilmDamping, &
       ImHoleImpedance, ReHoleImpedance, Freal, Fimag, CalculateDamping, &
       Visited, PaveReal, PaveImag, MaxAmplitudes, MaxPressures, &
       cR0, cL0, cutoff

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------

  IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

  EquationName = ListGetString( Solver % Values, 'Equation' )
  Pressure     => Solver % Variable % Values
  PressurePerm => Solver % Variable % Perm

  dofs = Solver % Variable % DOFs 
  IF(dofs /= 2) THEN
    CALL Fatal('HarmonicReynoldsSolver','Harmonic Simulation requires two dofs!')
  END IF

  IF( COUNT( PressurePerm > 0 ) <= 0) RETURN

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS
  Norm = Solver % Variable % Norm
  NoElements =  Solver % Mesh % NumberOfBulkElements
  NoNodes = Model % NumberOfNodes

!------------------------------------------------------------------------------
! Set pointers to already allocated variables 
!------------------------------------------------------------------------------

  NULLIFY(AmplitudeVar)
  AmplitudeVar => VariableGet( Model % Variables, 'Amplitude' )
  IF(ASSOCIATED(AmplitudeVar)) THEN
    NoAmplitudes = AmplitudeVar % DOFs
    IF(NoAmplitudes < 1) NoAmplitudes = 1
    AmplitudePerm  => AmplitudeVar % Perm
    Amplitude => AmplitudeVar % Values
    AmplitudeExists = .TRUE.
  ELSE
    AmplitudeExists = .FALSE.
    NoAmplitudes = 1
  ENDIF
  
  NULLIFY(ApertureVar)
  ApertureVar => VariableGet( Model % Variables, 'Aperture' )
  IF(ASSOCIATED (ApertureVar)) THEN
    AperturePerm  => ApertureVar % Perm
    Aperture => ApertureVar % Values
    ApertureExists = .TRUE.
  ELSE
    ApertureExists = .FALSE.
  ENDIF

!------------------------------------------------------------------------------
! Do some initial stuff
!------------------------------------------------------------------------------

  Solver % Matrix % COMPLEX = .FALSE.

  HoleCorrection = ListGetLogical( Solver % Values, 'Hole Correction',gotIt )
  EfficientViscosity = ListGetLogical( Solver % Values, 'Rarefaction',gotIt )
  Adiabatic = ListGetLogical( Solver % Values, 'Adiabatic', gotIt )

  SideCorrection = ListGetLogical( Solver % Values, 'Side Correction',gotIt )
  IF(.NOT. GotIt) THEN
    DO i=1,Model % NumberOfBCs
      stat = ListGetLogical(Model % BCs(i) % Values,'Open Side',gotIt) 
      IF(stat) SideCorrection = .TRUE.
    END DO
  END IF
   
  Frequency = ListGetConstReal( Solver % Values, 'Frequency', gotIt)
  IF(.NOT. gotIt) Frequency = ListGetConstReal( Model % Simulation,'res: Eigen Frequency',gotIt )
  IF(.NOT. GotIt) Frequency = ListGetConstReal( Model % Simulation,'res: Eigen Frequency 1',gotIt )    
  IF(.NOT. GotIt) CALL Fatal('HarmonicReynoldsSolver','Harmonic Reynolds requires Frequency')

  AngularVelocity0 = 2.0d0 * PI * Frequency

  AplacExport = .FALSE.
  DO k = 1, Model % NumberOfSolvers
    String1 = ListGetString( Model % Solvers(k) % Values, 'Equation',gotIt )
    IF(TRIM(String1) == 'aplac export') THEN
      AplacExport = .TRUE.
      AplacFluidMode = ListGetInteger( Model % Solvers(k) % Values, 'Fluid Mode',gotIt )
      IF(.NOT. GotIt) AplacFluidMode = 1
    END IF
  END DO
  IF(.NOT. AplacExport) THEN
    AplacExport = ListGetLogical( Model % Simulation, 'Aplac Export',gotIt)
  END IF
      
  NoIterations = 0
  NoPerturbations = 1
  NoModes = NoAmplitudes

  Perturbations = ListGetLogical( Solver % Values, 'Perturbations',gotIt )
  IF(Perturbations) NoPerturbations = 4
  
  ScanFrequency = ListGetLogical( Solver % Values, 'Scan Frequency',gotIt )
  
  IF(ScanFrequency) THEN
    NoIterations = ListGetInteger( Solver % Values, 'Scan Points')
    ScanRangeMin = ListGetConstReal( Solver % Values, 'Scan Range Min',gotIt )
    IF(.NOT. gotIt) ScanRangeMin = 0.01
    ScanRangeMax = ListGetConstReal( Solver % Values, 'Scan Range Max',gotIt )
    IF(.NOT. gotIt) ScanRangeMax = 2.0
    Filename = ListGetString(Solver % Values,'Filename',GotIt )
    IF(.NOT. gotIt) Filename = 'reynolds.dat'
    FilenameNames = TRIM(Filename) // '.' // TRIM("names")    
  END IF

!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
    N = Solver % Mesh % MaxElementNodes

    IF ( AllocationsDone ) THEN
      DEALLOCATE( ElementNodes % x, &
          ElementNodes % y, &
          ElementNodes % z, &
          LocalForce, &
          LocalStiffMatrix, &
          LocalMassMatrix, &
          Work, &
          MaxAmplitudes, &
          MaxPressures, &
          Viscosity, &
          Density, &
          HoleFraction, &
          HoleSize, &
          HoleDepth, &
          Impedance, &
          ImHoleImpedance, &
          ReHoleImpedance, &
          ElemPressure, &
          Pimag, &
          Preal, &
          Freal, &
          Fimag, &
          PressureDer, &
          cR0, & 
          cL0, &
          cutoff)

    END IF

    ALLOCATE(ElementNodes % x( N ), &
        ElementNodes % y( N ), &
        ElementNodes % z( N ), &
        Work(N), &
        Viscosity( N ), &
        ElemAperture(N), &
        ElemAmplitude(N), &
        Density(N), &
        HoleFraction(N), &
        HoleSize(N), &
        HoleDepth(N), &
        Impedance(N), &
        MaxAmplitudes(NoAmplitudes),         &
        MaxPressures(NoAmplitudes),         &
        ImHoleImpedance(N),         &
        ReHoleImpedance(N),         &
        LocalForce( dofs*N ),           &
        LocalStiffMatrix( dofs*N,dofs*N ), &
        LocalMassMatrix( dofs*N,dofs*N ), &
        Pimag( N ),              &
        Preal( N ),               &
        Freal( NoPerturbations, NoModes, NoModes ) , &
        Fimag( NoPerturbations, NoModes, NoModes ), &
        PaveReal( NoModes ) , &
        PaveImag( NoModes ), &
        ElemPressure(N), &
        cR0(NoModes, NoModes), &
        cL0(NoModes, NoModes), &
        cutoff(NoModes, NoModes), &
        STAT=istat )

    IF ( istat /= 0 ) CALL FATAL('HarmonicReynoldsSolver','Memory allocation error')


    NULLIFY(DampingVar)
    DampingVar => VariableGet( Model % Variables, 'FilmDamping' )
    IF(ASSOCIATED (DampingVar)) THEN
      CalculateDamping = .TRUE.
      FilmDamping => DampingVar % Values
    ELSE
      CalculateDamping = ListGetLogical( Solver % Values,'Calculate Damping', GotIt )
      IF ( CalculateDamping )  THEN
        ALLOCATE( FilmDamping( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) THEN
          CALL FATAL('HarmonicReynoldsSolver','Memory allocation error')
        ELSE
          PSolver => Solver
          FilmDamping = 0.0d0
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
              PSolver, 'FilmDamping', 1, FilmDamping, PressurePerm )
        END IF
      END IF
    END IF

    IF(Perturbations) THEN
      ALLOCATE( PressureDer(dofs*NoNodes), STAT=istat)
      IF ( istat /= 0 ) CALL FATAL('HarmonicReynoldsSolver','Memory allocation error')
      PressureDer = 0.0d0
    END IF

    AllocationsDone = .TRUE.
  END IF 

!------------------------------------------------------------------------------
! First compute some values for later use
!------------------------------------------------------------------------------
  MaxAperture = 0.0d0
  MinAperture = HUGE(MinAperture)
  MaxAmplitudes = 0.0d0
  DO t=1,Solver % NumberOfActiveElements          
    CurrentElement => Solver % Mesh % Elements(Solver % ActiveElements(t))
    n = CurrentElement % TYPE % NumberOfNodes
    NodeIndexes => CurrentElement % NodeIndexes

!------------------------------------------------------------------------------
!       Get material parameters
!------------------------------------------------------------------------------        
    mat_id = ListGetInteger( Model % Bodies( CurrentElement % &
         Bodyid ) % Values, 'Material', minv=1, maxv=Model % NumberOfMaterials )
          
    Material => Model % Materials(mat_id) % Values
          
    ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes,GotIt)
    IF(.NOT. GotIt) THEN
      IF(ApertureExists) THEN
        ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n) ))
      ELSE
        CALL Warn('HarmonicReynoldsSolver','variable Aperture is not defined')
      END IF
    END IF

    MaxAperture = MAX(MaxAperture, MAXVAL( ABS(ElemAperture(1:n)) ) )
    MinAperture = MIN(MinAperture, MINVAL( ABS(ElemAperture(1:n)) ) )
    
    DO Mode = 1,NoModes
      ElemAmplitude(1:n) = ListGetReal( Material,'Amplitude',n,NodeIndexes,GotIt)
      IF(.NOT. GotIt) THEN
        IF(AmplitudeExists) THEN
          ElemAmplitude(1:n) = Amplitude(NoAmplitudes*(AmplitudePerm(NodeIndexes(1:n))-1)+Mode)
        ELSE
          CALL Warn('HarmonicReynoldsSolver','variable Amplitude is not defined')
        END IF
      END IF
      MaxAmplitudes(Mode) = MAX(MaxAmplitudes(Mode), MAXVAL( ABS(ElemAmplitude(1:n)) ) )
    END DO
  END DO

!------------------------------------------------------------------------------
! Iterate over any nonlinearity of material or source
!------------------------------------------------------------------------------
  
  CALL Info('HarmonicReynoldsSolver','------------------------------------------------',Level=5)
  CALL Info('HarmonicReynoldsSolver','Solving the time-harmonic squeezed-film pressure',Level=5)
  CALL Info('HarmonicReynoldsSolver','------------------------------------------------',Level=5)

  OldNorm = Norm
  mat_idold = 0
  olditer = -1
  wmin = HUGE(wmin)
  wmax = -HUGE(wmax)

  DO iter=0,NoIterations

    DO Mode = 1,NoModes

      DO Perturbation = 1,NoPerturbations

        AngularVelocity = AngularVelocity0
        IF(iter > 0) THEN
          ScanGeometric = ScanRangeMin * (ScanRangeMax/ScanRangeMin)** &
              ((iter-1.0d0)/(NoIterations-1.0d0))
          IF(ScanFrequency) AngularVelocity = AngularVelocity0 * ScanGeometric
        END IF

        CALL InitializeToZero( StiffMatrix, ForceVector )

!    Do the bulk assembly:
!    ---------------------
 
        DO t=1,Solver % NumberOfActiveElements
          
          CurrentElement => Solver % Mesh % Elements(Solver % ActiveElements(t))
          Model % CurrentElement => CurrentElement
          
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
          
          ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes(1:n))
          ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes(1:n))
          ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes(1:n))
          
!------------------------------------------------------------------------------
!       Get material parameters
!------------------------------------------------------------------------------        
          mat_id = ListGetInteger( Model % Bodies( CurrentElement % &
              Bodyid ) % Values, 'Material', minv=1, maxv=Model % NumberOfMaterials )
          
          Material => Model % Materials(mat_id) % Values
          
          ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes,GotIt)
          IF(.NOT. GotIt .AND. ApertureExists) THEN
            ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n) ))
          ENDIF
          
          ElemAmplitude(1:n) = ListGetReal( Material,'Amplitude',n,NodeIndexes,GotIt)
          IF(.NOT. GotIt .AND. AmplitudeExists) THEN
            ElemAmplitude(1:n) = Amplitude(NoAmplitudes*(AmplitudePerm(NodeIndexes(1:n))-1)+Mode)
          ENDIF
          
          Viscosity(1:n) = ListGetReal( Material, 'Viscosity', n, NodeIndexes)
          
          IF(mat_id /= mat_idold) THEN          
            
            mat_idold = mat_id
            olditer = iter
            
            ReferencePressure = ListGetConstReal( Material,'Reference Pressure',gotIt )
            IF ( .NOT.gotIt ) ReferencePressure = 1.013d5
            
            IF ( Adiabatic ) THEN
              HeatRatio = ListGetConstReal( Material, 'Specific Heat Ratio',gotIt )
              IF ( .NOT. gotIt )  HeatRatio = 5.0d0/3.0d0
              
              ReferenceTemperature = ListGetConstReal( Material,'Reference Tempereture',gotIt )
              IF ( .NOT.gotIt ) ReferenceTemperature = 300.0d0
            END IF
            
            IF(HoleCorrection) THEN            
              ReImpedanceCorrection = ListGetConstReal( Material, &
                  'Re Acoustic Impedance Correction',gotIt )
              IF ( .NOT.gotIt ) ReImpedanceCorrection = 1.0d0

              ImImpedanceCorrection = ListGetConstReal( Material, &
                  'Im Acoustic Impedance Correction',gotIt )
              IF ( .NOT.gotIt ) ImImpedanceCorrection = ReImpedanceCorrection
            END IF

            Incompressible = .FALSE.
            CompressibilityFlag = ListGetString( Material, &
                'Compressibility Model', GotIt)
            IF(CompressibilityFlag == 'incompressible') Incompressible = .TRUE.

          END IF

          HolesExist = .FALSE.
          IF(HoleCorrection) HoleType = ListGetString(Material,'Hole Type',HolesExist)

          IF(EfficientViscosity .OR. HolesExist) THEN
            Density(1:n) = ListGetReal( Material, 'Gas Density',n, NodeIndexes,GotIt) 
            IF(.NOT. GotIt) THEN 
              Density(1:n) = ListGetReal( Material, 'Density',n, NodeIndexes,GotIt)
              IF(.NOT. GotIt) CALL Warn('HarmonicReynoldsSolver','Give Gas Density or Density for gas')
            END IF
          END IF

          IF(HolesExist) THEN
            HoleSize(1:n) = ListGetReal( Material, 'Hole Size', n, NodeIndexes)
            HoleFraction(1:n) = ListGetReal( Material, &
                'Hole Fraction', n, NodeIndexes,minv=0.0d0,maxv=1.0d0)
            HoleDepth(1:n) = ListGetReal( Material, 'Hole Depth', n, NodeIndexes,GotIt)
            IF(.NOT. GotIt) HoleDepth(1:n) = &
                ListGetReal( Material, 'Thickness', n, NodeIndexes,GotIt)
            IF(.NOT. GotIt) CALL Fatal('HarmonicReynoldsSolver','Give Hole Depth (or Thickness)')
            
            DO i=1,n
              Impedance(i) = ComputeHoleImpedance(HoleType, HoleSize(i), &
                  HoleDepth(i), HoleFraction(i), Viscosity(i), &
                  Density(i), ElemAperture(i), AngularVelocity)
              
              Impedance(i) = &
                  DCMPLX(ReImpedanceCorrection*REAL(Impedance(i)), &
                  ImImpedanceCorrection*AIMAG(Impedance(i)))             
            END DO
          END IF

          IF(.NOT. HolesExist) THEN
            ReHoleImpedance(1:n) = ListGetReal( Material, 'Re Specific Acoustic Impedance', &
                n, NodeIndexes, HolesExist )    
            IF(.NOT. HolesExist) THEN
              ReHoleImpedance(1:n) = ListGetReal( Material, 'Specific Fluidic Damping', &
                  n, NodeIndexes, HolesExist )                    
            END IF
            ImHoleImpedance(1:n) = ListGetReal( Material, 'Im Specific Acoustic Impedance', &
                n, NodeIndexes, GotIt )    
            IF(.NOT. GotIt) THEN
              ImHoleImpedance(1:n) = ListGetReal( Material, 'Specific Fluidic Spring', &
                  n, NodeIndexes, GotIt )                    
              ImHoleImpedance(1:n) = ImHoleImpedance(1:n) / AngularVelocity
            END IF
            HolesExist = HolesExist .OR. GotIt
            
            IF(HolesExist) THEN
              Impedance(1:n) = DCMPLX(ReHoleImpedance(1:n), ImHoleImpedance(1:n))
            END IF
          END IF

!          PRINT *,'Impedance',Impedance(1),HolesExist

          IF(EfficientViscosity) THEN
            DO i=1,n
              MeanFreePath = SQRT(PI/ (2.0 * Density(i) * ReferencePressure) ) * Viscosity(i)
              KnudsenNumber = MeanFreePath / ABS(ElemAperture(i))
              ViscCorr = 1.0d0 / (1+9.638*KnudsenNumber**1.159)
              Viscosity(i) = ViscCorr * Viscosity(i)
            END DO
          END IF
          
!------------------------------------------------------------------------------
!       Get element local matrix and rhs vector
!------------------------------------------------------------------------------
          LocalStiffMatrix = 0.0d0
          LocalForce = 0.0d0

!        print *,'iter',iter,'elem',t
!        print *,'AngularVelocity',AngularVelocity,'ReferencePressure',ReferencePressure
!        print *,'ElemAperture',ElemAperture(1:n),'ElemAmplitude',ElemAmplitude(1:n) 
        
          IF (Perturbation > 1) THEN
            Preal(1:n) = Pressure(dofs*PressurePerm(NodeIndexes(1:n))-1)
            Pimag(1:n) = Pressure(dofs*PressurePerm(NodeIndexes(1:n)))
          END IF

          CALL LinearLocalMatrix( LocalStiffMatrix, LocalForce, AngularVelocity, &
              ReferencePressure, ElemAperture, ElemAmplitude, Viscosity, &
              Perturbation, Preal, Pimag, HolesExist, Impedance, &
              CurrentElement, n, ElementNodes )                      

!       print *,'t',t,'Stiff',LocalStiffMatrix(1:1*n,1:1*n)
!       print *,'Force',LocalForce(1:1*n)
        
!------------------------------------------------------------------------------
!       Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
          CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
              ForceVector, LocalForce, n, dofs, PressurePerm(NodeIndexes(1:n)) )
!------------------------------------------------------------------------------
        END DO 
!------------------------------------------------------------------------------


!    Neumann & Newton BCs:
!    ---------------------

!------------------------------------------------------------------------------

        IF(SideCorrection) THEN
          
          DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
              Solver % Mesh % NumberOfBulkElements +  &
              Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
            CurrentElement => Solver % Mesh % Elements(t)
            Model % CurrentElement => CurrentElement

!------------------------------------------------------------------------------
!       The element type 101 (point element) can only be used
!       to set Dirichlet BCs, so skip em at this stage.
!------------------------------------------------------------------------------
            IF ( CurrentElement % TYPE % ElementCode == 101 ) CYCLE

!------------------------------------------------------------------------------
            DO i=1,Model % NumberOfBCs
              IF ( CurrentElement % BoundaryInfo % Constraint /= &
                  Model % BCs(i) % Tag ) CYCLE
              
              stat = ListGetLogical(Model % BCs(i) % Values,'Open Side',gotIt) 
              IF(.NOT. stat) CYCLE
!------------------------------------------------------------------------------
              n = CurrentElement % TYPE % NumberOfNodes
              NodeIndexes => CurrentElement % NodeIndexes
              
              IF ( ANY( PressurePerm(NodeIndexes(1:n)) == 0 ) ) CYCLE
              
              Parent => CurrentELement % BoundaryInfo % Left
              stat = ASSOCIATED( Parent )
              IF ( stat ) stat = stat .AND. ALL(PressurePerm(Parent % NodeIndexes) > 0)
              
              IF(.NOT. stat) THEN
                Parent => CurrentELement % BoundaryInfo % Right
                
                stat = ASSOCIATED( Parent )
                IF ( stat ) stat = stat .AND. ALL(PressurePerm(Parent % NodeIndexes) > 0)
                IF ( .NOT. stat )  CALL Fatal( 'HarmonicReynoldsSolver', &
                    'No proper parent element available for specified boundary' )
              END IF
              
              mat_id = ListGetInteger( Model % Bodies(Parent % BodyId) % Values, &
                  'Material', minv=1, maxv=Model % NumberOfMaterials )
              Material => Model % Materials(mat_id) % Values
              
              ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
              ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
              ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
              
              ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes,GotIt)
              IF(.NOT. GotIt .AND. ApertureExists) THEN
                ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n)))
              ENDIF
              
              Viscosity(1:n) = ListGetReal( Material, 'Viscosity', &
                  n, NodeIndexes)
              
              IF(EfficientViscosity) THEN 
                Density(1:n) = ListGetReal( Material, 'Gas Density', &
                    n, NodeIndexes,GotIt)
                IF(.NOT. GotIt) Density(1:n) = ListGetReal( Material, 'Density', &
                    n, NodeIndexes)
              END IF
              
              DO j=1,n
                Impedance(j) = ComputeSideImpedance(ElemAperture(j), &
                    Viscosity(j), Density(j), AngularVelocity, &
                    EfficientViscosity, ReferencePressure)
              END DO
            
!------------------------------------------------------------------------------
!             Get element local matrix and rhs vector
!------------------------------------------------------------------------------
              CALL LinearLocalBoundary(  LocalStiffMatrix, LocalForce, &
                  Impedance, AngularVelocity, CurrentElement, n, ElementNodes)

!------------------------------------------------------------------------------
!             Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
              CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
                  ForceVector, LocalForce, n, 2,  PressurePerm(NodeIndexes) )
!------------------------------------------------------------------------------
            END DO
!------------------------------------------------------------------------------

          END DO
!-----------------
        END IF

        CALL FinishAssembly( Solver, ForceVector )
      
!    Dirichlet BCs:
!    --------------
        CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
            TRIM(Solver % Variable % Name) // ' 1', 1, dofs, PressurePerm )
        CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
            TRIM(Solver % Variable % Name) // ' 2', 2, dofs, PressurePerm )
        
!    Solve the system. 
!    The pointer is set differently for the pressure and its perturbation.
!    -----------------------------------------------------------------

        IF(Perturbation == 1) THEN
          PressurePoint => Pressure
        ELSE
          PressurePoint => PressureDer
        END IF
        
        CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
            PressurePoint, Norm, dofs, Solver )
        
!------------------------------------------------------------------------------

        CALL ReynoldsLinearForce( Model, NoElements, PressurePoint, &
            Amplitude, PressurePerm, Freal, Fimag, PaveReal, PaveImag) 

        !---------------------------------------
        ! Calculate maximum absolute pressures

        IF ( Perturbation == 1) THEN
          MaxPressures(Mode) = 0.0d0
          DO i = 1, Solver % Mesh % NumberOfNodes
            j = PressurePerm(i) 
            IF (j > 0) THEN
              Pabs =  Pressure(2*j-1)**2 + Pressure(2*j)**2 
              MaxPressures(Mode) = MAX(Pabs, MaxPressures(Mode) )  
            END IF
          END DO
          MaxPressures(Mode) = SQRT(MaxPressures(Mode))
        END IF
      
      END DO

    END DO

    ! The spring constants must be multiplied with power 2
    ! of amplitude. One comes from scaling the source and the other
    ! from integrating over amplituce.        
    DO Mode = 1,NoModes
      Freal(:,Mode,:) = Freal(:,Mode,:) / MaxAmplitudes(Mode)**2.0d0
      Fimag(:,Mode,:) = Fimag(:,Mode,:) / MaxAmplitudes(Mode)**2.0d0
    END DO

    IF(AplacExport) THEN
      IF(AngularVelocity < wmin) THEN
        wmin = AngularVelocity
        DO i = 1,NoModes
          DO j = 1,NoModes
            WorkReal = Freal(1,i,j)**2.0d0 + Fimag(1,i,j)**2.0d0

            cR0(i,j) = ABS(Fimag(1,i,j) / WorkReal) * &
                Viscosity(1) * TotalArea**2.0 * AngularVelocity / MinAperture**3.0

            cL0(i,j) = ABS(Freal(1,i,j) / WorkReal) * &
                ReferencePressure * TotalArea / MinAperture   
        
            cutoff(i,j) = ABS(Fimag(1,i,j) * AngularVelocity / (Freal(1,i,j) ** 2.0d0 * PI))
          END DO
        END DO
      END IF
    END IF


    ! The same info may be echoed, printed to external files and
    ! saved for later usage with a prefix 'res:'

    NoValues = 0
    IF(ScanFrequency) THEN
      CALL AddToSaveList('Frequency',AngularVelocity/(2.0d0*PI),'(1/s)',.TRUE.,.FALSE.)
    END IF

    IF(NoModes > 1) THEN
      DO i = 1,NoModes
        DO j = 1,NoModes
          WRITE(Message,'(A,I1,I1)') 'Re Fluidic spring ',i,j
          CALL AddToSaveList(Message,Freal(1,i,j),'(N/m)')
          WRITE(Message,'(A,I1,I1)') 'Im Fluidic spring ',i,j
          CALL AddToSaveList(Message,Fimag(1,i,j),'(N/m)')
        END DO
      END DO
    ELSE
      CALL AddToSaveList('Re Fluidic spring ',Freal(1,1,1),'(N/m)')
      CALL AddToSaveList('Im Fluidic spring ',Fimag(1,1,1),'(N/m)')
      CALL AddToSaveList('Fluidic Damping',Fimag(1,1,1)/AngularVelocity,'(Ns/m)')
      CALL AddToSaveList('Phase angle',(180.0/PI)*ATAN2(-Fimag(1,1,1),-Freal(1,1,1)),&
          '(deg)',.TRUE.,.FALSE.)
      CALL AddToSaveList('In-phase Pressure Mean',PaveReal(1),'(Pa)',.TRUE.,.FALSE.)
      CALL AddToSaveList('Out-of-phase Pressure Mean',PaveImag(1),'(Pa)',.TRUE.,.FALSE.)

      !------------------
      ! Calculate temperature fluctuations    
      IF ( Adiabatic ) THEN
        MaxTemperature = ReferenceTemperature * &
            ((1.0d0 + MaxPressures(Mode) / ReferencePressure)**(1.0d0 - 1.0d0/HeatRatio) -1.0d0 )
        CALL AddToSaveList('Max Temperature Variation',MaxTemperature,'(K)')
      END IF
    END IF
 

    IF(NoPerturbations >= 2) THEN
      CALL AddToSaveList('Re (d/dw) Spring ',Freal(2,1,1),'(Ns/m)')
      CALL AddToSaveList('Im (d/dw) Spring ',Fimag(2,1,1),'(Ns/m)')
    END IF
    IF(NoPerturbations >= 3) THEN 
      CALL AddToSaveList('Re (d/dD) Spring ',Freal(3,1,1),'(N/m^2)')
      CALL AddToSaveList('Im (d/dD) Spring ',Fimag(3,1,1),'(N/m^2)')
    END IF
    IF(Perturbation >= 4) THEN
      CALL AddToSaveList('Re (d/dP) Spring ',Freal(4,1,1),'(N/m*Pa)')
      CALL AddToSaveList('Im (d/dP) Spring ',Fimag(4,1,1),'(N/m*Pa)')
    END IF


    ! Print the results during the iteration
    IF(iter <= 1 .AND. NoIterations > 1) THEN 
      WRITE(Message,'(A,I3,A)') 'Values after ',iter,' steps' 
      CALL Info('HarmonicReynoldsSolver',Message,Level=5)
      DO t=1,NoValues
        WRITE(Message,'(A,T35,ES15.5)') TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
        CALL Info('HarmonicReynoldsSolver',Message,Level=5)
      END DO
    END IF

    ! If there is a frequecy scan make save the results to an external file
    IF(iter == 1) THEN
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
      CLOSE(10)
      OPEN (10, FILE=Filename)
    END IF

    IF(iter >= 1) THEN
      DO t=1,NoValues
        IF(ValueSaveLocal(t)) WRITE(10,'(ES15.5)',ADVANCE='NO') Values(t)
      END DO
      WRITE(10,'(A)') ' '
    ENDIF

  END DO

  IF(NoIterations > 0) CLOSE(10)

  IF(AplacExport) THEN
    IF(AplacFluidMode == 1) THEN
      CALL ListAddInteger( Model % Simulation, 'aplac: reyno mode', 1)
      DO i=1,NoModes
        DO j=1,NoModes
          WRITE(Message,'(A,I1,I1)') 'aplac: reyno spring re ',i,j
          CALL ListAddConstReal( Model % Simulation, Message, Freal(1,i,j) )
          WRITE(Message,'(A,I1,I1)') 'aplac: reyno spring im ',i,j
          CALL ListAddConstReal( Model % Simulation, Message, Fimag(1,i,j) )
          WRITE(Message,'(A,I1,I1)') 'aplac: reyno damping ',i,j
          CALL ListAddConstReal( Model % Simulation, Message, Fimag(1,i,j) / AngularVelocity )
        END DO
      END DO

    ELSE IF(AplacFluidMode == 2) THEN
      ! Fluidic spring constants fitted to a physical model'
      ! Kr = v^2 A^3 w^2 / P D^5, Ki = v A^2 w / D^3

      CALL ListAddInteger( Model % Simulation, 'aplac: reyno mode', 2)
      CALL ListAddConstReal( Model % Simulation, 'aplac: reyno viscosity',Viscosity(1))
      CALL ListAddConstReal( Model % Simulation, 'aplac: reyno refpres',ReferencePressure)
      CALL ListAddConstReal( Model % Simulation, 'aplac: reyno area',TotalArea)

      DO i=1,NoModes
        DO j=1,NoModes
          WRITE(Message,'(A,I1,I1)') 'aplac: reyno spring corr ',i,j
          CALL ListAddConstReal( Model % Simulation,Message,cL0(i,j))
          WRITE(Message,'(A,I1,I1)') 'aplac: reyno damping corr ',i,j
          CALL ListAddConstReal( Model % Simulation, Message, cR0(i,j))
          CALL ListAddConstReal( Model % Simulation, &
              'aplac: reyno cutoff',MINVAL(cutoff))
        END DO
      END DO

      PRINT *,'cutoff',cutoff

    ELSE
      CALL Warn('HarmonicReynoldsSolver','unknown Aplac export mode')
    END IF
  END IF

  ! Finally write the information anaway  
  IF(NoIterations > 1) WRITE(Message,'(A,I3,A)') 'Values after ',iter,' steps' 
  CALL Info('HarmonicReynoldsSolver',Message,Level=5)
  DO t=1,NoValues
    WRITE(Message,'(A,T35,ES15.5)') TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
    CALL Info('HarmonicReynoldsSolver',Message,Level=5)
  END DO

  DO t=1,NoValues
    IF(ValueSaveRes(t)) CALL ListAddConstReal( Model % Simulation, &
        'res: '//TRIM(ValueNames(t)), Values(t) )
  END DO

  IF ( CalculateDamping )  THEN
    DO i = 1, Solver % Mesh % NumberOfNodes
      j = PressurePerm(i) 
      IF (j > 0) FilmDamping(j) = Pressure(2*j) / AngularVelocity
    END DO
  END IF
  
CONTAINS


  SUBROUTINE AddToSaveList(Name, Value, Unit, savelocal, saveres)

    INTEGER :: n
    CHARACTER(LEN=*) :: Name, Unit
    REAL(KIND=dp) :: Value
    LOGICAL, OPTIONAL :: savelocal,saveres

    n = NoValues
    n = n + 1
    IF(n > MaxNoValues) THEN
      CALL WARN('HarmonicReynoldsSolver','Too little space for the scalars')
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
  SUBROUTINE LinearLocalMatrix( StiffMatrix, Force, AngularVelocity, &
      ReferencePressure, ElemAperture, ElemAmplitude, Viscosity, &
      Perturbation, Preal, Pimag, Holes, Impedance, &
      Element, n, Nodes )

!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), Force(:), &
        AngularVelocity, ReferencePressure, &
        ElemAperture(:), ElemAmplitude(:), Viscosity(:), &
        Preal(:), Pimag(:)
    LOGICAL :: Holes
    COMPLEX(KIND=dp) :: Impedance(:)
    INTEGER :: Perturbation, n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(2*n),dBasisdx(2*n,3),ddBasisddx(n,3,3)
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,M,D,L1,L2
    COMPLEX(KIND=dp) :: LSTIFF(2*n,2*n), LFORCE(2*n), A, H
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,DIM, NBasis, CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    REAL(KIND=dp) :: X,Y,Z,Metric(3,3),SqrtMetric,Symb(3,3,3),dSymb(3,3,3,3)
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()

    Metric = 0.0d0
    Metric(1,1) = 1.0d0
    Metric(2,2) = 1.0d0
    Metric(3,3) = 1.0d0

    LSTIFF = 0.0d0
    LFORCE = 0.0d0
!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------

    NBasis = n
    IntegStuff = GaussPoints( Element )

!------------------------------------------------------------------------------
    DO t=1,IntegStuff % n
      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
      stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
          Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )
      
      s = s * SqrtElementMetric
      IF ( CoordSys /= Cartesian ) THEN
        X = SUM( Nodes % X(1:n) * Basis(1:n) )
        Y = SUM( Nodes % Y(1:n) * Basis(1:n) )
        Z = SUM( Nodes % Z(1:n) * Basis(1:n) )
        CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,X,Y,Z )
        s = s * SqrtMetric
      END IF
!------------------------------------------------------------------------------
!      The source term and the coefficient of the time derivative and 
!      diffusion terms at the integration point
!------------------------------------------------------------------------------

      M = -SUM(Basis(1:n)*ElemAperture(1:n)*ElemAperture(1:n)/ &
          (12.0d0*AngularVelocity*Viscosity(1:n)))
      D = -1.0d0/ReferencePressure
      IF (adiabatic )  D = D / HeatRatio
      
!      Impedance = DCMPLX(REAL(Impedance),0*AIMAG(Impedance))
!      Impedance = DCMPLX(1.0d-3,1.0d-3)

      IF(Holes) THEN 
        H = SUM(Basis(1:n) / (Impedance(1:n)*ElemAperture(1:n)) ) / AngularVelocity
      ELSE
        H = 0.0
      END IF

      ! For consistancy it seems that the real part must have different sign
      H = DCMPLX(-REAL(h),AIMAG(h))

      IF(.NOT. InCompressible) H = H + DCMPLX(0,D)

      IF (Perturbation == 1) THEN
        L1 = 0.0
        L2 = SUM( Basis(1:n) * ElemAmplitude(1:n) / ElemAperture(1:n) )
      ELSE IF (Perturbation == 2) THEN 
        L2 = SUM( Basis(1:n) * ElemAmplitude(1:n) / &
            (AngularVelocity * ElemAperture(1:n)))
        ! Source terms including the calculated pressure
        L1 = - SUM( Basis(1:n) * Pimag(1:n) / &
            (AngularVelocity * ReferencePressure) )
        L2 = L2 + SUM( Basis(1:n) * Preal(1:n) / &
            (AngularVelocity * ReferencePressure) )
      ELSE IF (Perturbation == 3) THEN
        L2 = - SUM( Basis(1:n) * ElemAmplitude(1:n)**2 / &
            (ElemAperture(1:n)**2) ) 
        L1 = SUM(Basis(1:n) * 2 * ElemAmplitude(1:n) * Pimag(1:n) / &
            (ElemAperture(1:n) * ReferencePressure) ) 
        L2 = L2 + SUM (Basis(1:n) * 2 * ElemAmplitude(1:n) * Preal(1:n) / &
            (ElemAperture(1:n) * ReferencePressure) )        
      ELSE IF (Perturbation == 4) THEN
        L1 = SUM(Basis(1:n) * Pimag(1:n) / &
            (ReferencePressure ** 2) ) 
        L2 = - SUM (Basis(1:n) * Preal(1:n) / &
            (ReferencePressure ** 2) )        
      END IF

!      PRINT *,'L1',L1,'L2',L2,'H',H,'D',D,'M',M
!------------------------------------------------------------------------------
!      The Reynolds equation
!------------------------------------------------------------------------------
      DO p=1,NBasis
        DO q=1,NBasis
          A = H * Basis(q) * Basis(p) 
          
          DO i=1,DIM
            DO j=1,DIM
              A = A + M * Metric(i,j) * dBasisdx(q,i) * dBasisdx(p,j)
            END DO
          END DO
          
          LSTIFF(p,q) = LSTIFF(p,q) + s*A
        END DO
        LFORCE(p) = LFORCE(p) + s * Basis(p) * DCMPLX( L1,L2 )
      END DO
    END DO
!------------------------------------------------------------------------------

    DO i=1,n
      Force( 2*i-1 ) = REAL( LFORCE(i) )
      Force( 2*i   ) = AIMAG( LFORCE(i) )
      DO j=1,n
        StiffMatrix( 2*i-1, 2*j-1 ) =  REAL( LSTIFF(i,j) )
        StiffMatrix( 2*i-1, 2*j   ) = -AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*i,   2*j-1 ) =  AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*i,   2*j   ) =  REAL( LSTIFF(i,j) )
      END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LinearLocalMatrix
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LinearLocalBoundary(  StiffMatrix, Force, Impedance, AngularFrequency, &
      Element, n, Nodes)
!------------------------------------------------------------------------------
    COMPLEX(KIND=dp) :: Impedance(:)
    REAL(KIND=dp) :: StiffMatrix(:,:),Force(:)
    REAL(KIND=dp) :: AngularFrequency
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,L1,L2
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3),X,Y,Z
    COMPLEX(KIND=dp) :: LSTIFF(n,n), H
    INTEGER :: i,p,q,t,DIM,CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()

    LSTIFF = 0.0d0 

!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------
    IntegStuff = GaussPoints( Element )
    DO t=1,IntegStuff % n
       U = IntegStuff % u(t)
       V = IntegStuff % v(t)
       W = IntegStuff % w(t)
       S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
              Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )

       s = s * SqrtElementMetric

       H = -1.0 / (AngularFrequency * SUM( Impedance(1:n) * Basis(1:n) ) )

!------------------------------------------------------------------------------
       DO p=1,n
         DO q=1,n
           LSTIFF(p,q) = LSTIFF(p,q) + s * Basis(q) * Basis(p) * H
         END DO
       END DO
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------
    DO i=1,n
      DO j=1,n
        StiffMatrix( 2*i-1, 2*j-1 ) =  REAL( LSTIFF(i,j) )
        StiffMatrix( 2*i-1, 2*j   ) = -AIMAG( LSTIFF(i,j) ) 
        StiffMatrix( 2*i,   2*j-1 ) =  AIMAG( LSTIFF(i,j) ) 
        StiffMatrix( 2*i,   2*j   ) =  REAL( LSTIFF(i,j) )
      END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LinearLocalBoundary
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE ReynoldsLinearForce( Model, NoElements, IntegrandFunction, &
      Amplitude, Reorder, Freal, Fimag, PaveReal, PaveImag) 
    !DLLEXPORT ReynoldsLinearForce
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: NoElements
    REAL(KIND=dp), POINTER :: Amplitude(:)
    REAL(KIND=dp) :: IntegrandFunction(:), PaveReal(:), PaveImag(:), Freal(:,:,:), Fimag(:,:,:)
    INTEGER :: Reorder(:)
!------------------------------------------------------------------------------
    INTEGER :: k, n, No
    TYPE(Element_t), POINTER :: CurrentElement
    INTEGER, POINTER :: NodeIndexes(:)
    TYPE(Nodes_t)   :: ElementNodes
    
    TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
    REAL(KIND=dp), DIMENSION(:), POINTER :: &
        U_Integ,V_Integ,W_Integ,S_Integ
    
    TYPE(ValueList_t), POINTER :: Material
    REAL(KIND=dp), ALLOCATABLE :: Preal(:), Pimag(:), ElementAmplitude(:,:)
    REAL(KIND=dp) :: s,ug,vg,wg
    REAL(KIND=dp) :: ddBasisddx(Model % MaxElementNodes,3,3)
    REAL(KIND=dp) :: Basis(Model % MaxElementNodes)
    REAL(KIND=dp) :: dBasisdx(Model % MaxElementNodes,3),SqrtElementMetric
    REAL(KIND=dp) :: PrealAtGP, PimagAtGP, dV
    INTEGER :: N_Integ, t, tg, i
    LOGICAL :: stat, gotIt, AllocationsDone=.FALSE.
    
    SAVE AllocationsDone, ElementNodes, ElementAmplitude, Preal, Pimag

! Need MaxElementNodes only in allocation
    
    IF(.NOT. AllocationsDone) THEN
      n = Model % MaxElementNodes
      ALLOCATE( ElementNodes % x( n ), ElementNodes % y( n ), ElementNodes % z( n ), &
          ElementAmplitude(NoAmplitudes,n), Preal(n), Pimag(n) )
      AllocationsDone = .TRUE.
    END IF
          
    TotalArea = 0.0d0
    PaveReal(Mode) = 0.0d0
    PaveImag(Mode) = 0.0d0
    Freal(Perturbation, Mode,:) = 0.0d0
    Fimag(Perturbation, Mode,:) = 0.0d0
    
! Loop over all elements in the list

    DO t=1,Solver % NumberOfActiveElements
      
      CurrentElement => Solver % Mesh % Elements(Solver % ActiveElements(t))
      Model % CurrentElement => CurrentElement      
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes
      
!------------------------------------------------------------------------------
! Get element nodal coordinates
!------------------------------------------------------------------------------
      ElementNodes % x(1:n) = Model % Nodes % x(NodeIndexes(1:n))
      ElementNodes % y(1:n) = Model % Nodes % y(NodeIndexes(1:n))
      ElementNodes % z(1:n) = Model % Nodes % z(NodeIndexes(1:n))
      
      k = ListGetInteger( Model % Bodies( CurrentElement % &
          Bodyid ) % Values, 'Material', minv=1, maxv=Model % NumberOFMaterials )
      Material => Model % Materials(k) % Values
      
      Preal(1:n) = IntegrandFunction(dofs*Reorder(NodeIndexes(1:n))-1)
      Pimag(1:n) = IntegrandFunction(dofs*Reorder(NodeIndexes(1:n)))
      
      IF(AmplitudeExists) THEN
        DO j = 1, NoAmplitudes
          ElementAmplitude(j,1:n) = &
              Amplitude(NoAmplitudes*(AmplitudePerm( NodeIndexes(1:n) ) -1) + j)
        END DO
      ELSE
        ElementAmplitude(1,1:n) = ListGetReal( Material, &
            'Amplitude',n,NodeIndexes,gotIt)
        IF(.NOT. gotIt) ElementAmplitude(1,1:n) = 1.0d0
      ENDIF

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
               
! Use general coordinate system for dV
        dV = CoordinateSqrtMetric( SUM( ElementNodes % x(1:n) * Basis(1:n)), &
            SUM( ElementNodes % y(1:n) * Basis(1:n) ), &
            SUM( ElementNodes % z(1:n) * Basis(1:n) ) )
        TotalArea = TotalArea + s * dV
        
        IF(Perturbation == 1) THEN
          PaveReal(Mode) = PaveReal(Mode) + s * SUM( Preal(1:n) * Basis(1:n))
          PaveImag(Mode) = PaveImag(Mode) + s * SUM( Pimag(1:n) * Basis(1:n))
        END IF
        
        ! Compute the lumped force acting on different modes    
        DO j = 1, NoAmplitudes
          PrealAtGP = SUM( Preal(1:n) * ElementAmplitude(j,1:n) * Basis(1:n) )
          PimagAtGP = SUM( Pimag(1:n) * ElementAmplitude(j,1:n) * Basis(1:n) )
          Freal(Perturbation,Mode,j) = Freal(Perturbation,Mode,j) + s*PrealAtGP*dV
          Fimag(Perturbation,Mode,j) = Fimag(Perturbation,Mode,j) + s*PimagAtGP*dV
        END DO
      END DO
  
    END DO
    
    PaveReal(Mode) = PaveReal(Mode) / TotalArea
    PaveImag(Mode) = PaveImag(Mode) / TotalArea

!------------------------------------------------------------------------------
  END SUBROUTINE ReynoldsLinearForce
!------------------------------------------------------------------------------
 
!------------------------------------------------------------------------------
END SUBROUTINE HarmonicReynoldsSolver
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Solves the transient version of the Reynolds equation
!------------------------------------------------------------------------------
SUBROUTINE TransientReynoldsSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: ReynoldsEquation
!------------------------------------------------------------------------------

  USE ReynoldsUtilities
  USE Types
  USE Lists
  USE Integration
  USE ElementDescription
  USE SolverUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement, Parent
  TYPE(Solver_t), POINTER :: PSolver 
  TYPE(ValueList_t), POINTER :: Material
  TYPE(Variable_t), POINTER :: AmplitudeVar, ApertureVar, ApertureVelocityVar, DampingVar

  INTEGER :: iter, i, j, k, l, n, t, istat, mat_id, mat_idold, &
      NonlinearIter, olditer, NoIterations, NoElements, NoNodes, &
      LimitDamping, PrevDoneTime=-1, TimeStepVisited=0
  INTEGER, POINTER :: NodeIndexes(:), PressurePerm(:), AmplitudePerm(:), AperturePerm(:)

  LOGICAL :: AllocationsDone = .FALSE., GotIt, &
      stat, EfficientViscosity, Adiabatic, CalculateDamping, Incompressible, &
      SubroutineVisited = .FALSE., HoleCorrection, SideCorrection, HolesExist, &
      ApertureExists, ApertureVelocityExists, AmplitudeExists

  REAL(KIND=dp), POINTER :: Pressure(:), &
      ForceVector(:), Aperture(:), Amplitude(:), ApertureVelocity(:), &
      PrevPressure(:,:), FilmDamping(:), PrevFilmPressure(:)
  REAL(KIND=dp) :: Norm, PrevNorm, RelativeChange, AngularVelocity, &
      Frequency, ReferenceTemperature, ReferencePressure, &
      MeanFreePath, KnudsenNumber, MaxAmplitude, MaxPressure, &
      ElasticMass, ElasticSpring, ReImpedanceCorrection, ImImpedanceCorrection, &
      MaxAperture, MinAperture, NonlinearTol, s, LumpedForce, AngularVelocity0, &
      WorkReal, Pave=0.0, MaxApertureVelocity, DampFlux1, DampFlux2, &
      DampConst, HeatRatio, MaxTemperature, ViscCorr, OldNorm, &
      RelaxDamping, TotalArea, Pres, Dens
  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), &
      LocalMassMatrix(:,:), LocalForce(:), Viscosity(:), ElemPressure(:), Work(:), &
      ElemAperture(:), ElemAmplitude(:), HoleFraction(:), &
      HoleSize(:), HoleDepth(:), Density(:), ImHoleImpedance(:), ReHoleImpedance(:)
  COMPLEX(KIND=dp), ALLOCATABLE :: Impedance(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, HoleType, CompressibilityFlag

  ! These variables are for efficient manipulation of local scalars data
  INTEGER, PARAMETER :: MaxNoValues = 100
  INTEGER :: NoValues
  REAL (KIND=dp) ::  Values(MaxNoValues) 
  CHARACTER(LEN=MAX_NAME_LEN) :: ValueNames(MaxNoValues), ValueUnits(MaxNoValues)
  LOGICAL :: ValueSaveLocal(MaxNoValues),ValueSaveRes(MaxNoValues)
  

  SAVE LocalMassMatrix, LocalStiffMatrix, Work, LocalForce, ElementNodes, &
       Amplitude, Aperture, ApertureVelocity, ElemAperture, ElemAmplitude, Viscosity, &
       Density, AllocationsDone, ElemPressure, CalculateDamping, DampConst, &
       HoleFraction, HoleSize, HoleDepth, Impedance, PrevFilmPressure, &
       ImHoleImpedance, ReHoleImpedance, FilmDamping, PrevPressure, SubroutineVisited, &
       TimeStepVisited, PrevDoneTime, Pave

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------

  i = ListGetInteger( Solver % Values,'Activate Solver',GotIt)
  IF(GotIt .AND. Solver % DoneTime < i) RETURN

  i = ListGetInteger( Solver % Values,'DeActivate Solver',GotIt)
  IF(GotIt .AND. Solver % DoneTime > i) RETURN

  IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

  IF(Solver % Variable % DOFs /= 1) THEN
    CALL Fatal('TransientReynoldsSolver','Transient Simulation requires one dof!')
  END IF

  EquationName = ListGetString( Solver % Values, 'Equation' )
  Pressure     => Solver % Variable % Values
  PressurePerm => Solver % Variable % Perm

  IF( COUNT( PressurePerm > 0 ) <= 0) RETURN

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS
  Norm = Solver % Variable % Norm
  NoElements =  Solver % Mesh % NumberOfBulkElements
  NoNodes = Model % NumberOfNodes

!------------------------------------------------------------------------------
! Set pointers to already allocated variables 
!------------------------------------------------------------------------------

  NULLIFY(AmplitudeVar)
  AmplitudeVar => VariableGet( Model % Variables, 'Amplitude' )
  IF(ASSOCIATED(AmplitudeVar)) THEN
    AmplitudePerm  => AmplitudeVar % Perm
    Amplitude => AmplitudeVar % Values
    AmplitudeExists = .TRUE.
  ELSE
    AmplitudeExists = .FALSE.
  ENDIF

  NULLIFY(ApertureVar)
  ApertureVar => VariableGet( Model % Variables, 'Aperture' )
  IF(ASSOCIATED (ApertureVar)) THEN
    AperturePerm  => ApertureVar % Perm
    Aperture => ApertureVar % Values
    ApertureExists = .TRUE.
  ELSE
    ApertureExists = .FALSE.
  ENDIF

  NULLIFY(ApertureVelocityVar)
  ApertureVelocityVar => VariableGet( Model % Variables, 'Aperture Velocity' )
  IF(ASSOCIATED (ApertureVelocityVar)) THEN
    ApertureVelocity => ApertureVelocityVar % Values
    ApertureVelocityExists = .TRUE. 
  ELSE
    ApertureVelocityExists = .FALSE. 
  ENDIF

!------------------------------------------------------------------------------
! Do some initial stuff
!------------------------------------------------------------------------------

  IF (TransientSimulation) THEN
    Solver % Matrix % COMPLEX = .FALSE.
    PrevPressure => Solver % Variable % PrevValues    
  ELSE 
    Solver % Matrix % COMPLEX = .FALSE.
  END IF

  HoleCorrection = ListGetLogical( Solver % Values, 'Hole Correction',gotIt )
  EfficientViscosity = ListGetLogical( Solver % Values, 'Rarefaction',gotIt )
  Adiabatic = ListGetLogical( Solver % Values, 'Adiabatic', gotIt )

  SideCorrection = ListGetLogical( Solver % Values, 'Side Correction',gotIt )
  IF(.NOT. GotIt) THEN
    DO i=1,Model % NumberOfBCs
      stat = ListGetLogical(Model % BCs(i) % Values,'Open Side',gotIt) 
      IF(stat) SideCorrection = .TRUE.
    END DO
  END IF
   
  Frequency = ListGetConstReal( Solver % Values, 'Frequency', gotIt)
  IF(.NOT. GotIt) Frequency = ListGetConstReal(&
      Model % Simulation,'res: Eigen Frequency',gotIt )
  IF(.NOT. GotIt) Frequency = ListGetConstReal(&
      Model % Simulation,'res: Eigen Frequency 1',gotIt )    

  IF(HoleCorrection .AND. .NOT. GotIt) THEN
    CALL Fatal('TransientReynoldsSolver','Hole impedance models require Frequency')
  END IF
  AngularVelocity0 = 2.0d0 * PI * Frequency

  NoIterations = 0
  NoIterations = ListGetInteger( Solver % Values,&
      'Nonlinear System Max Iterations',GotIt,minv=1)
  IF (.NOT.GotIt ) NoIterations = 20
    
  NonlinearTol = ListGetConstReal( Solver % Values, &
      'Nonlinear System Convergence Tolerance',gotIt)
  IF(.NOT. gotIt) NonlinearTol = 1.0d-5
  
  IF(PrevDoneTime /= Solver % DoneTime) THEN
    TimeStepVisited = 0
    PrevDoneTime = Solver % DoneTime
  END IF
  TimeStepVisited = TimeStepVisited + 1

!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
    N = Solver % Mesh % MaxElementNodes
    
    IF ( AllocationsDone ) THEN
      DEALLOCATE( ElementNodes % x, &
          ElementNodes % y,      &
          ElementNodes % z,      &
          LocalForce,            &
          LocalStiffMatrix,      &
          LocalMassMatrix,      &
          Work,                  &
          Viscosity,             &
          Density,            &
          HoleFraction,       &
          HoleSize,         &
          HoleDepth,         &
          Impedance,         &
          ImHoleImpedance,         &
          ReHoleImpedance,         &
          ElemPressure)
    END IF

    ALLOCATE(ElementNodes % x( N ),  &
        ElementNodes % y( N ),       &
        ElementNodes % z( N ),       &
        Work(N),                     &
        Viscosity( N ),              &
        ElemAperture(N),          &
        ElemAmplitude(N),         &
        Density(N),               &
        HoleFraction(N),         &
        HoleSize(N),         &
        HoleDepth(N),         &
        Impedance(N),         &
        ImHoleImpedance(N),         &
        ReHoleImpedance(N),         &
        LocalForce( N ),           &
        LocalStiffMatrix( N,N ), &
        LocalMassMatrix( N,N ), &
        ElemPressure(N), &
        STAT=istat )

    IF ( istat /= 0 ) CALL FATAL('TransientReynoldsSolver','Memory allocation error')
    
    NULLIFY(DampingVar)
    DampingVar => VariableGet( Model % Variables, 'FilmDamping' )

    IF(ASSOCIATED (DampingVar)) THEN
      CalculateDamping = .TRUE.
      FilmDamping => DampingVar % Values
    ELSE
      CalculateDamping = ListGetLogical( Solver % Values,'Calculate Damping', GotIt )
      IF ( CalculateDamping )  THEN
        ALLOCATE( FilmDamping( Model%NumberOfNodes ), STAT=istat )
        IF ( istat /= 0 ) THEN
          CALL FATAL('TransientReynoldsSolver','Memory allocation error')
        ELSE
          PSolver => Solver
          FilmDamping = 0.0d0
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
              PSolver, 'FilmDamping', 1, FilmDamping, PressurePerm )
        END IF
      END IF
    END IF

    IF(CalculateDamping) THEN
      NULLIFY(DampingVar)
      DampingVar => VariableGet( Model % Variables, 'PrevFilmPressure' )

      IF(ASSOCIATED (DampingVar)) THEN
        PrevFilmPressure => DampingVar % Values
      ELSE
        CALL Warn('ReynoldsEquation','Variable PrevFilmPressure should exist')
      END IF
    END IF

    AllocationsDone = .TRUE.
  END IF 

  IF(CalculateDamping) THEN
    LimitDamping = ListGetInteger(Solver % Values, 'Damping Limit Iterations',gotIt )
    RelaxDamping = ListGetConstReal(Solver % Values, 'Damping Limit Relaxation',gotIt )
    IF(.NOT. GotIt) RelaxDamping = 1.0d0
  END IF
    

!------------------------------------------------------------------------------
! Iterate over any nonlinearity of material or source
!------------------------------------------------------------------------------
  
  CALL Info('TransientReynoldsSolver','--------------------------------',Level=5)
  IF(TransientSimulation) THEN
    CALL Info('TransientReynoldsSolver','Solving in transient mode',Level=5)
  ELSE
    CALL Info('TransientReynoldsSolver','Solving in non-linear static mode',Level=5)    
  END IF
  CALL Info('TransientReynoldsSolver','--------------------------------',Level=5)

  NoValues = 0
  OldNorm = Norm
  mat_idold = 0
  olditer = -1

  DO iter=0,NoIterations

    AngularVelocity = AngularVelocity0
    
    MaxAperture = 0.0d0
    MinAperture = HUGE(MinAperture)
    MaxAmplitude = 0.0d0
    
    CALL InitializeToZero( StiffMatrix, ForceVector )
    
!    Do the bulk assembly:
!    ---------------------

    DO t=1,Solver % NumberOfActiveElements
      
      CurrentElement => Solver % Mesh % Elements(Solver % ActiveElements(t))
      Model % CurrentElement => CurrentElement
      
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes
      
      ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes(1:n))
      ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes(1:n))
      ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes(1:n))
      
!------------------------------------------------------------------------------
!       Get material parameters
!------------------------------------------------------------------------------        
      mat_id = ListGetInteger( Model % Bodies( CurrentElement % &
          Bodyid ) % Values, 'Material', minv=1, maxv=Model % NumberOfMaterials )
      
      Material => Model % Materials(mat_id) % Values
      
      IF(ApertureExists) THEN
        ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n) ))
      ELSE
        ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes)
      ENDIF
      
      IF(ApertureVelocityExists) THEN
        ElemAmplitude(1:n) = ApertureVelocity(AperturePerm(NodeIndexes(1:n) ))
      ELSE
        ElemAmplitude(1:n) = ListGetReal(Material,'Aperture Velocity',n,NodeIndexes)
      END IF
      
      IF(TimeStepVisited > 1) THEN
        ElemAperture(1:n) = ElemAperture(1:n) - 0.5 * ElemAmplitude(1:n) * dt
      END IF
      
      MaxAmplitude = MAX(MaxAmplitude, MAXVAL( ABS(ElemAmplitude(1:n)) ) )
      MaxAperture = MAX(MaxAperture, MAXVAL( ABS(ElemAperture(1:n)) ) )
      MinAperture = MIN(MinAperture, MINVAL( ABS(ElemAperture(1:n)) ) )
      
      Viscosity(1:n) = ListGetReal( Material, 'Viscosity', n, NodeIndexes)
      
      IF(mat_id /= mat_idold) THEN          
        
        mat_idold = mat_id
        olditer = iter
        
        ReferencePressure = ListGetConstReal( Material,'Reference Pressure',gotIt )
        IF ( .NOT.gotIt ) ReferencePressure = 1.013d5
        
        IF ( Adiabatic ) THEN
          HeatRatio = ListGetConstReal( Material, 'Specific Heat Ratio', &
              gotIt )
          IF ( .NOT. gotIt )  HeatRatio = 5.0d0 / 3.0d0
          
          ReferenceTemperature = ListGetConstReal( Material, & 
              'Reference Tempereture',gotIt )
          IF ( .NOT.gotIt ) ReferenceTemperature = 300.0d0
        END IF
        
        IF(HoleCorrection) THEN            
          ReImpedanceCorrection = ListGetConstReal( Material, &
              'Re Acoustic Impedance Correction',gotIt )
          IF ( .NOT.gotIt ) ReImpedanceCorrection = 1.0d0
          
          ImImpedanceCorrection = ListGetConstReal( Material, &
              'Im Acoustic Impedance Correction',gotIt )
          IF ( .NOT.gotIt ) ImImpedanceCorrection = ReImpedanceCorrection
        END IF

        Incompressible = .FALSE.
        CompressibilityFlag = ListGetString( Material, &
            'Compressibility Model', GotIt)
        IF(CompressibilityFlag == 'incompressible') Incompressible = .TRUE.

      END IF
      
      IF(EfficientViscosity .OR. HoleCorrection) THEN
        Density(1:n) = ListGetReal( Material, 'Gas Density',n, NodeIndexes,GotIt) 
        IF(.NOT. GotIt) THEN 
          Density(1:n) = ListGetReal( Material, 'Density',n, NodeIndexes,GotIt)
          IF(.NOT. GotIt) CALL Warn('TransientReynoldsSolver','Density required for the gas')
        END IF
      END IF
          
      HolesExist = .FALSE.

      IF(HoleCorrection) THEN
        HoleType = ListGetString(Material,'Hole Type',HolesExist)
        IF(HolesExist) THEN
          HoleSize(1:n) = ListGetReal( Material, 'Hole Size', n, NodeIndexes)
          HoleFraction(1:n) = ListGetReal( Material, &
              'Hole Fraction', n, NodeIndexes,minv=0.0d0,maxv=1.0d0)
          HoleDepth(1:n) = ListGetReal( Material, 'Hole Depth', n, NodeIndexes,GotIt)
          IF(.NOT. GotIt) HoleDepth(1:n) = &
              ListGetReal( Material, 'Thickness', n, NodeIndexes,GotIt)
          IF(.NOT. GotIt) CALL Fatal('TransientReynoldsSolver','Hole Depth (or Thickness) shold be given')
          
          DO i=1,n
            Impedance(i) = ComputeHoleImpedance(HoleType, HoleSize(i), &
                HoleDepth(i), HoleFraction(i), Viscosity(i), &
                Density(i), ElemAperture(i), AngularVelocity)            
            Impedance(i) = &
                DCMPLX(ReImpedanceCorrection*REAL(Impedance(i)), &
                ImImpedanceCorrection*AIMAG(Impedance(i)))             
          END DO
        END IF
      END IF

        
      IF(.NOT. HolesExist) THEN 
        ReHoleImpedance(1:n) = ListGetReal( Material, 'Re Specific Acoustic Impedance', &
            n, NodeIndexes, HolesExist )    
        IF(.NOT. HolesExist) THEN
          ReHoleImpedance(1:n) = ListGetReal( Material, 'Specific Fluidic Damping', &
              n, NodeIndexes, HolesExist )                    
        END IF

        ImHoleImpedance(1:n) = ListGetReal( Material, 'Im Specific Acoustic Impedance', &
            n, NodeIndexes, GotIt )    
        IF(.NOT. GotIt) THEN
          ImHoleImpedance(1:n) = ListGetReal( Material, 'Specific Fluidic Spring', &
              n, NodeIndexes, GotIt )                    
          IF(GotIt) ImHoleImpedance(1:n) = ImHoleImpedance(1:n) / AngularVelocity
        END IF

        HolesExist = HolesExist .OR. GotIt        

        IF ( HolesExist ) Impedance(1:n) = DCMPLX(ReHoleImpedance(1:n),ImHoleImpedance(1:n))

      END IF
      
!      PRINT *,'Impedance',Impedance(1),HolesExist

!-----------------------------------------------------------------------------
!  Note the dummy way to take care of the changing density in following 
!-----------------------------------------------------------------------------

      IF(EfficientViscosity) THEN
        DO i=1,n
          Pres = ReferencePressure + Pressure(PressurePerm(NodeIndexes(i)))
          Dens = Density(i) * Pres / 101325.0d0
          MeanFreePath = SQRT(PI/ (2.0 * Dens * Pres ) ) * Viscosity(i)
          KnudsenNumber = MeanFreePath / ABS(ElemAperture(i))
          ViscCorr = 1.0d0 / (1+9.638*KnudsenNumber**1.159)
          Viscosity(i) = ViscCorr * Viscosity(i)
        END DO
      END IF
          
!------------------------------------------------------------------------------
!       Get element local matrix and rhs vector
!------------------------------------------------------------------------------
      LocalStiffMatrix = 0.0d0
      LocalForce = 0.0d0

!        print *,'iter',iter,'elem',t
!        print *,'AngularVelocity',AngularVelocity,'ReferencePressure',ReferencePressure
!        print *,'ElemAperture',ElemAperture(1:n),'ElemAmplitude',ElemAmplitude(1:n) 
        
      ElemPressure(1:n) = Pressure(PressurePerm(NodeIndexes(1:n)))
      IF(TimeStepVisited > 1) THEN
        ElemPressure(1:n) = 0.5d0 * (ElemPressure(1:n) + PrevPressure( PressurePerm(NodeIndexes(1:n)), 1) )
      END IF


      CALL NonlinearLocalMatrix(   LocalMassMatrix, LocalStiffMatrix, LocalForce, &
          ElemAperture, ElemAmplitude, Viscosity, ElemPressure, &
          ReferencePressure, HolesExist, Impedance, &
          HeatRatio, Adiabatic, CurrentElement, n, ElementNodes)          
      
      IF(TransientSimulation) THEN
        CALL Add1stOrderTime( LocalMassMatrix, LocalStiffMatrix, &
            LocalForce,dt,n,1,PressurePerm(NodeIndexes(1:n)),Solver )
      END IF

!       print *,'t',t,'Stiff',LocalStiffMatrix(1:1*n,1:1*n)
!       print *,'Force',LocalForce(1:1*n)
        
!------------------------------------------------------------------------------
!       Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
      CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
          ForceVector, LocalForce, n, 1, PressurePerm(NodeIndexes(1:n)) )
!------------------------------------------------------------------------------
    END DO 
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!    Neumann & Newton BCs:
!------------------------------------------------------------------------------

    IF(SideCorrection) THEN
          
      DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
          Solver % Mesh % NumberOfBulkElements +  &
          Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
        CurrentElement => Solver % Mesh % Elements(t)
        Model % CurrentElement => CurrentElement

!------------------------------------------------------------------------------
!       The element type 101 (point element) can only be used
!       to set Dirichlet BCs, so skip em at this stage.
!------------------------------------------------------------------------------
        IF ( CurrentElement % TYPE % ElementCode == 101 ) CYCLE

!------------------------------------------------------------------------------
        DO i=1,Model % NumberOfBCs
          IF ( CurrentElement % BoundaryInfo % Constraint /= &
              Model % BCs(i) % Tag ) CYCLE
          
          stat = ListGetLogical(Model % BCs(i) % Values,'Open Side',gotIt) 
          IF(.NOT. stat) CYCLE
!------------------------------------------------------------------------------
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
          
          IF ( ANY( PressurePerm(NodeIndexes(1:n)) == 0 ) ) CYCLE
          
          Parent => CurrentELement % BoundaryInfo % Left
          stat = ASSOCIATED( Parent )
          IF ( stat ) stat = stat .AND. ALL(PressurePerm(Parent % NodeIndexes) > 0)
          
          IF(.NOT. stat) THEN
            Parent => CurrentELement % BoundaryInfo % Right
            
            stat = ASSOCIATED( Parent )
            IF ( stat ) stat = stat .AND. ALL(PressurePerm(Parent % NodeIndexes) > 0)
            IF ( .NOT. stat )  CALL Fatal( 'TransientReynoldsSolver', &
                'No proper parent element available for specified boundary' )
          END IF
          
          mat_id = ListGetInteger( Model % Bodies(Parent % BodyId) % Values, &
              'Material', minv=1, maxv=Model % NumberOfMaterials )
          Material => Model % Materials(mat_id) % Values
          
          ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
          ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
          ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
          
          IF(ASSOCIATED (Aperture)) THEN
            ElemAperture(1:n) = Aperture(AperturePerm(NodeIndexes(1:n) ))
          ELSE
            ElemAperture(1:n) = ListGetReal(Material,'Aperture',n,NodeIndexes)
          ENDIF
          
          Viscosity(1:n) = ListGetReal( Material, 'Viscosity', &
              n, NodeIndexes)
          
          IF(EfficientViscosity) THEN 
            Density(1:n) = ListGetReal( Material, 'Gas Density', &
                n, NodeIndexes,GotIt)
            IF(.NOT. GotIt) Density(1:n) = ListGetReal( Material, 'Density', &
                n, NodeIndexes)
          END IF
              
          DO j=1,n
            Impedance(j) = ComputeSideImpedance(ElemAperture(j), &
                Viscosity(j), Density(j), AngularVelocity, &
                EfficientViscosity, ReferencePressure)
          END DO
            
!------------------------------------------------------------------------------
!             Get element local matrix and rhs vector
!------------------------------------------------------------------------------

          CALL NonlinearLocalBoundary( LocalMassMatrix, LocalStiffMatrix, LocalForce, &
              Impedance, Pressure, ReferencePressure, ElemAperture, &
              CurrentElement, n, ElementNodes)
          
!------------------------------------------------------------------------------
!             Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
          CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
              ForceVector, LocalForce, n, 1,  PressurePerm(NodeIndexes) )
!------------------------------------------------------------------------------
        END DO 
!------------------------------------------------------------------------------

      END DO
!-----------------
    END IF

    CALL FinishAssembly( Solver, ForceVector )
      
!    Dirichlet BCs:
!    --------------
    CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
        TRIM(Solver % Variable % Name), 1, 1, PressurePerm )

!    Solve the system and we are done:
!    ---------------------------------

    PrevNorm = Norm
    CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
        Pressure, Norm, 1, Solver )
        
    IF ( PrevNorm + Norm /= 0.0d0 ) THEN
      RelativeChange = 2.0d0 * ABS(PrevNorm - Norm) / (PrevNorm + Norm)
    ELSE
      RelativeChange = 0.0d0
    END IF

    WRITE(Message,'(A,T35,E15.5)') 'Result Norm:',Norm
    CALL Info('TransientReynoldsSolver',Message,Level=5)
    
    WRITE(Message,'(A,T35,E15.5)') 'Relative Change:',RelativeChange
    CALL Info('TransientReynoldsSolver',Message,Level=5)
    
    IF (RelativeChange < NonlinearTol) EXIT
  END DO


  IF(NoIterations > 1) WRITE(Message,'(A,I3,A)') 'Values after ',iter,' steps' 
  CALL Info('TransientReynoldsSolver',Message,Level=5)
  DO t=1,NoValues
    WRITE(Message,'(A,T35,ES15.5)') TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
    CALL Info('TransientReynoldsSolver',Message,Level=5)
  END DO
  

  CALL ReynoldsNonlinearForce( Model, NoElements, Pressure, &
      Aperture, Amplitude, PressurePerm, LumpedForce, Pave) 
  
  IF(ABS(MaxAmplitude) > 1.0e-20) THEN
    LumpedForce = LumpedForce / MaxAmplitude
  END IF
  
!------------------------------------------------------------------------------
!   In explicit solution the pressure may sometimes be larger than the force 
!   causing it. In order to enable solution the pressure may therefore be 
!   linearized and used as a damping in the elastic solvers. 
!------------------------------------------------------------------------------
  IF(CalculateDamping) THEN
    

    ! The limiter defines a constant coefficient that is used to multiply the 
    ! suggested damping. 

    IF(LimitDamping == 0) THEN
      DampConst = 1.0
    ELSE IF(TimeStepVisited <= LimitDamping) THEN
      IF(DampFlux1 > DampFlux2 * RelaxDamping) THEN
        DampConst = 1.0
      ELSE
        DampConst = DampFlux1 / (DampFlux2 * RelaxDamping)
      END IF
    END IF

    IF(LimitDamping > 0) PRINT *,'Damping Limiter',TimeStepVisited,dt,DampConst,MinAperture

    IF(TimeStepVisited <= MAX(1,LimitDamping) ) THEN
      DO i=1,Model%NumberOfNodes
        IF ( PressurePerm(i) > 0 ) THEN
          FilmDamping(PressurePerm(i)) = DampConst * ReferencePressure * dt / Aperture(AperturePerm(i))
        END IF
      END DO
    END IF

    DO i=1,Model%NumberOfNodes
      IF ( PressurePerm(i) > 0 ) THEN
        IF(.NOT. SubroutineVisited) THEN
          PrevFilmPressure(PressurePerm(i)) = PrevPressure(i,1)
        ELSE
          PrevFilmPressure(PressurePerm(i)) = Pressure(PressurePerm(i)) + &
              FilmDamping(PressurePerm(i)) * ApertureVelocity(AperturePerm(i))
        END IF
      END IF
    END DO

  END IF

  MaxPressure = MAXVAL( ABS (Pressure) )
  
  !------------------
  ! Calculate temperature fluctuations
  IF ( Adiabatic ) THEN
    MaxTemperature = ReferenceTemperature * &
        ((1.0d0 + MaxPressure / ReferencePressure)**(1.0d0 - 1.0d0/HeatRatio) - 1.0)
  END IF

  IF(TransientSimulation) THEN
    CALL Info('TransientReynoldsSolver','Lumped Reynolds equation',Level=5)
    CALL AddToSaveList('Fluidic Force',LumpedForce,'(N/m)')
    CALL AddToSaveList('Pressure Mean',Pave,'(Pa)')
   IF ( Adiabatic ) CALL AddToSaveList('Max Temperature Variation',MaxTemperature,'(K)')
  END IF

  DO t=1,NoValues
    IF(ValueSaveRes(t)) CALL ListAddConstReal( Model % Simulation, &
        'res: '//TRIM(ValueNames(t)), Values(t) )
  END DO
  
  IF(TransientSimulation) THEN
    CALL Info('TransientReynoldsSolver',Message,Level=5)
    DO t=1,NoValues
      WRITE(Message,'(A,T35,ES15.5)') TRIM(ValueNames(t))//' '//TRIM(ValueUnits(t)),Values(t)
      CALL Info('TransientReynoldsSolver',Message,Level=5)
    END DO
  END IF

  SubroutineVisited = .TRUE.
  
CONTAINS


  SUBROUTINE AddToSaveList(Name, Value, Unit, savelocal, saveres)

    INTEGER :: n
    CHARACTER(LEN=*) :: Name, Unit
    REAL(KIND=dp) :: Value
    LOGICAL, OPTIONAL :: savelocal,saveres

    n = NoValues
    n = n + 1
    IF(n > MaxNoValues) THEN
      CALL WARN('TransientReynoldsSolver','Too little space for the scalars')
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
  SUBROUTINE NonlinearLocalMatrix(MassMatrix, StiffMatrix, ForceVector, &
      ElemAperture, ElemApertureVelocity, Viscosity, Pressure, &
      ReferencePressure, HolesExist, ElemImpedance, HeatRatio, Adiabatic, &
      Element, n, Nodes)
!------------------------------------------------------------------------------
    COMPLEX(KIND=dp) :: ElemImpedance(:)
    REAL(KIND=dp) :: MassMatrix(:,:), StiffMatrix(:,:), ForceVector(:), &
        ReferencePressure, Pressure(:), ElemAperture(:), ElemApertureVelocity(:), &
        Viscosity(:), HeatRatio
    INTEGER :: n
    LOGICAL :: HolesExist, Adiabatic
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    COMPLEX :: Impedance
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3)
    REAL(KIND=dp) :: Damping, Spring, SqrtElementMetric, U, V, W, S, &
        MS, MM, D, L, A, B, H1, H2, C, SQ(3)
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,DIM, NBasis, CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff
    REAL(KIND=dp) :: X,Y,Z,Metric(3,3),SqrtMetric,Symb(3,3,3),dSymb(3,3,3,3)

!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()

    Metric = 0.0d0
    Metric(1,1) = 1.0d0
    Metric(2,2) = 1.0d0
    Metric(3,3) = 1.0d0

    ForceVector = 0.0D0
    StiffMatrix = 0.0D0
    MassMatrix  = 0.0D0
    H1 = 0.0d0
    H2 = 0.0d0

!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------

    NBasis = n
    IntegStuff = GaussPoints( Element )

!------------------------------------------------------------------------------
    DO t=1,IntegStuff % n

      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      S = IntegStuff % s(t)

!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
      stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
          Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )
    
      s = s * SqrtElementMetric
      IF ( CoordSys /= Cartesian ) THEN
        X = SUM( Nodes % X(1:n) * Basis(1:n) )
        Y = SUM( Nodes % Y(1:n) * Basis(1:n) )
        Z = SUM( Nodes % Z(1:n) * Basis(1:n) )
        CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,X,Y,Z )
        s = s * SqrtMetric
      END IF
 
!------------------------------------------------------------------------------
!      The source term and the coefficient of the time derivative and 
!      diffusion terms at the integration point
!------------------------------------------------------------------------------

      ! The signs were checked for consistancy
      IF(HolesExist) THEN
        Impedance = SUM( ElemImpedance(1:n) * Basis(1:n) )
        Damping = REAL(1.0d0/Impedance) / AngularVelocity
        Spring = -AIMAG(1.0d0/Impedance) 
      END IF      

      ! Multipliers of p: Stiffness matrix 
      MS = -SUM(Basis(1:n) * (ElemAperture(1:n)**3.0d0) * &
          (SUM(Basis(1:n)*Pressure(1:n)) + ReferencePressure )/ &
          SUM(12.0*Basis(1:n)*Viscosity(1:n)))
      IF(HolesExist) THEN
        H1 = -Spring * SUM(Basis(1:n) * (Pressure(1:n) + ReferencePressure)) 
      END IF
      D = -SUM(Basis(1:n) * ElemApertureVelocity(1:n))

      ! Multipliers of dp/dt: Mass matrix 
      MM = -SUM(Basis(1:n) * ElemAperture(1:n))
      IF(HolesExist) THEN
        H2 = -Damping * SUM(Basis(1:n) * (Pressure(1:n) + ReferencePressure))
      ELSE
        H2 = 0.0d0
      END IF

      IF(.NOT. Incompressible) H2 = H2 + MM
      
      ! right-hand-side: Force vector 
      L = ReferencePressure * SUM( Basis(1:n) * ElemApertureVelocity(1:n))
      
      IF ( Adiabatic ) THEN
        MS = HeatRatio * MS
        D = HeatRatio * D
        L = HeatRatio * L
        DO i = 1, DIM
          SQ(i) = (1 - HeatRatio) * SUM(Basis(1:n) * (ElemAperture(1:n)**3.0d0)) * &
              SUM(dBasisdx(1:n,i) * Pressure(1:n)) / &
              SUM(12*Basis(1:n) * Viscosity(1:n))
        END DO
      END IF

!      print *,'Spring',Spring,'Damping',Damping,'H',H1,H2

!------------------------------------------------------------------------------
!      The Reynolds equation
!------------------------------------------------------------------------------
      DO p=1,NBasis
        DO q=1,NBasis
          A = (H1 + D) * Basis(q) * Basis(p) 
          B = H2 * Basis(q) * Basis(p)
          C = 0.0d0
        
!          print *,'A & B',A,B,C
          
          DO i=1,DIM
            IF ( Adiabatic )  C = C + SQ(i) * dBasisdx(q,i) * Basis(p)
            DO j=1,DIM
              A = A + MS * Metric(i,j) * dBasisdx(q,i) * dBasisdx(p,j)
            END DO
          END DO
          
          StiffMatrix(p,q) = StiffMatrix(p,q) + s * A + s * C
          MassMatrix(p,q)  = MassMatrix(p,q)  + s * B
        END DO
        ForceVector(p) = ForceVector(p) + s * Basis(p) * L

      END DO
    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE NonlinearLocalMatrix
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE NonlinearLocalBoundary(MassMatrix, StiffMatrix, ForceVector, &
      ElemImpedance, Pressure, ReferencePressure, ElemAperture, Element, n, Nodes)
!------------------------------------------------------------------------------
    COMPLEX(KIND=dp) :: ElemImpedance(:)
    REAL(KIND=dp) :: MassMatrix(:,:), StiffMatrix(:,:), ForceVector(:), Pressure(:)
    REAL(KIND=dp) :: ReferencePressure, ElemAperture(:)
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    COMPLEX(KIND=dp) :: Impedance 
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,H1,H2,TotalPressure, A, B, aper
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3),X,Y,Z
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,DIM,CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()

    ForceVector = 0.0D0
    StiffMatrix = 0.0D0
    MassMatrix  = 0.0D0

!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------
    IntegStuff = GaussPoints( Element )
!------------------------------------------------------------------------------
    DO t=1,IntegStuff % n
       U = IntegStuff % u(t)
       V = IntegStuff % v(t)
       W = IntegStuff % w(t)
       S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
              Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )

       s = s * SqrtElementMetric

       Impedance = SUM( ElemImpedance(1:n) * Basis(1:n) )
       TotalPressure = SUM(Pressure(1:n) * Basis(1:n)) + ReferencePressure
       aper = SUM( ElemAperture(1:n) * Basis(1:n) )

       ! spring term => stiffness matrix
       H2 = -REAL(1.0d0/Impedance) * aper * TotalPressure
       
!------------------------------------------------------------------------------
       DO p=1,n
         DO q=1,n
           StiffMatrix(p,q) = StiffMatrix(p,q) + s * Basis(q) * Basis(p) * H2
         END DO
       END DO
!------------------------------------------------------------------------------
     END DO
!------------------------------------------------------------------------------
  END SUBROUTINE NonlinearLocalBoundary
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE ReynoldsNonlinearForce( Model, NoElements, Pressure, &
      Aperture, Amplitude, Reorder, Force, Pint)
    !DLLEXPORT ReynoldsNonlinearForce
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: NoElements
    REAL(KIND=dp), POINTER :: Aperture(:), Amplitude(:), Pressure(:)
    REAL(KIND=dp) :: Force, Pint
    INTEGER :: Reorder(:)
!------------------------------------------------------------------------------
    INTEGER :: k,n
    TYPE(Element_t), POINTER :: CurrentElement
    INTEGER, POINTER :: NodeIndexes(:)
    TYPE(Nodes_t)   :: ElementNodes
    
    TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
    REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
    
    TYPE(ValueList_t), POINTER :: Material
    REAL(KIND=dp), DIMENSION(Model % MaxElementNodes) :: &
        ElemPressure, PrevElemPressure, ElemAperture, ElemAmplitude, ElemApertureVelocity
    REAL(KIND=dp) :: s,ug,vg,wg
    REAL(KIND=dp) :: ddBasisddx(Model % MaxElementNodes,3,3)
    REAL(KIND=dp) :: Basis(Model % MaxElementNodes)
    REAL(KIND=dp) :: dBasisdx(Model % MaxElementNodes,3),SqrtElementMetric
    REAL(KIND=dp) :: ForceAtGP, EfficiencyAtGP, dV, TotArea
    INTEGER :: N_Integ, t, tg, i
    LOGICAL :: stat, gotIt
    
! Need MaxElementNodes only in allocation
    n = Model % MaxElementNodes
    ALLOCATE( ElementNodes % x( n ), ElementNodes % y( n ), ElementNodes % z( n ) )
    
    Force = 0.0d0
    Pint = 0.0d0
    TotArea = 0.0d0
    DampFlux1 = 0.0d0
    DampFlux2 = 0.0d0

! Loop over all elements in the list
    DO t=1,Solver % NumberOfActiveElements
      
      CurrentElement => Solver % Mesh % Elements(Solver % ActiveElements(t))
      Model % CurrentElement => CurrentElement
 
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes

!------------------------------------------------------------------------------
! Get element nodal coordinates
!------------------------------------------------------------------------------
      ElementNodes % x(1:n) = Model % Nodes % x(NodeIndexes(1:n))
      ElementNodes % y(1:n) = Model % Nodes % y(NodeIndexes(1:n))
      ElementNodes % z(1:n) = Model % Nodes % z(NodeIndexes(1:n))

      k = ListGetInteger( Model % Bodies( CurrentElement % &
             Bodyid ) % Values, 'Material', minv=1, maxv=Model % NumberOfMaterials )
      Material => Model % Materials(k) % Values

      ElemPressure(1:n) = Pressure(Reorder(NodeIndexes(1:n)))
      IF(CalculateDamping) THEN
        PrevElemPressure(1:n) = PrevPressure(Reorder(NodeIndexes(1:n)),1)
        ElemApertureVelocity(1:n) = ApertureVelocity(Reorder(NodeIndexes(1:n)))
      END IF

      IF(ASSOCIATED (Aperture)) THEN
        ElemAperture(1:n) = Aperture(Reorder( NodeIndexes(1:n) ))
      ELSE
        ElemAperture(1:n) = ListGetReal( Material,'Aperture',n,NodeIndexes)
      ENDIF

      IF(ASSOCIATED (Amplitude)) THEN
        ElemAmplitude(1:n) = Amplitude(Reorder( NodeIndexes(1:n) ))
      ELSE
        ElemAmplitude(1:n) = ListGetReal( Material,'Amplitude',n,NodeIndexes,gotIt)
        IF(.NOT. GotIt) ElemAmplitude(1:n) = 1.0d0
      ENDIF

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

! Calculate the function to be integrated at the Gauss point
        ForceAtGP = SUM( Basis(1:n) * ElemPressure(1:n) * ElemAmplitude(1:n) )
        
! Use general coordinate system for dV
        dV = CoordinateSqrtMetric( SUM( ElementNodes % x(1:n) * Basis(1:n)), &
            SUM( ElementNodes % y(1:n) * Basis(1:n) ), &
            SUM( ElementNodes % z(1:n) * Basis(1:n) ) )
        
        TotArea = TotArea + s * dV
        Pint = Pint + s * dV * SUM( Basis(1:n) * ElemPressure(1:n))
        Force = Force + s * ForceAtGP * dV
        IF(CalculateDamping) THEN
          DampFlux1 = DampFlux1 + s * dV * ABS( SUM(Basis(1:n) * ElemAperture(1:n) * &
              (ElemPressure(1:n) - PrevElemPressure(1:n)) ) )
          DampFlux2 = DampFlux2 + s * dV * ABS( SUM(Basis(1:n) * ElemApertureVelocity(1:n)) ) * &
              ReferencePressure * dt
        END IF

      END DO! of the Gauss integration points
      
    END DO! of the bulk elements
    
    Pint = Pint / TotArea

    DEALLOCATE( ElementNodes % x, ElementNodes % y, ElementNodes % z )

!------------------------------------------------------------------------------
  END SUBROUTINE ReynoldsNonlinearForce
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE TransientReynoldsSolver
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! This is just temporal while there are old sif-files around
!------------------------------------------------------------------------------
SUBROUTINE ReynoldsEquation( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: ReynoldsEquation

  USE Types

  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation

  IF(Solver % Variable % DOFs == 2) THEN
    CALL Fatal('ReynoldsEquation', 'Use Solver HarmonicReynoldsSolver instead')
  ELSE
    CALL Fatal('ReynoldsEquation', 'Use Solver TransientReynoldsSolver instead')
  END IF

END SUBROUTINE ReynoldsEquation
!------------------------------------------------------------------------------
