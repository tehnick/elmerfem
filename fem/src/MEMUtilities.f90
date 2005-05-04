
MODULE MEMUtilities

  USE Types
  USE Lists
  USE Integration
  USE ElementDescription
  USE SolverUtils
  IMPLICIT NONE

  CONTAINS

!------------------------------------------------------------------------------
! Computes the impedance of small holes assuming some of the simple
! basic geometries. In addition, the impedance of the gathering flow
! (Skvor model) may be computed. This may be seen in the additional 
! source term in the Reynolds equation.
!------------------------------------------------------------------------------

  FUNCTION ComputeHoleImpedance(holemodel, r, b, p, visc, &
      dens, d, w) RESULT(impedance)
!!!DEC$ATTRIBUTES DLLEXPORT :: ComputeHoleImpedance
    
    CHARACTER(LEN=*) :: holemodel    
    COMPLEX(KIND=dp) :: corr, c, q, iu, qr, impedance,holeimp
    REAL(KIND=dp) :: r,b,d,p,visc,dens,w,eps,skvorimp
    INTEGER :: visited=0
    
    SAVE visited

    d = ABS(d)
    
    visited = visited + 1 
    eps = 1.0d-10
    c = 0.0
    iu = DCMPLX( 0, 1.0)
    q = SQRT(iu * w * dens / visc)

    holeimp = 0.0
    skvorimp = 0.0


    SELECT CASE( holemodel )

      CASE('slot')
      qr = q * r / 2.0d0
      corr = 1.0d0 - (1.0d0/qr)*((EXP(qr)-1.0)/(EXP(qr)+1.0))
      holeimp = (iu * w * dens * b) / corr

      CASE('round')
      qr = iu * q * r
      corr = 1.0d0 - (2.0d0/(qr**2)) * &
          BesselFunctionZ(qr,0,eps,.TRUE.) / &
          BesselFunctionZ(qr,0,eps,.FALSE.)
      holeimp = (iu * w * dens * b) / corr

      ! Flow gathering to the hole
      skvorimp = (6.0*visc*r*r / (d**3.0)) *  &
          (-2.0*LOG(p)-3.0+4.0*p-p*p)/(8.0*p)
      
      CASE ('square')
      CALL Warn('ComputeHoleImpedance','Square hole not implemented using slot instead!')

      qr = q * r / 2.0d0
      corr = 1.0d0 - (1.0d0/qr)*((EXP(qr)-1.0)/(EXP(qr)+1.0))
      holeimp = (iu * w * dens * b) / corr

    CASE DEFAULT 
      CALL WARN('ComputeHoleImpedance','Unknown hole type: '//TRIM(holemodel))       
    END SELECT

    IF(MOD(visited,400) == -1 ) THEN 
      PRINT *,'geom.cor :',corr
      PRINT *,'qr       :',qr
      PRINT *,'holeimp  :',holeimp
      PRINT *,'skvorimp :',skvorimp
    END IF

    impedance = holeimp/p + 1.0*skvorimp

  CONTAINS 

!------------------------------------------------------------------------------
! Calculates the Bessel function for integer n
! with relative accuracy eps. If integrate is true
! calculates its first integral int \int J(x)*x*dx
!------------------------------------------------------------------------------
    FUNCTION BesselFunctionZ( z, m, eps, integrate) RESULT(f)
      
      COMPLEX(KIND=dp) :: z, f, df
      REAL(KIND=dp) :: eps, prodk, prodl
      INTEGER :: m, n, k, l
      LOGICAL :: integrate
      
      f = 0.0
      k = 0
      prodk = 1.0d0
      prodl = 1.0d0
      
      n = ABS(m)
      IF(n > 0) THEN
        DO l=1,n
          prodl = l*prodl
        END DO
      END IF
      l = n
      
      DO
        df = z ** (2*k) * ((-1)**k) / (2**(2*k) * prodk * prodl)
        IF(integrate) THEN
          df = df * z ** 2 / (2*k+2+n)
        ENDIF
        f = f + df
        IF(ABS(df) < eps * ABS(f)) EXIT
        
        k = k+1
        prodk = k * prodk
        l = k+n
        prodl = l * prodl
      END DO
      
      f = f * (z ** n) / (2 ** n) 
      IF (m < 0) THEN
        f = f * ((-1)**m)
      END IF
    END FUNCTION BesselFunctionZ

!------------------------------------------------------------------------------

  END FUNCTION ComputeHoleImpedance
!------------------------------------------------------------------------------
    

!------------------------------------------------------------------------------
! Computes the additional impedance of the open side of an resonator. This
! may be used in the boundary conditions of Reynolds equation.
!------------------------------------------------------------------------------

  FUNCTION ComputeSideImpedance(d, visc, dens, omega, rarefaction, pref) RESULT(impedance)
!!DEC$ATTRIBUTES DLLEXPORT :: ComputeSideImpedance
    
    COMPLEX(KIND=dp) :: impedance
    REAL(KIND=dp) :: d, visc, dens, omega, mfp, kn, dl, pref
    LOGICAL :: rarefaction

    d = ABS(d)

    IF(rarefaction) THEN
      mfp = SQRT(PI/ (2.0 * dens * pref) ) * visc
      kn = mfp / d

      dl = 0.8488 * (1.0 + 2.676 * Kn**0.659)
    ELSE
      dl = 0.8488 
    END IF

    impedance = 12.0 * dl * visc / d

  END FUNCTION ComputeSideImpedance
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!  Computes from the displacement field the aperture and amplitude to be
!  used in the harmonic Reynolds equation
!------------------------------------------------------------------------------

  SUBROUTINE ComputeAperture(Model, Solver, dt, TransientSimulation, &
      NoDisplace, ElemAperture, ElemAmplitude, Initialize, DvarExists, &
      MaxAmplitude, NoModes) 
!DEC$ATTRIBUTES DLLEXPORT :: ComputeAperture

    TYPE(Solver_t), TARGET :: Solver
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: dt
    LOGICAL :: TransientSimulation, NoDisplace, Initialize
    LOGICAL, OPTIONAL :: DvarExists
    REAL(KIND=dp) :: ElemAmplitude(:,:), ElemAperture(:)
    REAL(KIND=dp), OPTIONAL :: MaxAmplitude
    INTEGER, OPTIONAL :: NoModes
   
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

    TYPE(Variable_t), POINTER :: DVar, Dvar2, AVar
    TYPE(ValueList_t), POINTER :: Material
    TYPE(Nodes_t) :: ElementNodes
    TYPE(Element_t),POINTER :: CurrentElement

    INTEGER, POINTER :: AmplitudePerm(:), AperturePerm(:)
    REAL (KIND=DP), POINTER :: Amplitude(:), Aperture(:)
   
    INTEGER :: iter, i, j, k, n, t, istat, eq, &
        eq_id, mat_id, body_id, NormalDirection, NoAmplitudes, NoEigenModes
    INTEGER, POINTER :: NodeIndexes(:), EigenModes(:), DvarPerm(:)    
    LOGICAL :: AllocationsDone = .FALSE., GotIt, Shell, Shell2, Solid, &
        AmplitudeExists, ApertureExists, LumpSix
    REAL(KIND=dp) :: CoordDiff(3), Frequency, ElasticMass, ElasticSpring, ds
    REAL(KIND=dp), POINTER :: MaxAmplitudes(:), AmplitudeComp(:)
    REAL(KIND=dp) :: Normal(3), Translate(3), Rotate(3,3), Displace(3), Coords(3), &
        TranslateVel(3), RotateVel(3,3)
    CHARACTER(LEN=MAX_NAME_LEN) :: String
    TYPE(Mesh_t), POINTER :: Mesh
     
    SAVE ElementNodes, AllocationsDone, Dvar, Dvar2, DvarPerm, MaxAmplitudes,  &
        Shell, Shell2, Solid, NormalDirection, NoEigenModes, ApertureExists, &
        Aperture, AperturePerm, Amplitude, AmplitudePerm, NoAmplitudes, &
        AmplitudeExists, Translate, Rotate, TranslateVel, RotateVel, Normal

    !-------------------------------------------------------------------------
    ! Compute the aperture and amplitude in the case of dofs being six 
    ! variables (3 translation and 3 rotations dofs)
    !------------------------------------------------------------------------- 
    LumpSix = ListGetLogical( Model % Simulation, 'Lump Six',gotIt )
    IF(LumpSix) THEN
      IF(Initialize) THEN

        NoEigenModes = 1 
        IF(PRESENT(MaxAmplitude)) THEN
          MaxAmplitude = 1.0d0
        END IF

        Mesh => Model % Meshes
        DO WHILE( ASSOCIATED( Mesh ) )
          DVar => VariableGet( Mesh % Variables, 'LumpSix', ThisOnly=.TRUE. )
          IF ( ASSOCIATED( DVar ) ) THEN
            IF ( ASSOCIATED( Mesh, DVar % PrimaryMesh ) ) THEN
              EXIT
            END IF
          END IF
          Mesh => Mesh % Next
        END DO

        IF(PRESENT(DvarExists)) DvarExists = ASSOCIATED(DVar)

        IF(.NOT. ASSOCIATED(Dvar)) THEN
          CALL Warn('MEMUtilities','Field variable LumpSix does not exist!')
          RETURN
        END IF
       
        NormalDirection = ListGetInteger( Solver % Values, 'Normal Direction',gotIt )
        IF(.NOT. GotIt) NormalDirection = 3
        Normal = 0.0d0
        Normal(NormalDirection) = 1.0d0
        
        Translate(1:3) = Dvar % Values(Dvar % Perm(1:3))
        Rotate = 0.0d0
        Rotate(1,2) = Dvar % Values(Dvar % Perm(6))
        Rotate(1,3) = -Dvar % Values(Dvar % Perm(5))
        Rotate(2,3) = Dvar % Values(Dvar % Perm(4))

        IF(TransientSimulation) THEN
          TranslateVel(1:3) = Dvar % PrevValues(Dvar % Perm(1:3),1)
          RotateVel = 0.0d0        
          RotateVel(1,2) = Dvar % PrevValues(Dvar % Perm(6),1)
          RotateVel(1,3) = -Dvar % PrevValues(Dvar % Perm(5),1)
          RotateVel(2,3) = Dvar % PrevValues(Dvar % Perm(4),1)
        END IF

        DO i=1,3
          DO j=i+1,3
            Rotate(j,i) = -Rotate(i,j)
            IF(TransientSimulation) RotateVel(j,i) = -RotateVel(i,j)
          END DO
        END DO
      ELSE       
        IF(.NOT. ASSOCIATED(Dvar)) THEN
          IF(PRESENT(DvarExists)) DvarExists = .FALSE.
          RETURN
        END IF
 
        CurrentElement => Model % CurrentElement        
        n = CurrentElement % TYPE % NumberOfNodes
        NodeIndexes => CurrentElement % NodeIndexes
        
        body_id = CurrentElement % BodyId        
        ElemAperture(1:n) = ListGetReal( Model % Bodies( body_id) % Values, &
            'Reference Aperture',n,NodeIndexes,GotIt)

        IF(.NOT. GotIt) THEN
          mat_id = ListGetInteger( Model % Bodies( body_id ) % Values, &
              'Material', minv=1,maxv=Model % NumberOfMaterials )
          Material => Model % Materials(mat_id) % Values          
          ElemAperture(1:n) = ListGetReal( Material, &
              'Reference Aperture',n,NodeIndexes)
        END IF
        
        DO i = 1,n          
          Coords(1) =  Solver % Mesh % Nodes % x(NodeIndexes(i))
          Coords(2) =  Solver % Mesh % Nodes % y(NodeIndexes(i))
          Coords(3) =  Solver % Mesh % Nodes % z(NodeIndexes(i))
          Displace = Translate + MATMUL(Rotate,Coords) 

          IF(ElemAperture(i) * (ElemAperture(i)+Displace(NormalDirection)) < 0.0d0 ) THEN
            CALL Fatal('MEMUtilities','The aperture became nagative')
          END IF
                   
          ElemAperture(i) = ElemAperture(i) + Displace(NormalDirection)
          
          IF(TransientSimulation) THEN         
            Displace = TranslateVel + MATMUL(RotateVel,Coords)
            ElemAmplitude(1,i) = Displace(NormalDirection) 
          ELSE
            ElemAmplitude(1,i) = 1.0d0
          END IF        
        END DO

!        DO i=1,n
!          IF(ElemAperture(i) < 0.0) THEN
!            ElemAmplitude(1,i) = -ElemAmplitude(1,i) 
!            ElemAperture(i) = -ElemAperture(i)
!          END IF
!        END DO

      END IF
    END IF

!------------------------------------------------------------------------------
! Check variables needed for computing aperture and amplitude
!------------------------------------------------------------------------------

  IF(.NOT. LumpSix) THEN

    IF(Initialize) THEN      
      CALL Info('ComputeAperture','Computing aperture on-the-fly')
      
      Shell = .FALSE.
      Shell2 = .FALSE.
      Solid = .FALSE.
      
      EigenModes => ListGetIntegerArray( Model % Simulation, 'MEM Eigen Modes',gotIt )
      IF(gotIt) THEN
        NoEigenModes = SIZE(EigenModes)
      ELSE
        NoEigenModes = 0
      END IF
      
      NULLIFY(DVar)
      DVar => VariableGet( Model % Variables, 'Displacement' )
      
      !  Set the normal direction so that it is the direction of maximum displacement. 
      IF (ASSOCIATED (DVar)) THEN
        Solid = .TRUE.       
        NormalDirection = ListGetInteger( Solver % Values, 'Normal Direction',gotIt )
        IF(.NOT. GotIt) THEN
          NormalDirection = 1
          
          IF(NoEigenModes > 0) THEN
            IF(.NOT. ASSOCIATED( DVar % EigenVectors) ) THEN
              NoEigenModes = 0
              CALL Warn('ComputeAperture','No EigenVectors exists')
            END IF
            DO j=1,DVar % DOFs
              DO i=1,SIZE(Dvar % Perm)
                k = Dvar % Perm(i) 
                IF(k > 0) THEN
                  ds = DVar % EigenVectors(EigenModes(1),Dvar%DOFs *(k-1)+j)
                  CoordDiff(j) = MAX(CoordDiff(j), ABS(ds))
                END IF
              END DO
            END DO
          ELSE
            DO j=1,DVar % DOFs
              DO i=1,SIZE(Dvar % Perm) 
                k = Dvar % Perm(i)
                IF(k > 0) THEN
                  CoordDiff(j) = MAX(CoordDiff(j), ABS(Dvar % Values(Dvar % DOFs *(k-1)+j)))
                END IF
              END DO
            END DO
          END IF
          
          DO j=1,DVar % DOFs
            IF( CoordDiff(j) > CoordDiff(NormalDirection) )  NormalDirection = j
          END DO
        END IF
      ELSE 
        NormalDirection = 1
        DVar => VariableGet( Model % Variables, 'Deflection' )
        IF(ASSOCIATED (DVar)) THEN
          Shell = .TRUE.
        END IF
        DVar2 => VariableGet( Model % Variables, 'Deflection B' )
        IF(ASSOCIATED (DVar2)) THEN
          Shell2 = .TRUE.
        END IF
      END IF
      
      WRITE(Message,'(A,T35,I1)') 'Normal Direction',NormalDirection
      CALL Info('ComputeAperture',Message,Level=5)
      
      IF(.NOT. (Shell .OR. Solid)) THEN 
        IF(PRESENT(DvarExists)) DvarExists = .FALSE.
        RETURN
      END IF
      
      IF(PRESENT(DvarExists)) DvarExists = .TRUE.            
      IF(PRESENT(NoModes)) NoModes = MAX(1,NoEigenModes)
    
      IF(NoEigenModes > 0 .AND. .NOT. ASSOCIATED(Dvar % EigenVectors)) THEN
        CALL Fatal('ComputeAperture','No EigenVectors exists!')
      END IF

!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
 
      IF ( .NOT. AllocationsDone ) THEN
        N = Solver % Mesh % MaxElementNodes        
        ALLOCATE( ElementNodes % x( N ), ElementNodes % y( N ), ElementNodes % z( N ),       &
            DvarPerm(N), MaxAmplitudes(MAX(1,NoEigenModes)), &
            STAT=istat )
        IF ( istat /= 0 ) CALL FATAL('ComputeAperture','Memory allocation error')
        AllocationsDone = .TRUE.
      END IF
    
!------------------------------------------------------------------------------
! Calculate the maximum displacements used in the normalization
!------------------------------------------------------------------------------  

      MaxAmplitudes = 0.0d0    
      
      IF(TransientSimulation) THEN    
        
        DO i=1,SIZE(Dvar % Perm) 
          k = Dvar % Perm(i)
          IF(k > 0) THEN
            IF(Solver % DoneTime == 1) THEN              
              MaxAmplitudes(1) = MAX(MaxAmplitudes(1), &
                  ABS( Dvar % Values(Dvar%DOFs*(k-1)+NormalDirection) ) )
            ELSE
              MaxAmplitudes(1) = MAX(MaxAmplitudes(1), &
                  ABS( Dvar % Values(Dvar%DOFs*(k-1)+NormalDirection) - &
                  Dvar % PrevValues(Dvar%DOFs*(k-1)+NormalDirection,3)) )              
            END IF
          END IF
        END DO
        MaxAmplitudes(1) = MaxAmplitudes(1) / dt
        
      ELSE
        
        DO i=1,SIZE(Dvar % Perm) 
          k = Dvar % Perm(i)
          IF(k > 0) THEN
            IF(Shell2) THEN
              MaxAmplitudes(1) = MAX(MaxAmplitudes(1), &
                  ABS( Dvar % Values(Dvar%DOFs*(k-1)+NormalDirection) - &
                  Dvar2 % Values(Dvar%DOFs*(k-1)+NormalDirection) ) )
            ELSE
              MaxAmplitudes(1) = MAX(MaxAmplitudes(1), &
                  ABS( Dvar % Values(Dvar%DOFs*(k-1)+NormalDirection) ) )
            END IF
          END IF
        END DO
        
        CALL ListAddConstReal( Model % Simulation,'res: Elastic Displacement', MaxAmplitudes(1))
        
        IF(NoEigenModes > 0) THEN
          MaxAmplitudes = 0.0d0
          
          DO j=1,NoEigenModes
            DO i=1,SIZE(Dvar % Perm)
              k = Dvar % Perm(i)
              IF(k > 0) THEN
                MaxAmplitudes(j) = MAX(MaxAmplitudes(j), &
                    ABS( Dvar % EigenVectors(EigenModes(j), Dvar % DOFs * (k-1) + NormalDirection ) ) )
              END IF
            END DO
          END DO
        END IF
        
      END IF
    
      IF(PRESENT(MaxAmplitude)) THEN
        j = MAX(1,NoEigenModes)
        MaxAmplitude = MaxAmplitudes(1)
      END IF

      IF(NoEigenModes > 0) THEN
        WRITE(Message,'(A,I2,A)') 'There are ',NoEigenModes,' eigen modes'    
        CALL Info('ComputeAperture',Message,Level=5)
      END IF

      DO j=1,NoEigenModes
        Frequency = SQRT( DVar % EigenValues(EigenModes(j)) ) /(2.0d0 * PI)
        ElasticMass = 1.0d0 / (MaxAmplitudes(j) ** 2.0)
        
        IF ( CurrentCoordinateSystem() /= Cartesian ) THEN
          ElasticMass = 2.0 * PI * ElasticMass
        END IF
        
        ElasticSpring = DVar % EigenValues(EigenModes(j)) * ElasticMass
        
        WRITE(Message,'(A,I1,T35,ES15.5)') 'Elastic Mass ',j,ElasticMass
        CALL Info('ComputeAperture',Message,Level=5)
        WRITE(Message,'(A,I1)') 'res: Elastic Mass ',j
        CALL ListAddConstReal(Model % Simulation, Message, ElasticMass)
        
        WRITE(Message,'(A,I1,T35,ES15.5)') 'Elastic Spring ',j,ElasticSpring
        CALL Info('ComputeAperture',Message,Level=5)
        WRITE(Message,'(A,I1)') 'res: Elastic Spring ',j
        CALL ListAddConstReal(Model % Simulation, Message, ElasticSpring)
        
        WRITE(Message,'(A,I1,T35,ES15.5)') 'Eigen Frequency ',j,Frequency
        CALL Info('ComputeAperture',Message,Level=5)
        WRITE(Message,'(A,I1)') 'res: Eigen Frequency ',j
        CALL ListAddConstReal(Model % Simulation, Message, Frequency)
        
        WRITE(Message,'(A,I1,T35,ES15.5)') 'Maximum Amplitude ',j,MaxAmplitudes(j)
        CALL Info('ComputeAperture',Message,Level=5)
      END DO
      
      IF(TransientSimulation) THEN
        WRITE(Message,'(A,T35,ES15.5)') 'Maximum Velocity ',MaxAmplitudes(1)
        CALL Info('ComputeAperture',Message,Level=5)
      END IF
    ELSE
      
    !--------------------------------------------------------------------------
    ! If the variables have been initialized then compute the aperture amplitude 
    !---------------------------------------------------------------------------
    
      IF(.NOT. (Shell .OR. Solid)) THEN 
        IF(PRESENT(DvarExists)) DvarExists = .FALSE.
        RETURN
      END IF
      
      CurrentElement => Model % CurrentElement 
      
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes
      
      ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes(1:n))
      ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes(1:n))
      ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes(1:n))
      
      body_id = CurrentElement % BodyId      
      ElemAperture(1:n) = ListGetReal( Model % Bodies( body_id) % Values, &
          'Reference Aperture',n,NodeIndexes,GotIt)

      IF(.NOT. GotIt) THEN
        mat_id = ListGetInteger( Model % Bodies( body_id ) % Values, &
            'Material', minv=1,maxv=Model % NumberOfMaterials )
        Material => Model % Materials(mat_id) % Values
        ElemAperture(1:n) = ListGetReal( Material, &
            'Reference Aperture',n,NodeIndexes)
      END IF
      DvarPerm(1:n) = Dvar % Perm(NodeIndexes(1:n))
      
      IF(NoEigenModes > 0) THEN
        DO i=1,n                    
          IF(.NOT. NoDisplace) THEN
            ElemAperture(i) = ElemAperture(i) + &
                (Dvar % Values(Dvar %DOFs * (DvarPerm(i)-1)+NormalDirection))  
          END IF
          IF(Shell2) THEN              
            ElemAperture(i) = ElemAperture(i) - &
                (Dvar2 % Values(Dvar2 %DOFs * (DvarPerm(i)-1)+NormalDirection))
          END IF
          DO j=1,NoEigenModes
            ElemAmplitude(j,i) = Dvar % EigenVectors(EigenModes(j), &
                Dvar % DOFs * (DvarPerm(i)-1) + NormalDirection )
            IF(ElemAperture(i) < 0.0) ElemAmplitude(j,i) = -ElemAmplitude(j,i) 
          END DO
          ElemAperture(i) = ABS(ElemAperture(i))
        END DO
        
      ELSE IF(TransientSimulation) THEN
        ElemAperture(1:n) = ElemAperture(1:n) + &
            Dvar % Values(Dvar % DOFs * (DvarPerm(1:n)-1) + NormalDirection )
        IF(Solver % DoneTime == 1) THEN
          ElemAmplitude(1,1:n) = & 
              Dvar % Values(Dvar % DOFs * (DvarPerm(1:n)-1) + NormalDirection ) / dt          
        ELSE 
          ! Note that the previous values are located in the third vector!
          ElemAmplitude(1,1:n) = ( &
              Dvar % Values(Dvar % DOFs * (DvarPerm(1:n)-1) + NormalDirection ) - &
              Dvar % PrevValues(Dvar % DOFs * (DvarPerm(1:n)-1) + NormalDirection, 3) ) / dt
        END IF
        
        DO i=1,n
          IF(ElemAperture(i) < 0.0) THEN
            ElemAmplitude(1,i) = -ElemAmplitude(1,i) 
            ElemAperture(i) = -ElemAperture(i)
          END IF
        END DO
        
      ELSE
        
        ElemAmplitude(1,1:n) = Dvar % Values(&
            Dvar % DOFs * (DvarPerm(1:n)-1) + NormalDirection )
        
        IF(.NOT. NoDisplace) ElemAperture(1:n) = ElemAperture(1:n) + ElemAmplitude(1,1:n)
        
        IF(Shell2) THEN
          DvarPerm(1:n) = Dvar2 % Perm(NodeIndexes(1:n))          
          ElemAmplitude(1,1:n) = ElemAmplitude(1,1:n) - &
              Dvar2 % Values(Dvar%DOFs*(DvarPerm(1:n)-1)+NormalDirection)
        END IF
        DO i=1,n
          IF(ElemAperture(i) < 0.0) THEN
            ElemAmplitude(1,i) = -ElemAmplitude(1,i) 
            ElemAperture(i) = -ElemAperture(i)
          END IF
        END DO
      END IF
      
      ! Normalize amplitude to unity 
      IF(.NOT. TransientSimulation) THEN
        DO j=1,NoAmplitudes
          IF(ABS(MaxAmplitudes(j)) <= TINY(MaxAmplitudes(j)) ) THEN
            ElemAmplitude(j,1:n) = 1.0d0
          ELSE
            ElemAmplitude(j,1:n) = ElemAmplitude(j,1:n) / MaxAmplitudes(j)
          END IF
        END DO
      END IF
    END IF
  END IF


  !-------------------------------------------------------------------------
  ! Check whether aperture and amplitude variables exist for saving the data
  ! Otherwise they will not be saved.
  !-------------------------------------------------------------------------
  IF(Initialize) THEN
    NULLIFY(AVar)
    AVar => VariableGet( Model % Variables, 'Aperture' )
    ApertureExists = ASSOCIATED(AVar)
    IF(ApertureExists) THEN
      AperturePerm  => AVar % Perm
      Aperture => AVar % Values
    END IF
    
    NULLIFY(AVar)
    IF(TransientSimulation) THEN
      AVar => VariableGet( Model % Variables, 'Aperture Velocity' )
    ELSE
      AVar => VariableGet( Model % Variables, 'Amplitude' )
    END IF
    AmplitudeExists = ASSOCIATED(AVar)
    IF(AmplitudeExists) THEN
      NoAmplitudes = AVar % DOFs
      AmplitudePerm  => AVar % Perm
      Amplitude => AVar % Values
      IF(NoAmplitudes /= MAX(1,NoEigenModes) ) THEN
        CALL Warn('MemsUtilities','Number of Amplitudes and Modes differ!')
      END IF
    END IF
  ELSE
    IF(ApertureExists) THEN
      Aperture(AperturePerm(NodeIndexes(1:n))) = ElemAperture(1:n)
    END IF    
    IF(AmplitudeExists) THEN
      DO j=1,NoAmplitudes
        Amplitude(NoAmplitudes*(AperturePerm(NodeIndexes(1:n))-1)+j) = &
            ElemAmplitude(j,1:n)
      END DO
    END IF
  END IF


!------------------------------------------------------------------------------
  END SUBROUTINE ComputeAperture
!------------------------------------------------------------------------------


END MODULE MEMUtilities
!------------------------------------------------------------------------------



