!/*****************************************************************************
! *
! *       ELMER, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 - , Center for Scientific Computing,
! *                                    Finland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! ****************************************************************************/
!
!/*****************************************************************************
! *
! *****************************************************************************
! *
! *                    Author:       Juha Ruokolainen
! *
! *                 Address: Center for Scientific Computing
! *                        Tietotie 6, P.O. BOX 405
! *                          02101 Espoo, Finland
! *                          Tel. +358 0 457 2723
! *                        Telefax: +358 0 457 2302
! *                      EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 04 Oct 2000
! *
! *                Modified by: Peter Råback, Mika Malinen
! *
! *       Date of modification: 04 Sep 2003
! *
! ****************************************************************************/

!------------------------------------------------------------------------------
SUBROUTINE DissipativeHelmholtzSolver( Model,Solver,dt,TransientSimulation )
  !DEC$ATTRIBUTES DLLEXPORT :: DissipativeHelmholtzSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the dissipative Helmholtz equations by decoupling the effects of
!  incompressibility and heat conduction. 
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
  USE Types
  USE Lists

  USE Integration
  USE ElementDescription

  USE SolverUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Matrix_t),POINTER  :: StiffMatrix, StiffMatrixB
  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement
  TYPE(Variable_t), POINTER :: TempVar, Var, TVar, PVar, VVar, IrrotVelVar
  TYPE(ValueList_t), POINTER :: Material

  INTEGER, POINTER :: NodeIndexes(:), PresPerm(:), TempPerm(:), &
      RealTemp(:), RealPres(:), AbsVals(:), VPerm(:), IrrotVelPerm(:) 
  REAL(KIND=dp), POINTER :: Pres(:), Temp(:), ForceVector(:), V(:), IrrotVel(:)

  INTEGER :: iter, i, j, k, n, p, t, istat, eq, LocalNodes, Visited = 0, DIM, &
      NonlinearIter, PrevDoneTime = 0, CoordSys
  REAL(KIND=dp) :: Norm1, PrevNorm1, Norm2, PrevNorm2, &
      RelativeChange1, RelativeChange2, & 
      AngularFrequency, GasRatio, Temperature0, Pressure0, Impedance1, Impedance2, &
      NonlinearTol,s, Viscosity, BulkViscosity, Density, Conductivity, Lambda, &
      HeatCapacity, Twall1, Twall2, RelaxationPar = 1.0d0, ElapsedTime, &
      ElapsedCPUTime, CPUTime, RealTime
  COMPLEX(KIND=dp) :: SurfForce

  LOGICAL :: AllocationsDone = .FALSE., GotIt, GotIt2, stat, WallTemperature, &
      OptimizeBW, NoPenetrate, IterationConverged, InitializeSolution, &
      BCPrediction

  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), Load(:,:), Work(:), &
       LocalForce(:), NormalVelocity1(:), NormalVelocity2(:), NormalForce1(:), &
       NormalForce2(:), NormalVelocity(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName

  SAVE LocalStiffMatrix, Work, Load, LocalForce, ElementNodes, &
      NormalVelocity1, NormalVelocity2, NormalForce1, NormalForce2, &
      AllocationsDone, Visited, AngularFrequency, NormalVelocity, &
      StiffMatrixB, RelaxationPar, PrevDoneTime

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------

  IterationConverged = .FALSE.
  DIM = CoordinateSystemDimension()
  CoordSys = CurrentCoordinateSystem()
  IF (.NOT. (CoordSys==Cartesian .OR. CoordSys==AxisSymmetric) ) THEN
    CALL Fatal( 'DissipativeHelmholtzSolver', 'Illegal coordinate system' )
  END IF

  InitializeSolution = .FALSE.  
  IF (TransientSimulation) THEN
    IF (PrevDoneTime /= Solver % DoneTime) THEN
      Visited = 0
      InitializeSolution = .TRUE.
      PrevDoneTime = Solver % DoneTime 
    END IF
  END IF

  Visited = Visited + 1

  IF ( .NOT.ASSOCIATED( Solver % Matrix ) ) RETURN
  Solver % Matrix % COMPLEX = .TRUE.

  Var => Solver % variable
  Pres     => Solver % Variable % Values
  PresPerm => Solver % Variable % Perm
  
  LocalNodes = COUNT( PresPerm > 0 )
  IF ( LocalNodes <= 0 ) RETURN

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS

  Norm1 = Solver % Variable % Norm

!------------------------------------------------------------------------------
! Get variables needed for solution of the second variable
!------------------------------------------------------------------------------

  NULLIFY(TempVar)
  TempVar => VariableGet( Model % Variables, 'TempCond' )  
  IF (ASSOCIATED (TempVar)) THEN
    IF(TempVar % DOFs /= 2) CALL Fatal('DissipativeHelmholtzSolver', &
        'A wrong number of dofs for TempCond')
    Temp     => TempVar % Values
    TempPerm => TempVar % Perm
    Norm2 = TempVar % Norm
  ELSE
    CALL Fatal('DissipativeHelmholtzSolver','Variable TempCond does not exist!')
  END IF

!-----------------------------------------------------------------------------
! Get the current velocity solution
!-----------------------------------------------------------------------------

  NULLIFY(VVar)
  VVar => VariableGet( Model % Variables, 'TotVel' )  
  IF (ASSOCIATED (VVar)) THEN
    V     => VVar % Values
    VPerm => VVar % Perm
  ELSE
    CALL Fatal('DissipativeHelmholtzSolver','Variable TotVel does not exist!')
  END IF

  IF (InitializeSolution) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      DO i = 1, dim
        V((t-1)*2*dim+(i-1)*2+1) = 0.0d0
        V((t-1)*2*dim+(i-1)*2+2) = 0.0d0
      END DO
    END DO
  END IF

!-----------------------------------------------------------------------------
! In the case of scanning over range of frequencies get the irrotational 
! velocity field for the previous frequency.
!------------------------------------------------------------------------------

  IF (InitializeSolution .AND. Solver % DoneTime > 1) THEN
    NULLIFY(IrrotVelVar)
    IrrotVelVar => VariableGet( Model % Variables, 'IrrotVel' )  
    IF (ASSOCIATED (IrrotVelVar)) THEN
      IrrotVel     => IrrotVelVar % Values
      IrrotVelPerm => IrrotVelVar % Perm
    ELSE
      CALL Warn('DissipativeHelmholtzSolver','Variable IrrotVel does not exist!')
    END IF
  END IF


!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone) THEN
    N = Solver % Mesh % MaxElementNodes

    ! Make sure that the new matrix is optimized as the old one
    ! Then the permutation will be the same

!    OptimizeBW = ListGetLogical(Solver % Values,'Bandwidth Optimize',gotIt)
!    IF(.NOT. GotIt) OptimizeBW = .TRUE.

    ! Create a new matrix...

!    StiffMatrixB => CreateMatrix( Model, Solver % Mesh,  &
!        TempPerm, TempVar % DOFs, MATRIX_CRS, &
!        OptimizeBW, ListGetString( Solver % Values, 'Equation' ) )
    
    ALLOCATE( ElementNodes % x( N ),  &
        ElementNodes % y( N ),       &
        ElementNodes % z( N ),       &
        LocalForce( 2*N ),           &
        NormalVelocity(N),           &
        NormalVelocity1(N),           &
        NormalVelocity2(N), &
        NormalForce1(N),           &
        NormalForce2(N),           &
        Work( N ),                   &
        LocalStiffMatrix( 2*N,2*N ), &
        Load( 6,N ), STAT=istat )

    IF ( istat /= 0 ) THEN
      CALL Fatal( 'DissipativeHelmholtzSolver', 'Memory allocation error.' )
    END IF

    AllocationsDone = .TRUE.
  END IF

!------------------------------------------------------------------------------
! Figure out angular frequency:
!------------------------------------------------------------------------------
  AngularFrequency = 0.0d0
  NodeIndexes => Solver % Mesh % Elements(1) % NodeIndexes
  Work(1:1) = ListGetReal( Model % Simulation, &
     'Angular Frequency', 1, NodeIndexes, GotIt )
  IF ( GotIt ) THEN
    AngularFrequency = Work(1)
  ELSE
    Work(1:1) = ListGetReal( Model % Simulation, &
        'Frequency', 1, NodeIndexes, GotIt)
    IF (GotIt) THEN
      AngularFrequency = 2*PI*Work(1)
    ELSE
      CALL Fatal('DissipativeHelmholtzSolver','Frequency for simulation must be given!')      
    END IF
  END IF


!------------------------------------------------------------------------------
! Do some additional initialization, and go for it
!------------------------------------------------------------------------------
  NonlinearTol = ListGetConstReal( Solver % Values, &
      'Nonlinear System Convergence Tolerance',GotIt )

  NonlinearIter = ListGetInteger( Solver % Values, &
       'Nonlinear System Max Iterations', GotIt )
  IF ( .NOT.GotIt ) NonlinearIter = 1

  EquationName = ListGetString( Solver % Values, 'Equation' )
  BCPrediction = ListGetLogical( Solver % Values, 'Utilize Previous Solution', GotIt )


!  CALL InitializeTimestep(Solver)

!------------------------------------------------------------------------------
! ITERATE UNTIL THE SOLUTIONS OF THE TWO EQUATIONS DO NOT CHANGE... 
!------------------------------------------------------------------------------
    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
    WRITE( Message, * ) 'Simulation frequency (Hz) ',AngularFrequency/(2*PI)
    CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )

    ElapsedCPUTime  = CPUTime()
    ElapsedTime = RealTime()

!------------------------------------------------------------------------------
  DO iter=1,NonlinearIter 
!------------------------------------------------------------------------------
!   PERFORM THE ASSEMBLY AND SOLUTION FOR THE FIRST EQUATION...        
!------------------------------------------------------------------------------
    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', '-------------------------------------', Level=4 )
    WRITE( Message, * ) 'Compressibility iteration', iter
    CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', '-------------------------------------', Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
    
    CALL InitializeToZero( StiffMatrix, ForceVector )
!------------------------------------------------------------------------------
!   Do the bulk assembly:
!------------------------------------------------------------------------------
    DO t=1,Solver % NumberOfActiveElements
!------------------------------------------------------------------------------

      CurrentElement => Solver % Mesh % Elements( Solver % ActiveElements(t) )
      Model % CurrentElement => CurrentElement
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes
      
      ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
      ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
      ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
!------------------------------------------------------------------------------
!     Get equation & material parameters
!------------------------------------------------------------------------------

      k = ListGetInteger( Model % Bodies( CurrentElement % &
          Bodyid ) % Values, 'Material', minv=1,maxv=Model % NumberOfMaterials )
      
      Material => Model % Materials(k) % Values
        
      GasRatio = ListGetConstReal( Material , 'Specific Heat Ratio')
      Pressure0 = ListGetConstReal( Material , 'Reference Pressure')
      Temperature0 = ListGetConstReal( Material , 'Equilibrium Temperature')
      Density = ListGetConstReal( Material , 'Equilibrium Density')
      Viscosity = ListGetConstReal( Material , 'Viscosity')
      Lambda = -Viscosity * 2.0d0/3.0d0
      BulkViscosity = ListGetConstReal( Material , 'Bulk Viscosity', GotIt)
      IF (GotIt) Lambda = BulkViscosity - Viscosity * 2.0d0/3.0d0 
      Conductivity = ListGetConstReal( Material , 'Heat Conductivity')

!------------------------------------------------------------------------------
!     Get element local matrix and rhs vector
!------------------------------------------------------------------------------
      CALL LocalMatrix(  LocalStiffMatrix, LocalForce, &
          CurrentElement, n, ElementNodes )
!------------------------------------------------------------------------------
!     Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
      CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
          ForceVector, LocalForce, n, Solver % Variable % DOFs, &
          PresPerm(NodeIndexes) )
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------
!
!    Neumann & Newton BCs:
!    ---------------------

!------------------------------------------------------------------------------
    DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
        Solver % Mesh % NumberOfBulkElements +  &
        Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
      CurrentElement => Solver % Mesh % Elements(t)
      Model % CurrentElement => CurrentElement
!------------------------------------------------------------------------------
!     The element type 101 (point element) can only be used
!     to set Dirichlet BCs, so skip em at this stage.
!------------------------------------------------------------------------------
      IF ( CurrentElement % TYPE % ElementCode == 101 ) CYCLE
!------------------------------------------------------------------------------
      DO i=1,Model % NumberOfBCs
        IF ( CurrentElement % BoundaryInfo % Constraint == &
            Model % BCs(i) % Tag ) THEN
!------------------------------------------------------------------------------
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
            
          IF ( ANY( PresPerm(NodeIndexes) == 0 ) ) CYCLE
            
          ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
          ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
          ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
          
          NoPenetrate = ListGetLogical( Model % BCs(i) % Values, &     
              'Prescribed Normal Velocity', GotIt )         
          IF ( .NOT. NoPenetrate) CYCLE 

          NormalVelocity1(1:n) = ListGetReal( Model % BCs(i) % Values, &
              'Re Normal Velocity', n, NodeIndexes, GotIt )
          NormalVelocity2(1:n) = ListGetReal( Model % BCs(i) % Values, &
              'Im Normal Velocity', n, NodeIndexes, GotIt2 )
          IF (.NOT. (GotIt .OR. GotIt2) ) CYCLE

          IF (InitializeSolution .AND. Solver % DoneTime > 1 .AND. BCPrediction) THEN 
            DO j=1,n
              k = IrrotVelPerm(NodeIndexes(j))
              DO p=1,dim
                Load((p-1)*2+1,j) = -1.0d0 * IrrotVel((k-1)*2*dim+(p-1)*2+1)
                Load((p-1)*2+2,j) = -1.0d0 * IrrotVel((k-1)*2*dim+(p-1)*2+2)
              END DO
            END DO
            NormalVelocity1(1:n) = 0.0d0
            NormalVelocity2(1:n) = 0.0d0
          ELSE
            DO j=1,n
              k = VPerm(NodeIndexes(j))
              DO p=1,dim
                Load((p-1)*2+1,j) = V((k-1)*2*dim+(p-1)*2+1)
                Load((p-1)*2+2,j) = V((k-1)*2*dim+(p-1)*2+2)
              END DO
            END DO
          END IF

!------------------------------------------------------------------------------
!         Get element local matrix and rhs vector
!------------------------------------------------------------------------------
          CALL LocalMatrixBoundary(  LocalStiffMatrix, LocalForce, &
              CurrentElement, n, ElementNodes)
!------------------------------------------------------------------------------
!         Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
          CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
              ForceVector, LocalForce, n, Solver % Variable % DOFs,  &
              PresPerm(NodeIndexes) )
!------------------------------------------------------------------------------
        END IF
      END DO
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------

    CALL FinishAssembly( Solver, ForceVector )

!------------------------------------------------------------------------------
!    Dirichlet BCs (boundary conditions for the normal component of the
!    traction vector)  
!------------------------------------------------------------------------------
    DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
        Solver % Mesh % NumberOfBulkElements +  &
        Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
      CurrentElement => Solver % Mesh % Elements(t)
      Model % CurrentElement => CurrentElement
!------------------------------------------------------------------------------
      DO i=1,Model % NumberOfBCs
        IF ( CurrentElement % BoundaryInfo % Constraint == &
            Model % BCs(i) % Tag ) THEN
!------------------------------------------------------------------------------
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
            
          IF ( ANY( PresPerm(NodeIndexes) == 0 ) ) CYCLE
          NoPenetrate = ListGetLogical( Model % BCs(i) % Values, &     
              'Prescribed Normal Velocity', GotIt )         
          IF (NoPenetrate) CYCLE

          IF (Visited == 1) THEN
            NormalForce1(1:n) = ListGetReal( Model % BCs(i) % Values, &
                'Re Normal Force', n, NodeIndexes, GotIt )
            NormalForce2(1:n) = ListGetReal( Model % BCs(i) % Values, &
                'Im Normal Force', n, NodeIndexes, GotIt2 )
            IF(.NOT. (GotIt .OR. GotIt2) ) CYCLE
          ELSE
            NormalForce1(1:n) = 0.0d0
            NormalForce2(1:n) = 0.0d0
          END IF

          WallTemperature = ListGetLogical( Model % BCs(i) % Values, &
              'Wall Temperature', GotIt )

          DO j=1,n
            k = PresPerm(NodeIndexes(j))
            IF ( k > 0 ) THEN
                
              ! For fixed wall temperature eliminate the known temperature
              IF(WallTemperature .OR. ANY(TempPerm(NodeIndexes) == 0) ) THEN
                SurfForce = DCMPLX(NormalForce1(j), NormalForce2(j))  
                SurfForce = SurfForce / (Lambda + 2.0 * Viscosity - &
                    DCMPLX(0,1.0) * Pressure0 / AngularFrequency)
                
                ! For free temperature its contribution needs to be taken into account
              ELSE
                SurfForce = DCMPLX(NormalForce1(j), NormalForce2(j))  + &
                    (Pressure0 / Temperature0) * &
                    DCMPLX( Temp(2*TempPerm(NodeIndexes(j))-1), Temp(2*TempPerm(NodeIndexes(j))) )
                SurfForce = SurfForce / (Lambda + 2.0 * Viscosity - &
                    DCMPLX(0,1.0) * GasRatio * Pressure0 / AngularFrequency)
              END IF

              s = StiffMatrix % Values(StiffMatrix % Diag(2*k-1))
              ForceVector(2*k-1) = s * REAL(SurfForce)
              CALL ZeroRow( StiffMatrix,2*k-1 )
              CALL SetMatrixElement( StiffMatrix,2*k-1,2*k-1, 1.0 * s )
                
              s = StiffMatrix % Values(StiffMatrix % Diag(2*k))
              ForceVector(2*k) = s * AIMAG(SurfForce)
              CALL ZeroRow( StiffMatrix,2*k )
              CALL SetMatrixElement( StiffMatrix,2*k,2*k, 1.0 * s )
                
            END IF
          END DO

        END IF

      END DO
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------

    CALL Info( 'Compressibility iteration', 'Assembly done', Level=4 )

!------------------------------------------------------------------------------
!    SOLVE THE FIRST EQUATION 
!------------------------------------------------------------------------------
    PrevNorm1 = Norm1

    CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
        Pres, Norm1, Solver % Variable % DOFs, Solver )

    IF (iter==1) THEN
      RelativeChange1 = 1.0d0      
    ELSE
      IF ( PrevNorm1 + Norm1 /= 0.0d0 ) THEN
        RelativeChange1 = 2*ABS(PrevNorm1 - Norm1) / (PrevNorm1 + Norm1)
      ELSE
        RelativeChange1 = 0.0d0
      END IF
    END IF

    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
    WRITE( Message, * ) 'Result Norm    : ',Norm1
    CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
    WRITE( Message, * ) 'Relative Change: ',RelativeChange1
    CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )

    IF (IterationConverged .OR. iter==NonlinearIter) EXIT

!------------------------------------------------------------------------------
!   PERFORM THE ASSEMBLY AND SOLUTION FOR THE SECOND EQUATION...        
!------------------------------------------------------------------------------

    LocalNodes = COUNT( TempPerm > 0 )
    IF ( LocalNodes <= 0 ) RETURN
    
    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', '-------------------------------------', Level=4 )
    WRITE( Message, * ) 'Heat conduction iteration', iter
    CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', '-------------------------------------', Level=4 )
    CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )

    CALL InitializeToZero( StiffMatrix, ForceVector )

!------------------------------------------------------------------------------
    DO t=1,Solver % NumberOfActiveElements
!------------------------------------------------------------------------------

      CurrentElement => Solver % Mesh % Elements( Solver % ActiveElements(t) )
      Model % CurrentElement => CurrentElement
      n = CurrentElement % TYPE % NumberOfNodes
      NodeIndexes => CurrentElement % NodeIndexes

      ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
      ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
      ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
!------------------------------------------------------------------------------
!     Get equation & material parameters
!------------------------------------------------------------------------------

      k = ListGetInteger( Model % Bodies( CurrentElement % &
          Bodyid ) % Values, 'Material', minv=1,maxv=Model % NumberOfMaterials )

      Material => Model % Materials(k) % Values

      GasRatio = ListGetConstReal( Material , 'Specific Heat Ratio')
      Pressure0 = ListGetConstReal( Material , 'Reference Pressure')
      Temperature0 = ListGetConstReal( Material , 'Equilibrium Temperature')
      Density = ListGetConstReal( Material , 'Equilibrium Density')
      Viscosity = ListGetConstReal( Material , 'Viscosity')
      Lambda = -2.0d0/3.0d0 * Viscosity
      BulkViscosity = ListGetConstReal( Material , 'Bulk Viscosity', GotIt)
      IF (GotIt) Lambda = BulkViscosity - 2.0d0/3.0d0 * Viscosity
      Conductivity = ListGetConstReal( Material , 'Heat Conductivity')
      HeatCapacity = ListGetConstReal( Material , 'Specific Heat')

!------------------------------------------------------------------------------
!     Get element local matrix and rhs vector
!------------------------------------------------------------------------------
      CALL LocalMatrix2(  LocalStiffMatrix, LocalForce, &
          CurrentElement, n, ElementNodes )
!------------------------------------------------------------------------------
!     Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
      CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
          ForceVector, LocalForce, n, TempVar % DOFs, &
          TempPerm(NodeIndexes) )
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!    Set the Dirichlet boundary conditions so that Temperature variation 
!    at wall vanishes.
!    NOTE: Vanishing heat flux conditions need not be handled explicitly.   
!------------------------------------------------------------------------------
    DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
        Solver % Mesh % NumberOfBulkElements +  &
        Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
      CurrentElement => Solver % Mesh % Elements(t)
      Model % CurrentElement => CurrentElement
!------------------------------------------------------------------------------
!     The element type 101 (point element) can only be used
!     to set Dirichlet BCs, so skip em at this stage.
!------------------------------------------------------------------------------
      IF ( CurrentElement % TYPE % ElementCode == 101 ) CYCLE
!------------------------------------------------------------------------------
      DO i=1,Model % NumberOfBCs
        IF ( CurrentElement % BoundaryInfo % Constraint == &
            Model % BCs(i) % Tag ) THEN
!------------------------------------------------------------------------------
          n = CurrentElement % TYPE % NumberOfNodes
          NodeIndexes => CurrentElement % NodeIndexes
            
          IF ( ANY( TempPerm(NodeIndexes) == 0 ) ) CYCLE
                     
          WallTemperature = ListGetLogical( Model % BCs(i) % Values, &
              'Wall Temperature', GotIt )
          IF(.NOT. WallTemperature) CYCLE

          DO j=1,n
            k = TempPerm(NodeIndexes(j))
            IF ( k > 0 ) THEN
              Twall1 = (Temperature0 * (GasRatio -1.0) / AngularFrequency) * &
                  Pres(2*PresPerm(NodeIndexes(j))) 

              s = StiffMatrix % Values(StiffMatrix % Diag(2*k-1))
              ForceVector(2*k-1) = s * Twall1 
              CALL ZeroRow( StiffMatrix,2*k-1 )
              CALL SetMatrixElement( StiffMatrix,2*k-1,2*k-1, 1.0 * s )
                
              Twall2 = -(Temperature0 * (GasRatio -1.0) / AngularFrequency) * &
                  Pres(2*PresPerm(NodeIndexes(j))-1) 
               
              s = StiffMatrix % Values(StiffMatrix % Diag(2*k))
              ForceVector(2*k) = s * Twall2
              CALL ZeroRow( StiffMatrix,2*k )
              CALL SetMatrixElement( StiffMatrix,2*k,2*k, 1.0 * s )
            END IF
          END DO

        END IF
      END DO
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------

     CALL Info( 'Heat conduction iteration', 'Assembly done', Level=4 )

!------------------------------------------------------------------------------
!    SOLVE THE SECOND EQUATION 
!------------------------------------------------------------------------------

     PrevNorm2 = Norm2

     CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
         Temp, Norm2, TempVar % DOFs, Solver )

     IF (iter==1) THEN
       RelativeChange2 = 1.0d0
     ELSE
       IF ( PrevNorm2 + Norm2 /= 0.0d0 ) THEN
         RelativeChange2 = 2*ABS(PrevNorm2 - Norm2) / (PrevNorm2 + Norm2)
       ELSE
         RelativeChange2 = 0.0d0
       END IF
     END IF

     CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
     WRITE( Message, * ) 'Result Norm    : ',Norm2
     CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
     WRITE( Message, * ) 'Relative Change: ',RelativeChange2
     CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )


     IF ( (RelativeChange1 < NonlinearTol) .AND. (RelativeChange2 < NonlinearTol) ) THEN
       IterationConverged = .TRUE.
     END IF
     
!------------------------------------------------------------------------------
   END DO ! of nonlinear iteration
!------------------------------------------------------------------------------
    ElapsedCPUTime  = CPUTime() - ElapsedCPUTime
    ElapsedTime = RealTime() - ElapsedTime

! Modify the norm of the solver variable so that the coupled system
! converge is obtained when the pressure change is small compared to
! the current pressure...  

  NULLIFY(PVar)
  PVar => VariableGet( Model % Variables, 'Pressure' )  
  IF ( .NOT. ASSOCIATED (Pvar)) THEN
    CALL Fatal('DissipativeHelmholtzSolver','Variable Pressure does not exist!')
  END IF

  LocalNodes = COUNT( PresPerm > 0 )

  IF (Visited==1) THEN
    RelaxationPar = 1.0d0
  ELSE
    Work(1:1) = ListGetConstReal( Solver % Values, &
        'Coupled System Relaxation Factor',GotIt )
    IF (GotIt) RelaxationPar = Work(1)
  END IF

  IF (Visited == 1) THEN
    Solver % Variable % Norm = 1
  ELSE
    Solver % Variable % Norm = RelaxationPar * Pressure0/AngularFrequency  * GasRatio * &
        SQRT( SUM( Pres(1:2*LocalNodes)**2) ) / &
        SQRT( SUM( PVar % Values(1:2*LocalNodes)**2) )
  END IF


  CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )
  WRITE(Message,'(a,F8.2)') ' Coupled system iteration (s):', ElapsedCPUTime
  CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 ) 
  WRITE( Message, * ) 'Estimated relative pressure change:   ',Solver % Variable % Norm
  CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
  WRITE( Message, * ) 'Using relaxation factor:              ',RelaxationPar
  CALL Info( 'DissipativeHelmholtzSolver', Message, Level=4 )
  CALL Info( 'DissipativeHelmholtzSolver', ' ', Level=4 )


  DO t = 1,Solver % Mesh % NumberOfNodes
    Pres(2*t-1) = RelaxationPar * Pres(2*t-1)
    Pres(2*t) = RelaxationPar * Pres(2*t)
    Temp(2*t-1) = RelaxationPar * Temp(2*t-1)
    Temp(2*t) = RelaxationPar * Temp(2*t)
  END DO


! Add the velocity for the SaveScalars subroutine
!    CALL ListAddConstReal( Model % Simulation, 'res: Frequency',AngularFrequency/(2*PI))    


 CONTAINS

!------------------------------------------------------------------------------
   SUBROUTINE LocalMatrix(  StiffMatrix, Force, Element, n, Nodes )
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: StiffMatrix(:,:), Force(:)
     INTEGER :: n
     TYPE(Nodes_t) :: Nodes
     TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3)
     REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,WaveNumber,M,D,L1,L2,r
     REAL(KIND=dp) :: DiffCoef(3,3), Velo(3)
     COMPLEX(KIND=dp) :: LSTIFF(n,n), LFORCE(n), A, B, ConvCoef, ImUnit
     LOGICAL :: Stat
     INTEGER :: i,p,q,t,DIM, NBasis, CoordSys
     TYPE(GaussIntegrationPoints_t) :: IntegStuff

     REAL(KIND=dp) :: X,Y,Z,Metric(3,3),SqrtMetric,Symb(3,3,3),dSymb(3,3,3,3)
     REAL(KIND=dp) :: R0R, R0I, R2R, R2I, T2R, T2I
!------------------------------------------------------------------------------
     DIM = CoordinateSystemDimension()
     CoordSys = CurrentCoordinateSystem()
     ImUnit = DCMPLX(0.0,1.0)

     Metric = 0.0d0
     Metric(1,1) = 1.0d0
     Metric(2,2) = 1.0d0
     Metric(3,3) = 1.0d0

     LSTIFF = 0.0d0
     LFORCE = 0.0d0
!------------------------------------------------------------------------------
!    Numerical integration
!------------------------------------------------------------------------------

     NBasis = n
     IntegStuff = GaussPoints( Element )

!------------------------------------------------------------------------------
!    All constants are fixed in this simple test case
!------------------------------------------------------------------------------
     R0R = 1.0
     R0I = 0.0
     R2R = -GasRatio * Pressure0 / (AngularFrequency**2.0 * Density)
     R2I = -(2.0*Viscosity + Lambda) / (AngularFrequency * Density)
     T2R = 0.0
     T2I = -Pressure0 / (AngularFrequency * Density * Temperature0)

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
       IF (CoordSys==AxisSymmetric) THEN
         r = SUM( Basis(1:n) * Nodes % x(1:n) )
         s = r * s * SqrtElementMetric
       ELSE 
         s = s * SqrtElementMetric
       END IF
!------------------------------------------------------------------------------
!      Stiffness matrix and load vector
!------------------------------------------------------------------------------
       DO p=1,NBasis
         DO q=1,NBasis

           A = DCMPLX( R0R, R0I ) * Basis(q) * Basis(p)
           B = 0

           DO i=1,DIM
             DO j=1,DIM
               A = A + Metric(i,j) * DCMPLX(R2R, R2I) * dBasisdx(q,i) * dBasisdx(p,j)
               B = B + Metric(i,j) * DCMPLX(T2R, T2I) * dBasisdx(q,i) * dBasisdx(p,j)
             END DO
           END DO

           LSTIFF(p,q) = LSTIFF(p,q) + s*A

           IF(ALL (TempPerm(NodeIndexes) /= 0) ) THEN
             LFORCE(p) = LFORCE(p) + s*B*Temp(2*TempPerm(NodeIndexes(q))-1)
             LFORCE(p) = LFORCE(p) + s*B*Temp(2*TempPerm(NodeIndexes(q))) * ImUnit
           END IF
         END DO

       END DO

     END DO

!------------------------------------------------------------------------------
    
     DO i=1,n
       Force( 2*(i-1)+1 ) = REAL( LFORCE(i) )
       Force( 2*(i-1)+2 ) = AIMAG( LFORCE(i) )

       DO j=1,n
         StiffMatrix( 2*(i-1)+1, 2*(j-1)+1 ) =  REAL( LSTIFF(i,j) )
         StiffMatrix( 2*(i-1)+1, 2*(j-1)+2 ) = -AIMAG( LSTIFF(i,j) )
         StiffMatrix( 2*(i-1)+2, 2*(j-1)+1 ) =  AIMAG( LSTIFF(i,j) )
         StiffMatrix( 2*(i-1)+2, 2*(j-1)+2 ) =  REAL( LSTIFF(i,j) )
       END DO
    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBoundary(  StiffMatrix, Force, Element, n, Nodes)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:),Force(:)
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,L1,L2
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3)
    REAL(KIND=dp) :: Normal(3), NormVelo1, NormVelo2, TempDer1, TempDer2, T1, T2
    REAL(KIND=dp) :: detJ, x(n), y(n), z(n), L(6), r
    COMPLEX(KIND=dp) :: LSTIFF(n,n), LFORCE(n), A, HH
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,DIM,CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()

    LSTIFF = 0.0d0
    LFORCE = 0.0d0
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
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
      stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
          Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )

      IF (CoordSys==AxisSymmetric) THEN
        r = SUM( Basis(1:n) * Nodes % x(1:n) )
        s = r * s * SqrtElementMetric
      ELSE 
        s = s * SqrtElementMetric
      END IF

      NormVelo1 = SUM(Basis(1:n) * NormalVelocity1(1:n))
      NormVelo2 = SUM(Basis(1:n) * NormalVelocity2(1:n))

      Normal = Normalvector(Element, Nodes, U, V, .TRUE.)
      DO i=1,2*DIM
        L(i) = SUM( Load(i,1:n) * Basis(1:n) )
      END DO
      DO i=1,dim
        NormVelo1 = NormVelo1 - L(2*(i-1)+1) * Normal(i) 
        NormVelo2 = NormVelo2 - L(2*(i-1)+2) * Normal(i)
      END DO

!------------------------------------------------------------------------------
      DO p=1,n
        LFORCE(p) = LFORCE(p) + s * Basis(p) * DCMPLX(NormVelo1, NormVelo2) 
      END DO
!------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------
    DO i=1,n
      Force( 2*(i-1)+1 ) =  REAL( LFORCE(i) )
      Force( 2*(i-1)+2 ) = AIMAG( LFORCE(i) )

      DO j=1,n
        StiffMatrix( 2*(i-1)+1, 2*(j-1)+1 ) =  REAL( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+1, 2*(j-1)+2 ) = -AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+2, 2*(j-1)+1 ) =  AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+2, 2*(j-1)+2 ) =  REAL( LSTIFF(i,j) )
      END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBoundary
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix2(  StiffMatrix, Force, Element, n, Nodes )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), Force(:)
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3)
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,WaveNumber,M,D,L1,L2
    REAL(KIND=dp) :: DiffCoef(3,3), Velo(3)
    COMPLEX(KIND=dp) :: LSTIFF(n,n), LFORCE(n), A, B, ConvCoef, ImUnit
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,DIM, NBasis, CoordSys
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    REAL(KIND=dp) :: X,Y,Z,Metric(3,3),SqrtMetric,Symb(3,3,3),dSymb(3,3,3,3)
    REAL(KIND=dp) :: T0R, T0I, T2R, T2I, R2R, R2I, R0R, R0I
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()
    ImUnit = DCMPLX(0.0,1.0)

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
!   All constants are fixed in this simple test case
!------------------------------------------------------------------------------
    T0R = 1.0
    T0I = 0.0
    T2R = 0.0
    T2I = -Conductivity / (AngularFrequency * Density * HeatCapacity)
    R0R = 0.0
    R0I = 0.0
    R2R = -Conductivity * Temperature0 * (GasRatio - 1.0) / &
        (AngularFrequency**2.0 * Density * HeatCapacity) 
    R2I = 0.0

!------------------------------------------------------------------------------
    DO t=1,IntegStuff % n

      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
      stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
          Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )

      s = s * SqrtElementMetric
      IF ( CoordSys /= Cartesian ) THEN
        X = SUM( Nodes % X(1:n) * Basis(1:n) )
        s = X * s
      END IF

!     Stiffness matrix and load vector
!      --------------------------------
      DO p=1,NBasis
        DO q=1,NBasis

          A = DCMPLX( T0R, T0I ) * Basis(q) * Basis(p)
          B = DCMPLX( R0R, R0I ) * Basis(q) * Basis(p) 

          DO i=1,DIM
            DO j=1,DIM
              A = A + Metric(i,j) * DCMPLX(T2R, T2I) * dBasisdx(q,i) * dBasisdx(p,j)
              B = B + Metric(i,j) * DCMPLX(R2R, R2I) * dBasisdx(q,i) * dBasisdx(p,j)
            END DO
          END DO

          LSTIFF(p,q) = LSTIFF(p,q) + s*A

          LFORCE(p) = LFORCE(p) + s*B*Pres(2*PresPerm(NodeIndexes(q))-1)
          LFORCE(p) = LFORCE(p) + s*B*Pres(2*PresPerm(NodeIndexes(q))) * ImUnit
        END DO
      END DO
      
    END DO
!------------------------------------------------------------------------------
    DO i=1,n
      Force( 2*(i-1)+1 ) = REAL( LFORCE(i) )
      Force( 2*(i-1)+2 ) = AIMAG( LFORCE(i) )

      DO j=1,n
        StiffMatrix( 2*(i-1)+1, 2*(j-1)+1 ) =  REAL( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+1, 2*(j-1)+2 ) = -AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+2, 2*(j-1)+1 ) =  AIMAG( LSTIFF(i,j) )
        StiffMatrix( 2*(i-1)+2, 2*(j-1)+2 ) =  REAL( LSTIFF(i,j) )
      END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix2
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE DissipativeHelmholtzSolver
!------------------------------------------------------------------------------



