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

SUBROUTINE IrrotationalVelocity( Model,Solver,dt,TransientSimulation )
  !DEC$ATTRIBUTES DLLEXPORT :: IrrotationalVelocity
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Compute the irrotational velocity!
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
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(Nodes_t) :: ElementNodes, ParentNodes
  TYPE(Element_t),POINTER :: CurrentElement, Parent
  TYPE(Variable_t), POINTER :: TVar, PVar, Var

  INTEGER, POINTER :: NodeIndexes(:)

  LOGICAL :: AllocationsDone = .FALSE., GotIt, Outlet, stat, &
      WallTemperature, NoSlip, NoPenetrate, Bubbles, InitializeSolution

  INTEGER, POINTER :: PresPerm(:), TempPerm(:), VeloPerm(:), IrrotVelPerm(:)
  REAL(KIND=dp), POINTER :: Pres(:), Temp(:), AbsVals(:), &
      RealTemp(:), RealPres(:), ForceVector(:), TrueVelocity(:), IrrotVel(:)
  REAL(KIND=dp), POINTER :: Velo(:)

  INTEGER :: iter, i, j, k, l, n, t, pn, istat, eq, LocalNodes, Visited = 0, &
      DIM, AbsDofs, TangentDirection, Dofs, PrevDoneTime = 0, CoordSys
  REAL(KIND=dp) :: Norm, PrevNorm, RelativeChange, AngularFrequency, &
      GasRatio, Temperature0, Pressure0, f1, f2, dx, dy, dz
  
  TYPE(ValueList_t), POINTER :: Material

  INTEGER :: NonlinearIter
  REAL(KIND=dp) :: NonlinearTol,s

  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), Work(:), &
       LocalForce(:), NormalVelocity(:)
  REAL(KIND=dp) :: Viscosity, BulkViscosity, Density, Conductivity, Lambda

  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, VariableName

  SAVE LocalStiffMatrix, Work, LocalForce, ElementNodes, &
      NormalVelocity, AllocationsDone, Visited, ParentNodes, PrevDoneTime

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------
  InitializeSolution = .FALSE.  
  IF (TransientSimulation) THEN
    IF (PrevDoneTime /= Solver % DoneTime) THEN
      Visited = 0
      InitializeSolution = .TRUE.
      PrevDoneTime = Solver % DoneTime 
    END IF
  END IF

  Visited = Visited + 1
  DIM = CoordinateSystemDimension()
  CoordSys = CurrentCoordinateSystem()
  IF (.NOT. (CoordSys==Cartesian .OR. CoordSys==AxisSymmetric) ) THEN
    CALL Fatal( 'ShearWaveSolver', 'Illegal coordinate system' )
  END IF

  IF ( .NOT.ASSOCIATED( Solver % Matrix ) ) RETURN
  Solver % Matrix % COMPLEX = .FALSE.

  Var => Solver % variable
  Velo => Solver % Variable % Values
  VeloPerm => Solver % Variable % Perm
  Dofs = Solver % Variable % DOFs

  NULLIFY(Tvar)
  TVar => VariableGet( Model % Variables, 'AbsVals' )  
  IF (ASSOCIATED (Tvar)) THEN
    AbsDofs = Tvar % Dofs
    AbsVals     => Tvar % Values
    AbsVals = 0.0
  ELSE
    AbsDofs = 0
    CALL Warn('IrrotationalVelocity','Variable AbsVals does not exist!')
  END IF

  NULLIFY(TVar)
  TVar => VariableGet( Model % Variables, 'TempCond' )  
  IF (ASSOCIATED (Tvar)) THEN
    Temp     => Tvar % Values
    TempPerm => Tvar % Perm
  ELSE
    CALL Fatal('IrrotationalVelocity','Variable TempCond does not exist!')
  END IF
 
  NULLIFY(PVar)
  TVar => VariableGet( Model % Variables, 'DivVelo' )  
  IF (ASSOCIATED (Tvar)) THEN
    Pres     => Tvar % Values
    PresPerm => Tvar % Perm
  ELSE
    CALL Fatal('IrrotationalVelocity','Variable DivVelo does not exist!')
  END IF
  
  LocalNodes = COUNT( VeloPerm > 0 )
  IF ( LocalNodes <= 0 ) RETURN

  LocalNodes = COUNT( PresPerm > 0 )
  IF ( LocalNodes <= 0 ) RETURN

  LocalNodes = COUNT( TempPerm > 0 )
  IF ( LocalNodes <= 0 ) RETURN

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS

  Norm = Solver % Variable % Norm


!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone) THEN
     N = Solver % Mesh % MaxElementNodes

     ALLOCATE( ElementNodes % x( N ),  &
          ElementNodes % y( N ),       &
          ElementNodes % z( N ),       &
          ParentNodes % x( N ),  &
          ParentNodes % y( N ),       &
          ParentNodes % z( N ),       &
          LocalForce( Dofs*N ),           &
          NormalVelocity(N),           &
          Work( N ),                   &
          LocalStiffMatrix( Dofs*N,Dofs*N ), &
          STAT=istat )

     IF ( istat /= 0 ) THEN
        CALL Fatal( 'HelmholzVelocity', 'Memory allocation error.' )
     END IF

     LocalStiffMatrix = 0.0
     LocalForce = 0.0

     AllocationsDone = .TRUE.
  END IF
!------------------------------------------------------------------------------

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
      CALL Fatal('IrrotationalVelocity','Frequency for simulation must be given!')      
    END IF
  END IF
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Do some additional initialization, and go for it
!------------------------------------------------------------------------------
  NonlinearTol = ListGetConstReal( Solver % Values, &
       'Nonlinear System Convergence Tolerance',GotIt )

  NonlinearIter = ListGetInteger( Solver % Values, &
       'Nonlinear System Max Iterations', GotIt )
  IF ( .NOT.GotIt ) NonlinearIter = 1

  Bubbles = ListGetLogical( Solver % Values,'Bubbles',GotIt )
  Bubbles = .FALSE.

  EquationName = ListGetString( Solver % Values, 'Equation' )
  

!------------------------------------------------------------------------------
  CALL Info( 'IrrotationalVelocity', ' ', Level=4 )
  CALL Info( 'IrrotationalVelocity', '-------------------------------------', Level=4 )
  WRITE( Message, * ) 'Irrotational velocity computation'
  CALL Info( 'IrrotationalVelocity', Message, Level=4 )
  CALL Info( 'IrrotationalVelocity', '-------------------------------------', Level=4 )
  CALL Info( 'IrrotationalVelocity', ' ', Level=4 )
  
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
!       Get equation & material parameters
!------------------------------------------------------------------------------

    k = ListGetInteger( Model % Bodies( CurrentElement % &
        Bodyid ) % Values, 'Material', minv=1,maxv=Model % NumberOfMaterials )
    
    Material => Model % Materials(k) % Values
    
    GasRatio = ListGetConstReal( Material , 'Specific Heat Ratio')
    Pressure0 = ListGetConstReal( Material , 'Reference Pressure')
    Temperature0 = ListGetConstReal( Material , 'Equilibrium Temperature')
    Density = ListGetConstReal( Material , 'Equilibrium Density')
    Viscosity = ListGetConstReal( Material , 'Viscosity',GotIt)
    Lambda = -2.0d0/3.0d0 * Viscosity
    BulkViscosity = ListGetConstReal( Material , 'Bulk Viscosity', GotIt)
    IF (GotIt) Lambda = BulkViscosity - 2.0d0/3.0d0 * Viscosity
    Conductivity = ListGetConstReal( Material , 'Heat Conductivity')
      
!------------------------------------------------------------------------------
!       Get element local matrix and rhs vector
!------------------------------------------------------------------------------

    CALL LocalMatrix(  LocalStiffMatrix, LocalForce, &
        CurrentElement, n, ElementNodes )

!------------------------------------------------------------------------------
!   Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
    CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
        ForceVector, LocalForce, n, Dofs, VeloPerm(NodeIndexes) )
!------------------------------------------------------------------------------

  END DO

!  CALL FinishAssembly( Solver, ForceVector )

  CALL Info( 'IrrotationalVelocity', 'Assembly done', Level=4 )

!---------------------------------------------------------------------------
!    Solve the system and we are done:
!---------------------------------------------------------------------------


  PrevNorm = Norm
  
  CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
      Velo, Norm, Dofs, Solver )
  
!------------------------------------------------------------------------------

  CALL Info( 'IrrotationalVelocity', ' ', Level=4 )
  WRITE( Message, * ) 'Result Norm    : ',Norm
  CALL Info( 'IrrotationalVelocity', Message, Level=4 )
  CALL Info( 'IrrotationalVelocity', ' ', Level=4 )

!----------------------------------------------------------------------------
! Modify the solver norm so that the norm of this solution is not used to
! determine whether the solution is converged or not  
!----------------------------------------------------------------------------

     Norm = 5.0d-1
  
!------------------------------------------------------------------------------
! Finally do some postprocessing and compute the abs of 
! velocity, pressure and temperature, and also the real 
! temperature and pressure fields.
! Assumes that all the permutation vectors are the same!   
!--------------------------------------------------------------------------------
  
  IF(AbsDofs > 0) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      AbsVals(AbsDofs*(t-1)+1) = SQRT(Velo(4*t-3)**2.0 + Velo(4*t-2)**2.0 + &
          Velo(4*t-1)**2.0 + Velo(4*t)**2.0 )
    END DO
  END IF
  
  NULLIFY(PVar)
  PVar => VariableGet( Model % Variables, 'Pressure' )  
  IF (ASSOCIATED (Pvar)) THEN
    RealPres     => Pvar % Values
  ELSE
    NULLIFY(RealPres)
    CALL Warn('IrrotationalVelocity','Variable Pressure for postprocessing does not exist!')
  END IF

  IF (InitializeSolution .AND. ASSOCIATED(RealPres)) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      RealPres(2*t-1) = 0.0d0
      RealPres(2*t) = 0.0d0
    END DO
  END IF


  DO t = 1,Solver % Mesh % NumberOfNodes
    f1 = (Pressure0/Temperature0 )* Temp(2*t-1) - &
        (GasRatio*Pressure0/AngularFrequency) * Pres(2*t)
    f2 = (Pressure0/Temperature0 )* Temp(2*t) + &
        (GasRatio*Pressure0/AngularFrequency) * Pres(2*t-1)
    IF(ASSOCIATED(RealPres)) THEN
      RealPres(2*t-1) = RealPres(2*t-1) + f1
      RealPres(2*t) = RealPres(2*t) + f2
    END IF
    IF(AbsDofs > 1) THEN
      AbsVals(AbsDofs*(t-1)+2) = SQRT(f1*f1+f2*f2)
    END IF
  END DO

  NULLIFY(PVar)
  PVar => VariableGet( Model % Variables, 'Temperature' )  
  IF (ASSOCIATED (Pvar)) THEN
    RealTemp     => Pvar % Values
  ELSE 
    NULLIFY(RealTemp)
    CALL Warn('IrrotationalVelocity','Variable Temperature for postprocessing does not exist!')
  END IF

  DO t = 1,Solver % Mesh % NumberOfNodes
    f1 = Temp(2*t-1) - (Temperature0*(GasRatio-1.0)/AngularFrequency) * Pres(2*t)
    f2 = Temp(2*t) + (Temperature0*(GasRatio-1.0)/AngularFrequency) * Pres(2*t-1)
    IF(ASSOCIATED(RealTemp)) THEN
      RealTemp(2*t-1) = RealTemp(2*t-1) + f1
      RealTemp(2*t) = RealTemp(2*t) + f2
    END IF
    IF(AbsDofs > 2) THEN
      AbsVals(AbsDofs*(t-1)+3) = SQRT(f1*f1+f2*f2)
    END IF
  END DO

  NULLIFY(PVar)
  PVar => VariableGet( Model % Variables, 'TotVel' )  
  IF (ASSOCIATED (Pvar)) THEN
    TrueVelocity => Pvar % Values
  ELSE 
    NULLIFY(TrueVelocity)
    CALL Warn('IrrotationalVelocity','Variable TotVel for postprocessing does not exist!')
  END IF

  IF(ASSOCIATED(TrueVelocity)) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      DO i = 1, dim
        TrueVelocity((t-1)*2*dim+(i-1)*2+1) = TrueVelocity((t-1)*2*dim+(i-1)*2+1) + &
            Velo((t-1)*2*dim+(i-1)*2+1) 
        TrueVelocity((t-1)*2*dim+(i-1)*2+2) = TrueVelocity((t-1)*2*dim+(i-1)*2+2) + &
            Velo((t-1)*2*dim+(i-1)*2+2) 
      END DO
    END DO
  END IF

  NULLIFY(PVar)
  PVar => VariableGet( Model % Variables, 'IrrotVel' )  
  IF (ASSOCIATED (Pvar)) THEN
    IrrotVel => Pvar % Values
  ELSE 
    NULLIFY(IrrotVel)
    CALL Warn('IrrotationalVelocity','Variable IrrotVel for postprocessing does not exist!')
  END IF

  IF (InitializeSolution .AND. ASSOCIATED(IrrotVel)) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      DO i = 1, dim
        IrrotVel((t-1)*2*dim+(i-1)*2+1) = 0.0d0
        IrrotVel((t-1)*2*dim+(i-1)*2+2) = 0.0d0
      END DO
    END DO
  END IF

  IF(ASSOCIATED(IrrotVel)) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      DO i = 1, dim
        IrrotVel((t-1)*2*dim+(i-1)*2+1) = IrrotVel((t-1)*2*dim+(i-1)*2+1) + &
            Velo((t-1)*2*dim+(i-1)*2+1) 
        IrrotVel((t-1)*2*dim+(i-1)*2+2) = IrrotVel((t-1)*2*dim+(i-1)*2+2) + &
            Velo((t-1)*2*dim+(i-1)*2+2) 
      END DO
    END DO
  END IF



CONTAINS


!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  StiffMatrix, Force, Element, n, Nodes )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), Force(:)
    INTEGER :: n
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(2*n),dBasisdx(2*n,3),ddBasisddx(n,3,3)
    REAL(KIND=dp) :: SqrtElementMetric,U,V,W,S,M,D,L1,L2
    COMPLEX(KIND=dp) :: LSTIFF(4*n,4*n), LFORCE(4*n), ImUnit
    LOGICAL :: Stat
    INTEGER :: i,p,q,t,l,DIM, NBasis, CoordSys, axis
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    REAL(KIND=dp) :: X,Y,Z,Metric(3,3),SqrtMetric,Symb(3,3,3),dSymb(3,3,3,3)
    REAL(KIND=dp) :: DerTempR, DerTempI, DerPresR, DerPresI, PresR, PresI  
    REAL(KIND=dp) :: V0R, V0I, V2R, V2I, T1R, T1I, R1R, R1I
!------------------------------------------------------------------------------
    DIM = CoordinateSystemDimension()
    CoordSys = CurrentCoordinateSystem()
    ImUnit = DCMPLX(0.0,1.0)

    Metric = 0.0d0
    Metric(1,1) = 1.0d0
    Metric(2,2) = 1.0d0
    Metric(3,3) = 1.0d0

!------------------------------------------------------------------------------
!   Numerical integration
!------------------------------------------------------------------------------
    IF ( Bubbles ) THEN
      IntegStuff = GaussPoints( Element, Element % TYPE % GaussPoints2 )
      NBasis = 2*n
    ELSE
      NBasis = n
      IntegStuff = GaussPoints( Element )
    END IF

!------------------------------------------------------------------------------
!   All constants are fixed in this simple test case
!------------------------------------------------------------------------------
    V0R = 1.0
    V0I = 0.0
    T1R = 0.0
    T1I = Pressure0 / (AngularFrequency * Density * Temperature0)
    R1R = -Pressure0 * GasRatio / (AngularFrequency**2.0 * Density)
    R1I = -(2.0*Viscosity + Lambda) / (AngularFrequency * Density)

!------------------------------------------------------------------------------

    LSTIFF = 0.0d0
    LFORCE = 0.0d0

    DO t=1,IntegStuff % n
      
      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
      stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
          Basis, dBasisdx, ddBasisddx, .FALSE., Bubbles )

      s = s * SqrtElementMetric
      IF ( CoordSys /= Cartesian ) THEN
        X = SUM( Nodes % X(1:n) * Basis(1:n) )
        s = s * X
      END IF
!------------------------------------------------------------------------------
!      The source term and the coefficient of the time derivative and 
!      diffusion terms at the integration point
!------------------------------------------------------------------------------
      PresR = SUM(Basis(1:n) * Pres(2*PresPerm(NodeIndexes(1:n))-1) )
      PresI = SUM(Basis(1:n) * Pres(2*PresPerm(NodeIndexes(1:n))) )

      DO axis = 0,DIM-1

        DerTempR = SUM (dBasisdx(1:n,axis+1) * Temp(2*TempPerm(NodeIndexes(1:n))-1) ) 
        DerTempI = SUM (dBasisdx(1:n,axis+1) * Temp(2*TempPerm(NodeIndexes(1:n))) ) 
        DerPresR = SUM (dBasisdx(1:n,axis+1) * Pres(2*PresPerm(NodeIndexes(1:n))-1) ) 
        DerPresI = SUM (dBasisdx(1:n,axis+1) * Pres(2*PresPerm(NodeIndexes(1:n))) ) 

!      Stiffness matrix and load vector
!      --------------------------------
        DO p=1,NBasis
          DO q=1,NBasis            
            LSTIFF(DIM*p-1+axis,DIM*q-1+axis) = LSTIFF(DIM*p-1+axis,DIM*q-1+axis) + &
                DCMPLX( V0R, V0I ) * Basis(q) * Basis(p) * s
          END DO
          
          LFORCE(DIM*p-1+axis) = LFORCE(DIM*p-1+axis) + &
              DCMPLX(T1R,T1I) * DCMPLX(DerTempR,DerTempI) * Basis(p) * s + &
              DCMPLX(R1R,R1I) * DCMPLX(DerPresR,DerPresI) * Basis(p) * s

        END DO

      END DO
!------------------------------------------------------------------------------
      
    END DO

    IF ( Bubbles ) THEN
      CALL LCondensate( n,DIM,LSTIFF,LFORCE )
    END IF

    DO p=1,n
      DO i=1,DIM
        Force( 2*DIM*(p-1)+2*i-1 ) = REAL(LFORCE( (p-1)*DIM+i ))
        Force( 2*DIM*(p-1)+2*i ) = AIMAG(LFORCE( (p-1)*DIM+i ))
        DO q=1,n
          DO j=1,DIM+2
            StiffMatrix( 2*DIM*(p-1)+2*i-1, 2*DIM*(q-1)+2*j-1 ) =  &
                REAL( LSTIFF(DIM*(p-1)+i,DIM*(q-1)+j) )
            StiffMatrix( 2*DIM*(p-1)+2*i-1, 2*DIM*(q-1)+2*j ) =  &
                -AIMAG( LSTIFF(DIM*(p-1)+i,DIM*(q-1)+j) )
            StiffMatrix( 2*DIM*(p-1)+2*i, 2*DIM*(q-1)+2*j-1 ) =  &
                AIMAG( LSTIFF(DIM*(p-1)+i,DIM*(q-1)+j) )
            StiffMatrix( 2*DIM*(p-1)+2*i, 2*DIM*(q-1)+2*j ) =  &
                REAL( LSTIFF(DIM*(p-1)+i,DIM*(q-1)+j) )
          END DO
        END DO
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
    REAL(KIND=dp) :: ParentBasis(pn), ParentdBasisdx(pn,3)
    REAL(KIND=dp) :: Normal(3), NormVelo, PresR, PresI, T1, T2
    REAL(KIND=dp) :: detJ, x(n), y(n), z(n)
    COMPLEX(KIND=dp) :: LSTIFF(n,n), LFORCE(n), A, CC
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
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, U, V, W, SqrtElementMetric, &
              Basis, dBasisdx, ddBasisddx, .FALSE., .FALSE. )

       s = s * SqrtElementMetric

       PresR = SUM (Basis(1:n) * Pres(2*PresPerm(NodeIndexes(1:n))-1) ) 
       PresI = SUM (Basis(1:n) * Pres(2*PresPerm(NodeIndexes(1:n))) ) 

       CC = DCMPLX(0,1.0) * Viscosity / ( AngularFrequency * Density) &
           * DCMPLX(PresR, PresI)
 
!------------------------------------------------------------------------------
       DO p=1,n
         DO q=1,n
!           LSTIFF(p,q) = LSTIFF(p,q) + s * DCMPLX(0, L2) * Basis(q)*Basis(p)
         END DO
         LFORCE(p) = LFORCE(p) + s * Basis(p) * CC
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
  SUBROUTINE LCondensate( n, DIM, K, F )
!------------------------------------------------------------------------------
    USE LinearAlgebra
!------------------------------------------------------------------------------
    INTEGER :: n, DIM
    COMPLEX(KIND=dp) :: K(:,:), F(:), Kbb(DIM*n,DIM*n), &
         Kbl(DIM*n,DIM*n), Klb(DIM*n,DIM*n), Fb(n*DIM)

    INTEGER :: i, Ldofs(DIM*n), Bdofs(DIM*n)

    Ldofs = (/ (i, i=1,DIM*n) /)
    Bdofs = (/ (DIM*n+i, i=1, DIM*n) /)

    Kbb = K(Bdofs,Bdofs)
    Kbl = K(Bdofs,Ldofs)
    Klb = K(Ldofs,Bdofs)
    Fb  = F(Bdofs)

    CALL ComplexInvertMatrix( Kbb,n )

    F(1:DIM*n) = F(1:DIM*n) - MATMUL( Klb, MATMUL( Kbb, Fb  ) )
    K(1:DIM*n,1:DIM*n) = K(1:DIM*n,1:DIM*n) - MATMUL( Klb, MATMUL( Kbb, Kbl ) )
!------------------------------------------------------------------------------
  END SUBROUTINE LCondensate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
END SUBROUTINE IrrotationalVelocity
!------------------------------------------------------------------------------
