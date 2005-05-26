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
! *                Modified by: Mika Malinen
! *
! *       Date of modification: 04 Sep 2003
! *
! ****************************************************************************/

!------------------------------------------------------------------------------
SUBROUTINE ShearWaveSolver( Model,Solver,dt,TransientSimulation )
  !DEC$ATTRIBUTES DLLEXPORT :: ShearWaveSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the transverse part of the velocity
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
  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement, Parent
  TYPE(ValueList_t), POINTER :: Material
  TYPE(Variable_t), POINTER :: VVar, PVar

  INTEGER, POINTER :: NodeIndexes(:), FlowPerm(:), VPerm(:)
  INTEGER :: i, j, k, n, p, t, istat, LocalNodes, Dofs, &
      VelocityComponents, VelocityDofs, CoordSys, TangentDirection, &
      Visited = 0

  LOGICAL :: AllocationsDone = .FALSE., Bubbles, GotIt, stat, &
      VisitedNodes(Model % NumberOfNodes), NoSlip
  CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, VariableName

  COMPLEX(KIND=dp) :: A 

  REAL(KIND=dp), POINTER :: Flow(:), ForceVector(:), V(:), TrueVelocity(:)
  REAL(KIND=dp) :: Norm, PrevNorm, RelativeChange, AngularFrequency
  REAL(KIND=dp) :: NonlinearTol, at, at0, totat, st, totst, CPUTime, &
      RealTime, Normal(3), ReVelo(3), ImVelo(3), ReNormalVelo, &
      ImNormalVelo, tmp, s, dx, dy

  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:), Load(:,:), &
      HeatSource(:,:), temp(:), &
      LocalForce(:), SoundSpeed(:), SpecificHeat(:), HeatRatio(:), &
      Density(:), Pressure(:), Temperature(:), Conductivity(:), &
      Viscosity(:), Lambda(:), BulkViscosity(:), Impedance(:,:)

  SAVE LocalStiffMatrix, temp, Load, HeatSource,  LocalForce, ElementNodes, &
       SoundSpeed, SpecificHeat, HeatRatio, Density, Pressure, &
       Temperature, Conductivity, Viscosity, Lambda, BulkViscosity, &
       Impedance, AllocationsDone, Visited

!------------------------------------------------------------------------------
! Get variables needed for solution
!------------------------------------------------------------------------------
  IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN
  Solver % Matrix % COMPLEX = .FALSE.

  Flow     => Solver % Variable % Values
  FlowPerm => Solver % Variable % Perm

  LocalNodes = COUNT( FlowPerm > 0 )
  IF ( LocalNodes <= 0 ) RETURN

  StiffMatrix => Solver % Matrix
  ForceVector => StiffMatrix % RHS
  Norm = Solver % Variable % Norm
  Dofs = Solver % Variable % DOFs
  VelocityComponents = CoordinateSystemDimension()
  VelocityDofs = VelocityComponents*2
  IF (Dofs /= VelocityDofs + 2) THEN
    CALL Warn('ShearWaveSolver', 'Inconsistent number of Variable Dofs')
  END IF

!------------------------------------------------------------------------------
! Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
    N = Solver % Mesh % MaxElementNodes
    
    IF ( AllocationsDone ) THEN
      DEALLOCATE(                &
          ElementNodes % x,      &
          ElementNodes % y,      &
          ElementNodes % z,      &
          LocalForce,            &
          temp,                  &
          LocalStiffMatrix,      &
          SoundSpeed, Load,      &
          HeatSource,            &
          SpecificHeat,          &
          HeatRatio,             &
          Density,               &
          Pressure,              &
          Temperature,           &
          Conductivity,          &
          Viscosity,             &
          Lambda,                &
          BulkViscosity,         &
          Impedance )           
    END IF
    
    ALLOCATE( ElementNodes % x( N ),       &
        ElementNodes % y( N ),             &
        ElementNodes % z( N ),             &
        LocalForce( Dofs*N ),              &
        temp( N ),                         &
        LocalStiffMatrix( Dofs*N,Dofs*N ), &
        SoundSpeed( N ),             &
        Load( 6,N ),                 &
        HeatSource(2,N),             &
        SpecificHeat(N),             &
        HeatRatio(N),                &
        Density(N),                  &
        Pressure(N),                 &
        Temperature(N),              &
        Conductivity(N),             &
        Viscosity(N),                &
        Lambda(N),                   &
        BulkViscosity(N),            &
        Impedance( 4,N ),            &
        STAT=istat )

    IF ( istat /= 0 ) THEN
      CALL Fatal( 'ShearWaveSolver', 'Memory allocation error.' )
    END IF

    AllocationsDone = .TRUE.
  END IF

!------------------------------------------------------------------------------
! Do some additional initialization, and go for it
!------------------------------------------------------------------------------
  Visited = Visited + 1 
  EquationName = ListGetString( Solver % Values, 'Equation' )
!  Bubbles = ListGetLogical( Solver % Values, 'Bubbles', GotIt )
  Bubbles = .TRUE.
  CoordSys = CurrentCoordinateSystem()
  IF (.NOT. (CoordSys==Cartesian .OR. CoordSys==AxisSymmetric) ) THEN
    CALL Fatal( 'ShearWaveSolver', 'Illegal coordinate system' )
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
    CALL Fatal('ShearWaveSolver','Variable TotVel does not exist!')
  END IF

!------------------------------------------------------------------------------
! Figure out angular frequency:
!------------------------------------------------------------------------------
  AngularFrequency = 0.0d0
  NodeIndexes => Solver % Mesh % Elements(1) % NodeIndexes
  temp(1:1) = ListGetReal( Model % Simulation, &
     'Angular Frequency', 1, NodeIndexes, GotIt )
  IF ( GotIt ) THEN
    AngularFrequency = temp(1)
  ELSE
    temp(1:1) = ListGetReal( Model % Simulation, &
        'Frequency', 1, NodeIndexes, GotIt)
    IF (GotIt) THEN
      AngularFrequency = 2*PI*temp(1)
    ELSE
      CALL Fatal('ShearWaveSolver','Frequency for simulation must be given!')      
    END IF
  END IF
!------------------------------------------------------------------------------

  totat = 0.0d0
  totst = 0.0d0
  at  = CPUTime()
  at0 = RealTime()

  CALL Info( 'ShearWaveSolver', ' ', Level=4 )
  CALL Info( 'ShearWaveSolver', '-------------------------------------', &
      Level=4 )
  WRITE( Message, * ) 'Frequency (Hz): ', AngularFrequency/(2*PI)
  CALL Info( 'ShearWaveSolver', Message, Level=4 )
  CALL Info( 'ShearWaveSolver', '-------------------------------------', &
      Level=4 )
  CALL Info( 'ShearWaveSolver', ' ', Level=4 )
  CALL Info( 'ShearWaveSolver', 'Starting Assembly', Level=4 )
  
  CALL InitializeToZero( StiffMatrix, ForceVector )

!------------------------------------------------------------------------------
  DO t=1,Solver % Mesh % NumberOfBulkElements
!------------------------------------------------------------------------------
    IF ( RealTime() - at0 > 1.0 ) THEN
      WRITE(Message,'(a,i3,a)' ) '   Assembly: ', INT(100.0 - 100.0 * &
          (Solver % Mesh % NumberOfBulkElements-t) / &
          (Solver % Mesh % NumberOfBulkElements)), ' % done'
      CALL Info( 'ShearWaveSolver', Message, Level=5 )
      at0 = RealTime()
    END IF
!------------------------------------------------------------------------------
!   Check if this element belongs to a body where this equation
!   should be computed
!------------------------------------------------------------------------------
    CurrentElement => Solver % Mesh % Elements(t)
    IF ( .NOT. CheckElementEquation( Model, &
        CurrentElement, EquationName ) ) CYCLE
!------------------------------------------------------------------------------
    Model % CurrentElement => CurrentElement
    n = CurrentElement % TYPE % NumberOfNodes
    NodeIndexes => CurrentElement % NodeIndexes
    ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
    ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
    ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)

!------------------------------------------------------------------------------
!   Get equation & material parameters
!------------------------------------------------------------------------------

    k = ListGetInteger( Model % Bodies( CurrentElement % &
        Bodyid ) % Values, 'Material' )
    Material => Model % Materials(k) % Values

    SpecificHeat(1:n) = ListGetReal( Material, 'Specific Heat', &
        n, NodeIndexes )
    HeatRatio(1:n) = ListGetReal( Material, 'Specific Heat Ratio', &
        n, NodeIndexes )
    Density(1:n) = ListGetReal( Material, 'Equilibrium Density', &
        n, NodeIndexes )
    Pressure(1:n) = ListGetReal( Material, 'Reference Pressure', &
        n, NodeIndexes )
    Temperature(1:n) = ListGetReal( Material, 'Equilibrium Temperature', &
        n, NodeIndexes )        
    Conductivity(1:n) = ListGetReal( Material, 'Heat Conductivity', &
        n, NodeIndexes )   
    Viscosity(1:n) = ListGetReal( Material, 'Viscosity', &
        n, NodeIndexes )   
    Lambda = -2.0d0/3.0d0 * Viscosity
    BulkViscosity(1:n) = ListGetReal( Material, ' Bulk Viscosity', &
        n, NodeIndexes, GotIt )
    IF (GotIt) Lambda = BulkViscosity - 2.0d0/3.0d0 * Viscosity

!------------------------------------------------------------------------------
!   The heat source and body force at nodes
!------------------------------------------------------------------------------
    Load = 0.0d0
    HeatSource = 0.0d0
    k = ListGetInteger( Model % Bodies( CurrentElement % BodyId ) % &
                       Values, 'Body Force', GotIt )
    IF ( k > 0 ) THEN
      Load(1,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Re Body Force 1', n, NodeIndexes, GotIt )
      Load(2,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Im Body Force 1', n, NodeIndexes, GotIt )
      Load(3,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Re Body Force 2', n, NodeIndexes, GotIt )
      Load(4,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Im Body Force 2', n, NodeIndexes, GotIt )
      Load(5,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Re Body Force 3', n, NodeIndexes, GotIt )
      Load(6,1:n) = ListGetReal( Model % BodyForces(k) % Values, &
          'Im Body Force 3', n, NodeIndexes, GotIt )
    END IF

!------------------------------------------------------------------------------
!   Get element local matrix and rhs vector
!------------------------------------------------------------------------------
    IF (CoordSys==Cartesian) THEN
      CALL LocalMatrix(  LocalStiffMatrix, LocalForce, AngularFrequency, &
          SpecificHeat, HeatRatio, Density, Pressure,                    &
          Temperature, Conductivity, Viscosity, Lambda,                  &
          HeatSource, Load, Bubbles, CurrentElement, n, ElementNodes,    &
          Dofs)
    ELSE
      CALL LocalMatrix2(  LocalStiffMatrix, LocalForce, AngularFrequency, &
          SpecificHeat, HeatRatio, Density, Pressure,                    &
          Temperature, Conductivity, Viscosity, Lambda,                  &
          HeatSource, Load, Bubbles, CurrentElement, n, ElementNodes,    &
          Dofs)
    END IF
      
!------------------------------------------------------------------------------
!   Update global matrix and rhs vector from local matrix & vector
!------------------------------------------------------------------------------
    CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
        ForceVector, LocalForce, n, Dofs, FlowPerm(NodeIndexes) )
!------------------------------------------------------------------------------
     END DO
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
     CALL FinishAssembly( Solver, ForceVector )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!   Dirichlet BCs: 
!------------------------------------------------------------------------------
!   Set the boundary values of the velocities so that the tangential components
!   of the irrotational velocity are cancelled out. It is assumed that 
!   the element sides are aligned with the coordinate lines.
!   NOTE: the boundary conditions for the (tangential) surface force cannot be
!         treated currently
!----------------------------------------------------------------------------
     DO t = Solver % Mesh % NumberOfBulkElements + 1,  &
         Solver % Mesh % NumberOfBulkElements +  &
         Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
       CurrentElement => Solver % Mesh % Elements(t)
       Model % CurrentElement => CurrentElement
       DO i=1,Model % NumberOfBCs
         IF ( CurrentElement % BoundaryInfo % Constraint == &
             Model % BCs(i) % Tag ) THEN
           n = CurrentElement % TYPE % NumberOfNodes
           NodeIndexes => CurrentElement % NodeIndexes
           IF ( ANY( FlowPerm(NodeIndexes) == 0 ) ) CYCLE  
           NoSlip = ListGetLogical( Model % BCs(i) % Values, &     
               'No Slip Condition', GotIt )         
           IF ( .NOT. NoSlip) CYCLE

           !-------------------------------------
           ! Find out the tangential direction...
           !--------------------------------------

           ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
           ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
           ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)  
           dx = MAXVAL(ElementNodes % x(1:n)) - MINVAL(ElementNodes % x(1:n))
           dy = MAXVAL(ElementNodes % y(1:n)) - MINVAL(ElementNodes % y(1:n))
           IF(dx > 100 * dy) THEN
             TangentDirection = 1
           ELSE IF(dy > 100 * dx) THEN
             TangentDirection = 2
           ELSE
             TangentDirection = 0
             CALL Warn('ShearWaveSolver','Undefined tangent direction')
           END IF
           IF(TangentDirection == 0) CYCLE

           DO j=1,n
             k = VPerm(NodeIndexes(j))
             p = FlowPerm(NodeIndexes(j))

             !---------------------------------------------------------------------
             ! NOTE: The case in which there are nonvanishing tangential velocities 
             ! cannot be handled currently
             !---------------------------------------------------------------------

             IF (TangentDirection == 1) THEN
               s = StiffMatrix % Values(StiffMatrix % Diag(Dofs*(p-1)+1))
               ForceVector(Dofs*(p-1)+1) = -1.0 * s * V((k-1)*2*VelocityComponents+1)
               CALL ZeroRow( StiffMatrix,Dofs*(p-1)+1)
               CALL SetMatrixElement( StiffMatrix,Dofs*(p-1)+1,Dofs*(p-1)+1, 1.0 * s )
               s = StiffMatrix % Values(StiffMatrix % Diag(Dofs*(p-1)+2))
               ForceVector(Dofs*(p-1)+2) = -1.0 * s * V((k-1)*2*VelocityComponents+2)
               CALL ZeroRow( StiffMatrix,Dofs*(p-1)+2)
               CALL SetMatrixElement( StiffMatrix,Dofs*(p-1)+2,Dofs*(p-1)+2, 1.0 * s )         
             END IF
             IF (TangentDirection == 2) THEN
               s = StiffMatrix % Values(StiffMatrix % Diag(Dofs*(p-1)+3))
               ForceVector(Dofs*(p-1)+3) = -1.0 * s * V((k-1)*2*VelocityComponents+3)
               CALL ZeroRow( StiffMatrix,Dofs*(p-1)+3)
               CALL SetMatrixElement( StiffMatrix,Dofs*(p-1)+3,Dofs*(p-1)+3, 1.0 * s )
               s = StiffMatrix % Values(StiffMatrix % Diag(Dofs*(p-1)+4))
               ForceVector(Dofs*(p-1)+4) = -1.0 * s * V((k-1)*2*VelocityComponents+4)
               CALL ZeroRow( StiffMatrix,Dofs*(p-1)+4)
               CALL SetMatrixElement( StiffMatrix,Dofs*(p-1)+4,Dofs*(p-1)+4, 1.0 * s )      
             END IF
           END DO
         END IF
       END DO
     END DO

     CALL Info( 'ShearWaveSolver', 'Assembly done', Level=4 )

!------------------------------------------------------------------------------
!    Solve the system and we are done:
!------------------------------------------------------------------------------

     at = CPUTime() - at
     st = CPUTime()

     CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
          Flow, Norm, Dofs, Solver )
     st = CPUTime() - st

     CALL Info( 'ShearWaveSolver', ' ', Level=4 )
     WRITE(Message,'(a,F8.2)') ' Assembly: (s)', at  
     CALL Info( 'ShearWaveSolver', Message, Level=4 )
     WRITE(Message,'(a,F8.2)') ' Solve:    (s)', st
     CALL Info( 'ShearWaveSolver', Message, Level=4 )
     CALL Info( 'ShearWaveSolver', ' ', Level=4 )
     WRITE( Message, * ) 'Result Norm    : ', Norm
     CALL Info( 'ShearWaveSolver', Message, Level=4 )
     CALL Info( 'ShearWaveSolver', ' ', Level=4 )

!----------------------------------------------------------------------------
! Modify the solver norm so that the norm of this solution is not used to
! determine whether the solution is converged or not  
!----------------------------------------------------------------------------

     Norm = 5.0d-1

!------------------------------------------------------------------------------
!   Add the computed velocity to the true velocity variable
!-------------------------------------------------------------------------------

  IF(ASSOCIATED(V)) THEN
    DO t = 1,Solver % Mesh % NumberOfNodes
      DO i = 1, VelocityComponents
        V((t-1)*2*VelocityComponents+(i-1)*2+1) = &
            V((t-1)*2*VelocityComponents+(i-1)*2+1) + &
            Flow((t-1)*dofs+(i-1)*2+1) 
        V((t-1)*2*VelocityComponents+(i-1)*2+2) = &
            V((t-1)*2*VelocityComponents+(i-1)*2+2) + &
            Flow((t-1)*dofs+(i-1)*2+2) 
      END DO
    END DO
  END IF



CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  StiffMatrix, Force, AngularFrequency , &
      SpecificHeat, HeatRatio, Density, Pressure,               &
      Temperature, Conductivity, Viscosity, Lambda,             &
      HeatSource, Load, Bubbles, Element, n, Nodes, Dofs )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), Force(:), AngularFrequency, &
        SpecificHeat(:), HeatRatio(:), Density(:), Pressure(:),    &       
        Temperature(:), Conductivity(:), Viscosity(:), Lambda(:),  &
        HeatSource(:,:), Load(:,:)
    LOGICAL :: Bubbles
    INTEGER :: n, Dofs
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3)
    REAL(KIND=dp) :: SqrtElementMetric, U, V, W, S, L(6), &
        CV, gamma, rho0, P0, T0, kappa, mu, la, f1, f2, K1, K2, K3, &
        PenaltyFactor  
    COMPLEX(KIND=dp) :: LSTIFF(n*(Dofs-1),n*(Dofs-1)), LFORCE(n*(Dofs-1)), A
        
    INTEGER :: i, j, p, q, t, DIM, NBasis, CoordSys, VelocityDofs, & 
        VelocityComponents
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    REAL(KIND=dp) :: X, Y, Z, Metric(3,3), SqrtMetric, Symb(3,3,3), &
        dSymb(3,3,3,3)
!------------------------------------------------------------------------------

    DIM = CoordinateSystemDimension()

    Metric = 0.0d0
    Metric(1,1) = 1.0d0
    Metric(2,2) = 1.0d0
    Metric(3,3) = 1.0d0

    LSTIFF = 0.0d0
    LFORCE = 0.0d0
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
!------------------------------------------------------------------------------
!     Problem parameters and the real and imaginary part of the 
!     load at the integration point
!------------------------------------------------------------------------------
       CV = SUM( SpecificHeat(1:n) * Basis(1:n) )       
       gamma = SUM( HeatRatio(1:n) * Basis(1:n) )
       rho0 = SUM( Density(1:n) * Basis(1:n) )
       P0 = SUM( Pressure(1:n) * Basis(1:n) )
       T0 = SUM( Temperature(1:n) * Basis(1:n) )
       kappa = SUM( Conductivity(1:n) * Basis(1:n) )
       mu = SUM( Viscosity(1:n) * Basis(1:n) )
       la = SUM( Lambda(1:n) * Basis(1:n) )

       DO i=1,2*DIM
         L(i) = rho0*SUM( Load(i,1:n) * Basis(1:n) )
       END DO

!------------------------------------------------------------------------------
!      The stiffness matrix and load vector...
!------------------------------------------------------------------------------
       DO p=1,N
         DO q=1,N
           DO i=1,DIM
!------------------------------------------------------------------------------
!            Coefficients for the nodal velocities...
!------------------------------------------------------------------------------
             LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) =  &
                 LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) - &
                 DCMPLX(1.0d0, 0.0d0) * dBasisdx(q,i) * Basis(p) * s 
           END DO
         END DO
       END DO

       IF ( Bubbles ) THEN
         DO p=1,n
           DO q=n+1,NBasis
             DO i=1,DIM
!------------------------------------------------------------------------------
!              Coefficients for the nodal velocities
!------------------------------------------------------------------------------
               LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) = &
                   LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) - &
                   DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(q,i) * Basis(p) * s 
             END DO
           END DO
         END DO
       END IF

!------------------------------------------------------------------------------
!      The part arising from the loop over the test functions for velocity 
!------------------------------------------------------------------------------

       DO i=1,DIM
         DO p=1,n
           DO q=1,n
!------------------------------------------------------------------------------
!            Coefficients for the nodal pressures...               
!------------------------------------------------------------------------------
             LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) = &
                 LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) - &
                 DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(p,i) * Basis(q) * s
!------------------------------------------------------------------------------
!            Coefficients for the nodal velocities...             
!------------------------------------------------------------------------------
             LSTIFF( (p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) = &
                 LSTIFF( (p-1)*(DIM+1)+i,(q-1)*(DIM+1)+i) + &
                 DCMPLX( rho0, 0.0d0 ) * &
                 Basis(q) * Basis(p) * s
                
             DO j=1,DIM
!------------------------------------------------------------------------------
!              grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
               LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) = &
                   LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) + &
                   DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,j) * &
                   dBasisdx(p,j) * s
               LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+j) = &
                   LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+j) + &
                   DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,i) * &
                   dBasisdx(p,j) * s
             END DO
           END DO
         END DO
       END DO

       IF ( Bubbles ) THEN
         DO i=1,DIM
           DO p=1,n
             DO q=n+1,NBasis
!------------------------------------------------------------------------------
!            coefficients for the nodal velocities             
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*(DIM+1)+i, (q-1)*DIM+n+i) = &
                   LSTIFF( (p-1)*(DIM+1)+i,(q-1)*DIM+n+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) = &
                     LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+j) = &
                     LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO
             END DO
           END DO
         END DO

         DO i=1,DIM
           DO p=n+1,NBasis
             DO q=1,n
!------------------------------------------------------------------------------
!            Coefficients for the nodal pressures...               
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) = &
                   LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) - &
                   DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(p,i) * Basis(q) * s
!------------------------------------------------------------------------------
!              coefficients for the nodal velocities 
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) = &
                   LSTIFF( (p-1)*DIM+n+i,(q-1)*(DIM+1)+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+j) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO
             END DO
           END DO
         END DO

         DO i=1,DIM
           DO p=n+1,NBasis
             DO q=n+1,NBasis
!------------------------------------------------------------------------------
!              coefficients for the nodal velocities 
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) = &
                   LSTIFF( (p-1)*DIM+n+i,(q-1)*DIM+n+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+j) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO
             END DO
           END DO
         END DO         
!------------------------------------------------------------------------------
       END IF   ! IF (Bubbles)...
!------------------------------------------------------------------------------
    END DO      ! Loop over integration points
!------------------------------------------------------------------------------

    IF ( Bubbles ) THEN
      CALL LCondensate( n, dim, LSTIFF, LFORCE )
    END IF

    DO p=1,n
      DO i=1,DIM+1
        Force( 2*(DIM+1)*(p-1)+2*i-1 ) = REAL(LFORCE( (p-1)*(DIM+1)+i ))
        Force( 2*(DIM+1)*(p-1)+2*i ) = AIMAG(LFORCE( (p-1)*(DIM+1)+i ))
        DO q=1,n
          DO j=1,DIM+1
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i-1, 2*(DIM+1)*(q-1)+2*j-1 ) =  &
                REAL( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i-1, 2*(DIM+1)*(q-1)+2*j ) =  &
                -AIMAG( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i, 2*(DIM+1)*(q-1)+2*j-1 ) =  &
                AIMAG( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i, 2*(DIM+1)*(q-1)+2*j ) =  &
                REAL( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
          END DO
        END DO
      END DO
    END DO
   
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE LCondensate( n, dim, K, F )
!------------------------------------------------------------------------------
    USE LinearAlgebra
!------------------------------------------------------------------------------
    INTEGER :: n, dim
    COMPLEX(KIND=dp) :: K(:,:), F(:), Kbb(dim*n,dim*n), &
         Kbl(dim*n,(dim+1)*n), Klb((dim+1)*n,dim*n), Fb(n*dim)

    INTEGER :: i, Ldofs((dim+1)*n), Bdofs(dim*n)

    Ldofs = (/ (i, i=1,(dim+1)*n) /)
    Bdofs = (/ ((dim+1)*n+i, i=1, dim*n) /)

    Kbb = K(Bdofs,Bdofs)
    Kbl = K(Bdofs,Ldofs)
    Klb = K(Ldofs,Bdofs)
    Fb  = F(Bdofs)

    CALL ComplexInvertMatrix( Kbb,n )

    F(1:(dim+1)*n) = F(1:(dim+1)*n) - MATMUL( Klb, MATMUL( Kbb, Fb  ) )
    K(1:(dim+1)*n,1:(dim+1)*n) = &
         K(1:(dim+1)*n,1:(dim+1)*n) - MATMUL( Klb, MATMUL( Kbb, Kbl ) )
!------------------------------------------------------------------------------
  END SUBROUTINE LCondensate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! The element matrices in the case of axial symmetry...
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix2(  StiffMatrix, Force, AngularFrequency , &
      SpecificHeat, HeatRatio, Density, Pressure,               &
      Temperature, Conductivity, Viscosity, Lambda,             &
      HeatSource, Load, Bubbles, Element, n, Nodes, Dofs )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: StiffMatrix(:,:), Force(:), AngularFrequency, &
        SpecificHeat(:), HeatRatio(:), Density(:), Pressure(:),    &       
        Temperature(:), Conductivity(:), Viscosity(:), Lambda(:),  &
        HeatSource(:,:), Load(:,:)
    LOGICAL :: Bubbles
    INTEGER :: n, Dofs
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3)
    REAL(KIND=dp) :: SqrtElementMetric, U, V, W, S, L(6), &
        CV, gamma, rho0, P0, T0, kappa, mu, la, f1, f2, K1, K2, K3, &
        PenaltyFactor, r  
    COMPLEX(KIND=dp) :: LSTIFF(n*(Dofs-1),n*(Dofs-1)), LFORCE(n*(Dofs-1)), A
        
    INTEGER :: i, j, p, q, t, DIM, NBasis, CoordSys, VelocityDofs, & 
        VelocityComponents
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    REAL(KIND=dp) :: X, Y, Z, Metric(3,3), SqrtMetric, Symb(3,3,3), &
        dSymb(3,3,3,3)
!------------------------------------------------------------------------------

    DIM = CoordinateSystemDimension()

    Metric = 0.0d0
    Metric(1,1) = 1.0d0
    Metric(2,2) = 1.0d0
    Metric(3,3) = 1.0d0

    LSTIFF = 0.0d0
    LFORCE = 0.0d0
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
       r = SUM( Basis(1:n) * Nodes % x(1:n) )
       s = r * s * SqrtElementMetric
!------------------------------------------------------------------------------
!     Problem parameters and the real and imaginary part of the 
!     load at the integration point
!------------------------------------------------------------------------------
       CV = SUM( SpecificHeat(1:n) * Basis(1:n) )       
       gamma = SUM( HeatRatio(1:n) * Basis(1:n) )
       rho0 = SUM( Density(1:n) * Basis(1:n) )
       P0 = SUM( Pressure(1:n) * Basis(1:n) )
       T0 = SUM( Temperature(1:n) * Basis(1:n) )
       kappa = SUM( Conductivity(1:n) * Basis(1:n) )
       mu = SUM( Viscosity(1:n) * Basis(1:n) )
       la = SUM( Lambda(1:n) * Basis(1:n) )

       DO i=1,2*DIM
         L(i) = rho0*SUM( Load(i,1:n) * Basis(1:n) )
       END DO

!------------------------------------------------------------------------------
!      The stiffness matrix and load vector...
!------------------------------------------------------------------------------
       DO p=1,N
         DO q=1,N
           DO i=1,DIM
!------------------------------------------------------------------------------
!            Coefficients for the nodal velocities...
!------------------------------------------------------------------------------
             LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) =  &
                 LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) - &
                 DCMPLX(1.0d0, 0.0d0) * dBasisdx(q,i) * Basis(p) * s
             IF (i==1) THEN
               LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) =  &
                   LSTIFF( p*(DIM+1), (q-1)*(DIM+1)+i) - &
                   DCMPLX(1.0d0, 0.0d0) * 1/r * Basis(q) * Basis(p) * s
             END IF
           END DO
         END DO
       END DO

       IF ( Bubbles ) THEN
         DO p=1,n
           DO q=n+1,NBasis
             DO i=1,DIM
!------------------------------------------------------------------------------
!              Coefficients for the nodal velocities
!------------------------------------------------------------------------------
               LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) = &
                   LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) - &
                   DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(q,i) * Basis(p) * s
               IF (i==1) THEN
                 LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) = &
                     LSTIFF( p*(DIM+1), (q-1)*DIM+n+i) - &
                     DCMPLX(1.0d0, 0.0d0) * 1/r * Basis(q) * Basis(p) * s
               END IF
             END DO
           END DO
         END DO
       END IF

!------------------------------------------------------------------------------
!      The part arising from the loop over the test functions for velocity 
!------------------------------------------------------------------------------

       DO i=1,DIM
         DO p=1,n
           DO q=1,n
!------------------------------------------------------------------------------
!            Coefficients for the nodal pressures...               
!------------------------------------------------------------------------------
             LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) = &
                 LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) - &
                 DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(p,i) * Basis(q) * s
             IF (i==1) THEN
               LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) = &
                   LSTIFF( (p-1)*(DIM+1)+i, q*(DIM+1) ) - &
                   DCMPLX( 1.0d0, 0.0d0 ) * 1/r * Basis(q) * Basis(p) * s
             END IF
!------------------------------------------------------------------------------
!            Coefficients for the nodal velocities...             
!------------------------------------------------------------------------------
             LSTIFF( (p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) = &
                 LSTIFF( (p-1)*(DIM+1)+i,(q-1)*(DIM+1)+i) + &
                 DCMPLX( rho0, 0.0d0 ) * &
                 Basis(q) * Basis(p) * s
                
             DO j=1,DIM
!------------------------------------------------------------------------------
!              grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
               LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) = &
                   LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) + &
                   DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,j) * &
                   dBasisdx(p,j) * s
               LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+j) = &
                   LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+j) + &
                   DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,i) * &
                   dBasisdx(p,j) * s
             END DO
             
             IF (i==1) THEN
               LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) = &
                   LSTIFF((p-1)*(DIM+1)+i, (q-1)*(DIM+1)+i) + &
                   DCMPLX( 0.0d0, -2*mu/AngularFrequency ) * 1/r**2 * &
                   Basis(q) * Basis(p) * s
             END IF
           END DO
         END DO
       END DO

       IF ( Bubbles ) THEN
         DO i=1,DIM
           DO p=1,n
             DO q=n+1,NBasis
!------------------------------------------------------------------------------
!            coefficients for the nodal velocities             
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*(DIM+1)+i, (q-1)*DIM+n+i) = &
                   LSTIFF( (p-1)*(DIM+1)+i,(q-1)*DIM+n+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) = &
                     LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+j) = &
                     LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency ) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO

               IF (i==1) THEN
                 LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) = &
                     LSTIFF((p-1)*(DIM+1)+i, (q-1)*DIM+n+i) + &                 
                     DCMPLX( 0.0d0, -2*mu/AngularFrequency ) * 1/r**2 * &
                     Basis(q) * Basis(p) * s
               END IF
             END DO
           END DO
         END DO

         DO i=1,DIM
           DO p=n+1,NBasis
             DO q=1,n
!------------------------------------------------------------------------------
!            Coefficients for the nodal pressures...               
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) = &
                   LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) - &
                   DCMPLX( 1.0d0, 0.0d0 ) * dBasisdx(p,i) * Basis(q) * s
               IF (i==1) THEN
                 LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) = &
                     LSTIFF( (p-1)*DIM+n+i, q*(DIM+1) ) - & 
                     DCMPLX( 1.0d0, 0.0d0 ) * 1/r * Basis(p) * Basis(q) * s
               END IF
!------------------------------------------------------------------------------
!              coefficients for the nodal velocities 
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) = &
                   LSTIFF( (p-1)*DIM+n+i,(q-1)*(DIM+1)+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+j) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO
               
               IF (i==1) THEN
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*(DIM+1)+i) + &
                     DCMPLX( 0.0d0, -2*mu/AngularFrequency) * 1/r**2 * &
                     Basis(q) * Basis(p) * s
               END IF
             END DO
           END DO
         END DO

         DO i=1,DIM
           DO p=n+1,NBasis
             DO q=n+1,NBasis
!------------------------------------------------------------------------------
!              coefficients for the nodal velocities 
!------------------------------------------------------------------------------
               LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) = &
                   LSTIFF( (p-1)*DIM+n+i,(q-1)*DIM+n+i) + &
                   DCMPLX( rho0, 0.0d0 ) * &
                   Basis(q) * Basis(p) * s
                
               DO j=1,DIM
!------------------------------------------------------------------------------
!                grad(v)grav(w)-type terms
!------------------------------------------------------------------------------
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,j) * &
                     dBasisdx(p,j) * s
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+j) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+j) + &
                     DCMPLX( 0.0d0, -mu/AngularFrequency) * dBasisdx(q,i) * &
                     dBasisdx(p,j) * s
               END DO
               IF (i==1) THEN
                 LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) = &
                     LSTIFF( (p-1)*DIM+n+i, (q-1)*DIM+n+i) + &
                     DCMPLX( 0.0d0, -2*mu/AngularFrequency) * 1/r**2 * &
                     Basis(q) * Basis(p) * s
               END IF
             END DO
           END DO
         END DO         
!------------------------------------------------------------------------------
       END IF   ! IF (Bubbles)...
!------------------------------------------------------------------------------
    END DO      ! Loop over integration points
!------------------------------------------------------------------------------

    IF ( Bubbles ) THEN
      CALL LCondensate( n, dim, LSTIFF, LFORCE )
    END IF

    DO p=1,n
      DO i=1,DIM+1
        Force( 2*(DIM+1)*(p-1)+2*i-1 ) = REAL(LFORCE( (p-1)*(DIM+1)+i ))
        Force( 2*(DIM+1)*(p-1)+2*i ) = AIMAG(LFORCE( (p-1)*(DIM+1)+i ))
        DO q=1,n
          DO j=1,DIM+1
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i-1, 2*(DIM+1)*(q-1)+2*j-1 ) =  &
                REAL( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i-1, 2*(DIM+1)*(q-1)+2*j ) =  &
                -AIMAG( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i, 2*(DIM+1)*(q-1)+2*j-1 ) =  &
                AIMAG( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
            StiffMatrix( 2*(DIM+1)*(p-1)+2*i, 2*(DIM+1)*(q-1)+2*j ) =  &
                REAL( LSTIFF((DIM+1)*(p-1)+i,(DIM+1)*(q-1)+j) )
          END DO
        END DO
      END DO
    END DO
   
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix2
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
END SUBROUTINE ShearWaveSolver
!------------------------------------------------------------------------------
