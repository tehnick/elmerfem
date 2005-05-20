!/******************************************************************************
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
! *****************************************************************************/
!
!/******************************************************************************
! *
! * Module containing a solver for the MHD Maxwell equations (or the induction
! * equation)
! *
! ******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                            Tietotie 6, P.O. Box 405
! *                              02101 Espoo, Finland
! *                              Tel. +358 0 457 2723
! *                            Telefax: +358 0 457 2302
! *                          EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 08 Jun 1997
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/


!------------------------------------------------------------------------------
   SUBROUTINE MagneticSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: MagneticSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve Maxwell equations for one timestep
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  REAL(KIND=dp) :: dt,
!     INPUT: Timestep size for time dependent simulations
!
!******************************************************************************

    USE Maxwell
    USE MaxwellAxiS
    USE MaxwellGeneral

    USE DefUtils
    USE Differentials
!------------------------------------------------------------------------------

    IMPLICIT NONE

     TYPE(Model_t)  :: Model
     TYPE(Solver_t), TARGET :: Solver

     LOGICAL :: TransientSimulation
     REAL(KIND=dp) :: dt

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
     TYPE(Matrix_t),POINTER :: StiffMatrix
     INTEGER :: i,j,k,l,n,t,iter,LocalNodes,k1,k2,istat

     TYPE(ValueList_t),POINTER :: Material, Equation, BF
     TYPE(Nodes_t) :: ElementNodes
     TYPE(Element_t),POINTER :: CurrentElement

     REAL(KIND=dp) :: RelativeChange,UNorm,PrevUNorm,Gravity(3), &
         Tdiff,Normal(3),s,r,NewtonTol,NonlinearTol

     INTEGER :: NSDOFs,NewtonIter,NonlinearIter, dim

     TYPE(Variable_t), POINTER :: MagneticSol, ElectricSol, FlowSol, &
         ExMagSol, MeshSol
     INTEGER, POINTER :: MagneticPerm(:),FlowPerm(:),ExMagPerm(:),MeshPerm(:)

     REAL(KIND=dp), POINTER :: MagneticField(:),ElectricCurrent(:), &
      FlowSolution(:),Work(:,:), M1(:),M2(:),M3(:),E1(:),E2(:),E3(:), &
      ForceVector(:), divB(:),ExB(:), MeshVelocity(:), &
      LrF(:), LrFr(:), LrFz(:), LrFp(:)

     LOGICAL :: Stabilize,NewtonLinearization = .FALSE.,GotForceBC,GotIt

     INTEGER :: body_id,bf_id,eq_id
     INTEGER, POINTER :: NodeIndexes(:)
!
     LOGICAL :: AllocationsDone = .FALSE., FreeSurfaceFlag, UserDefinedVelo

     REAL(KIND=dp),ALLOCATABLE:: LocalMassMatrix(:,:),LocalStiffMatrix(:,:),&
       LoadVector(:,:),LocalForce(:), &
          Conductivity(:),Mx(:),My(:),Mz(:),U(:),V(:),W(:),Alpha(:),Beta(:), &
            Permeability(:),ExBx(:),ExBy(:),ExBz(:),B1(:),B2(:),B3(:)

     SAVE Mx,My,Mz,U,V,W,LocalMassMatrix,LocalStiffMatrix,LoadVector, &
       LocalForce,ElementNodes,Alpha,Beta, &
         Conductivity, AllocationsDone,LocalNodes, &
           Permeability, divB, ExBx,ExBy,ExBz,B1,B2,B3, LrF
!------------------------------------------------------------------------------
     INTEGER :: NumberOfBoundaryNodes
     INTEGER, POINTER :: BoundaryReorder(:)

     REAL(KIND=dp) :: Bu,Bv,Bw,RM(3,3)
     REAL(KIND=dp), POINTER :: BoundaryNormals(:,:), &
           BoundaryTangent1(:,:), BoundaryTangent2(:,:)

     TYPE(Solver_t), POINTER :: SolverPointer

     SAVE NumberOfBoundaryNodes,BoundaryReorder,BoundaryNormals, &
              BoundaryTangent1, BoundaryTangent2

     REAL(KIND=dp) :: at,at0,totat,st,totst,t1,CPUTime,RealTime
!------------------------------------------------------------------------------
!    Get variables needed for solving the system
!------------------------------------------------------------------------------
     IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN
     dim = CoordinateSystemDimension()

     MagneticSol => Solver % Variable
     MagneticPerm  => MagneticSol % Perm
     MagneticField => MagneticSol % Values

     LocalNodes = COUNT( MagneticPerm > 0 )
     IF ( LocalNodes <= 0 ) RETURN

     ExMagSol => VariableGet( Model % Variables, &
         'Magnetic Flux Density' )
     IF ( ASSOCIATED( ExMagSol ) ) THEN
       ExMagPerm => ExMagSol % Perm
       ExB => ExMagSol % Values
     END IF

     ElectricSol   => VariableGet( Model % Variables, 'Electric Current' )
     ElectricCurrent => ElectricSol % Values

     FlowSol => VariableGet( Model % Variables, 'Flow Solution' )
     IF ( ASSOCIATED( FlowSol ) ) THEN
       NSDOFs       =  FlowSol % DOFs
       FlowPerm     => FlowSol % Perm
       FlowSolution => FlowSol % Values
     END IF

     MeshSol => VariableGet( Solver % Mesh % Variables, 'Mesh Velocity' )
     NULLIFY( MeshVelocity )
     IF ( ASSOCIATED( MeshSol ) ) THEN
       MeshPerm => MeshSol % Perm
       MeshVelocity => MeshSol % Values
     END IF

     StiffMatrix => Solver % Matrix
     ForceVector => StiffMatrix % RHS
 
     UNorm = Solver % Variable % Norm
!------------------------------------------------------------------------------
!    Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------

     IF ( .NOT.AllocationsDone ) THEN

       N = Model % MaxElementNodes

       ALLOCATE( U(N), V(N), W(N), MX(N), MY(N), MZ(N), &
                 ExBx(N),ExBy(N),ExBz(N), &
                 ElementNodes % x( N ), &
                 ElementNodes % y( N ), &
                 ElementNodes % z( N ), &
                 Conductivity( N ),  &
                 Permeability( N ),  &
                 LocalForce( 3*N ), &
                 LocalMassMatrix(  3*N,3*N ),  &
                 LocalStiffMatrix( 3*N,3*N ),  &
                 divB(Model % NumberOfNodes), &
                 LrF(3 * Model % NumberOfNodes), &
                 B1(LocalNodes), &
                 B2(LocalNodes), &
                 B3(LocalNodes), &
                 LoadVector( 3,N ), Alpha( N ), Beta( N ),STAT=istat )

       IF ( istat /= 0 ) THEN
         CALL Fatal( 'MagneticSolve', 'Memory allocation error.' )
       END IF

! Add extra variables to *.result and *.ep files

! div B for the divergence check
!#if 0
!      IF (.NOT. ASSOCIATED(VariableGet(Model % Variables, 'div B'))) THEN
!        SolverPointer => Solver
!        CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
!              SolverPointer, 'div B', 1, divB, MagneticPerm)
!      END IF
!#endif

! Lorentz force, either total or the high-f part
!#if 0
!     IF (.NOT. ASSOCIATED(VariableGet(Model % Variables, 'Lorentz Force'))) THEN
!        SolverPointer => Solver
!
!        LrFr => LrF(1:3*LocalNodes-2:3)
!        CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
!             SolverPointer, 'Lorentz Force 1', 1, LrFr, MagneticPerm)
!
!        LrFz => LrF(2:3*LocalNodes-1:3)
!        CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
!             SolverPointer, 'Lorentz Force 2', 1, LrFz, MagneticPerm)
!
!        LrFp => LrF(3:3*LocalNodes:3)
!        CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
!             SolverPointer, 'Lorentz Force 3', 1, LrFp, MagneticPerm)
!
!        CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
!              SolverPointer, 'Lorentz Force', 3, LrF, MagneticPerm)
!      END IF
!#endif

!------------------------------------------------------------------------------
!    Check for normal/tangetial coordinate system defined velocities
!------------------------------------------------------------------------------
       CALL CheckNormalTangentialBoundary( Model, &
           'Normal-Tangential Magnetic Field', NumberOfBoundaryNodes, &
          BoundaryReorder,BoundaryNormals,BoundaryTangent1, &
             BoundaryTangent2, dim )
!------------------------------------------------------------------------------

       AllocationsDone = .TRUE.
     END IF

!------------------------------------------------------------------------------

     Stabilize = ListGetLogical( Solver % Values,'Stabilize',GotIt )
     IF ( .NOT.GotIt ) Stabilize = .TRUE.

     NonlinearTol = ListGetConstReal( Solver % Values, &
        'Nonlinear System Convergence Tolerance' )

     NewtonTol = ListGetConstReal( Solver % Values, &
        'Nonlinear System Newton After Tolerance' )

     NewtonIter = ListGetInteger( Solver % Values, &
        'Nonlinear System Newton After Iterations' )

     NonlinearIter = ListGetInteger( Solver % Values, &
        'Nonlinear System Max Iterations' )

!------------------------------------------------------------------------------
!    Check if free surfaces present
!------------------------------------------------------------------------------
     FreeSurfaceFlag = .FALSE.
     DO i=1,Model % NumberOfBCs
       FreeSurfaceFlag = FreeSurfaceFlag.OR. &
          ListGetLogical( Model % BCs(i) % Values, 'Free Surface', GotIt )
       IF ( FreeSurfaceFlag ) EXIT
     END DO
!------------------------------------------------------------------------------

     totat = 0.0d0
     totst = 0.0d0

     DO iter=1,NonlinearIter

       at  = CPUTime()
       at0 = RealTime()

       CALL Info( 'MagneticSolve', ' ', Level=4 )
       CALL Info( 'MagneticSolve', ' ', Level=4 )
       CALL Info( 'MagneticSolve', &
             '-------------------------------------', Level=4 )
       WRITE( Message, * ) 'Magnetic induction iteration: ', iter
       CALL Info( 'MagneticSolve', Message, Level=4 )
       CALL Info( 'MagneticSolve', &
             '-------------------------------------', Level=4 )
       CALL Info( 'MagneticSolve', ' ', Level=4 )
!------------------------------------------------------------------------------
!      Compute average normals for boundaries having the normal & tangetial
!      field components specified on the boundaries
!------------------------------------------------------------------------------
       IF ( (iter == 1.OR.FreeSurfaceFlag).AND.NumberOfBoundaryNodes>0 )  THEN
          CALL AverageBoundaryNormals( Model, &
             'Normal-Tangential Magnetic Field',NumberOfBoundaryNodes, &
            BoundaryReorder, BoundaryNormals, BoundaryTangent1, &
               BoundaryTangent2, dim )
       END IF
!------------------------------------------------------------------------------

       CALL InitializeToZero( StiffMatrix, ForceVector )

       DO t=1,Solver % NumberOFActiveElements

         IF ( RealTime() - at0 > 1.0 ) THEN
           WRITE(Message,'(a,i3,a)' ) '   Assembly: ', INT(100.0 - 100.0 * &
            (Model % NumberOfBulkElements-t) / &
               (1.0*Solver % NumberOfActiveElements)), ' % done'
                       
           CALL Info( 'MagneticSolve', Message, Level=5 )
           at0 = RealTime()
         END IF

         CurrentElement => GetActiveElement(t)
!
!------------------------------------------------------------------------------
         n = GetElementNOFNodes()
         NodeIndexes => CurrentElement % NodeIndexes

         Equation => GetEquation()

         UserDefinedVelo = .FALSE.
         UserDefinedVelo = GetLogical(Equation,'User Defined Velocity', gotIt)
         
         ElementNodes % x(1:n) = Model % Nodes % x(NodeIndexes)
         ElementNodes % y(1:n) = Model % Nodes % y(NodeIndexes)
         ElementNodes % z(1:n) = Model % Nodes % z(NodeIndexes)

         Material => GetMaterial()

         Conductivity(1:n) = GetReal(Material, 'Electrical Conductivity' )
         Permeability(1:n) = GetReal(Material, 'Magnetic Permeability'   )

         Mx = GetReal( Material, 'Applied Magnetic Field 1', Gotit )
         My = GetReal( Material, 'Applied Magnetic Field 2', Gotit )
         Mz = GetReal( Material, 'Applied Magnetic Field 3', Gotit )

         ExBx=0
         ExBy=0
         ExBz=0
! If you want to use time-domain solution for high-frequendy part,
! leave external field out. Better to use frequency-domain solver!
#if 1
         IF ( ASSOCIATED( ExMagSol ) ) THEN
           ExBx(1:n) = ExB(3*ExMagPerm(NodeIndexes)-2)
           ExBy(1:n) = ExB(3*ExMagPerm(NodeIndexes)-1)
           ExBz(1:n) = ExB(3*ExMagPerm(NodeIndexes))
         END IF
#endif

         Mx(1:n) = Mx(1:n) + ExBx(1:n)
         My(1:n) = My(1:n) + ExBy(1:n)
         Mz(1:n) = Mz(1:n) + ExBz(1:n)

         U = 0.0D0
         V = 0.0D0
         W = 0.0D0

! For high-f part (in time-domain), leave velocity contribution out.
#if 1
         IF ( ASSOCIATED( FlowSol ) ) THEN
           DO i=1,n
             k = FlowPerm(NodeIndexes(i))
             IF ( k > 0 ) THEN
               SELECT CASE( NSDOFs )
                 CASE(3)
                   U(i) = FlowSolution( NSDOFs*k-2 )
                   V(i) = FlowSolution( NSDOFs*k-1 )
                   W(i) = 0.0D0
                   IF ( ASSOCIATED( MeshVelocity ) ) THEN
                     IF ( MeshPerm(NodeIndexes(i)) > 0 ) THEN
                       U(i) = U(i) - MeshVelocity( 2*MeshPerm(NodeIndexes(i))-1 )
                       V(i) = V(i) - MeshVelocity( 2*MeshPerm(NodeIndexes(i))-0 )
                     END IF
                   END IF

                 CASE(4)
                   U(i) = FlowSolution( NSDOFs*k-3 )
                   V(i) = FlowSolution( NSDOFs*k-2 )
                   W(i) = FlowSolution( NSDOFs*k-1 )
                   IF ( ASSOCIATED( MeshVelocity ) ) THEN
                     IF ( MeshPerm(NodeIndexes(i)) > 0 ) THEN
                       U(i) = U(i) - MeshVelocity( 3*MeshPerm(NodeIndexes(i))-2 )
                       V(i) = V(i) - MeshVelocity( 3*MeshPerm(NodeIndexes(i))-1 )
                       W(i) = W(i) - MeshVelocity( 3*MeshPerm(NodeIndexes(i))-0 )
                     END IF
                   END IF
               END SELECT
             END IF
           END DO
        END IF

        IF ( UserDefinedVelo ) THEN     
           ! check for given constant velocity

           U(1:n) = GetReal( Material, 'MHD Velocity 1', gotIt )
           V(1:n) = GetReal( Material, 'MHD Velocity 2', gotIt )
           W(1:n) = GetReal( Material, 'MHD Velocity 3', gotIt )

        END IF
#endif

!------------------------------------------------------------------------------
!        Set body forces
!------------------------------------------------------------------------------
         BF => getBodyForce()
 
         LoadVector = 0.0D0
         IF ( ASSOCIATED(BF) ) THEN
           LoadVector(1,1:n) = LoadVector(1,1:n) + GetReal( &
                   BF, 'Magnetic Bodyforce 1', GotIt )
           LoadVector(2,1:n) = LoadVector(2,1:n) + GetReal( &
                   BF, 'Magnetic Bodyforce 2', GotIt )
           LoadVector(3,1:n) = LoadVector(3,1:n) + GetReal( &
                   BF, 'Magnetic Bodyforce 3', GotIt )
         END IF
!------------------------------------------------------------------------------
!        Get element local stiffness & mass matrices
!------------------------------------------------------------------------------
         IF ( CurrentCoordinateSystem() == Cartesian ) THEN
            CALL MaxwellCompose( &
                LocalMassMatrix,LocalStiffMatrix,LocalForce, &
                    LoadVector,Conductivity*Permeability,Mx,My,Mz,U,V,W, &
                        CurrentElement,n,ElementNodes )
          ELSE IF ( CurrentCoordinateSystem() == CylindricSymmetric ) THEN
            CALL MaxwellAxiSCompose( &
                LocalMassMatrix,LocalStiffMatrix,LocalForce, &
                    LoadVector,Conductivity*Permeability,Mx,My,Mz,U,V,W, &
                       CurrentElement,n,ElementNodes )
         ELSE 
            CALL MaxwellGeneralCompose( &
                LocalMassMatrix,LocalStiffMatrix,LocalForce, &
                    LoadVector,Conductivity*Permeability,Mx,My,Mz,U,V,W, &
                        CurrentElement,n,ElementNodes )
         END IF

!------------------------------------------------------------------------------
!        If time dependent simulation, add mass matrix to global 
!        matrix and global RHS vector
!------------------------------------------------------------------------------
         IF ( TransientSimulation ) THEN
!------------------------------------------------------------------------------
!          NOTE: This will replace LocalStiffMatrix and LocalForce with the
!                combined information...
!------------------------------------------------------------------------------
            CALL Add1stOrderTime(LocalMassMatrix,LocalStiffMatrix,LocalForce, &
                    dt, n, 3, MagneticPerm(NodeIndexes), Solver )
         END IF

!------------------------------------------------------------------------------
!        If boundary fields have been defined in normal/tangetial
!        coordinate systems, we´ll have to rotate the matrix & force vector
!        to that coordinate system
!------------------------------------------------------------------------------
         IF ( NumberOfBoundaryNodes > 0 ) THEN
           CALL RotateMatrix( LocalStiffMatrix,LocalForce,n,3,3, &
            BoundaryReorder(NodeIndexes),BoundaryNormals,BoundaryTangent1,   &
                              BoundaryTangent2 )
         END IF

!------------------------------------------------------------------------------
!        Update global matrices from local matrices
!------------------------------------------------------------------------------
         CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
           ForceVector, LocalForce, n, 3, MagneticPerm(NodeIndexes) )
!------------------------------------------------------------------------------
      END DO

      CALL Info( 'MagneticSolve', 'Assembly done', Level=4 )

      at = CPUTime() - at
      st = CPUTime()

!------------------------------------------------------------------------------
!     Neumann & Newton boundary conditions
!------------------------------------------------------------------------------
      DO t = Model % NumberOfBulkElements+1, &
                Model % NumberOfBulkElements + Model % NumberOfBoundaryElements

        CurrentElement => Model % Elements(t)
!------------------------------------------------------------------------------
!        Set also the current element pointer in the model structure to 
!        reflect the element being processed
!------------------------------------------------------------------------------
        Model % CurrentElement => Model % Elements(t)
!------------------------------------------------------------------------------
        n = CurrentElement % TYPE % NumberOfNodes
        NodeIndexes => CurrentElement % NodeIndexes
!
!       The element type 101 (point element) can only be used
!       to set Dirichlet BCs, so skip ´em at this stage.
!
        IF ( ANY( MagneticPerm(NodeIndexes) <= 0 ) ) CYCLE 

        IF ( CurrentElement % TYPE % ElementCode /= 101 ) THEN

        ElementNodes % x(1:n) = Model % Nodes % x(NodeIndexes)
        ElementNodes % y(1:n) = Model % Nodes % y(NodeIndexes)
        ElementNodes % z(1:n) = Model % Nodes % z(NodeIndexes)

        DO i=1,Model % NumberOfBCs
          IF ( CurrentElement % BoundaryInfo % Constraint == &
                 Model % BCs(i) % Tag ) THEN
!------------------------------------------------------------------------------
!           (at the moment the following is done...)
!           BC: \tau \cdot n = \alpha n +  @\beta/@t + F
!------------------------------------------------------------------------------
            LoadVector = 0.0D0
            Alpha      = 0.0D0
            Beta       = 0.0D0

            GotForceBC = ListGetLogical(Model % BCs(i) % Values, &
                       'Magnetic Force BC',gotIt )
            IF ( GotForceBC ) THEN
!------------------------------------------------------------------------------
!             normal force BC: \tau\cdot n = \alpha n
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!             tangential force BC:
!             \tau\cdot n = @\beta/@t (tangential derivative of something)
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!            force in given direction BC: \tau\cdot n = F
!------------------------------------------------------------------------------
              LoadVector(1,1:n) =  ListGetReal( Model % BCs(i) % Values, &
                        'Magnetic Force 1',n,NodeIndexes,GotIt )

              LoadVector(2,1:n) =  ListGetReal( Model % BCs(i) % Values, &
                        'Magnetic Force 2',n,NodeIndexes,GotIt )

              LoadVector(3,1:n) =  ListGetReal( Model % BCs(i) % Values, &
                        'Magnetic Force 3',n,NodeIndexes,GotIt )

!------------------------------------------------------------------------------
              IF ( CurrentCoordinateSystem() == Cartesian ) THEN
                CALL MaxwellBoundary( LocalStiffMatrix,LocalForce, &
                 LoadVector,Alpha,Beta,CurrentElement,n,ElementNodes )
              ELSE
                CALL MaxwellGeneralBoundary( LocalStiffMatrix,LocalForce, &
                 LoadVector,Alpha,Beta,CurrentElement,n,ElementNodes )
              END IF

!------------------------------------------------------------------------------
!             If boundary field components have been defined in normal/tangetial
!             coordinate systems, we´ll have to rotate the matrix & force vector
!             to that coordinate system
!------------------------------------------------------------------------------

              IF ( NumberOfBoundaryNodes > 0 ) THEN
                CALL RotateMatrix( LocalStiffMatrix,LocalForce,n,3,3, &
                 BoundaryReorder(NodeIndexes),BoundaryNormals,BoundaryTangent1,&
                                   BoundaryTangent2 )
              END IF

!------------------------------------------------------------------------------
!             Update global matrices from local matrices
!------------------------------------------------------------------------------
              IF ( TransientSimulation ) THEN
                LocalMassMatrix = 0.0d0
                CALL Add1stOrderTime(LocalMassMatrix,LocalStiffMatrix,LocalForce, &
                       dt, n, 3, MagneticPerm(NodeIndexes), Solver )
              END IF

              CALL UpdateGlobalEquations( StiffMatrix, LocalStiffMatrix, &
                ForceVector, LocalForce, n, 3, MagneticPerm(NodeIndexes) )
!------------------------------------------------------------------------------
            END IF
!------------------------------------------------------------------------------
          END IF
        END DO
        END IF
      END DO
!------------------------------------------------------------------------------

      CALL FinishAssembly( Solver,ForceVector )

!------------------------------------------------------------------------------
!     Dirichlet boundary conditions
!------------------------------------------------------------------------------
      CALL DefaultDirichletBCs()

!------------------------------------------------------------------------------

      CALL Info( 'MagneticSolve', 'Set boundaries done', Level=4 )
!------------------------------------------------------------------------------
!     Solve the system and check for convergence
!------------------------------------------------------------------------------
      PrevUNorm = UNorm

      UNorm = DefaultSolve()

      st = CPUTIme()-st
      totat = totat + at
      totst = totst + st
      WRITE(*,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,' Assembly: (s)', at, totat
      WRITE(*,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,' Solve:    (s)', st, totst

!------------------------------------------------------------------------------
!     If boundary fields have been defined in normal/tangetial coordinate
!     systems, we´ll have to rotate the solution back to coordinate axis
!     directions
!------------------------------------------------------------------------------
      IF ( NumberOfBoundaryNodes > 0 ) THEN
        DO i=1,Model % NumberOfNodes
          k = BoundaryReorder(i)

          IF ( k > 0 ) THEN
            j = MagneticPerm(i)

            IF ( j > 0 ) THEN
              Bu = MagneticField( 3*(j-1)+1 )
              Bv = MagneticField( 3*(j-1)+2 )
              Bw = MagneticField( 3*(j-1)+3 )

              RM(1,:) = BoundaryNormals(k,:)
              RM(2,:) = BoundaryTangent1(k,:)
              RM(3,:) = BoundaryTangent2(k,:)
              CALL InvertMatrix( RM,3 )

              MagneticField(3*(j-1)+1) = RM(1,1)*Bu+RM(2,1)*Bv+RM(3,1)*Bw
              MagneticField(3*(j-1)+2) = RM(1,2)*Bu+RM(2,2)*Bv+RM(2,2)*Bw
              MagneticField(3*(j-1)+3) = RM(1,3)*Bu+RM(2,3)*Bv+RM(3,3)*Bw
            END IF
          END IF
        END DO 
      END IF
!------------------------------------------------------------------------------
      IF ( PrevUNorm + UNorm /= 0.0d0 ) THEN
         RelativeChange = 2 * ABS(PrevUNorm-UNorm)/(UNorm + PrevUNorm)
      ELSE
         RelativeChange = 0.0d0
      END IF

      WRITE( Message, * ) 'Result Norm     : ',UNorm
      CALL Info( 'MagneticSolve', Message, Level=4 )
      WRITE( Message, * ) 'Relative Change : ',RelativeChange
      CALL Info( 'MagneticSolve', Message, Level=4 )

      IF ( RelativeChange < NewtonTol .OR. &
             iter > NewtonIter ) NewtonLinearization = .TRUE.

     IF ( RelativeChange < NonLinearTol ) EXIT
    END DO

!------------------------
! Calculate div B
!#if 0
!    M1 => ExB(1:3*Model%NumberOfNodes-2:3)
!    M2 => ExB(2:3*Model%NumberOfNodes-1:3)
!    M3 => ExB(3:3*Model%NumberOfNodes-0:3)
!    CALL Divergence(divB,M1,M2,M3,ExMagPerm)
!#endif
!---------------------------

    M1 => MagneticField(1:3*LocalNodes-2:3)
    M2 => MagneticField(2:3*LocalNodes-1:3)
    M3 => MagneticField(3:3*LocalNodes-0:3)

    E1 => ElectricCurrent(1:3*LocalNodes-2:3)
    E2 => ElectricCurrent(2:3*LocalNodes-1:3)
    E3 => ElectricCurrent(3:3*LocalNodes-0:3)

!------------------------------------------------------------------------------
!   Compute the magnetic flux density from the vector potential: B = curl A
!------------------------------------------------------------------------------

    IF (CurrentCoordinateSystem() == CylindricSymmetric) THEN
      CALL AxiSCurl( M1,M2,M3,E1,E2,E3,MagneticPerm )
    ELSE
      CALL Curl( M1,M2,M3,E1,E2,E3,MagneticPerm )
    END IF
    CALL InvalidateVariable( Model % Meshes, Solver % Mesh,'Electric Current' )

!------------------------------------------------------------------------------
!   Compute the Lorentz force 
!   (high-frequency B_i contribution or total)
!------------------------------------------------------------------------------

!#if 0
!    LrFr => LrF(1:3*LocalNodes-2:3)
!    LrFz => LrF(2:3*LocalNodes-1:3)
!    LrFp => LrF(3:3*LocalNodes:3)
!
!    CALL LorentzForceNodal(LrFr,LrFz,LrFp,M1,M2,M3,MagneticPerm)
!#endif

  CONTAINS

   SUBROUTINE LorentzForceNodal( LrFr,LrFz,LrFp,Br,Bz,Bp,Reorder )
     USE Types
     IMPLICIT NONE
     REAL(KIND=dp) :: Br(:),Bz(:),Bp(:)
     REAL(KIND=dp) :: LrFr(:),LrFz(:),LrFp(:), Lorentz(3)
     INTEGER :: Reorder(:)

     TYPE(Element_t), POINTER :: Element
     TYPE(Nodes_t) :: Nodes 


     LOGICAL :: Stat

     INTEGER, POINTER :: NodeIndexes(:),Visited(:)
     INTEGER :: p,q,i,t,n

     REAL(KIND=dp) :: u,v,w

     REAL(KIND=dp) :: ddBasisddx(MAX_NODES,3,3)
     REAL(KIND=dp) :: Basis(MAX_NODES)
     REAL(KIND=dp) :: dBasisdx(MAX_NODES,3),SqrtElementMetric
  
!------------------------------------------

     ALLOCATE( Visited(CurrentModel % NumberOfNodes) )

     ALLOCATE(Nodes % x(MAX_NODES),Nodes % y(MAX_NODES),Nodes % z(MAX_NODES))

     Visited = 0

     LrFr = 0.0d0
     LrFz = 0.0d0
     LrFp = 0.0d0

     DO t=1,CurrentModel % NumberOfBulkElements

        Element => CurrentModel % Elements(t)
        n = Element % TYPE % NumberOfNodes
        NodeIndexes => Element % NodeIndexes

        Nodes % x(1:n) = CurrentModel % Nodes % x( NodeIndexes )
        Nodes % y(1:n) = CurrentModel % Nodes % y( NodeIndexes )
        Nodes % z(1:n) = CurrentModel % Nodes % z( NodeIndexes )

        IF ( MINVAL(Reorder(NodeIndexes)) > 0 ) THEN

           DO p=1,n

              q = Reorder(NodeIndexes(p))
              u = Element % TYPE % NodeU(p)
              v = Element % TYPE % NodeV(p)

              IF ( Element % TYPE % DIMENSION == 3 ) THEN
                 w = Element % TYPE % NodeW(p)
              ELSE
                 w = 0.0D0
              END IF

              stat = ElementInfo( Element, Nodes, u, v, w, SqrtElementMetric, &
                   Basis, dBasisdx, ddBasisddx, .FALSE. )

! Call LorentzForce from here (and zero B_e, B_ac for high-f part)
! For r < 1.0d-10, avoid the 1/r term in ComputeLorentz (if CylindricSymmetric)
! by using SI_UNITS
              Lorentz = LorentzForce( Element,Nodes,u,v,w )
              LrFr(q) = LrFr(q) + Lorentz(1)
              LrFz(q) = LrFz(q) + Lorentz(2)
              LrFp(q) = LrFp(q) + Lorentz(3)

              Visited(q) = Visited(q) + 1
           
           END DO
        END IF
      END DO

      DO i=1,CurrentModel % NumberOfNodes
         IF ( Visited(i) > 1 ) THEN
            LrFr(i) = LrFr(i) / Visited(i)
            LrFp(i) = LrFp(i) / Visited(i)
            LrFz(i) = LrFz(i) / Visited(i)
         END IF
      END DO

      DEALLOCATE( Visited )
      DEALLOCATE( Nodes % x, Nodes % y, Nodes % z )

    END SUBROUTINE LorentzForceNodal

!------------------------------------------------------------------------------
  END SUBROUTINE MagneticSolver
!------------------------------------------------------------------------------
