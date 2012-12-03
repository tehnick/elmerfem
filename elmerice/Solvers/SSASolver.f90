!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
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
! *****************************************************************************/
! ******************************************************************************
! *
! *  Authors: Olivier Gagliardini             
! *  Email:   gagliar@lgge.obs.ujf-grenoble.fr
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: 30. April 2010
! * 
! *****************************************************************************
!> SSolver to inquire the velocity from the SSA solution            
!> Variables IntVisco dof = 1                                                 
!> Exported Variables SSABasalFlow, dof=dim-1                                 
!> Exported Variables SSAFlow, dof=dim                                         
!> Need to first compute the Depth and the FreeSurfGrad using FlowDepth Solver
SUBROUTINE SSABasalSolver( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the in-plane basal velocity with the SSA solution !
!  To be computed only at the base. Use then the SSASolver to export verticaly 
!  the basal velocity and compute the vertical velocity and pressure (if needed)
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
  USE DefUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Nodes_t)   :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement, Element, ParentElement, BoundaryElement
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(ValueList_t), POINTER :: SolverParams, BodyForce, Material, BC
  TYPE(Variable_t), POINTER :: PointerToVariable, Grad1Sol, Grad2Sol, &
                               DepthSol, VeloSol, ViscoSol, DensSol

  LOGICAL :: AllocationsDone = .FALSE., Found, GotIt, CalvingFront

  INTEGER :: i, n, m, t, istat, DIM, p, STDOFs
  INTEGER :: NonlinearIter, iter, other_body_id
          
  INTEGER, POINTER :: Permutation(:), &
       DepthPerm(:), GradSurface1Perm(:), GradSurface2Perm(:), &
       NodeIndexes(:), ViscoPerm(:), DensPerm(:)

  REAL(KIND=dp), POINTER :: ForceVector(:)
  REAL(KIND=dp), POINTER :: VariableValues(:), Depth(:), GradSurface1(:), &
                            GradSurface2(:), Viscosity(:), Density(:) 
  REAL(KIND=dp) :: UNorm, cn, dd, NonlinearTol, MinSRInv, rhow, &
                   PrevUNorm, relativeChange

  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), LOAD(:), FORCE(:), &
           NodalGravity(:), NodalViscosity(:), NodalDensity(:), &
           NodalDepth(:), NodalSurfGrad1(:), NodalSurfGrad2(:), &
           NodalU(:), NodalV(:), NodalSliding(:,:)

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
  REAL(KIND=dp) :: at, at0, CPUTime, RealTime
       

  SAVE STIFF, LOAD, FORCE, AllocationsDone, DIM, SolverName, ElementNodes
  SAVE NodalGravity, NodalViscosity, NodalDensity, &
           NodalDepth, NodalSurfGrad1, NodalSurfGrad2, &
           NodalU, NodalV, NodeIndexes, NodalSliding
!------------------------------------------------------------------------------
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values
  STDOFs = PointerToVariable % DOFs 
  WRITE(SolverName, '(A)') 'SSASolver-SSABasalSolver'

!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------
        DIM = CoordinateSystemDimension()

        ViscoSol => VariableGet( Solver % Mesh % Variables, & 
                          'Integrated Viscosity' ) 
        IF (ASSOCIATED(ViscoSol)) THEN
           Viscosity => ViscoSol % Values
           ViscoPerm => ViscoSol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >Integrated Viscosity<')
        END IF
        DensSol => VariableGet( Solver % Mesh % Variables, & 
                          'Mean Density' ) 
        IF (ASSOCIATED(DensSol)) THEN
           Density => DensSol % Values
           DensPerm => DensSol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >Integrated Viscosity<')
        END IF

        DepthSol => VariableGet( Solver % Mesh % Variables, 'Depth' )
        IF (ASSOCIATED(DepthSol)) THEN
           Depth => DepthSol % Values
           DepthPerm => DepthSol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >Depth<')
        END IF
        Grad1Sol => VariableGet( Solver % Mesh % Variables, 'FreeSurfGrad1')
        IF (ASSOCIATED(Grad1Sol)) THEN
           GradSurface1 => Grad1Sol % Values
           GradSurface1Perm => Grad1Sol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >FreeSurfGrad1<')
        END IF
        IF (dim > 2) THEN
           Grad2Sol => VariableGet( Solver % Mesh % Variables, 'FreeSurfGrad2')
           IF (ASSOCIATED(Grad2Sol)) THEN
              GradSurface2 => Grad2Sol % Values
              GradSurface2Perm => Grad2Sol % Perm
           ELSE
              CALL FATAL(SolverName,'Could not find variable >FreeSurfGrad2<')
           END IF
        END IF

  !--------------------------------------------------------------
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed  ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays
     M = Model % Mesh % NumberOfNodes
     IF (AllocationsDone) DEALLOCATE(FORCE, LOAD, STIFF, NodalGravity, &
                       NodalViscosity, NodalDensity, NodalDepth, &
                       NodalSurfGrad1, NodalSurfGrad2, NodalU, NodalV, &
                       NodalSliding, ElementNodes % x, &
                       ElementNodes % y, ElementNodes % z )

     ALLOCATE( FORCE(N), LOAD(N), STIFF(N,N), &
          NodalGravity(N), NodalDensity(N), NodalViscosity(N), &
          NodalDepth(N), NodalSurfGrad1(N), NodalSurfGrad2(N), &
          NodalU(N), NodalV(N), NodalSliding(2,N), &
          ElementNodes % x(M), ElementNodes % y(M), ElementNodes % z(M), &
           STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL Fatal( SolverName, 'Memory allocation error.' )
     END IF

     AllocationsDone = .TRUE.
     CALL INFO( SolverName, 'Memory allocation done.',Level=1 )
  END IF

     StiffMatrix => Solver % Matrix
     ForceVector => StiffMatrix % RHS

!------------------------------------------------------------------------------
!    Do some additional initialization, and go for it
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
      NonlinearTol = GetConstReal( Solver % Values, &
        'Nonlinear System Convergence Tolerance' )

      NonlinearIter = GetInteger( Solver % Values, &
         'Nonlinear System Max Iterations',GotIt )

      IF ( .NOT.GotIt ) NonlinearIter = 1

!------------------------------------------------------------------------------
      DO iter=1,NonlinearIter

       at  = CPUTime()
       at0 = RealTime()

       CALL Info( SolverName, ' ', Level=4 )
       CALL Info( SolverName, ' ', Level=4 )
       CALL Info( SolverName, &
                   '-------------------------------------',Level=4 )
       WRITE( Message, * ) 'SSA BASAL VELCOITY NON-LINEAR ITERATION', iter
       CALL Info( SolverName, ' ', Level=4 )
       CALL Info( SolverName, &
                   '-------------------------------------',Level=4 )
       CALL Info( SolverName, ' ', Level=4 )

  ! Loop over the velocity components and pressure 
  ! If DIM = 2 u, w, p
  ! If DIM = 3 u, v, w, p
  !-----------------------------------------------

! VariableValues = 0.0d0

  !Initialize the system and do the assembly:
  !------------------------------------------
  CALL DefaultInitialize()
  ! bulk assembly
  DO t=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(t)
     IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
     n = GetElementNOFNodes()

     NodeIndexes => Element % NodeIndexes

 ! set coords of highest occuring dimension to zero (to get correct path element)
        !-------------------------------------------------------------------------------
        ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
        IF (DIM == 2) THEN
           ElementNodes % y(1:n) = 0.0_dp
           ElementNodes % z(1:n) = 0.0_dp
        ELSE IF (DIM == 3) THEN
           ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
           ElementNodes % z(1:n) = 0.0_dp
        ELSE
           WRITE(Message,'(a,i1,a)')&
                'It is not possible to compute SSA problems in DIM=',&
                DIM, ' dimensions. Aborting'
           CALL Fatal( SolverName, Message)
           STOP
        END IF

     ! Read the gravity in the Body Force Section 
     BodyForce => GetBodyForce()
     NodalGravity = 0.0_dp
     IF ( ASSOCIATED( BodyForce ) ) THEN
           IF (DIM==2) THEN 
           NodalGravity(1:n) = ListGetReal( &
                   BodyForce, 'Flow BodyForce 2', n, NodeIndexes, Found)
           ELSE 
           NodalGravity(1:n) = ListGetReal( &
                   BodyForce, 'Flow BodyForce 3', n, NodeIndexes, Found)
           END IF
     END IF
     
     ! Read the Viscosity eta, density, and exponent m in Material Section
     ! Same definition as NS Solver in Elmer - n=1/m , A = 1/ (2 eta^n) 
     Material => GetMaterial()

     NodalDensity(1:n) = Density(DensPerm(NodeIndexes(1:n)))
     NodalViscosity(1:n) = Viscosity(ViscoPerm(NodeIndexes(1:n)))
     
     cn = ListGetConstReal( Material, 'Viscosity Exponent',Found)
     MinSRInv = ListGetConstReal( Material, 'Critical Shear Rate',Found)
     rhow = GetConstReal( Model % Constants, 'Water Density', Found )

     ! Get the Nodal value of Depth, FreeSurfGrad1 and FreeSurfGrad2
     NodalDepth(1:n) = Depth(DepthPerm(NodeIndexes(1:n)))


     NodalSurfGrad1(1:n) = GradSurface1(GradSurface1Perm(NodeIndexes(1:n)))
     NodalSurfGrad2 = 0.0_dp
     IF (DIM==3) NodalSurfGrad2(1:n) = GradSurface2(GradSurface2Perm(NodeIndexes(1:n)))

     ! Previous Velocity 
     NodalU(1:n) = VariableValues(STDOFs*(Permutation(NodeIndexes(1:n))-1)+1)
     NodalV = 0.0
     IF (DIM.EQ.3) NodalV(1:n) = VariableValues(STDOFs*(Permutation(NodeIndexes(1:n))-1)+2)


     CALL LocalMatrixUVSSA (  STIFF, FORCE, Element, n, ElementNodes, NodalGravity, &
        NodalDensity, NodalViscosity, NodalDepth, NodalSurfGrad1, &
        NodalSurfGrad2, NodalU, NodalV, cn, MinSRInv)

     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  
!  
! Calving front condition and basal friction
!
  DO t=1,Solver % Mesh % NUmberOfBoundaryElements
     BoundaryElement => GetBoundaryElement(t)
     IF ( GetElementFamily() == 1 ) CYCLE
     NodeIndexes => BoundaryElement % NodeIndexes
     IF (ParEnv % myPe .NE. BoundaryElement % partIndex) CYCLE
     n = GetElementNOFNodes()
     FORCE = 0.0e0
     STIFF = 0.0e0

 ! set coords of highest occuring dimension to zero (to get correct path element)
        !-------------------------------------------------------------------------------
        ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
        IF (DIM == 2) THEN
           ElementNodes % y(1:n) = 0.0_dp
           ElementNodes % z(1:n) = 0.0_dp
        ELSE IF (DIM == 3) THEN
           ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
           ElementNodes % z(1:n) = 0.0_dp
        ELSE
           WRITE(Message,'(a,i1,a)')&
                'It is not possible to compute SSA problems in DIM=',&
                DIM, ' dimensions. Aborting'
           CALL Fatal( SolverName, Message)
           STOP
        END IF
     BC => GetBC()
     IF ( ASSOCIATED( BC ) ) THEN

! Basal Friction (We should go only for bedrock BC)            
! Find the keyword 'SSA Slip Coefficient'
     NodalSliding = 0.0_dp
     NodalSliding(1,1:n) = ListGetReal( &
           BC, 'SSA Slip Coefficient 1', n, NodeIndexes(1:n), Found )
     IF (DIM==3) THEN
        NodalSliding(2,1:n) = ListGetReal( &
             Material, 'SSA Slip Coefficient 2', n, NodeIndexes(1:n), Found )  
     END IF
     IF (ANY(NodalSliding(:,1:n)>0.0)) THEN 
        CALL LocalMatrixSliding( STIFF, FORCE,  &
                    BoundaryElement, n,  ElementNodes, NodalSliding )
        CALL DefaultUpdateEquations( STIFF, FORCE )
     END IF

! Find the nodes for which 'Calving Front' = True             
        CalvingFront=.False. 
        CalvingFront = ListGetLogical( BC, &
                             'Calving Front', GotIt )
                             
        IF (CalvingFront) THEN
           NodalDensity(1:n) = Density(DensPerm(NodeIndexes(1:n)))
           NodalViscosity(1:n) = Viscosity(ViscoPerm(NodeIndexes(1:n)))
           NodalDepth(1:n) = Depth(DepthPerm(NodeIndexes(1:n)))

           CALL LocalMatrixBCSSA(  STIFF, FORCE, BoundaryElement, n, &
                  NodalDensity, NodalGravity, NodalDepth, rhow )
           CALL DefaultUpdateEquations( STIFF, FORCE )
        END IF
     END IF
  END DO

  CALL DefaultFinishAssembly()

  ! Dirichlet 
  CALL DefaultDirichletBCs()
  
!------------------------------------------------------------------------------
!     Solve the system and check for convergence
!------------------------------------------------------------------------------
      PrevUNorm = UNorm

      UNorm = DefaultSolve()

      IF ( PrevUNorm + UNorm /= 0.0d0 ) THEN
         RelativeChange = 2.0d0 * ABS( PrevUNorm - UNorm) / ( PrevUnorm + UNorm)
      ELSE
         RelativeChange = 0.0d0
      END IF

      WRITE( Message, * ) 'Result Norm   : ', UNorm, PrevUNorm
      CALL Info(SolverName, Message, Level=4 )
      WRITE( Message, * ) 'Relative Change : ', RelativeChange
      CALL Info(SolverName, Message, Level=4 )

!------------------------------------------------------------------------------

      IF ( RelativeChange < NonLinearTol ) EXIT

!------------------------------------------------------------------------------

  END DO ! Loop Non-Linear Iterations

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixUVSSA(  STIFF, FORCE, Element, n, Nodes, gravity, &
           Density, Viscosity, LocalDepth, SurfGrad1, SurfGrad2, LocalU, &
           LocalV, cm, MinSRInv )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), gravity(:), Density(:), &
                     Viscosity(:), LocalDepth(:), SurfGrad1(:), SurfGrad2(:), &
                     LocalU(:), LocalV(:)
    INTEGER :: n, cp
    REAL(KIND=dp) :: cm
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ 
    REAL(KIND=dp) :: g, rho, eta, h, dhdx, dhdy, dU2dz2
    REAL(KIND=dp) :: gradS(2), A(2,2), Exx, Eyy, Exy, Ezz, Ee, MinSRInv                            
    LOGICAL :: Stat
    INTEGER :: i, j, t, p, q , dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
!------------------------------------------------------------------------------
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

! Needed Intergration Point value

       g = ABS(SUM( Gravity(1:n) * Basis(1:n) ))
       rho = SUM( Density(1:n) * Basis(1:n) )
       eta = SUM( Viscosity(1:n) * Basis(1:n) )
       gradS(1) = SUM( SurfGrad1(1:n) * Basis(1:n) )
       gradS(2) = SUM( SurfGrad2(1:n) * Basis(1:n) )
       h = SUM( LocalDepth(1:n) * Basis(1:n) )
       
! In the non-linear case, effective viscosity       
       IF (cm.NE.1.0_dp) THEN
           Exx = SUM(LocalU(1:n)*dBasisdx(1:n,1))
           Eyy = 0.0
           IF (DIM.EQ.3) THEN
              Eyy = SUM(LocalV(1:n)*dBasisdx(1:n,2))
              Ezz = -Exx - Eyy
              Exy = SUM(LocalU(1:n)*dBasisdx(1:n,2))
              Exy = 0.5*(Exy + SUM(LocalV(1:n)*dBasisdx(1:n,1)))
              Ee = 0.5*(Exx**2.0 + Eyy**2.0 + Ezz**2.0) + Exy**2.0
              Ee = SQRT(Ee)
           ELSE
              Ee = ABS(Exx)
           END IF
           IF (Ee < MinSRInv) Ee = MinSRInv
           eta = eta * 0.5 * (2**cm) * Ee**(cm-1.0)
       END IF 

       A = 0.0_dp
       DO p=1,n
         DO q=1,n
         A(1,1) = 2.0*dBasisdx(q,1)*dBasisdx(p,1)  
           IF (DIM.EQ.3) THEN
           A(1,1) = A(1,1) + 0.5*dBasisdx(q,2)*dBasisdx(p,2)
           A(1,2) = dBasisdx(q,2)*dBasisdx(p,1) + &
                             0.5*dBasisdx(q,1)*dBasisdx(p,2)
           A(2,1) = dBasisdx(q,1)*dBasisdx(p,2) + &
                             0.5*dBasisdx(q,2)*dBasisdx(p,1)
           A(2,2) = 2.0*dBasisdx(q,2)*dBasisdx(p,2) +&
                             0.5*dBasisdx(q,1)*dBasisdx(p,1)  
         END IF
           A = 2.0 * eta * A
           DO i=1,DIM-1
             DO j=1,DIM-1
                STIFF((dim-1)*(p-1)+i,(dim-1)*(q-1)+j) = STIFF((dim-1)*(p-1)+i,(dim-1)*(q-1)+j) +& 
                      A(i,j) * IP % S(t) * detJ 
             END DO 
           END DO 
         END DO
         DO i=1,DIM-1
         FORCE((dim-1)*(p-1)+i) =   FORCE((dim-1)*(p-1)+i) - &   
            rho*g*h*gradS(i) * IP % s(t) * detJ * Basis(p) 
         END DO
       END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixUVSSA
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixSliding(  STIFF, FORCE, Element, n, Nodes,  &
            Sliding )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), Sliding(:,:) 
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ 
    REAL(KIND=dp) :: s(2)
    LOGICAL :: Stat
    INTEGER :: i, j, t, p, q , DIM
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
!------------------------------------------------------------------------------
    STIFF = 0.0d0
    FORCE = 0.0d0

    DIM = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

! Needed Integration Point value

       s = 0.0_dp
       DO i=1,DIM-1
          s(i) = SUM( Sliding(i,1:n) * Basis(1:n) )
       END DO

       DO p=1,n
         DO q=1,n
           DO i=1,DIM-1
             STIFF((DIM-1)*(p-1)+i,(DIM-1)*(q-1)+i) = STIFF((DIM-1)*(p-1)+i,(DIM-1)*(q-1)+i) +& 
                    s(i) * Basis(q) * Basis(p) * IP % S(t) * detJ 
           END DO 
         END DO
       END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixSliding
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBCSSA(  STIFF, FORCE, Element, n, Density, & 
                      Gravity, Depth, rhow)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), density(:), Gravity(:), Depth(:),&
                         rhow
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    TYPE(Element_t), TARGET :: SideElement
    TYPE(Element_t), POINTER :: EdgeElement 
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3), &
                      DetJ,Normal(3), rhoi, g, alpha, h, norm, MeanDepth
    LOGICAL :: Stat
    INTEGER :: t, DIM, iNode(2), i, j, nn
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: FaceNodes, Nodes
    SAVE FaceNodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( FaceNodes, Element )
!------------------------------------------------------------------------------
    STIFF = 0.0d0
    FORCE = 0.0d0

    DIM = CoordinateSystemDimension()

! The front force is a concentrated nodal force in 2D and
! a force distributed along a line in 3D    
! 2D Case : concentrated force at each nodes
    IF (DIM==2) THEN
      DO i = 1, n
         g = ABS( Gravity(i) )
         rhoi = Density(i)
         h = Depth(i) 
         alpha = 0.5*rhoi/rhow * (rhow - rhoi) * g * h**2.0 
         FORCE(i) = FORCE(i) + alpha
      END DO

! 3D Case : force distributed along the deepest line       
    ELSE IF (DIM==3) THEN 
      ALLOCATE( Nodes % x(2), Nodes % y(2), Nodes % z(2) )
      IF (n <=4) THEN     
          ! T3 or Q4 elements 
          nn = 2
          SideElement % TYPE => GetElementType( 202, .FALSE.)
          SideElement % Bdofs = 0
          EdgeElement => SideElement

! Find the two nodes on the deepest line      
          MeanDepth = Depth(1)
          DO i=2,n
             MeanDepth = MeanDepth + Depth(i)
          END DO
          MeanDepth = MeanDepth/n

          j=1
          DO i=1,n
             IF (Depth(i) > MeanDepth) THEN
                iNode(j)=i
                j=j+1
             END IF
          END DO
! triangle with only one node on the boundary          
          IF ((n==3).AND.(j==1)) RETURN

          Nodes % x(1:nn) = FaceNodes % x(iNode(1:nn))
          Nodes % y(1:nn) = FaceNodes % y(iNode(1:nn))
          Nodes % z(1:nn) = 0.0  

          IP = GaussPoints( EdgeElement )
          DO t=1,IP % n
             stat = ElementInfo( EdgeElement, Nodes, IP % U(t), IP % V(t), &
                 IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )
 
             g = ABS(SUM( Gravity(iNode(1:nn)) * Basis(1:nn) ))
             rhoi = SUM( Density(iNode(1:nn)) * Basis(1:nn) )
             h = SUM( Depth(iNode(1:nn)) * Basis(1:nn))
             alpha = 0.5*rhoi/rhow * (rhow - rhoi) * g * h**2.0 


! Normal in the (x,y) plane
             Normal = NormalVector( EdgeElement, Nodes, IP % U(t), IP % V(t), .TRUE.)
             norm=SQRT(normal(1)**2.0+normal(2)**2.0)
             Normal(1) = Normal(1)/norm
             Normal(2) = Normal(2)/norm

             DO p=1,nn
                DO i=1,DIM-1
                   FORCE((DIM-1)*(iNode(p)-1)+i) =   FORCE((DIM-1)*(iNode(p)-1)+i) +&   
                    alpha * Normal(i) * IP % s(t) * detJ * Basis(p) 
                END DO
             END DO
          END DO

         DEALLOCATE( Nodes % x, Nodes % y, Nodes % z )
      ELSE
         CALL FATAL('SSASolver-SSABasalSolver','Do not work for non-linear elements')
      END IF 
    
    ELSE   
      CALL FATAL('SSASolver-SSABasalSolver','Do not work for DIM <> 2 or 3')
    END IF


!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBCSSA
!------------------------------------------------------------------------------
END SUBROUTINE SSABasalSolver
!------------------------------------------------------------------------------




! *****************************************************************************
!>   Compute the depth integrated viscosity = sum_zb^zs eta dz
!>     and the depth integrated density = sum_zb^zs rho dz
SUBROUTINE GetMeanValueSolver( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
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
  USE DefUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Element_t),POINTER :: CurrentElement, Element, ParentElement, &
                             BoundaryElement
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(ValueList_t), POINTER :: SolverParams, BodyForce, Material
  TYPE(Variable_t), POINTER :: PointerToVariable, IntViscoSol, IntDensSol,&
                                DepthSol  

  LOGICAL :: AllocationsDone = .FALSE., Found

  INTEGER :: i, n, m, t, istat, DIM, COMP, other_body_id   
  INTEGER, POINTER :: Permutation(:), NodeIndexes(:), IntViscoPerm(:),&
                      IntDensPerm(:), DepthPerm(:) 
       
  REAL(KIND=dp), POINTER :: ForceVector(:)
  REAL(KIND=dp), POINTER :: VariableValues(:), IntVisco(:), IntDens(:), Depth(:)
  REAL(KIND=dp) :: Norm, cn, dd 

  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), LOAD(:), FORCE(:), &
           NodalVar(:) 

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName

  SAVE STIFF, LOAD, FORCE, AllocationsDone, DIM, SolverName
  SAVE NodalVar 
!------------------------------------------------------------------------------
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values
  WRITE(SolverName, '(A)') 'SSASolver-IntValue'

  IntViscoSol => VariableGet( Solver % Mesh % Variables, 'Integrated Viscosity' )
  IF (ASSOCIATED(IntViscoSol)) THEN
     IntVisco => IntViscoSol % Values
     IntViscoPerm => IntViscoSol % Perm
  ELSE
     CALL FATAL(SolverName,'Could not find variable >Integrated Viscosity<')
  END IF
  IntDensSol => VariableGet( Solver % Mesh % Variables, 'Mean Density' )
  IF (ASSOCIATED(IntDensSol)) THEN
     IntDens => IntDensSol % Values
     IntDensPerm => IntDensSol % Perm
  ELSE
     CALL FATAL(SolverName,'Could not find variable >Mean Density<')
  END IF
  DepthSol => VariableGet( Solver % Mesh % Variables, 'Depth' )
  IF (ASSOCIATED(DepthSol)) THEN
     Depth => DepthSol % Values
     DepthPerm => DepthSol % Perm
  ELSE
     CALL FATAL(SolverName,'Could not find variable >Depth<')
  END IF
  !--------------------------------------------------------------
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed  ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays
     M = Model % Mesh % NumberOfNodes
     IF (AllocationsDone) DEALLOCATE(FORCE, LOAD, STIFF, NodalVar) 

     ALLOCATE( FORCE(N), LOAD(N), STIFF(N,N), NodalVar(N), &
                          STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL Fatal( SolverName, 'Memory allocation error.' )
     END IF
     AllocationsDone = .TRUE.
     CALL INFO( SolverName, 'Memory allocation done.',Level=1 )
  END IF

     StiffMatrix => Solver % Matrix
     ForceVector => StiffMatrix % RHS

! Loop for viscosity and density
DO COMP=1, 2
! No non-linear iteration, no time dependency  
  VariableValues = 0.0d0
  Norm = Solver % Variable % Norm

  !Initialize the system and do the assembly:
  !------------------------------------------
  CALL DefaultInitialize()
  ! bulk assembly
  DO t=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(t)
     IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
     n = GetElementNOFNodes()

     NodeIndexes => Element % NodeIndexes
     Material => GetMaterial(Element)

     IF (COMP==1) THEN
     ! Read the Viscosity eta, 
     ! Same definition as NS Solver in Elmer - n=1/m , A = 1/ (2 eta^n) 
     NodalVar = 0.0D0
     NodalVar(1:n) = ListGetReal( &
         Material, 'Viscosity', n, NodeIndexes, Found )
     ELSE IF (COMP==2) THEN
     NodalVar = 0.0D0
     NodalVar(1:n) = ListGetReal( &
         Material, 'Density', n, NodeIndexes, Found )
     END IF

     CALL LocalMatrix (  STIFF, FORCE, Element, n, NodalVar )
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  
  ! Neumann conditions 
  DO t=1,Solver % Mesh % NUmberOfBoundaryElements
     BoundaryElement => GetBoundaryElement(t)
     IF ( GetElementFamily() == 1 ) CYCLE
     NodeIndexes => BoundaryElement % NodeIndexes
     IF (ParEnv % myPe .NE. BoundaryElement % partIndex) CYCLE
     n = GetElementNOFNodes()

! Find the Parent element     
     other_body_id = BoundaryElement % BoundaryInfo % outbody
     IF (other_body_id < 1) THEN ! only one body in calculation
         ParentElement => BoundaryElement % BoundaryInfo % Right
         IF ( .NOT. ASSOCIATED(ParentElement) ) ParentElement => BoundaryElement % BoundaryInfo % Left
         ELSE ! we are dealing with a body-body boundary and asume that the normal is pointing outwards
             ParentElement => BoundaryElement % BoundaryInfo % Right
             IF (ParentElement % BodyId == other_body_id) ParentElement => BoundaryElement % BoundaryInfo % Left
         END IF

     Material => GetMaterial(ParentElement)

     IF (COMP==1) THEN
     ! Read the Viscosity eta, 
     ! Same definition as NS Solver in Elmer - n=1/m , A = 1/ (2 eta^n) 
     NodalVar = 0.0D0
     NodalVar(1:n) = ListGetReal( &
         Material, 'Viscosity', n, NodeIndexes, Found )
     ELSE IF (COMP==2) THEN
     NodalVar = 0.0D0
     NodalVar(1:n) = ListGetReal( &
         Material, 'Density', n, NodeIndexes, Found )
     END IF
     CALL LocalMatrixBC(  STIFF, FORCE, BoundaryElement, n, NodalVar)
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO

  CALL DefaultFinishAssembly()
  ! Dirichlet 
  IF (COMP==1) THEN
     CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
          'Integrated Viscosity', 1,1, Permutation )
  ELSE
     CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
          'Mean Density', 1,1, Permutation )
  END IF
  Norm = DefaultSolve()

  ! Save the solution on the right variable
  IF (COMP==1) THEN
     DO i = 1, Model % Mesh % NumberOfNodes
        IF (IntViscoPerm(i)>0) THEN
            IntVisco(IntViscoPerm(i)) = VariableValues(Permutation(i)) 
        END IF
     END DO
  ELSE IF (COMP==2) THEN
     DO i = 1, Model % Mesh % NumberOfNodes
        IF (IntDensPerm(i)>0) THEN
            IntDens(IntDensPerm(i)) = VariableValues(Permutation(i)) 
            IF (Depth(DepthPerm(i))>0.0_dp) IntDens(IntDensPerm(i)) = &
                IntDens(IntDensPerm(i)) / Depth(DepthPerm(i))
                                                 
                                                 
        END IF
     END DO
  END IF
  
END DO !COMP


CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  STIFF, FORCE, Element, n, var)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), var(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ, grad
    LOGICAL :: Stat
    INTEGER :: t, p,q ,dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
          IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )
         
        grad  = SUM( var(1:n) * dBasisdx(1:n,dim) )
        FORCE(1:n) = FORCE(1:n) + grad * IP % s(t) * DetJ  * Basis(1:n)
       
       DO p=1,n
         DO q=1,n
           STIFF(p,q) = STIFF(p,q) + IP % S(t) * detJ * dBasisdx(q,dim)*dBasisdx(p,dim)
         END DO
       END DO
    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBC(  STIFF, FORCE, Element, n, var ) 
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), var(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3), &
                      DetJ,Normal(3), eta, grad 
    LOGICAL :: Stat
    INTEGER :: t, dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
      stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
       IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       grad  = SUM( var(1:n) * Basis(1:n) )

      Normal = NormalVector( Element, Nodes, IP % U(t), IP % V(t), .TRUE.)
      FORCE(1:n) = FORCE(1:n) - grad * IP % s(t) * DetJ * Normal(dim) * Basis(1:n)
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBC
!------------------------------------------------------------------------------
END SUBROUTINE GetMeanValueSolver
!------------------------------------------------------------------------------


! *****************************************************************************
SUBROUTINE SSASolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: SSASolver
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Export vertically the SSABasal Velocity (given as a Dirichlet Boundary condition) 
!  Compute also the vertical velocity and the pressure
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
  USE DefUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Element_t),POINTER :: CurrentElement, Element
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(ValueList_t), POINTER :: SolverParams, BodyForce, Material
  TYPE(Variable_t), POINTER :: PointerToVariable, Grad1Sol, Grad2Sol, &
                               DepthSol, VeloSol

  LOGICAL :: AllocationsDone = .FALSE., Found

  INTEGER :: i, n, m, t, istat, DIM, p, Indexes(128), COMP 
  INTEGER, POINTER :: Permutation(:), VeloPerm(:), &
       DepthPerm(:), GradSurface1Perm(:), GradSurface2Perm(:), &
       NodeIndexes(:)

  REAL(KIND=dp), POINTER :: ForceVector(:)
  REAL(KIND=dp), POINTER :: VariableValues(:), Depth(:), GradSurface1(:), &
                            GradSurface2(:), Velocity(:), PrevVelo(:,:)
  REAL(KIND=dp) :: Norm, cn, dd 

  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), LOAD(:), FORCE(:), &
           NodalGravity(:), NodalDensity(:), &
           NodalDepth(:), NodalSurfGrad1(:), NodalSurfGrad2(:), &
           NodalU(:), NodalV(:)

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
       

  SAVE STIFF, LOAD, FORCE, AllocationsDone, DIM, SolverName
  SAVE NodalGravity, NodalDensity, &
           NodalDepth, NodalSurfGrad1, NodalSurfGrad2, &
           NodalU, NodalV
!------------------------------------------------------------------------------
  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values
  WRITE(SolverName, '(A)') 'SSASolver'

!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------
        DIM = CoordinateSystemDimension()

        VeloSol => VariableGet( Solver % Mesh % Variables, 'SSAFlow' )
        IF (ASSOCIATED(veloSol)) THEN
           Velocity => VeloSol % Values
           VeloPerm => VeloSol % Perm
           PrevVelo => veloSol % PrevValues
        ELSE
           CALL FATAL(SolverName,'Could not find variable >SSAFlow<')
        END IF
        DepthSol => VariableGet( Solver % Mesh % Variables, 'Depth' )
        IF (ASSOCIATED(DepthSol)) THEN
           Depth => DepthSol % Values
           DepthPerm => DepthSol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >Depth<')
        END IF
        Grad1Sol => VariableGet( Solver % Mesh % Variables, 'FreeSurfGrad1')
        IF (ASSOCIATED(Grad1Sol)) THEN
           GradSurface1 => Grad1Sol % Values
           GradSurface1Perm => Grad1Sol % Perm
        ELSE
           CALL FATAL(SolverName,'Could not find variable >FreeSurfGrad1<')
        END IF
        IF (dim > 2) THEN
           Grad2Sol => VariableGet( Solver % Mesh % Variables, 'FreeSurfGrad2')
           IF (ASSOCIATED(Grad2Sol)) THEN
              GradSurface2 => Grad2Sol % Values
              GradSurface2Perm => Grad2Sol % Perm
           ELSE
              CALL FATAL(SolverName,'Could not find variable >FreeSurfGrad2<')
           END IF
        END IF

  !--------------------------------------------------------------
  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( (.NOT. AllocationsDone) .OR. Solver % Mesh % Changed  ) THEN
     N = Solver % Mesh % MaxElementNodes ! just big enough for elemental arrays
     M = Model % Mesh % NumberOfNodes
     IF (AllocationsDone) DEALLOCATE(FORCE, LOAD, STIFF, NodalGravity, &
                       NodalDensity, NodalDepth, &
                       NodalSurfGrad1, NodalSurfGrad2, NodalU, NodalV )

     ALLOCATE( FORCE(N), LOAD(N), STIFF(N,N), &
          NodalGravity(N), NodalDensity(N), &
          NodalDepth(N), NodalSurfGrad1(N), NodalSurfGrad2(N), &
          NodalU(N), NodalV(N), STAT=istat )
     IF ( istat /= 0 ) THEN
        CALL Fatal( SolverName, 'Memory allocation error.' )
     END IF


     AllocationsDone = .TRUE.
     CALL INFO( SolverName, 'Memory allocation done.',Level=1 )
  END IF

     StiffMatrix => Solver % Matrix
     ForceVector => StiffMatrix % RHS

  ! Loop over the velocity components and pressure 
  ! If DIM = 2 u, w, p
  ! If DIM = 3 u, v, w, p
  !-----------------------------------------------
  DO  COMP = 1, DIM+1

! No non-linear iteration, no time dependency  
  VariableValues = 0.0d0
  Norm = Solver % Variable % Norm


  !Initialize the system and do the assembly:
  !------------------------------------------
  CALL DefaultInitialize()
  ! bulk assembly
  DO t=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(t)
     IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
     n = GetElementNOFNodes()

     NodeIndexes => Element % NodeIndexes

     ! Read the gravity in the Body Force Section 
     BodyForce => GetBodyForce()
     NodalGravity = 0.0_dp
     IF ( ASSOCIATED( BodyForce ) ) THEN
           IF (DIM==2) THEN 
           NodalGravity(1:n) = ListGetReal( &
                   BodyForce, 'Flow BodyForce 2', n, NodeIndexes, Found)
           ELSE 
           NodalGravity(1:n) = ListGetReal( &
                   BodyForce, 'Flow BodyForce 3', n, NodeIndexes, Found)
           END IF
     END IF
     
     ! Read the Viscosity eta, density, and exponent m in Material Section
     ! Same definition as NS Solver in Elmer - n=1/m , A = 1/ (2 eta^n) 
     Material => GetMaterial()

     NodalDensity = 0.0D0
     NodalDensity(1:n) = ListGetReal( &
         Material, 'Density', n, NodeIndexes, Found )

     ! Get the Nodal value of Depth, FreeSurfGrad1 and FreeSurfGrad2
     NodalDepth(1:n) = Depth(DepthPerm(NodeIndexes(1:n)))
     NodalSurfGrad1(1:n) = GradSurface1(GradSurface1Perm(NodeIndexes(1:n)))
     NodalSurfGrad2 = 0.0D0
     IF (DIM==3) NodalSurfGrad2(1:n) = GradSurface2(GradSurface2Perm(NodeIndexes(1:n)))

     IF (COMP==1) THEN     ! u
        CALL LocalMatrixUV (  STIFF, FORCE, Element, n ) 

     ELSE IF (COMP==DIM) THEN  ! w
        NodalU(1:n) = Velocity((DIM+1)*(VeloPerm(NodeIndexes(1:n))-1)+1)
        NodalV = 0.0D0
        IF (DIM==3) NodalV(1:n) = Velocity((DIM+1)*(VeloPerm(NodeIndexes(1:n))-1)+2)
        CALL LocalMatrixW (  STIFF, FORCE, Element, n, NodalU, NodalV ) 

     ELSE IF (COMP==DIM+1) THEN ! p
        CALL LocalMatrixP (  STIFF, FORCE, Element, n )

     ELSE               ! v if dim=3
        CALL LocalMatrixUV (  STIFF, FORCE, Element, n )

     END IF

     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  
  ! Neumann conditions only for w and p
  IF (COMP .GE. DIM) THEN
  DO t=1,Solver % Mesh % NUmberOfBoundaryElements
     Element => GetBoundaryElement(t)
     IF ( GetElementFamily() == 1 ) CYCLE
     NodeIndexes => Element % NodeIndexes
     IF (ParEnv % myPe .NE. Element % partIndex) CYCLE
     n = GetElementNOFNodes()
     STIFF = 0.0D00
     FORCE = 0.0D00

     IF (COMP==DIM) THEN
     ! only for the surface nodes
        dd = SUM(ABS(Depth(Depthperm(NodeIndexes(1:n)))))
        IF (dd < 1.0e-6) THEN
           NodalU(1:n) = Velocity((DIM+1)*(VeloPerm(NodeIndexes(1:n))-1)+1)
           NodalV = 0.0D0
           IF (DIM==3) NodalV(1:n) = Velocity((DIM+1)*(VeloPerm(NodeIndexes(1:n))-1)+2)
           CALL LocalMatrixBCW (  STIFF, FORCE, Element, n, NodalU, NodalV ) 
        END IF
     ELSE IF (COMP==DIM+1) THEN
            CALL LocalMatrixBCP(  STIFF, FORCE, Element, n, NodalDensity, &
                    NodalGravity )
     END IF
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  END IF

  CALL DefaultFinishAssembly()

  ! Dirichlet 
     CALL SetDirichletBoundaries( Model, StiffMatrix, ForceVector, &
          ComponentName('SSAFlow',COMP), 1,1, Permutation )
  
  !Solve the system
  Norm = DefaultSolve()

  ! Save the solution on the right variable
         DO i = 1, Model % Mesh % NumberOfNodes
           IF (VeloPerm(i)>0) THEN
           Velocity ((DIM+1)*(VeloPerm(i)-1) + COMP) = VariableValues(Permutation(i)) 
           END IF
         END DO 

  END DO ! Loop p

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixUV(  STIFF, FORCE, Element, n ) 
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ 
    LOGICAL :: Stat
    INTEGER :: t, p, q , dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()


    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       DO p=1,n
         DO q=1,n
           STIFF(p,q) = STIFF(p,q) + IP % S(t) * detJ * dBasisdx(q,dim)*dBasisdx(p,dim)
         END DO
       END DO

    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixUV
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixW(  STIFF, FORCE, Element, n, VeloU, VeloV)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), VeloU(:), VeloV(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ, &
                     dU2dxz, dV2dyz
    LOGICAL :: Stat
    INTEGER :: t, p,q , DIM
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    DIM = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .TRUE. )

       DO p=1,n
         DO q=1,n
           STIFF(p,q) = STIFF(p,q) + IP % S(t) * detJ * dBasisdx(q,dim)*dBasisdx(p,dim)
         END DO
       END DO

       dU2dxz = SUM(VeloU(1:n)*ddBasisddx(1:n,1,dim))
       dV2dyz = 0.0d0
       IF (DIM==3) dV2dyz = SUM(VeloV(1:n)*ddBasisddx(1:n,2,3))
       

       FORCE(1:n) = FORCE(1:n) + (dU2dxz + dV2dyz) * IP % s(t) * detJ * Basis(1:n) 

    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixW

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixP(  STIFF, FORCE, Element, n)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3), detJ
    LOGICAL :: Stat
    INTEGER :: t, p,q ,dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
        IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       DO p=1,n
         DO q=1,n
           STIFF(p,q) = STIFF(p,q) + IP % S(t) * detJ * dBasisdx(q,dim)*dBasisdx(p,dim)
         END DO
       END DO
    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBCW(  STIFF, FORCE, Element, n, VeloU, VeloV )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), veloU(:), veloV(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3), &
                      DetJ, Normal(3), grad, dUdx, dVdy  
    LOGICAL :: Stat
    INTEGER :: t, DIM
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    DIM = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
      stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
       IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       dUdx = SUM( VeloU(1:n) * dBasisdx(1:n,1) )
       dVdy = 0.0e0
       IF (DIM==3) dVdy = SUM( VeloV(1:n) * dBasisdx(1:n,2) )

       grad = - (dUdx + dVdy) 

      Normal = NormalVector( Element, Nodes, IP % U(t), IP % V(t), .TRUE.)
      FORCE(1:n) = FORCE(1:n) + grad * IP % s(t) * DetJ * Normal(dim) * Basis(1:n)
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBCW
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrixBCP(  STIFF, FORCE, Element, n, Density, & 
                      Gravity)
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), density(:), Gravity(:)
    INTEGER :: n
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(n,3,3), &
                      DetJ,Normal(3), rho, g, grad
    LOGICAL :: Stat
    INTEGER :: t, dim
    TYPE(GaussIntegrationPoints_t) :: IP

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    FORCE = 0.0d0

    dim = CoordinateSystemDimension()

    IP = GaussPoints( Element )
    DO t=1,IP % n
      stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
       IP % W(t),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )

       g = ABS(SUM( Gravity(1:n) * Basis(1:n) ))
       rho = SUM( Density(1:n) * Basis(1:n) )

       grad = - rho * g 

      Normal = NormalVector( Element, Nodes, IP % U(t), IP % V(t), .TRUE.)
      FORCE(1:n) = FORCE(1:n) + grad * IP % s(t) * DetJ * Normal(dim) * Basis(1:n)
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrixBCP
!------------------------------------------------------------------------------
END SUBROUTINE SSASolver
!------------------------------------------------------------------------------


