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
! ******************************************************************************
! *
! *                    Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2302
! *                              EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 08 Jun 1997
! *
! *                Modified by: Mikko, Juha
! *
! *       Date of modification: 08 Apr 2004
! *
! *****************************************************************************/
 
!------------------------------------------------------------------------------
  RECURSIVE SUBROUTINE AdvReactSolver( Model,Solver,dt,Transient )
!DEC$ATTRIBUTES DLLEXPORT :: AdvReactSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
! Solves the advection-reaction equation with discontinous Galerkin method!
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
!  LOGICAL :: Transient
!     INPUT: Steady state or transient simulation
!
!******************************************************************************
     USE DefUtils

     IMPLICIT NONE
!------------------------------------------------------------------------------
     TYPE(Model_t) :: Model
     TYPE(Solver_t), TARGET :: Solver
     REAL(KIND=dp) :: dt
     LOGICAL :: Transient
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
     TYPE(ValueList_t), POINTER :: BC, BodyForce, Material

     TYPE(Element_t), POINTER :: Element, Face, &
       ParentElement, LeftParent, RightParent

     LOGICAL :: AllocationsDone = .FALSE., Found, Stat
     INTEGER :: n1,n2, k, n, t, istat, i, j, NumberOfFAces, Indexes(128)
     REAL(KIND=dp) :: Norm, at, st, CPUTime
     REAL(KIND=dp), ALLOCATABLE :: MASS(:,:), STIFF(:,:), LOAD(:), &
              FORCE(:), Velo(:,:), Gamma(:), Ref(:)

     TYPE(Variable_t), POINTER ::  Var
     SAVE MASS, STIFF, LOAD, FORCE, Velo, Gamma, AllocationsDone
!*******************************************************************************

     TYPE( Element_t ), POINTER :: Faces(:)
     TYPE(Nodes_t) :: ElementNodes

     INTEGER :: DOFs
!*******************************************************************************
     IF ( CoordinateSystemDimension() == 2 ) THEN
        Faces => Solver % Mesh % Edges
        NumberOfFaces = Solver % Mesh % NumberOfEdges
     ELSE
        Faces => Solver % Mesh % Faces
        NumberOfFaces = Solver % Mesh % NumberOfFaces
     END IF

     ! Initialize & allocate some permanent storage, this is done first time only:
     !----------------------------------------------------------------------------
     IF ( .NOT. AllocationsDone ) THEN
        N = 2 * MAX(Solver % Mesh % MaxElementDOFs, Solver % Mesh % MaxElementNodes )
        ALLOCATE( FORCE(N), MASS(n,n), STIFF(N,N), LOAD(N),  &
                  Velo(3,N), Gamma(n), STAT = istat )
        
       IF ( istat /= 0 ) CALL FATAL('AdvReactDG','Memory allocation error.' )
       AllocationsDone = .TRUE.
     END IF

     ! Assembly of the bulk elements:
     !-------------------------------
     at = CPUTime()
     CALL DefaultInitialize()
     DO t = 1, Solver % NumberOfActiveElements
        Element => GetActiveElement( t ) 
        n = GetElementNOfNodes( Element )
        
        BodyForce => GetBodyForce( Element )
        LOAD(1:n) = GetReal( BodyForce, 'Source', Found )
        
        Material => GetMaterial()
        Velo(1,1:n) = GetReal( Material, 'u 1', Found )
        Velo(2,1:n) = GetReal( Material, 'u 2', Found )
        Velo(3,1:n) = GetReal( Material, 'u 3', Found )
        Gamma(1:n)  = GetReal( Material, 'Gamma', Found )

        CALL LocalMatrix( MASS, STIFF, FORCE, LOAD, Velo, Gamma, Element, n ) 
        IF ( Transient ) CALL Default1stOrderTime( MASS, STIFF, FORCE )
        CALL DefaultUpdateEquations( STIFF, FORCE )
     END DO

     ! Assembly of the face terms:
     !----------------------------
     FORCE = 0.0d0
     DO t=1,NumberOfFaces
        Face => Faces(t)
        IF ( .NOT. ActiveBoundaryElement(Face) ) CYCLE
      
        LeftParent  => Face % BoundaryInfo % Left
        RightParent => Face % BoundaryInfo % Right
        IF ( ASSOCIATED(RightParent) .AND. ASSOCIATED(LeftParent) ) THEN
          n  = GetElementNOFNodes( Face )
          n1 = GetElementNOFNodes( LeftParent )
          n2 = GetElementNOFNodes( RightParent )

          Material => GetMaterial( LeftParent )
          Velo(1,1:n) = GetReal( Material, 'u 1', Found, Face )
          Velo(2,1:n) = GetReal( Material, 'u 2', Found, Face )
          Velo(3,1:n) = GetReal( Material, 'u 3', Found, Face )

          CALL LocalJumps( STIFF,Face,n,LeftParent,n1,RightParent,n2,Velo )
          CALL DefaultUpdateEquations( STIFF, FORCE, Face )
        END IF
     END DO

     ! Loop over the boundary elements:
     !---------------------------------
     DO t=1,Solver % Mesh % NumberOfBoundaryElements
       Element => GetBoundaryElement(t)
       IF( .NOT. ActiveBoundaryElement() )  CYCLE
       IF( GetElementFamily(Element) == 1 ) CYCLE

       ParentElement => Element % BoundaryInfo % Left
       IF ( .NOT. ASSOCIATED( ParentElement ) ) &
         ParentElement => Element % BoundaryInfo % Right
        
       n  = GetElementNOFNodes( Element )
       n1 = GetElementNOFnodes( ParentElement )
      
       Material => GetMaterial( ParentElement )
       Velo(1,1:n) = GetReal( Material, 'u 1', Found )
       Velo(2,1:n) = GetReal( Material, 'u 2', Found )
       Velo(3,1:n) = GetReal( Material, 'u 3', Found )

       BC => GetBC()
       LOAD = 0.0d0
       Found = .FALSE.
       IF ( ASSOCIATED(BC) ) THEN
         LOAD(1:n) = GetReal( BC, Solver % Variable % Name, Found )
       END IF

       MASS = 0.0d0
       CALL LocalMatrixBoundary(  STIFF, FORCE, LOAD, &
         Element, n, ParentElement, n1, Velo, Found )

       IF ( Transient ) CALL Default1stOrderTime( MASS, STIFF, FORCE )
       CALL DefaultUpdateEquations( STIFF, FORCE )
     END DO

     CALL DefaultFinishAssembly()

     ! Solve the system:
     !------------------
     Norm = DefaultSolve()

     ! Average the elemental results to nodal values:
     !-----------------------------------------------
     Var => VariableGet( Solver % Mesh % Variables, 'Nodal Result' )
     IF ( ASSOCIATED( Var ) ) THEN
       n1 = Solver % Mesh % NumberOfNodes
       ALLOCATE( Ref(n1) )
       Ref = 0

       IF ( ASSOCIATED( Var % Perm, Solver % Variable % Perm ) ) THEN
         ALLOCATE( Var % Perm(SIZE(Solver % Variable % Perm))  )
         Var % Perm = 0
         DO i = 1,n1
            Var % Perm(i) = i
         END DO
       END IF

       Var % Values = 0.0d0
       DO t=1,Solver % NumberOfActiveElements
          Element => GetActiveElement(t) 
          n = GetElementDOFs( Indexes )
          n = GetElementNOFNodes()
          DO i=1,n
             k = Element % NodeIndexes(i)
             Var % Values(k) = Var % Values(k) + &
               Solver % Variable % Values( Solver % Variable % Perm(Indexes(i)) )
             Ref(k) = Ref(k) + 1
          END DO
       END DO

       WHERE( Ref > 0 )
         Var % Values(1:n1) = Var % Values(1:n1) / Ref
       END WHERE
       DEALLOCATE( Ref )
     END IF

   CONTAINS

!------------------------------------------------------------------------------      
     SUBROUTINE LocalMatrix(MASS, STIFF, FORCE, LOAD, Velo, Gamma, Element, n)
!------------------------------------------------------------------------------
       REAL(KIND=dp) :: MASS(:,:), STIFF(:,:), FORCE(:), &
                  LOAD(:), Velo(:,:), Gamma(:)
       INTEGER :: n
       TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
       REAL(KIND=dp) :: Basis(n),dBasisdx(n,3)
       REAL(KIND=dp) :: detJ,U,V,W,S,A,L,cu(3),g
       LOGICAL :: Stat
       INTEGER :: i,p,q,t,dim
       TYPE(GaussIntegrationPoints_t) :: IntegStuff
       TYPE(Nodes_t) :: Nodes
       SAVE Nodes
!------------------------------------------------------------------------------
       dim = CoordinateSystemDimension()
       FORCE = 0.0d0
       STIFF = 0.0d0
       MASS  = 0.0d0
       CALL GetElementNodes( Nodes, Element )
!------------------------------------------------------------------------------
!      Numerical integration
!------------------------------------------------------------------------------
       IntegStuff = GaussPoints( Element )
 
       DO t=1,IntegStuff % n
         U = IntegStuff % u(t)
         V = IntegStuff % v(t)
         W = IntegStuff % w(t)
         S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!        Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
         stat = ElementInfo( Element, Nodes, U, V, W, detJ, &
                 Basis, dBasisdx )

         S = S * detJ
         L = SUM( LOAD(1:n) *  Basis(1:n) )
         g = SUM( Basis(1:n) * Gamma(1:n) )

         cu = 0.0d0
         DO i=1,dim
            cu(i) = SUM( Basis(1:n) * Velo(i,1:n) )
         END DO
!------------------------------------------------------------------------------
!        The advection-reaction equation
!------------------------------------------------------------------------------
         DO p=1,n
            DO q=1,n
              MASS(p,q)  = MASS(p,q)  + s * Basis(q) * Basis(p)
              STIFF(p,q) = STIFF(p,q) + s * g * Basis(q) * Basis(p)
              DO i=1,dim
                STIFF(p,q) = STIFF(p,q) - s * cu(i) * Basis(q) * dBasisdx(p,i)
              END DO
            END DO
         END DO
         FORCE(1:n) = FORCE(1:n) + s*L*Basis(1:n)
!------------------------------------------------------------------------------
      END DO
!------------------------------------------------------------------------------
     END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    SUBROUTINE FindParentUVW(Face, nFace, Parent, nParent, U, V, W, Basis)
!------------------------------------------------------------------------------
      IMPLICIT NONE
      TYPE(Element_t) :: Face, Parent
      INTEGER :: nFace, nParent
      REAL( KIND=dp ) :: U, V, W, Basis(:)
!------------------------------------------------------------------------------
      INTEGER :: i,j
      REAL(KIND=dp) :: ParentU(nFace), ParentV(nFace), ParentW(nFace)
!------------------------------------------------------------------------------
      DO i = 1,nFace
        DO j = 1,nParent
          IF ( Face % NodeIndexes(i) == Parent % NodeIndexes(j) ) THEN
            ParentU(i) = Parent % Type % NodeU(j)
            ParentV(i) = Parent % Type % NodeV(j)
            ParentW(i) = Parent % Type % NodeW(j)
            EXIT
          END IF
        END DO
      END DO
      U = SUM( Basis(1:nFace) * ParentU(1:nFace) )
      V = SUM( Basis(1:nFace) * ParentV(1:nFace) )
      W = SUM( Basis(1:nFace) * ParentW(1:nFace) )
!------------------------------------------------------------------------------      
    END SUBROUTINE FindParentUVW
!------------------------------------------------------------------------------      


!------------------------------------------------------------------------------
    SUBROUTINE LocalJumps( STIFF,Face,n,LeftParent,n1,RightParent,n2,Velo )
!------------------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=dp) :: STIFF(:,:), Velo(:,:)
      INTEGER :: n,n1,n2
      TYPE(Element_t), POINTER :: Face, LeftParent, RightParent
!------------------------------------------------------------------------------
      REAL(KIND=dp) :: FaceBasis(n), FacedBasisdx(n,3)
      REAL(KIND=dp) :: LeftBasis(n1), LeftdBasisdx(n1,3)
      REAL(KIND=dp) :: RightBasis(n2), RightdBasisdx(n2,3)
      REAL(KIND=dp) :: Jump(n1+n2), Average(n1+n2)
      REAL(KIND=dp) :: detJ, U, V, W, S, Udotn, xx, yy
      LOGICAL :: Stat
      INTEGER :: i, j, p, q, dim, t, nFace, nParent
      TYPE(GaussIntegrationPoints_t) :: IntegStuff
      REAL(KIND=dp) :: hE, Normal(3), cu(3), LeftOut(3)

      TYPE(Nodes_t) :: FaceNodes, LeftParentNodes, RightParentNodes
      SAVE FaceNodes, LeftParentNodes, RightParentNodes
!------------------------------------------------------------------------------
      dim = CoordinateSystemDimension()
      STIFF = 0.0d0

      CALL GetElementNodes( FaceNodes, Face )
      CALL GetElementNodes( LeftParentNodes,  LeftParent )
      CALL GetElementNodes( RightParentNodes, RightParent )
!------------------------------------------------------------------------------
!     Numerical integration over the edge
!------------------------------------------------------------------------------
      IntegStuff = GaussPoints( Face )

      LeftOut(1) = SUM(LeftParentNodes % x(1:n1)) / n1
      LeftOut(2) = SUM(LeftParentNodes % y(1:n1)) / n1
      LeftOut(3) = SUM(LeftParentNodes % z(1:n1)) / n1
      LeftOut(1) = SUM(FaceNodes % x(1:n)) / n - LeftOut(1)
      LeftOut(2) = SUM(FaceNodes % y(1:n)) / n - LeftOut(2)
      LeftOut(3) = SUM(FaceNodes % z(1:n)) / n - LeftOut(3)

      DO t=1,IntegStuff % n
        U = IntegStuff % u(t)
        V = IntegStuff % v(t)
        W = IntegStuff % w(t)
        S = IntegStuff % s(t)

        ! Basis function values & derivatives at the integration point:
        !--------------------------------------------------------------
        stat = ElementInfo( Face, FaceNodes, U, V, W, detJ, &
             FaceBasis, FacedBasisdx )

        S = S * detJ

        Normal = NormalVector( Face, FaceNodes, U, V, .FALSE. )
        IF ( SUM( LeftOut*Normal ) < 0 ) Normal = -Normal

        ! Find basis functions for the parent elements:
        ! ---------------------------------------------
        CALL FindParentUVW( Face,n,LeftParent,n1,U,V,W,FaceBasis )
        stat = ElementInfo( LeftParent, LeftParentNodes, U, V, W, detJ, &
                LeftBasis, LeftdBasisdx )

        CALL FindParentUVW( Face,n,RightParent,n2,U,V,W,FaceBasis )
        stat = ElementInfo( RightParent, RightParentNodes, U, V, W, detJ, &
              RightBasis, RightdBasisdx )

        ! Integrate jump terms:
        ! ---------------------
        Jump(1:n1) = LeftBasis(1:n1)
        Jump(n1+1:n1+n2) = -RightBasis(1:n2)

        Average(1:n1) = LeftBasis(1:n1) / 2
        Average(n1+1:n1+n2) = RightBasis(1:n2) / 2

        cu = 0.0d0
        DO i=1,dim
          cu(i) = SUM( Velo(i,1:n) * FaceBasis(1:n) )
        END DO
        Udotn = SUM( Normal * cu )

        DO p=1,n1+n2
          DO q=1,n1+n2
            STIFF(p,q) = STIFF(p,q) + s * Udotn * Average(q) * Jump(p)
            STIFF(p,q) = STIFF(p,q) + s * ABS(Udotn)/2 * Jump(q) * Jump(p)
          END DO
        END DO
      END DO
!------------------------------------------------------------------------------
    END SUBROUTINE LocalJumps
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
    SUBROUTINE LocalMatrixBoundary( STIFF, FORCE, LOAD, &
        Element, n, ParentElement, np, Velo, InFlowBC )
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: STIFF(:,:),  FORCE(:), LOAD(:), Velo(:,:)
     INTEGER :: n, np
     LOGICAL :: InFlowBC
     TYPE(Element_t), POINTER :: Element, ParentElement
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(n), dBasisdx(n,3)
     REAL(KIND=dp) :: ParentBasis(np), ParentdBasisdx(np,3)
     INTEGER :: i,j,p,q,t,dim

     REAL(KIND=dp) :: Normal(3), g, L, Udotn, cu(3), cu1(3), detJ,U,V,W,S
     LOGICAL :: Stat, Inflow
     TYPE(GaussIntegrationPoints_t) :: IntegStuff

     TYPE(Nodes_t) :: Nodes, ParentNodes
     SAVE Nodes, ParentNodes
!------------------------------------------------------------------------------
     dim = CoordinateSystemDimension()
     FORCE = 0.0d0
     STIFF = 0.0d0
 
     CALL GetElementNodes( Nodes, Element )
     CALL GetElementNodes( ParentNodes, ParentElement )

     Normal = NormalVector( Element, Nodes, 0.0d0, 0.0d0, .TRUE. ) 
     DO i=1,3
       cu(i) = SUM( Velo(i,1:n) ) / n
     END DO
     Inflow = InFlowBC .AND. SUM( Normal * cu ) < 0.0d0

     ! Numerical integration:
     !-----------------------
     IntegStuff = GaussPoints( Element )

     DO t=1,IntegStuff % n
       U = IntegStuff % u(t)
       V = IntegStuff % v(t)
       W = IntegStuff % w(t)
       S = IntegStuff % s(t)

       Normal = NormalVector( Element, Nodes, U, V, .TRUE. ) 

       ! Basis function values & derivatives at the integration point:
       ! -------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, U, V, W, detJ, &
               Basis, dBasisdx )
       S = S * detJ

       CALL FindParentUVW( Element, n, ParentElement, np, U, V, W, Basis )
       stat = ElementInfo( ParentElement, ParentNodes, U, V, W, &
            detJ, ParentBasis, ParentdBasisdx )

       L = SUM( LOAD(1:n) * Basis(1:n) )
       cu = 0.0d0
       DO i=1,dim
          cu(i)  = SUM( Velo(i,1:n) * Basis(1:n) )
       END DO
       Udotn = SUM( Normal * cu )

       DO p = 1,np
         IF ( Inflow ) THEN
            FORCE(p) = FORCE(p) - s * Udotn*L*ParentBasis(p)
         ELSE
           DO q=1,np
             STIFF(p,q) = STIFF(p,q) + s*Udotn*ParentBasis(q)*ParentBasis(p)
           END DO
         END IF
       END DO
     END DO
!------------------------------------------------------------------------------
   END SUBROUTINE LocalMatrixBoundary
!------------------------------------------------------------------------------
 END SUBROUTINE AdvReactSolver
!------------------------------------------------------------------------------
