SUBROUTINE StokesSolver( Model,Solver,dt,TransientSimulation )
!DEC$ATTRIBUTES DLLEXPORT :: StokesSolver
!------------------------------------------------------------------------------
!******************************************************************************
!
! Q1Q0 NS-solver
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
  LOGICAL :: AllocationsDone = .FALSE., Newton = .FALSE., Found, Convect
  TYPE(Element_t),POINTER :: Element

  INTEGER :: i,j,k,n, nb, nd, t, istat, dim, BDOFs=1,Active, dofs
  REAL(KIND=dp) :: Norm = 0, PrevNorm, RelC

  TYPE(ValueList_t), POINTER :: BodyForce, Material
  TYPE(Mesh_t), POINTER :: Mesh
  REAL(KIND=dp), ALLOCATABLE, SAVE :: STIFF(:,:), LOAD(:,:), Pressure(:), &
    FORCE(:), rho(:), mu(:), Velocity(:,:), MASS(:,:), Velo(:)

  TYPE(Variable_t), POINTER :: P

  INTEGER, ALLOCATABLE, SAVE :: Indexes(:), pCount(:)

  SAVE STIFF, MASS, LOAD, FORCE, rho, mu, Velo, Velocity, AllocationsDone
!------------------------------------------------------------------------------

   dim = CoordinateSystemDimension()
   Mesh => GetMesh()
   DOFs = Solver % Variable % DOFs

  !Allocate some permanent storage, this is done first time only:
  !--------------------------------------------------------------
  IF ( .NOT. AllocationsDone ) THEN
     n = (dim+1)*(Mesh % MaxElementDOFs+BDOFs)  ! just big enough for elemental arrays
     ALLOCATE( FORCE(n), LOAD(n,4), STIFF(n,n), MASS(n,n), Pressure(n), &
         rho(n), mu(n), Velocity(dim+1,n), Velo(dim*n), Indexes(n),STAT=istat )

     IF ( istat /= 0 ) THEN
        CALL Fatal( 'StokesSolve', 'Memory allocation error.' )
     END IF
     AllocationsDone = .TRUE.
  END IF

  Convect = GetLogical( GetSolverParams(), 'Convect', Found )
  IF ( .NOT. Found ) Convect = .TRUE.

  Velocity = 0.0d0

  P => VariableGet( Mesh % Variables, 'aPressure' )
  IF ( .NOT. ASSOCIATED(P) .OR. DOFs>dim ) P=>NULL()

  IF ( ASSOCIATED(P) ) THEN
    ALLOCATE( pCount(SIZE(P % Values)) )
    pCount = 0
    P % Values = 0.0_dp
  END IF

  !Initialize the system and do the assembly:
  !------------------------------------------
  Active = GetNOFActive()
  CALL DefaultInitialize()
  DO t=1,Active
     Element => GetActiveElement(t)
     n  = GetElementNOFNodes()
     nb = GetElementNOFBDOFs()
     nd = GetElementDOFs(Indexes)

     ! Volume forces:
     !---------------
     BodyForce => GetBodyForce()
     LOAD = 0.0d0
     IF ( ASSOCIATED(BodyForce) ) THEN
        Load(1:n,1) = GetReal( BodyForce, 'Source x', Found )
        Load(1:n,2) = GetReal( BodyForce, 'Source y', Found )
        Load(1:n,3) = GetReal( BodyForce, 'Source z', Found )
     END IF

     ! Material parameters:
     !---------------------
     Material => GetMaterial()
     rho(1:n) = GetReal( Material, 'Density' )
     mu(1:n)  = GetReal( Material, 'Viscosity' )

     ! Get previous elementwise velocity iterate:
     !-------------------------------------------
     IF ( Convect ) CALL GetVectorLocalSolution( Velocity )

     ! Get element local matrix and rhs vector:
     !-----------------------------------------
     CALL LocalMatrix(  MASS, STIFF, FORCE, LOAD, rho, mu, &
         Velocity, Element, n, nd, nd+nb, dim )

     IF ( TransientSimulation ) &
       CALL Default1stOrderTime( MASS, STIFF, FORCE )

     IF (nb>0) CALL LCondensate( nd,nb,dim,STIFF,FORCE )

     IF ( ASSOCIATED(P) ) THEN
       DO i=1,dim
         Velo(i:nd*dim:dim) = Velocity(i,1:nd)
       END DO
       CALL LCondensateP( nd, nd, dim, STIFF, FORCE, &
                  Pressure, Velo )

       P % Values(P % Perm(Indexes(1:nd))) = &
         P % Values(P % Perm(Indexes(1:nd))) + Pressure(1:nd) 

       pCount(P % Perm(Indexes(1:nd))) = &
          pCount(P % Perm(Indexes(1:nd))) + 1
     ELSE
       CALL LCondensateP( nd, nd, dim, STIFF, FORCE )
     END IF

     ! Update global matrix and rhs vector from local matrix & vector:
     !----------------------------------------------------------------
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO

  IF ( ASSOCIATED(P) ) THEN
    WHERE( pCount>0 ) 
      P % Values = P % Values / PCount
    END WHERE
    DEALLOCATE(pCount)
  END IF
  
  CALL DefaultFinishAssembly()
  CALL DefaultDirichletBCs()

  PrevNorm = Norm
  Norm = DefaultSolve()

  RELC = 2.0d0 * ABS(PrevNorm - Norm) / (PrevNorm + Norm)
  IF ( RELC < 1.0d-2 ) Newton = .TRUE.

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE LocalMatrix(  MASS, STIFF, FORCE, LOAD, Nodalrho, &
     Nodalmu, NodalVelo, Element, n, nd, ntot, dim )
!------------------------------------------------------------------------------
    REAL(KIND=dp), TARGET :: MASS(:,:), STIFF(:,:), FORCE(:), LOAD(:,:)
    REAL(KIND=dp) :: Nodalmu(:), Nodalrho(:), NodalVelo(:,:)
    INTEGER :: dim, n, nd, ntot
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: Basis(ntot),dBasisdx(ntot,3), &
               DetJ,LoadAtIP(dim+1),Velo(dim), Grad(dim,dim)
    REAL(KIND=dp), POINTER :: A(:,:), F(:),M(:,:)
    LOGICAL :: Stat
    INTEGER :: t, i, j, k, l, p, q
    TYPE(GaussIntegrationPoints_t) :: IP

    REAL(KIND=dp) :: mu = 1.0d0, rho = 1.0d0, s, c

    TYPE(Nodes_t) :: Nodes
    SAVE Nodes
!------------------------------------------------------------------------------
    CALL GetElementNodes( Nodes )
    STIFF = 0.0d0
    MASS  = 0.0d0
    FORCE = 0.0d0

    ! Numerical integration:
    !-----------------------
    IP = GaussPoints( Element )
    DO t=1,IP % n
       ! Basis function values & derivatives at the integration point:
       !--------------------------------------------------------------
       stat = ElementInfo( Element, Nodes, IP % U(t), IP % V(t), &
         IP % W(t),  detJ, Basis, dBasisdx )

       s = IP % s(t) * detJ

       ! Material parameters at the integration point:
       !----------------------------------------------
       mu  = SUM( Basis(1:n) * Nodalmu(1:n) )
       rho = SUM( Basis(1:n) * Nodalrho(1:n) )

       ! Previous velocity at the integration point:
       !--------------------------------------------
       Velo = MATMUL( NodalVelo(1:dim,1:nd), Basis(1:nd) )
       Grad = MATMUL( NodalVelo(1:dim,1:nd), dBasisdx(1:nd,1:dim) )

       ! The source term at the integration point:
       !------------------------------------------
       LoadAtIP(1:dim+1) = MATMUL( Basis(1:n), LOAD(1:n,1:dim+1) )
       IF ( Convect .AND. Newton ) THEN
         LoadAtIp(1:dim) = LoadAtIp(1:dim) + rho * MATMUL(Grad,Velo)
       END IF

       ! Finally, the elemental matrix & vector:
       !----------------------------------------
       DO p=1,ntot
         DO q=1,ntot
           i = (dim+1) * (p-1) + 1
           j = (dim+1) * (q-1) + 1
           A => STIFF(i:i+dim,j:j+dim)
           M => MASS(i:i+dim,j:j+dim)
           DO i=1,dim
             M(i,i) = M(i,i) + s * rho * Basis(q) * Basis(p)
             DO j = 1,dim
               A(i,i) = A(i,i) + s * mu * dBasisdx(q,j) * dBasisdx(p,j)
               A(i,j) = A(i,j) + s * mu * dBasisdx(q,i) * dBasisdx(p,j)
               IF ( Convect ) THEN
                 A(i,i) = A(i,i) + s * rho * Velo(j) * dBasisdx(q,j) * Basis(p)
                 IF ( Newton ) THEN
                    A(i,j) = A(i,j) + s * rho * Grad(i,j) * Basis(q) * Basis(p)
                 END IF
               END IF
             END DO
             A(i,dim+1) = A(i,dim+1) - s * dBasisdx(p,i)/ntot
             A(dim+1,i) = A(dim+1,i) + s * rho * dBasisdx(q,i)/ntot
           END DO
           IF ( p==q ) A(dim+1,dim+1) = A(dim+1,dim+1) + s * 1.d-7
         END DO
         i = (dim+1) * (p-1) + 1
         F => FORCE(i:i+dim)
         F = F + s * LoadAtIP * Basis(p) 
       END DO
    END DO

   ! for p2/p1 elements set Dirichlet constraint for unused dofs,
   ! EliminateDirichlet will get rid of these:
   !-------------------------------------------------------------
    DO p = n+1,ntot
      i = (dim+1) * p
      FORCE(i)   = 0.0d0
      MASS(:,i)  = 0.0d0
      MASS(i,:)  = 0.0d0
      STIFF(i,:) = 0.0d0
      STIFF(:,i) = 0.0d0
      STIFF(i,i) = 1.0d0
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
    SUBROUTINE LCondensate( N, nb, dim, K, F )
!------------------------------------------------------------------------------
      USE LinearAlgebra
      INTEGER :: N, nb, dim
      REAL(KIND=dp) :: K(:,:),F(:), Kbb(Nb*dim,Nb*dim), &
       Kbl(nb*dim,n*(dim+1)),Klb(n*(dim+1),nb*dim),Fb(nb*dim)

      INTEGER :: m, i, j, l, p, Cdofs((dim+1)*n), Bdofs(dim*nb)

      m = 0
      DO p = 1,n
        DO i = 1,dim+1
          m = m + 1
          Cdofs(m) = (dim+1)*(p-1) + i
        END DO
      END DO
      
      m = 0
      DO p = 1,nb
        DO i = 1,dim
          m = m + 1
          Bdofs(m) = (dim+1)*(p-1) + i + n*(dim+1)
        END DO
      END DO

      Kbb = K(Bdofs,Bdofs)
      Kbl = K(Bdofs,Cdofs)
      Klb = K(Cdofs,Bdofs)
      Fb  = F(Bdofs)

      CALL InvertMatrix( Kbb,Nb*dim )

      F(1:(dim+1)*n) = F(1:(dim+1)*n) - MATMUL( Klb, MATMUL( Kbb, Fb ) )
      K(1:(dim+1)*n,1:(dim+1)*n) = &
           K(1:(dim+1)*n,1:(dim+1)*n) - MATMUL( Klb, MATMUL( Kbb,Kbl ) )
!------------------------------------------------------------------------------
    END SUBROUTINE LCondensate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE LCondensateP( N, Nb, dim, K, F, Pres, Velo )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  DESCRIPTION:
!     Subroutine for condensation of p element bubbles from linear problem.
!     Modifies given stiffness matrix and force vector(s) 
!
!  ARGUMENTS:
!    INTEGER :: N
!      INPUT: Sum of nodal, edge and face degrees of freedom 
!
!    INTEGER :: Nb
!      INPUT: Sum of internal (bubble) degrees of freedom
!
!    REAL(Kind=dp) :: K(:,:)
!      INOUT: Local stiffness matrix
!
!    REAL(Kind=dp) :: F(:)
!      INOUT: Local force vector
!
!******************************************************************************
!------------------------------------------------------------------------------

    USE LinearAlgebra
    INTEGER :: N, Nb, dim
    REAL(KIND=dp) :: K(:,:),F(:),Kbb(Nb,Nb), &
         Kbl(Nb,N*dim), Klb(N*dim,Nb), Fb(Nb)
    REAL(KIND=dp), OPTIONAL :: Pres(:), Velo(:)

    INTEGER :: m, i, j, l, p, Ldofs(N*dim), Bdofs(Nb)

    m = 0
    DO p = 1,n
     DO i = 1,dim
        m = m + 1
        Ldofs(m) = (dim+1)*(p-1) + i
      END DO
    END DO
      
    m = 0
    DO p = 1,nb
      m = m + 1
      Bdofs(m) = (dim+1)*(p-1) + dim+1
    END DO

    Kbb = K(Bdofs,Bdofs)
    Kbl = K(Bdofs,Ldofs)
    Klb = K(Ldofs,Bdofs)
    Fb  = F(Bdofs)
    CALL InvertMatrix( Kbb,nb )

    IF ( PRESENT(Pres) ) THEN
      Pres(1:nb) = -MATMUL(Kbb,MATMUL(Kbl,Velo(1:n*dim)))
    END IF

    F(1:n*dim) = F(1:n*dim) - MATMUL( Klb, MATMUL( Kbb, Fb  ) )
    K(1:n*dim,1:n*dim) = &
         K(Ldofs,Ldofs) - MATMUL( Klb, MATMUL( Kbb, Kbl ) )
!------------------------------------------------------------------------------
  END SUBROUTINE LCondensateP
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
  SUBROUTINE SolvePressure( N, Nb, dim, K, U, Pres )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  DESCRIPTION:
!     Subroutine for condensation of p element bubbles from linear problem.
!     Modifies given stiffness matrix and force vector(s) 
!
!  ARGUMENTS:
!    INTEGER :: N
!      INPUT: Sum of nodal, edge and face degrees of freedom 
!
!    INTEGER :: Nb
!      INPUT: Sum of internal (bubble) degrees of freedom
!
!    REAL(Kind=dp) :: K(:,:)
!      INOUT: Local stiffness matrix
!
!    REAL(Kind=dp) :: F(:)
!      INOUT: Local force vector
!
!    REAL(Kind=dp), OPTIONAL :: F1(:)
!      INOUT: Local second force vector 
!    
!******************************************************************************
!------------------------------------------------------------------------------

    USE LinearAlgebra
    INTEGER :: N, Nb, dim
    REAL(KIND=dp) :: K(:,:), U(:),Pres(:), Kbb(Nb,Nb), &
         Kbl(Nb,N*dim), Klb(N*dim,Nb), Fb(Nb)

    INTEGER :: m, i, j, l, p, Ldofs(N*dim), Bdofs(Nb)

    m = 0
    DO p = 1,n
     DO i = 1,dim
        m = m + 1
        Ldofs(m) = (dim+1)*(p-1) + i
      END DO
    END DO
      
    m = 0
    DO p = 1,nb
      m = m + 1
      Bdofs(m) = (dim+1)*(p-1) + dim+1
    END DO

    Kbb = K(Bdofs,Bdofs)
    Kbl = K(Bdofs,Ldofs)
    CALL InvertMatrix( Kbb,nb )
!------------------------------------------------------------------------------
  END SUBROUTINE SolvePressure
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
END SUBROUTINE StokesSolver
!------------------------------------------------------------------------------
