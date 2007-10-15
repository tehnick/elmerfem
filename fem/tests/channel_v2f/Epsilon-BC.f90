FUNCTION Epsilon( Model,n,Eval ) RESULT(E)
  USE DefUtils

  IMPLICIT NONE

  TYPE(Model_t) :: Model
  INTEGER :: n
  REAL(KIND=dp) :: Eval,E

  LOGICAL :: stat

  INTEGER :: i, ne, np
  REAL(KIND=dp) :: rho(32), mu(32), KVals(32), detJ,U,V,W
  REAL(KIND=dp) :: Kder,Normal(3),Basis(32),dBasisdx(32,3)

  TYPE(ValueList_t), POINTER :: Material
  TYPE(Nodes_t) :: Nodes, ParentNodes
  TYPE(Element_t), POINTER   :: Element, Parent

  SAVE Nodes, ParentNodes

  Element => Model % CurrentElement
  Parent  => Element % BoundaryInfo % Left
  IF ( .NOT. ASSOCIATED(Parent) ) Parent => Element % BoundaryInfo % Right

  ne = Element % Type % NumberOfNodes
  np = Parent  % Type % NumberOfNodes

  DO i=1,ne
    IF ( Element % NodeIndexes(i) == n ) EXIT
  END DO

  U = Element % Type % NodeU(i)
  V = Element % Type % NodeV(i)
  W = Element % Type % NodeW(i)

  CALL GetElementNodes( Nodes, Element )
  stat = ElementInfo( Element,Nodes,U,V,W,detJ, Basis )

  CALL GetParentUVW( Element,ne,Parent,np,U,V,W,Basis )

  CALL GetElementNodes( ParentNodes, Parent )
  stat = ElementInfo( Parent,ParentNodes,U,V,W,detJ, &
            Basis, dBasisdx )

  Material => GetMaterial(Parent)
  rho(1:np) = GetReal( Material, 'Density',   UElement=Parent )
  mu(1:np)  = GetReal( Material, 'Viscosity', UElement=Parent )

  Normal = NormalVector( Element,Nodes,  0.0d0, 0.0d0, .FALSE. )
  CALL GetScalarLocalSolution( Kvals, 'Kinetic Energy', Parent )

  KVals(1:np) = SQRT(KVals(1:np))
  Kder = 0.0d0
  DO i=1,3
    Kder = Kder + SUM(dBasisdx(1:np,i)*Kvals(1:np))*Normal(i)
  END DO
  E = 0.9_dp * Eval + 0.1_dp * 2*(mu(1)/rho(1))*Kder**2
END FUNCTION Epsilon
