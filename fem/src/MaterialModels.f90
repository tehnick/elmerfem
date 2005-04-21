! ******************************************************************************
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
! ******************************************************************************/
!
!/*******************************************************************************
! *
! *     Material Models
! *
! *******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2302
! *                              EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 24 Apr 1997
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! ******************************************************************************/

MODULE MaterialModels
 
   USE CoordinateSystems
   USE Integration
   USE LinearAlgebra
   USE ElementDescription

   IMPLICIT NONE
   INTEGER, PARAMETER :: Incompressible = 0, UserDefined1 = 1,UserDefined2 = 2
   INTEGER, PARAMETER :: PerfectGas1 = 3, PerfectGas2 = 4, PerfectGas3 = 5, Thermal = 6


CONTAINS


 
!------------------------------------------------------------------------------
!
!  Actually SQUARE of the second invariant of velocity is returned
!
   FUNCTION SecondInvariant( Velo,dVelodx,CtrMetric,Symb ) RESULT(SecInv)
DLLEXPORT SecondInvariant
!------------------------------------------------------------------------------

     REAL(KIND=dp), OPTIONAL :: CtrMetric(3,3),Symb(3,3,3)
     REAL(KIND=dp) :: Velo(3),dVelodx(3,3),SecInv

!------------------------------------------------------------------------------

     INTEGER :: i,j,k,l
     REAL(KIND=dp) :: CovMetric(3,3),s,t

     SecInv = 0.0D0

     IF ( CurrentCoordinateSystem() == Cartesian ) THEN
!------------------------------------------------------------------------------

       DO i=1,3
         DO j=1,3
           s = dVelodx(i,j) + dVelodx(j,i)
           SecInv = SecInv + s * s
         END DO
       END DO
!------------------------------------------------------------------------------
     ELSE IF ( CurrentCoordinateSystem() == AxisSymmetric ) THEN

        SecInv = (2*dVelodx(1,1))**2 + (2*dVelodx(2,2))**2 + &
          2*(dVelodx(1,2) + dVelodx(2,1))**2 + (2*Velo(1)*symb(1,3,3))**2

     ELSE

!------------------------------------------------------------------------------
       CovMetric = CtrMetric
       CALL InvertMatrix( CovMetric,3 )

       DO i=1,3
         DO j=1,3
            s = 0.0d0
            t = 0.0d0

            DO k=1,3
               s = s + CovMetric(i,k) * dVelodx(k,j) + &
                       CovMetric(j,k) * dVelodx(k,i)

              t = t + CtrMetric(j,k) * dVelodx(i,k) + &
                      CtrMetric(i,k) * dVelodx(j,k)

              DO l=1,3
                s = s - CovMetric(i,k) * Symb(l,j,k) * Velo(l)
                s = s - CovMetric(j,k) * Symb(l,i,k) * Velo(l)

                t = t - CtrMetric(j,k) * Symb(l,k,i) * Velo(l)
                t = t - CtrMetric(i,k) * Symb(l,k,j) * Velo(l)
              END DO
           END DO
           SecInv = SecInv + s * t
         END DO
       END DO
!------------------------------------------------------------------------------

     END IF
!------------------------------------------------------------------------------
   END FUNCTION SecondInvariant
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
   SUBROUTINE FrictionHeat( Heat,Viscosity,Ux,Uy,Uz,Element,Nodes )
DLLEXPORT FrictionHeat
!------------------------------------------------------------------------------
     REAL(KIND=dp)  :: Heat(:),Viscosity(:),Ux(:),Uy(:),Uz(:)
     TYPE(Nodes_t)     :: Nodes
     TYPE(Element_t)   :: Element
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(MAX_NODES),dBasisdx(MAX_NODES,3),ddBasisddx(1,1,1)
     REAL(KIND=dp) :: s,u,v,w,SqrtMetric,SqrtElementMetric,Velo(3)
     REAL(KIND=dp) :: Metric(3,3),dVelodx(3,3), &
                           CtrMetric(3,3),Symb(3,3,3),dSymb(3,3,3,3)

     LOGICAL :: stat
     INTEGER :: i,j,n
!------------------------------------------------------------------------------

     n = Element % TYPE % NumberOfNodes
     DO i=1,n
        u = Element % TYPE % NodeU(i)
        v = Element % TYPE % NodeV(i)
        w = Element % TYPE % NodeW(i)
!------------------------------------------------------------------------------
!       Basis function values & derivatives at the calculation point
!------------------------------------------------------------------------------
        stat = ElementInfo( Element,Nodes, u, v, w, &
          SqrtElementMetric, Basis,dBasisdx,ddBasisddx,.FALSE. )
!------------------------------------------------------------------------------
!       Coordinate system dependent information
!------------------------------------------------------------------------------
        CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb, &
               Nodes % x(i),Nodes % y(i),Nodes % z(i) )
!------------------------------------------------------------------------------

        DO j=1,3
          dVelodx(1,j) = SUM( Ux(1:n) * dBasisdx(1:n,j) )
          dVelodx(2,j) = SUM( Uy(1:n) * dBasisdx(1:n,j) )
          dVelodx(3,j) = SUM( Uz(1:n) * dBasisdx(1:n,j) )
        END DO

        Velo(1) = Ux(i)
        Velo(2) = Uy(i)
        Velo(3) = Uz(i)
        Heat(i) = 0.5d0*Viscosity(i)*SecondInvariant(Velo,dVelodx,Metric,Symb)
     END DO

!------------------------------------------------------------------------------
   END SUBROUTINE FrictionHeat
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
   FUNCTION EffectiveViscosity( Viscosity,Density,Ux,Uy,Uz,Element, &
                      Nodes,n,u,v,w ) RESULT(PVisc)
DLLEXPORT EffectiveViscosity
!------------------------------------------------------------------------------

     USE ModelDescription

     REAL(KIND=dp)  :: Viscosity,Density,u,v,w,PVisc,Ux(:),Uy(:),Uz(:)
     TYPE(Nodes_t)  :: Nodes
     INTEGER :: n
     TYPE(Element_t),POINTER :: Element

!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(1,1,1)
     REAL(KIND=dp) :: ss,s,SqrtMetric,SqrtElementMetric,Velo(3)
     REAL(KIND=dp) :: Metric(3,3), dVelodx(3,3), CtrMetric(3,3), &
                      Symb(3,3,3), dSymb(3,3,3,3)

     INTEGER :: i,j,k
     LOGICAL :: stat,GotIt

     CHARACTER(LEN=MAX_NAME_LEN) :: ViscosityFlag
     TYPE(ValueList_t), POINTER :: Material
     REAL(KIND=dp) :: x, y, z, c1n(n), c2n(n), c3n(n), c4n(n), &
         c1, c2, c3, c4, c5, c6, c7, Temp, h

     ! Temperature is needed for thermal models
     TYPE(Variable_t), POINTER :: TempSol 
     REAL(KIND=dp), POINTER :: Temperature(:)
     INTEGER, POINTER :: TempPerm(:)

     INTEGER(KIND=AddrInt) :: Fnc

     TYPE(Variable_t), POINTER :: Var
     REAL(KIND=dp) :: KE_K, KE_E, Clip, Cmu, KECmu(n)

     CHARACTER(LEN=MAX_NAME_LEN) :: str

     INTERFACE
       FUNCTION MaterialUserFunction( Proc,Model,Element,Nodes,n, &
          Basis,dBasisdx,Viscosity,Velo, dVelodx ) RESULT(s)
       USE Types
       INTEGER(KIND=AddrInt) :: Proc
       TYPE(Model_t) :: Model
       TYPE(Nodes_t) :: Nodes
       TYPE(Element_t), POINTER :: Element
       INTEGER :: n
       REAL(KIND=dp) :: Basis(:),dBasisdx(:,:),Viscosity, &
                    Velo(:), dVelodx(:,:), s
       END FUNCTION MaterialUserFunction
     END INTERFACE
!------------------------------------------------------------------------------
     PVisc = Viscosity
      
     k = ListGetInteger( CurrentModel % Bodies(Element % BodyId) % Values, 'Material', &
                   minv=1, maxv=CurrentModel % NumberOFMaterials )

     Material => CurrentModel % Materials(k) % Values

     ViscosityFlag = ListGetString( Material,'Viscosity Model', GotIt)

     IF(.NOT. gotIt) RETURN
!------------------------------------------------------------------------------
!    Basis function values & derivatives at the calculation point
!------------------------------------------------------------------------------
     stat = ElementInfo( Element,Nodes,u,v,w, &
         SqrtElementMetric, Basis,dBasisdx,ddBasisddx,.FALSE. )
!------------------------------------------------------------------------------
!   Coordinate system dependent information
!------------------------------------------------------------------------------
     x = SUM( Nodes % x(1:n) * Basis )
     y = SUM( Nodes % y(1:n) * Basis )
     z = SUM( Nodes % z(1:n) * Basis )
     CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,x,y,z )
!------------------------------------------------------------------------------
     DO j=1,3
       dVelodx(1,j) = SUM( Ux(1:n)*dBasisdx(1:n,j) )
       dVelodx(2,j) = SUM( Uy(1:n)*dBasisdx(1:n,j) )
       dVelodx(3,j) = SUM( Uz(1:n)*dBasisdx(1:n,j) )
     END DO
     
     Velo(1) = SUM( Basis * Ux(1:n) )
     Velo(2) = SUM( Basis * Uy(1:n) )
     Velo(3) = SUM( Basis * Uz(1:n) )
     
     ss = SecondInvariant( Velo,dVelodx,Metric,Symb ) / 2

     SELECT CASE( ViscosityFlag )

       CASE('power law')
       s = SQRT(ss)
       c1n = ListGetReal( Material, 'Viscosity Factor',n, Element % NodeIndexes,gotIt)
       IF(GotIt) THEN
         c1 = SUM( Basis * c1n(1:n) )
       ELSE 
         c1 = 1.0
       END IF
       c2n = ListGetReal( Material, 'Viscosity Exponent', n, Element % NodeIndexes,gotit )
       c2 = SUM( Basis * c2n(1:n) )
       c3n = ListGetReal( Material, 'Critical Shear Rate',n, Element % NodeIndexes,gotIt )
       IF (GotIt) THEN
         c3 = SUM( Basis * c3n(1:n) )
         IF(s < c3) s = c3
       END IF
       PVisc = Viscosity * c1 * (s ** (c2-1.0D0))

       CASE('power law too')
       c1n = ListGetReal( Material, 'Viscosity Factor',n, Element % NodeIndexes)
       c1 = SUM( Basis * c1n(1:n) )
       c2n = ListGetReal( Material, 'Viscosity Exponent', n, Element % NodeIndexes,gotit )
       c2 = SUM( Basis * c2n(1:n) )
       PVisc = Viscosity * c1**(-1.0d0/c2)* ss**(-(c2-1)/(2*c2)) / 2
       
       CASE ('carreau')
       c1n = ListGetReal( Material, 'Viscosity Difference',n,Element % NodeIndexes)
       c1 = SUM( Basis * c1n(1:n) )
       c2n = ListGetReal( Material, 'Viscosity Exponent', n, Element % NodeIndexes,gotit )
       c2 = SUM( Basis * c2n(1:n) )
       c3n = ListGetReal( Material, 'Viscosity Transition',n,Element % NodeIndexes)
       c3 = SUM( Basis * c3n(1:n) )
       c4 = ListGetConstReal( Material, 'Yasuda Coefficient',gotIt)
       IF(gotIt) THEN
         s = SQRT(ss)
         PVisc = Viscosity + c1 * (1.0d0 + (c3*s)**c4)**((c2-1)/c4) 
       ELSE
         PVisc = Viscosity + c1 * (1.0d0 + (c3*c3*ss))**((c2-1)/2.0) 
       END IF

       CASE ('cross')
       c1n = ListGetReal( Material, 'Viscosity Difference',n,Element % NodeIndexes)
       c1 = SUM( Basis * c1n(1:n) )
       c2n = ListGetReal( Material, 'Viscosity Exponent', n, Element % NodeIndexes,gotit )
       c2 = SUM( Basis * c2n(1:n) )
       c3n = ListGetReal( Material, 'Viscosity Transition',n,Element % NodeIndexes)
       c3 = SUM( Basis * c3n(1:n) )

       PVisc = Viscosity + c1 / (1.0d0 + c3*ss**(c2/2.0))
       
       CASE ('powell eyring')
       c1n = ListGetReal( Material, 'Viscosity Difference',n,Element % NodeIndexes)
       c1 = SUM( Basis * c1n )
       c2 = ListGetConstReal( Material, 'Viscosity Transition')
       s = SQRT(ss)

       IF(c2*s < 1.0d-5) THEN
         Pvisc = Viscosity + c1
       ELSE
         PVisc = Viscosity + c1 * LOG(c2*s+SQRT(c2*c2*ss+1.0d0))/(c2*s)
       END IF

       CASE ('thermal carreau')

       TempSol => VariableGet( CurrentModel % Variables, 'Temperature' )
       IF ( ASSOCIATED( TempSol) ) THEN
         TempPerm    => TempSol % Perm
         Temperature => TempSol % Values
       ELSE
         CALL Warn('EffectiveViscosity','Temperature required as a variable')
       END IF
       
       c1n = ListGetReal( Material, 'Viscosity Difference',n,Element % NodeIndexes)
       c2n = ListGetReal( Material, 'Viscosity Exponent', n, Element % NodeIndexes,gotit )
       c2 = SUM( Basis * c2n(1:n) )
       c3n = ListGetReal( Material, 'Viscosity Transition',n,Element % NodeIndexes)
       c4 = ListGetConstReal( Material, 'Yasuda Coefficient',gotIt)

       c5 = ListGetConstReal( Material, 'Viscosity Temp Ref')
       c6 = ListGetConstReal( Material, 'Viscosity Temp Exp')
       Temp = SUM(Basis(1:n) * Temperature(TempPerm(Element % NodeIndexes(1:n))))
       c7 = EXP(c6*(1/Temp-1/c5))
       
       c1 = c7 * SUM( Basis * c1n(1:n) )
       c3 = c7 * SUM( Basis * c3n(1:n) )

       IF(gotIt) THEN
         s = SQRT(ss)
         PVisc = Viscosity + c1 * (1.0d0 + (c3*s)**c4)**((c2-1)/c4) 
       ELSE
         PVisc = Viscosity + c1 * (1.0d0 + (c3*c3*ss))**((c2-1)/2.0) 
       END IF

       CASE( 'smagorinsky' )
          c2n = ListGetReal( Material, 'Smagorinsky Constant', &
                    n, Element % NodeIndexes,gotit )
          c2 = SUM( Basis * c2n(1:n) )
          h  = ElementDiameter( Element, Nodes )
          Viscosity = Viscosity + Density * c2 * h**2 * SQRT( 2*ss ) / 2

       CASE( 'ke','k-epsilon' )
          Var => VariableGet( CurrentModel % Variables, 'Kinetic Energy' )
          IF ( .NOT. ASSOCIATED( Var ) ) &
             CALL Fatal( 'Viscosity Model', 'The kinetic energy variable not defined?' )
          KE_K = SUM(Basis(1:n) * Var % Values(Var % Perm(Element % NodeIndexes)))

          Var => VariableGet( CurrentModel % Variables, 'Kinetic Dissipation' )
          IF ( .NOT. ASSOCIATED( Var ) ) &
             CALL Fatal( 'Viscosity Model', 'The kinetic energy dissipation variable not defined?' )
          KE_E = SUM(Basis(1:n) * Var % Values(Var % Perm(Element % NodeIndexes)))

          KECmu(1:n) = ListGetReal( Material, 'KE Cmu',n,Element % NodeIndexes )
          Clip       = ListGetConstReal( Material, 'KE Clip' )

          Cmu = SUM( Basis(1:n) * KECmu(1:n) )

          PVisc = Viscosity + Cmu*Density*KE_K**2 / KE_E

       CASE( 'rng k-epsilon' )
          Var => VariableGet( CurrentModel % Variables, 'Effective Viscosity')
          PVisc = SUM( Basis(1:n) * Var % Values( Var % Perm( Element % NodeIndexes )))

       CASE( 'user function' )
         str = ListGetString( Material, 'Viscosity Function' )
         Fnc = GetProcAddr( str, Quiet=.TRUE. )
         PVisc = MaterialUserFunction( Fnc, CurrentModel, Element, Nodes, n, &
                Basis, dBasisdx, Viscosity, Velo, dVelodx )
       
     CASE DEFAULT 
       CALL WARN('EffectiveViscosity','Unknown material model')

     END SELECT
!------------------------------------------------------------------------------
   END FUNCTION EffectiveViscosity
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
   SUBROUTINE KEPhi( SecInv,Ux,Uy,Uz,Element,Nodes )
DLLEXPORT KEPhi
!------------------------------------------------------------------------------

     REAL(KIND=dp)  :: Ux(:),Uy(:),Uz(:),SecInv(:)
     TYPE(Nodes_t)     :: Nodes
     TYPE(Element_t)   :: Element

!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(MAX_NODES),dBasisdx(MAX_NODES,3),ddBasisddx(1,1,1)
     REAL(KIND=dp) :: s,u,v,w,SqrtMetric,SqrtElementMetric,Velo(3)
     REAL(KIND=dp) :: Metric(3,3),dVelodx(3,3), &
                           CtrMetric(3,3),Symb(3,3,3),dSymb(3,3,3,3)

     LOGICAL :: stat
     INTEGER :: i,j,n
!------------------------------------------------------------------------------

     n = Element % TYPE % NumberOfNodes

     DO i=1,n
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the calculation point
!------------------------------------------------------------------------------
        u = Element % TYPE % NodeU(i)
        v = Element % TYPE % NodeV(i)
        w = Element % TYPE % NodeW(i)

        stat = ElementInfo( Element,Nodes,u,v,w, &
          SqrtElementMetric, Basis,dBasisdx,ddBasisddx,.FALSE. )
!------------------------------------------------------------------------------
!       Coordinate system dependent information
!------------------------------------------------------------------------------
        CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb, &
               Nodes % x(i),Nodes % y(i),Nodes % z(i) )
!------------------------------------------------------------------------------

        DO j=1,3
          dVelodx(1,j) = SUM( Ux(1:n)*dBasisdx(1:n,j) )
          dVelodx(2,j) = SUM( Uy(1:n)*dBasisdx(1:n,j) )
          dVelodx(3,j) = SUM( Uz(1:n)*dBasisdx(1:n,j) )
        END DO

        Velo(1) = Ux(i)
        Velo(2) = Uy(i)
        Velo(3) = Uz(i)
        SecInv(i) = SecondInvariant( Velo,dVelodx,Metric,Symb ) / 2.0d0

     END DO

!------------------------------------------------------------------------------
   END SUBROUTINE KEPhi
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
   FUNCTION EffectiveConductivity( Conductivity,Density,Element, &
        Temperature,Ux,Uy,Uz,Nodes,n,u,v,w ) RESULT(PCond)
DLLEXPORT EffectiveConductivity
!------------------------------------------------------------------------------
     USE ModelDescription

     REAL(KIND=dp)  :: Conductivity,Density,u,v,w,PCond, &
              Ux(:),Uy(:),Uz(:), Temperature(:)
     TYPE(Nodes_t)  :: Nodes
     INTEGER :: n
     TYPE(Element_t),POINTER :: Element

     INTERFACE
       FUNCTION MaterialUserFunction( Proc,Model,Element,Nodes,n, &
          Basis,dBasisdx,Conductivity,Temp, dTempdx ) RESULT(s)
       USE Types
       INTEGER(KIND=AddrInt) :: Proc
       TYPE(Model_t) :: Model
       TYPE(Nodes_t) :: Nodes
       TYPE(Element_t), POINTER :: Element
       INTEGER :: n
       REAL(KIND=dp) :: Basis(:),dBasisdx(:,:),Conductivity, &
                    Temp(:), dTempdx(:,:), s
       END FUNCTION MaterialUserFunction
     END INTERFACE
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),ddBasisddx(1,1,1)
     REAL(KIND=dp) :: ss,s,SqrtMetric,SqrtElementMetric,Velo(3)
     REAL(KIND=dp) :: Metric(3,3), dVelodx(3,3), CtrMetric(3,3), &
                      Symb(3,3,3), dSymb(3,3,3,3)

     INTEGER :: i,j,k
     LOGICAL :: stat,GotIt

     CHARACTER(LEN=MAX_NAME_LEN) :: ConductivityFlag
     TYPE(ValueList_t), POINTER :: Material
     REAL(KIND=dp) :: x, y, z, c1n(n), c2n(n), c3n(n), c4n(n), &
         c1, c2, c3, c4, c5, c6, c7, Temp(1), h, dTempdx(3,1), c_p

     ! Temperature is needed for thermal models
     TYPE(Variable_t), POINTER :: TempSol 

     INTEGER(KIND=AddrInt) :: Fnc

     TYPE(Variable_t), POINTER :: Var
     REAL(KIND=dp) :: KE_K, KE_E, Clip, Cmu, KECmu(n)

     CHARACTER(LEN=MAX_NAME_LEN) :: str
!------------------------------------------------------------------------------
     PCond = Conductivity
      
     k = ListGetInteger( CurrentModel % Bodies(Element % BodyId) % Values, &
             'Material', minv=1, maxv=CurrentModel % NumberOFMaterials )

     Material => CurrentModel % Materials(k) % Values

     ConductivityFlag = ListGetString( Material,'Heat Conductivity Model', GotIt)

     IF(.NOT. gotIt) RETURN
!------------------------------------------------------------------------------
!    Basis function values & derivatives at the calculation point
!------------------------------------------------------------------------------
     stat = ElementInfo( Element,Nodes,u,v,w, &
         SqrtElementMetric, Basis,dBasisdx,ddBasisddx,.FALSE. )
!------------------------------------------------------------------------------
!   Coordinate system dependent information
!------------------------------------------------------------------------------
     x = SUM( Nodes % x(1:n) * Basis )
     y = SUM( Nodes % y(1:n) * Basis )
     z = SUM( Nodes % z(1:n) * Basis )
     CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,x,y,z )
!------------------------------------------------------------------------------
     DO j=1,3
       dVelodx(1,j) = SUM( Ux(1:n)*dBasisdx(1:n,j) )
       dVelodx(2,j) = SUM( Uy(1:n)*dBasisdx(1:n,j) )
       dVelodx(3,j) = SUM( Uz(1:n)*dBasisdx(1:n,j) )
       dTempdx(j,1) = SUM( Temperature(1:n) * dBasisdx(1:n,j) )
     END DO
     
     Velo(1) = SUM( Basis * Ux(1:n) )
     Velo(2) = SUM( Basis * Uy(1:n) )
     Velo(3) = SUM( Basis * Uz(1:n) )

     Temp(1) = SUM( Basis * Temperature(1:n) )
     
     SELECT CASE( ConductivityFlag )

       CASE( 'ke','k-epsilon' )
          Var => VariableGet( CurrentModel % Variables, 'Kinetic Energy' )
          IF ( .NOT. ASSOCIATED( Var ) ) &
             CALL Fatal( 'Viscosity Model', 'The kinetic energy variable not defined?' )
          KE_K = SUM(Basis(1:n) * Var % Values(Var % Perm(Element % NodeIndexes)))

          Var => VariableGet( CurrentModel % Variables, 'Kinetic Dissipation' )
          IF ( .NOT. ASSOCIATED( Var ) ) &
             CALL Fatal( 'Viscosity Model', 'The kinetic energy dissipation variable not defined?' )
          KE_E = SUM(Basis(1:n) * Var % Values(Var % Perm(Element % NodeIndexes)))

          KECmu(1:n) = ListGetReal( Material, 'KE Cmu',n,Element % NodeIndexes )
          Cmu = SUM( Basis(1:n) * KECmu(1:n) )

          c1n(1:n) = ListGetReal( Material, 'Heat Capacity',n,Element % NodeIndexes )
          c_p = SUM( Basis(1:n) * c1n(1:n) )

          PCond = Conductivity + Density*c_p*Cmu*Density*KE_K**2 / KE_E

       CASE( 'user function' )
         str = ListGetString( Material, 'Heat Conductivity Function' )
         Fnc = GetProcAddr( str, Quiet=.TRUE. )
         PCond = MaterialUserFunction( Fnc, CurrentModel, Element, Nodes, n, &
              Basis, dBasisdx, Conductivity, Temp, dTempdx )
       
     CASE DEFAULT 
       CALL WARN('EffectiveConductivity','Unknown material model')

     END SELECT
!------------------------------------------------------------------------------

   END FUNCTION EffectiveConductivity
!------------------------------------------------------------------------------


END MODULE MaterialModels
