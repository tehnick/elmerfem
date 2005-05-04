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
! ******************************************************************************/
!
!/*******************************************************************************
! *
! * Diffuse-convective local matrix computing (cartesian coordinates)
! *
! *******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                              Tietotie 6, P.O. BOX 405
! *                                02101 Espoo, Finland
! *                                Tel. +358 0 457 2723
! *                              Telefax: +358 0 457 2302
! *                            EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 01 Oct 1996
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! ******************************************************************************/


MODULE DiffuseConvective

  USE MaterialModels
  USE Integration
  USE Differentials

  IMPLICIT NONE

  CONTAINS

!------------------------------------------------------------------------------
   SUBROUTINE DiffuseConvectiveCompose( MassMatrix,StiffMatrix,ForceVector,  &
      LoadVector,NodalCT,NodalC0,NodalC1,NodalC2,PhaseChange,Temperature, &
         Enthalpy,UX,UY,UZ,MUX,MUY,MUZ,NodalViscosity,NodalDensity,NodalPressure, &
            Compressible, Stabilize,UseBubbles,Element,n,Nodes )
DLLEXPORT DiffuseConvectiveCompose
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RSH vector for diffusion-convection
!  equation: 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: MassMatrix(:,:)
!     OUTPUT: time derivative coefficient matrix
!
!  REAL(KIND=dp) :: StiffMatrix(:,:)
!     OUTPUT: rest of the equation coefficients
!
!  REAL(KIND=dp) :: ForceVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: LoadVector(:)
!     INPUT:
!
!  REAL(KIND=dp) :: NodalCT,NodalC0,NodalC1
!     INPUT: Coefficient of the time derivative term, 0 degree term, and
!            the convection term respectively
!
!  REAL(KIND=dp) :: NodalC2(:,:,:)
!     INPUT: Nodal values of the diffusion term coefficient tensor
!
!  LOGICAL :: PhaseChange
!     INPUT: Do we model phase change here...
!
!  REAL(KIND=dp) :: Temperature
!     INPUT: Temperature from previous iteration, needed if we model
!            phase change
!
!  REAL(KIND=dp) :: Enthalpy
!     INPUT: Enthalpy from previous iteration, needed if we model
!            phase change
!
!  REAL(KIND=dp) :: UX(:),UY(:),UZ(:)
!     INPUT: Nodal values of velocity components from previous iteration
!           used only if coefficient of the convection term (C1) is nonzero
!
!  REAL(KIND=dp) :: NodalViscosity(:)
!     INPUT: Nodal values of the viscosity
!
!  LOGICAL :: Stabilize
!     INPUT: Should stabilzation be used ? Used only if coefficient of the
!            convection term (C1) is nonzero
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  INTEGER :: n
!       INPUT: Number of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!******************************************************************************

     REAL(KIND=dp), DIMENSION(:)   :: ForceVector,UX,UY,UZ,MUX,MUY,MUZ,LoadVector
     REAL(KIND=dp), DIMENSION(:,:) :: MassMatrix,StiffMatrix
     REAL(KIND=dp) :: Temperature(:),Enthalpy(:),NodalViscosity(:), &
                      NodalPressure(:), NodalDensity(:)
     REAL(KIND=dp) :: NodalC0(:),NodalC1(:),NodalCT(:),NodalC2(:,:,:),dT

     LOGICAL :: Stabilize,UseBubbles,PhaseChange,Compressible

     INTEGER :: n

     TYPE(Nodes_t) :: Nodes
     TYPE(Element_t), POINTER :: Element

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
!
     REAL(KIND=dp) :: ddBasisddx(n,3,3)
     REAL(KIND=dp) :: Basis(2*n)
     REAL(KIND=dp) :: dBasisdx(2*n,3),SqrtElementMetric

     REAL(KIND=dp) :: Velo(3),dVelodx(3,3),Force

     REAL(KIND=dp) :: A,M
     REAL(KIND=dp) :: Load

     REAL(KIND=dp) :: VNorm,hK,mK
     REAL(KIND=dp) :: Lambda=1.0,Pe,Pe1,Pe2,Tau,x,y,z

     INTEGER :: i,j,k,c,p,q,t,dim,N_Integ,NBasis

     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
     REAL(KIND=dp) :: s,u,v,w,dEnth,dTemp,Viscosity,DivVelo,Pressure,Density

     REAL(KIND=dp) :: C0,C00,C1,CT,C2(3,3),dC2dx(3,3,3),SU(n),SW(n)

     REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ

     LOGICAL :: stat,Convection,ConvectAndStabilize,Bubbles

!------------------------------------------------------------------------------

     dim = CoordinateSystemDimension()
     c = dim + 1

     ForceVector = 0.0D0
     StiffMatrix = 0.0D0
     MassMatrix  = 0.0D0
     Load = 0.0D0

     Convection =  ANY( NodalC1 /= 0.0d0 )
     NBasis = n
     Bubbles = .FALSE.
     IF ( Convection .AND. .NOT. Stabilize .AND. UseBubbles ) THEN
        NBasis = 2*n
        Bubbles = .TRUE.
     END IF

!------------------------------------------------------------------------------
!    Integration stuff
!------------------------------------------------------------------------------
     IF ( Bubbles ) THEN
        IntegStuff = GaussPoints( element, Element % Type % GaussPoints2 )
     ELSE
        IntegStuff = GaussPoints( element )
     END IF
     U_Integ => IntegStuff % u
     V_Integ => IntegStuff % v
     W_Integ => IntegStuff % w
     S_Integ => IntegStuff % s
     N_Integ =  IntegStuff % n

!------------------------------------------------------------------------------
!    Stabilization parameters: hK, mK (take a look at Franca et.al.)
!    If there is no convection term we don t need stabilization.
!------------------------------------------------------------------------------
     ConvectAndStabilize = .FALSE.
     IF ( Stabilize .AND. Convection ) THEN
       ConvectAndStabilize = .TRUE.
       hK = element % hK
       mK = element % StabilizationMK
     END IF

!------------------------------------------------------------------------------
!    Now we start integrating
!------------------------------------------------------------------------------
     DO t=1,N_Integ

       u = U_Integ(t)
       v = V_Integ(t)
       w = W_Integ(t)

!------------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
             Basis,dBasisdx,ddBasisddx,ConvectAndStabilize,Bubbles )

       s = SqrtElementMetric * S_Integ(t)
!------------------------------------------------------------------------------
!      Coefficient of the convection and time derivative terms
!      at the integration point
!------------------------------------------------------------------------------
       C0 = SUM( NodalC0(1:n) * Basis(1:n) )
       C1 = SUM( NodalC1(1:n) * Basis(1:n) )
       CT = SUM( NodalCT(1:n) * Basis(1:n) )
!------------------------------------------------------------------------------
!       Compute effective heatcapacity, if modelling phase change,
!       at the integration point.
!       NOTE: This is for heat equation only, not generally for diff.conv. equ.
!------------------------------------------------------------------------------
        IF ( PhaseChange ) THEN
          dEnth = 0.0D0
          dTemp = 0.0D0
          DO i=1,3
            dEnth = dEnth + SUM( Enthalpy(1:n) * dBasisdx(1:n,i) )**2
            dTemp = dTemp + SUM( Temperature(1:n) * dBasisdx(1:n,i) )**2
          END DO

          CT = SQRT( dEnth/dTemp )
        END IF
!------------------------------------------------------------------------------
!      Coefficient of the diffusion term & it s derivatives at the
!      integration point
!------------------------------------------------------------------------------
       Density = SUM( NodalDensity(1:n) * Basis(1:n) ) 

       DO i=1,dim
         DO j=1,dim
           C2(i,j) = SUM( NodalC2(i,j,1:n) * Basis(1:n) )
         END DO
       END DO

       DO i=1,dim
          C2(i,i) = EffectiveConductivity( C2(i,i), Density, Element, &
                 Temperature, UX,UY,UZ, Nodes, n, n, u, v, w )
       END DO
!------------------------------------------------------------------------------
!      If there's no convection term we don't need the velocities, and
!      also no need for stabilization
!------------------------------------------------------------------------------
       Convection = .FALSE.
       IF ( C1 /= 0.0D0 ) THEN
          Convection = .TRUE.
          IF ( PhaseChange ) C1 = CT
!------------------------------------------------------------------------------
!         Velocity from previous iteration at the integration point
!------------------------------------------------------------------------------
          Velo = 0.0D0
          Velo(1) = SUM( (UX(1:n)-MUX(1:n))*Basis(1:n) )
          Velo(2) = SUM( (UY(1:n)-MUY(1:n))*Basis(1:n) )
          IF ( dim > 2 ) Velo(3) = SUM( (UZ(1:n)-MUZ(1:n))*Basis(1:n) )

          IF ( Compressible ) THEN
            dVelodx = 0.0D0
            DO i=1,3
              dVelodx(1,i) = SUM( UX(1:n)*dBasisdx(1:n,i) )
              dVelodx(2,i) = SUM( UY(1:n)*dBasisdx(1:n,i) )
              IF ( dim > 2 ) dVelodx(3,i) = SUM( UZ(1:n)*dBasisdx(1:n,i) )
            END DO

            Pressure = SUM( NodalPressure(1:n)*Basis(1:n) )
            DivVelo = 0.0D0
            DO i=1,dim
              DivVelo = DivVelo + dVelodx(i,i)
            END DO
          END IF

          IF ( Stabilize ) THEN
!------------------------------------------------------------------------------
!           Stabilization parameter Tau
!------------------------------------------------------------------------------
            VNorm = SQRT( SUM(Velo(1:dim)**2) )

#if 1
            Pe  = MIN( 1.0D0, mK*hK*C1*VNorm/(2*ABS(C2(1,1))) )

            Tau = 0.0D0
            IF ( VNorm /= 0.0 ) THEN
               Tau = hK * Pe / (2 * C1 * VNorm)
            END IF
#else
            C00 = C0
            IF ( DT /= 0.0d0 ) C00 = C0 + CT / DT

            Pe1 = 0.0d0
            IF ( C00 /= 0.0d0 ) THEN
              Pe1 = 2 * ABS(C2(1,1)) / ( mK * C00 * hK**2 )
              Pe1 = C00 * hK**2 * MAX( 1.0d0, Pe1 )
            ELSE
              Pe1 = 2 * ABS(C2(1,1)) / mK
            END IF

            Pe2 = 0.0d0
            IF ( C2(1,1) /= 0.0d0 ) THEN
              Pe2 = ( mK * C1 * VNorm * hK ) / ABS(C2(1,1))
              Pe2 = 2 * ABS(C2(1,1)) * MAX( 1.0d0, Pe2 ) / mK
            ELSE
              Pe2 = 2 * hK * C1 * VNorm
            END IF

            Tau = hk**2 / ( Pe1 + Pe2 )
#endif
!------------------------------------------------------------------------------

            DO i=1,dim
              DO j=1,dim
                DO k=1,dim
                  dC2dx(i,j,k) = SUM( NodalC2(i,j,1:n)*dBasisdx(1:n,k) )
                END DO
              END DO
            END DO

!------------------------------------------------------------------------------
!           Compute residual & stablization vectors
!------------------------------------------------------------------------------
            DO p=1,N
              SU(p) = C0 * Basis(p)
              DO i = 1,dim
                SU(p) = SU(p) + C1 * dBasisdx(p,i) * Velo(i)
                DO j=1,dim
                  SU(p) = SU(p) - C2(i,j) * ddBasisddx(p,i,j)
                  SU(p) = SU(p) - dC2dx(i,j,j) * dBasisdx(p,i)
                END DO
              END DO

              SW(p) = C0 * Basis(p)
              DO i = 1,dim
                SW(p) = SW(p) + C1 * dBasisdx(p,i) * Velo(i)
                DO j=1,dim
                  SW(p) = SW(p) - C2(i,j) * ddBasisddx(p,i,j)
                  SW(p) = SW(p) - dC2dx(i,j,j) * dBasisdx(p,i)
                END DO
              END DO
            END DO
          END IF
        END IF

!------------------------------------------------------------------------------
!       Loop over basis functions of both unknowns and weights
!------------------------------------------------------------------------------
        DO p=1,NBasis
        DO q=1,NBasis
!------------------------------------------------------------------------------
!         The diffusive-convective equation without stabilization
!------------------------------------------------------------------------------
          M = CT * Basis(q) * Basis(p)
          A = C0 * Basis(q) * Basis(p)
!------------------------------------------------------------------------------
!         The diffusion term
!------------------------------------------------------------------------------
          DO i=1,dim
            DO j=1,dim
              A = A + C2(i,j) * dBasisdx(q,i) * dBasisdx(p,j)
            END DO
          END DO

          IF ( Convection ) THEN
!------------------------------------------------------------------------------
!           The convection term
!------------------------------------------------------------------------------
            DO i=1,dim
              A = A + C1 * Velo(i) * dBasisdx(q,i) * Basis(p)
            END DO
!------------------------------------------------------------------------------
!           Next we add the stabilization...
!------------------------------------------------------------------------------
            IF ( Stabilize ) THEN
              A = A + Tau * SU(q) * SW(p)
              M = M + Tau * CT * Basis(q) * SW(p)
            END IF
          END IF

          StiffMatrix(p,q) = StiffMatrix(p,q) + s * A
          MassMatrix(p,q)  = MassMatrix(p,q)  + s * M
        END DO
        END DO

!------------------------------------------------------------------------------
!       The righthand side...
!------------------------------------------------------------------------------
!       Force at the integration point
!------------------------------------------------------------------------------
        Force = SUM( LoadVector(1:n)*Basis(1:n) ) + &
          JouleHeat( Element, Nodes, u, v, w )

        IF ( Convection ) THEN
          IF ( Compressible ) Force = Force - Pressure * DivVelo
          Viscosity = SUM( NodalViscosity(1:n) * Basis(1:n) )
          Viscosity = EffectiveViscosity( Viscosity, 1.0d0, UX, UY, UZ, &
                       Element, Nodes, n, n, U, V, W )
          IF ( Viscosity > 0.0D0 ) THEN
            IF ( .NOT.Compressible ) THEN
              dVelodx = 0.0D0
              DO i=1,3
                dVelodx(1,i) = SUM( UX(1:n)*dBasisdx(1:n,i) )
                dVelodx(2,i) = SUM( UY(1:n)*dBasisdx(1:n,i) )
                IF ( dim > 2 ) dVelodx(3,i) = SUM( UZ(1:n)*dBasisdx(1:n,i) )
              END DO
            END IF
            Force = Force + 0.5d0 * Viscosity*SecondInvariant(Velo,dVelodx)
          END IF
        END IF
!------------------------------------------------------------------------------
        DO p=1,NBasis
          Load = Basis(p)
          IF ( ConvectAndStabilize ) Load = Load + Tau * SW(p)
          ForceVector(p) = ForceVector(p) + s * Force * Load
        END DO
      END DO
!------------------------------------------------------------------------------
   END SUBROUTINE DiffuseConvectiveCompose
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
   SUBROUTINE DiffuseConvectiveBoundary( BoundaryMatrix,BoundaryVector, &
               LoadVector,NodalAlpha,Element,n,Nodes )
DLLEXPORT DiffuseConvectiveBoundary
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RSH vector for boundary conditions
!  of diffusion convection equation: 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: BoundaryMatrix(:,:)
!     OUTPUT: coefficient matrix if equations
!
!  REAL(KIND=dp) :: BoundaryVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: LoadVector(:)
!     INPUT: coefficient of the force term
!
!  REAL(KIND=dp) :: NodalAlpha
!     INPUT: coefficient for temperature dependent term
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!   INTEGER :: n
!       INPUT: Number  of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!******************************************************************************

     REAL(KIND=dp) :: BoundaryMatrix(:,:),BoundaryVector(:), &
                    LoadVector(:),NodalAlpha(:)

     TYPE(Nodes_t)   :: Nodes
     TYPE(Element_t) :: Element

     INTEGER :: n

     REAL(KIND=dp) :: ddBasisddx(n,3,3)
     REAL(KIND=dp) :: Basis(n)
     REAL(KIND=dp) :: dBasisdx(n,3),SqrtElementMetric

     REAL(KIND=dp) :: U,V,W,S
     REAL(KIND=dp) :: Force,Alpha
     REAL(KIND=dp), POINTER :: U_Integ(:),V_Integ(:),W_Integ(:),S_Integ(:)

     INTEGER :: i,t,q,p,N_Integ

     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

     LOGICAL :: stat
!------------------------------------------------------------------------------

     BoundaryVector = 0.0D0
     BoundaryMatrix = 0.0D0
!------------------------------------------------------------------------------
!    Integration stuff
!------------------------------------------------------------------------------
     IntegStuff = GaussPoints( Element )
     U_Integ => IntegStuff % u
     V_Integ => IntegStuff % v
     W_Integ => IntegStuff % w
     S_Integ => IntegStuff % s
     N_Integ =  IntegStuff % n

!------------------------------------------------------------------------------
!   Now we start integrating
!------------------------------------------------------------------------------
     DO t=1,N_Integ
       U = U_Integ(t)
       V = V_Integ(t)
       W = W_Integ(t)
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
       stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
                  Basis,dBasisdx,ddBasisddx,.FALSE. )

       S = SqrtElementMetric * S_Integ(t)
!------------------------------------------------------------------------------
       Force = SUM( LoadVector(1:n)*Basis )
       Alpha = SUM( NodalAlpha(1:n)*Basis )

       DO p=1,N
         DO q=1,N
           BoundaryMatrix(p,q) = BoundaryMatrix(p,q) + &
              s * Alpha * Basis(q) * Basis(p)
         END DO
       END DO

       DO q=1,N
         BoundaryVector(q) = BoundaryVector(q) + s * Basis(q) * Force
       END DO
     END DO
   END SUBROUTINE DiffuseConvectiveBoundary
!------------------------------------------------------------------------------

END MODULE DiffuseConvective
