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
! * Utility routines for radiation computation
! *
! ******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                          Tietotie 6, P.O. BOX 405
! *                            02
! *                            Tel. +358 0 457 2723
! *                          Telefax: +358 0 457 2302
! *                        EMail: Juha.Ruokolainen@csc.fi
! *
! *                     Date: 02 Jun 1997
! *
! *              Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

MODULE Radiation

   USE ElementUtils
   USE CoordinateSystems

   IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
   FUNCTION ComputeRadiationLoad( Model, Mesh, Element, Temperature, &
                 Reorder, Emissivity, AngleFraction) RESULT(T)
DLLEXPORT ComputeRadiationLoad
!------------------------------------------------------------------------------
     TYPE(Mesh_t), POINTER :: Mesh
     TYPE(Model_t) :: Model
     TYPE(Element_t)  :: Element
     INTEGER :: Reorder(:)
     REAL(KIND=dp) :: Temperature(:),Emissivity
     REAL(KIND=dp), OPTIONAL :: AngleFraction
     REAL(KIND=dp) :: T

     REAL(KIND=dp) :: Asum

     TYPE(Element_t),POINTER  :: CurrentElement
     INTEGER :: i,j,n

     REAL(KIND=dp), POINTER :: Vals(:)
     INTEGER, POINTER :: Cols(:)

     REAL(KIND=dp) :: A1,A2,Emissivity1
!------------------------------------------------------------------------------
     A1 = Emissivity * ElementArea(Mesh,Element,Element % TYPE % NumberOfNodes)

     Cols => Element % BoundaryInfo % GebhardtFactors % Elements
     Vals => Element % BoundaryInfo % GebhardtFactors % Factors

     T = 0.0D0
     Asum = 0.0d0
     DO i=1,Element % BoundaryInfo % GebhardtFactors % NumberOfFactors
       CurrentElement => Mesh % Elements(Cols(i))
       n = CurrentElement % TYPE % NumberOfNodes

       Emissivity1 = SUM(ListGetReal( Model % BCs(CurrentElement % &
            BoundaryInfo % Constraint) % Values, 'Emissivity', &
            n, CurrentElement % NodeIndexes)) / n

       A2 = Emissivity1 * ElementArea( Mesh,CurrentElement, &
              CurrentElement % TYPE % NumberOfNodes )

       T = T + A2 * ABS(Vals(i)) * &
         SUM(Temperature(Reorder(CurrentElement % NodeIndexes))/N)**4

       Asum = Asum + A2 * ABS(Vals(i))
     END DO

     T = (T/A1)**(1.0D0/4.0D0)
     IF(PRESENT(AngleFraction)) THEN
       AngleFraction = Asum / A1
     END IF

   END FUNCTION ComputeRadiationLoad
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
   FUNCTION ComputeRadiationCoeff( Model,Mesh,Element,k ) RESULT(T)
DLLEXPORT ComputeRadiationCoeff
!------------------------------------------------------------------------------

     TYPE(Mesh_t), POINTER :: Mesh
     TYPE(Model_t)  :: Model
     TYPE(Element_t) :: Element
     INTEGER :: k
!------------------------------------------------------------------------------

     REAL(KIND=dp) :: T

     TYPE(Element_t),POINTER  :: CurrentElement
     INTEGER :: i,j,n

     REAL(KIND=dp) :: Area,Emissivity
!------------------------------------------------------------------------------

     CurrentElement => Model % Elements( &
             Element % BoundaryInfo % GebhardtFactors % Elements(k) )
     n = CurrentElement % TYPE % NumberOfNodes

     Emissivity = SUM(ListGetReal(Model % BCs(CurrentElement % &
        BoundaryInfo % Constraint) % Values, 'Emissivity', &
        n, CurrentElement % NodeIndexes)) / n

     Area = Emissivity * ElementArea( Mesh,CurrentElement, n)

     T =  ABS(Element % BoundaryInfo % GebhardtFactors % Factors(k)) * Area

   END FUNCTION ComputeRadiationCoeff
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE Radiation
!------------------------------------------------------------------------------
