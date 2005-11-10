! *****************************************************************************/
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
! *  Solver to compute Eigenvalues and Euler Angles of the second order
! *       orientation tensor
! *
! ******************************************************************************
! *
! *                     Author:       Fabien
! *
! *                       Date: 12/05/2004
! *
! *
! *****************************************************************************/

!------------------------------------------------------------------------------
   RECURSIVE SUBROUTINE EigenFabric( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------

    USE DefUtils

    IMPLICIT NONE
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve stress equations for one timestep
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
!     INPUT: Timestep size for time dependent simulations (NOTE: Not used
!            currently)
!
!******************************************************************************

     TYPE(Model_t)  :: Model
     TYPE(Solver_t), TARGET :: Solver

     LOGICAL ::  TransientSimulation
     REAL(KIND=dp) :: dt
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------


     TYPE(Nodes_t) :: ElementNodes
     TYPE(Element_t),POINTER :: CurrentElement


     TYPE(Variable_t), POINTER ::  FabricVariable,EigenFabricVariable

     REAL(KIND=dp), POINTER ::  FabricValues(:),EigenFabricValues(:)

     CHARACTER(LEN=MAX_NAME_LEN) :: EquationName

     INTEGER, POINTER :: FabricPerm(:),EigenFabricPerm(:),NodeIndexes(:)

!
     LOGICAL :: AllocationsDone = .FALSE.

     save AllocationsDone

     
     REAL(KIND=dp),ALLOCATABLE::  K1(:), K2(:), E1(:), E2(:), E3(:), &
      Ai1(:),Ai2(:),Ai3(:),Euler1(:),Euler2(:),Euler3(:)

     SAVE K1,K2,E1,E2,E3,Ai1,Ai2,Ai3,Euler1,Euler2,Euler3
           
     REAL(KIND=dp) :: at,at0,CPUTime,RealTime

     Real(KIND=dp), parameter :: Rad2deg=180._dp/Pi

     REAL(KIND=dp) :: a2(6)
     REAL(KIND=dp) :: ai(3), Angle(3)
           
     INTEGER :: t,n,i,dim

     save dim
     
      INTERFACE
        Subroutine R2Ro(a2,dim,ai,angle)
        USE Types
        REAL(KIND=dp),intent(in) :: a2(6)
        Integer :: dim
        REAL(KIND=dp),intent(out) :: ai(3), Angle(3)
       End Subroutine R2Ro
      End Interface                                                       
     
              
!
!

!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------
     IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN


      FabricVariable => VariableGet( Solver % Mesh % Variables, 'Fabric' )
      IF ( ASSOCIATED( FabricVariable ) ) THEN
       FabricPerm    => FabricVariable % Perm
       FabricValues => FabricVariable % Values
      END IF

      EigenFabricVariable => &
       VariableGet( Solver % Mesh % Variables, 'EigenV' )
      IF ( ASSOCIATED( EigenFabricVariable ) ) THEN
       EigenFabricPerm    => EigenFabricVariable % Perm
       EigenFabricValues => EigenFabricVariable % Values
      END IF
      


!------------------------------------------------------------------------------
!     Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
      IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed) THEN
       N = Model % MaxElementNodes
       dim = CoordinateSystemDimension()

       ALLOCATE( K1( N ), K2( N ), E1( N ), E2( N ), E3( N ), &
                 Ai1(N), Ai2(N) , Ai3(N), Euler1(N), Euler2(N), Euler3(N))

       AllocationsDone = .TRUE.
      END IF
!--------------------------------------------------------------------------------

!------------------------------------------------------------------------------

     EquationName = GetString( Solver % Values, 'Equation' )


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       at  = CPUTime()
       at0 = RealTime()

       CALL Info( 'EigenFabric', ' ', Level=4 )
       CALL Info( 'EigenFabric', ' ', Level=4 )
       CALL Info( 'EigenFabric','-------------------------------------',Level=4 )
       CALL Info( 'EigenFabric', '   EIGENFABRIC  SOLVER              ',Level=4 )
       CALL Info( 'EigenFabric', '-------------------------------------',Level=4 )
       CALL Info( 'EigenFabric', ' ', Level=4 )
       CALL Info( 'EigenFabric', ' ', Level=4 )
                             

!------------------------------------------------------------------------------
       DO t=1,Solver % NumberOFActiveElements


         CurrentElement => GetActiveElement(t)
         n = GetElementNOFNodes()
         NodeIndexes => CurrentElement % NodeIndexes


         K1 = FabricValues( 5 * (FabricPerm(NodeIndexes)-1) + 1 )
         K2 = FabricValues( 5 * (FabricPerm(NodeIndexes)-1) + 2 )
         E1 = FabricValues( 5 * (FabricPerm(NodeIndexes)-1) + 3 )
         E2 = FabricValues( 5 * (FabricPerm(NodeIndexes)-1) + 4 )
         E3 = FabricValues( 5 * (FabricPerm(NodeIndexes)-1) + 5 )

         
         Do i=1,n
          a2(1)=K1(i)
          a2(2)=K2(i)
          a2(3)=1._dp-a2(1)-a2(2)
          a2(4)=E1(i)
          a2(5)=E2(i)
          a2(6)=E3(i)

          call R2Ro(a2,dim,ai,angle)

          angle(:)=angle(:)*rad2deg
          
          Ai1(i)=ai(1)
          Ai2(i)=ai(2)
          Ai3(i)=ai(3)
          Euler1(i)=angle(1)
          Euler2(i)=angle(2)
          Euler3(i)=angle(3)
         End do
          
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 1)=Ai1
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 2)=Ai2
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 3 )=Ai3
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 4 )=Euler1
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 5 )=Euler2
         EigenFabricValues( 6 * (EigenFabricPerm(NodeIndexes)-1) + 6 )=Euler3


      END DO

!------------------------------------------------------------------------------
!        
!------------------------------------------------------------------------------
  END SUBROUTINE EigenFabric
!------------------------------------------------------------------------------
