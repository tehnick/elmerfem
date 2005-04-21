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
! *            Module Author: Peter Råback
! *
! *                  Address: CSC - Scientific Computing Ltd.
! *                           Tekniikantie 15 a D, Box 405
! *                           02101 Espoo, Finland
! *                           Tel. +358 0 457 2080
! *                           Telefax: +358 0 457 2302
! *                    EMail: Peter.Raback@csc.fi
! *
! *                     Date: 20 Nov 2001
! *
! *               Modified by: 
! *                     EMail: 
! *
! *      Date of modification: 
! *
! ****************************************************************************/


!------------------------------------------------------------------------------
SUBROUTINE AplacExport( Model,Solver,dt,TransientSimulation )
  !DEC$ATTRIBUTES DLLEXPORT :: AplacExport
!------------------------------------------------------------------------------
!******************************************************************************
!
!  This subroutine saves scalar values to a matrix.
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
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

  LOGICAL :: SubroutineVisited=.FALSE., GotIt, GotIt2
  CHARACTER(LEN=MAX_NAME_LEN) :: AplacFile, String1, String2
  INTEGER :: i,j,k,l, DIM, NoEigenModes
  INTEGER :: ElstatMode, ReynoMode, NoModes
  REAL(KIND=dp) :: Const1, Const2

  SAVE SubroutineVisited


  DIM = CoordinateSystemDimension()
   
  AplacFile = ListGetString(Solver % Values,'Filename',GotIt )
  IF(.NOT. GotIt) WRITE(AplacFile,*) 'Elmer2Aplac-test.e2a'
 
  CALL Info('AplacExport','------------------------------------------------------',Level=5)
  WRITE(Message,*) 'Making lumped Aplac model into file',TRIM(AplacFile)
  CALL Info('AplacExport',Message)
  CALL Info('AplacExport','------------------------------------------------------',Level=5)
 
  ElstatMode = ListGetInteger( Model % Simulation, 'aplac: elstat mode', GotIt)
  ReynoMode = ListGetInteger( Model % Simulation, 'aplac: reyno mode', GotIt)
  NoEigenModes = ListGetInteger( Model % Simulation, 'aplac: aper nomodes', GotIt)

  PRINT *,'Modes',ElstatMode,ReynoMode,NoEigenModes


  OPEN (10, FILE=AplacFile)
  
  IF(NoEigenModes > 0) THEN
    WRITE(10,'(A)') '$ eigenmode basis resonator'
  ELSE
    WRITE(10,'(A)') '$ statically biased resonator'    
  END IF

  ! Save some parameters for identification if they exist
  Const1 = ListGetConstReal( Model % Simulation, 'aplac: aper area',GotIt)
  IF(GotIt) WRITE(10,'(A,E10.3)') '$ resonator area',Const1
  Const1 = ListGetConstReal( Model % Simulation, 'aplac: aper vol',GotIt)
  IF(GotIt) WRITE(10,'(A,E10.3)') '$ resonator volume',Const1
  Const1 = ListGetConstReal( Model % Simulation, 'aplac: aper min',GotIt)
  IF(GotIt) WRITE(10,'(A,E10.3)') '$ minimum gap',Const1
  Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat voltage',GotIt)
  IF(GotIt) WRITE(10,'(A,E10.3)') '$ bias voltage',Const1
  Const1 = ListGetConstReal( Model % Simulation, 'aplac: aper tension',GotIt)
  IF(GotIt) WRITE(10,'(A,E10.3)') '$ tension',Const1


  ! Make the model for the elastic resonator
  
  WRITE(10,'(A)') ''
  WRITE(10,'(A)') 'Model "reso-parameters" USER_MODEL'

  IF(NoEigenModes > 0) THEN
    WRITE(10,'(A,I2,T35,A)') '+ N=',NoEigenModes,'$ number of eigenmodes'
    DO i=1,NoEigenModes
      WRITE(String1,'(A,I1)') 'aplac: aper mass ',i
      Const1 = ListGetConstReal( Model % Simulation, String1, GotIt) 
      WRITE(String2,'(A,I1)') 'aplac: aper spring ',i
      Const2 = ListGetConstReal( Model % Simulation, String2, GotIt2) 
      
      IF(NoEigenModes == 1) THEN
        IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ M=',Const1,'$ effective mass'
        IF(GotIt2) WRITE(10,'(A,ES13.6,T35,A)') '+ K=',Const2,'$ effective mechanical spring'
      ELSE
        IF(GotIt) WRITE(10,'(A,I1,A,ES13.6,T35,A,I1)') '+ M',i,'=',&
            Const1,'$ effective mass ',i
        IF(GotIt2) WRITE(10,'(A,I1,A,ES13.6,T35,A,I1)') '+ K',i,'=',&
            Const2,'$ effective mechanical spring ',i
      END IF
    END DO
  ELSE 
    WRITE(String1,'(A,I1)') 'aplac: aper mass ',i
    Const1 = ListGetConstReal( Model % Simulation, String1, GotIt) 
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ M=',Const1,'$ effective mass'    
    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat displ', GotIt)
    Const2 = ListGetConstReal( Model % Simulation, 'aplac: elstat force 1', GotIt2)
    IF(GotIt .AND. GotIt2) THEN
      WRITE(10,'(A,ES13.6,T35,A)') '+ K=',Const2/Const1,'$ effective mechanical spring'
    ELSE
      CALL Warn('AplacExport','Cant make lumped model for elasticity')
    END IF
  END IF


  ! Make the model for electrostatics
  WRITE(10,'(A)') ''
  WRITE(10,'(A)') 'Model "transducer-parameters" USER_MODEL'

  IF(ElstatMode == 1) THEN 
    WRITE(10,'(A,I2,T35,A)') '+ N=',NoEigenModes,'$ number of eigenmodes'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat capa', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ CE=',Const1,'$ capacitance'
    
    DO i=1,NoEigenModes
      WRITE(String1,'(A,I1)') 'aplac: elstat charge ',i
      Const1 = ListGetConstReal( Model % Simulation, String1, GotIt)
      IF(NoEigenModes == 1) THEN
        IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ IE=',&
            Const1,'$ el.mech transconductance'
      ELSE
        IF(GotIt) WRITE(10,'(A,I1,A,ES13.6,T35,A,I1)') '+ IE',i,'=',&
            Const1,'$ el.mech transconductance',i
      END IF
      
      DO j=1,NoEigenModes
        WRITE(String1,'(A,I1,I1)') 'aplac: elstat spring ',i,j
        Const1 = ListGetConstReal( Model % Simulation, String1, GotIt)
        
        IF(NoEigenModes == 1) THEN
          IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ KE=',&
              Const1,'$ electric spring coefficient'
        ELSE
          IF(GotIt) WRITE(10,'(A,I1,I1,A,ES13.6,T35,A,I1,I1)') '+ KE',i,j,'=',&
              Const1,'$ electric spring coefficient',i,j
        END IF
      END DO
    END DO
  ELSE IF(ElstatMode == 2) THEN
    WRITE(10,'(A)') '$ Capacitance is fitted to a physical model'
    WRITE(10,'(A)') '$ C = C0 * (B0 + B1 p + B2 (1+p)^a + B3 (1-p)^a), p=D/D0'

    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat capa', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ CE=',Const1,'$ capacitance at rest, C0'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat zcrit', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ D0=',Const1,'$ critical displacement, D0'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat c5', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ B0=',Const1,'$ factor for p^0, B0'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat c4', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ B1=',Const1,'$ factor for p^1, B1'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat c3', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ B2=',Const1,'$ factor for (1+p)^a, B2'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat c2', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ B3=',Const1,'$ factor for (1-p)^a, B3'
    Const1 = ListGetConstReal( Model % Simulation,'aplac: elstat c1', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ Ea=',Const1,'$ exponent a'
  ELSE
    CALL Warn('AplacExport','Electrostatics export format is unknown')
  END IF

  ! In some cases the fluidic resistance may be computed by a local approximation
  ! and the the solution of Reynolds equations is not necessary
  IF(ReynoMode == 0 .AND. ElstatMode > 0) THEN
    WRITE(10,'(A)') ''

    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat aeff1', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Aeff1 ',Const1,'$ effective area u'
    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat aeff2', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Aeff2 ',Const1,'$ effective area u^2'
    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat deff3', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Deff3 ',Const1,'$ effective distance'
    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat displ', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Displ ',Const1,'$ static displacement'
    Const1 = ListGetConstReal( Model % Simulation, 'aplac: elstat thick', GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Dthick ',Const1,'$ diaphragm thickness'

  ELSE IF(ReynoMode == 1) THEN 

    WRITE(10,'(A)') ''
    WRITE(10,'(A)') 'Model "squeezed-film-parameters" USER_MODEL'


    DO i=1,NoEigenModes
      DO j=1,NoEigenModes
        WRITE(String1,'(A,I1,I1)') 'aplac: reyno spring re ',i,j
        Const1 = ListGetConstReal( Model % Simulation, String1, GotIt)
        
        WRITE(String1,'(A,I1,I1)') 'aplac: reyno damping ',i,j
        Const2 = ListGetConstReal( Model % Simulation, String1, GotIt)
        
        IF(NoEigenModes == 1) THEN
          IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ KF=',&
              Const1,'$ fluidic spring coefficient'
          IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') '+ BF=',&
              Const2,'$ fluidic damping coefficient'
        ELSE
          IF(GotIt) WRITE(10,'(A,I1,I1,A,ES13.6,T35,A,I1,I1)') '+ KE',i,j,'=',&
              Const1,'$ fluidic spring coefficient ',i,j
          IF(GotIt) WRITE(10,'(A,I1,I1,A,ES13.6,T35,A,I1,I1)') '+ KE',i,j,'=',&
              Const2,'$ fluidic damping coefficient ',i,j
        END IF
      END DO
    END DO

  ELSE IF(ReynoMode == 2) THEN 
    
    WRITE(10,'(A)') ''
    WRITE(10,'(A)') 'Model "squeezed-film-parameters" USER_MODEL'
    WRITE(10,'(A)') '$ Fluidic spring constants fitted to a physical model'
    WRITE(10,'(A)') '$ Z = 1/(R+iwL), R=d^3/vA^2, L=d/AP'

    Const1 =  ListGetConstReal( Model % Simulation, 'aplac: reyno cutoff',GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Cutoff ',Const1,'$ cutoff frequecy'
    Const1 =  ListGetConstReal( Model % Simulation, 'aplac: reyno viscosity',GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Visc ',Const1,'$ viscosity'
    Const1 =  ListGetConstReal( Model % Simulation, 'aplac: reyno refpres',GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Pres0 ',Const1,'$ reference pressure'
    Const1 =  ListGetConstReal( Model % Simulation, 'aplac: reyno area',GotIt)
    IF(GotIt) WRITE(10,'(A,ES13.6,T35,A)') 'Var Area ',Const1,'$ fluidic area'

    DO i=1,NoEigenModes
      DO j=1,NoEigenModes
        WRITE(String1,'(A,I1,I1)') 'aplac: reyno spring corr ',i,j
        Const1 = ListGetConstReal( Model % Simulation, String1, GotIt)
        IF(GotIt) WRITE(10,'(A,I1,I1,ES13.6,T35,A)') 'Var Lcorr ',i,j,Const1,'$ spring correction'

        WRITE(String1,'(A,I1,I1)') 'aplac: reyno damping corr ',i,j
        Const1 = ListGetConstReal( Model % Simulation, String1, GotIt)
        IF(GotIt) WRITE(10,'(A,I1,I1,ES13.6,T35,A)') 'Var Rcorr ',i,j,Const1,'$ damping correction'
      END DO
    END DO

  ELSE
    CALL Warn('AplacExport','Fluidic export format is unknown')
  END IF

  CLOSE(10)
  

!------------------------------------------------------------------------------
END SUBROUTINE AplacExport
!------------------------------------------------------------------------------


