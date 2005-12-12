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
!*
! ******************************************************************************
! *
! *                    Author:  Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *
! *
!/******************************************************************************
! *
! *  iceproperties.f90  material parameters and physical models for ice flow
! *
! *
! *       Module Author:           Thomas Zwinger
! *       Address:                 Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2183
! *                                EMail: Thomas.Zwinger@csc.fi
! *
! *       Modified by:             Thomas Zwinger
! *
! *       Date of modification: 11/11/2005
! *
! *****************************************************************************/
!
!
!




!*********************************************************************************************************************************
!*
!*  basal melting rate as a function of internal and external heat flux and latent heat
!*
!*********************************************************************************************************************************
FUNCTION basalMelting( Model, Node, dummyArgument ) RESULT(basalMeltingRate)

!-----------------------------------------------------------
  USE DefUtils
  USE SolverUtils
!-----------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------
  !external variables
  TYPE(Model_t), TARGET :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: dummyArgument, basalMeltingRate
  !internal variables
  TYPE(Element_t), POINTER :: CurrentElementAtBeginning, BoundaryElement, ParentElement
  TYPE(Nodes_t) :: Nodes
  TYPE(ValueList_t), POINTER :: ParentMaterial, BC
  TYPE(Variable_t), POINTER :: varTemperature, varPressure
  INTEGER :: N, NBoundary, NumberOfBoundaryNodes, NParent, BoundaryElementNode, ParentElementNode, &
       i, k, M, DIM, other_body_id, body_id, material_id, istat, NSDOFs
  INTEGER, POINTER :: BoundaryReorder(:)
  REAL(KIND=dp) :: U, V, W, gradTemperature(3), Gravity(3),&
       Normal(3), Tangent1(3), Tangent2(3), NormalVelocity, TangentialVelocity(3),&
       TangentialVelocity1, TangentialVelocity2,  StressVector(3), FrictionalHeatProduction,&
       grav, InternalHeatFlux, HeatFlux, SqrtElementMetric, pressure, Tolerance
  REAL(KIND=dp), ALLOCATABLE :: Basis(:),dBasisdx(:,:), ddBasisddx(:,:,:),&
       LatentHeat(:), HeatConductivity(:), Density(:), Temperature(:),&
       ExternalHeatFlux(:), ClausiusClapeyron(:),PressureMeltingPoint(:),&
       Velocity(:,:), SlipCoefficient(:,:)
  REAL(KIND=dp), DIMENSION(:,:),  POINTER :: Work, BoundaryNormals,BoundaryTangent1, &
                       BoundaryTangent2
  CHARACTER(LEN=MAX_NAME_LEN) :: TempName, VariableName
  LOGICAL ::  FirstTime = .TRUE., GotIt, stat, ComputeFrictionalHeat, NormalTangentialVelocity

  SAVE FirstTime, NumberOfBoundaryNodes,BoundaryReorder,BoundaryNormals, &
       BoundaryTangent1, BoundaryTangent2, DIM, N, &
       Nodes, Basis, dBasisdx, ddBasisddx, LatentHeat, &
       HeatConductivity, Density, PressureMeltingPoint,&
       Temperature, ExternalHeatFlux, ClausiusClapeyron,&
       Velocity, SlipCoefficient
                
!----------------------------------------------------------  
  CurrentElementAtBeginning => Model % CurrentElement
  !--------------------------------
  ! Allocations
  !--------------------------------
 
  IF ( FirstTime .OR. Model % Mesh % Changed) THEN
      DIM = CoordinateSystemDimension()
      N = Model % MaxElementNodes 

      IF (.NOT.FirstTime) THEN
         DEALLOCATE( Nodes % x, Nodes % y, Nodes % z,&
              Basis, dBasisdx, ddBasisddx, &
              LatentHeat,&
              HeatConductivity,&               
              Density,&
              PressureMeltingPoint,&
              Temperature,&
              ExternalHeatFlux,&
              ClausiusClapeyron,&
              Velocity,&
              SlipCoefficient)
      END IF
      ALLOCATE(Nodes % x(N), Nodes % y(N), Nodes % z(N),&
           Basis(N), dBasisdx(N,3), ddBasisddx(N,3,3), &
           LatentHeat( N ),&
           HeatConductivity( N ),&
           PressureMeltingPoint( N ),&
           Density( N ),&
           Temperature( N ),&
           ExternalHeatFlux( N ),&
           ClausiusClapeyron( N),&
           Velocity(DIM,N), &
           SlipCoefficient(DIM,N),&       
           STAT = istat)
      IF (istat /= 0) THEN
         CALL FATAL('iceproperties (basalMelting)','Allocations failed')
      ELSE 
         CALL INFO('iceproperties (basalMelting)','Allocations done')
      END IF
      CALL CheckNormalTangentialBoundary( Model, &
           'Basal Melting', NumberOfBoundaryNodes, &
           BoundaryReorder, BoundaryNormals, BoundaryTangent1, &
           BoundaryTangent2, DIM )

      WRITE(Message,'(A,i6)') &
          'Number of boundary nodes on boundaries associated with melting:',&
           NumberOfBoundaryNodes
      CALL INFO('iceproperties (basalMelting)','Message',Level=3)
      
      CALL AverageBoundaryNormals( Model, &
           'Basal Melting', NumberOfBoundaryNodes, &
           BoundaryReorder, BoundaryNormals, BoundaryTangent1, &
           BoundaryTangent2, DIM )
      FirstTime = .FALSE.
   END IF

 
  !-----------------------------------------------------------------
  ! get some information upon active boundary element and its parent
  !-----------------------------------------------------------------
  BoundaryElement => Model % CurrentElement
  IF ( .NOT. ASSOCIATED(BoundaryElement) ) THEN
     CALL FATAL('iceproperties (basalMelting)','No boundary element found')
  END IF  
  NBoundary = BoundaryElement % Type % NumberOfNodes
  DO BoundaryElementNode=1,NBoundary
     IF (Node .EQ. BoundaryElement % NodeIndexes(BoundaryElementNode)) THEN
        GotIt = .TRUE.
        EXIT
     END IF
  END DO
  IF (.NOT.GotIt) THEN
     CALL WARN('iceproperties (basalMelting)','Node not found in Current Element')
     BasalMeltingRate = 0.0D00
     RETURN
  END IF
  other_body_id = BoundaryElement % BoundaryInfo % outbody
  IF (other_body_id < 1) THEN ! only one body in calculation
     ParentElement => BoundaryElement % BoundaryInfo % Right
     IF ( .NOT. ASSOCIATED(ParentElement) ) ParentElement => BoundaryElement % BoundaryInfo % Left
  ELSE ! we are dealing with a body-body boundary and asume that the normal is pointing outwards
     ParentElement => BoundaryElement % BoundaryInfo % Right
     IF (ParentElement % BodyId == other_body_id) ParentElement => BoundaryElement % BoundaryInfo % Left
  END IF
  ! just to be on the save side, check again
  IF ( .NOT. ASSOCIATED(ParentElement) ) THEN
     WRITE(Message,'(A,I10,A)')&
          'Parent Element for Boundary element no. ',&
          BoundaryElement % ElementIndex, ' not found'
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF  

  body_id = ParentElement % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  ParentMaterial => Model % Materials(material_id) % Values
  IF ((.NOT. ASSOCIATED(ParentMaterial)) .OR. (.NOT. GotIt)) THEN
     WRITE(Message,'(A,I10,A,I10)')&
          'No material values found for body no ', body_id,&
          ' under material id ', material_id
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF  
  !----------------------
  ! get boundary values
  !----------------------  
  BC => GetBC()
  IF (.NOT.ASSOCIATED(BC)) THEN
     CALL FATAL('iceproperties (basalSlip)','No Boundary Condition associated')
  ELSE
     ComputeFrictionalHeat = GetLogical(BC, 'Compute Frictional Heat', GotIt)
     IF (.NOT.GotIt) THEN
        ComputeFrictionalHeat = .FALSE.
        SlipCoefficient(1:3,1:NBoundary) = 0.0D00
        Velocity(1:DIM,1:NBoundary) = 0.0D00
     ELSE
        NormalTangentialVelocity = GetLogical(BC, 'Normal-Tangential Velocity', GotIt)
        IF (.NOT.GotIt) THEN
           NormalTangentialVelocity = .FALSE.
        END IF
 
        DO i=1,DIM
           WRITE(VariableName, '(A,i1)') 'Slip Coefficient ', i 
           SlipCoefficient(i,1:NBoundary) = &
                ListGetReal(BC,TRIM(VariableName),NBoundary, BoundaryElement % NodeIndexes, GotIt)
           IF (.NOT.GotIt) SlipCoefficient(i,1:NBoundary) = 0.0D00
        END DO
     END IF
  END IF
  !----------------------
  ! get velocities
  !----------------------  
  IF (ComputeFrictionalHeat) THEN     
     DO i=1,DIM
        WRITE(VariableName,'(A,i1)') 'Velocity ', i 
        CALL GetScalarLocalSolution( Velocity(i,:), TRIM(VariableName))
     END DO
  ELSE
     Velocity(1:DIM,1:NBoundary) = 0.0D00
  END IF
  !--------------------------------------------------------
  ! Get normal of the boundary element at node
  !--------------------------------------------------------
  k = BoundaryReorder(Node)
  Normal(1:DIM) = BoundaryNormals(k,1:DIM)
  IF (ComputeFrictionalHeat) THEN
     Tangent1(1:DIM) = BoundaryTangent1(k,1:DIM)
     Tangent2(1:DIM) = BoundaryTangent1(k,1:DIM)
     !--------------------------
     ! Get tangential velocity
     !--------------------------
     NormalVelocity = SUM(Velocity(1:DIM,BoundaryElementNode)*Normal(1:DIM))
     TangentialVelocity(1:DIM) = Velocity(1:DIM,BoundaryElementNode) - NormalVelocity * Normal(1:DIM)
     IF (NormalTangentialVelocity) THEN
        TangentialVelocity1 = SUM(Velocity(1:DIM,BoundaryElementNode)*Tangent1(1:DIM))
        TangentialVelocity2 = SUM(Velocity(1:DIM,BoundaryElementNode)*Tangent2(1:DIM))
        StressVector(1:DIM) = SlipCoefficient(1,BoundaryElementNode)*TangentialVelocity1*Tangent1(1:DIM) + &
             SlipCoefficient(2,BoundaryElementNode)*TangentialVelocity2*Tangent2(1:DIM)     
     ELSE
        DO i=1,DIM
           StressVector(i) = SlipCoefficient(i,BoundaryElementNode)*TangentialVelocity(i)
        END DO
     END IF
  END IF
  ! ----------------------------------
  ! Get information on parent element
  ! ----------------------------------
  NParent = ParentElement % Type % NumberOfNodes
  DO ParentElementNode=1,NParent
     IF ( Node == ParentElement % NodeIndexes(ParentElementNode) ) EXIT
  END DO
  U = ParentElement % Type % NodeU(ParentElementNode)
  V = ParentElement % Type % NodeV(ParentElementNode)
  W = ParentElement % Type % NodeW(ParentElementNode)
  Nodes % x(1:NParent) = Model % Nodes % x(ParentElement % NodeIndexes)
  Nodes % y(1:NParent)  = Model % Nodes % y(ParentElement % NodeIndexes)
  Nodes % z(1:NParent)  = Model % Nodes % z(ParentElement % NodeIndexes)
  stat = ElementInfo( ParentElement,Nodes,U,V,W,SqrtElementMetric, &
       Basis,dBasisdx,ddBasisddx,.FALSE.,.FALSE. )

  !-------------------------
  ! Get Temperature Field
  !-------------------------
  TempName =  GetString(ParentMaterial ,'Temperature Name', GotIt)
  IF (.NOT.GotIt) THEN
     CALL FATAL('iceproperties (basalMelting)','No Temperature Name found')
  ELSE
     WRITE(Message,'(a,a)') 'Variable Name for temperature: ', TempName
     CALL INFO('iceproperties (basalMelting)',Message,Level=12)
  END IF
  VarTemperature => VariableGet( Model % Variables, TempName, .TRUE. )
  IF ( ASSOCIATED( VarTemperature ) ) THEN
     Temperature(1:NParent) = VarTemperature % Values(VarTemperature % Perm(ParentElement % NodeIndexes))
  ELSE
     CALL FATAL('iceproperties (basalMelting)','No Temperature Variable found')
  END IF
  !-------------------------
  ! Get Pressure Melting Point
  !-------------------------
  Model % CurrentElement => ParentElement
  PressureMeltingPoint(1:NParent) =&
       ListGetReal( ParentMaterial, TRIM(TempName) // ' Upper Limit',&
       NParent, ParentElement % NodeIndexes)
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,a,a)') 'No entry for ', TRIM(TempName) // ' Upper Limit', ' found'
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF
  !---------------------------------------
  ! Limit Temperature to physical values
  !--------------------------------------
  DO i=1,NParent
     Temperature(i) = MIN(Temperature(i), PressureMeltingPoint(i))
  END DO
  !-------------------------
  ! Get Temperature Gradient
  !-------------------------
  gradTemperature = 0.0D00
  DO i=1,DIM
     gradTemperature(i) = SUM(dBasisdx(1:NParent,i)*Temperature(1:NParent))
  END DO
  !-------------------------
  ! Get material parameters
  !-------------------------
  LatentHeat(1:NParent) = ListGetReal(ParentMaterial, 'Latent Heat', NParent, ParentElement % NodeIndexes, GotIt)
  IF (.NOT. GotIt) THEN
     CALL FATAL('iceproperties (basalMelting)','No value for Latent Heat found')
  END IF
  HeatConductivity(1:NParent) = ListGetReal(Model % Materials(material_id) % Values, &
       TRIM(TempName) // ' Heat Conductivity', NParent, ParentElement % NodeIndexes, GotIt)
  IF (.NOT. GotIt) THEN
     CALL FATAL('iceproperties (basalMelting)','No value for Heat Conductivity found')
  END IF
  Density(1:NParent) = ListGetReal( ParentMaterial, 'Density', NParent, ParentElement % NodeIndexes)
  IF (.NOT. GotIt) THEN
     CALL FATAL('iceproperties (basalMelting)','No value for Density found')
  END IF
  ClausiusClapeyron(1:NParent) = & 
       ListGetReal( ParentMaterial, 'Clausius Clapeyron', NParent, ParentElement % NodeIndexes)
  IF (.NOT. GotIt) THEN
     CALL FATAL('iceproperties (basalMelting)','No value for Clausius Clapeyron parameter found')
  END IF

  Model % CurrentElement => BoundaryElement
  BC => GetBC()
  Tolerance = GetConstReal(BC, TRIM(TempName) // ' Tolerance', GotIt)
  IF (.NOT.GotIt) THEN
     Tolerance = 0.0D00
     WRITE(Message,'(A)') 'No Keyword >'// TRIM(TempName) // ' Tolerance < found'
  ELSE
     WRITE(Message,'(A, e10.4)') TRIM(TempName) // ' Tolerance = ', Tolerance
  END IF
  CALL INFO('iceproperties (basalMelting)',Message,level=9)
  IF (Temperature(ParentElementNode) .GE. PressureMeltingPoint(ParentElementNode) - Tolerance) THEN
     !-----------------------------------
     ! compute contribution from friction
     ! u_|| . t = u^2_|| . R
     !-----------------------------------
     IF (ComputeFrictionalHeat) THEN
        FrictionalHeatProduction = SUM(TangentialVelocity(1:DIM)*StressVector(1:DIM)) 
     ELSE
        FrictionalHeatProduction = 0.0D00
     END IF
     !----------------------------
     ! compute internal heat flux
     !----------------------------
     InternalHeatFlux = -1.0D00 * HeatConductivity(ParentElementNode) * SUM(gradTemperature(1:DIM)*Normal(1:DIM))
     !-------------------------
     ! get external heat flux
     !------------------------
     ExternalHeatFlux = 0.0D00
     IF (other_body_id < 1) THEN ! we are dealing with an external heat flux
        ExternalHeatFlux(1:NBoundary) = GetReal(BC, TRIM(TempName) // ' Heat Flux', GotIt)
        IF (.NOT. GotIt) THEN
           CALL INFO('iceproperties (basalMelting)','No external heat flux given', Level=4)
        END IF
     ELSE ! we are dealing with a heat conducting body on the other side 
        CALL FATAL('iceproperties (basalMelting)','Interface condition not implemented!')
     END IF

     HeatFlux = ExternalHeatFlux(BoundaryElementNode) + InternalHeatFlux + FrictionalHeatProduction


     WRITE(Message,'(A,e10.4,A,e10.4,A,e10.4,A,e10.4)') 'Q=', HeatFlux, &
          'Q_ext %=', ExternalHeatFlux(BoundaryElementNode)/HeatFlux, &
          'Q_int %=', InternalHeatFlux/HeatFlux, &
          'S_frict %=', FrictionalHeatProduction/HeatFlux
     CALL INFO('iceproperties (basalMelting)',Message,Level=5)
!     HeatFlux = FrictionalHeatProduction

     IF (HeatFlux <= 0.0D00) THEN
        WRITE(Message,'(A, i7, A, e10.4, A, e10.4, A, e10.4, A)') &
             'Heatflux towards temperate boundary node', Node, ': ', &
             HeatFlux, ' = ', ExternalHeatFlux(BoundaryElementNode), ' + ', InternalHeatFlux, &
             ' < 0!'
        CALL INFO('iceproperties (basalMelting)',Message,level=9)
        BasalMeltingRate = 0.0D00
     ELSE
     ! basal melting rate (volume/time and area) == normal velocity    
        BasalMeltingRate =  HeatFlux/&
             (LatentHeat(ParentElementNode) * Density(ParentElementNode))
     END IF
  !----------------------------------------------
  ! T < T_m no basal melting flux (cold ice base) 
  !----------------------------------------------
  ELSE 
     BasalMeltingRate = 0.0D00
  END IF
  !----------------------------------------------
  ! clean up before leaving
  !----------------------------------------------
  Model % CurrentElement => CurrentElementAtBeginning 
END FUNCTION basalMelting

!*********************************************************************************************************************************
!*
!*  basal slip coefficient as a function of temperature
!*
!*********************************************************************************************************************************

FUNCTION basalSlip( Model, Node, Temperature ) RESULT(basalSlipCoefficient)
!-----------------------------------------------------------
  USE DefUtils
!-----------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------
  !external variables
  TYPE(Model_t), TARGET :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: Temperature, basalSlipCoefficient
  !internal variables
  TYPE(Element_t), POINTER :: CurrentElementAtBeginning, BoundaryElement, ParentElement
  TYPE(ValueList_t), POINTER :: ParentMaterial, BC
  TYPE(Variable_t), POINTER :: varTemperature, varPressure
  INTEGER :: N, NBoundary, NParent, BoundaryElementNode, ParentElementNode, &
       i, DIM, other_body_id, body_id, material_id, istat, NSDOFs
  REAL(KIND=dp) :: TempHom, ThermalCoefficient
  REAL(KIND=dp), ALLOCATABLE :: PressureMeltingPoint(:), TemperateSlipCoefficient(:)
  CHARACTER(LEN=MAX_NAME_LEN) :: TempName
  LOGICAL ::  GotIt, stat, Jump=.FALSE.

  !---------------
  ! Initialization
  !---------------
  CurrentElementAtBeginning => Model % CurrentElement 
  N = Model % MaxElementNodes 
  ALLOCATE(TemperateSlipCoefficient(N),&  
       PressureMeltingPoint(N),&
       STAT = istat)
  IF (istat /= 0) THEN
     CALL FATAL('iceproperties (basalSlip)','Allocations failed')
  END IF

  TemperateSlipCoefficient = 1.0D30 ! high value - > no slip by default
  PressureMeltingPoint = 273.16D00

  !-----------------------------------------------------------------
  ! get some information upon active boundary element and its parent
  !-----------------------------------------------------------------
  BoundaryElement => Model % CurrentElement
  IF ( .NOT. ASSOCIATED(BoundaryElement) ) THEN
     CALL FATAL('iceproperties (basalMelting)','No boundary element found')
  END IF
  other_body_id = BoundaryElement % BoundaryInfo % outbody
  IF (other_body_id < 1) THEN ! only one body in calculation
     ParentElement => BoundaryElement % BoundaryInfo % Right
     IF ( .NOT. ASSOCIATED(ParentElement) ) ParentElement => BoundaryElement % BoundaryInfo % Left
  ELSE ! we are dealing with a body-body boundary and asume that the normal is pointing outwards
     ParentElement => BoundaryElement % BoundaryInfo % Right
     IF (ParentElement % BodyId == other_body_id) ParentElement => BoundaryElement % BoundaryInfo % Left
  END IF
  ! just to be on the save side, check again
  IF ( .NOT. ASSOCIATED(ParentElement) ) THEN
     WRITE(Message,'(A,I10,A)')&
          'Parent Element for Boundary element no. ',&
          BoundaryElement % ElementIndex, ' not found'
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF  
  Model % CurrentElement => ParentElement
  body_id = ParentElement % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  ParentMaterial => Model % Materials(material_id) % Values
  IF ((.NOT. ASSOCIATED(ParentMaterial)) .OR. (.NOT. GotIt)) THEN
     WRITE(Message,'(A,I10,A,I10)')&
          'No material values found for body no ', body_id,&
          ' under material id ', material_id
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF
  ! number of nodes and node in elements
  NBoundary = BoundaryElement % Type % NumberOfNodes
  NParent = ParentElement % Type % NumberOfNodes
  DO BoundaryElementNode=1,Nboundary
     IF ( Node == BoundaryElement % NodeIndexes(BoundaryElementNode) ) EXIT
  END DO
  DO ParentElementNode=1,NParent
     IF ( Node == ParentElement % NodeIndexes(ParentElementNode) ) EXIT
  END DO
  !-------------------------
  ! Get Pressure Melting Point
  !-------------------------
  TempName =  GetString(ParentMaterial ,'Temperature Name', GotIt)
  PressureMeltingPoint(1:NParent) =&
       ListGetReal( ParentMaterial, TRIM(TempName) // ' Upper Limit',&
       NParent, ParentElement % NodeIndexes, GotIt)
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,a,a)') 'No entry for ', TRIM(TempName) // ' Upper Limit', ' found'
     CALL FATAL('iceproperties (basalMelting)',Message)
  END IF
  !-------------------------------------------------------------------
  ! get slip coefficient if temperature reached pressure melting point
  !-------------------------------------------------------------------
  Model % CurrentElement => BoundaryElement
  BC => GetBC()

  IF (.NOT.ASSOCIATED(BC)) THEN
     CALL FATAL('iceproperties (basalSlip)','No Boundary Condition associated')
  ELSE
     TemperateSlipCoefficient(1:NBoundary) = GetReal(BC, 'Temperate Slip Coefficient', GotIt)
     IF (.NOT. GotIt) THEN
        CALL WARN('iceproperties (basalSlip)','Keyword >Temperate Slip Coefficient< not found')
        CALL WARN('iceproperties (basalSlip)','Asuming Default 5.63D08 [kg /(s m^2)]')
        TemperateSlipCoefficient(1:NBoundary) = 5.63D08
     END IF
     ThermalCoefficient = GetConstReal(BC, 'Thermal Coefficient', GotIt)
     IF (.NOT. GotIt) THEN
        CALL WARN('iceproperties (basalSlip)','Keyword >Thermal Coefficient< not found')
        CALL WARN('iceproperties (basalSlip)','Asuming Default 1 [1/K]')
        ThermalCoefficient =  1.0D00
     END IF
  END IF
  !------------------------------
  ! check homologous temperature
  !------------------------------
  TempHom = MIN(Temperature - PressureMeltingPoint(ParentElementNode),0.0D00)
  basalSlipCoefficient = TemperateSlipCoefficient(BoundaryElementNode)*EXP(-1.0D00*TempHom*ThermalCoefficient) 
  IF (basalSlipCoefficient < TemperateSlipCoefficient(BoundaryElementNode)) &
       CALL FATAL('iceproperties (basalSlip)','Unphysical slip coefficient')
  !------------------------------
  ! clean up
  !------------------------------
  DEALLOCATE(TemperateSlipCoefficient, PressureMeltingPoint)
END FUNCTION basalSlip

!*********************************************************************************************************************************
!*
!*  projecting vertical geothermal heat flux to boundary normal
!*
!*********************************************************************************************************************************
FUNCTION getNormalFlux( Model, Node, dummyArgument ) RESULT(NormalFlux)
!-----------------------------------------------------------
  USE DefUtils
!-----------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------
  !external variables
  TYPE(Model_t), TARGET :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: dummyArgument, NormalFlux
  !internal variables
  TYPE(Element_t), POINTER :: BoundaryElement
  TYPE(Nodes_t) :: Nodes
  TYPE(ValueList_t), POINTER :: BC
  INTEGER :: N, NBoundary, BoundaryElementNode, i, DIM, body_id, NumberOfBoundaryNodes, istat
  INTEGER, POINTER :: BoundaryReorder(:)
  REAL(KIND=dp) :: U, V, W, Normal(3), Gravity(3), direction(3),&
       HeatFlux, SqrtElementMetric
  REAL(KIND=dp), ALLOCATABLE ::  ExternalHeatFlux(:)
  REAL(KIND=dp), DIMENSION(:,:),  POINTER :: Work, BoundaryNormals,BoundaryTangent1, &
       BoundaryTangent2
  LOGICAL ::  FirstTime = .TRUE., GotIt, stat
!-----------------------------------------------------------
  SAVE FirstTime, NumberOfBoundaryNodes,BoundaryReorder,BoundaryNormals, &
       BoundaryTangent1, BoundaryTangent2, DIM

  IF (FirstTime) THEN 
     DIM = CoordinateSystemDimension()
     CALL CheckNormalTangentialBoundary( Model, &
          'Basal Melting', NumberOfBoundaryNodes, &
          BoundaryReorder, BoundaryNormals, BoundaryTangent1, &
          BoundaryTangent2, DIM )

     WRITE(Message,'(A,i6)') &
          'Number of boundary nodes on boundaries associated with melting:',&
          NumberOfBoundaryNodes
     CALL INFO('iceproperties (basalMelting)','Message',Level=3)
     
     CALL AverageBoundaryNormals( Model, &
          'Basal Melting', NumberOfBoundaryNodes, &
          BoundaryReorder, BoundaryNormals, BoundaryTangent1, &
          BoundaryTangent2, DIM )
     FirstTime = .FALSE.
  END IF


  !--------------------------------
  ! Allocations
  !--------------------------------
  N = Model % MaxElementNodes 
  ALLOCATE(Nodes % x(N), Nodes % y(N), Nodes % z(N),&
       ExternalHeatFlux( N ),&
       STAT = istat)
  IF (istat /= 0) THEN
     CALL FATAL('iceproperties (normalFlux)','Allocations failed')
  END IF

  !-----------------------------------------------------------------
  ! get some information upon active boundary element and its parent
  !-----------------------------------------------------------------
  BoundaryElement => Model % CurrentElement
  IF ( .NOT. ASSOCIATED(BoundaryElement) ) THEN
     CALL FATAL('iceproperties (normalFlux)','No boundary element found')
  END IF
  !-------------------------------------------
  ! Get normal of the boundary element at node
  !-------------------------------------------
  Nboundary = BoundaryElement % Type % NumberOfNodes
  DO BoundaryElementNode=1,Nboundary
     IF ( Node == BoundaryElement % NodeIndexes(BoundaryElementNode) ) EXIT
  END DO
  Normal(1:DIM) = BoundaryNormals(BoundaryReorder(Node),1:DIM)
  !-------------------------------
  ! get gravitational acceleration
  !-------------------------------
  Work => ListGetConstRealArray( Model % Constants,'Gravity',GotIt)
  IF ( GotIt ) THEN
     Gravity = Work(1:3,1)
  ELSE
     Gravity = 0.0D00
     CALL INFO('iceproperties (normalFlux)','No vector for Gravity (Constants) found', level=1)
     IF (DIM == 1) THEN
        Gravity(1) = -1.0D00
        CALL INFO('iceproperties (normalFlux)','setting direction to -1', level=1)
     ELSE IF (DIM == 2) THEN
        Gravity    =  0.00D0
        Gravity(2) = -1.0D00
        CALL INFO('iceproperties (normalFlux)','setting direction to (0,-1)', level=1)
     ELSE
        Gravity    =  0.00D00
        Gravity(3) = -1.0D00
        CALL INFO('iceproperties (normalFlux)','setting direction to (0,0,-1)', level=1)
     END IF
  END IF
  !------------------------
  ! get external heat flux
  !------------------------
  BC => GetBC()
  ExternalHeatFlux = 0.0D00  
  ExternalHeatFlux(1:NBoundary) = GetReal(BC, 'External Heat Flux', GotIt)
  IF (.NOT. GotIt) THEN
     CALL INFO('iceproperties (normalFlux)','No external heat flux given', Level=4)
  END IF
  !--------------------------------------------------------
  ! compute normal component of vertically aligned heatflux
  !--------------------------------------------------------
  NormalFlux = ExternalHeatFlux(BoundaryElementNode) * ABS(SUM(Gravity(1:DIM)*Normal(1:DIM)))
  !----------------------------------------------
  ! clean up before leaving
  !----------------------------------------------
  DEALLOCATE( Nodes % x,&
       Nodes % y,&
       Nodes % z,&
       ExternalHeatFlux)
END FUNCTION getNormalFlux

!*********************************************************************************************************************************
!*
!* heat conductivity of ice as a function of temperature (K):  k = c_1 * exp(c_2 * T[K]); c_2 < 0 
!*
!*********************************************************************************************************************************
FUNCTION getHeatConductivity( Model, N, temperature ) RESULT(conductivity)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: N
  REAL(KIND=dp) :: temperature, conductivity
!------------ internal variables----------------------------
  TYPE(ValueList_t), POINTER :: Material
  INTEGER :: nMax,i,j,body_id,material_id,elementNodes,nodeInElement,istat
  REAL (KIND=dp), ALLOCATABLE :: conductivityExponentFactor(:), conductivityFactor(:)
  LOGICAL :: FirstTime = .TRUE., GotIt
!------------ remember this -------------------------------
  Save FirstTime, conductivityExponentFactor, conductivityFactor
  !-------------------------------------------
  ! Allocations 
  !------------------------------------------- 
  IF (FirstTime) THEN
     nMax = Model % MaxElementNodes
     ALLOCATE(conductivityExponentFactor(nMax),&
          conductivityFactor(nMax),&
          STAT=istat)
     IF ( istat /= 0 ) THEN
        CALL FATAL('iceproperties (getHeatConductivity)','Memory allocation error, Aborting.')
     END IF
     FirstTime = .FALSE.
     CALL INFO('iceproperties (getHeatConductivity)','Memory allocation done', level=3)
  END IF
  !-------------------------------------------
  ! get element properties
  !-------------------------------------------   
  IF ( .NOT. ASSOCIATED(Model % CurrentElement) ) THEN
     CALL FATAL('iceproperties (getHeatConductivity)', 'Model % CurrentElement not associated')
  END IF
  body_id = Model % CurrentElement % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  elementNodes = Model % CurrentElement % Type % NumberOfNodes
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No material id for current element of node ',n,', body ',body_id,' found'
     CALL FATAL('iceproperties (getHeatConductivity)', Message)
  END IF
  DO nodeInElement=1,elementNodes
     IF ( N == Model % CurrentElement % NodeIndexes(nodeInElement) ) EXIT
  END DO
  Material => Model % Materials(material_id) % Values
  !-------------------------------------------
  ! get material properties
  !-------------------------------------------
  conductivityExponentFactor(1:elementNodes) = ListGetReal( Material,'Conductivity Exponent Factor', elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No Conductivity Exponent Factor found in Material ', &
          material_id,' for node ', n, '.setting E=1'
     CALL FATAL('iceproperties (getHeatConductivity)', Message)
  END IF
  conductivityFactor(1:elementNodes) = ListGetReal( Material,'Conductivity Factor', elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No Conductivity Factor found in Material ', material_id,' for node ', n, '.setting E=1'
     CALL FATAL('iceproperties (getHeatConductivity)', Message)
  END IF
  !-------------------------------------------
  ! compute heat conductivity
  !-------------------------------------------
  conductivity = conductivityFactor(nodeInElement)*EXP(conductivityExponentFactor(nodeInElement)*temperature)
END FUNCTION getHeatConductivity

!*********************************************************************************************************************************
!*
!* heat capacity of ice as a function of temperature (K):  k = c_1 + c_2 * T[C];
!*
!*********************************************************************************************************************************
FUNCTION getHeatCapacity( Model, N, temperature ) RESULT(capacity)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: N
  REAL(KIND=dp) :: temperature, capacity
!------------ internal variables----------------------------
  REAL(KIND=dp) :: celsius

  !-------------------------------------------
  ! compute celsius temperature and limit it 
  ! to 0 deg
  !-------------------------------------------  
  celsius = MIN(temperature - 2.7316D02,0.0d00)
  !-------------------------------------------
  ! compute heat capacity
  !-------------------------------------------  
  capacity = 2.1275D03 + 7.253D00*celsius
END FUNCTION getHeatCapacity

!****************************************************************************************************************
!*
!* viscosity factor as a function of homologous temperature
!*
!****************************************************************************************************************
FUNCTION getViscosityFactor( Model, n, temperature ) RESULT(visFact)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
  USE DefUtils
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: n
  REAL(KIND=dp) :: temperature, visFact
!------------ internal variables----------------------------
  TYPE(ValueList_t), POINTER :: Material
  INTEGER :: DIM,nMax,i,j,body_id,material_id,elementNodes,nodeInElement,istat
  REAL(KIND=dp) ::&
       rateFactor, aToMinusOneThird, gasconst, temphom
  REAL(KIND=dp), POINTER :: Hwrk(:,:,:)
  REAL (KIND=dp), ALLOCATABLE :: activationEnergy(:,:), arrheniusFactor(:,:),&
       enhancementFactor(:), viscosityExponent(:), PressureMeltingPoint(:),&
       Ux(:), Uy(:), Uz(:)
  LOGICAL :: FirstTime = .TRUE., GotIt
  CHARACTER(LEN=MAX_NAME_LEN) :: TempName
!------------ remember this -------------------------------
  Save DIM, FirstTime, gasconst, activationEnergy, arrheniusFactor,&
       enhancementFactor, viscosityExponent, Hwrk, PressureMeltingPoint, &
       Ux, Uy, Uz
!-----------------------------------------------------------
  !-----------------------------------------------------------
  ! Read in constants from SIF file and do some allocations
  !-----------------------------------------------------------
  IF (FirstTime) THEN
     ! inquire coordinate system dimensions  and degrees of freedom from NS-Solver
     ! ---------------------------------------------------------------------------
     DIM = CoordinateSystemDimension()
     ! inquire minimum temperature
     !------------------------- 
     gasconst = ListGetConstReal( Model % Constants,'Gas Constant',GotIt)
     IF (.NOT. GotIt) THEN
        gasconst = 8.314D00 ! m-k-s
        WRITE(Message,'(a,e10.4,a)') 'No entry for Gas Constant (Constants) in input file found. Setting to ',&
             gasconst,' (J/mol)'
        CALL INFO('iceproperties (getViscosityFactor)', Message, level=4)
     END IF
     nMax = Model % MaxElementNodes
     ALLOCATE(activationEnergy(2,nMax),&
          arrheniusFactor(2,nMax),&
          enhancementFactor(nMax),&
          PressureMeltingPoint( nMax ),&
          viscosityExponent(nMax),&
          Ux(nMax),&
          Uy(nMax),&
          Uz(nMax),&
          STAT=istat)
     IF ( istat /= 0 ) THEN
        CALL Fatal('iceproperties (getViscosityFactor)','Memory allocation error, Aborting.')
     END IF
     NULLIFY( Hwrk )
     FirstTime = .FALSE.
     CALL Info('iceproperties (getViscosityFactor)','Memory allocations done', Level=3)
  END IF
  !-------------------------------------------
  ! get element properties
  !-------------------------------------------   
  body_id = Model % CurrentElement % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  IF (.NOT.GotIt) CALL FATAL('iceproperties (getViscosityFactor)','No Material ID found')
  elementNodes = Model % CurrentElement % Type % NumberOfNodes
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No material id for current element of node ',n,', body ',body_id,' found'
     CALL FATAL('iceproperties (getViscosityFactor)', Message)
  END IF
  DO nodeInElement=1,elementNodes
     IF ( N == Model % CurrentElement % NodeIndexes(nodeInElement) ) EXIT
  END DO
  Material => Model % Materials(material_id) % Values
  IF (.NOT.ASSOCIATED(Material)) THEN 
     WRITE(Message,'(a,I2,a,I2,a)') 'No Material for current element of node ',n,', body ',body_id,' found'
     CALL FATAL('iceproperties (getViscosityFactor)',Message)
  END IF
  !-------------------------------------------
  ! get material properties
  !-------------------------------------------
  ! activation energies
  !--------------------
  CALL ListGetRealArray( Material,'Activation Energies',Hwrk,elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2)') 'No Value for Activation Energy  found in Material ', material_id,' for node ', n
     CALL FATAL('iceproperties (getViscosityFactor)',Message)
  END IF
  IF ( SIZE(Hwrk,2) == 1 ) THEN
     DO i=1,MIN(3,SIZE(Hwrk,1))
        activationEnergy(i,1:elementNodes) = Hwrk(i,1,1:elementNodes)
     END DO
  ELSE
     WRITE(Message,'(a,I2,a,I2)') 'Incorrect array size for Activation Energy in Material ', material_id,' for node ', n
     CALL FATAL('iceproperties (getViscosityFactor)',Message)
  END IF
  ! Arrhenius Factors
  !------------------
  CALL ListGetRealArray( Material,'Arrhenius Factors',Hwrk,elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2)') 'No Value for Arrhenius Factors  found in Material ', material_id,' for node ', n
     CALL FATAL('iceproperties (getViscosityFactor)',Message)
  END IF
  IF ( SIZE(Hwrk,2) == 1 ) THEN
     DO i=1,MIN(3,SIZE(Hwrk,1))
        arrheniusFactor(i,1:elementNodes) = Hwrk(i,1,1:elementNodes)
     END DO
  ELSE
     WRITE(Message,'(a,I2,a,I2)') 'Incorrect array size for Arrhenius Factors in Material ', material_id,' for node ', n
     CALL FATAL('iceproperties (getViscosityFactor)',Message)
  END IF
  ! Enhancement Factor
  !-------------------
  enhancementFactor(1:elementNodes) = ListGetReal( Material,'Enhancement Factor', elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     enhancementFactor(1:elementNodes) = 1.0D00
     WRITE(Message,'(a,I2,a,I2,a)') 'No Enhancement Factor found in Material ', material_id,' for node ', n, '.setting E=1'
     CALL INFO('iceproperties (getViscosityFactor)', Message, level=9)
  END IF
  ! Viscosity Exponent
  !-------------------
  viscosityExponent(1:elementNodes) = ListGetReal( Material,'Viscosity Exponent', elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     viscosityExponent(1:elementNodes) = 1.0D00/3.0D00
     WRITE(Message,'(a,I2,a,I2,a)') 'No Viscosity Exponent found in Material ', material_id,' for node ', n, '.setting k=1/3'
     CALL INFO('iceproperties (getViscosityFactor)', Message, level=9)
  END IF
  ! Pressure Melting Point and homologous temperature
  !--------------------------------------------------
  TempName =  GetString(Material ,'Temperature Name', GotIt)
  IF (.NOT.GotIt) CALL FATAL('iceproperties (getViscosityFactor)','No Temperature Name found')
  PressureMeltingPoint(1:elementNodes) =&
       ListGetReal( Material, TRIM(TempName) // ' Upper Limit',&
       elementNodes, Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT.GotIt) THEN
     temphom = 0.0d00
     WRITE(Message,'(A,A,A,i3,A)') 'No entry for ',TRIM(TempName) // ' Upper Limit',&
          ' found in material no. ', material_id,'. Using 273.16 K.'
     CALL WARN('iceproperties (getViscosityFactor)',Message)
  ELSE
     temphom = MIN(temperature - PressureMeltingPoint(nodeInElement), 0.0d00)
  END IF
  !-------------------------------------------
  ! homologous Temperature is below 10 degrees
  !-------------------------------------------
  IF (temphom < -1.0D01) THEN
     i=1
     !-------------------------------------------
     ! homologous Temperature is above 10 degrees
     !-------------------------------------------
  ELSE
     i=2
  END IF
  rateFactor =&
       arrheniusFactor(i,nodeInElement)*exp(-1.0D00*activationEnergy(i,nodeInElement)/(gasconst*(2.7316D02 + temphom)))
  visFact = 0.5*(enhancementFactor(nodeInElement)&
       *rateFactor)**(-1.0e00*viscosityExponent(nodeInElement))
END FUNCTION getViscosityFactor

!*********************************************************************************************************************************
RECURSIVE SUBROUTINE getTotalViscosity(Model,Solver,Timestep,TransientSimulation)
  USE DefUtils
  USE Materialmodels
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t)  :: Model
  TYPE(Solver_t), TARGET :: Solver
  LOGICAL :: TransientSimulation
  REAL(KIND=dp) :: Timestep
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
  TYPE(GaussIntegrationPoints_t) :: IP
  TYPE(ValueList_t), POINTER :: Material, SolverParams
  TYPE(Variable_t), POINTER :: TotViscSol
  TYPE(Element_t), POINTER :: Element
  TYPE(Nodes_t) :: Nodes
  INTEGER :: DIM,nMax,i,p,q,t,N,body_id,material_id,nodeInElement,istat
  INTEGER, POINTER :: TotViscPerm(:)
  REAL(KIND=dp) ::  LocalDensity, LocalViscosity,effVisc,detJ,Norm
  REAL(KIND=dp), POINTER :: Hwrk(:,:,:)
  REAL(KIND=dp), POINTER :: TotVisc(:)
  REAL (KIND=dp), ALLOCATABLE ::  Ux(:), Uy(:), Uz(:), Density(:), Viscosity(:),&
       STIFF(:,:),FORCE(:),Basis(:),dBasisdx(:,:),ddBasisddx(:,:,:)
  LOGICAL :: FirstTime = .TRUE., GotIt, stat, limitation
!------------ remember this -------------------------------
  Save DIM, FirstTime, Nodes, Density, Ux, Uy, Uz, Viscosity, STIFF, FORCE, Basis,dBasisdx,ddBasisddx
  

  IF (FirstTime) THEN
     ! inquire coordinate system dimensions  and degrees of freedom from NS-Solver
     ! ---------------------------------------------------------------------------
     DIM = CoordinateSystemDimension()
     nMax = Model % MaxElementNodes 
     ALLOCATE(Ux(nMax),&
          Uy(nMax),&
          Uz(nMax),&
          Density(nMax),&
          Viscosity(nMax),&
          FORCE(nMax), &
          STIFF(nMax,nMax),&
          Nodes % x(nMax), &
          Nodes % y(nMax), &
          Nodes % z(nMax), &
          Basis(nMax),&
          dBasisdx(nMax,3),&
          ddBasisddx(nMax,3,3),&
          STAT=istat)
     IF ( istat /= 0 ) THEN
        CALL Fatal('iceproperties (getTotalViscosity)','Memory allocation error, Aborting.')
     END IF
     NULLIFY( Hwrk )
     FirstTime = .FALSE.
     CALL Info('iceproperties (getTotalViscosity)','Memory allocations done', Level=3)
  END IF

  CALL DefaultInitialize()

  DO t=1,Solver % NumberOfActiveElements 
     !-------------------------------------------
     ! get element properties
     !------------------------------------------- 
     Element => GetActiveElement(t)
     N = GetElementNOFNodes()
     body_id = Element % BodyId
     material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
     IF (.NOT. GotIt) THEN
        WRITE(Message,'(a,I2,a,I2,a)') &
             'No material id for current element of node ',n,', body ',body_id,' found'
        CALL FATAL('iceproperties (getTotalViscosity)', Message)
     
     END IF

     !-----------------------------------------
     ! get Material properties
     !-----------------------------------------
     Material => GetMaterial()
     IF (.NOT.ASSOCIATED(Material)) THEN 
        WRITE(Message,'(a,I2,a,I2,a)') &
             'No Material for current element of node ',n,', body ',body_id,' found'
        CALL FATAL('iceproperties (getTotalViscosit)',Message)
     END IF
     Density = ListGetReal( Material,'Density', N, Element % NodeIndexes, GotIt )
     IF (.NOT. GotIt) THEN
        WRITE(Message,'(a,I2,a,I2)') 'No Value for Density found in Material ', material_id,' for node ', n
        CALL FATAL('iceproperties (getTotalViscosity)',Message)
     END IF
     Viscosity = ListGetReal( Material,'Viscosity', N, Element % NodeIndexes, GotIt )
     IF (.NOT. GotIt) THEN
        WRITE(Message,'(a,I2,a,I2)') 'No Value for Viscosity found in Material ', material_id,' for node ', n
        CALL FATAL('iceproperties (getTotalViscosity)',Message)
     END IF

     !-----------------------------------------
     ! Velocity Field
     !----------------------------------------
     Ux = 0.0d00
     Uy = 0.0d00
     Uz = 0.0d00
  
     CALL GetScalarLocalSolution( Ux, 'Velocity 1')
     IF (DIM>1) THEN
        CALL GetScalarLocalSolution( Uy, 'Velocity 2')
        IF (DIM == 3) CALL GetScalarLocalSolution( Uz, 'Velocity 3')
     END IF
     

     STIFF = 0.0d00
     FORCE = 0.0d00

     IP = GaussPoints( Element )
     CALL GetElementNodes( Nodes )

     DO i=1,IP % n
        stat = ElementInfo( Element, Nodes, IP % U(i), IP % V(i), &
             IP % W(i),  detJ, Basis, dBasisdx, ddBasisddx, .FALSE. )
        !
        ! get local Material parameters at Gauss-point
        !
        LocalDensity = SUM(Density(1:n)*Basis(1:n))
        LocalViscosity = SUM(Viscosity(1:n)*Basis(1:n))
        !
        ! get effective Viscosity at Integration point
        !
        effVisc = EffectiveViscosity( LocalViscosity, LocalDensity,&
             Ux, Uy, Uz, &
             Element, Nodes, N, N,&
             IP % U(i), IP % V(i), IP % W(i))
         IF (effVisc .le. 0.0E00) THEN
           WRITE(Message,'(A,i10,A,i10,A,e13.3)')&
                'effective viscosity for Gauss point no. ', i, ' in element no. ', t,' is negative:', effVisc
           CALL WARN('iceproperties (getTotalViscosity)',Message)
        END IF
        DO p=1,n
           FORCE(p) = FORCE(p) + IP % S(i) * DetJ * effVisc * Basis(p)
           DO q=1,n
              STIFF(p,q) = STIFF(p,q) + IP % S(i) * detJ * Basis(q)*Basis(p)
           END DO
        END DO
     END DO
     CALL DefaultUpdateEquations( STIFF, FORCE )
  END DO
  CALL DefaultFinishAssembly()
!   CALL DefaultDirichletBCs()
  Norm = DefaultSolve()
  SolverParams => GetSolverParams()
  limitation = GetLogical( SolverParams,'Positive Values',GotIt )
  IF (.NOT. GotIt) limitation = .FALSE.
  IF (limitation) THEN
     CALL INFO('iceproperties (getTotalViscosity)','Results limited to positive values',Level=1)
     TotViscSol => Solver % Variable
     TotViscPerm  => TotViscSol % Perm
     TotVisc => TotViscSol % Values
     DO i= 1,Solver % Mesh % NumberOfNodes
        TotVisc(i) = MAX(TotVisc(i),0.0D00)
     END DO
  END IF
END SUBROUTINE getTotalViscosity

!*********************************************************************************************************************************
!*
!* total capacity (heat capacity * density) as a function of Kelvin temperature
!*
!*********************************************************************************************************************************
FUNCTION getCapacity( Model, N, Temp ) RESULT(capacity)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: N
  REAL(KIND=dp) :: Temp, capacity
!------------ internal variables----------------------------
  REAL(KIND=dp) :: celsius
  REAL(KIND=dp), ALLOCATABLE :: density(:)
  INTEGER :: istat, elementNodes,nodeInElement,material_id,body_id,nmax
  LOGICAL :: FirstTime=.TRUE.,GotIt
  TYPE(Element_t), POINTER :: Element
  TYPE(ValueList_t),POINTER :: Material

  SAVE FirstTime, density

  IF (FirstTime) THEN
     nMax = Model % MaxElementNodes
     ALLOCATE(density(nMax),&
          STAT=istat)
     IF ( istat /= 0 ) THEN
        CALL Fatal('iceproperties (getCapacity)','Memory allocation error, Aborting.')
     END IF
     FirstTime = .FALSE.
     CALL Info('iceproperties (getCapacity)','Memory allocations done', Level=3)
  END IF

  !-------------------------------------------
  ! get element properties
  !-------------------------------------------   
  body_id = Model % CurrentElement % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  elementNodes = Model % CurrentElement % Type % NumberOfNodes
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No material id for current element of node ',n,', body ',body_id,' found'
     CALL FATAL('iceproperties (getCapacity)', Message)
  END IF
  DO nodeInElement=1,elementNodes
     IF ( N == Model % CurrentElement % NodeIndexes(nodeInElement) ) EXIT
  END DO
  Material => Model % Materials(material_id) % Values
  !-------------------------------------------
  ! get density
  !-------------------------------------------
  density(1:elementNodes) = ListGetReal( Material,'Density', elementNodes, &
       Model % CurrentElement % NodeIndexes, GotIt )
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2)') 'No Value for Activation Energy  found in Material ', material_id,' for node ', n
     CALL FATAL('iceproperties (getCapacity)',Message)
  END IF


  !-------------------------------------------
  ! compute heat capacity
  !-------------------------------------------  
  capacity = 2.1275D03 *density(nodeInElement) + 7.253D00*celsius
END FUNCTION getCapacity

!*********************************************************************************************************************************
!*
!* viscosity as a function of the viscosity factor
!*
!*********************************************************************************************************************************
FUNCTION getCriticalShearRate( Model, n, temperature ) RESULT(critShear)
   USE types
   USE CoordinateSystems
   USE SolverUtils
   USE ElementDescription
!-----------------------------------------------------------
   IMPLICIT NONE
!------------ external variables ---------------------------
   TYPE(Model_t) :: Model
   INTEGER :: n
   REAL(KIND=dp) :: critShear, temperature
!------------ internal variables----------------------------
   TYPE(Element_t), POINTER :: Element
   TYPE(ValueList_t),POINTER :: Material
   INTEGER :: DIM, body_id, material_id
   REAL(KIND=dp) ::&
        visFact, cuttofViscosity, power, rateFactor, aToMinusOneThird,&
        activationEnergy, gasconst, enhancementFactor
   LOGICAL :: GotIt, FirstTime = .TRUE. 
!------------ remember this -------------------------------
   Save DIM, FirstTime
!-----------------------------------------------------------
! Read in constants from SIF file
!-----------------------------------------------------------
   IF (FirstTime) THEN
      ! inquire coordinate system dimensions  and degrees of freedom from NS-Solver
      ! ---------------------------------------------------------------------------
      DIM = CoordinateSystemDimension()
      gasconst = ListGetConstReal( Model % Constants,'Gas Constant',GotIt)
      IF (.NOT. GotIt) THEN
         gasconst = 8.314 ! m-k-s
         WRITE(Message,'(a,e10.4,a)') 'No entry for Gas Constant (Constants) in input file found. Setting to ',&
              gasconst,' (J/mol)'
         CALL INFO('iceproperties (getCriticalShearRate)', Message, level=9)
      END IF
      FirstTime = .FALSE.
   END IF
   ! inquire Model parameters
   !-------------------------
   Element => Model % CurrentElement
   body_id = Element % BodyId
   material_id = ListGetInteger( Model % Bodies(body_id) % Values,&
        'Material', Gotit, minv=1, maxv=Model % NumberOFMaterials)
   Material => Model % Materials(material_id) % Values
   power=  ListGetConstReal( Material, 'Viscosity Exponent', Gotit)
   IF (.NOT.Gotit) THEN
      CALL FATAL('iceproperties (getCriticalShearRate)', 'Viscosity Exponent not found')
   ELSE
      WRITE(Message,'(a,e10.4)') 'Viscosity Exponent =', power
      CALL INFO('iceproperties (getCriticalShearRate)', Message, level=9)
   END IF
   cuttofViscosity =  ListGetConstReal(Material, 'Cutoff Viscosity', Gotit)
   IF (.NOT.Gotit) THEN
      CALL FATAL('iceproperties (getCriticalShearRate)', 'Cutoff Viscosity not found')
   ELSE
      WRITE(Message,'(a,e10.4)') 'Cutoff Viscosity =', cuttofViscosity
      CALL INFO('iceproperties (getCriticalShearRate)', Message, level=9) 
   END IF

   ! get viscosity factor for local node
   ! -----------------------------------
!   visFact = ListGetReal(Material, 'Viscosity Factor',1, n, GotIt)
!   IF (.NOT.Gotit) THEN
!      WRITE(Message,'(a,i4,a)') 'Viscosity Factor for point no. ', n, ' not found' 
!      CALL FATAL('iceproperties (getCriticalShearRate)', 'Cutoff Viscosity not found'Message)
!   ELSE
!      WRITE(Message,'(a,i4,a,e10.4)') 'Viscosity Factor for point no. ',&
!           n, ' = ', visFact
!      CALL INFO('iceproperties (getCriticalShearRate)', Message, level=4) 
!   END IF

   IF (temperature < 263.15) THEN
      activationEnergy = 6.0e04 ! m-k-s
      aToMinusOneThird = 4.42577e16
      enhancementFactor = 1.0e00
   ELSE 
      activationEnergy = 1.39e05
      aToMinusOneThird = 2.62508e09
      enhancementFactor = 1.0e00
   END IF
   rateFactor = exp(-activationEnergy/(gasconst*temperature))
   visFact = 0.5*aToMinusOneThird*(enhancementFactor*rateFactor)**(-1.0e00/3.00e00)
   IF (visFact .NE. 0.0e0) THEN
      critShear = (cuttofViscosity/visFact)**(1.0e0/(1.0e0 - power))
   ELSE
      critShear = 0.0d0
   END IF
END FUNCTION getCriticalShearRate


!*********************************************************************************************************************************
!*
!* density  as a function of the position; Special and dirty workaround for crater2d_* cases -DO NOT USE ELSEWHERE!!
!*
!*********************************************************************************************************************************
FUNCTION getDensity( Model, Node, Depth ) RESULT(density)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: Depth, density
!------------ internal variables----------------------------
  TYPE(ValueList_t), POINTER :: Material
  INTEGER :: i,j
  REAL (KIND=dp) :: XCoord, Height, porosity

  INTERFACE
     FUNCTION getPorosity( Model, Node, Depth ) RESULT(porosity)
       USE types
!       REAL(KIND=dp) :: getPorosity 
       !------------ external variables ---------------------------
       TYPE(Model_t) :: Model
       INTEGER :: Node
       REAL(KIND=dp) :: Depth, porosity
     END FUNCTION getPorosity
  END INTERFACE

  porosity = getPorosity(Model, Node, Depth)
  density = 9.18D02 * porosity

END FUNCTION getDensity


!*********************************************************************************************************************************
!*
!* viscosity factor (due to porosity); Special and dirty workaround for crater2d_* cases -DO NOT USE ELSEWHERE!!
!*
!*********************************************************************************************************************************
FUNCTION getViscosity( Model, Node, Depth ) RESULT(viscosity)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: Depth, viscosity
  INTERFACE
     FUNCTION getPorosity( Model, Node, Depth ) RESULT(porosity)
       USE types
!       REAL(KIND=dp) :: getPorosity 
       !------------ external variables ---------------------------
       TYPE(Model_t) :: Model
       INTEGER :: Node
       REAL(KIND=dp) :: Depth, porosity
     END FUNCTION getPorosity
  END INTERFACE

  viscosity = getPorosity(Model, Node, Depth)
END FUNCTION getViscosity



!*********************************************************************************************************************************
!*
!* heat conductivity factor 
!*
!*********************************************************************************************************************************
FUNCTION getHeatConductivityFactor( Model, Node, Depth ) RESULT(heatCondFact)
  USE types
  USE CoordinateSystems
  USE SolverUtils
  USE ElementDescription
!-----------------------------------------------------------
  IMPLICIT NONE
!------------ external variables ---------------------------
  TYPE(Model_t) :: Model
  INTEGER :: Node
  REAL(KIND=dp) :: Depth, heatCondFact
  REAL(KIND=dp) :: porosity
  INTERFACE
     FUNCTION getPorosity( Model, Node, Depth ) RESULT(porosity)
       USE types
       !------------ external variables ---------------------------
       TYPE(Model_t), TARGET :: Model
       INTEGER :: Node
       REAL(KIND=dp) :: Depth, porosity
     END FUNCTION getPorosity
  END INTERFACE
  porosity = getPorosity(Model, Node, Depth)
  heatCondFact = 9.828e00 * porosity
END FUNCTION getHeatConductivityFactor

!*********************************************************************************************************************************
!*
!* pressure melting point
!*
!*********************************************************************************************************************************
FUNCTION getPressureMeltingPoint( Model, n, Pressure ) RESULT(PressureMeltingPoint)
   USE types
   USE CoordinateSystems
   USE SolverUtils
   USE ElementDescription
!-----------------------------------------------------------
   IMPLICIT NONE
!------------ external variables ---------------------------
   TYPE(Model_t) :: Model
   INTEGER :: n
   REAL(KIND=dp) :: PressureMeltingPoint, Pressure
!------------ internal variables----------------------------
   TYPE(Element_t), POINTER :: Element
   TYPE(ValueList_t),POINTER :: Material
   INTEGER :: body_id, material_id, nodeInElement, istat, elementNodes
   REAL(KIND=dp), ALLOCATABLE :: ClausiusClapeyron(:)
   LOGICAL :: GotIt, FirstTime = .TRUE. 
!------------ remember this -------------------------------
   Save FirstTime, ClausiusClapeyron
  !-------------------------------------------
  ! Allocations 
  !------------------------------------------- 
  IF (FirstTime) THEN
     ALLOCATE(ClausiusClapeyron(Model % MaxElementNodes),&
          STAT=istat)
     IF ( istat /= 0 ) THEN
        CALL FATAL('iceproperties (getPressureMeltingPoint)','Memory allocation error, Aborting.')
     END IF
     FirstTime = .FALSE.
     CALL INFO('iceproperties (getPressureMeltingPoint)','Memory allocation done', level=3)
  END IF
  !-------------------------------------------
  ! get element properties
  !-------------------------------------------   
  IF ( .NOT. ASSOCIATED(Model % CurrentElement) ) THEN
     CALL FATAL('iceproperties (getPressureMeltingPoint)', 'Model % CurrentElement not associated')
  ELSE
     Element => Model % CurrentElement
  END IF
  body_id = Element % BodyId
  material_id = ListGetInteger(Model % Bodies(body_id) % Values, 'Material', GotIt)
  elementNodes = Element % Type % NumberOfNodes
  IF (.NOT. GotIt) THEN
     WRITE(Message,'(a,I2,a,I2,a)') 'No material id for current element of node ',elementNodes,', body ',body_id,' found'
     CALL FATAL('iceproperties (getPressureMeltingPoint)', Message)
  END IF
  DO nodeInElement=1,elementNodes
     IF ( N == Model % CurrentElement % NodeIndexes(nodeInElement) ) EXIT
  END DO
  Material => Model % Materials(material_id) % Values
  !-------------------------
  ! get material parameters
  !-------------------------
  ClausiusClapeyron(1:elementNodes) = &
       ListGetReal( Material, 'Clausius Clapeyron', elementNodes, Element % NodeIndexes, GotIt)
  IF (.NOT. GotIt) THEN
     CALL FATAL('iceproperties (getPressureMeltingPoint)','No value for Clausius Clapeyron parameter found')
  END IF
  !-------------------------------
  ! compute pressure melting point
  !-------------------------------
  PressureMeltingPoint = 2.7316D02 - ClausiusClapeyron(nodeInElement)*MAX(Pressure,0.0d00)
END FUNCTION getPressureMeltingPoint

