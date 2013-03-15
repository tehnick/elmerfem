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
! *  Authors: 
! *  Email:   
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: 
! * 
! *****************************************************************************
!Compute the Cost function of the Adjoint inverse Problem
!      as Sum_Surface 0.5*(u - uobs)^2
!      with a regularization as Sum_bedrock 0.5 Lambda (dBeta/dx)^2
!
!   Serial/Parallel    2D/3D
!
! Need :
!   - Name of the Cost Variable
!   - Lambda and Beta for regularization
!   - define in the sif Name='surface' and Name='bed' in appropriate BC.
!
!
!
!
! *****************************************************************************
SUBROUTINE CostSolver_Adjoint( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
  USE DefUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!  
  CHARACTER(LEN=MAX_NAME_LEN), PARAMETER :: DefaultCostFile = 'CostOfT.dat'
  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName,CostFile
  CHARACTER(LEN=MAX_NAME_LEN) :: BCName,CostSolName,VarSolName
  TYPE(Element_t),POINTER ::  Element
  TYPE(Variable_t), POINTER :: TimeVar,CostVar,BetaSol
  TYPE(ValueList_t), POINTER :: BC,SolverParams
  TYPE(Nodes_t) :: ElementNodes
  TYPE(GaussIntegrationPoints_t) :: IntegStuff
  REAL(KIND=dp), POINTER :: Beta(:)
  INTEGER, POINTER :: NodeIndexes(:), BetaPerm(:)
  Logical :: Firsttime=.true.,Found,Parallel,stat,Gotit
  integer :: i,j,k,l,t,n,NMAX,DIM,ierr
  real(kind=dp) :: Cost,Cost_bed,Cost_surf,Cost_S,Cost_bed_S,Cost_surf_S,Lambda
  real(kind=dp) :: Bu,Bv,u,v,w,s,coeff,SqrtElementMetric,x
  REAL(KIND=dp) :: NodeCost(Model % MaxElementNodes),Basis(Model % MaxElementNodes), dBasisdx(Model % MaxElementNodes,3)
  CHARACTER*10 :: date,temps

  save Firsttime,Parallel 
  save SolverName,CostSolName,VarSolName,Lambda,CostFile
  save DIM,ElementNodes

  If (Firsttime) then

!!!!!!! Check for parallel run 
    Parallel = .FALSE.
    IF ( ASSOCIATED( Solver % Matrix % ParMatrix ) ) THEN
            IF ( Solver %  Matrix % ParMatrix % ParEnv % PEs > 1 )  THEN
                    Parallel = .TRUE.
            END IF
    END IF

     DIM = CoordinateSystemDimension()
     WRITE(SolverName, '(A)') 'CostSolver_Adjoint'

!!!!!!!!!!! get Solver Variables
  SolverParams => GetSolverParams()

  CostFile = ListGetString(Solver % Values,'Cost Filename',Found )
    IF (.NOT. Found) CostFile = DefaultCostFile
    CALL DATE_AND_TIME(date,temps)
    If (Parallel) then
        if (ParEnv % MyPe.EQ.0) then
           OPEN (12, FILE=CostFile)
                    write(12,'(a1,a2,a1,a2,a1,a4,5x,a2,a1,a2,a1,a2)') '#',date(5:6),'/',date(7:8),'/',date(1:4), &
                                 temps(1:2),':',temps(3:4),':',temps(5:6)
           CLOSE(12)
         End if
    Else
           OPEN (12, FILE=CostFile)
                    write(12,'(a1,a2,a1,a2,a1,a4,5x,a2,a1,a2,a1,a2)') '#',date(5:6),'/',date(7:8),'/',date(1:4), &
                                 temps(1:2),':',temps(3:4),':',temps(5:6)
           CLOSE(12)
    End if
    

  VarSolName =  GetString( SolverParams,'Optimized Variable Name', Found)
      IF(.NOT.Found) THEN
              CALL WARN(SolverName,'Keyword >Optimized Variable Name< not found in section >Solver<')
              CALL WARN(SolverName,'Taking default value >Beta<')
              WRITE(VarSolName,'(A)') 'Beta'
      END IF

   CostSolName =  GetString( SolverParams,'Cost Variable Name', Found)
          IF(.NOT.Found) THEN
                    CALL WARN(SolverName,'Keyword >Cost Variable Name< not found  in section >Solver<')
                    CALL WARN(SolverName,'Taking default value >CostValue<')
                    WRITE(CostSolName,'(A)') 'CostValue'
          END IF

   Lambda =  GetConstReal( SolverParams,'Lambda', Found)
   IF(.NOT.Found) THEN
           CALL WARN(SolverName,'Keyword >Lambda< not found  in section >Solver<')
           CALL WARN(SolverName,'Taking default value Lambda=0.0')
           Lambda = 0.0
   End if

  
  !!! End of First visit
    Firsttime=.false.
  Endif

    BetaSol => VariableGet( Solver % Mesh % Variables, VarSolName  )
    IF ( ASSOCIATED( BetaSol ) ) THEN
            Beta => BetaSol % Values
            BetaPerm => BetaSol % Perm
    ELSE
            WRITE(Message,'(A,A,A)') &
                               'No variable >',VarSolName,' < found'
            CALL FATAL(SolverName,Message)
    END IF            

    Cost=0._dp
    Cost_surf=0._dp
    Cost_bed=0._dp
    DO t=1,Solver % Mesh % NumberOfBoundaryElements

      Element => GetBoundaryElement(t)

      BC => GetBC()
      IF ( .NOT. ASSOCIATED(BC) ) CYCLE

      BCName =  ListGetString( BC,'Name', Found)
      IF((BCName /= 'surface').AND.(BCName /= 'bed')) CYCLE

      CALL GetElementNodes( ElementNodes )
      n = GetElementNOFNodes()
      NodeIndexes => Element % NodeIndexes

      NodeCost=0.0_dp
      IF (BCName == 'surface') THEN
          NodeCost(1:n) = ListGetReal(BC, 'Adjoint Cost', n, NodeIndexes, GotIt)
          !PRINT *,NodeCost(1:n)
          IF (.NOT.GotIt) Then
                  WRITE(Message,'(A)') &
                     'No variable >Adjoint Cost< found in "surface" BC'
                  CALL FATAL(SolverName,Message)
          END IF 
      Else IF (BCName == 'bed') Then
          NodeCost(1:n)=Beta(BetaPerm(NodeIndexes(1:n)))
      End if

!------------------------------------------------------------------------------
!    Numerical integration
!------------------------------------------------------------------------------
        IntegStuff = GaussPoints( Element )

        DO i=1,IntegStuff % n
          U = IntegStuff % u(i)
          V = IntegStuff % v(i)
          W = IntegStuff % w(i)
!------------------------------------------------------------------------------
!        Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
          stat = ElementInfo( Element,ElementNodes,U,V,W,SqrtElementMetric, &
              Basis,dBasisdx )

          x = SUM( ElementNodes % x(1:n) * Basis(1:n) )
          s = 1.0d0

            IF ( CurrentCoordinateSystem() /= Cartesian ) THEN
              s = 2.0d0 * PI * x 
            END IF
            s = s * SqrtElementMetric * IntegStuff % s(i)
          
           IF (BCName == 'surface') Then   
            coeff = SUM(NodeCost(1:n)  * Basis(1:n))
            Cost_surf=Cost_surf+coeff*s
           else IF (BCName == 'bed') Then
            coeff = SUM(NodeCost(1:n) * dBasisdx(1:n,1))
            coeff =  coeff * coeff
            IF (DIM.eq.3) then
                    coeff=coeff+ & 
                    SUM(NodeCost(1:n)*dBasisdx(1:n,2))*SUM(NodeCost(1:n) * dBasisdx(1:n,2))
            END IF
            Cost_bed=Cost_bed+coeff*s
           else 
            coeff = 0.0
           End if

            !Cost=Cost+coeff*s
            !PRINT *,Solver % Matrix % ParMatrix % ParEnv % MyPE,Cost
        End do
    End do

   Cost=Cost_surf+0.5*Lambda*Cost_bed

    TimeVar => VariableGet( Solver % Mesh % Variables, 'Time' )

    IF (Parallel) THEN
           CALL MPI_ALLREDUCE(Cost,Cost_S,1,&
                  MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)  
           CALL MPI_ALLREDUCE(Cost_surf,Cost_surf_S,1,&
                  MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)  
           CALL MPI_ALLREDUCE(Cost_bed,Cost_bed_S,1,&
                  MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)  
          CostVar => VariableGet( Solver % Mesh % Variables, CostSolName )
          IF (ASSOCIATED(CostVar)) THEN
                 CostVar % Values(1)=Cost_S
          END IF
         IF (Solver % Matrix % ParMatrix % ParEnv % MyPE == 0) then
                 OPEN (12, FILE=CostFile,POSITION='APPEND')
                 write(12,'(e13.5,2x,e15.8,2x,e15.8,2x,e15.8)') TimeVar % Values(1),Cost_S,Cost_surf_S,Cost_bed_S
                 CLOSE(12)
         End if
   ELSE
            CostVar => VariableGet( Solver % Mesh % Variables, CostSolName )
            IF (ASSOCIATED(CostVar)) THEN
                    CostVar % Values(1)=Cost
            END IF
                    OPEN (12, FILE=CostFile,POSITION='APPEND')
                       write(12,'(e13.5,2x,e15.8,2x,e15.8,2x,e15.8)') TimeVar % Values(1),Cost,Cost_surf,Cost_bed
                    close(12)
   END IF
   
   Return
!------------------------------------------------------------------------------
END SUBROUTINE CostSolver_Adjoint
!------------------------------------------------------------------------------
! *****************************************************************************
