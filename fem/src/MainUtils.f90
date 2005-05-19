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
! * Utility routines for the elmer main program.
! *
! ******************************************************************************
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
! *                       Date: 08 Jun 1997
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! * $Log: MainUtils.f90,v $
! * Revision 1.3  2005/05/17 07:28:05  jpr
! * *** empty log message ***
! *
! * Revision 1.2  2005/05/04 09:16:04  vierinen
! * minor modifications
! *
! * Revision 1.157  2005/04/14 11:36:15  jpr
! * Removed the Sparse library interface.
! *
! * Revision 1.156  2005/04/04 06:40:54  jpr
! * *** empty log message ***
! *
! *
! * Revision 1.148  2004/03/03 10:22:37  jpr
! * Corrected bug in radiation factors&matrix structured introduced
! * by previous changes.
! *
! * Revision 1.147  2004/03/03 09:40:36  jpr
! * Changed strategy of allocation of solver primary variables somewhat.
! *
! * 
! * $Id: MainUtils.f90,v 1.3 2005/05/17 07:28:05 jpr Exp $
! *****************************************************************************/


MODULE MainUtils

!------------------------------------------------------------------------------

  USE SolverUtils
  USE ModelDescription

!------------------------------------------------------------------------------
  IMPLICIT NONE
!------------------------------------------------------------------------------

  EXTERNAL FlowSolver, HeatSolver, MagneticSolver, StressSolver, MeshSolver
 INTEGER :: FlowSolver, HeatSolver, MagneticSolver, StressSolver, MeshSolver


CONTAINS

!------------------------------------------------------------------------------
  FUNCTION GetMatrixFormat( Model, Equation ) RESULT(FORMAT)
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    CHARACTER(LEN=*) :: Equation
!------------------------------------------------------------------------------
    INTEGER :: FORMAT

    INTEGER :: i
    LOGICAL :: GotIt
    CHARACTER(LEN=MAX_NAME_LEN) :: str
!------------------------------------------------------------------------------
!
!
    FORMAT = MATRIX_CRS

    DO i=1,Model % NumberOfSolvers
       IF (  &
            ListGetString(Model % Solvers(i) % Values, 'Equation') == Equation &
            ) THEN

          str = ListGetString( Model % Solvers(i) % Values, & 
                  'Linear System Solver', GotIt )
          IF ( .NOT. GotIt ) str = 'direct'

          IF ( TRIM(str) == 'direct' ) THEN

             str = ListGetString(Model % Solvers(i) % Values, &
                  'Linear System Direct Method', GotIt )

             IF ( .NOT. GotIt ) THEN
                FORMAT = MATRIX_CRS
             ELSE
                SELECT CASE( TRIM(str) )
                CASE( 'banded', 'symmetric banded' )
                   FORMAT = MATRIX_CRS

                CASE( 'umfpack' )
#ifdef HAVE_UMFPACK
                   FORMAT = MATRIX_CRS
#else
                   CALL Fatal( 'GetMatrixFormat', 'UMPACK solver has not been installed.' )
#endif

                CASE DEFAULT
                  
                   CALL Warn( 'GetMatrixFormat', 'Unknown direct solver method: ' // TRIM(str) )
                   CALL Warn( 'GetMatrixFormat', 'band solver (LAPACK) will be used.' )
                   FORMAT = MATRIX_CRS
 
                END SELECT
             END IF

          ELSE IF ( TRIM(str) == 'iterative' ) THEN

             FORMAT = MATRIX_CRS

          ELSE IF ( TRIM(str) == 'multigrid' ) THEN

             FORMAT = MATRIX_CRS

          ELSE

             CALL Fatal( 'GetMatrixFormat', 'Unknown linear system solver: ' // TRIM(str) )
             STOP

          END IF

          EXIT
       END IF
    END DO
!------------------------------------------------------------------------------
  END FUNCTION GetMatrixFormat
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE AddSolverProcedure( Solver,PROCEDURE  )
!------------------------------------------------------------------------------
    TYPE(Solver_t) :: Solver
    EXTERNAL :: PROCEDURE
    INTEGER  :: PROCEDURE
!------------------------------------------------------------------------------
    INTEGER(KIND=AddrInt) :: AddrFunc
!------------------------------------------------------------------------------
    Solver % PROCEDURE = AddrFunc( PROCEDURE )
!------------------------------------------------------------------------------
  END SUBROUTINE AddSolverProcedure
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE AddEquation( Solver, Name,Transient )
DLLEXPORT AddEquation
!------------------------------------------------------------------------------
    TYPE(Solver_t), POINTER :: Solver
    LOGICAL :: Transient
    CHARACTER(LEN=*) :: Name
!------------------------------------------------------------------------------
    REAL(KIND=dp), POINTER :: Solution(:), Component(:)

    INTEGER, POINTER :: Perm(:)

    INTEGER :: i,j,k,NDeg,Nrows,nSize,n,DOFs,MatrixFormat,istat
    INTEGER :: MaxDGDOFs, MaxNDOFs, MaxEDOFs, MaxFDOFs, MaxBDOFs

    LOGICAL :: GotIt, BandwidthOptimize, EigAnal, ComplexFlag, &
        MultigridActive, VariableOutput, GlobalBubbles

    CHARACTER(LEN=MAX_NAME_LEN) :: str,eq,var_name, tmpname

    TYPE(Mesh_t),   POINTER :: NewMesh,OldMesh
    TYPE(Matrix_t), POINTER :: NewMatrix, Oldmatrix
    TYPE(Element_t), POINTER :: CurrentElement

    TYPE(Variable_t), POINTER :: Var
    TYPE(Variable_t), POINTER :: NewVariable

    REAL(KIND=dp) :: tt, CPUTime
!------------------------------------------------------------------------------
    MaxNDOFs  = 0
    MaxDGDOFs = 0
    DO i=1,Solver % Mesh % NumberOFBulkElements
       CurrentElement => Solver % Mesh % Elements(i)
       MaxDGDOFs = MAX( MaxDGDOFs, CurrentElement % DGDOFs )
       MaxNDOFs  = MAX( MaxNDOFs,  CurrentElement % NDOFs )
    END DO
   
    MaxEDOFs = 0
    DO i=1,Solver % Mesh % NumberOFEdges 
       CurrentElement => Solver % Mesh % Edges(i)
       MaxEDOFs  = MAX( MaxEDOFs,  CurrentElement % BDOFs )
    END DO

    MaxFDOFs = 0
    DO i=1,Solver % Mesh % NumberOFFaces 
       CurrentElement => Solver % Mesh % Faces(i)
       MaxFDOFs  = MAX( MaxFDOFs,  CurrentElement % BDOFs )
    END DO

    MaxBDOFs = 0
    DO i=1,Solver % Mesh % NumberOFBulkElements
       CurrentElement => Solver % Mesh % Elements(i)
       MaxBDOFs  = MAX( MaxFDOFs,  CurrentElement % BDOFs )
    END DO

   GlobalBubbles = ListGetLogical( Solver % Values, 'Global Bubbles', GotIt )

    Ndeg = 0
    Ndeg = Ndeg + Solver % Mesh % NumberOfNodes 
    IF ( MaxEDOFs > 0 ) Ndeg = Ndeg + MaxEDOFs * Solver % Mesh % NumberOFEdges
    IF ( MaxFDOFs > 0 ) Ndeg = Ndeg + MaxFDOFs * Solver % Mesh % NumberOFFaces
    IF ( GlobalBubbles ) &
                 Ndeg = Ndeg + MaxBDOFs * Solver % Mesh % NumberOfBulkElements
    Ndeg = MAX( NDeg, MaxDGDOFs * Solver % Mesh % NumberOfBulkElements )

    Solver % Order = 1

    IF ( Transient ) THEN
       str = ListGetString( Solver % Values, 'Timestepping Method',GotIt )
       IF ( .NOT. GotIt ) THEN
          str = ListGetString( CurrentModel % Simulation, 'Timestepping Method',GotIt )
          IF ( GotIt ) THEN
             CALL ListAddString( Solver % Values, 'Timestepping Method', str )
          END IF
       END IF

       IF ( GotIt ) THEN
          IF (str=='bdf') THEN
             Solver % Order = ListGetInteger( Solver % Values, &
                   'BDF Order', GotIt, minv=1, maxv=5 )
             IF ( .NOT. GotIt ) THEN
                Solver % Order = ListGetInteger( CurrentModel % &
                   Simulation, 'BDF Order', GotIt, minv=1, maxv=5 )
             END IF
             IF ( .NOT.GotIt ) THEN
                Solver % Order = 2
                CALL Warn( 'AddEquation', 'BDF order defaulted to 2.' )
             END IF
          ELSE IF ( str=='runge-kutta') THEN
             Solver % Order = ListGetInteger( CurrentModel % &
                   Simulation, 'Runge-Kutta Order', GotIt, minv=2, maxv=4 )
             IF ( .NOT.GotIt ) Solver % Order = 2
          END IF
       ELSE
          CALL Warn( 'AddEquation', 'Time stepping method defaulted to IMPLICIT EULER' )
          CALL ListAddString( Solver % Values, 'Timestepping Method', 'Implicit Euler' )
       END IF
    END IF

    Solver % TimeOrder = 1
!------------------------------------------------------------------------------

    DOFs = CoordinateSystemDimension()
    IF ( CurrentCoordinateSystem() == CylindricSymmetric ) DOFs = DOFs + 1

    MatrixFormat = GetMatrixFormat( CurrentModel, Name )

    BandwidthOptimize = ListGetLogical( Solver % Values, &
           'Optimize Bandwidth', GotIt )
    IF ( .NOT. GotIt ) BandwidthOptimize = .TRUE.

    VariableOutput = ListGetLogical( Solver % Values, 'Variable Output', gotit )
    IF ( .NOT. Gotit ) VariableOutput = .TRUE.

    var_name = ListGetString( Solver % Values, 'Variable', gotit )
    IF ( gotIt ) THEN
       IF ( var_name(1:9) == '-nooutput' ) THEN
          VariableOutput = .FALSE.
          var_name(1:LEN(var_name)-10) = var_name(11:)
       END IF
    END IF

!------------------------------------------------------------------------------
    SELECT CASE( Name )       
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
    CASE('navier-stokes')
!------------------------------------------------------------------------------
       Solver % Variable => VariableGet(Solver % Mesh % Variables, 'Flow Solution')

       IF ( ASSOCIATED( Solver % Variable ) ) THEN
          Perm => Solver % Variable % Perm
       ELSE
         ALLOCATE( Perm(Ndeg), STAT=istat )
         IF ( istat /= 0 ) THEN
            CALL Fatal( 'AddEquation', 'Memory allocation error.' )
         END IF
       END IF

       Solver % Matrix => CreateMatrix( CurrentModel, Solver % Mesh, Perm, &
            DOFs+1, MatrixFormat, BandwidthOptimize, 'Navier-Stokes', GlobalBubbles=GlobalBubbles )

       Nrows =(DOFs+1)* Ndeg
       IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows
       IF ( .NOT. ASSOCIATED( Solver % Variable ) ) THEN
          ALLOCATE( Solution(Nrows), STAT=istat )

          ! First add components to the variable list separately (must be done
          ! this way for the output routines to work properly...):
          !---------------------------------------------------------------------
          Component => Solution( 1 : Nrows: DOFs+1 )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,'Velocity 1', &
               1, Component, Perm, Output=VariableOutput )

          Component => Solution( 2 : Nrows: DOFs+1 )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,'Velocity 2', &
               1, Component, Perm, Output=VariableOutput )
  
          IF ( DOFs+1 == 3 ) THEN
             Component => Solution( 3 : Nrows: DOFs+1 )
             CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,'Pressure', &
                  1, Component, Perm, Output=VariableOutput )
          ELSE
             Component => Solution( 3 : Nrows: DOFs+1 )
             CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,'Velocity 3', &
                  1, Component, Perm, Output=VariableOutput )

             Component => Solution( 4 : Nrows : DOFs+1 )
             CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,'Pressure', &
                  1, Component, Perm, Output=VariableOutput )
          END IF

          ! Then add the thing itself:
          !---------------------------
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Flow Solution',DOFs+1,Solution,Perm, Output=VariableOutput )
 
          Solver % Variable => VariableGet( Solver % Mesh % Variables, 'Flow Solution' )
          Solution = 1.0d-6
       END IF
       CALL AddSolverProcedure( Solver, FlowSolver )
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    CASE('magnetic induction')
!------------------------------------------------------------------------------
       Solver % Variable => VariableGet(Solver % Mesh % Variables, 'Magnetic Field')

       IF ( ASSOCIATED( Solver % Variable ) ) THEN
          Perm => Solver % Variable % Perm
       ELSE
         ALLOCATE( Perm(NDeg),STAT=istat )
         IF ( istat /= 0 ) THEN
            CALL Fatal( 'AddEquation', 'Elmer Solver: Memory allocation error.' )
         END IF
       END IF

       Solver % Matrix => CreateMatrix( CurrentModel, Solver % Mesh, &
            Perm,3, MatrixFormat, BandwidthOptimize, 'Magnetic Induction', GlobalBubbles=GlobalBubbles )

       Nrows = 3*Ndeg
       IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows
       IF ( .NOT. ASSOCIATED( Solver % Variable ) ) THEN
          ALLOCATE( Solution(Nrows), STAT=istat )

          Component => Solution( 1 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver, &
               'Magnetic Field 1', 1, Component, Perm,Output=VariableOutput )

          Component => Solution( 2 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver, &
               'Magnetic Field 2', 1, Component, Perm,Output=VariableOutput )

          Component => Solution( 3 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver, &
               'Magnetic Field 3', 1, Component, Perm,Output=VariableOutput )
!------------------------------------------------------------------------------
!         Then add the thing itself
!------------------------------------------------------------------------------
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Magnetic Field',3,Solution,Perm,Output=VariableOutput )

          Solver % Variable => VariableGet( Solver % Mesh % Variables, &
               'Magnetic Field' )

          Solution = 0.0d0
!------------------------------------------------------------------------------
!         Add first components to the variable list separately...
!------------------------------------------------------------------------------
          ALLOCATE( Solution(Nrows), STAT=istat )

          Component => Solution( 1 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Electric Current 1', 1, Component, Perm,Output=VariableOutput )

          Component => Solution( 2 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Electric Current 2', 1, Component, Perm,Output=VariableOutput )

          Component => Solution( 3 : Nrows : 3 )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Electric Current 3', 1, Component, Perm,Output=VariableOutput )
!------------------------------------------------------------------------------
!         Then add the thing itself
!------------------------------------------------------------------------------
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,&
               'Electric Current', 3, Solution, Perm, Output=VariableOutput )
          Solution = 0.0d0
       END IF
       CALL AddSolverProcedure( Solver, MagneticSolver )
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    CASE('stress analysis')
!------------------------------------------------------------------------------
       IF ( CurrentCoordinateSystem() == CylindricSymmetric ) DOFs = 2

       Solver % Variable => VariableGet(Solver % Mesh % Variables, 'Displacement')
       IF ( ASSOCIATED( Solver % Variable ) ) THEN
          Perm => Solver % Variable % Perm
       ELSE
          ALLOCATE( Perm(NDeg),STAT=istat )
          IF ( istat /= 0 ) THEN
             CALL Fatal( 'AddEquation', 'Memory allocation error.' )
          END IF
       END IF

       Solver % Matrix => CreateMatrix( CurrentModel, Solver % Mesh, &
            Perm, DOFs, MatrixFormat, BandwidthOptimize, 'Stress Analysis', GlobalBubbles=GlobalBubbles )

       Nrows = DOFs*Ndeg
       IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows
       IF ( .NOT.ASSOCIATED( Solver % Variable ) ) THEN
          ALLOCATE( Solution(Nrows), STAT=istat )

          ! First add the components to the variable list separately:
          !----------------------------------------------------------
          Component => Solution( 1 : Nrows : DOFs )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Displacement 1',1, Component, Perm,Output=VariableOutput )

          Component => Solution( 2 : Nrows : DOFs )
          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver, &
               'Displacement 2', 1, Component, Perm,Output=VariableOutput )

          IF ( CoordinateSystemDimension() >= 3 ) THEN
             Component => Solution( 3 : Nrows : DOFs )
             CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver,  &
                  'Displacement 3', 1, Component, Perm, Output=VariableOutput )
          END IF

          ! Then add the thing itself:
          !---------------------------
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,&
               'Displacement',DOFs,Solution,Perm,Output=VariableOutput )

          Solver % Variable => VariableGet( Solver % Mesh % Variables, &
               'Displacement' )

          Solution = 0.0D0
       END IF
       CALL AddSolverProcedure( Solver, StressSolver )
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    CASE('mesh update')
!------------------------------------------------------------------------------
       IF ( CurrentCoordinateSystem() == CylindricSymmetric ) DOFs = 2

       Solver % Variable => VariableGet(Solver % Mesh % Variables, 'Mesh Update')
       IF ( ASSOCIATED( Solver % Variable ) ) THEN
          Perm => Solver % Variable % Perm
       ELSE
          ALLOCATE( Perm(Ndeg), STAT=istat )
          IF ( istat /= 0 ) THEN
             CALL Fatal( 'AddEquation', 'Memory allocation error.' )
          END IF
       END IF

       Solver % Matrix => CreateMatrix( CurrentModel, Solver % Mesh, &
            Perm, DOFs, MatrixFormat, BandwidthOptimize, 'Mesh Update', GlobalBubbles=GlobalBubbles )
       IF ( .NOT. ASSOCIATED( Solver % Matrix ) )  RETURN
 
       Nrows = DOFs*Ndeg
       IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows
       IF (.NOT. ASSOCIATED( Solver % Variable ) ) THEN
          ALLOCATE( Solution(Nrows), STAT=istat )

          ! First add the components to the variable list separately:
          !----------------------------------------------------------
          Component => Solution( 1 : Nrows : DOFs )
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver, &
               'Mesh Update 1',1, Component, Perm,Output=VariableOutput )

          Component => Solution( 2 : Nrows : DOFs )
          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver, &
               'Mesh Update 2', 1, Component, Perm,Output=VariableOutput )

          IF ( CoordinateSystemDimension() >= 3 ) THEN
             Component => Solution( 3 : Nrows : DOFs )
             CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh,Solver,  &
                  'Mesh Update 3', 1, Component, Perm,Output=VariableOutput )
          END IF

          !      Then add the thing itself...
          !------------------------------------------------------------------------------
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,&
             'Mesh Update',DOFs,Solution,Perm,Output=VariableOutput )
          Solution = 0.0d0
          Solver % Variable => VariableGet( Solver % Mesh % Variables, 'Mesh Update' )
       END IF

       IF ( .NOT. ASSOCIATED( VariableGet( Solver % Mesh % Variables, 'Mesh Velocity') ) ) THEN
          IF ( Transient ) THEN
             ALLOCATE( Solution(Nrows), STAT=istat )
             IF ( istat /= 0 ) THEN
                CALL Fatal( 'AddEquation', 'Memory allocation error.' )
             END IF

             Component => Solution( 1 : Nrows : DOFs )
             CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh, &
                  Solver, 'Mesh Velocity 1',1, Component, Perm,Output=VariableOutput )

             Component => Solution( 2 : Nrows : DOFs )
             CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh, &
                  Solver, 'Mesh Velocity 2', 1, Component, Perm,Output=VariableOutput )

             IF ( CoordinateSystemDimension() >= 3 ) THEN
                Component => Solution( 3 : Nrows : DOFs )
                CALL VariableAdd(Solver % Mesh % Variables,Solver % Mesh, &
                     Solver, 'Mesh Velocity 3', 1, Component, Perm,Output=VariableOutput )
             END IF

             CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh,Solver,&
                  'Mesh Velocity',DOFs,Solution,Perm,Output=VariableOutput )

             Solution = 0.0D0
          END IF
       END IF

       CALL AddSolverProcedure( Solver, MeshSolver )
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    CASE('heat equation')
!------------------------------------------------------------------------------
       Solver % Variable => VariableGet( Solver % Mesh % Variables, 'Temperature' )

       IF ( ASSOCIATED( Solver % Variable ) ) THEN
          Perm => Solver % Variable % Perm
       ELSE
          ALLOCATE( Perm(Ndeg),STAT=istat )
       END IF

       eq = ListGetString( CurrentModel % Simulation, 'Gebhardt Factors',GotIt )
       IF ( GotIt ) THEN
         CALL LoadGebhardtFactors( Solver % Mesh,eq )
       ELSE
         CALL RadiationFactors( Solver, .TRUE.)
       END IF

       Solver % Matrix => CreateMatrix( CurrentModel,Solver % Mesh, &
            Perm,1, MatrixFormat, BandwidthOptimize, 'Heat Equation', GlobalBubbles=GlobalBubbles)

       Nrows = Ndeg
       IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows
       IF ( .NOT. ASSOCIATED( Solver % Variable ) ) THEN
          ALLOCATE( Solution(Nrows),STAT=istat )
          IF ( istat /= 0 ) THEN
             CALL Fatal( 'AddEquation', 'Memory allocation error.' )
          END IF

          CALL VariableAdd( Solver % Mesh % Variables,Solver % Mesh, Solver,&
               'Temperature',1, Solution, Perm, Output=VariableOutput )

          Solver % Variable => VariableGet( Solver % Mesh % Variables, 'Temperature' )
          Solution = 0.0d0
       END IF
       CALL AddSolverProcedure( Solver, HeatSolver )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
    CASE DEFAULT
!------------------------------------------------------------------------------
       NULLIFY( Solver % Matrix )
       NULLIFY( Solver % Variable )
       Solver % TimeOrder = 0

       eq = ListGetString( Solver  % Values, 'Equation', GotIt )

       IF ( GotIt ) THEN
          DOFs = ListGetInteger( Solver % Values, 'Variable DOFs', GotIt, minv=1 )
          IF ( .NOT.GotIt ) DOFs = 1
          var_name = ListGetString( Solver % Values, 'Variable', gotit )

          IF ( Ndeg >= 1 .AND. gotit ) THEN
             VariableOutput = ListGetLogical( Solver % Values, 'Variable Output', gotit )
             IF ( .NOT. Gotit ) VariableOutput = .TRUE.

             DO WHILE( var_name(1:1) == '-' )
                IF ( var_name(1:10) == '-nooutput ' ) THEN
                   VariableOutput = .FALSE.
                   var_name(1:LEN(var_name)-10) = var_name(11:)
                END IF

                IF ( var_name(1:6) == '-dofs ' ) THEN
                   READ( var_name(7:), * ) DOFs
                   i = 7
                   j = LEN_TRIM( var_name )
                   DO WHILE( var_name(i:i) /= ' '  )
                      i = i + 1
                      IF ( i > j ) EXIT
                   END DO
                   var_name(1:LEN(var_name)-i) = var_name(i+1:)
                END IF
             END DO

             n = LEN_TRIM(var_name)
             Solver % Variable => VariableGet( Solver % Mesh % Variables,var_name(1:n) )
             IF ( ASSOCIATED( Solver % Variable ) ) THEN
                Perm => Solver % Variable % Perm
             ELSE
                ALLOCATE( Perm(Ndeg) )
             END IF

             Solver % Matrix => CreateMatrix( CurrentModel, Solver % Mesh, &
                  Perm, DOFs, MatrixFormat, BandwidthOptimize, eq(1:LEN_TRIM(eq)), &
                  ListGetLogical( Solver % Values, 'Discontinuous Galerkin', GotIt ), GlobalBubbles=GlobalBubbles )

             Nrows = DOFs * Ndeg
             IF ( ASSOCIATED( Solver % Matrix ) ) Nrows = Solver % Matrix % NumberOfRows

             IF ( .NOT. ASSOCIATED( Solver % Variable ) ) THEN
                ALLOCATE( Solution( Nrows ) )
                Solution = 0.0d0

                CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, Solver,&
                   var_name(1:n), DOFs, Solution, Perm, Output=VariableOutput )

                Solver % Variable => VariableGet( Solver % Mesh % Variables, var_name(1:n) )

                IF ( DOFs > 1 ) THEN
                   DO i=1,DOFs
                      tmpname = ComponentName( var_name(1:n), i )
                      Component => Solution( i:Nrows-DOFs+i:DOFs )
                      CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, Solver,&
                          tmpname, 1, Component, Perm, Output=VariableOutput )
                   END DO
                END IF
             END IF
          ELSE
             ALLOCATE( Solver % Variable )
             Solver % Variable % Norm = 0.0d0
             NULLIFY( Solver % Variable % Values )
          END IF
       ELSE
          ALLOCATE( Solver % Variable )
          Solver % Variable % Norm = 0.0d0
          NULLIFY( Solver % Variable % Values )
       END IF


       i = 1
       DO WHILE( .TRUE. )
          str = ComponentName( 'exported variable', i )
          var_name = ListGetString( Solver % Values, str, gotit )

          IF ( gotit ) THEN
             str = TRIM( ComponentName( 'exported variable', i ) ) // ' Output'
             VariableOutput = ListGetLogical( Solver % Values, str, gotit )
             IF ( .NOT. Gotit ) VariableOutput = .TRUE.

             str = TRIM( ComponentName( 'exported variable', i ) ) // ' DOFs'
             DOFs = ListGetInteger( Solver % Values, str, gotit )
             IF ( .NOT. gotit ) DOFs = 1

             VariableOutput = .TRUE.
             DO WHILE( var_name(1:1) == '-' )
                IF ( var_name(1:10) == '-nooutput ' ) THEN
                   VariableOutput = .FALSE.
                   var_name(1:LEN(var_name)-10) = var_name(11:)
                END IF

                IF ( var_name(1:6) == '-dofs ' ) THEN
                   READ( var_name(7:), * ) DOFs 
                   i = 7
                   j = LEN_TRIM( var_name )
                   DO WHILE( var_name(i:i) /= ' '  )
                      i = i + 1
                      IF ( i > j ) EXIT
                   END DO
                   var_name(1:LEN(var_name)-(i+2)) = var_name(i+1:)
                END IF
             END DO

             NewVariable => VariableGet( Solver % Mesh % Variables, TRIM(Var_name) )

             IF ( .NOT. ASSOCIATED( NewVariable ) ) THEN
               nSize = Nrows * DOFs / Solver % Variable % DOFs
               ALLOCATE( Solution(nSize) )
               Solution = 0.0d0
               Perm => Solver % Variable % Perm

               CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, Solver,&
                 TRIM(var_name), DOFs, Solution, Solver % Variable % Perm,Output=VariableOutput )

               IF ( DOFs > 1 ) THEN
                  n = LEN_TRIM( var_name )
                  DO j=1,DOFs
                     tmpname = ComponentName( var_name(1:n), j )
                     Component => Solution( j:nSize-DOFs+j:DOFs )
                     CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, Solver,&
                            tmpname, 1, Component, Perm, Output=VariableOutput )
                  END DO
               END IF
             END IF
          ELSE
             EXIT
          END IF
          i = i + 1
       END DO

       str = ListGetString( Solver  % Values, 'Procedure' )
       Solver % PROCEDURE = GetProcAddr( str )
!------------------------------------------------------------------------------
    END SELECT
!------------------------------------------------------------------------------

    Solver % NOFEigenValues = 0
!   NULLIFY( Solver % Variable % PrevValues )
!   NULLIFY( Solver % Variable % EigenValues )
!   NULLIFY( Solver % Variable % EigenVectors )

    Solver % DoneTime  = 0
    Solver % MultiGridLevel = 1
    Solver % MultiGridTotal = 0
    Solver % MultiGridSolver = .FALSE.
    Solver % MultiGridEqualSplit = .FALSE.
!------------------------------------------------------------------------------
!
!   Check for special solvers, to be executed only
!   at a certain points in simulation:
!   ----------------------------------------------

    Solver % SolverExecWhen = SOLVER_EXEC_ALWAYS

    SELECT CASE( ListGetString( Solver % Values, 'Exec Solver', GotIt )  )
      CASE( 'never' )
         Solver % SolverExecWhen = SOLVER_EXEC_NEVER
      CASE( 'always' )
         Solver % SolverExecWhen = SOLVER_EXEC_ALWAYS
      CASE( 'after simulation', 'after all' )
         Solver % SolverExecWhen = SOLVER_EXEC_AFTER_ALL
      CASE( 'before simulation', 'before all' )
         Solver % SolverExecWhen = SOLVER_EXEC_AHEAD_ALL
      CASE( 'before timestep' )
         Solver % SolverExecWhen = SOLVER_EXEC_AHEAD_TIME
      CASE( 'after timestep' )
         Solver % SolverExecWhen = SOLVER_EXEC_AFTER_TIME
      CASE DEFAULT
         Solver % SolverExecWhen = SOLVER_EXEC_ALWAYS
    END SELECT

    IF ( ListGetLogical( Solver % Values, 'Before All', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AHEAD_ALL
    ELSE IF ( ListGetLogical( Solver % Values, 'Before Simulation', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AHEAD_ALL
    ELSE IF ( ListGetLogical( Solver % Values, 'After All', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AFTER_ALL
    ELSE IF ( ListGetLogical( Solver % Values, 'After Simulation', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AFTER_ALL
    ELSE IF ( ListGetLogical( Solver % Values, 'Before Timestep', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AHEAD_TIME
    ELSE IF ( ListGetLogical( Solver % Values, 'After Timestep', GotIt ) ) THEN
       Solver % SolverExecWhen = SOLVER_EXEC_AFTER_TIME
    END IF

    Solver % LinAfterProc  = 0
    Solver % LinBeforeProc = 0
    str = ListGetString( Solver  % Values, 'Before Linsolve', GotIt )
    IF ( GotIt ) Solver % LinBeforeProc = GetProcAddr( str )

    str = ListGetString( Solver  % Values, 'After Linsolve', GotIt )
    IF ( GotIt ) Solver % LinAfterProc = GetProcAddr( str )

!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Variable % Values ) ) RETURN
!------------------------------------------------------------------------------

    IF ( ASSOCIATED( Solver % Matrix ) ) THEN
       ALLOCATE( Solver % Matrix % RHS(Solver % Matrix % NumberOFRows) )
       Solver % Matrix % RHS = 0.0d0
    END IF
!------------------------------------------------------------------------------

    EigAnal = ListGetLogical( Solver % Values, 'Eigen Analysis', GotIt )

    IF ( Transient .AND. .NOT. EigAnal ) THEN
       k = ListGetInteger( Solver % Values, 'Time Derivative Order', GotIt, &
                  minv=0, maxv=2 )
       Solver % TimeOrder = 1
       IF ( GotIt ) Solver % TimeOrder = MIN(MAX(1,k),2)

       IF ( ASSOCIATED( Solver % Matrix ) ) THEN
          ALLOCATE( Solver % Matrix % Force(Solver % Matrix % NumberOFRows, &
                          Solver % TimeOrder+1) )
          Solver % Matrix % Force = 0.0d0

          ALLOCATE(Solver % Matrix % MassValues(Solver % Matrix % NumberOfRows))
          Solver % Matrix % MassValues(:) = 1.0d0
       END IF

       IF ( .NOT. ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
          IF ( Solver % TimeOrder == 2 ) THEN
             ALLOCATE( Solver % Variable % PrevValues( &
              SIZE(Solver % Variable % Values),5) )
          ELSE IF ( Solver % Order > Solver % TimeOrder ) THEN
             ALLOCATE(Solver % Variable % PrevValues( &
                   SIZE(Solver % Variable % Values), Solver % Order))
          ELSE
             ALLOCATE(Solver % Variable % PrevValues( &
                   SIZE(Solver % Variable % Values), Solver % TimeOrder))
          END IF
          Solver % Variable % PrevValues = 0.0d0

          IF ( Solver % Variable % DOFs > 1 ) THEN
             IF ( Solver % Variable % Name == 'flow solution' ) THEN
               DO k=1,Solver % Variable % DOFs-1
                  str = 'Velocity ' // CHAR(k+ICHAR('0'))
                  Var => VariableGet( Solver % Mesh % Variables, str, .TRUE. )
                  Var % PrevValues =>  &
                      Solver % Variable % PrevValues(k::Solver % Variable % DOFs,:)
               END DO
               Var => VariableGet( Solver % Mesh % Variables, 'Pressure', .TRUE. )
               Var % PrevValues =>  &
                     Solver % Variable % PrevValues(k::Solver % Variable % DOFs,:)
             ELSE
               DO k=1,Solver % Variable % DOFs
                  str = ComponentName( Solver % Variable % Name, k )
                  Var => VariableGet( Solver % Mesh % Variables, str, .TRUE. )
                  Var % PrevValues =>  &
                      Solver % Variable % PrevValues(k::Solver % Variable % DOFs,:)
               END DO
             END IF
          END IF
       END IF
    ELSE
       Solver % TimeOrder = 0
       IF ( EigAnal ) THEN
          ComplexFlag = ListGetLogical( Solver % Values,  'Eigen System Complex', GotIt )
          IF ( .NOT. GotIt ) ComplexFlag = .FALSE.

          n = ListGetInteger( Solver % Values,  'Eigen System Values', GotIt )
          IF ( GotIt .AND. n > 0 ) THEN
             Solver % NOFEigenValues = n
             IF ( .NOT. ASSOCIATED( Solver % Variable % EigenValues ) ) THEN
               ALLOCATE( Solver % Variable % EigenValues(n) )
               ALLOCATE( Solver % Variable % EigenVectors(n, &
                    SIZE( Solver % Variable % Values ) ) )

               Solver % Variable % EigenValues  = 0.0d0
               Solver % Variable % EigenVectors = 0.0d0

               DO k=1,Solver % Variable % DOFs
                 str = ComponentName( Solver % Variable % Name, k )
                 Var => VariableGet( Solver % Mesh % Variables, str, .TRUE. )
                 IF ( ASSOCIATED( Var ) ) THEN
                    Var % EigenValues => Solver % Variable % EigenValues
                    IF ( ComplexFlag ) THEN
                       Var % EigenVectors => Solver % Variable % EigenVectors
                    ELSE
                       Var % EigenVectors =>  & 
                          Solver % Variable % EigenVectors(:,k::Solver % Variable % DOFs )
                    END IF
                 END IF
               END DO
             END IF
             ALLOCATE( Solver % Matrix % MassValues(SIZE(Solver % Matrix % Values)) )
             Solver % Matrix % MassValues = 0.0d0
          END IF
       END IF
    END IF

!------------------------------------------------------------------------------

    IF ( ASSOCIATED( Solver % Matrix ) ) THEN
       Solver % Matrix % Symmetric = ListGetLogical( Solver % Values, &
                  'Linear System Symmetric', GotIt )

       Solver % Matrix % Lumped = ListGetLogical( Solver % Values, &
                  'Lumped Mass Matrix', GotIt )

       MultigridActive = &
         ListGetString( Solver % Values, 'Linear System Solver', gotIt ) == 'multigrid' .OR. &
         ListGetString( Solver % Values, 'Linear System Preconditioning', gotIt ) == 'multigrid'


!      Check for multigrid solver:
!      ---------------------------
       IF ( MultigridActive ) THEN

          Solver % MultiGridTotal = ListGetInteger( Solver % Values, &
                        'MG Levels', GotIt, minv=1 )

          IF ( .NOT. GotIt ) THEN
             Solver % MultiGridTotal = ListGetInteger( Solver % Values, &
                     'Multigrid Levels', GotIt, minv=1 )
          END IF
          IF ( .NOT. GotIt ) Solver % MultiGridTotal = 1
!
!         Check if h/2 splitting of mesh requested:
!         ------------------------------------------
          Solver % MultiGridEqualSplit = ListGetLogical( &
             Solver % Values, 'MG Equal Split', GotIt )

          IF ( Solver % MultiGridEqualSplit ) THEN
             CALL ParallelInitMatrix( Solver, Solver % Matrix )
             Solver % MultiGridLevel = 1
             DO WHILE( Solver % MultiGridLevel < Solver % MultiGridTotal )
                IF ( ASSOCIATED( Solver % Mesh % Child ) ) THEN
                   NewMesh => Solver % Mesh % Child

                   OldMesh   => Solver % Mesh
                   OldMatrix => Solver % Matrix

                   CALL UpdateSolverMesh( Solver, NewMesh )
                   Solver % Mesh % Changed = .FALSE.
                ELSE
                   NewMesh => SplitMeshEqual( Solver % Mesh )
                   NewMesh % Next => CurrentModel % Meshes
                   CurrentModel % Meshes => NewMesh

                   OldMesh   => Solver % Mesh
                   OldMatrix => Solver % Matrix

                   CALL UpdateSolverMesh( Solver, NewMesh )
                   Solver % Mesh % Changed = .FALSE.

                   NewMesh % Parent => OldMesh
                   OldMesh % Child  => NewMesh
                   NewMesh % Name = OldMesh % Name
                END IF

                NewMatrix => Solver % Matrix
                NewMatrix % Parent => OldMatrix
                OldMatrix % Child  => NewMatrix

                CALL ParallelInitMatrix( Solver, Solver % Matrix )
                Solver % MultiGridLevel = Solver % MultiGridLevel + 1
             END DO
          END IF
          CALL MeshStabParams( Solver % Mesh )
          Solver % MultiGridSolver = ListGetString(Solver % Values, &
             'Linear System Solver', GotIt ) == 'multigrid'
          Solver % MultiGridLevel  = Solver % MultiGridTotal
       END IF
    END IF
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE AddEquation
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE SolveEquations( Model, dt, TransientSimulation, &
      CoupledMinIter, CoupledMaxIter, SteadyStateReached )
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: dt
    INTEGER :: CoupledMinIter, CoupledMaxIter
    LOGICAL :: TransientSimulation, SteadyStateReached
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: PrevNorm, RelativeChange, Tolerance, PrevDT = 0.0d0
    INTEGER :: i,j,k,n
    LOGICAL :: gotit, AbsNorm, Convergence, RungeKutta
    LOGICAL, ALLOCATABLE :: DoneThis(:)
    TYPE(Solver_t), POINTER :: Solver
    TYPE(Mesh_t),   POINTER :: Mesh
    CHARACTER(LEN=max_name_len) :: When
    REAL(KIND=dp), ALLOCATABLE :: k1(:), k2(:), k3(:), k4(:)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!   Intialize equation solvers for new timestep
!------------------------------------------------------------------------------
    IF ( TransientSimulation ) THEN
       DO k=1,Model % NumberOfSolvers
          Solver => Model % Solvers(k)
          CALL InitializeTimestep( Solver )
       END DO
    END IF
!------------------------------------------------------------------------------

    DO k=1,Model % NumberOfSolvers
      Solver => Model % Solvers(k)
      when  = ListGetString( Solver % Values, 'Exec Solver', GotIt )
      IF ( GotIt ) THEN
         IF ( When == 'before timestep' ) THEN
           CALL SolverActivate( Model,Solver,dt,TransientSimulation )
         END IF
      ELSE
         IF ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_TIME ) THEN
            CALL SolverActivate( Model,Solver,dt,TransientSimulation )
         END IF
      END IF
    END DO

!------------------------------------------------------------------------------

    ALLOCATE( DoneThis( Model % NumberOfSolvers ) )

    IF ( PrevDT == 0.0d0 ) PrevDT = dt
!------------------------------------------------------------------------------
    DO i=1,CoupledMaxIter
       IF ( TransientSimulation ) THEN
          CALL Info( 'SolveEquations', '-------------------------------------', Level=3 )
          WRITE( Message, * ) 'Coupled system iteration: ', i
          CALL Info( 'SolveEquations', Message, Level=3 )
          CALL Info( 'SolveEquations', '-------------------------------------', Level=3 )
       END IF

       DoneThis = .FALSE.

!      Initialize the mesh output flag to FALSE here, reactivated
!      later for meshes connected to active solvers.
!      ----------------------------------------------------------
       Mesh => Model % Meshes
       DO WHILE( ASSOCIATED( Mesh ) )
          Mesh % OutputActive = .FALSE.
          Mesh => Mesh % Next
       END DO
!------------------------------------------------------------------------------
!      Go trough number of solvers (heat,laminar or turbulent flow, etc...)
!------------------------------------------------------------------------------
       DO k=1,Model % NumberOfSolvers
!------------------------------------------------------------------------------
          Solver => Model % Solvers(k)

          When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
          IF ( GotIt ) THEN
             IF ( When /= 'always' ) THEN
                DoneThis(k) = .TRUE.
                CYCLE
             END IF
          ELSE
            IF ( Solver % SolverExecWhen /= SOLVER_EXEC_ALWAYS ) THEN
               DoneThis(k) = .TRUE.
               CYCLE
            END IF
          END IF
!------------------------------------------------------------------------------

          RungeKutta = .FALSE.
          IF ( TransientSimulation .AND. Solver % TimeOrder == 1 ) THEN
             RungeKutta = ListGetString( Solver % Values, &
                    'Timestepping Method', GotIt ) == 'runge-kutta'
          END IF

          PrevNorm = Solver % Variable % Norm
          IF ( RungeKutta ) THEN
            n = SIZE(Solver % Variable % Values)

            SELECT CASE( Solver % Order )
            CASE(2)
              ALLOCATE( k1(n), k2(n) )

              k1 = Solver % Variable % PrevValues(:,1)-Solver % Variable % PrevValues(:,2)
              k1 = dt * k1 / PrevDT
              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + k1
              CALL SolverActivate( Model,Solver,dt,TransientSimulation )
              k2 = Solver % Variable % Values - Solver % Variable % PrevValues(:,1)

              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + &
                                  ( k1 + k2 ) / 2
              DEALLOCATE( k1, k2 )


            CASE DEFAULT
              ALLOCATE( k1(n), k2(n), k3(n), k4(n) )

              k1 = Solver % Variable % PrevValues(:,1)-Solver % Variable % PrevValues(:,2)
              k1 = dt * k1 / PrevDT
              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + k1/2
              CALL SolverActivate( Model,Solver,dt/2,TransientSimulation )
              k2 = 2*(Solver % Variable % Values - Solver % Variable % PrevValues(:,1))

              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + k2
              CALL SolverActivate( Model,Solver,dt/2,TransientSimulation )
              k3 = 2*(Solver % Variable % Values - Solver % Variable % PrevValues(:,1))

              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + k3
              CALL SolverActivate( Model,Solver,dt,TransientSimulation )
              k4 = Solver % Variable % Values - Solver % Variable % PrevValues(:,1)

              Solver % Variable % Values = Solver % Variable % PrevValues(:,1) + &
                                ( k1 + 2*k2 + 2*k3 + k4 ) / 6
              DEALLOCATE( k1, k2, k3, k4 )
            END SELECT
            Solver %  Variable % Norm = &
                    SQRT( SUM( Solver % Variable % Values(1:n)**2 ) / n )
          ELSE
             CALL SolverActivate( Model,Solver,dt,TransientSimulation )
          END IF
!------------------------------------------------------------------------------
!         check for coupled system convergence
!------------------------------------------------------------------------------
          IF ( i >= CoupledMinIter .OR. .NOT. TransientSimulation ) THEN
             IF ( i /= CoupledMaxIter .OR..NOT. TransientSimulation ) THEN
                IF ( PrevNorm + Solver % Variable % Norm == 0.0d0 ) THEN
                   RelativeChange = 0.0d0
                ELSE
                   RelativeChange = 2.0d0*ABS(PrevNorm-Solver % Variable % Norm)/&
                            ( PrevNorm + Solver % Variable % Norm )
                END IF

                WRITE( Message, '(a,g20.12,g20.12,a)') &
                  '(NRM,RELC): (',Solver % Variable % Norm, RelativeChange, &
                      '  ) :: ' // TRIM(ListGetString( Solver % Values, 'Equation'))
                CALL Info( 'SolveEquations', Message, Level=3 )

                absnorm = ListGetLogical( Solver % Values, 'Use Absolute Norm for Convergence', GotIt )
                IF ( .NOT. Gotit ) absnorm = .FALSE.

                Tolerance = ListGetConstReal( Solver % Values,   &
                              'Steady State Convergence Tolerance', gotIt ) 

                Convergence = .FALSE.
                Convergence = Convergence .OR. (RelativeChange < Tolerance) .AND. .NOT. Absnorm
                Convergence = Convergence .OR. (Solver % Variable % Norm < Tolerance) .AND. Absnorm

                IF ( GotIt .AND. .NOT. Convergence ) THEN
                   DoneThis(k) = .FALSE.
                ELSE
                   DoneThis(k) = .TRUE.
                   IF( ALL(DoneThis) ) EXIT
                END IF
             END IF
          END IF
!------------------------------------------------------------------------------
       END DO
!------------------------------------------------------------------------------
       Model % Mesh % Changed = .FALSE.
       IF ( ALL(DoneThis) ) EXIT
    END DO
    PrevDT = dt

    IF ( TransientSimulation .AND. .NOT. ALL(DoneThis) ) THEN
       IF ( ListGetLogical( Model % Simulation,  &
               'Coupled System Abort Not Converged', gotIt ) ) THEN
          CALL Error( 'SolveEquations', ' ' )
          WRITE( Message, * ) 'Coupled system iteration: ', i
          CALL Error( 'SolveEquations', Message )
          CALL Fatal( 'SolveEquations', ' ' )
       ELSE
!         CALL Error( 'SolveEquations', ' ' )
!         WRITE( Message, * ) 'Coupled system iteration: ', i
!         CALL Error( 'SolveEquations', Message )
!         CALL Error( 'SolveEquations', ' ' )
       END IF
    END IF

!------------------------------------------------------------------------------

    DO k=1,Model % NumberOfSolvers
      Solver => Model % Solvers(k)
      When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
      IF (  GotIt ) THEN
         IF ( When == 'after timestep' ) THEN
             CALL SolverActivate( Model,Solver,dt,TransientSimulation )
         END IF
      ELSE
         IF ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_TIME ) &
            CALL SolverActivate( Model,Solver,dt,TransientSimulation )
      END IF
    END DO

!------------------------------------------------------------------------------
    IF ( .NOT.TransientSimulation ) SteadyStateReached = ALL(DoneThis)
!------------------------------------------------------------------------------
    DEALLOCATE( DoneThis )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE SolveEquations
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE SolverActivate( Model, Solver, dt, TransientSimulation )
!------------------------------------------------------------------------------
     TYPE(Model_t)  :: Model
     TYPE(Solver_t),POINTER :: Solver
     REAL(KIND=dp) :: dt
     LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
     INTEGER :: i, j, SolverAddr, BDOFs
     LOGICAL :: stat, Found, GB
     TYPE(Element_t), POINTER :: CurrentElement
     CHARACTER(LEN=MAX_NAME_LEN) :: EquationName, str
!------------------------------------------------------------------------------
     CALL SetCurrentMesh( Model, Solver % Mesh )
     Model % Solver => Solver

     IF ( Solver % Mesh % Changed .OR. Solver % NumberOfActiveElements <= 0 ) THEN
        Solver % NumberOFActiveElements = 0
        EquationName = ListGetString( Solver % Values, 'Equation', stat )

        IF ( Stat ) THEN
           IF (  ASSOCIATED( Solver % ActiveElements ) ) DEALLOCATE( Solver % ActiveElements )

           ALLOCATE( Solver % ActiveElements( Solver % Mesh % NumberOfBulkElements + &
                        Solver % Mesh % NumberOFBoundaryElements ) )

           GB = ListGetLogical( Solver % Values,'Global Bubbles', Found )
           BDOFs  = 0
           DO i=1,Solver % Mesh % NumberOfBulkElements+Solver % Mesh % NumberOFBoundaryElements
              CurrentElement => Solver % Mesh % Elements(i)
              IF ( CheckElementEquation( Model, CurrentElement, EquationName ) ) THEN
                 Solver % NumberOfActiveElements = Solver % NumberOFActiveElements + 1
                 Solver % ActiveElements( Solver % NumberOFActiveElements ) = i

                 IF ( .NOT. GB .AND. CurrentElement % BDOFs > 0 ) THEN
                    ALLOCATE(CurrentElement % BubbleIndexes(CurrentElement % BDOFs))
                    DO j=1,CurrentElement %  BDOFs
                       BDOFs = BDOFs + 1
                       CurrentElement % BubbleIndexes(j) = BDOFs
                    END DO
                 END IF
              END IF
           END DO

           IF ( BDOFs>0 ) THEN
             ALLOCATE( Solver % Variable % Pvalues(BDOFs*Solver % Variable % DOFs))
             Solver % Variable % PValues = 0.0d0
           END IF
        END IF

     END IF
!------------------------------------------------------------------------------
     Solver % Mesh % OutputActive = .TRUE.
     Solver % dt = dt

#ifdef SGIn32
     SolverAddr = Solver % PROCEDURE
     CALL ExecSolver( SolverAddr, Model, Solver, dt, TransientSimulation )
#else
     CALL ExecSolver( &
              Solver % PROCEDURE, Model, Solver, dt, TransientSimulation )
#endif
!------------------------------------------------------------------------------
   END SUBROUTINE SolverActivate
!------------------------------------------------------------------------------

END MODULE MainUtils
