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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Interpolate scattered 2D data readed from an ASCII file (x y Value)
!    in the mesh nodes using:
!     - nn-c libary (http://code.google.com/p/nn-c/): linear and Natural
!               Neighbours interpolation
!     - csa-c libary (http://code.google.com/p/csa-c): cubic spline approximation
!
! TODO: - add possibility to prescibe std for csa method
!       - netcdf reader ??
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE Scattered2DDataInterpolator( Model,Solver,dt,TransientSimulation )

        USE DefUtils
        USE NearestNeighbour

        IMPLICIT NONE

        TYPE(Solver_t), TARGET :: Solver
        TYPE(Model_t) :: Model
        REAL(KIND=dp) :: dt
        LOGICAL :: TransientSimulation

        TYPE(ValueList_t), POINTER :: Params
        TYPE(Variable_t), POINTER :: Var
        REAL(KIND=dp), POINTER :: Values(:)
        INTEGER, POINTER :: Perm(:)

        REAL(KIND=DP) :: Win,NaNVal
        REAL(KIND=DP) :: x,y,z,MinD,D

        INTEGER,parameter :: io=20
        INTEGER :: ok,nNaN
        INTEGER :: i,k,kmin,NoVar
        INTEGER :: nppcin,csakin

        CHARACTER(LEN=MAX_NAME_LEN) :: VariableName,DataF
        CHARACTER(LEN=MAX_NAME_LEN) :: Name,FName,WName,MName,CSVName
        CHARACTER(LEN=MAX_NAME_LEN),parameter :: &
                         SolverName='Scattered2DDataInterpolator'
        CHARACTER(2) :: method

        LOGICAL :: GotVar,Found
        LOGICAL :: CheckNaN,ReplaceNaN

        ! Variables to pass to the nn C library
        type(POINT),dimension(:),allocatable :: pout,pin
        INTEGER(C_INT) :: nout,nin,nppc,csak
        REAL(C_DOUBLE) :: W

       CALL INFO(Trim(SolverName), &
           '-----Initialise Variables using nn Library----------',Level=5)

       Params => GetSolverParams()

       CheckNaN = ListGetLogical(Params, 'Look For NaN',Found)
       If(.NOT.Found) CheckNaN=.True.
       NaNVal = ListGetConstReal( Params, 'Replace NaN by', ReplaceNaN )

       ! Mesh nodes coordinates for the NN iterpolation
       nout=Model % Mesh % NumberOfNodes
       allocate(pout(nout))

        Do i=1,Model % Mesh % NumberOfNodes
           pout(i)%x = Model % Mesh % Nodes % x(i)
           pout(i)%y = Model % Mesh % Nodes % y(i)
        End Do

       ! Read variable to initialize and Data
        NoVar=0
        GotVar=.True.

        DO WHILE(GotVar)
            NoVar = NoVar + 1
            WRITE (Name,'(A,I0)') 'Variable ',NoVar

            VariableName = ListGetString( Params, TRIM(Name), GotVar )
            IF (.NOT.GotVar) exit

            Var => VariableGet(Model %  Mesh % Variables, VariableName )
            IF(.NOT.ASSOCIATED(Var)) Then
                
            ELSE
                Values => Var % Values
                Perm => Var % Perm
            END IF

            WRITE (FName,'(A,I0,A)') 'Variable ',NoVar,' Data File'
            DataF = ListGetString( Params, TRIM(FName), Found )

            IF (.NOT.Found) then
               write(message,'(A,A,A)') &
                        'Keyword <',Trim(Fname),'> not found'
               CALL Fatal(Trim(SolverName),Trim(message))
            END IF

            WRITE (WName,'(A,I0,A)') 'Variable ',NoVar,' W'
            Win = ListGetConstReal( Params, TRIM(WName), Found )
            if (.NOT.Found) then
                W=-HUGE(0.0d0)
            else
                W=Win
            endif

            WRITE (MName,'(A,I0,A)') 'Variable ',NoVar,' method'
            method = ListGetString( Params, TRIM(MName), Found )
            if (.NOT.Found) then
                method='n'
            endif
            
            
            open(unit = io, file = TRIM(DataF), status = 'old',iostat = ok)

            if(ok /= 0) then
               write(message,'(A,A)') 'Unable to open file ',TRIM(DataF)
               CALL Fatal(Trim(SolverName),Trim(message))
            end if
            
            nin=0
            !count the line number in the file
            do while(ok == 0) 
              read(io,*,iostat = ok)
              if (ok == 0) nin = nin + 1
            end do 
          
            allocate(pin(nin))

            ! comes back to begining of file
            rewind(unit=io,iostat=ok)

            ! read datas
            do i = 1, nin
                read(io,*,iostat = ok) pin(i)%x,pin(i)%y,pin(i)%z
            end do
            close(io)

            ! call the nn C library
            SELECT CASE (method(1:1))
                  CASE ('c')
                   WRITE (CSVName,'(A,I0,A)') 'Variable ',NoVar,' nppc'
                   nppcin = ListGetInteger( Params, TRIM(CSVName), Found )
                   if (.NOT.Found) then  
                           nppc=-1
                   else
                           nppc=nppcin
                   endif
                   WRITE (CSVName,'(A,I0,A)') 'Variable ',NoVar,' k'
                   csakin = ListGetInteger( Params, TRIM(CSVName), Found )
                   if (.NOT.Found) then  
                           csak=-1
                   else
                           csak=csakin
                   endif
                       
                       write(message,'(A,A,A)') 'Initialise ',&
                                                  Trim(Variablename),&
                                     ' using cubic spline interpolation'
                        CALL INFO(Trim(SolverName),Trim(message),Level=5)
                        call csa_interpolate_points(nin, pin, nout, pout, nppc ,csak)
                  CASE ('l')
                      write(message,'(A,A,A)') 'Initialise ',&
                                                  Trim(Variablename),&
                                    ' using Linear interpolation'
                       CALL INFO(Trim(SolverName),Trim(message),Level=5)
                      call lpi_interpolate_points(nin, pin, nout, pout)
                  CASE DEFAULT
                    SELECT CASE (method(2:2))
                        CASE ('s')
                         nn_rule=1
                         write(message,'(A,A,A)') 'Initialise ',&
                                                  Trim(Variablename),&
                 ' using Natural Neighbours Non-Sibsonian interpolation'
                        CASE DEFAULT
                         write(message,'(A,A,A)') 'Initialise ',&
                                                  Trim(Variablename),&
                       ' using Natural Neighbours Sibson interpolation'
                     END SELECT
                   CALL INFO(Trim(SolverName),Trim(message), Level=5)
                   call nnpi_interpolate_points(nin,pin,w,nout,pout)
           END SELECT
            
            
            !update variable value
            nNaN=0
            Do i=1,nout
               z=pout(i)%z
               If (CheckNaN) Then
                  If (isnan(z)) Then
                     nNaN=nNaN+1
                     If (ReplaceNaN) then
                         z=NaNVal
                     Else
                         x=pout(i)%x
                         y=pout(i)%y
                         kmin=1
                         MinD=sqrt((x-pin(1)%x)*(x-pin(1)%x)+ &
                                (y-pin(1)%y)*(y-pin(1)%y))
                         Do k=2,nin
                            D=sqrt((x-pin(k)%x)*(x-pin(k)%x)+ &
                               (y-pin(k)%y)*(y-pin(k)%y))
                          If (D.LT.MinD) then
                           kmin=k
                           MinD=D
                          End if
                         End Do
                         z=pin(kmin)%z
                      End IF
                   End IF
               End IF
               Values(Perm(i))=z
            End do
            If (nNaN.GT.0) then
                If (ReplaceNaN) then
                   write(message,'(I0,A,A,e14.7)') nNaN, &
                                 ' values where NaN and have', &
                                 ' been replaced by ',NaNVal
                Else
                   write(message,'(I0,A,A)') nNaN, &
                                 ' values where NaN and have', &
                                 ' been replaced by nearest data value'
                EndIf
                CALL INFO(Trim(SolverName),Trim(message), Level=5)
             End If
           

            deallocate(pin)
        END DO

        deallocate(pout)

       CALL INFO(Trim(SolverName), &
           '-----ALL DONE----------',Level=5)

        END SUBROUTINE Scattered2DDataInterpolator
