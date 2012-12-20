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
! *  Authors: Olivier Gagliardini
! *  Email:   
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: 
! * 
! *****************************************************************************
!>  Create a Synthetic 3D mesh starting from a 
!>  1m thick mesh using the right contour of the final mesh
!>  and bedrock and surface elevations given as functions
!>  Work for partitioned mesh
!>  number of partitions in mesh_partition.in
!> 
!>  Need to be compiled with fbed(x,y) 
!>            and fsurf(x,y) 
PROGRAM  MshGlacierSynthetic 
USE types
!
IMPLICIT NONE 
!
INTEGER :: NtN
!
REAL(KIND=dp)  ::   znew, zb, zs
REAL(KIND=dp) :: fbed, fsurf
REAL(KIND=dp), ALLOCATABLE :: x(:), y(:), z(:)
INTEGER, ALLOCATABLE :: Node(:)
Character :: Rien*1
Character(len=10) :: iNp, iNN
CHARACTER :: NameMsh*30
Integer i, j, k, N, Np
Logical :: Serial = .False.

!
! Read the name mesh and number of partitions
!
      OPEN(10,file="mesh_info.in")
        READ(10,*)Rien
        READ(10,*)NameMsh 
        READ(10,*)Rien
        READ(10,*)Np 
      CLOSE(10)
      
      IF (Np==1) Serial=.True.

      IF (.NOT.Serial) THEN
        IF (Np < 10) THEN
          WRITE(iNp,'(i1.1)')Np
        ELSE IF (Np < 100) THEN
          WRITE(iNp,'(i2.2)')Np
        ELSE IF (Np < 1000) THEN
          WRITE(iNp,'(i3.3)')Np
        ELSE IF (Np < 10000) THEN
          WRITE(iNp,'(i4.4)')Np
        ELSE
          WRITE(*,*)'Work for a number of partitions < 1000'
          STOP
        END IF
        iNp = ADJUSTL(iNp)
      END IF

      DO k = 1, Np
        write(*,*)k,' of ', Np

        IF (.NOT.Serial) THEN
          IF (k < 10) THEN
            WRITE(iNN,'(i1.1)')k
          ELSE IF (k < 100) THEN
            WRITE(iNN,'(i2.2)')k 
          ELSE IF (k < 1000) THEN
            WRITE(iNN,'(i3.3)')k 
          ELSE IF (k < 10000) THEN
            WRITE(iNN,'(i4.4)')k 
          END IF
          iNN = ADJUSTL(iNN)
          write(*,*)'iNN',iNN
 
          OPEN(11,file=TRIM(NameMsh)//"/partitioning."//TRIM(iNp)//"/part."//TRIM(iNN)//".header")
        ELSE
          OPEN(11,file=TRIM(NameMsh)//"/mesh.header")
        END IF 

        READ(11,*)NtN
        CLOSE(11)

        ALLOCATE (Node(NtN), x(NtN), y(NtN), z(NtN))
        write(*,*)'Part ', k, ' NtN = ', NtN
        
        IF (.NOT.Serial) THEN
          OPEN(12,file=TRIM(NameMsh)//"/partitioning."//TRIM(iNp)//"/part."//TRIM(iNN)//".nodes")
        ELSE
          OPEN(12,file=TRIM(NameMsh)//"/mesh.nodes")
        END IF
          DO i=1,NtN
            READ(12,*)Node(i),j,x(i),y(i),z(i)
          END DO
          REWIND(12)
          DO i=1,NtN
            zs = fsurf(x(i),y(i)) 
            zb = fbed(x(i),y(i))
            IF ((zs-zb).LE.0.0) THEN 
               WRITE(*,*)'NEGATIVE OR NULL THICKNESS!!!'
               STOP
            END IF 
            znew = zb + z(i)*(zs - zb) 
            WRITE(12,1200)Node(i),j,x(i),y(i),znew
          END DO
        CLOSE(12)

        DEALLOCATE (Node, x, y, z)
      END DO 
!
!
1000 Format(a1)
1001 Format(28x,3(i3,x))
1002 Format(x,e14.8)

1200 Format(i7,2x,i5,3(2x,e22.15)) 

END PROGRAM MshGlacierSynthetic
