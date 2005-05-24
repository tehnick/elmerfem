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
! * Mesh manipulation utilities for *Solver - routines
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
! *                       Date: 02 Apr 2001
! *
! *                Modified by: Peter R�back
! *
! *       Date of modification: 27.8.2002
! *
! * $Log: MeshUtils.f90,v $
! * Revision 1.3  2005/05/23 11:04:22  jpr
! * *** empty log message ***
! *
! * Revision 1.2  2005/05/04 09:16:13  vierinen
! * minor modifications
! *
! * Revision 1.56  2005/04/19 08:53:47  jpr
! * Renamed module LUDecomposition as LinearAlgebra.
! *
! * Revision 1.55  2005/04/04 06:18:30  jpr
! * *** empty log message ***
! *
! * Revision 1.50  2004/09/03 09:16:47  byckling
! * Added p elements
! *
! * Revision 1.49  2004/07/01 10:03:57  raback
! * *** empty log message ***
! *
! * Revision 1.48  2004/06/30 14:08:51  raback
! * *** empty log message ***
! *
! * Revision 1.47  2004/06/24 12:14:47  jpr
! * *** empty log message ***
! *
! * Revision 1.46  2004/06/18 10:57:36  jpr
! * *** empty log message ***
! *
! * Revision 1.45  2004/06/16 09:16:32  jpr
! * *** empty log message ***
! *
! * Revision 1.44  2004/05/12 12:02:48  jpr
! * *** empty log message ***
! *
! * Revision 1.43  2004/05/03 06:34:51  jpr
! * *** empty log message ***
! *
! * Revision 1.42  2004/04/30 11:35:58  jpr
! * *** empty log message ***
! *
! * Revision 1.41  2004/04/30 11:20:21  jpr
! * *** empty log message ***
! *
! * Revision 1.40  2004/04/29 08:57:00  jpr
! * *** empty log message ***
! *
! * Revision 1.39  2004/03/25 06:35:21  jpr
! * Modified the periodic boundary project.
! *
! * Revision 1.38  2004/03/01 14:06:37  jpr
! * Modified to use AllocateVector/AllocateArray.
! *
! * Revision 1.37  2004/03/01 10:42:14  jpr
! * Added some allocation failure checks.
! *
! * Revision 1.36  2004/03/01 09:53:28  jpr
! * Started log.
! * Added code to exit with a message if Mesh cannot be loaded.
! *
! *
! * $Id: MeshUtils.f90,v 1.3 2005/05/23 11:04:22 jpr Exp $
! *****************************************************************************/

MODULE MeshUtils

    USE ElementUtils
    USE ElementDescription
    USE ParallelUtils

    IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
   FUNCTION AllocateElement() RESULT( Element )
DLLEXPORT AllocateElement
!------------------------------------------------------------------------------
     TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
    INTEGER :: istat
!------------------------------------------------------------------------------

     ALLOCATE( Element, STAT=istat )
     IF ( istat /= 0 ) &
        CALL Fatal( 'AllocateElement', 'Unable to allocate a few bytes of memory?' )
     Element % BDOFs    = 0
     Element % NDOFs    = 0
     Element % BodyId   = -1
     Element % Splitted = 0
     Element % hConvergence = 0
     Element % hK = 0
     Element % StabilizationMk = 0
     NULLIFY( Element % TYPE )
     NULLIFY( Element % PDefs )
     NULLIFY( Element % BubbleIndexes )
     NULLIFY( Element % DGIndexes )
     NULLIFY( Element % NodeIndexes )
     NULLIFY( Element % EdgeIndexes )
     NULLIFY( Element % FaceIndexes )
     NULLIFY( Element % BoundaryInfo )
!------------------------------------------------------------------------------
   END FUNCTION AllocateElement
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
   SUBROUTINE AllocatePDefinitions(Element)
DLLEXPORT AllocatePDefinitions
!------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER :: istat

     Type(Element_t) :: Element

     ALLOCATE(Element % PDefs, STAT=istat)
     IF ( istat /= 0) CALL Fatal('AllocatePDefinitions','Unable to allocate memory')

     ! Initialize fields
     Element % PDefs % P = 0 
     Element % PDefs % TetraType = 0
     Element % PDefs % isEdge = .false.
     Element % PDefs % pyramidQuadEdge = .false.
     Element % PDefs % localNumber = 0
     Element % PDefs % GaussPoints = 0

!------------------------------------------------------------------------------
   END SUBROUTINE AllocatePDefinitions
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
   FUNCTION AllocateMesh() RESULT(Mesh)
DLLEXPORT AllocateMesh
!------------------------------------------------------------------------------
     TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
     INTEGER :: istat

     ALLOCATE( Mesh, STAT=istat )
     IF ( istat /= 0 ) &
        CALL Fatal( 'AllocateMesh', 'Unable to allocate a few bytes of memory?' )

!    Nothing computed on this mesh yet!
!    ----------------------------------
     Mesh % SavesDone    = 0
     Mesh % OutputActive = .FALSE.

     Mesh % AdaptiveDepth = 0
     Mesh % Changed   = .FALSE. !  TODO: Change this sometime

     Mesh % Stabilize = .FALSE.

     NULLIFY( Mesh % Variables )
     NULLIFY( Mesh % Parent )
     NULLIFY( Mesh % Child  )
     NULLIFY( Mesh % Next)
     NULLIFY( Mesh % RootQuadrant )
     NULLIFY( Mesh % Elements )
     NULLIFY( Mesh % Edges )
     NULLIFY( Mesh % Faces )
     NULLIFY( Mesh % Projector )
     Mesh % NumberOfEdges = 0
     Mesh % NumberOfFaces = 0

     Mesh % MaxFaceDOFs = 0
     Mesh % MaxEdgeDOFs = 0
     Mesh % MaxBDOFs = 0
     Mesh % MaxElementDOFs  = 0
     Mesh % MaxElementNodes = 0

     ALLOCATE( Mesh % Nodes, STAT=istat )
     IF ( istat /= 0 ) &
        CALL Fatal( 'AllocateMesh', 'Unable to allocate a few bytes of memory?' )
     NULLIFY( Mesh % Nodes % x )
     NULLIFY( Mesh % Nodes % y )
     NULLIFY( Mesh % Nodes % z )
     NULLIFY( Mesh % Nodes % INTERFACE )
     NULLIFY( Mesh % Nodes % NeighbourList )
     NULLIFY( Mesh % Nodes % GlobalNodeNumber )
     NULLIFY( Mesh % Nodes % Perm, Mesh % Nodes % INVPerm )
!------------------------------------------------------------------------------
   END FUNCTION AllocateMesh
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Function to load mesh from disk
!------------------------------------------------------------------------------
  FUNCTION LoadMesh( Model, MeshDirPar, MeshNamePar,&
    BoundariesOnly, NumProcs,MyPE ) RESULT( Mesh )
DLLEXPORT LoadMesh
!------------------------------------------------------------------------------
    IMPLICIT NONE

    CHARACTER(LEN=*) :: MeshDirPar,MeshNamePar
    LOGICAL :: BoundariesOnly

    INTEGER, OPTIONAL :: numprocs,mype

    TYPE(Mesh_t),  POINTER :: Mesh
    TYPE(Model_t) :: Model
!------------------------------------------------------------------------------

    INTEGER :: eio_info
 
    REAL(KIND=dp) :: Coord(3*MAX_NODES)

    REAL(KIND=dp), POINTER :: cCoord(:)

    INTEGER :: i,j,k,code,tag,body,TYPE,nodes(MAX_NODES),left,right,fields, &
                       lbody,rbody,npart,parts(1024)

    INTEGER :: bodies,body_forces,body_equations,materials, &
      boundary_conditions,initial_conditions, mesh_parameters,bndry, &
       name,n,linsys,procs,CountByType(64),Types(64),TypeCount

    LOGICAL :: GotIt, NeedEdges

    INTEGER :: MinIndex,MaxIndex,MinEIndex,MaxEIndex, &
        BDOFs, EDOFs,FDOFs,inDOFs(6), DGIndex, istat, mesh_dim, save_dim
    INTEGER, POINTER :: NodeTags(:),CoordMap(:),BList(:)
    INTEGER, POINTER :: LocalPerm(:),LocalEPerm(:), &
           ElementTags(:), EdgeDOFs(:), FaceDOFs(:)

    Type(Element_t), POINTER :: Element, Edge, Face, Parent

    LOGICAL :: parallel
    CHARACTER(LEN=MAX_NAME_LEN) :: MeshDir, MeshName
!------------------------------------------------------------------------------

    Mesh => AllocateMesh()
    parallel = .FALSE.

    MeshDir(1:LEN_TRIM(MeshDirPar)+1)   = TRIM(MeshDirPar)  // CHAR(0)
    MeshName(1:LEN_TRIM(MeshNamePar)+1) = TRIM(MeshNamePar) // CHAR(0)
!------------------------------------------------------------------------------
    IF ( PRESENT(numprocs) .AND. PRESENT(mype) ) THEN
      IF ( numprocs > 1 ) THEN
        CALL eio_init( eio_info )
#ifdef  EIO_GET_TOTAL
#define EIO_GET_TOTAL
           CALL eio_open_mesh( MeshName,eio_info )
           IF ( eio_info /= 0 ) THEN
              WRITE( Message, * ) 'Unable to load mesh: ', MeshName )
              CALL Fatal( 'LoadMesh', Message )
           END IF

           CALL eio_get_mesh_description( Mesh % Nodes % TotalNodes,i,j, &
                    Typecount,Types,CountByType,eio_info )

           CALL eio_close_mesh( eio_info )
!          CALL eio_close_model( eio_info )
        CALL eio_close( eio_info )
#endif
        parallel = .TRUE.
        CALL eio_init_parallel( numprocs,mype+1,eio_info )
      ELSE
        CALL eio_init( eio_info )
      END IF
    ELSE
      CALL eio_init( eio_info )
    END IF

!------------------------------------------------------------------------------
!   Mesh
!------------------------------------------------------------------------------
    CALL eio_open_mesh( MeshName,eio_info )
    IF ( eio_info /= 0 ) THEN
       WRITE( Message, * ) 'Unable to load mesh: ', TRIM( MeshName )
       CALL Fatal( 'LoadMesh', Message )
    END IF

    CALL eio_get_mesh_description( Mesh % NumberOfNodes, &
      Mesh % NumberOfBulkElements,Mesh % NumberOfBoundaryElements, &
             Typecount,Types,CountByType,eio_info )
    IF ( eio_info /= 0 ) THEN
       WRITE( Message, * ) 'Unable to read mesh header from mesh: ', TRIM(MeshName)
       CALL Fatal( 'LoadMesh', Message )
    END IF

    Mesh % Nodes % NumberOfNodes = Mesh % NumberOfNodes

    IF ( BoundariesOnly ) Mesh % NumberOfBulkElements = 0

    Mesh % MaxElementNodes = 0
    Mesh % MaxElementDOFs  = 0
    Mesh % MaxEdgeDOFs = 0
    Mesh % MaxFaceDOFs = 0
    Mesh % MaxBDOFs = 0
    DO i=1,TypeCount
      Mesh % MaxElementNodes = MAX( &
                 Mesh % MaxElementNodes,Types(i)-100*(Types(i)/100))
    END DO

    CALL AllocateVector( Mesh % Nodes % x, Mesh % NumberOfNodes, 'LoadMesh' )
    CALL AllocateVector( Mesh % Nodes % y, Mesh % NumberOfNodes, 'LoadMesh' )
    CALL AllocateVector( Mesh % Nodes % z, Mesh % NumberOfNodes, 'LoadMesh' )
    CALL AllocateVector( Mesh % Elements, Mesh % NumberOfBulkElements + &
                    Mesh % NumberOfBoundaryElements, 'LoadMesh' )
!
!------------------------------------------------------------------------------
!    Mesh nodes
!------------------------------------------------------------------------------
    CALL AllocateVector( cCoord, 3*Mesh % NumberOfNodes, 'LoadMesh' )
    CALL AllocateVector( Mesh % Nodes % GlobalNodeNumber, Mesh % NumberOfNodes, 'LoadMesh' )

    NodeTags => Mesh % Nodes % GlobalNodeNumber
    CALL eio_get_mesh_nodes( NodeTags,cCoord,eio_info )

    CoordMap => ListGetIntegerArray( Model % Simulation, &
             'Coordinate Mapping',GotIt )

    IF ( GotIt ) THEN
      IF ( SIZE( CoordMap ) /= 3 ) THEN
         WRITE( Message, * ) 'Inconsistent Coordinate Mapping: ', CoordMap
         CALL Error( 'LoadMesh', Message )
         WRITE( Message, * ) 'Coordinate mapping should be a permutation of 1,2 and 3'
         CALL Fatal( 'LoadMesh', Message )
      END IF

      IF ( MINVAL( CoordMap ) < 1 .OR. MAXVAL( CoordMap ) > 3 ) THEN
         WRITE( Message, * ) 'Inconsistent Coordinate Mapping: ', CoordMap
         CALL Error( 'LoadMesh', Message )
         WRITE( Message, * ) 'Coordinate mapping should be a permutation of 1,2 and 3'
         CALL Fatal( 'LoadMesh', Message )
      END IF

      Mesh % Nodes % x = cCoord(CoordMap(1):3*Mesh % NumberOfNodes:3)
      Mesh % Nodes % y = cCoord(CoordMap(2):3*Mesh % NumberOfNodes:3)
      Mesh % Nodes % z = cCoord(CoordMap(3):3*Mesh % NumberOfNodes:3)
    ELSE
      Mesh % Nodes % x = cCoord(1:3*Mesh % NumberOfNodes:3)
      Mesh % Nodes % y = cCoord(2:3*Mesh % NumberOfNodes:3)
      Mesh % Nodes % z = cCoord(3:3*Mesh % NumberOfNodes:3)
    END IF

    mesh_dim = 0
    IF ( ANY( Mesh % Nodes % x /= Mesh % Nodes % x(1) ) ) mesh_dim=mesh_dim+1
    IF ( ANY( Mesh % Nodes % y /= Mesh % Nodes % y(1) ) ) mesh_dim=mesh_dim+1
    IF ( ANY( Mesh % Nodes % z /= Mesh % Nodes % z(1) ) ) mesh_dim=mesh_dim+1

    save_dim = Model % DIMENSION
    IF ( Model % DIMENSION <= 0 ) THEN
       Model % DIMENSION = mesh_dim
    END IF
 
    DEALLOCATE( cCoord )

    MinIndex = MINVAL( NodeTags )
    MaxIndex = MAXVAL( NodeTags )

    CALL AllocateVector( LocalPerm, MaxIndex-MinIndex+1, 'LoadMesh' )
    LocalPerm = 0
    DO i=1,Mesh % NumberOfNodes
      LocalPerm(NodeTags(i) - MinIndex + 1) = i
    END DO

!-----------------------------------------------------------------------------
!   Mesh elements
!------------------------------------------------------------------------------
    CALL AllocateVector( ElementTags, Mesh % NumberOfBulkElements+1, 'LoadMesh' )
    CALL AllocateVector( EdgeDOFs, Mesh % NumberOfBulkElements, 'LoadMesh' )
    CALL AllocateVector( FaceDOFs, Mesh % NumberOfBulkElements, 'LoadMesh' )

    ElementTags = 0
    DGIndex = 0
    NeedEdges = .FALSE.

    DO i=1,Mesh % NumberOfBulkElements+1
       ! Clear indofs
       inDOFs = 0
      CALL eio_get_mesh_element_conns( ElementTags(i),body,TYPE,inDOFs,nodes,eio_info )
      IF ( eio_info /= 0 ) EXIT

      Element => Mesh % Elements(i)
      NULLIFY( Element % TYPE)
      NULLIFY( Element % BoundaryInfo )
      Element % TYPE => GetElementType( TYPE )

      IF ( ASSOCIATED( Element % TYPE ) ) THEN
        Element % BodyId = body

        n = Element % TYPE % NumberOfNodes
        CALL AllocateVector( Element % NodeIndexes, n )
        DO j=1,n
          Element % NodeIndexes(j) = LocalPerm( nodes(j) - MinIndex + 1)
        END DO

        IF ( inDOFs(1) /= 0 ) THEN
           Element % NDOFs = n
        ELSE
           Element % NDOFs = 0
        END IF

        EdgeDOFs(i) = inDOFs(2)
        FaceDOFs(i) = inDOFs(3)

        Element % DGDOFs = inDOFs(4)
        IF ( inDOFs(4) > 0 ) THEN
          CALL AllocateVector( Element % DGIndexes, inDOFs(4))
          DO j=1,inDOFs(4)
             DGIndex = DGIndex + 1
             Element % DGIndexes(j) = DGIndex
          END DO
        ELSE
          NULLIFY( Element % DGIndexes )
        END IF
        NeedEdges = NeedEdges .OR. ANY( inDOFs(2:4) /= 0 )

        NULLIFY( Element % EdgeIndexes )
        NULLIFY( Element % FaceIndexes )
        NULLIFY( Element % BubbleIndexes )

        ! Check if given element is a p element
        IF (inDOFs(6) /= 0) THEN
           IF (EdgeDOFs(i) /= 0 .OR. FaceDOFs(i) /= 0) THEN
              CALL Warn('MeshUtils','Unable to use p elements with manually set DOFs')
           ELSE
              CALL AllocatePDefinitions(Element) 

              NeedEdges = .TRUE.
              
              ! Calculate element bubble dofs and set element p
              Element % PDefs % P = inDOFs(6)
              EdgeDOFs(i) = Element % PDefs % P
              FaceDOFs(i) = Element % PDefs % P
              IF ( inDOFs(5) == 0 ) THEN
                 Element % BDOFs = getBubbleDOFs(Element, Element % PDefs % P)
              ELSE
                 Element % BDOFs = inDOFs(5)
              END IF
              
              ! All elements in actual mesh are not edges
              Element % PDefs % pyramidQuadEdge = .FALSE.
              Element % PDefs % isEdge = .FALSE.

              ! If element is of type tetrahedron and is a p element, 
              ! do the Ainsworth & Coyle trick
              IF (Element % Type % ElementCode == 504) CALL ConvertToACTetra( Element )
           END IF
        ELSE 
           ! Clear P element definitions and set manual bubbles
           NULLIFY(Element % PDefs)
           Element % BDOFs = inDOFs(5)
           ! WRITE (*,*) Element % BDOFs
        END IF

        Mesh % MaxElementNodes = MAX( &
                 Mesh % MaxElementNodes,Element % TYPE % NumberOfNodes )

      ELSE
        WRITE( Message, * ) 'Unknown element type ',TYPE,' ignoring element.'
        CALL Warn( 'LoadMesh', Message )
      END IF
    END DO

!------------------------------------------------------------------------------
    MinEIndex = MINVAL( ElementTags(1:Mesh % NumberOfBulkElements) )
    MaxEIndex = MAXVAL( ElementTags(1:Mesh % NumberOfBulkElements) )

    CALL AllocateVector( LocalEPerm, MaxEIndex - MinEIndex + 1, 'LoadMesh' )
    LocalEPerm = 0
    DO i=1,Mesh % NumberOfBulkElements
       LocalEPerm( ElementTags(i) - MinEIndex + 1 ) = i
    END DO
!------------------------------------------------------------------------------
!            Mesh boundary elements
!------------------------------------------------------------------------------
    DO i=Mesh % NumberOfBulkElements + 1, &
      Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements  + 1

      CALL eio_get_mesh_bndry_element( tag, bndry, left, &
                right, TYPE, nodes, coord, eio_info )

      IF ( eio_info /= 0 ) THEN
         Mesh % NumberOfBoundaryElements = &
              i - (Mesh % NumberOfBulkElements + 1)
         EXIT
      END IF

      IF ( Left >= MinEIndex .AND. Left <= MaxEIndex ) THEN
         Left  = LocalEPerm( Left - MinEIndex + 1)
      ELSE IF ( Left > 0 ) THEN
         WRITE( Message, * ) mype,'BOUNDARY PARENT out of range: ', Tag, Left
         CALL Error( 'LoadMesh', Message )
         Left = 0
      END IF

      IF ( Right >= MinEIndex .AND. Right <= MaxEIndex ) THEN
         Right = LocalEPerm( Right - MinEIndex + 1)
      ELSE IF ( Right > 0 ) THEN
         WRITE( Message, * ) mype,'BOUNDARY PARENT out of range: ', Tag,Right
         CALL Error( 'LoadMesh', Message )
         Right = 0
      END IF

      rbody = -1
      lbody = -1
      IF (  left > 0 .AND. left <= Mesh % NumberOfBulkElements ) &
        lbody = Mesh % Elements( left) % BodyId

      IF ( right > 0 .AND. right <= Mesh % NumberOfBulkElements ) &
        rbody = Mesh % Elements(right) % BodyId

      IF ( lbody > Model % NumberOfBodies ) lbody = 0;
      IF ( rbody > Model % NumberOfBodies ) rbody = 0;

      NULLIFY(Mesh % Elements(i) % TYPE)
      Mesh % Elements(i) % TYPE => GetElementType( TYPE )

      Mesh % MaxElementNodes = MAX( &
                 Mesh % MaxElementNodes,TYPE-100*(TYPE/100))

      IF ( ASSOCIATED( Mesh % Elements(i) % TYPE ) ) THEN
        n = Mesh % Elements(i) % TYPE % NumberOfNodes

        ALLOCATE( Mesh % Elements(i) % BoundaryInfo, STAT=istat )
        IF ( istat /= 0 ) &
           CALL Fatal( 'LoadMesh', 'Unable to allocate mesh arrays.' )

        Mesh % Elements(i) % BoundaryInfo % Constraint =  0

        DO j=1,Model % NumberOfBoundaries
          IF ( Model % BoundaryId(j) == bndry ) THEN
            Mesh % Elements(i) % BoundaryInfo % Constraint = &
              ListGetInteger( Model % Boundaries(j) % Values, &
                 'Boundary Condition',GotIt, minv=1, maxv=Model % NumberOFBCs )
            EXIT
          END IF
        END DO

        Mesh % Elements(i) % BodyId = 0

        DO j=1,Model % NumberOfBCs
          BList => ListGetIntegerArray( Model % BCs(j) % Values, &
                     'Target Boundaries', GotIt ) 
          IF ( GotIt ) THEN
            DO k=1,SIZE(BList)
              IF ( BList(k) == bndry ) THEN
                Mesh % Elements(i) % BoundaryInfo % Constraint = j
                Mesh % Elements(i) % BodyId  = ListGetInteger( &
                  Model % BCs(j) % Values, 'Body Id', Gotit, 1, Model % NumberOfBodies )
                EXIT
              END IF
            END DO
          END IF
        END DO

        j = Mesh % Elements(i) % BoundaryInfo % Constraint
        IF ( j > 0 .AND. j <= Model % NumberOfBCs ) &
          Mesh % Elements(i) % BoundaryInfo % OutBody = &
             ListGetInteger( Model % BCs(j) % Values, &
                'Normal Target Body', GotIt, maxv=Model % NumberOFBodies ) 

        CALL AllocateVector( Mesh % Elements(i) % NodeIndexes, n )

        ! Set local to global mapping for boundary element
        DO j=1,n
              Mesh % Elements(i) % NodeIndexes(j) = LocalPerm(nodes(j) - MinIndex + 1)
        END DO

        NULLIFY( Mesh % Elements(i) % EdgeIndexes )
        NULLIFY( Mesh % Elements(i) % FaceIndexes )

        Mesh % Elements(i) % BoundaryInfo % LBody =  lbody
        Mesh % Elements(i) % BoundaryInfo % RBody =  rbody

        Mesh % Elements(i) % BoundaryInfo % LElement = left 
        Mesh % Elements(i) % BoundaryInfo % RElement = right

        NULLIFY( Mesh % Elements(i) % BoundaryInfo % Left )
        IF ( Left >= 1 ) THEN
          Mesh % Elements(i) % BoundaryInfo % Left => &
                  Mesh % Elements(left)
        END IF

        NULLIFY( Mesh % Elements(i) % BoundaryInfo % Right )
        IF ( Right >= 1 ) THEN
          Mesh % Elements(i) % BoundaryInfo % Right => &
                 Mesh % Elements(right)
        END IF

        NULLIFY( Mesh % Elements(i) % Boundaryinfo %  &
               GebhardtFactors % Elements )

        NULLIFY( Mesh % Elements(i) % Boundaryinfo %  &
              GebhardtFactors % Factors ) 

        NULLIFY( Mesh % Elements(i) % Boundaryinfo %  &
               ViewFactors % Elements )

        NULLIFY( Mesh % Elements(i) % Boundaryinfo %  &
              ViewFactors % Factors ) 

        Mesh % Elements(i) % BoundaryInfo % &
            GebhardtFactors % NumberOfFactors = 0

        Mesh % Elements(i) % NDOFs  = n
        IF ( ASSOCIATED(Mesh % Elements(i) % BoundaryInfo % Left) ) THEN
           IF( Mesh % Elements(i) % BoundaryInfo % Left % NDOFs == 0 )  &
              Mesh % Elements(i) % NDOFs = 0

           IF ( Mesh % Elements(i) % TYPE % DIMENSION == 1 ) THEN
              Mesh % Elements(i) % BDOFs = &
                    EdgeDOFs( Mesh % Elements(i) % BoundaryInfo % LElement )
           ELSE
              Mesh % Elements(i) % BDOFs = &
                    FaceDOFs( Mesh % Elements(i) % BoundaryInfo % LElement )
           END IF
        END IF

        IF ( ASSOCIATED(Mesh % Elements(i) % BoundaryInfo % Right) ) THEN
           IF( Mesh % Elements(i) % BoundaryInfo % Right % NDOFs == 0 )  &
              Mesh % Elements(i) % NDOFs = 0

           IF ( Mesh % Elements(i) % TYPE % DIMENSION == 1 ) THEN
              Mesh % Elements(i) % BDOFs = &
                    EdgeDOFs( Mesh % Elements(i) % BoundaryInfo % RElement )
           ELSE
              Mesh % Elements(i) % BDOFs = &
                    FaceDOFs( Mesh % Elements(i) % BoundaryInfo % RElement )
           END IF
        END IF

        Mesh % Elements(i) % BDOFs  = 0
        Mesh % Elements(i) % DGDOFs = 0
        NULLIFY( Mesh % Elements(i) % PDefs )
        NULLIFY( Mesh % Elements(i) % DGIndexes )
        NULLIFY( Mesh % Elements(i) % EdgeIndexes )
        NULLIFY( Mesh % Elements(i) % FaceIndexes )
        NULLIFY( Mesh % Elements(i) % BubbleIndexes )
      ELSE
        WRITE( Message, * ) 'Unknown element type ',TYPE,' ignoring element.'
        CALL Warn( 'LoadMesh', Message )
      END IF
    END DO

    IF ( Mesh % MaxElementDOFs <= 0 ) Mesh % MaxElementDOFs = Mesh % MaxElementNodes 

    DEALLOCATE( LocalEPerm, ElementTags )

    NULLIFY( Model % FreeSurfaceNodes )
    NULLIFY( Model % BoundaryCurvatures )
!------------------------------------------------------------------------------

    Mesh % Nodes % NumberOfIfNodes = 0

    IF ( parallel ) THEN
      CALL eio_get_part_description( fields,eio_info )

      ALLOCATE( Mesh % Nodes % NeighbourList(Mesh % NumberOfNodes), STAT=istat )
      IF ( istat /= 0 ) CALL Fatal( 'LoadMesh', 'Unable to allocate mesh arrays.' )

      DO i=1,Mesh % NumberOfNodes
        NULLIFY( Mesh % Nodes % NeighbourList(i) % Neighbours )
      END DO

      CALL AllocateVector( Mesh % Nodes % INTERFACE, Mesh % NumberOfNodes, 'LoadMesh')
      Mesh % Nodes % INTERFACE = .FALSE.

      DO i=1,fields
        CALL eio_get_part_node( tag,bndry,coord,npart,parts,eio_info )
        IF ( eio_info /= 0 ) &
           CALL Fatal( 'LoadMesh', 'Error in shared nodes definition.' )
        k = LocalPerm( tag-MinIndex+1 )

        Mesh % Nodes % INTERFACE(k) = .TRUE.
        CALL AllocateVector( Mesh % Nodes % NeighbourList(k) % Neighbours, npart )
        Mesh % Nodes % NeighbourList(k) % Neighbours = parts(1:npart) - 1

        IF ( parts(1)-1 /= mype ) Mesh % Nodes % NumberOfIfNodes = &
                  Mesh % Nodes % NumberOfIfNodes + 1
      END DO

      DO i=1,Mesh % NumberOfNodes
        IF ( .NOT.ASSOCIATED( Mesh % Nodes % NeighbourList(i) % Neighbours) ) THEN
           CALL AllocateVector(Mesh % Nodes % NeighbourList(i) % Neighbours, 1 )
           Mesh % Nodes % NeighbourList(i) % Neighbours(1) = mype
        END IF
      END DO
    END IF

    DEALLOCATE( LocalPerm )
    CALL eio_close_mesh( eio_info )

!  not needed anymore
!   CALL eio_close_model( eio_info )

    CALL eio_close( eio_info )

    IF ( NeedEdges ) THEN
      CALL FindMeshEdges( Mesh )
      ! Set edge and face polynomial degree and degrees of freedom for
      ! all elements
      DO i=1,Mesh % NumberOFBulkElements
         Element => Mesh % Elements(i)
         
         ! Iterate each edge of element
         DO j = 1,Element % Type % NumberOfEdges
            Edge => Mesh % Edges( Element % EdgeIndexes(j) ) 
            
            ! Set attributes of p element edges
            IF ( ASSOCIATED(Element % PDefs) ) THEN   
               ! Set edge polynomial degree and dofs
               Edge % PDefs % P = MAX(EdgeDOFs(i), Edge % PDefs % P)
               Edge % BDOFs = MAX(0, Edge % PDefs % P - 1)
               Edge % PDefs % isEdge = .TRUE.
               ! Get gauss points for edge. If no dofs 2 gauss points are 
               ! still needed for integration of linear equation!
               Edge % PDefs % GaussPoints = (Edge % BDOFs+2) ** Edge % Type % Dimension  
               
            ! Other element types, which need edge dofs
            ELSE
               Edge % BDOFs = MAX(EdgeDOFs(i), Edge % BDOFs)
            END IF

            ! Get maximum dof for edges
            Mesh % MaxEdgeDOFs = MAX(Edge % BDOFs, Mesh % MaxEdgeDOFs)
         END DO

         ! Iterate each face of element
         DO j=1,Element % Type % NumberOfFaces
            Face => Mesh % Faces( Element % FaceIndexes(j) )

            ! Set attibutes of p element faces
            IF ( ASSOCIATED(Element % PDefs) ) THEN
               ! Set face polynomial degree and dofs
               Face % PDefs % P = MAX(FaceDOFs(i), Face % PDefs % P)
               ! Get number of face dofs
               Face % BDOFs = getFaceDOFs(Element, Face % PDefs % P, j)
               Face % PDefs % isEdge = .TRUE.
               Face % PDefs % GaussPoints = getNumberOfGaussPointsFace( Face, Mesh )
            ELSE
               Face % BDOFs = MAX(FaceDOFs(i), Face % BDOFs)
            END IF
               
            ! Get maximum dof for faces
            Mesh % MaxFaceDOFs = MAX(Face % BDOFs, Mesh % MaxFaceDOFs)
         END DO
      END DO

      ! Set local edges for boundary elements
      DO i=Mesh % NumberOfBulkElements + 1, &
           Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements
         Element => Mesh % Elements(i)

         ! Here set local number and copy attributes to this boundary element for left parent.
         IF (ASSOCIATED(Element % BoundaryInfo % Left)) THEN
            ! Local edges are only assigned for p elements
            IF (ASSOCIATED(Element % BoundaryInfo % Left % PDefs)) THEN

               CALL AllocatePDefinitions(Element)
               Element % PDefs % isEdge = .TRUE.
               CALL AssignLocalNumber(Element, Element % BoundaryInfo % Left, Mesh)
               ! CYCLE
            END IF
         END IF

         ! Here set local number and copy attributes to this boundary element for right parent
         IF (ASSOCIATED(Element % BoundaryInfo % Right)) THEN
            ! Local edges are only assigned for p elements
            IF (ASSOCIATED(Element % BoundaryInfo % Right % PDefs)) THEN
               CALL AllocatePDefinitions(Element)
               Element % PDefs % isEdge = .TRUE.
               CALL AssignLocalNumber(Element, Element % BoundaryInfo % Right, Mesh)
            END IF
         END IF
      END DO
   END IF

   ! Set gauss points for each p element
   DO i=1,Mesh % NumberOfBulkElements
      Element => Mesh % Elements(i)
      IF ( ASSOCIATED(Element % PDefs) ) THEN
         Element % PDefs % GaussPoints = getNumberOfGaussPoints( Element, Mesh )
      END IF

      ! Set max element dofs here (because element size may have changed
      ! when edges and faces have been set). This is the absolute worst case.
      ! Element which has MaxElementDOFs may not even be present as a 
      ! real element
      Mesh % MaxElementDOFs = MAX( Mesh % MaxElementDOFs, &
           Element % Type % NumberOfNodes + &
           Element % Type % NumberOfEdges * Mesh % MaxEdgeDOFs + &
           Element % Type % NumberOfFaces * Mesh % MaxFaceDOFs + &
           Element % BDOFs, &
           Element % DGDOFs)

      Mesh % MaxBDOFs = MAX( Element % BDOFs, Mesh % MaxBDOFs )
   END DO

   DO i=1,Mesh % NumberOFBulkElements
     Element => Mesh % Elements(i)
     IF ( Element % BDOFs > 0 ) THEN
        ALLOCATE( Element % BubbleIndexes(Element % BDOFs) )
        DO j=1,Element % BDOFs
          Element % BubbleIndexes(j) = Mesh % MaxBDOFs*(i-1)+j
        END DO
     END IF
   END DO
 
   DEALLOCATE( EdgeDOFs, FaceDOFs )

    ! DO i=1,Mesh % NumberOfBoundaryElements
    !   j = Mesh % NumberOfBulkElements + i
    !  Element => Mesh % Elements(j)
    ! END DO

!
!   If periodic BC given, compute boundary mesh projector:
!   ------------------------------------------------------
    DO i = 1,Model % NumberOfBCs
       k = ListGetInteger( Model % BCs(i) % Values, 'Periodic BC', GotIt, &
                     minv=1, maxv=Model % NumberOFBCs )
       Model % BCs(i) % PMatrix => PeriodicProjector( Model, Mesh, i, k )
    END DO

    Model % DIMENSION = save_dim
!------------------------------------------------------------------------------
  END FUNCTION LoadMesh
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE MeshStabParams( Mesh )
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    TYPE(Solver_t), POINTER :: Solver
    INTEGER :: i,n, istat
    LOGICAL :: stat
    TYPE(Nodes_t) :: Nodes
    TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------

    DO i=1,CurrentModel % NumberOfSolvers
       Solver => CurrentModel % Solvers(i)
       IF ( ASSOCIATED( Mesh, Solver % Mesh ) ) &
          Mesh % Stabilize = Mesh % Stabilize .OR. &
             ListGetLogical( Solver % Values, 'Stabilize', Stat )
    END DO

    CALL AllocateVector( Nodes % x, Mesh % MaxElementNodes )
    CALL AllocateVector( Nodes % y, Mesh % MaxElementNodes )
    CALL AllocateVector( Nodes % z, Mesh % MaxElementNodes )

    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)
       n = Element % TYPE % NumberOfNodes
       Nodes % x(1:n) = Mesh % Nodes % x(Element % NodeIndexes)
       Nodes % y(1:n) = Mesh % Nodes % y(Element % NodeIndexes)
       Nodes % z(1:n) = Mesh % Nodes % z(Element % NodeIndexes)
       IF ( Mesh % Stabilize ) THEN
          CALL StabParam( Element, Nodes,n, &
              Element % StabilizationMK, Element % hK )
       ELSE
          Element % hK = ElementDiameter( Element, Nodes )
       END IF
    END DO
 
    DEALLOCATE( Nodes % x, Nodes % y, Nodes % z )
!----------------------------------------------------------------------------
  END SUBROUTINE MeshStabParams
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  FUNCTION PeriodicProjector( Model, Mesh, This, Trgt, cdim ) RESULT(Projector)
!------------------------------------------------------------------------------
!******************************************************************************
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: This, Trgt
    INTEGER, OPTIONAL :: cdim
    TYPE(Mesh_t) :: Mesh
    TYPE(Matrix_t), POINTER :: Projector
!------------------------------------------------------------------------------

    INTEGER :: i,j,k,l,m,n,n1,n2,k1,k2, Constraint, DIM
    TYPE(Element_t), POINTER :: Element, Elements(:)
    LOGICAL :: GotIt
    INTEGER, POINTER :: NodeIndexes(:), Perm1(:), Perm2(:), &
                 InvPerm1(:), InvPerm2(:)
    TYPE(Mesh_t), POINTER ::  BMesh1, BMesh2
    REAL(KIND=dp), POINTER :: PArray(:,:)
    REAL(KIND=dp) :: x(4), RotMatrix(4,4),TrsMatrix(4,4),SclMatrix(4,4), &
                         TrfMatrix(4,4),Identity(4,4)
!------------------------------------------------------------------------------

    NULLIFY( Projector )
    IF ( This <= 0 .OR. Trgt <= 0 ) RETURN

    Elements => Mesh % Elements( Mesh % NumberOfBulkElements+1: )

!
!   Search elements in this boundary and its periodic
!   counterpart:
!   --------------------------------------------------
    n1 = 0
    n2 = 0
    DO i=1, Mesh % NumberOfBoundaryElements
       Constraint = Elements(i) % BoundaryInfo % Constraint
       IF ( Model % BCs(This) % Tag == Constraint ) n1 = n1 + 1
       IF ( Model % BCs(Trgt) % Tag == Constraint ) n2 = n2 + 1
    END DO

    IF ( n1 <= 0 .OR. n2 <= 0 ) RETURN
!
!   Initialize mesh structures for boundaries, this
!   is for getting the mesh projector:
!   ------------------------------------------------
    BMesh1 => AllocateMesh()
    BMesh2 => AllocateMesh()

    CALL AllocateVector( BMesh1 % Elements,n1 )
    CALL AllocateVector( BMesh2 % Elements,n2 )
    CALL AllocateVector( Perm1, Mesh % NumberOfNodes )
    CALL AllocateVector( Perm2, Mesh % NumberOfNodes )

!
!   Fill in the mesh element structures with the
!   boundary elements:
!   ---------------------------------------------
    n1 = 0
    n2 = 0
    Perm1 = 0
    Perm2 = 0
    BMesh1 % MaxElementNodes = 0
    BMesh2 % MaxElementNodes = 0
    DO i=1, Mesh % NumberOfBoundaryElements
       Element => Elements(i)
       n = Element % TYPE % NumberOfNodes
       Constraint = Element % BoundaryInfo % Constraint

       IF ( Model % BCs(This) % Tag == Constraint ) THEN
          n1 = n1 + 1
          BMesh1 % MaxElementNodes = MAX( BMesh1 % MaxElementNodes, n )
          BMesh1 % Elements(n1) = Elements(i)
          CALL AllocateVector(BMesh1 % Elements(n1) % NodeIndexes,n )
          BMesh1 % Elements(n1) % NodeIndexes =  Elements(i) % NodeIndexes
          NULLIFY( BMesh1 % Elements(n1) % EdgeIndexes )
          NULLIFY( BMesh1 % Elements(n1) % FaceIndexes )
          Perm1( Elements(i) % NodeIndexes ) = 1
       END IF

       IF ( Model % BCs(Trgt) % Tag == Constraint ) THEN
          n2 = n2 + 1
          BMesh2 % MaxElementNodes = MAX( BMesh2 % MaxElementNodes, n )
          BMesh2 % Elements(n2) = Elements(i)
          CALL AllocateVector(BMesh2 % Elements(n2) % NodeIndexes,n )
          BMesh2 % Elements(n2) % NodeIndexes =  Elements(i) % NodeIndexes
          NULLIFY( BMesh2 % Elements(n2) % EdgeIndexes )
          NULLIFY( BMesh2 % Elements(n2) % FaceIndexes )
          Perm2( Elements(i) % NodeIndexes ) = 1
       END IF
    END DO

    BMesh1 % NumberOfBulkElements = n1
    BMesh2 % NumberOfBulkElements = n2


!   Fill in the mesh node structures with the
!   boundary nodes:
!   -----------------------------------------
    ALLOCATE( BMesh1 % Nodes )
    BMesh1 % NumberOfNodes = COUNT( Perm1==1 )
    CALL AllocateVector( BMesh1 % Nodes % x, BMesh1 % NumberOfNodes ) 
    CALL AllocateVector( BMesh1 % Nodes % y, BMesh1 % NumberOfNodes ) 
    CALL AllocateVector( BMesh1 % Nodes % z, BMesh1 % NumberOfNodes )

    BMesh2 % NumberOfNodes = COUNT( Perm2==1 )
    CALL AllocateVector( BMesh2 % Nodes % x, BMesh2 % NumberOfNodes ) 
    CALL AllocateVector( BMesh2 % Nodes % y, BMesh2 % NumberOfNodes ) 
    CALL AllocateVector( BMesh2 % Nodes % z, BMesh2 % NumberOfNodes )

    CALL AllocateVector( InvPerm1, BMesh1 % NumberOfNodes )
    CALL AllocateVector( InvPerm2, BMesh2 % NumberOfNodes )

    k1 = 0
    k2 = 0
    DO i=1,Mesh % NumberOfNodes
       IF ( Perm1(i) > 0 ) THEN
          k1 = k1 + 1
          Perm1(i) = k1
          InvPerm1(k1) = i
          BMesh1 % Nodes % x(k1)  = Mesh % Nodes % x(i)
          BMesh1 % Nodes % y(k1)  = Mesh % Nodes % y(i)
          BMesh1 % Nodes % z(k1)  = Mesh % Nodes % z(i)
       END IF

       IF ( Perm2(i) > 0 ) THEN
          k2 = k2 + 1
          Perm2(i) = k2
          InvPerm2(k2) = i
          BMesh2 % Nodes % x(k2)  = Mesh % Nodes % x(i)
          BMesh2 % Nodes % y(k2)  = Mesh % Nodes % y(i)
          BMesh2 % Nodes % z(k2)  = Mesh % Nodes % z(i)
       END IF
    END DO

!
!   Renumber the element node pointers to use
!   only boundary nodes:
!   -----------------------------------------
    DO i=1,n1
       BMesh1 % Elements(i) % NodeIndexes = &
          Perm1(BMesh1 % Elements(i) % NodeIndexes )
    END DO

    DO i=1,n2
       BMesh2 % Elements(i) % NodeIndexes = &
           Perm2(BMesh2 % Elements(i) % NodeIndexes)
    END DO

!
!   Transform the target boundary on top of this
!   boundary (hopefully):
!   --------------------------------------------
    Identity = 0.0d0
    DO i=1,4
       Identity(i,i) = 1.0d0
    END DO

    TrsMatrix = Identity
    RotMatrix = Identity
    SclMatrix = Identity

    NULLIFY( PArray )
!
!   Translations:
!   -------------
    Parray => ListGetConstRealArray( Model % BCs(This) % Values, &
                     'Periodic BC Translate', Gotit )

    IF ( GotIt ) THEN
       DO i=1,SIZE(Parray,1)
          TrsMatrix(4,i) = Parray(i,1)
       END DO
    END IF
!
!   Scales:
!   -------
    Parray => ListGetConstRealArray( Model % BCs(This) % Values, &
                   'Periodic BC Scale', Gotit )

    IF ( GotIt ) THEN
       DO i=1,SIZE(Parray,1)
          SclMatrix(i,i) = Parray(i,1)
       END DO
    END IF

!
!   Rotations:
!   ---------
    Parray => ListGetConstRealArray( Model % BCs(This) % Values, &
                   'Periodic BC Rotate', Gotit )

    IF ( GotIt ) THEN
       Parray = Parray * PI / 180
       DO i=1,SIZE(Parray,1)
          TrfMatrix = Identity
          SELECT CASE(i)
            CASE(1)
               TrfMatrix(2,2) =  COS(Parray(i,1))
               TrfMatrix(2,3) = -SIN(Parray(i,1))
               TrfMatrix(3,2) =  SIN(Parray(i,1))
               TrfMatrix(3,3) =  COS(Parray(i,1))
            CASE(2)
               TrfMatrix(1,1) =  COS(Parray(i,1))
               TrfMatrix(1,3) = -SIN(Parray(i,1))
               TrfMatrix(3,1) =  SIN(Parray(i,1))
               TrfMatrix(3,3) =  COS(Parray(i,1))
            CASE(3)
               TrfMatrix(1,1) =  COS(Parray(i,1))
               TrfMatrix(1,2) = -SIN(Parray(i,1))
               TrfMatrix(2,1) =  SIN(Parray(i,1))
               TrfMatrix(2,2) =  COS(Parray(i,1))
           END SELECT
           RotMatrix = MATMUL( RotMatrix, TrfMatrix )
        END DO
     END IF

     TrfMatrix = MATMUL( MATMUL(RotMatrix,SclMatrix), TrsMatrix )

!
!    If whole transf. matrix given, it will override
!    all the other settings:
!    ------------------------------------------------
     Parray => ListGetConstRealArray( Model % BCs(This) % Values, &
                  'Periodic BC Matrix', Gotit )

     IF ( GotIt ) THEN
        DO i=1,SIZE(Parray,1)
           DO j=1,SIZE(Parray,2)
              TrfMatrix(i,j) = Parray(j,i)
           END DO
        END DO
     END IF

!    IF ( ASSOCIATED(Parray) ) DEALLOCATE( Parray )
!
!    Now transform the coordinates:
!    ------------------------------
     DO i=1,BMesh2 % NumberOfNodes
        x(1) = BMesh2 % Nodes % x(i)
        x(2) = BMesh2 % Nodes % y(i)
        x(3) = BMesh2 % Nodes % z(i)
        x(4) = 1.0d0

PRINT*,'bef: ', REAL(x)
        x = MATMUL( x, TrfMatrix ) 
PRINT*,'aft: ', REAL(x)

        BMesh2 % Nodes % x(i) = x(1) / x(4)
        BMesh2 % Nodes % y(i) = x(2) / x(4)
        BMesh2 % Nodes % z(i) = x(3) / x(4)
     END DO
!
!    Get the mesh projetor, which now contains
!    weigths between the two boundaries nodes:
!    ------------------------------------------
     Projector => MeshProjector( BMesh2, BMesh1 )

     Projector % InvPerm => InvPerm1
     DO i=1,Projector % NumberOfRows
        DO j=Projector % Rows(i),Projector % Rows(i+1)-1
           k = Projector % Cols(j)    
           IF ( k > 0 ) Projector % Cols(j) = InvPerm2(k)
        END DO
     END DO

DO i=1,SIZE(Projector % Values)
IF ( ABS( Projector % Values(i) ) < 1.0d-12 ) Projector % Values(i) = 0.0d0
IF ( ABS( Projector % Values(i) - 1 ) < 1.0d-12 ) Projector % Values(i) = 1.0d0
END DO
!
!    Deallocate mesh structures:
!    ---------------------------
     DO i=1,BMesh1 % NumberOfBulkElements
        DEALLOCATE( BMesh1 % Elements(i) % NodeIndexes )
     END DO
     DEALLOCATE( BMesh1 % Elements )
     CALL FreeQuadrantTree( BMesh1 % RootQuadrant )

     DO i=1,BMesh2 % NumberOfBulkElements
        DEALLOCATE( BMesh2 % Elements(i) % NodeIndexes )
     END DO
     DEALLOCATE( BMesh2 % Elements )
     CALL FreeQuadrantTree( BMesh2 % RootQuadrant )

     DEALLOCATE( BMesh2 % Nodes % x )
     DEALLOCATE( BMesh2 % Nodes % y )
     DEALLOCATE( BMesh2 % Nodes % z )
     DEALLOCATE( BMesh2 % Nodes )

     DEALLOCATE( BMesh1 % Nodes % x )
     DEALLOCATE( BMesh1 % Nodes % y )
     DEALLOCATE( BMesh1 % Nodes % z )
     DEALLOCATE( BMesh1 % Nodes )

     DEALLOCATE( Perm1, Perm2, InvPerm2, BMesh1, BMesh2 )
!------------------------------------------------------------------------------
  END FUNCTION PeriodicProjector
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE WriteMeshToDisk( NewMesh, Path )
!------------------------------------------------------------------------------
    CHARACTER(LEN=*) :: Path
    TYPE(Mesh_t), POINTER :: NewMesh
!------------------------------------------------------------------------------
    INTEGER :: i,j,k,MaxNodes,ElmCode
!------------------------------------------------------------------------------

    OPEN( 1,FILE=TRIM(Path) // '/mesh.header',STATUS='UNKNOWN' )
    WRITE( 1,'(3i8)' ) NewMesh % NumberOfNodes, &
         NewMesh % NumberOfBulkElements, NewMesh % NumberOfBoundaryElements
    
    WRITE( 1,* ) 2
    MaxNodes = 0
    ElmCode  = 0
    DO i=1,NewMesh % NumberOfBoundaryElements
       k = i + NewMesh % NumberOfBulkElements
       IF ( NewMesh % Elements(k) % TYPE % NumberOfNodes > MaxNodes ) THEN
          ElmCode  = NewMesh % Elements(k) % TYPE % ElementCode
          MaxNodes = NewMesh % Elements(k) % TYPE % NumberOfNodes
       END IF
    END DO
    WRITE( 1,'(2i8)' ) ElmCode,NewMesh % NumberOfBoundaryElements

    MaxNodes = 0
    ElmCode  = 0
    DO i=1,NewMesh % NumberOfBulkElements
       IF ( NewMesh % Elements(i) % TYPE % NumberOfNodes > MaxNodes ) THEN
          ElmCode  = NewMesh % Elements(i) % TYPE % ElementCode
          MaxNodes = NewMesh % Elements(i) % TYPE % NumberOfNodes
       END IF
    END DO
    WRITE( 1,'(2i8)' ) ElmCode,NewMesh % NumberOfBulkElements
    CLOSE(1)

    OPEN( 1,FILE=TRIM(Path) // '/mesh.nodes', STATUS='UNKNOWN' )
    DO i=1,NewMesh % NumberOfNodes
       WRITE(1,'(i6,a,3e23.15)',ADVANCE='NO') i,' -1 ', &
            NewMesh % Nodes % x(i), &
            NewMesh % Nodes % y(i), NewMesh % Nodes % z(i)
       WRITE( 1,* ) ''
    END DO
    CLOSE(1)

    OPEN( 1,FILE=TRIM(Path) // '/mesh.elements', STATUS='UNKNOWN' )
    DO i=1,NewMesh % NumberOfBulkElements
       WRITE(1,'(3i7)',ADVANCE='NO') i, &
            NewMesh % Elements(i) % BodyId, &
            NewMesh % Elements(i) % TYPE % ElementCode
       DO j=1,NewMesh % Elements(i) % TYPE % NumberOfNodes
          WRITE(1,'(i7)', ADVANCE='NO') &
               NewMesh % Elements(i) % NodeIndexes(j)
       END DO
       WRITE(1,*) ''
    END DO
    CLOSE(1)

    OPEN( 1,FILE=TRIM(Path) // '/mesh.boundary', STATUS='UNKNOWN' )
    DO i=1,NewMesh % NumberOfBoundaryElements
       k = i + NewMesh % NumberOfBulkElements
       WRITE(1,'(5i7)',ADVANCE='NO') i, &
            NewMesh % Elements(k) % BoundaryInfo % Constraint, &
            NewMesh % Elements(k) % BoundaryInfo % LElement, &
            NewMesh % Elements(k) % BoundaryInfo % RElement, &
            NewMesh % Elements(k) % TYPE % ElementCode
       DO j=1,NewMesh % Elements(k) % TYPE % NumberOfNodes
          WRITE(1,'(i7)', ADVANCE='NO') &
               NewMesh % Elements(k) % NodeIndexes(j)
       END DO
       WRITE(1,*) ''
    END DO
    CLOSE(1)
!------------------------------------------------------------------------------
  END SUBROUTINE WriteMeshToDisk
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Generate element edge (faces in 3D) tables for given mesh.
! Currently only for triangles and tetras. If mesh already
! has edges do nothing.
!------------------------------------------------------------------------------
  SUBROUTINE FindMeshEdges( Mesh, FindEdges)
DLLEXPORT FindMeshEdges
!------------------------------------------------------------------------------
     TYPE(Mesh_t), POINTER :: Mesh
     LOGICAL, OPTIONAL :: FindEdges

     LOGICAL :: FindEdges3D

     IF(PRESENT(FindEdges)) THEN
       FindEdges3D = FindEdges
     ELSE
       FindEdges3D = .TRUE.
     END IF

!------------------------------------------------------------------------------

     SELECT CASE( CoordinateSystemDimension() )
        CASE(2)
          IF ( .NOT.ASSOCIATED( Mesh % Edges ) ) CALL FindMeshEdges2D( Mesh )
        CASE(3)
          IF ( .NOT.ASSOCIATED( Mesh % Faces) ) CALL FindMeshFaces3D( Mesh )
          IF(FindEdges3D) THEN
            IF ( .NOT.ASSOCIATED( Mesh % Edges) ) CALL FindMeshEdges3D( Mesh )
          END IF
     END SELECT

CONTAINS

!------------------------------------------------------------------------------
! 2D mesh edges.
!------------------------------------------------------------------------------
  SUBROUTINE FindMeshEdges2D( Mesh )
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    TYPE HashEntry_t
       INTEGER :: Node,Edge
       TYPE(HashEntry_t), POINTER :: Next
    END TYPE HashEntry_t

    TYPE HashTable_t
       TYPE(HashEntry_t), POINTER :: Head
    END TYPE HashTable_t
     
    TYPE(HashTable_t), ALLOCATABLE :: HashTable(:)
    TYPE(HashEntry_t), POINTER :: HashPtr, HashPtr1

    TYPE(Element_t), POINTER :: Element, Edges(:)

    LOGICAL :: Found
    INTEGER :: i,j,k,n,NofEdges,Edge,Swap,Node1,Node2,istat,Degree
!------------------------------------------------------------------------------
!
!   Initialize:
!   -----------
    CALL AllocateVector( Mesh % Edges, 4*Mesh % NumberOfBulkElements )
    Edges => Mesh % Edges

    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)

       SELECT CASE( Element % TYPE % ElementCode / 100 )
         CASE(3)
            n = 3
         CASE(4)
            n = 4
       END SELECT

       IF ( .NOT. ASSOCIATED( Element % EdgeIndexes ) ) &
          CALL AllocateVector( Element % EdgeIndexes, n)
       Element % EdgeIndexes = 0
    END DO

    ALLOCATE( HashTable( Mesh % NumberOfNodes ) )
    DO i=1,Mesh % NumberOfNodes
       NULLIFY( HashTable(i) % Head )
    END DO
!------------------------------------------------------------------------------

!   Loop over elements:
!   -------------------
    NofEdges = 0
    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)

       SELECT CASE( Element % Type % ElementCode / 100 )
         CASE(3)
            n = 3
         CASE(4)
            n = 4
       END SELECT

!      Loop over every edge of every element:
!      --------------------------------------
       DO k=1,n
!         We use MIN(Node1,Node2) as the hash table key:
!         ----------------------------------------------
          Node1 = Element % NodeIndexes(k)
          IF ( k<n ) THEN
             Node2 = Element % NodeIndexes(k+1)
          ELSE
             Node2 = Element % NodeIndexes(1)
          END IF

          IF ( Node2 < Node1 ) THEN
             Swap  = Node1
             Node1 = Node2
             Node2 = Swap
          END IF

!         Look the edge from the hash table:
!         ----------------------------------
          HashPtr => HashTable(Node1) % Head
          Found = .FALSE.         
          DO WHILE( ASSOCIATED( HashPtr ) )
             IF ( HashPtr % Node == Node2 ) THEN
                Found = .TRUE.
                Edge = HashPtr % Edge
                EXIT
             END IF
             HashPtr => HashPtr % Next
          END DO

!         Exisiting edge, update structures:
!         ----------------------------------
          IF ( Found ) THEN
             Element % EdgeIndexes(k) = Edge
             Edges(Edge) % BoundaryInfo % RElement = i
             Edges(Edge) % BoundaryInfo % Right => Element
          ELSE

!            Edge not yet there, create:
!            ---------------------------
             NofEdges = NofEdges + 1
             Edge = NofEdges

             Degree = Element % Type % BasisFunctionDegree

             CALL AllocateVector( Edges(Edge) % NodeIndexes, Degree+1)
             ALLOCATE( Edges(Edge) % BoundaryInfo )
             Edges(Edge) % TYPE => GetElementType( 201+Degree, .FALSE. )

             Edges(Edge) % NodeIndexes(1) = Element % NodeIndexes(k)
             IF ( k < n ) THEN
                Edges(Edge) % NodeIndexes(2) = Element % NodeIndexes(k+1)
             ELSE
                Edges(Edge) % NodeIndexes(2) = Element % NodeIndexes(1)
             END IF

             DO j=2,Degree
                Edges(Edge) % NodeIndexes(j+1) = Element % NodeIndexes(k+n+j-2)
             END DO
             
             ! Create P element definitions if needed
             IF ( ASSOCIATED( Element % PDefs ) ) THEN
               CALL AllocatePDefinitions(Edges(Edge))
               Edges(Edge) % PDefs % P = 0
             ELSE
               NULLIFY( Edges(Edge) % PDefs )
             END IF

             Edges(Edge) % NDOFs  = 0
             Edges(Edge) % BDOFs  = 0
             Edges(Edge) % DGDOFs = 0
             NULLIFY( Edges(Edge) % EdgeIndexes )
             NULLIFY( Edges(Edge) % FaceIndexes )

             Element % EdgeIndexes(k) = Edge

             Edges(Edge) % BoundaryInfo % LElement = i
             Edges(Edge) % BoundaryInfo % Left => Element

             Edges(Edge) % BoundaryInfo % RElement = 0
             NULLIFY( Edges(Edge) % BoundaryInfo % Right )
              
!            Update the hash table:
!            ----------------------
             ALLOCATE( HashPtr )
             HashPtr % Edge = Edge
             HashPtr % Node = Node2
             HashPtr % Next => HashTable(Node1) % Head
             HashTable(Node1) % Head => HashPtr
          END IF
       END DO
    END DO

    Mesh % NumberOfEdges = NofEdges

!   Delete the hash table:
!   ----------------------
    DO i=1,Mesh % NumberOfNodes
       HashPtr => HashTable(i) % Head
       DO WHILE( ASSOCIATED(HashPtr) )
          HashPtr1 => HashPtr % Next
          DEALLOCATE( HashPtr )
          HashPtr  => HashPtr1
       END DO
    END DO
    DEALLOCATE( HashTable )
!------------------------------------------------------------------------------
  END SUBROUTINE FindMeshEdges2D
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! 3D mesh faces.
!------------------------------------------------------------------------------
  SUBROUTINE FindMeshFaces3D( Mesh )
    USE PElementMaps, ONLY : GetElementFaceMap
    USE PElementBase, ONLY : isPTetra

    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    TYPE HashEntry_t
       INTEGER :: Node1,Node2,Face
       TYPE(HashEntry_t), POINTER :: Next
    END TYPE HashEntry_t

    TYPE HashTable_t
       TYPE(HashEntry_t), POINTER :: Head
    END TYPE HashTable_t
    
    TYPE(HashTable_t), ALLOCATABLE :: HashTable(:)
    TYPE(HashEntry_t), POINTER :: HashPtr, HashPtr1

    LOGICAL :: Found
    INTEGER :: n1,n2,n3,n4
    INTEGER :: i,j,k,n,NofFaces,Face,Swap,Node1,Node2,Node3,istat,Degree
     
    TYPE(Element_t), POINTER :: Element, Faces(:)

    INTEGER, POINTER :: FaceMap(:,:)
    INTEGER, TARGET  :: TetraFaceMap(4,6), BrickFaceMap(6,9), &
         WedgeFaceMap(5,4), PyramidFaceMap(5,8)
    
    INTEGER :: nf(4)
!------------------------------------------------------------------------------
    
    TetraFaceMap(1,:) = (/ 1, 2, 3, 5, 6, 7 /)
    TetraFaceMap(2,:) = (/ 1, 2, 4, 5, 9, 8 /)
    TetraFaceMap(3,:) = (/ 2, 3, 4, 6,10, 9 /)
    TetraFaceMap(4,:) = (/ 3, 1, 4, 7, 8,10 /)

    WedgeFaceMap(1,:) = (/ 1, 2, 3,-1 /)
    WedgeFaceMap(2,:) = (/ 4, 5, 6,-1 /)
    WedgeFaceMap(3,:) = (/ 1, 2, 5, 4 /)
    WedgeFaceMap(4,:) = (/ 3, 2, 5, 6 /)
    WedgeFaceMap(5,:) = (/ 3, 1, 4, 6 /)

    PyramidFaceMap(1,:) = (/ 1, 2, 3, 4,  6,  7,  8,  9 /)
    PyramidFaceMap(2,:) = (/ 1, 2, 5, 6, 11, 10, -1, -1 /)
    PyramidFaceMap(3,:) = (/ 2, 3, 5, 7, 12, 11, -1, -1 /)
    PyramidFaceMap(4,:) = (/ 3, 4, 5, 8, 13, 12, -1, -1 /)
    PyramidFaceMap(5,:) = (/ 4, 1, 5, 9, 10, 13, -1, -1 /)

    BrickFaceMap(1,:) = (/ 1, 2, 3, 4,  9, 10, 11, 12, 25 /)
    BrickFaceMap(2,:) = (/ 5, 6, 7, 8, 17, 18, 19, 20, 26 /)
    BrickFaceMap(3,:) = (/ 1, 2, 6, 5,  9, 14, 17, 13, 21 /)
    BrickFaceMap(4,:) = (/ 2, 3, 7, 6, 10, 15, 17, 14, 22 /)
    BrickFaceMap(5,:) = (/ 3, 4, 8, 7, 11, 16, 19, 15, 23 /)
    BrickFaceMap(6,:) = (/ 4, 1, 5, 8, 12, 13, 20, 16, 24 /)

!
!   Initialize:
!   -----------
    CALL AllocateVector( Mesh % Faces, 6*Mesh % NumberOfBulkElements, 'FindMeshFaces3D' )
    Faces => Mesh % Faces

    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)
       IF ( .NOT. ASSOCIATED( Element % FaceIndexes ) ) &
          CALL AllocateVector(Element % FaceIndexes, 6)
       Element % FaceIndexes = 0
    END DO

    ALLOCATE( HashTable( Mesh % NumberOfNodes ) )
    DO i=1,Mesh % NumberOfNodes
       NULLIFY( HashTable(i) % Head )
    END DO
!------------------------------------------------------------------------------

!   Loop over elements:
!   -------------------
    NofFaces = 0
    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)

       ! For P elements mappings are different
       IF ( ASSOCIATED(Element % PDefs) ) THEN
          CALL GetElementFaceMap(Element, FaceMap)
          n = Element % Type % NumberOfFaces
       ELSE
          SELECT CASE( Element % Type % ElementCode / 100 )
          CASE(5)
             n = 4
             FaceMap => TetraFaceMap
          CASE(6)
             n = 5
             FaceMap => PyramidFaceMap
          CASE(7)
             n = 5 
             FaceMap => WedgeFaceMap
          CASE(8)
             n = 6
             FaceMap => BrickFaceMap
          CASE DEFAULT
             CYCLE
             ! WRITE(Message,*) 'Element type',Element % Type % ElementCode,'not implemented.' 
             ! CALL Fatal('FindMeshFaces',Message)
          END SELECT
       END IF
 
!      Loop over every face of every element:
!      --------------------------------------
       DO k=1,n
          
          
!         We use MIN(Node1,Node2,Node3) as the hash table key:
!         ---------------------------------------------------
          SELECT CASE( Element % TYPE % ElementCode / 100 )
             CASE(5)
!
!               Tetras:
!               =======
                nf(1:3) = Element % NodeIndexes( FaceMap(k,1:3) )
                CALL sort( 3, nf )

             CASE(6)
!
!               Pyramids:
!               =========
                IF ( k == 1 ) THEN
                   nf(1:4) = Element % NodeIndexes( FaceMap(k,1:4) )
                   CALL sort( 4, nf )
                ELSE
                   nf(1:3) = Element % NodeIndexes( FaceMap(k,1:3) )
                   CALL sort( 3, nf )
                END IF

             CASE(7)
!
!               Wedges:
!               =======
                IF ( k <= 2 ) THEN
                   nf(1:3) = Element % NodeIndexes( FaceMap(k,1:3) )
                   CALL sort( 3, nf )
                ELSE
                   nf(1:4) = Element % NodeIndexes( FaceMap(k,1:4) )
                   CALL sort( 4, nf )
                END IF
                
             CASE(8)
!
!               Bricks:
!               =======
                nf(1:4) = Element % NodeIndexes( FaceMap(k,1:4) )
                CALL sort( 4, nf )

             CASE DEFAULT
                WRITE(Message,*) 'Element type',Element % TYPE % ElementCode,'not implemented.' 
                CALL Fatal('FindMeshFaces',Message)
          END SELECT

          Node1 = nf(1)
          Node2 = nf(2)
          Node3 = nf(3)
          
!         Look the face from the hash table:
!         ----------------------------------
          HashPtr => HashTable(Node1) % Head
          Found = .FALSE.
          DO WHILE( ASSOCIATED( HashPtr ) )
             IF ( HashPtr % Node1 == Node2 .AND. HashPtr % Node2 == Node3) THEN
                Found = .TRUE.
                Face = HashPtr % Face
                EXIT
             END IF
             HashPtr => HashPtr % Next
          END DO

!         Exisiting face, update structures:
!         ----------------------------------
          IF ( Found ) THEN       
             Element % FaceIndexes(k) = Face
             Faces(Face) % BoundaryInfo % RElement = i
             Faces(Face) % BoundaryInfo % Right => Element
          ELSE

!            Face not yet there, create:
!            ---------------------------
             NofFaces = NofFaces + 1
             Face = NofFaces

             Degree = Element % TYPE % BasisFunctionDegree

             SELECT CASE( Element % TYPE % ElementCode / 100 )
                CASE(5)
!
!               for tetras:
!               -----------
                SELECT CASE( Degree ) 
                   CASE(1)
                   n1 = 3
                   CASE(2)
                   n1 = 6
                   CASE(3)
                   n1 = 10
                END SELECT
                n1 = 3
                
                Faces(Face) % Type => GetElementType( 300+n1, .FALSE. )

                CASE(6)

!               Pyramids ( only 605 supported )
!               -------------------------------
                IF ( k == 1 ) THEN
                   n1 = 4
                   Faces(Face) % TYPE => GetElementType( 400+n1, .FALSE. )
                ELSE
                   n1 = 3
                   Faces(Face) % TYPE => GetElementType( 300+n1, .FALSE. )
                END IF
                
                CASE(7)

!               for wedges, only 706 supported:
!               -------------------------------
                IF ( k <= 2 ) THEN
                   n1 = 3
                   Faces(Face) % TYPE => GetElementType( 303, .FALSE. )
                ELSE
                   n1 = 4
                   Faces(Face) % TYPE => GetElementType( 404, .FALSE. )
                END IF

            
                CASE(8)
!
!               for bricks:
!               -----------
                SELECT CASE( Element % TYPE % NumberOfNodes ) 
                   CASE(8)
                   n1 = 4
                   CASE(20)
                   n1 = 8
                   CASE(27)
                   n1 = 9
                END SELECT

                Faces(Face) % TYPE => GetElementType( 400+n1, .FALSE.)

                CASE DEFAULT
                   WRITE(Message,*) 'Element type',Element % TYPE % ElementCode,'not implemented.' 
                   CALL Fatal('FindMeshFaces',Message)

             END SELECT

             ! Allocate p structures for p elements
             IF ( ASSOCIATED( Element % PDefs ) ) THEN
                CALL AllocatePDefinitions(Faces(Face))
                Faces(Face) % PDefs % P = 0
             ELSE
               NULLIFY( Faces(Face) % PDefs )
             END IF
             
             Faces(Face) % NDOFs  = 0
             Faces(Face) % BDOFs  = 0
             Faces(Face) % DGDOFs = 0
             NULLIFY( Faces(Face) % EdgeIndexes )
             NULLIFY( Faces(Face) % FaceIndexes )

             CALL AllocateVector( Faces(Face) % NodeIndexes,n1 )
             DO n2=1,n1
                Faces(Face) % NodeIndexes(n2) = &
                         Element % NodeIndexes( FaceMap(k,n2) )
             END DO

             Element % FaceIndexes(k) = Face

             ALLOCATE( Faces(Face) % BoundaryInfo )
             Faces(Face) % BoundaryInfo % LElement = i
             Faces(Face) % BoundaryInfo % Left => Element

             Faces(Face) % BoundaryInfo % RElement = 0
             NULLIFY( Faces(Face) % BoundaryInfo % Right )
              
!            Update the hash table:
!            ----------------------
             ALLOCATE( HashPtr )
             HashPtr % Face = Face
             HashPtr % Node1 = Node2
             HashPtr % Node2 = Node3
             HashPtr % Next => HashTable(Node1) % Head
             HashTable(Node1) % Head => HashPtr
          END IF
       END DO
    END DO

    Mesh % NumberOfFaces = NofFaces

!   Delete the hash table:
!   ----------------------
    DO i=1,Mesh % NumberOfNodes
       HashPtr => HashTable(i) % Head
       DO WHILE( ASSOCIATED(HashPtr) )
          HashPtr1 => HashPtr % Next
          DEALLOCATE( HashPtr )
          HashPtr  => HashPtr1
       END DO
    END DO
    DEALLOCATE( HashTable )
!------------------------------------------------------------------------------
  END SUBROUTINE FindMeshFaces3D
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! 3D mesh edges.
!------------------------------------------------------------------------------
  SUBROUTINE FindMeshEdges3D( Mesh )
    USE PElementMaps, ONLY : GetElementEdgeMap, GetElementFaceEdgeMap
    USE PElementBase, ONLY : isPPyramid

    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    TYPE HashEntry_t
       INTEGER :: Node1,Edge
       TYPE(HashEntry_t), POINTER :: Next
    END TYPE HashEntry_t

    TYPE HashTable_t
       TYPE(HashEntry_t), POINTER :: Head
    END TYPE HashTable_t
    
    TYPE(HashTable_t), ALLOCATABLE :: HashTable(:)
    TYPE(HashEntry_t), POINTER :: HashPtr, HashPtr1

    LOGICAL :: Found
    INTEGER :: n1,n2
    INTEGER :: i,j,k,n,NofEdges,Edge,Node1,Node2,istat,Degree,ii,jj
     
    TYPE(Element_t), POINTER :: Element, Edges(:), Face

    INTEGER, POINTER :: EdgeMap(:,:), FaceEdgeMap(:,:)
    INTEGER, TARGET  :: TetraEdgeMap(6,3), BrickEdgeMap(12,3), &
      WedgeEdgeMap(9,3), PyramidEdgeMap(8,3), TetraFaceEdgeMap(4,3), &
      BrickFaceEdgeMap(8,4), WedgeFaceEdgeMap(6,4), PyramidFaceEdgeMap(5,4)
!------------------------------------------------------------------------------

    TetraFaceEdgeMap(1,:) = (/ 3,2,1 /)
    TetraFaceEdgeMap(2,:) = (/ 1,5,4 /)
    TetraFaceEdgeMap(3,:) = (/ 2,6,5 /)
    ! TetraFaceEdgeMap(4,:) = (/ 3,6,4 /) ! <- If negative orientation is used this is incorrect!
    TetraFaceEdgeMap(4,:) = (/ 3,4,6 /)

    TetraEdgeMap(1,:) = (/ 1,2,5 /)
    TetraEdgeMap(2,:) = (/ 2,3,6 /)
    TetraEdgeMap(3,:) = (/ 3,1,7 /)
    TetraEdgeMap(4,:) = (/ 1,4,8 /)
    TetraEdgeMap(5,:) = (/ 2,4,9 /)
    TetraEdgeMap(6,:) = (/ 3,4,10 /)

    PyramidEdgeMap(1,:) = (/ 1,2,1 /)
    PyramidEdgeMap(2,:) = (/ 2,3,1 /)
    PyramidEdgeMap(3,:) = (/ 3,4,1 /)
    PyramidEdgeMap(4,:) = (/ 4,1,1 /)
    PyramidEdgeMap(5,:) = (/ 1,5,1 /)
    PyramidEdgeMap(6,:) = (/ 2,5,1 /)
    PyramidEdgeMap(7,:) = (/ 3,5,1 /)
    PyramidEdgeMap(8,:) = (/ 4,5,1 /)

    PyramidFaceEdgeMap(1,:) = (/ 1,2,3,4 /)
    PyramidFaceEdgeMap(2,:) = (/ 1,6,5,0 /)
    PyramidFaceEdgeMap(3,:) = (/ 2,7,6,0 /)
    PyramidFaceEdgeMap(4,:) = (/ 3,8,7,0 /)
    PyramidFaceEdgeMap(5,:) = (/ 4,5,8,0 /)

    WedgeEdgeMap(1,:) = (/ 1, 2,1 /)
    WedgeEdgeMap(2,:) = (/ 2, 3,1 /)
    WedgeEdgeMap(3,:) = (/ 1, 3,1 /)
    WedgeEdgeMap(4,:) = (/ 4, 5,1 /)
    WedgeEdgeMap(5,:) = (/ 5, 6,1 /)
    WedgeEdgeMap(6,:) = (/ 6, 4,1 /)
    WedgeEdgeMap(7,:) = (/ 1, 4,1 /)
    WedgeEdgeMap(8,:) = (/ 2, 5,1 /)
    WedgeEdgeMap(9,:) = (/ 3, 6,1 /)

    WedgeFaceEdgeMap(1,:) = (/ 1,2,3,0 /)
    WedgeFaceEdgeMap(2,:) = (/ 4,5,6,0 /)
    WedgeFaceEdgeMap(3,:) = (/ 1,8,4,7 /)
    WedgeFaceEdgeMap(4,:) = (/ 2,9,5,8 /)
    WedgeFaceEdgeMap(5,:) = (/ 3,7,6,9 /)

    BrickEdgeMap(1,:) = (/ 1, 2,  9 /)
    BrickEdgeMap(2,:) = (/ 2, 3,  10 /)
    BrickEdgeMap(3,:) = (/ 4, 3,  11 /)
    BrickEdgeMap(4,:) = (/ 1, 4,  12 /)
    BrickEdgeMap(5,:) = (/ 5, 6,  13 /)
    BrickEdgeMap(6,:) = (/ 6, 7,  14 /)
    BrickEdgeMap(7,:) = (/ 8, 7,  15 /)
    BrickEdgeMap(8,:) = (/ 5, 8,  16 /)
    BrickEdgeMap(9,:) = (/ 1, 5,  17 /)
    BrickEdgeMap(10,:) = (/ 2, 6, 18 /)
    BrickEdgeMap(11,:) = (/ 3, 7, 19 /)
    BrickEdgeMap(12,:) = (/ 4, 8, 20 /)

    BrickFaceEdgeMap(1,:) = (/ 1,2,3,4   /)
    BrickFaceEdgeMap(2,:) = (/ 5,6,7,8   /)    
    BrickFaceEdgeMap(3,:) = (/ 1,10,5,9  /)
    BrickFaceEdgeMap(4,:) = (/ 2,11,6,10 /)
    BrickFaceEdgeMap(5,:) = (/ 3,12,7,11 /)
    BrickFaceEdgeMap(6,:) = (/ 4,9,8,12  /)

!
!   Initialize:
!   -----------
    CALL AllocateVector( Mesh % Edges, 12*Mesh % NumberOfBulkElements )
    Edges => Mesh % Edges

    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)
       IF ( .NOT. ASSOCIATED( Element % EdgeIndexes ) ) &
          CALL AllocateVector(Element % EdgeIndexes, 12 )
       Element % EdgeIndexes = 0
    END DO

    ALLOCATE( HashTable( Mesh % NumberOfNodes ) )
    DO i=1,Mesh % NumberOfNodes
       NULLIFY( HashTable(i) % Head )
    END DO
!------------------------------------------------------------------------------

!   Loop over elements:
!   -------------------
    NofEdges = 0
    DO i=1,Mesh % NumberOfBulkElements
       Element => Mesh % Elements(i)

       ! For P elements mappings are different
       IF ( ASSOCIATED(Element % PDefs) ) THEN
          CALL GetElementEdgeMap( Element, EdgeMap )
          CALL GetElementFaceEdgeMap( Element, FaceEdgeMap ) 
          n = Element % Type % NumberOfEdges
       ELSE 
          SELECT CASE( Element % Type % ElementCode / 100 )
          CASE(5)
             n = 6
             EdgeMap => TetraEdgeMap
             FaceEdgeMap => TetraFaceEdgeMap
          CASE(6)
             n = 8
             EdgeMap => PyramidEdgeMap
             FaceEdgeMap => PyramidFaceEdgeMap
          CASE(7)
             n = 9
             EdgeMap => WedgeEdgeMap
             FaceEdgeMap => WedgeFaceEdgeMap
          CASE(8)
             n = 12
             EdgeMap => BrickEdgeMap
             FaceEdgeMap => BrickFaceEdgeMap
          CASE DEFAULT
             CYCLE
             WRITE(Message,*) 'Element type',Element % Type % ElementCode,'not implemented.' 
             CALL Fatal('FindMeshEdges',Message)
          END SELECT
       END IF

!      Loop over every edge of every element:
!      --------------------------------------
       DO k=1,n

!         Use MIN(Node1,Node2) as key to hash table:
!         ------------------------------------------
          n1 = Element % NodeIndexes( EdgeMap(k,1) )
          n2 = Element % NodeIndexes( EdgeMap(k,2) )
          IF ( n1 < n2 ) THEN
             Node1 = n1
             Node2 = n2
          ELSE
             Node1 = n2
             Node2 = n1
          END IF
!
!         Look the edge from the hash table:
!         ----------------------------------
          HashPtr => HashTable(Node1) % Head
          Found = .FALSE.
          DO WHILE( ASSOCIATED( HashPtr ) )
             IF ( HashPtr % Node1 == Node2 ) THEN
                Found = .TRUE.
                Edge = HashPtr % Edge
                EXIT
             END IF
             HashPtr => HashPtr % Next
          END DO
!
!         Existing edge, update structures:
!         ---------------------------------
          IF ( Found ) THEN
             Element % EdgeIndexes(k) = Edge

             ! Mark edge as an edge of pydamid square face 
             IF (isPPyramid(Element) .AND. k < 5) THEN
                Edges(Edge) % PDefs % pyramidQuadEdge = .TRUE.
             END IF

             DO ii=1,Element % Type % NumberOfFaces

               Face => Mesh % Faces( Element % FaceIndexes(ii) )
               IF ( .NOT. ASSOCIATED( Face % EdgeIndexes ) ) THEN
                 ALLOCATE( Face % EdgeIndexes( Face % TYPE % NumberOfEdges ) )
                 Face % EdgeIndexes = 0
               END IF
               DO jj=1,Face % TYPE % NumberOfEdges
                  IF ( FaceEdgeMap(ii,jj) == k ) THEN
                     Face % EdgeIndexes(jj) = Edge
                     IF ( .NOT. ASSOCIATED( Edges(Edge) % BoundaryInfo % Left ) ) THEN
                        Edges(Edge) % BoundaryInfo % Left => Face
                        Edges(Edge) % BoundaryInfo % Lelement = Element % FaceIndexes(ii)
                     ELSE
                        Edges(Edge) % BoundaryInfo % Right => Face
                        Edges(Edge) % BoundaryInfo % Relement = Element % FaceIndexes(ii)
                     END IF
                  END IF
               END DO
             END DO
          ELSE

!            Edge not yet there, create:
!            ---------------------------
             NofEdges = NofEdges + 1
             Edge = NofEdges
             Degree = Element % TYPE % BasisFunctionDegree

!            Edge is always a line segment with deg+1 nodes:
!            -----------------------------------------------
             Edges(Edge) % TYPE => GetElementType( 201 + degree, .FALSE.)

             Edges(Edge) % NDOFs  = 0
             Edges(Edge) % BDOFs  = 0
             Edges(Edge) % DGDOFs = 0
             NULLIFY( Edges(Edge) % EdgeIndexes )
             NULLIFY( Edges(Edge) % FaceIndexes )

             CALL AllocateVector( Edges(Edge) % NodeIndexes, degree + 1 )
             DO n2=1,degree+1
                Edges(Edge) % NodeIndexes(n2) = Element % NodeIndexes( EdgeMap(k,n2) )
             END DO

             Element % EdgeIndexes(k) = Edge
             ALLOCATE( Edges(Edge) % BoundaryInfo )
             Edges(Edge) % BoundaryInfo % LElement = 0
             Edges(Edge) % BoundaryInfo % RElement = 0
             NULLIFY( Edges(Edge) % BoundaryInfo % Left )
             NULLIFY( Edges(Edge) % BoundaryInfo % Right )

             ! Allocate P element definitions 
             IF ( ASSOCIATED( Element % PDefs ) ) THEN
                CALL AllocatePDefinitions(Edges(Edge))
             
                Edges(Edge) % PDefs % P = 0
                Edges(Edge) % PDefs % pyramidQuadEdge = .FALSE.
                ! Here mark edge as edge of pyramid if needed (or set as not)
                IF (isPPyramid(Element) .AND. k < 5) THEN
                   Edges(Edge) % PDefs % pyramidQuadEdge = .TRUE.
                END IF
             ELSE
                NULLIFY( Edges(Edge) % PDefs )
             END IF

             DO ii=1,Element % Type % NumberOfFaces
               Face => Mesh % Faces( Element % FaceIndexes(ii) )
               IF ( .NOT. ASSOCIATED( Face % EdgeIndexes ) ) THEN
                  ALLOCATE( Face % EdgeIndexes( Face % TYPE % NumberOfEdges ) )
                  Face % EdgeIndexes = 0
               END IF
               DO jj=1,Face % TYPE % NumberOfEdges
                  IF ( FaceEdgeMap(ii,jj) == k ) THEN
                     Face % EdgeIndexes(jj) = Edge
                     IF ( .NOT. ASSOCIATED( Edges(Edge) % BoundaryInfo % Left ) ) THEN
                        Edges(Edge) % BoundaryInfo % Left => Face
                        Edges(Edge) % BoundaryInfo % Lelement = Element % FaceIndexes(ii)
                     ELSE
                        Edges(Edge) % BoundaryInfo % Right => Face
                        Edges(Edge) % BoundaryInfo % Relement = Element % FaceIndexes(ii)
                     END IF
                  END IF
               END DO
             END DO

!            Update the hash table:
!            ----------------------
             ALLOCATE( HashPtr )
             HashPtr % Edge = Edge
             HashPtr % Node1 = Node2
             HashPtr % Next => HashTable(Node1) % Head
             HashTable(Node1) % Head => HashPtr
          END IF
       END DO
    END DO

    Mesh % NumberOfEdges = NofEdges

!   Delete the hash table:
!   ----------------------
    DO i=1,Mesh % NumberOfNodes
       HashPtr => HashTable(i) % Head
       DO WHILE( ASSOCIATED(HashPtr) )
          HashPtr1 => HashPtr % Next
          DEALLOCATE( HashPtr )
          HashPtr  => HashPtr1
       END DO
    END DO
    DEALLOCATE( HashTable )
!------------------------------------------------------------------------------
  END SUBROUTINE FindMeshEdges3D
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE FindMeshEdges
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE FindNeighbourNodes( Mesh,Direction,Neighbours,EndNeighbours)
DLLEXPORT FindNeighbourNodes
!------------------------------------------------------------------------------
! Finds neigbours of the nodes in given direction.
! The algorithm finds the neighbour that within 45 degrees of the 
! given direction has the smallest distance.
!------------------------------------------------------------------------------

  TYPE(Mesh_t) , POINTER :: Mesh 
  REAL(KIND=dp) :: Direction(:)
  INTEGER :: Neighbours(:)
  INTEGER, OPTIONAL :: EndNeighbours(:)

  TYPE(Nodes_t) :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement
  REAL(KIND=dp), POINTER :: Distances(:)
  REAL(KIND=dp) :: rn(3), rs(3), ss, sn
  INTEGER, POINTER :: NodeIndexes(:)
  INTEGER :: i,j,k,n,t,DIM,istat

  IF(SIZE(Neighbours) < Mesh % NumberOfNodes) THEN
    CALL Warn('FindNeigbourNodes','SIZE of Neigbours should equal Number of Nodes!')
    RETURN
  END IF


  IF(PRESENT(EndNeighbours)) THEN
    IF(SIZE(EndNeighbours) < Mesh % NumberOfNodes) THEN
      CALL Warn('FindNeigbourNodes','SIZE of EndNeigbours should equal Number of Nodes!')
      RETURN
    END IF
  END IF


  DIM = CoordinateSystemDimension()
  N = Mesh % MaxElementNodes

  CALL AllocateVector( ElementNodes % x, n )
  CALL AllocateVector( ElementNodes % y, n )
  CALL AllocateVector( ElementNodes % z, n )
  CALL AllocateVector( Distances, Mesh % NumberOfNodes )

  Neighbours = 0
  Distances = HUGE(Distances)
 
  rn(1:DIM) = Direction(1:DIM)
  ss = SQRT(SUM(rn(1:DIM)**2.0))
  rn = rn / ss

  DO t=1,Mesh % NumberOfBulkElements

    CurrentElement => Mesh % Elements(t)
    n = CurrentElement % TYPE % NumberOfNodes
    NodeIndexes => CurrentElement % NodeIndexes
  
    ElementNodes % x(1:n) = Mesh % Nodes % x(NodeIndexes(1:n))
    ElementNodes % y(1:n) = Mesh % Nodes % y(NodeIndexes(1:n))
    IF(DIM == 3) THEN
      ElementNodes % z(1:n) = Mesh % Nodes % z(NodeIndexes(1:n))
    END IF


    DO i=1,n
      DO j=i+1,n
        rs(1) = ElementNodes % x(j) - ElementNodes % x(i)
        rs(2) = ElementNodes % y(j) - ElementNodes % y(i)
        IF (DIM == 3) THEN
          rs(3) = ElementNodes % z(j) - ElementNodes % z(i)
        END IF
        
        ss = SQRT(SUM(rs(1:DIM)**2.0))
        sn = SUM(rs(1:DIM)*rn(1:DIM))

        IF(ss < SQRT(2.0) * ABS(sn)) THEN
          IF(sn > 0) THEN
            IF(ss < Distances(NodeIndexes(i))) THEN
              Distances(NodeIndexes(i)) = ss
              Neighbours(NodeIndexes(i)) = NodeIndexes(j)
            END IF
          ELSE
            IF(ss < Distances(NodeIndexes(j))) THEN
              Distances(NodeIndexes(j)) = ss
              Neighbours(NodeIndexes(j)) = NodeIndexes(i)
            END IF
          END IF
        END IF

      END DO
    END DO
    
  END DO

  ! This loop finds the final neighbour in the end of the chain 
  IF(PRESENT(EndNeighbours)) THEN
    EndNeighbours = Neighbours

    DO t=1,Mesh%NumberOfNodes
      j = Neighbours(t)
      DO WHILE(j /= 0)
        EndNeighbours(t) = j
        j = Neighbours(j)
      END DO
    END DO
  END IF

  DEALLOCATE( ElementNodes % x, &
      ElementNodes % y, ElementNodes % z, Distances )  

END SUBROUTINE FindNeighbourNodes
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE UpdateSolverMesh( Solver, Mesh )
!------------------------------------------------------------------------------
     TYPE( Mesh_t ), POINTER :: Mesh
     TYPE( Solver_t ), POINTER :: Solver
!------------------------------------------------------------------------------
     INTEGER :: i,j,k,n,n1,n2,DOFs
     LOGICAL :: Found, OptimizeBandwidth
     TYPE(Matrix_t), POINTER   :: Matrix
     TYPE(Variable_t), POINTER :: TimeVar
     REAL(KIND=dp), POINTER :: Work(:)
     INTEGER, POINTER :: Permutation(:)
!------------------------------------------------------------------------------
     DOFs = Solver % Variable % DOFs

     Solver % Mesh => Mesh
     CALL SetCurrentMesh( CurrentModel, Mesh )
!
!    Create matrix and variable structures for
!    current equation on the new mesh:
!    -----------------------------------------
     Solver % Variable => VariableGet( Mesh % Variables, &
        Solver % Variable % Name, ThisOnly = .FALSE. )

     CALL AllocateVector( Permutation, SIZE(Solver % Variable % Perm) )

     OptimizeBandwidth = ListGetLogical( Solver % Values, 'Optimize Bandwidth', Found )
     IF ( .NOT. Found ) OptimizeBandwidth = .TRUE.

     Matrix => CreateMatrix( CurrentModel, &
        Mesh, Permutation, DOFs, MATRIX_CRS, OptimizeBandwidth, &
        ListGetString( Solver % Values, 'Equation' ) )

     Matrix % Symmetric = ListGetLogical( Solver % Values, &
             'Linear System Symmetric', Found )

     Matrix % Lumped = ListGetLogical( Solver % Values, &
             'Lumped Mass Matrix', Found )

     ALLOCATE( Work(SIZE(Solver % Variable % Values)) )
     Work = Solver % Variable % Values
     DO k=0,DOFs-1
        DO i=1,SIZE(Permutation)
           IF ( Permutation(i) > 0 ) THEN
              Solver % Variable % Values( DOFs*Permutation(i)-k ) = &
                 Work( DOFs*Solver % Variable % Perm(i)-k )
           END IF
        END DO
     END DO

     IF ( ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
        DO j=1,SIZE(Solver % Variable % PrevValues,2)
           Work = Solver % Variable % PrevValues(:,j)
           DO k=0,DOFs-1
              DO i=1,SIZE(Permutation)
                 IF ( Permutation(i) > 0 ) THEN
                    Solver % Variable % PrevValues( DOFs*Permutation(i) - k,j ) =  &
                        Work( DOFs * Solver % Variable % Perm(i) - k )
                  END IF
              END DO
           END DO
        END DO
     END IF
     DEALLOCATE( Work )

     Solver % Variable % Perm = Permutation
     Solver % Variable % Solver => Solver

     DEALLOCATE( Permutation )
     CALL AllocateVector( Matrix % RHS, Matrix % NumberOfRows )

     IF ( ListGetLogical( Solver % Values, 'Eigen Analysis',Found ) ) THEN
        n = ListGetInteger( Solver % Values,  &
            'Eigen System Values', Found  )

        IF ( Found .AND. n > 0 ) THEN
           Solver % NOFEigenValues = n
           CALL AllocateVector( Solver % Variable % EigenValues,n )
           CALL AllocateArray( Solver % Variable % EigenVectors, n, &
                    SIZE( Solver % Variable % Values ) ) 

           Solver % Variable % EigenValues  = 0.0d0
           Solver % Variable % EigenVectors = 0.0d0

           CALL AllocateVector( Matrix % MassValues, SIZE(Matrix % Values) )
           Matrix % MassValues = 0.0d0
        END IF
     ELSE IF ( ASSOCIATED( Solver % Matrix % Force ) ) THEN
        n1 = SIZE( Solver % Matrix % Force,1 )
        n2 = SIZE( Solver % Matrix % Force,2 )
        CALL AllocateArray( Matrix % Force,n1,n2 )
        Matrix % Force = 0.0d0

        CALL AllocateVector(Matrix % MassValues, n1)
        Matrix % MassValues(:) = 1.0d0
     END IF

     Solver % Matrix => Matrix
     Solver % Mesh % Changed = .TRUE.

!------------------------------------------------------------------------------
  END SUBROUTINE UpdateSolverMesh
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  FUNCTION SplitMeshEqual( Mesh ) RESULT( NewMesh )
DLLEXPORT SplitMeshEqual
!------------------------------------------------------------------------------
!   h/2 split of  given mesh, currently works only
!   for linear and bilinear elements
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh, NewMesh
!------------------------------------------------------------------------------
    REAL(KIND=dp), POINTER :: u(:),v(:),w(:),x(:),y(:),z(:)
    INTEGER :: i, j, k, n, NewElCnt, NodeCnt, EdgeCnt, Node, ParentId, Diag
    LOGICAL :: Found
    TYPE(Element_t), POINTER :: Enew,Eold,Edge,Eptr,Eparent,Face,Faces(:)
    INTEGER, POINTER :: Child(:,:)
    INTEGER :: n1,n2,n3,EoldNodes(4),FaceNodes(4),EdgeNodes(2) ! Only linears so far
    INTEGER :: FaceNumber,Edge1,Edge2,Edge3,Edge4,Node12,Node23,Node34,Node41,Node31
    REAL(KIND=dp) :: dxyz(3,3),Dist(3)
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Mesh ) ) RETURN

    NewMesh => AllocateMesh()

    CALL FindMeshEdges( Mesh )

    CALL Info( 'SplitMeshEqual', '******** Old mesh ********', Level = 6 )
    WRITE( Message, * ) 'Nodes             : ',Mesh % NumberOfNodes
    CALL info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Bulk elements     : ',Mesh % NumberOfBulkElements
    CALL info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Boundary elements : ',Mesh % NumberOfBoundaryElements
    CALL info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Edges             : ',Mesh % NumberOfEdges
    CALL info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Faces             : ',Mesh % NumberOfFaces
    CALL info( 'SplitMeshEqual', Message, Level=6 )
!
!   Update nodal coordinates:
!   -------------------------
    NodeCnt = Mesh % NumberOfNodes + Mesh % NumberOfEdges
!
!   For bricks, count faces:
!   ------------------------
    DO i = 1, Mesh % NumberOfFaces
       Face => Mesh % Faces(i)
       IF( Face % TYPE % NumberOfNodes == 4 ) NodeCnt = NodeCnt+1
    END DO
!
!   For quads and bricks, count centerpoints:
!   -----------------------------------------
    DO i=1,Mesh % NumberOfBulkElements
       Eold => Mesh % Elements(i)
       SELECT CASE( Eold % TYPE % ElementCode / 100 )
       CASE(4,8)
          NodeCnt = NodeCnt + 1
       END SELECT
    END DO
!
!   new mesh nodecoordinate arrays:
!   -------------------------------
    CALL AllocateVector( NewMesh % Nodes % x, NodeCnt )
    CALL AllocateVector( NewMesh % Nodes % y, NodeCnt )
    CALL AllocateVector( NewMesh % Nodes % z, NodeCnt )

!   shortcuts (u,v,w) old mesh  nodes,
!   (x,y,z) new mesh nodes:
!   ----------------------------------
    u => Mesh % Nodes % x
    v => Mesh % Nodes % y
    w => Mesh % Nodes % z

    x => NewMesh % Nodes % x
    y => NewMesh % Nodes % y
    z => NewMesh % Nodes % z
!
!   new mesh includes old mesh nodes:
!   ----------------------------------
    u => Mesh % Nodes % x
    x(1:Mesh % NumberOfNodes) = u
    y(1:Mesh % NumberOfNodes) = v
    z(1:Mesh % NumberOfNodes) = w
!
!   add edge centers:
!   -----------------
    DO i=1,Mesh % NumberOfEdges
       Edge => Mesh % Edges(i)
       k = Edge % TYPE % NumberOfNodes
       j = i + Mesh % NumberOfNodes
       x(j) = SUM(u(Edge % NodeIndexes)) / k
       y(j) = SUM(v(Edge % NodeIndexes)) / k
       z(j) = SUM(w(Edge % NodeIndexes)) / k
    END DO
!
!   add face centers for bricks:
!   ----------------------------
    DO i=1,Mesh % NumberOfFaces
       Face => Mesh % Faces(i)
       k = Face % TYPE % NumberOfNodes
       IF( k == 4 ) THEN
          j = i + Mesh % NumberOfNodes + Mesh % NumberOfEdges
          x(j) = SUM(u(Face % NodeIndexes)) / k
          y(j) = SUM(v(Face % NodeIndexes)) / k
          z(j) = SUM(w(Face % NodeIndexes)) / k
       END IF
    END DO
!
!   add centerpoint for quad & bricks:
!   ----------------------------------
    DO i=1,Mesh % NumberOfBulkElements
       Eold => Mesh % Elements(i)
       k = Eold % TYPE % NumberOfNodes
       SELECT CASE( Eold % TYPE % ElementCode / 100 )
       CASE(4,8)
          j = j + 1
          x(j) = SUM( u(Eold % NodeIndexes) ) / k
          y(j) = SUM( v(Eold % NodeIndexes) ) / k
          z(j) = SUM( w(Eold % NodeIndexes) ) / k
       END SELECT
    END DO
!
!   Update new mesh node count:
!   ---------------------------
    NewMesh % NumberOfNodes = NodeCnt
    NewMesh % Nodes % NumberOfNodes = NodeCnt
!
!   Update bulk elements:
!   =====================
!
!   First count new elements:
!   -------------------------
    NewElCnt = 0
    DO i=1, Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements
       Eold => Mesh % Elements(i)
       SELECT CASE( Eold % TYPE % ElementCode/100 )

!      Each element will be divided into 2**Dim new elements:
!      ------------------------------------------------------
       CASE(2)
          NewElCnt = NewElCnt + 2 ! lines
       CASE(3)
          NewElCnt = NewElCnt + 4 ! trias
       CASE(4)
          NewElCnt = NewElCnt + 4 ! quads
       CASE(5)
          NewElCnt = NewElCnt + 8 ! tetras
       CASE(8)
          NewElCnt = NewElCnt + 8 ! hexas
       END SELECT

    END DO
    CALL AllocateVector( NewMesh % Elements, NewElCnt )

    CALL AllocateArray( Child, Mesh % NumberOfBulkElements, 8 )
    NewElCnt = 0
    NodeCnt = Mesh % NumberOfNodes
    EdgeCnt = Mesh % NumberOfEdges
!
!   Index to old quad/hexa centerpoint node in the new mesh nodal arrays:
!   ---------------------------------------------------------------------
    Node = NodeCnt + Mesh % NumberOfEdges + Mesh % NumberOfFaces
!
!   Now update all new mesh elements:
!   ---------------------------------
    DO i=1,Mesh % NumberOfBulkElements

       Eold => Mesh % Elements(i)

       SELECT CASE( Eold % TYPE % ElementCode )
       CASE(303)
!
!         Split triangle to four triangles from
!         edge centerpoints:
!         --------------------------------------
!
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Child(i,1) = NewElCnt
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 3)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Child(i,2) = NewElCnt
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 3)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
!
!         3rd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Child(i,3) = NewElCnt
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 3)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
!
!         4th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Child(i,4) = NewElCnt
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 3)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(2) = Eold % NodeIndexes(3)
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt

       CASE(404)
!
!         Index to old quad centerpoint node in the
!         new mesh nodal arrays:
!         ------------------------------------------
          Node = Node + 1
!
!         Split quad to four new quads from edge
!         centerpoints and centerpoint of the
!         element:
!         --------------------------------------
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,1) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Node
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(4) + NodeCnt
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,2) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(4) = Node
!
!         3rd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,3) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Node
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % NodeIndexes(3)
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
!
!         4th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,4) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(2) = Node
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(4) = Eold % NodeIndexes(4)


       CASE(504)
!
!         Split tetra to 8 new elements from
!         corners and edge centerpoints:
!         ----------------------------------
!
!         1st new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,1) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(4) + NodeCnt
!
!         2nd new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,2) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(2) + NodeCnt
!
!         3rd new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,3) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(3)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
!
!         4th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,4) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(4)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(5) + NodeCnt

!         Then the annoying part; we still have to split the
!         remaining octahedron into four elements. This can
!         be done in three ways of which only one preserves
!         the minimum angle condition (Delaunay splitting):
!         --------------------------------------------------
          dxyz(1,1) = x(Eold % EdgeIndexes(4) + NodeCnt) &
                    - x(Eold % EdgeIndexes(2) + NodeCnt)
          dxyz(2,1) = y(Eold % EdgeIndexes(4) + NodeCnt) &
                    - y(Eold % EdgeIndexes(2) + NodeCnt)
          dxyz(3,1) = z(Eold % EdgeIndexes(4) + NodeCnt) &
                    - z(Eold % EdgeIndexes(2) + NodeCnt)

          dxyz(1,2) = x(Eold % EdgeIndexes(5) + NodeCnt) &
                    - x(Eold % EdgeIndexes(3) + NodeCnt)
          dxyz(2,2) = y(Eold % EdgeIndexes(5) + NodeCnt) &
                    - y(Eold % EdgeIndexes(3) + NodeCnt)
          dxyz(3,2) = z(Eold % EdgeIndexes(5) + NodeCnt) &
                    - z(Eold % EdgeIndexes(3) + NodeCnt)

          dxyz(1,3) = x(Eold % EdgeIndexes(6) + NodeCnt) &
                    - x(Eold % EdgeIndexes(1) + NodeCnt)
          dxyz(2,3) = y(Eold % EdgeIndexes(6) + NodeCnt) &
                    - y(Eold % EdgeIndexes(1) + NodeCnt)
          dxyz(3,3) = z(Eold % EdgeIndexes(6) + NodeCnt) &
                    - z(Eold % EdgeIndexes(1) + NodeCnt)

          Dist(1) = SQRT( dxyz(1,1)**2 + dxyz(2,1)**2 + dxyz(3,1)**2 )
          Dist(2) = SQRT( dxyz(1,2)**2 + dxyz(2,2)**2 + dxyz(3,2)**2 )
          Dist(3) = SQRT( dxyz(1,3)**2 + dxyz(2,3)**2 + dxyz(3,3)**2 )

          Diag = 1  ! The default diagonal for splitting is between edges 2-4
          IF (Dist(2) < Dist(1) .AND. Dist(2) < Dist(3)) Diag = 2 ! Edges 3-5
          IF (Dist(3) < Dist(1) .AND. Dist(3) < Dist(2)) Diag = 3 ! Edges 1-6

          SELECT CASE( Diag )
          CASE(1)
!
!         5th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,5) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(2) + NodeCnt
!
!         6th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,6) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
!
!         7th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,7) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(1) + NodeCnt
!
!         8th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,8) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(2) + NodeCnt
!
          CASE(2)
!
!         5th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,5) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
!
!         6th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,6) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
!
!         7th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,7) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(5) + NodeCnt
!
!         8th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,8) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(5) + NodeCnt
!
          CASE(3)
!
!         5th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,5) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(1) + NodeCnt
!
!         6th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,6) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(1) + NodeCnt
!
!         7th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,7) = NewElCnt 
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(6) + NodeCnt
!
!         8th new element:
!         ----------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,8) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 4)
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(6) + NodeCnt

          END SELECT

       CASE(808)
!
!         Index to old quad centerpoint node in the
!         new mesh nodal arrays:
!         ------------------------------------------
          Node = Node + 1
!
!         Split brick to 8 new bricks from edge
!         centerpoints and centerpoint of the
!         element:
!         --------------------------------------
!
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,1) = NewElCnt
          Enew = Eold
          CALL  AllocateVector( ENew % NodeIndexes, 8)
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(3) = Eold % FaceIndexes(1) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(5) = Eold % EdgeIndexes(9) + NodeCnt
          Enew % NodeIndexes(6) = Eold % FaceIndexes(3) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(7) = Node
          Enew % NodeIndexes(8) = Eold % FaceIndexes(6) + NodeCnt + EdgeCnt
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,2) = NewElCnt
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(1) + NodeCnt
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(4) = Eold % FaceIndexes(1) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(5) = Eold % FaceIndexes(3) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(6) = Eold % EdgeIndexes(10)+ NodeCnt
          Enew % NodeIndexes(7) = Eold % FaceIndexes(4) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(8) = Node
!
!         3rd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,3) = NewElCnt
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(4) + NodeCnt
          Enew % NodeIndexes(2) = Eold % FaceIndexes(1) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(4) = Eold % NodeIndexes(4)
          Enew % NodeIndexes(5) = Eold % FaceIndexes(6) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(6) = Node
          Enew % NodeIndexes(7) = Eold % FaceIndexes(5) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(8) = Eold % EdgeIndexes(12)+ NodeCnt
!
!         4th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,4) = NewElCnt 
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % FaceIndexes(1) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(2) + NodeCnt
          Enew % NodeIndexes(3) = Eold % NodeIndexes(3)
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(3) + NodeCnt
          Enew % NodeIndexes(5) = Node
          Enew % NodeIndexes(6) = Eold % FaceIndexes(4) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(7) = Eold % EdgeIndexes(11)+ NodeCnt
          Enew % NodeIndexes(8) = Eold % FaceIndexes(5) + NodeCnt + EdgeCnt
!
!         5th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,5) = NewElCnt 
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % EdgeIndexes(9) + NodeCnt
          Enew % NodeIndexes(2) = Eold % FaceIndexes(3) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(3) = Node
          Enew % NodeIndexes(4) = Eold % FaceIndexes(6) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(5) = Eold % NodeIndexes(5)
          Enew % NodeIndexes(6) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(7) = Eold % FaceIndexes(2) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(8) = Eold % EdgeIndexes(8) + NodeCnt
!
!         6th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,6) = NewElCnt 
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % FaceIndexes(3) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(2) = Eold % EdgeIndexes(10)+ NodeCnt
          Enew % NodeIndexes(3) = Eold % FaceIndexes(4) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(4) = Node
          Enew % NodeIndexes(5) = Eold % EdgeIndexes(5) + NodeCnt
          Enew % NodeIndexes(6) = Eold % NodeIndexes(6)
          Enew % NodeIndexes(7) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(8) = Eold % FaceIndexes(2) + NodeCnt + EdgeCnt
!
!         7th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,7) = NewElCnt 
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Eold % FaceIndexes(6) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(2) = Node
          Enew % NodeIndexes(3) = Eold % FaceIndexes(5) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(4) = Eold % EdgeIndexes(12)+ NodeCnt
          Enew % NodeIndexes(5) = Eold % EdgeIndexes(8) + NodeCnt
          Enew % NodeIndexes(6) = Eold % FaceIndexes(2) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(7) = Eold % EdgeIndexes(7) + NodeCnt
          Enew % NodeIndexes(8) = Eold % NodeIndexes(8)
!
!         8th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Child(i,8) = NewElCnt
          Enew = Eold
          CALL AllocateVector( ENew % NodeIndexes, 8 )
          Enew % NodeIndexes(1) = Node
          Enew % NodeIndexes(2) = Eold % FaceIndexes(4) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(3) = Eold % EdgeIndexes(11)+ NodeCnt
          Enew % NodeIndexes(4) = Eold % FaceIndexes(5) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(5) = Eold % FaceIndexes(2) + NodeCnt + EdgeCnt
          Enew % NodeIndexes(6) = Eold % EdgeIndexes(6) + NodeCnt
          Enew % NodeIndexes(7) = Eold % NodeIndexes(7)
          Enew % NodeIndexes(8) = Eold % EdgeIndexes(7) + NodeCnt

       CASE DEFAULT
          WRITE( Message,* ) 'Element type ', Eold % TYPE % ElementCode, &
              ' not supprted by the multigrid solver.'
          CALL Fatal( 'SplitMeshEqual', Message )
       END SELECT
    END DO

!
!   Update new mesh element counts:
!   -------------------------------
    NewMesh % NumberOfBulkElements = NewElCnt

!
!   Update boundary elements:
!   NOTE: Internal boundaries not taken care of...:!!!!
!   ---------------------------------------------------
    DO i=1,Mesh % NumberOfBoundaryElements

       j = i + Mesh % NumberOfBulkElements
       Eold => Mesh % Elements(j)
!
!      get parent of the boundary element:
!      -----------------------------------
       ParentId = Eold % BoundaryInfo % LElement
       IF ( ParentId <= 0 ) THEN
          ParentId = Eold % BoundaryInfo % RElement
       END IF

       NULLIFY( Eparent )
       IF ( ParentId > 0 ) Eparent => Mesh % Elements(ParentId)

       IF ( .NOT. ASSOCIATED( Eparent ) ) CYCLE

       SELECT CASE( Eold % TYPE % ElementCode / 100 )
       CASE(2)
!
!         Line segments:
!         ==============
!
!         which edge of the parent element are we ?
!         -----------------------------------------
          DO Edge1=1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge1) )
             IF ( Eold % NodeIndexes(1) == Edge % NodeIndexes(1) .AND. &
                  Eold % NodeIndexes(2) == Edge % NodeIndexes(2) .OR.  &
                  Eold % NodeIndexes(2) == Edge % NodeIndexes(1) .AND. &
                  Eold % NodeIndexes(1) == Edge % NodeIndexes(2) ) EXIT
          END DO
!
!         index of the old edge centerpoint in the
!         new mesh nodal arrays:
!         ----------------------------------------
          Node = Eparent % EdgeIndexes(Edge1) + Mesh % NumberOfNodes
!
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 2 )
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Node
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,4
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             Found = .FALSE.
             DO k=1,n-1
                IF ( Enew % NodeIndexes(1) == Eptr % NodeIndexes(k)   .AND. &
                     Enew % NodeIndexes(2) == Eptr % NodeIndexes(k+1) .OR.  &
                     Enew % NodeIndexes(2) == Eptr % NodeIndexes(k)   .AND. &
                     Enew % NodeIndexes(1) == Eptr % NodeIndexes(k+1) ) THEN
                   Found = .TRUE.
                   EXIT
                END IF
             END DO
             IF ( Enew % NodeIndexes(1) == Eptr % NodeIndexes(n) .AND. &
                  Enew % NodeIndexes(2) == Eptr % NodeIndexes(1) .OR.  &
                  Enew % NodeIndexes(2) == Eptr % NodeIndexes(n) .AND. &
                  Enew % NodeIndexes(1) == Eptr % NodeIndexes(1) ) THEN
                Found = .TRUE.
             END IF
             IF ( Found ) EXIT
          END DO
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 2 )
          Enew % NodeIndexes(1) = Node
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,4
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             Found = .FALSE.
             DO k=1,n-1
                IF ( Enew % NodeIndexes(1) == Eptr % NodeIndexes(k)   .AND. &
                     Enew % NodeIndexes(2) == Eptr % NodeIndexes(k+1) .OR.  &
                     Enew % NodeIndexes(2) == Eptr % NodeIndexes(k)   .AND. &
                     Enew % NodeIndexes(1) == Eptr % NodeIndexes(k+1) ) THEN
                   Found = .TRUE.
                   EXIT
                END IF
             END DO
             IF ( Enew % NodeIndexes(1) == Eptr % NodeIndexes(n) .AND. &
                  Enew % NodeIndexes(2) == Eptr % NodeIndexes(1) .OR.  &
                  Enew % NodeIndexes(2) == Eptr % NodeIndexes(n) .AND. &
                  Enew % NodeIndexes(1) == Eptr % NodeIndexes(1) ) THEN
                Found = .TRUE.
             END IF
             IF ( Found ) EXIT
          END DO
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)

       CASE(3)
!
!         Trias:
!         ======
!
!         On which face of the parent element are we ?
!         --------------------------------------------
          EoldNodes(1:3) = Eold % NodeIndexes(1:3)
          CALL sort( 3, EoldNodes )

          DO FaceNumber = 1, SIZE( Eparent % FaceIndexes )
             Face => Mesh % Faces( Eparent % FaceIndexes(FaceNumber) )
             FaceNodes(1:3) = Face % NodeIndexes(1:3)
             CALL sort( 3, FaceNodes )

             IF ( EoldNodes(1) == FaceNodes(1) .AND. &
                  EoldNodes(2) == FaceNodes(2) .AND. &
                  EoldNodes(3) == FaceNodes(3) ) EXIT

          END DO
!
!         Then, what are the edges on this face?
!         --------------------------------------
!
!         First edge:
!         -----------
          EoldNodes(1) = MIN( Eold % NodeIndexes(1), Eold % NodeIndexes(2) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(1), Eold % NodeIndexes(2) )
          DO Edge1 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge1) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO

!         Second edge:
!         ------------
          EoldNodes(1) = MIN( Eold % NodeIndexes(2), Eold % NodeIndexes(3) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(2), Eold % NodeIndexes(3) )
          DO Edge2 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge2) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO

!         Third edge:
!         -----------
          EoldNodes(1) = MIN( Eold % NodeIndexes(3), Eold % NodeIndexes(1) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(3), Eold % NodeIndexes(1) )
          DO Edge3 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge3) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO
!
!         index of the old face and edge centerpoints
!         in the new mesh nodal arrays:
!         ----------------------------------------
          Node12 = Eparent % EdgeIndexes(Edge1) + Mesh % NumberOfNodes
          Node23 = Eparent % EdgeIndexes(Edge2) + Mesh % NumberOfNodes
          Node31 = Eparent % EdgeIndexes(Edge3) + Mesh % NumberOfNodes
!
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 3 )
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Node12
          Enew % NodeIndexes(3) = Node31
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,3
                DO n2 = 1,4
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 3 )
          Enew % NodeIndexes(1) = Node12
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(3) = Node23
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,3
                DO n2 = 1,4
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         3rd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 3 )
          Enew % NodeIndexes(1) = Node12
          Enew % NodeIndexes(2) = Node23
          Enew % NodeIndexes(3) = Node31
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,3
                DO n2 = 1,4
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         4th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 3 )
          Enew % NodeIndexes(1) = Node31
          Enew % NodeIndexes(2) = Node23
          Enew % NodeIndexes(3) = Eold % NodeIndexes(3)
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,3
                DO n2 = 1,4
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)

       CASE(4)
!
!         Quads:
!         ======
!
!         On which face of the parent element are we ?
!         --------------------------------------------
          EoldNodes(1:4) = Eold % NodeIndexes(1:4)
          CALL sort( 4, EoldNodes )

          DO FaceNumber = 1, SIZE( Eparent % FaceIndexes )
             Face => Mesh % Faces( Eparent % FaceIndexes(FaceNumber) )
             FaceNodes(1:4) = Face % NodeIndexes(1:4)
             CALL sort( 4, FaceNodes )

             IF ( EoldNodes(1) == FaceNodes(1) .AND. &
                  EoldNodes(2) == FaceNodes(2) .AND. &
                  EoldNodes(3) == FaceNodes(3) ) EXIT

          END DO

!         Then, what are the edges on this face?
!         --------------------------------------
!
!         First edge:
!         -----------
          EoldNodes(1) = MIN( Eold % NodeIndexes(1), Eold % NodeIndexes(2) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(1), Eold % NodeIndexes(2) )
          DO Edge1 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge1) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO

!         Second edge:
!         ------------
          EoldNodes(1) = MIN( Eold % NodeIndexes(2), Eold % NodeIndexes(3) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(2), Eold % NodeIndexes(3) )
          DO Edge2 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge2) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO

!         Third edge:
!         -----------
          EoldNodes(1) = MIN( Eold % NodeIndexes(3), Eold % NodeIndexes(4) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(3), Eold % NodeIndexes(4) )
          DO Edge3 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge3) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO

!         Fourth edge:
!         -----------
          EoldNodes(1) = MIN( Eold % NodeIndexes(4), Eold % NodeIndexes(1) )
          EoldNodes(2) = MAX( Eold % NodeIndexes(4), Eold % NodeIndexes(1) )
          DO Edge4 = 1,SIZE(Eparent % EdgeIndexes)
             Edge => Mesh % Edges( Eparent % EdgeIndexes(Edge4) )
             EdgeNodes(1) = MIN( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             EdgeNodes(2) = MAX( Edge % NodeIndexes(1), Edge % NodeIndexes(2) )
             IF ( EoldNodes(1) == EdgeNodes(1) .AND. &
                  EoldNodes(2) == EdgeNodes(2) ) EXIT
          END DO
!
!         index of the old face and edge centerpoints
!         in the new mesh nodal arrays:
!         ----------------------------------------
          Node = Eparent % FaceIndexes(FaceNumber) & ! faces mid-point
               + Mesh % NumberOfNodes + Mesh % NumberOfEdges
          Node12 = Eparent % EdgeIndexes(Edge1) + Mesh % NumberOfNodes
          Node23 = Eparent % EdgeIndexes(Edge2) + Mesh % NumberOfNodes
          Node34 = Eparent % EdgeIndexes(Edge3) + Mesh % NumberOfNodes
          Node41 = Eparent % EdgeIndexes(Edge4) + Mesh % NumberOfNodes
!
!         1st new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 4 )
          Enew % NodeIndexes(1) = Eold % NodeIndexes(1)
          Enew % NodeIndexes(2) = Node12
          Enew % NodeIndexes(3) = Node
          Enew % NodeIndexes(4) = Node41
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,4
                DO n2 = 1,8
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 )  CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         2nd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 4 )
          Enew % NodeIndexes(1) = Node12
          Enew % NodeIndexes(2) = Eold % NodeIndexes(2)
          Enew % NodeIndexes(3) = Node23
          Enew % NodeIndexes(4) = Node
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,4
                DO n2 = 1,8
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         3rd new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 4 )
          Enew % NodeIndexes(1) = Node41
          Enew % NodeIndexes(2) = Node
          Enew % NodeIndexes(3) = Node34
          Enew % NodeIndexes(4) = Eold % NodeIndexes(4)
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,4
                DO n2 = 1,8
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
!
!         4th new element
!         ---------------
          NewElCnt = NewElCnt + 1
          Enew => NewMesh % Elements(NewElCnt)
          Enew = Eold
          CALL AllocateVector( Enew % NodeIndexes, 4 )
          Enew % NodeIndexes(1) = Node
          Enew % NodeIndexes(2) = Node23
          Enew % NodeIndexes(3) = Eold % NodeIndexes(3)
          Enew % NodeIndexes(4) = Node34
          ALLOCATE( Enew % BoundaryInfo )
          Enew % BoundaryInfo = Eold % BoundaryInfo
          Enew % BoundaryInfo % Lelement = 0
          Enew % BoundaryInfo % Relement = 0
          NULLIFY( Enew % BoundaryInfo % Left )
          NULLIFY( Enew % BoundaryInfo % Right )
!
!         Search the new mesh parent element among the
!         children of the old mesh parent element:
!         --------------------------------------------
          DO j=1,8
             Eptr => NewMesh % Elements( Child(ParentId,j) )
             n = Eptr % TYPE % NumberOfNodes
             n3 = 0 ! Count matches (metodo stupido)
             DO n1 = 1,4
                DO n2 = 1,8
                   IF( Enew % NodeIndexes(n1) == Eptr % NodeIndexes(n2) ) n3 = n3+1
                END DO
             END DO
             IF ( n3 > 2 ) EXIT
          END DO
          IF( n3 < 3 ) CALL Error( 'SplitMeshEqual', 'Parent element not found' )
          Enew % BoundaryInfo % Left => Eptr
          Enew % BoundaryInfo % LElement = Child(ParentId,j)
       END SELECT
    END DO

!
!   Update new mesh boundary element counts:
!   ----------------------------------------
    NewMesh % NumberOfBoundaryElements = NewElCnt - &
            NewMesh % NumberOfBulkElements
    NewMesh % MaxElementDOFs  = Mesh % MaxElementDOFs
    NewMesh % MaxElementNodes = Mesh % MaxElementNodes

    DO i=1,NewMesh % NumberOfBulkElements+NewMesh % NumberOfBoundaryElements
      NULLIFY( NewMesh % Elements(i) % EdgeIndexes )
      NULLIFY( NewMesh % Elements(i) % FaceIndexes )
    END DO

    CALL Info( 'SplitMeshEqual', '******** New mesh ********', Level=6 )
    WRITE( Message, * ) 'Nodes             : ',NewMesh % NumberOfNodes
    CALL Info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Bulk elements     : ',NewMesh % NumberOfBulkElements
    CALL Info( 'SplitMeshEqual', Message, Level=6 )
    WRITE( Message, * ) 'Boundary elements : ',NewMesh % NumberOfBoundaryElements
    CALL Info( 'SplitMeshEqual', Message, Level=6 )

!
!   Update structures needed for parallel execution:
!   ------------------------------------------------
    CALL UpdateParallelMesh( Mesh, NewMesh )
!
!   If periodic BC given, compute boundary mesh projector:
!   ------------------------------------------------------
!
    DO i = 1,CurrentModel % NumberOfBCs
       k = ListGetInteger(CurrentModel % BCs(i) % Values, 'Periodic BC', Found, &
               minv=1, maxv=CurrentModel % NumberOFBCs )
       CurrentModel % BCs(i) % PMatrix => &
                 PeriodicProjector( CurrentModel, Mesh, i, k )
    END DO
!
!   Finalize:
!   ---------
    DEALLOCATE( Child )
    CALL ReleaseMeshEdgeTables( Mesh )
    CALL ReleaseMeshFaceTables( Mesh )

!call writemeshtodisk( NewMesh, "." )
!stop
CONTAINS

!------------------------------------------------------------------------------
    SUBROUTINE UpdateParallelMesh( Mesh, NewMesh )
!------------------------------------------------------------------------------
       TYPE(Mesh_t), POINTER :: Mesh, NewMesh
!------------------------------------------------------------------------------
       TYPE(Element_t), POINTER :: Edge, Face
       INTEGER :: i,j,k,n
       INTEGER, POINTER :: IntCnts(:),IntArray(:),Reorder(:)

       INTEGER :: jedges
!------------------------------------------------------------------------------

       IF ( ParEnv % PEs <= 1 ) RETURN
!
!      Update mesh interfaces for parallel execution.
!      ==============================================
!
!      Try to get an agreement about the  global numbering
!      of new mesh nodes among set of processes solving
!      this specific eq. Also allocate and generate
!      all other control information needed in parallel
!      execution:
!      ----------------------------------------------------
       n = NewMesh % NumberOfNodes
       ALLOCATE( NewMesh % Nodes % NeighbourList(n) )
       CALL AllocateVector( NewMesh % Nodes % INTERFACE,n  )
       CALL AllocateVector( NewMesh % Nodes % GlobalNodeNumber,n )

       DO i=1,n
          NULLIFY( NewMesh % Nodes % NeighbourList(i) % Neighbours )
       END DO

       n = Mesh % NumberOfNodes
       NewMesh % Nodes % INTERFACE = .FALSE.
       NewMesh % Nodes % INTERFACE(1:n) = Mesh % Nodes % INTERFACE

       NewMesh % Nodes % GlobalNodeNumber = 0
       NewMesh % Nodes % GlobalNodeNumber(1:n) = &
           Mesh % Nodes % GlobalNodeNumber
!
!      My theory is, that a new node will be an
!      interface node only if all the edge or face
!      nodes which contribute to its existence are
!      interface nodes (the code immediately below
!      will only count sizes):
!      -------------------------------------------

       j = 0 ! Count interface elements
       k = 0 ! For memory allocation
       DO i = 1,Mesh % NumberOfEdges
          Edge => Mesh % Edges(i)
          IF ( ALL(Mesh % Nodes % INTERFACE(Edge % NodeIndexes)) ) THEN
             NewMesh % Nodes % INTERFACE(Mesh % NumberOfNodes+i) = .TRUE.
             j = j + 1
             k = k + Edge % TYPE % NumberOfNodes
          END IF
       END DO

!      print *,'Found',j,'interface edges'
       jedges = j

!      For bricks, check also the faces:
!      ---------------------------------
       DO i = 1,Mesh % NumberOfFaces
          Face => Mesh % Faces(i) 
          IF( Face % TYPE % NumberOfNodes == 4 ) THEN
             IF ( ALL( Mesh % Nodes % INTERFACE( Face % NodeIndexes ) ) ) THEN
                NewMesh % Nodes % INTERFACE( Mesh % NumberOfNodes &
                     + Mesh % NumberOfEdges + i ) = .TRUE.
                j = j + 1
                k = k + Face % TYPE % NumberOfNodes
             END IF
          END IF
       END DO

!      print*,'Found',j-jedges,'interface faces'

       CALL AllocateVector( IntCnts,  j )
       CALL AllocateVector( IntArray, k )
!
!      Old mesh nodes were copied as is...
!      -----------------------------------
       DO i=1,Mesh % NumberOfNodes
          CALL AllocateVector( NewMesh % Nodes % NeighbourList(i) % Neighbours, &
             SIZE( Mesh % Nodes % Neighbourlist(i) % Neighbours ) )

          NewMesh % Nodes % NeighbourList(i) % Neighbours = &
             Mesh % Nodes % NeighbourList(i) % Neighbours
       END DO
!
!      Take care of the new mesh internal nodes.
!      Parallel global numbering will take care
!      of the interface nodes:
!      ----------------------------------------
       DO i=Mesh % NumberOfNodes+1, NewMesh % NumberOfNodes
          IF ( .NOT. NewMesh % Nodes % INTERFACE(i) ) THEN
             CALL AllocateVector( NewMesh % Nodes % NeighbourList(i) % Neighbours,1 )
             NewMesh % Nodes % NeighbourList(i) %  Neighbours(1) = ParEnv % MyPE
          END IF
       END DO
!
!      Copy global indices of edge and/or face nodes
!      to temporary work arrays:
!      ---------------------------------------------
!
       j = 0
       k = 0
       DO i = 1,Mesh % NumberOfEdges
          Edge => Mesh % Edges(i)
          IF ( ALL(Mesh % Nodes % INTERFACE(Edge % NodeIndexes)) ) THEN
             j = j + 1
             IntCnts(j) = Edge % TYPE % NumberOfNodes
             IntArray( k+1:k+IntCnts(j) ) = &
                Mesh % Nodes % GlobalNodeNumber(Edge % NodeIndexes)
             CALL Sort( IntCnts(j), IntArray(k+1:k+IntCnts(j)) )
             k = k + IntCnts(j)
          END IF
       END DO
!
!      For bricks, check also the faces:
!      ---------------------------------
       DO i = 1,Mesh % NumberOfFaces
          Face => Mesh % Faces(i)
          IF( Face % TYPE % NumberOfNodes == 4 ) THEN
             IF ( ALL( Mesh % Nodes % INTERFACE(Face % NodeIndexes) ) ) THEN
                j = j + 1
                IntCnts(j) = Face % TYPE % NumberOfNodes
                IntArray(k+1:k+IntCnts(j)) = &
                     Mesh % Nodes % GlobalNodeNumber(Face % NodeIndexes)
                CALL Sort( IntCnts(j), IntArray(k+1:k+IntCnts(j)) )
                k = k + IntCnts(j)
             END IF
          END IF
       END DO
!
!      Finally the beef, do the exchange of new
!      interfaces. The parallel global numbering
!      subroutine will also do reordering of the
!      nodes, hence the reorder array:
!      -------------------------------------------
       CALL AllocateVector( Reorder, NewMesh % NumberOfNodes )
       Reorder = (/ (i, i=1,NewMesh % NumberOfNodes) /)

       k = NewMesh % Nodes % NumberOfNodes - Mesh % Nodes % NumberOfNodes
       CALL ParallelGlobalNumbering( NewMesh % Nodes, k, &
                 IntCnts, IntArray, Reorder )

!      Account for the reordering of the nodes:
!      ----------------------------------------
       DO i=1,NewMesh % NumberOfBulkElements + &
                        NewMesh % NumberOfBoundaryElements
          NewMesh % Elements(i) % NodeIndexes = &
              Reorder( NewMesh % Elements(i) % NodeIndexes )
       END DO

       DEALLOCATE( IntCnts, IntArray, Reorder )
!------------------------------------------------------------------------------
    END SUBROUTINE UpdateParallelMesh
!------------------------------------------------------------------------------
  END FUNCTION SplitMeshEqual
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE ReleaseVariableList( VariableList )
!------------------------------------------------------------------------------
    TYPE(Variable_t), POINTER :: VariableList
!------------------------------------------------------------------------------
    REAL(KIND=dp), POINTER :: Ptr(:)
    LOGICAL :: GotIt
    INTEGER :: i
    TYPE(Variable_t), POINTER :: Var, Var1
!------------------------------------------------------------------------------

    Var => VariableList
    DO WHILE( ASSOCIATED( Var ) ) 

       SELECT CASE( Var % Name )
         CASE( 'time', 'coordinate 1', 'coordinate 2', 'coordinate 3' )
            NULLIFY( Var % Values )
            NULLIFY( Var % Perm )
            NULLIFY( Var % PrevValues )
            Var => Var % Next
            CYCLE
       END SELECT

       IF ( Var % DOFs > 1 ) THEN
          Var => Var % Next
          CYCLE
       END IF
!
!      Check that the variable is actually allocated,
!      not pointer to some other variables memory:
!      ----------------------------------------------
       GotIt = .TRUE.
       Var1 => VariableList
       DO WHILE( ASSOCIATED( Var1 ) )
          IF ( Var % Name /= Var1 % Name ) THEN
             IF ( ASSOCIATED(Var1 % Values) ) THEN
                DO i=1,Var1 % DOFs
                   ptr => Var1 % Values(i::Var1 % DOFs)
                   IF ( ASSOCIATED(Var % Values,ptr) ) THEN
                      GotIt = .FALSE.
                      EXIT
                   END IF
                END DO
             END IF
          END IF

          IF ( .NOT. GotIt ) EXIT
          Var1 => Var1 % Next
       END DO

       IF ( GotIt ) THEN
          IF ( ASSOCIATED( Var % Values ) ) &
             DEALLOCATE( Var % Values )

          IF ( ASSOCIATED( Var % Perm ) ) &
             DEALLOCATE( Var % Perm )

          IF ( ASSOCIATED( Var % PrevValues ) ) &
             DEALLOCATE( Var % PrevValues )

          IF ( ASSOCIATED( Var % EigenValues ) ) &
             DEALLOCATE( Var % EigenValues )

          IF ( ASSOCIATED( Var % EigenVectors ) ) &
             DEALLOCATE( Var % EigenVectors )
       END IF
       NULLIFY( Var % EigenVectors, Var % EigenValues )
       NULLIFY( Var % Values, Var % PrevValues, Var % Perm )

       Var => Var % Next
    END DO

    Var => VariableList
    DO WHILE( ASSOCIATED( Var ) )
       IF ( Var % DOFs > 1 ) THEN
         IF ( ASSOCIATED( Var % Values ) ) &
            DEALLOCATE( Var % Values )

         IF ( ASSOCIATED( Var % Perm ) ) &
            DEALLOCATE( Var % Perm )

         IF ( ASSOCIATED( Var % PrevValues ) ) &
            DEALLOCATE( Var % PrevValues )

         IF ( ASSOCIATED( Var % EigenValues ) ) &
            DEALLOCATE( Var % EigenValues )

         IF ( ASSOCIATED( Var % EigenVectors ) ) &
            DEALLOCATE( Var % EigenVectors )
       END IF
       NULLIFY( Var % EigenVectors, Var % EigenValues )
       NULLIFY( Var % Values, Var % PrevValues, Var % Perm )
       Var => Var % Next
    END DO

!   Deallocate mesh variable list:
!   ------------------------------
    Var => VariableList
    DO WHILE( ASSOCIATED( Var ) )
       Var1 => Var % Next
       DEALLOCATE( Var )
       Var => Var1
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE ReleaseVariableList
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE ReleaseMesh( Mesh )
!------------------------------------------------------------------------------
     TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
     TYPE(Projector_t), POINTER :: Projector, Projector1
     TYPE(Variable_t), POINTER  :: Var, Var1
     INTEGER :: i,j,k
     LOGICAL :: GotIt
     REAL(KIND=dp), POINTER :: ptr(:)
!------------------------------------------------------------------------------
 
!    Deallocate mesh variables:
!    --------------------------
     CALL ReleaseVariableList( Mesh % Variables )
     NULLIFY( Mesh % Variables )

!    Deallocate mesh geometry (nodes,elements and edges):
!    ----------------------------------------------------
     IF ( ASSOCIATED( Mesh % Nodes ) ) THEN
        IF ( ASSOCIATED( Mesh % Nodes % x ) ) DEALLOCATE( Mesh % Nodes % x )
        IF ( ASSOCIATED( Mesh % Nodes % y ) ) DEALLOCATE( Mesh % Nodes % y )
        IF ( ASSOCIATED( Mesh % Nodes % z ) ) DEALLOCATE( Mesh % Nodes % z )

        IF ( ASSOCIATED( Mesh % Nodes % Perm ) ) &
           DEALLOCATE( Mesh % Nodes % Perm )

        IF ( ASSOCIATED( Mesh % Nodes % INVPerm ) ) &
           DEALLOCATE( Mesh % Nodes % INVPerm )

        IF ( ASSOCIATED( Mesh % Nodes % GlobalNodeNumber ) ) &
           DEALLOCATE( Mesh % Nodes % GlobalNodeNumber )

        DEALLOCATE( Mesh % Nodes )
     END IF
     NULLIFY( Mesh % Nodes )

     IF ( ASSOCIATED( Mesh % Edges ) ) CALL ReleaseMeshEdgeTables( Mesh )
     NULLIFY( Mesh % Edges )

! ML
     IF ( ASSOCIATED( Mesh % Faces ) ) CALL ReleaseMeshFaceTables( Mesh )
     NULLIFY( Mesh % Faces )

     IF ( ASSOCIATED( Mesh % Elements ) ) THEN
        DO i=1,Mesh % NumberOfBulkElements+Mesh % NumberOfBoundaryElements
!          Boundaryinfo structure for boundary elements
!          ---------------------------------------------
           IF ( i > Mesh % NumberOfBulkElements ) THEN
              IF ( ASSOCIATED( Mesh % Elements(i) % BoundaryInfo ) ) THEN
                 IF ( ASSOCIATED( Mesh % Elements(i) % BoundaryInfo % &
                    GebhardtFactors % Elements ) ) THEN
                    DEALLOCATE( Mesh % Elements(i) % BoundaryInfo % &
                      GebhardtFactors % Elements )
                    DEALLOCATE( Mesh % Elements(i) % BoundaryInfo % &
                      GebhardtFactors % Factors )
                 END IF
                 IF ( ASSOCIATED( Mesh % Elements(i) % BoundaryInfo % &
                    ViewFactors % Elements ) ) THEN
                    DEALLOCATE( Mesh % Elements(i) % BoundaryInfo % &
                      ViewFactors % Elements )
                    DEALLOCATE( Mesh % Elements(i) % BoundaryInfo % &
                      ViewFactors % Factors )
                 END IF
                 DEALLOCATE( Mesh % Elements(i) % BoundaryInfo )
              END IF
           END IF

           IF ( ASSOCIATED( Mesh % Elements(i) % NodeIndexes ) ) &
              DEALLOCATE( Mesh % Elements(i) % NodeIndexes )
           NULLIFY( Mesh % Elements(i) % NodeIndexes )

           IF ( ASSOCIATED( Mesh % Elements(i) % EdgeIndexes ) ) &
              DEALLOCATE( Mesh % Elements(i) % EdgeIndexes )
           NULLIFY( Mesh % Elements(i) % EdgeIndexes )

! ML
           IF ( ASSOCIATED( Mesh % Elements(i) % FaceIndexes ) ) &
              DEALLOCATE( Mesh % Elements(i) % FaceIndexes )
           NULLIFY( Mesh % Elements(i) % FaceIndexes )

           IF ( ASSOCIATED( Mesh % Elements(i) % DGIndexes ) ) &
              DEALLOCATE( Mesh % Elements(i) % DGIndexes )
           NULLIFY( Mesh % Elements(i) % DGIndexes )
        END DO
        DEALLOCATE( Mesh % Elements )
     END IF
     NULLIFY( Mesh % Elements )

!    Deallocate mesh to mesh projector structures:
!    ---------------------------------------------
     Projector => Mesh % Projector
     DO WHILE( ASSOCIATED( Projector ) )
        CALL FreeMatrix( Projector % Matrix )
        CALL FreeMatrix( Projector % TMatrix )
        Projector1 => Projector
        Projector => Projector % Next
        DEALLOCATE( Projector1 )
     END DO
     NULLIFY( Mesh % Projector )

!    Deallocate quadrant tree (used in mesh to mesh interpolation):
!    --------------------------------------------------------------
     CALL FreeQuadrantTree( Mesh % RootQuadrant )
     NULLIFY( Mesh % RootQuadrant )

!    DEALLOCATE( Mesh )

!------------------------------------------------------------------------------
  END SUBROUTINE ReleaseMesh
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE ReleaseMeshEdgeTables( Mesh )
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    INTEGER :: i
    TYPE(Element_t), POINTER :: Edge
!------------------------------------------------------------------------------
    IF ( ASSOCIATED( Mesh % Edges ) ) THEN
       DO i=1,Mesh % NumberOfEdges
          Edge => Mesh % Edges(i)
          IF ( ASSOCIATED( Edge % NodeIndexes ) ) THEN
             DEALLOCATE( Edge % NodeIndexes )
          END IF
          IF ( ASSOCIATED( Edge % BoundaryInfo ) ) THEN
             DEALLOCATE( Edge % BoundaryInfo )
          END IF
       END DO

       DEALLOCATE( Mesh % Edges )
    END IF
    NULLIFY( Mesh % Edges )
    Mesh % NumberOfEdges = 0

    DO i=1,Mesh % NumberOfBulkElements
       IF ( ASSOCIATED( Mesh % Elements(i) % EdgeIndexes ) ) THEN
          DEALLOCATE( Mesh % Elements(i) % EdgeIndexes )
          NULLIFY( Mesh % Elements(i) % EdgeIndexes )
       END IF
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE ReleaseMeshEdgeTables
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE ReleaseMeshFaceTables( Mesh )
!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh
!------------------------------------------------------------------------------
    INTEGER :: i
    TYPE(Element_t), POINTER :: Face
!------------------------------------------------------------------------------
    IF ( ASSOCIATED( Mesh % Faces ) ) THEN
       DO i=1,Mesh % NumberOfFaces
          Face => Mesh % Faces(i)
          IF ( ASSOCIATED( Face % NodeIndexes ) ) THEN
             DEALLOCATE( Face % NodeIndexes )
          END IF
          IF ( ASSOCIATED( Face % BoundaryInfo ) ) THEN
             DEALLOCATE( Face % BoundaryInfo )
          END IF
       END DO

       DEALLOCATE( Mesh % Faces )
    END IF
    NULLIFY( Mesh % Faces )
    Mesh % NumberOfFaces = 0

    DO i=1,Mesh % NumberOfBulkElements
       IF ( ASSOCIATED( Mesh % Elements(i) % FaceIndexes ) ) THEN
          DEALLOCATE( Mesh % Elements(i) % FaceIndexes )
          NULLIFY( Mesh % Elements(i) % FaceIndexes )
       END IF
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE ReleaseMeshFaceTables
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE SetCurrentMesh( Model, Mesh )
DLLEXPORT SetCurrentMesh
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    TYPE(Mesh_t),  POINTER :: Mesh
!------------------------------------------------------------------------------
    Model % Variables => Mesh % Variables

    Model % Mesh  => Mesh
    Model % Nodes => Mesh % Nodes
    Model % NumberOfNodes = Mesh % NumberOfNodes
    Model % Nodes % NumberOfNodes = Mesh % NumberOfNodes

    Model % Elements => Mesh % Elements
    Model % MaxElementNodes = Mesh % MaxElementNodes
    Model % NumberOfBulkElements = Mesh % NumberOfBulkElements
    Model % NumberOfBoundaryElements = Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
  END SUBROUTINE SetCurrentMesh
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE DisplaceMesh( Mesh, Update, SIGN, Perm, DOFs, StabRecomp )
DLLEXPORT DisplaceMesh
!------------------------------------------------------------------------------
    TYPE(Mesh_t) , POINTER :: Mesh 
    REAL(KIND=dp) :: Update(:)
    INTEGER :: DOFs,SIGN,Perm(:)
    LOGICAL, OPTIONAL :: StabRecomp

    INTEGER :: i,k
    LOGICAL :: StabFlag

    TYPE(Nodes_t) :: ElementNodes
    TYPE(Element_t), POINTER :: Element

     DO i=1,Mesh % NumberOfNodes
       k = Perm(i)
       IF ( k /= 0 ) THEN
         k = DOFs * (k-1)

         Mesh % Nodes % x(i) =  Mesh % Nodes % x(i) + SIGN * Update(k+1)

         IF ( DOFs > 1 ) &
           Mesh % Nodes % y(i) = Mesh % Nodes % y(i) + SIGN * Update(k+2)

         IF ( DOFs > 2 ) &
           Mesh % Nodes % z(i) = Mesh % Nodes % z(i) + SIGN * Update(k+3)
       END IF
     END DO

     StabFlag = .TRUE.
     IF ( PRESENT( StabRecomp ) ) StabFlag = StabRecomp

     IF ( SIGN == 1 .AND. StabFlag ) THEN
        k = Mesh % MaxElementNodes
        CALL AllocateVector( ElementNodes % x,k )
        CALL AllocateVector( ElementNodes % y,k )
        CALL AllocateVector( ElementNodes % z,k )

        DO i=1,Mesh % NumberOfBulkElements
           Element => Mesh % Elements(i)
           IF ( ANY( Perm( Element % NodeIndexes ) == 0 ) ) CYCLE

           k = Element % TYPE % NumberOfNodes
           ElementNodes % x(1:k) = Mesh % Nodes % x(Element % NodeIndexes)
           ElementNodes % y(1:k) = Mesh % Nodes % y(Element % NodeIndexes)
           ElementNodes % z(1:k) = Mesh % Nodes % z(Element % NodeIndexes)
           IF ( Mesh % Stabilize ) THEN
              CALL StabParam( Element,ElementNodes,k, &
                           Element % StabilizationMk, Element % Hk )
           ELSE
              Element % hK = ElementDiameter( Element, ElementNodes )
           END IF
        END DO

        DEALLOCATE( ElementNodes % x, ElementNodes % y, ElementNodes % z)
     END IF
!------------------------------------------------------------------------------
  END SUBROUTINE DisplaceMesh
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE ConvertToACTetra( Tetra )
!******************************************************************************
!
!  DESCRIPTION:
!     Convert tetrahedral element to Ainsworth & Coyle type tetrahedron.
!
!  ARGUMENTS:
!    Type(Element_t) :: Tetra
!      INOUT: Tetrahedral element to convert
!    
!******************************************************************************
!------------------------------------------------------------------------------
    USE PElementMaps, ONLY : getTetraEdgeMap, getTetraFaceMap
    IMPLICIT NONE
    
    TYPE(Element_t), POINTER :: Tetra
    INTEGER :: i, globalMin, globalMax, globalMinI
    INTEGER, DIMENSION(3) :: face, globalFace
    INTRINSIC MIN, MAX, CSHIFT

    ! Sanity check
    IF (Tetra % Type % ElementCode /= 504 .OR. &
         .NOT. ASSOCIATED(Tetra % PDefs)) THEN
       CALL Warn('MeshUtils::ConvertToACTetra','Element to convert not p tetrahedron!')
       RETURN
    END IF    
   
    ! Find global min and max vertices
    globalMin = Tetra % NodeIndexes(1)
    globalMinI = 1
    globalMax = Tetra % NodeIndexes(1)
    DO i=2,4
       ! Find min
       IF (globalMin > Tetra % NodeIndexes(i)) THEN
          globalMin = Tetra % NodeIndexes(i)
          globalMinI = i
       ELSE IF (globalMax < Tetra % NodeIndexes(i)) THEN
          globalMax = Tetra % NodeIndexes(i)
       END IF
    END DO
    
    ! Get face containing global min (either face 1 or 2)
    IF (globalMinI == 4) THEN
       face = getTetraFaceMap(2)
    ELSE
       face = getTetraFaceMap(1)
    END IF
    globalFace(1:3) = Tetra % NodeIndexes(face)

    ! Rotate face until first local index is min global
    DO 
       ! Check if first node matches global min node
       IF (globalMin == globalFace(1)) EXIT
       
       globalFace(1:3) = CSHIFT(globalFace,1)
    END DO
    ! Assign new local numbering
    Tetra % NodeIndexes(face) = globalFace(1:3)

    ! Face 3 now contains global max
    face = getTetraFaceMap(3)
    globalFace(1:3) = Tetra % NodeIndexes(face)
    ! Rotate face until last local index is max global
    DO 
       ! Chech if last node matches global max node
       IF (globalMax == globalFace(3)) EXIT
       
       globalFace(1:3) = CSHIFT(globalFace,1)
    END DO
    ! Assign new local numbering
    Tetra % NodeIndexes(face) = globalFace(1:3)

    ! Set AC tetra type
    IF (Tetra % NodeIndexes(2) < Tetra % NodeIndexes(3)) THEN
       Tetra % PDefs % TetraType = 1
    ELSE IF (Tetra % NodeIndexes(3) < Tetra % NodeIndexes(2)) THEN
       Tetra % PDefs % TetraType = 2
    ELSE 
       CALL Fatal('MeshUtils::ConvertToACTetra','Corrupt element type')
    END IF
   
  END SUBROUTINE ConvertToACTetra


!------------------------------------------------------------------------------
  SUBROUTINE AssignLocalNumber( EdgeElement, Element, Mesh )
!******************************************************************************
!
!  DESCRIPTION:
!     Assign local number of edge to given boundary element. Also copies all 
!     p element attributes from element edge to boundary edge.
!
!  ARGUMENTS:
!    Type(Element_t), POINTER :: EdgeElement, Element
!      INOUT: Edge element to which assign local number and element 
!        with some global numbering to use to assign local number.
!    
!    Type(Mesh_t), POINTER :: Mesh
!      INPUT: Finite element mesh containing faces and edges.
!
!******************************************************************************
!------------------------------------------------------------------------------
    USE PElementMaps, ONLY : getFaceEdgeMap 
    IMPLICIT NONE
    ! Parameters
    Type(Element_t), POINTER :: EdgeElement, Element 
    Type(Mesh_t), POINTER :: Mesh
    ! Variables
    INTEGER i,j,n,edgeNumber, numEdges, bMap(4)
    Type(Element_t), POINTER :: Edge
    
    ! Get number of points, edges or faces
    numEdges = 0
    SELECT CASE (Element % Type % Dimension)
    CASE (2)
       numEdges = Element % Type % NumberOfEdges
    CASE (3)   
       numEdges = Element % Type % NumberOfFaces
    CASE DEFAULT
       WRITE (*,*) 'MeshUtils::AssignLocalNumber Unsupported dimension'
       RETURN
    END SELECT

    ! For each edge or face in element try to find local number
    DO edgeNumber=1, numEdges
       ! If edges have not been created, stop search. This should not happen, actually.
       IF (.NOT. ASSOCIATED(Element % EdgeIndexes)) THEN
          ! EdgeElement % localNumber = 0
          RETURN
       END IF

       Edge => GetElementEntity(Element,edgeNumber,Mesh)

       ! Edge element not found. This should not be possible, unless there
       ! is an error in the mesh read in process..
       IF (.NOT. ASSOCIATED(Edge)) THEN
          CALL Warn('MeshUtils::AssignLocalNumber','Edge element not found')
          ! EdgeElement % localNumber = 0
          RETURN
       END IF

       n = 0
       ! For each element node
       DO i=1, Edge % Type % NumberOfNodes
          ! For each node in edge element
          DO j=1, EdgeElement % Type % NumberOfNodes
             ! If edge and edgeelement node match increment counter
             IF (Edge % NodeIndexes(i) == EdgeElement % NodeIndexes(j)) n = n + 1
          END DO
       END DO

       ! If all nodes are on boundary, edge was found
       IF (n == EdgeElement % Type % NumberOfNodes) THEN
          EdgeElement % PDefs % localNumber = edgeNumber

          ! Change ordering of global nodes to match that of element
          bMap = getElementBoundaryMap( Element, edgeNumber )
          EdgeElement % NodeIndexes(1:EdgeElement % Type % NumberOfNodes) = &
               Element % NodeIndexes(bMap)

          ! Copy attributes of edge element to boundary element
          ! Misc attributes
          EdgeElement % PDefs % isEdge = Edge % PDefs % isEdge
          
          ! Gauss points
          EdgeElement % PDefs % GaussPoints = Edge % PDefs % GaussPoints
          ! Element p (and boundary bubble dofs)
          EdgeElement % BDOFs = Edge % BDOFs
          EdgeElement % PDefs % P = Edge % PDefs % P

          ! If this boundary has edges copy edge indexes
          IF (ASSOCIATED(Edge % EdgeIndexes)) THEN
             ! Allocate element edges to element
             n = Edge % Type % NumberOfEdges
             bmap(1:4) = getFaceEdgeMap( Element, edgeNumber )
             
             IF ( ASSOCIATED( EdgeElement % EdgeIndexes) ) THEN
                DEALLOCATE( EdgeElement % EdgeIndexes )
             END IF
             
             CALL AllocateVector( EdgeElement % EdgeIndexes, n )
             ! Copy edges from edge to boundary edge
             DO i=1,n
                EdgeElement % EdgeIndexes(i) = Element % EdgeIndexes(bmap(i))
             !    EdgeElement % EdgeIndexes(i) = Element % EdgeIndexes(i)
             END DO
          END IF
          
          ! Edge fields copied and local edge found so return
          RETURN
       END IF
    END DO

    ! If we are here local number not found
    CALL Warn('MeshUtils::AssignLocalNumber','Unable to find local edge')
    ! EdgeElement % localNumber = 1
  CONTAINS

    FUNCTION GetElementEntity(Element, which, Mesh) RESULT(Entity)
      IMPLICIT NONE

      Type(Element_t), POINTER :: Element, Entity 
      Type(Mesh_t), POINTER :: Mesh
      INTEGER :: which

      NULLIFY(Entity)
      ! Switch by element dimension
      SELECT CASE (Element % Type % Dimension)
         CASE (2)
            Entity => Mesh % Edges( Element % EdgeIndexes(which))
         CASE (3)
            Entity => Mesh % Faces( Element % FaceIndexes(which))
         CASE DEFAULT
            WRITE (*,*) 'AssignLocalNumber::GetElementEntity: Unsupported dimension'
            RETURN
      END SELECT
    END FUNCTION GetElementEntity
  END SUBROUTINE AssignLocalNumber
    

!------------------------------------------------------------------------------
  FUNCTION getElementMaxDOFs( Mesh, Element ) RESULT(dofs)
!******************************************************************************
!
!  DESCRIPTION:
!     Based on element degrees of freedom, return the sum of element
!     degrees of freedom.
!
!  ARGUMENTS:
!
!    Type(Mesh_t), POINTER :: Mesh
!      INPUT: Finite element mesh
!
!    Type(Element_t), POINTER :: Element
!      INPUT: Element to get maximum dofs for
!
!  FUNCTION VALUE:
!    REAL(KIND=dp) :: dofs
!       maximum number of dofs for Element
!    
!******************************************************************************
!------------------------------------------------------------------------------
    IMPLICIT NONE

    Type(Mesh_t), POINTER :: Mesh
    Type(Element_t), POINTER :: Element
    Type(ELement_t), POINTER :: Edge, Face
    INTEGER :: i, edgeDofs, faceDofs, dofs
    
    ! Get sum of edge dofs if any
    edgeDofs = 0
    IF (ASSOCIATED(Element % EdgeIndexes)) THEN
       DO i=1, Element % Type % NumberOfEdges
          Edge => Mesh % Edges(Element % EdgeIndexes(i))
          edgeDofs = edgeDofs + Edge % BDOFs
       END DO
    END IF

    ! Get sum of face dofs if any
    faceDofs = 0
    IF (ASSOCIATED(Element % FaceIndexes)) THEN
       DO i=1, Element % Type % NumberOfFaces
          Face => Mesh % Faces(Element % FaceIndexes(i))
          faceDofs = faceDofs + Face % BDOFs
       END DO
    END IF

    ! Get sum of all dofs in element
    dofs = Element % Type % NumberOfNodes + &
         edgeDofs + faceDofs + Element % BDOFs
  END FUNCTION getElementMaxDOFs


!------------------------------------------------------------------------------
END MODULE MeshUtils
!------------------------------------------------------------------------------
