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
! *  Module defining mappings for p elements. These include nodal points 
! *  contained by faces and edges, element boundary maps (edges for 2d elements,
! *  faces for 3d) and mappings from faces to edge numbers. Mappings defined in 
! *  this module are compatible with basis functions defined in module 
! *  PElementBase 
! *
! *******************************************************************************
! *
! *                     Author:       Mikko Byckling
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *
! *
! *                       Date: 23 Aug 2004
! *
! *******************************************************************************/

MODULE PElementMaps
  USE Messages
  USE Types, ONLY : Element_t

  IMPLICIT NONE

  ! Private mappings. For access use get[Element][Type]Map(i)
  PRIVATE QuadEdgeMap, TriangleEdgeMap, &
       TetraEdgeMap1, TetraFaceMap1, TetraFaceEdgeMap1, &
       TetraEdgeMap2, TetraFaceMap2, TetraFaceEdgeMap2, &
       BrickEdgeMap, BrickFaceMap, BrickFaceEdgeMap, &
       WedgeEdgeMap, WedgeFaceMap, WedgeFaceEdgeMap, &
       PyramidEdgeMap, PyramidFaceMap, PyramidFaceEdgeMap, &
       MInit
  ! Mappings
  INTEGER, TARGET, SAVE :: QuadEdgeMap(4,2), TriangleEdgeMap(3,2), &
       TetraEdgeMap1(6,2), TetraFaceMap1(4,3), TetraFaceEdgeMap1(4,3), &
       TetraEdgeMap2(6,2), TetraFaceMap2(4,3), TetraFaceEdgeMap2(4,3),&
       BrickEdgeMap(12,2), BrickFaceMap(6,4), BrickFaceEdgeMap(6,4), &
       WedgeEdgeMap(9,2), WedgeFaceMap(5,4), WedgeFaceEdgeMap(5,4), &
       PyramidEdgeMap(8,2), PyramidFaceMap(5,4), PyramidFaceEdgeMap(5,4)

  LOGICAL, SAVE :: MInit = .FALSE.
CONTAINS

  ! MAPPINGS

  ! First some direct mappings to elements. These should not be used directly 
  ! unless element type is implicitly known from context. Better way is to use
  ! getElement[Boundary,Edge,Face]Map -routines.

    ! Call: localEdge = getQuadEdge(i)
    !
    ! Function returns mapping from edge number to edge endpoints 

    FUNCTION getQuadEdgeMap(i) RESULT(localEdge)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(2) :: localEdge

      IF (.NOT. MInit) CALL InitializeMappings()
      
      localEdge(:) = QuadEdgeMap(i,:)
    END FUNCTION getQuadEdgeMap

    ! Call: localEdge = getTriangleEdge(i)
    ! 
    ! Function returns mapping from edge number to edge endpoints

    FUNCTION getTriangleEdgeMap(i) RESULT(localEdge)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(2) :: localEdge

      IF (.NOT. MInit) CALL InitializeMappings()

      localEdge(:)=TriangleEdgeMap(i,:)
    END FUNCTION getTriangleEdgeMap
    
    ! Call: localEdge = getBrickEdgeMap(i)
    ! 
    ! Function returns mapping from edge number to edge endpoints

    FUNCTION getBrickEdgeMap(i) RESULT(localEdge)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(2) :: localEdge

      IF (.NOT. MInit) CALL InitializeMappings()

      localEdge(:) = BrickEdgeMap(i,:)
    END FUNCTION getBrickEdgeMap
    
    ! Call: localFace = getBrickFaceMap(i)
    ! 
    ! Function returns mapping from face number to face nodes

    FUNCTION getBrickFaceMap(i) RESULT(localFace)
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(4) :: localFace

      IF (.NOT. MInit) CALL InitializeMappings()

      localFace(:) = BrickFaceMap(i,:)
    END FUNCTION getBrickFaceMap

    ! Call: localEdge = getFaceEdgeMap(face, localNode)
    !
    ! getFaceEdgeMap returns number of local edge when given face and 
    ! its local node number. Node number is treated as edges beginning point

    FUNCTION getBrickFaceEdgeMap(face, localNode) RESULT(localEdge)
      IMPLICIT NONE

      ! Parameters 
      INTEGER, INTENT(IN) :: face, localNode
      ! Variables
      INTEGER :: localEdge

      IF (.NOT. MInit) CALL InitializeMappings()

      localEdge = BrickFaceEdgeMap(face,localNode)

      IF (localEdge == 0) THEN
         WRITE (*,'(A,I2,I3)') 'Unknown combination node for (face,node)', face,localNode 
         STOP
      END IF
    END FUNCTION getBrickFaceEdgeMap


    FUNCTION getTetraEdgeMap(i,type) RESULT(edge)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, INTENT(IN), OPTIONAL :: type
      INTEGER :: t
      INTEGER, DIMENSION(2) :: edge

      IF (.NOT. MInit) CALL InitializeMappings()

      ! If type not present use default (1)
      t = 1
      IF (PRESENT(type)) t = type

      ! Select edge map by tetra type
      SELECT CASE (t)
      CASE (1)
         edge(:) = TetraEdgeMap1(i,:)
      CASE (2)
         edge(:) = TetraEdgeMap2(i,:)
      CASE DEFAULT
         CALL Fatal('PElementMaps::getTetraEdgeMap','Unknown tetra type')
      END SELECT
    END FUNCTION getTetraEdgeMap


    FUNCTION getTetraFaceMap(i,type) RESULT(face)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, INTENT(IN), OPTIONAL :: type
      INTEGER :: t
      INTEGER, DIMENSION(3) :: face

      IF (.NOT. MInit) CALL InitializeMappings()
      
      ! If type not present use default (1)
      t = 1
      IF (PRESENT(type)) t = type

      ! Select face map by tetra type 
      SELECT CASE(t)
      CASE (1)
         face(:) = TetraFaceMap1(i,:)
      CASE (2)
         face(:) = TetraFaceMap2(i,:)
      CASE DEFAULT 
         CALL Fatal('PElementMaps::getTetraFaceMap','Unknown tetra type')
      END SELECT
    END FUNCTION getTetraFaceMap

    FUNCTION getWedgeEdgeMap(i) RESULT(edge)
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(2) :: edge

      IF (.NOT. MInit) CALL InitializeMappings()

      edge(:) = WedgeEdgeMap(i,:)
    END FUNCTION getWedgeEdgeMap


    FUNCTION getWedgeFaceMap(i) RESULT(face)
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(4) :: face

      IF (.NOT. MInit) CALL InitializeMappings()

      face(:) = WedgeFaceMap(i,:)
    END FUNCTION getWedgeFaceMap


    FUNCTION getPyramidEdgeMap(i) RESULT(edge)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(2) :: edge

      IF (.NOT. MInit) CALL InitializeMappings()

      edge(:) = PyramidEdgeMap(i,:)
    END FUNCTION getPyramidEdgeMap


    FUNCTION getPyramidFaceMap(i) RESULT(face)
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: i
      INTEGER, DIMENSION(4) :: face

      IF (.NOT. MInit) CALL InitializeMappings()

      face(:) = PyramidFaceMap(i,:)
    END FUNCTION getPyramidFaceMap


!------------------------------------------------------------------------------
    FUNCTION getElementBoundaryMap(Element, i) RESULT(map)
!******************************************************************************
!
!  DESCRIPTION:
!     Mapping from element local edge or face number to nodes contained in 
!     that edge or face. 
!
!  ARGUMENTS:
!    Type(Element_t) :: Element
!      INPUT: Element to get map for
!
!    INTEGER, INTENT(IN) :: i
!      INPUT: Local number of elements edge or face
!
!  FUNCTION VALUE:
!    INTEGER :: map(4)
!       Map containing local node numbers of given local edge or face
!    
!******************************************************************************
!------------------------------------------------------------------------------
      IMPLICIT NONE

      Type(Element_t) :: Element
      INTEGER, INTENT(IN) :: i
      
      INTEGER :: map(4)

      IF (.NOT. MInit) CALL InitializeMappings()

      ! Function is not defined for non p elements
      IF (.NOT. ASSOCIATED(Element % PDefs)) THEN
         CALL Warn('PElementMaps::getElementBoundaryMap','Element not p element') 
         map = 0
         RETURN
      END IF

      SELECT CASE(Element % Type % ElementCode / 100)
      CASE (3)
         map = 0
         map(1:2) = getTriangleEdgeMap(i)
      CASE (4)
         map = 0
         map(1:2) = getQuadEdgeMap(i)
      CASE (5)
         map = 0
         map(1:3) = getTetraFaceMap(i,Element % PDefs % TetraType)
      CASE (6)
         map(1:4) = getPyramidFaceMap(i)
      CASE (7)
         map(1:4) = getWedgeFaceMap(i)
      CASE (8)
         map(1:4) = getBrickFaceMap(i)
      CASE DEFAULT
         CALL Fatal('PElementMaps::getElementBoundaryMap','Unsupported element type')
      END SELECT
    END FUNCTION getElementBoundaryMap


!------------------------------------------------------------------------------
    FUNCTION getFaceEdgeMap( Element, i) RESULT(map)
!******************************************************************************
!
!  DESCRIPTION:
!     Mapping from element local face to local edges contained in face. Given
!     element and local face number this routine returns numbers of local edges
!     on face. 
!
!  ARGUMENTS:
!    Type(Element_t) :: Element
!      INPUT: Element to get map for
!
!    INTEGER, INTENT(IN) :: i
!      INPUT: Local number of element face
!
!  FUNCTION VALUE:
!    INTEGER :: map(4)
!       Map containing local numbers of edges on face
!    
!******************************************************************************
!------------------------------------------------------------------------------
      IMPLICIT NONE

      Type(Element_t) :: Element
      INTEGER, INTENT(IN) :: i

      INTEGER :: elementCode, map(4)

      elementCode = Element % Type % ElementCode

      IF (.NOT. MInit) CALL InitializeMappings()

      ! Function is not defined for non p elements
      IF (.NOT. ASSOCIATED(Element % PDefs)) THEN
         CALL Warn('PElementMaps::getFaceEdgeMap','Element not p element') 
         map = 0
         RETURN
      END IF

      SELECT CASE(elementCode / 100)
      CASE (5)
         map = 0
         SELECT CASE (Element % PDefs % TetraType)
         CASE (1)
            map(1:3) = TetraFaceEdgeMap1(i,:)
         CASE (2)
            map(1:3) = TetraFaceEdgeMap2(i,:)
         CASE DEFAULT
            CALL Fatal('PElementMaps::getFaceEdgeMap','Unknown tetra type')
         END SELECT
      CASE (6)
         map(1:4) = PyramidFaceEdgeMap(i,:)
      CASE (7)
         map(1:4) = WedgeFaceEdgeMap(i,:)
      CASE (8)
         map(1:4) = BrickFaceEdgeMap(i,:)
      CASE DEFAULT
         CALL Fatal('PElementMaps::getFaceEdgeMap','Unsupported element type')
      END SELECT
    END FUNCTION getFaceEdgeMap


!------------------------------------------------------------------------------
    SUBROUTINE GetElementEdgeMap( Element, map )
!******************************************************************************
!
!  DESCRIPTION:
!     Get mappings for given element to element edges and their nodes. Given 
!     element, this routine returns a map containing nodes (endpoints) of
!     elements edges. 
!
!  ARGUMENTS:
!    Type(Element_t) :: Element
!      INPUT: Element to get map for
!
!    INTEGER :: map(:,:)
!       OUTPUT: Map containing local node numbers of local edges
!    
!******************************************************************************
!------------------------------------------------------------------------------
      IMPLICIT NONE
      Type(Element_t) :: Element
      INTEGER,  POINTER :: map(:,:)

      IF (.NOT. MInit) CALL InitializeMappings()

      ! Function is not defined for non p elements
      IF (.NOT. ASSOCIATED(Element % PDefs)) THEN
         CALL Warn('PElementMaps::GetElementEdgeMap','Element not p element') 
         map = 0
         RETURN
      END IF

      SELECT CASE (Element % Type % ElementCode / 100)
      CASE (3)
         map => TriangleEdgeMap
      CASE (4)
         map => QuadEdgeMap
      CASE (5)
         SELECT CASE( Element % PDefs % TetraType )
         CASE (1)
            map => TetraEdgeMap1
         CASE (2)
            map => TetraEdgeMap2
         CASE DEFAULT
            CALL Fatal('PElementMaps::GetElementEdgeMap','Unknown tetra type for p element')
         END SELECT
      CASE (6)
         map => PyramidEdgeMap
      CASE (7)
         map => WedgeEdgeMap
      CASE (8)
         map => BrickEdgeMap
      CASE DEFAULT
         CALL Fatal('PElementMaps::GetElementEdgeMap','Unsupported element type')
      END SELECT
    END SUBROUTINE GetElementEdgeMap
   

!------------------------------------------------------------------------------
    SUBROUTINE GetElementFaceMap( Element, faceMap )
!******************************************************************************
!
!  DESCRIPTION:
!     Get mappings for given element to element faces and their nodes. Given 
!     element, this routine returns a map containing nodes (endpoints) of
!     elements face. 
!
!  ARGUMENTS:
!    Type(Element_t) :: Element
!      INPUT: Element to get map for
!
!    INTEGER :: map(:,:)
!       OUTPUT: Map containing local node numbers of local faces
!    
!******************************************************************************
!------------------------------------------------------------------------------
      IMPLICIT NONE
      
      Type(Element_t) :: Element
      INTEGER, POINTER :: faceMap(:,:)

      IF (.NOT. MInit) CALL InitializeMappings()

      ! Function is not defined for non p elements
      IF (.NOT. ASSOCIATED(Element % PDefs)) THEN
         CALL Warn('PElementMaps::GetElementFaceMap','Element not p element') 
         NULLIFY(faceMap)
         RETURN
      END IF

      SELECT CASE (Element % Type % ElementCode / 100)
      CASE (5)
         SELECT CASE( Element % PDefs % TetraType )
         CASE (1)
            faceMap => TetraFaceMap1
         CASE (2)
            faceMap => TetraFaceMap2
         CASE DEFAULT
            CALL Fatal('PElementMaps::GetElementFaceMap','Unknown tetra type for p element')
         END SELECT
      CASE (6)
         faceMap => PyramidFaceMap
      CASE (7)
         faceMap => WedgeFaceMap
      CASE (8)
         faceMap => BrickFaceMap
      CASE DEFAULT
         CALL Fatal('PElementMaps::GetElementFaceMap','Unsupported element type')
      END SELECT
    END SUBROUTINE GetElementFaceMap


!------------------------------------------------------------------------------    
    SUBROUTINE GetElementFaceEdgeMap( Element, faceEdgeMap )
!******************************************************************************
!
!  DESCRIPTION:
!     Get mappings for given element to elements faces and their edge. Given 
!     element, this routine returns a map containing local edge numbers of
!     elements faces. 
!
!  ARGUMENTS:
!    Type(Element_t) :: Element
!      INPUT: Element to get map for
!
!    INTEGER :: map(:,:)
!       OUTPUT: Map containing local edge numbers of local faces
!    
!******************************************************************************
!------------------------------------------------------------------------------
      IMPLICIT NONE

      Type(Element_t) :: Element
      INTEGER, POINTER :: faceEdgeMap(:,:)
      
      IF (.NOT. MInit) CALL InitializeMappings()

      ! Function is not defined for non p elements
      IF (.NOT. ASSOCIATED(Element % PDefs)) THEN
         CALL Warn('PElementMaps::GetElementFaceEdgeMap','Element not p element') 
         NULLIFY(faceEdgeMap)
         RETURN
      END IF

      SELECT CASE (Element % Type % ElementCode / 100)
      CASE (5)
         SELECT CASE( Element % PDefs % TetraType )
         CASE (1)
            faceEdgeMap => TetraFaceEdgeMap1
         CASE (2)
            faceEdgeMap => TetraFaceEdgeMap2
         CASE DEFAULT
            CALL Fatal('PElementMaps::GetElementFaceEdgeMap','Unknown tetra type for p element')
         END SELECT
      CASE (6)
         faceEdgeMap => PyramidFaceEdgeMap
      CASE (7)
         faceEdgeMap => WedgeFaceEdgeMap
      CASE (8)
         faceEdgeMap => BrickFaceEdgeMap
      CASE DEFAULT
         CALL Fatal('PElementMaps::GetElementFaceEdgeMap','Unsupported element type')
      END SELECT
    END SUBROUTINE GetElementFaceEdgeMap


    SUBROUTINE InitializeMappings() 
!------------------------------------------------------------------------------
!   This subroutine initializes element mappings
!------------------------------------------------------------------------------
      IMPLICIT NONE
      
      CALL Info('PElementMaps::InitializeMappings','Initializing mappings for elements')

      ! Quad edge mappings
      QuadEdgeMap(1,:) = (/ 1,2 /)
      QuadEdgeMap(2,:) = (/ 2,3 /)
      QuadEdgeMap(3,:) = (/ 4,3 /)
      QuadEdgeMap(4,:) = (/ 1,4 /)

      ! Triangle edge mappings
      TriangleEdgeMap(1,:) = (/ 1,2 /)
      TriangleEdgeMap(2,:) = (/ 2,3 /)
      TriangleEdgeMap(3,:) = (/ 3,1 /)

      ! Brick edge mappings
      BrickEdgeMap(1,:) = (/ 1,2 /)
      BrickEdgeMap(2,:) = (/ 2,3 /)
      BrickEdgeMap(3,:) = (/ 4,3 /)
      BrickEdgeMap(4,:) = (/ 1,4 /)
      BrickEdgeMap(5,:) = (/ 5,6 /)
      BrickEdgeMap(6,:) = (/ 6,7 /)
      BrickEdgeMap(7,:) = (/ 8,7 /)
      BrickEdgeMap(8,:) = (/ 5,8 /)
      BrickEdgeMap(9,:) = (/ 1,5 /)
      BrickEdgeMap(10,:) = (/ 2,6 /)
      BrickEdgeMap(11,:) = (/ 3,7 /)
      BrickEdgeMap(12,:) = (/ 4,8 /)

      ! Brick face mappings
      BrickFaceMap(1,:) = (/ 1,2,3,4 /) ! xi,eta
      BrickFaceMap(2,:) = (/ 5,6,7,8 /) ! xi,eta
      BrickFaceMap(3,:) = (/ 1,2,6,5 /) ! xi,zeta
      BrickFaceMap(4,:) = (/ 2,3,7,6 /) ! eta,zeta
      ! BrickFaceMap(5,:) = (/ 3,4,8,7 /)
      BrickFaceMap(5,:) = (/ 4,3,7,8 /)
      ! BrickFaceMap(6,:) = (/ 4,1,5,8 /) 
      BrickFaceMap(6,:) = (/ 1,4,8,5 /)

      BrickFaceEdgeMap(1,:) = (/ 1,2,3,4 /)
      BrickFaceEdgeMap(2,:) = (/ 5,6,7,8 /)    
      BrickFaceEdgeMap(3,:) = (/ 1,10,5,9 /)
      BrickFaceEdgeMap(4,:) = (/ 2,11,6,10 /)
      ! BrickFaceEdgeMap(5,:) = (/ 3,12,7,11 /)
      BrickFaceEdgeMap(5,:) = (/ 3,11,7,12 /)
      ! BrickFaceEdgeMap(6,:) = (/ 4,9,8,12 /)
      BrickFaceEdgeMap(6,:) = (/ 4,12,8,9 /)

      ! Tetra edge mappings (not needed for enforcing parity!)
      ! Type 1
      TetraEdgeMap1(1,:) = (/ 1,2 /)
      TetraEdgeMap1(2,:) = (/ 2,3 /)
      TetraEdgeMap1(3,:) = (/ 1,3 /)
      TetraEdgeMap1(4,:) = (/ 1,4 /)
      TetraEdgeMap1(5,:) = (/ 2,4 /)
      TetraEdgeMap1(6,:) = (/ 3,4 /)
      ! Type 2
      TetraEdgeMap2(1,:) = (/ 1,2 /)
      TetraEdgeMap2(2,:) = (/ 3,2 /)
      TetraEdgeMap2(3,:) = (/ 1,3 /)
      TetraEdgeMap2(4,:) = (/ 1,4 /)
      TetraEdgeMap2(5,:) = (/ 2,4 /)
      TetraEdgeMap2(6,:) = (/ 3,4 /)

      ! Tetra face mappings (not needed for enforcing parity!)
      ! Type 1
      TetraFaceMap1(1,:) = (/ 1,2,3 /)
      TetraFaceMap1(2,:) = (/ 1,2,4 /)
      TetraFaceMap1(3,:) = (/ 2,3,4 /)
      TetraFaceMap1(4,:) = (/ 1,3,4 /)
      ! Type 2 
      TetraFaceMap2(1,:) = (/ 1,3,2 /)
      TetraFaceMap2(2,:) = (/ 1,2,4 /)
      TetraFaceMap2(3,:) = (/ 3,2,4 /)
      TetraFaceMap2(4,:) = (/ 1,3,4 /)

      ! Type 1 
      TetraFaceEdgeMap1(1,:) = (/ 1,2,3 /)
      TetraFaceEdgeMap1(2,:) = (/ 1,5,4 /)
      TetraFaceEdgeMap1(3,:) = (/ 2,6,5 /)
      TetraFaceEdgeMap1(4,:) = (/ 3,6,4 /)
      ! Type 2 
      TetraFaceEdgeMap2(1,:) = (/ 3,2,1 /)
      TetraFaceEdgeMap2(2,:) = (/ 1,5,4 /)
      TetraFaceEdgeMap2(3,:) = (/ 2,5,6 /)
      TetraFaceEdgeMap2(4,:) = (/ 3,6,4 /)

      ! Wedge edge mappings
      WedgeEdgeMap(1,:) = (/ 1,2 /)
      WedgeEdgeMap(2,:) = (/ 2,3 /)
      WedgeEdgeMap(3,:) = (/ 3,1 /)
      WedgeEdgeMap(4,:) = (/ 4,5 /)
      WedgeEdgeMap(5,:) = (/ 5,6 /)
      WedgeEdgeMap(6,:) = (/ 6,4 /)
      WedgeEdgeMap(7,:) = (/ 1,4 /)
      WedgeEdgeMap(8,:) = (/ 2,5 /)
      WedgeEdgeMap(9,:) = (/ 3,6 /)

      ! Wedge face mappings
      WedgeFaceMap(1,:) = (/ 1,2,3,0 /)
      WedgeFaceMap(2,:) = (/ 4,5,6,0 /)
      WedgeFaceMap(3,:) = (/ 1,2,5,4 /)
      WedgeFaceMap(4,:) = (/ 2,3,6,5 /)
      WedgeFaceMap(5,:) = (/ 3,1,4,6 /)

      WedgeFaceEdgeMap(1,:) = (/ 1,2,3,0 /)
      WedgeFaceEdgeMap(2,:) = (/ 4,5,6,0 /)
      WedgeFaceEdgeMap(3,:) = (/ 1,8,4,7 /)
      WedgeFaceEdgeMap(4,:) = (/ 2,9,5,8 /)
      WedgeFaceEdgeMap(5,:) = (/ 3,7,6,9 /)
      
      ! Pyramid edge mappings 
      PyramidEdgeMap(1,:) = (/ 1,2 /)
      PyramidEdgeMap(2,:) = (/ 2,3 /)
      PyramidEdgeMap(3,:) = (/ 4,3 /)
      PyramidEdgeMap(4,:) = (/ 1,4 /)
      PyramidEdgeMap(5,:) = (/ 1,5 /)
      PyramidEdgeMap(6,:) = (/ 2,5 /)
      PyramidEdgeMap(7,:) = (/ 3,5 /)
      PyramidEdgeMap(8,:) = (/ 4,5 /)

      ! Pyramid face mappings
      PyramidFaceMap(1,:) = (/ 1,2,3,4 /)
      PyramidFaceMap(2,:) = (/ 1,2,5,0 /)
      PyramidFaceMap(3,:) = (/ 2,3,5,0 /)
      PyramidFaceMap(4,:) = (/ 3,4,5,0 /)
      PyramidFaceMap(5,:) = (/ 4,1,5,0 /)

      PyramidFaceEdgeMap(1,:) = (/ 1,2,3,4 /)
      PyramidFaceEdgeMap(2,:) = (/ 1,6,5,0 /)
      PyramidFaceEdgeMap(3,:) = (/ 2,7,6,0 /)
      PyramidFaceEdgeMap(4,:) = (/ 3,8,7,0 /)
      PyramidFaceEdgeMap(5,:) = (/ 4,5,8,0 /)

      MInit = .TRUE.
    END SUBROUTINE InitializeMappings

!------------------------------------------------------------------------------
  FUNCTION getFaceDOFs( Element, p, faceNumber ) RESULT(faceDOFs)
!******************************************************************************
!
!  DESCRIPTION:
!     Based on element face polynomial degree p, return degrees of freedom for
!     given face 
!
!  ARGUMENTS:
!    Type(Element_t), POINTER :: Element
!      INPUT: Element to get face dofs to 
!
!    INTEGER :: p
!      INPUT: Face polynomial degree p
!
!    INTEGER :: faceNumber
!      INPUT: Local number of face for element (important for wedges and 
!        pyramids).
!
!  FUNCTION VALUE:
!    REAL(KIND=dp) :: faceDOFs
!       number of face dofs for Element
!    
!******************************************************************************
!------------------------------------------------------------------------------
    IMPLICIT NONE
    
    Type(Element_t), POINTER :: Element
    INTEGER, INTENT(IN) :: p
    INTEGER, INTENT(IN), OPTIONAL :: faceNumber
    INTEGER :: faceDOFs

    ! This function is not defined for non p elements
    IF (.NOT. ASSOCIATED(Element % PDefs) ) THEN
       faceDOFs = 0
       RETURN
    END IF

    faceDOFs = 0
    SELECT CASE(Element % Type % ElementCode / 100)
    ! Tetrahedron
    CASE (5)
       IF (p >= 3) faceDOFs = (p-1)*(p-2)/2
    ! Pyramid
    CASE (6)
       SELECT CASE(faceNumber)
          CASE (1)
             IF (p >= 4) faceDOFs = (p-2)*(p-3)/2
          CASE (2:5)
             IF (p >= 3) faceDOFs = (p-1)*(p-2)/2
       END SELECT
    ! Wedge
    CASE (7)
       SELECT CASE(faceNumber)
       CASE (1,2)
          IF (p >= 3) faceDOFs = (p-1)*(p-2)/2
       CASE (3:5)
          IF (p >= 4) faceDOFs = (p-2)*(p-3)/2
       END SELECT
    ! Brick   
    CASE (8)
       IF (p >= 4) faceDOFs = (p-2)*(p-3)/2
    CASE DEFAULT
       CALL Warn('MeshUtils::getFaceDOFs','Unsupported p element type')
       faceDOFs = p
    END SELECT

    faceDOFs = MAX(0, faceDOFs)
  END FUNCTION getFaceDOFs


!------------------------------------------------------------------------------
  FUNCTION getBubbleDOFs( Element, p) RESULT(bubbleDOFs)
!******************************************************************************
!
!  DESCRIPTION:
!     Based on element bubble polynomial degree p, return degrees of freedom for
!     given elements bubbles 
!
!  ARGUMENTS:
!    Type(Element_t), POINTER :: Element
!      INPUT: Element to get bubble dofs to 
!
!    INTEGER :: p
!      INPUT: Element polynomial degree p
!
!  FUNCTION VALUE:
!    REAL(KIND=dp) :: bubbleDOFs
!       number of bubble dofs for Element
!    
!******************************************************************************
!------------------------------------------------------------------------------
    IMPLICIT NONE
    
    Type(Element_t) :: Element
    INTEGER, INTENT(IN) :: p
    INTEGER :: bubbleDOFs
    
    ! This function is not defined for non p elements
    IF (.NOT. ASSOCIATED(Element % PDefs) ) THEN
       bubbleDOFs = 0
       RETURN
    END IF

    ! Select by element type
    bubbleDOFs = 0
    SELECT CASE (Element % Type % ElementCode / 100)
    ! Line 
    CASE (2)
      IF (p >= 2) bubbleDOFs = p - 1
    ! Triangle
    CASE (3)
      IF (p >= 3) bubbleDOFs = (p-1)*(p-2)/2
    ! Quad
    CASE (4)
       IF (p >= 4) bubbleDOFs = (p-2)*(p-3)/2
    ! Tetrahedron
    CASE (5)
       IF (p >= 4) bubbleDOFs = (p-1)*(p-2)*(p-3)/6
    ! Pyramid
    CASE (6)
       IF (p >= 4) bubbleDOFs = (p-1)*(p-2)*(p-3)/6
    ! Wedge
    CASE (7)
       IF (p >= 5) bubbleDOFs = (p-2)*(p-3)*(p-4)/6
    ! Brick
    CASE (8)
       IF (p >= 6) bubbleDOFs = (p-3)*(p-4)*(p-5)/6
    CASE DEFAULT
       CALL Warn('MeshUtils::getBubbleDOFs','Unsupported p element type')
       bubbleDOFs = p
    END SELECT

    bubbleDOFs = MAX(0, bubbleDOFs)
  END FUNCTION getBubbleDOFs


END MODULE
