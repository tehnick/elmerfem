/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , Center for Scientific Computing,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: Center for Scientific Computing
*                         Tietotie 6, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front 
Module:     ecif_body3D.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Implementation

************************************************************************/

#include "ecif_body3D.h"
#include "ecif_bodyElement.h"
#include "ecif_bodyElement3D.h"
#include "ecif_bodyElementLoop.h"
#include "ecif_model.h"
#include "ecif_renderer.h"
#include "ecif_userinterface.h"

 
// Constructors.

Body3D::Body3D()
  : Body()
{
}

//--Body-elements etc are not known
Body3D::Body3D(bodyGmtrType body_type, int ext_tag, char* name, colorIndices color)
  : Body(body_type, ext_tag, name, color)
{
}

//--Component ids are known, but not more.
Body3D::Body3D(ecif_Body_X& trx_body, bool add_default_layer)
  : Body(trx_body, add_default_layer)
{
}


Body3D::Body3D(bodyGmtrType body_type, int int_tag, int ext_tag,
               int nof_mesh_elements, int* mesh_element_ids)
               :Body(body_type, int_tag, ext_tag, NULL)
{
  maxNofMeshElements = nof_mesh_elements;
  nofMeshElements = nof_mesh_elements;
  meshElementIds = mesh_element_ids;
}


// Returns nof elements added
// NOTE: Not implmented yet!
int
Body3D::addAllPendingVertices(int layer)
{
  return 0;
}



// Method checks how face-loop is oriented.
int
Body3D::calcDirection()
{
  // Default is ccw.
  int result = 1;

  return result;
}


// NOTE: Not yet proper version for the 3D geometry!!!
// Return true if body is ok, otherwise false.
bool
Body3D::check()
{
  if (nofLayers == 0) return false;

  initName();

  // For a mesh body
  // ===============
  if (gmtrType == MESH_BODY) {
    return Body::check();

  // For a cad body
  // ==============
  } else {
    // Same as mesh body!
    // NOTE: We should do something real here!!!
    return Body::check();
  }
}

