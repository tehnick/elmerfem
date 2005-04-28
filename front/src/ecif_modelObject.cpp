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
Module:     ecif_modelObject.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Implementation

************************************************************************/

#include "ecif_modelObject.h"
#include "ecif_func.h"

// Init static class variables
Model* ModelObject::model = NULL;


// =================
// ModelObject class
// =================

ModelObject::ModelObject()
{
  id = NO_INDEX;
  objectOk = true;
  otype = OT_NONE;

  tag = NO_INDEX;
  name = NULL;
  active = true;
}

ModelObject::ModelObject(int oid, enum objectType tp, int tg, char* nm)
{
  id = oid;
  objectOk = true;
  otype = tp;

  tag = tg;
  name = NULL; update_dyna_string(name, nm);
  active = true;
}


ModelObject::~ModelObject()
{
  delete[] name;
}


// Check if name defined
//
bool
ModelObject::hasName() const
{
  if ( name == NULL || name[0] == '\0' ) {
    return false;
  } else {
    return true;
  }
}


void
ModelObject::initClass(Model* mdl)
{
  ModelObject::model = mdl;
}


