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
Module:     ecif_gridparameter.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Implementation

************************************************************************/
 
#include "ecif_gridParameter.h"
#include "ecif_body.h"
#include "ecif_bodyLayer.h"
#include "ecif_model.h"

//Initialize static class variables.
int GridParameter::last_id = 0;
Model* GridParameter::model = NULL;


// Constructors
GridParameter::GridParameter()
{
}


GridParameter::GridParameter(int pid) : Parameter(pid)
{
  if ( pid > last_id ) last_id = pid;
}


GridParameter::GridParameter(int pid, int parent_id, char* data_string, char* param_name)
{
  if ( pid > last_id ) last_id = pid;
  setData(pid, parent_id, data_string, param_name);
}


// Parent object tag for emf-file
int
GridParameter::getParentEmfTag()
{
  // Parent object is a body layer, but we need the body as the parent and
  // the layer as the subParent in emf-file
  //
  // NOTE: This is because we do not store Layers as objects in Emf-file, but
  // they are (if shown explicitely) always under bodies!
  //
  BodyLayer* bl = model->getBodyLayerById(parentId);

  if ( bl == NULL ) return NO_INDEX;

  Body* body = model->getBodyById(bl->getBodyId());

  if ( body == NULL ) return NO_INDEX;

  return body->Tag();
}


objectType
GridParameter::getParentEmfType()
{
  // Parent object is a body layer, but we need the body as the parent and
  // the layer as the subParent in emf-file
  //
  BodyLayer* bl = model->getBodyLayerById(parentId);

  if ( bl == NULL ) return OT_NONE;

  Body* body = model->getBodyById(bl->getBodyId());

  if ( body == NULL ) return OT_NONE;

  return body->getObjectType();
}


int
GridParameter::getSubParentEmfTag()
{
  // Parent object is a body layer, but we need the body as the parent and
  // the layer as the subParent in emf-file
  //
  BodyLayer* bl = model->getBodyLayerById(parentId);

  if ( bl == NULL ) return NO_INDEX;

  return bl->Tag();
}



void
GridParameter::initClass(Model* mdl)
{
  GridParameter::model = mdl;
  GridParameter::last_id = 0;
}


void
GridParameter::setName(char* param_name)
{
  Parameter::setName(param_name, "MeshStructure");
}


void
GridParameter::updateParentId()
{
  // We first read from emf-file the body and the body layer info
  // Then get body-layer object id with this info
  
  parentId = NO_INDEX;

  Body* body = model->getBodyByTag(parentEmfTag);

  if ( body == NULL ) return;

  int layer = body->getLayerIndexByTag(subParentEmfTag);

  parentId = body->getLayerId(layer);
}


void
GridParameter::updateParentInfo(int parent_id)
{
  // Update parent object's id
  //
  parentId = parent_id;

  BodyLayer* lr = model->getBodyLayerById(parent_id);

  if ( lr != NULL ) {

    const Body* bd = lr->getBody();
    
    // Emf parent is always the body
    if ( bd != NULL ) {
      parentEmfTag = bd->Tag();
      parentEmfType = bd->getObjectType();

      // For 'technical' layers we don use emf sub-parents
      //
      if ( IMPLICIT_LAYER == lr->getLayerType() ) {
        subParentEmfTag = NO_INDEX;
        subParentEmfType = OT_NONE;

      // For 'real'layer we the layer's tag
      } else {
        subParentEmfTag = lr->Tag();
        subParentEmfType = lr->getObjectType();
      }

    } else {
      parentEmfTag = NO_INDEX;
      parentEmfType = OT_NONE;
      subParentEmfTag = NO_INDEX;
      subParentEmfType = OT_NONE;
    }
  }
}



