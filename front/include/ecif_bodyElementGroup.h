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
Module:     ecif_bodyElementGroup.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for bodyelemets.
            A bodyelement is a separable entity in a body.
            They can be edges, surfaces etc. depending on the dimension.

************************************************************************/

#ifndef _ECIF_BODYELEMENT_GROUP_
#define _ECIF_BODYELEMENT_GROUP_

#include "ecif_def.h"
#include "ecif_def_stl.h"
#include "ecif_boundbox.h"
#include "ecif_geometry.h"
#include "ecif_modelObject.h"


// Element group for boundary conditions etc.
//
class BodyElementGroup : public ModelObject
{
friend class Control;
friend class Model;
public:
  BodyElementGroup();
  BodyElementGroup(ecif_ElementGroup_X& trx_bg, enum objectType bndr_type);
  virtual ~BodyElementGroup();
  int addElement(int bn_id);
  bool check();
  const BodyElement* getElement(int index);
  int getElementId(int index);
  const int* getElementIds() { return elementIds; }
  int getElementTag(int index);
  const int* getElementTags() { return elementTags; }
  int getBoundaryConditionId() { return boundaryConditionId;}
  int getBoundaryParameterId() { return boundaryParameterId;}
  enum objectType getElementType() const { return elemType;}
  enum elementGroupType getGroupType() const { return groupType;}
  int getNofElements() { return nofElements; }
  int getParentId(short parent) const;
  int getParentLayer(short parent) const;
  int getParentTag(short parent) const;
  bool hasElement(int be_id);
  void initName();
  static void initClass(Model* model);
  bool isExplicit() { return groupType == EXPLICIT_GROUP; }
  bool isImplicit() { return groupType == IMPLICIT_GROUP; }
  bool isVirtual() { return groupType == VIRTUAL_GROUP; }
  virtual ostream& output_emf(ostream& out, short indent_size, short indent_level);
  int removeElement(int bn_id);
  void setElementIdsAndTags(int nof_ids, int* be_ids, int* be_tgs);
  void setBoundaryConditionId(int bc_id);
  void setBoundaryParameterId(int pid);
  void setElementType(enum objectType et);
  void setType(enum elementGroupType gt);
  void setParentId(short parent_nbr, int parent_id);
  void setParentLayer(short parent_nbr, int layer);
  void setParentTag(short parent_nbr, int parent_tag);

protected:
  void init();

  static int last_tag;
  int* elementIds;
  int* elementTags;
  int boundaryConditionId;
  int boundaryParameterId;
  int nofElements;
  enum elementGroupType groupType;  // REAL_GROUP etc.
  enum objectType elemType;  // OT_EDGE etc.
  int parent1Id;
  int parent2Id;
  int parent1Layer;
  int parent2Layer;
  int parent1Tag;
  int parent2Tag;

} ;

#endif

