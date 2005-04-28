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
Module:     ecif_boundaryCondition.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for boundary conditions.

************************************************************************/

#ifndef _ECIF_BOUNDARY_CONDITION_
#define _ECIF_BOUNDARY_CONDITION_

#include "ecif_parameter.h"


// ****** Boundary Condition class ******
class BoundaryCondition : public Parameter{
public:
  BoundaryCondition();
  BoundaryCondition(int p_id);
  BoundaryCondition(int cid, int parent_id, char* data_string, char* param_name);
  ~BoundaryCondition();
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Constraint"; }
  const char* getArrayName() { return "BoundaryCondition"; }
  const char* getEmfName() { return "Boundary Condition"; }
  const char* getSifName() { return SIF_BOUNDARY_CONDITION; }
  ecif_parameterType getParameterType() { return ECIF_BOUNDARY_CONDITION; }
  bool hasZeroVelocity();
  static void initClass(Model* model);
  virtual ostream& output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc);
  void setName(char* param_name);
  void updateParentId();
  void updateParentInfo(int parent_id);
  void updateTargetTags();

protected:
  static int last_id;
  static Model* model;
  int nofTargetBoundaries;
  int* targetBoundaryTags;
  int targetBodyTag;
  const ModelObject* getParentEmfObject();
};


#endif
