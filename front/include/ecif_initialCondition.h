/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: CSC - IT Center for Science Ltd.
*                         Keilaranta 14, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front
Module:     ecif_initialCondition.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for initial conditions.

************************************************************************/

#ifndef _ECIF_INITIAL_CONDITION_
#define _ECIF_INITIAL_CONDITION_

#include "ecif_parameter.h"


// ****** Initial Condition class ******
class InitCondition : public Parameter{
public:
  InitCondition();
  InitCondition(int pid);
  InitCondition(int pid, int parent_id, char* data_string, char* param_name);
  void setLastId(int lid) {last_id = lid;}
  int getLastId() {return last_id;}
  const char* getGuiName() { return "InitialCondition"; }
  const char* getArrayName() { return "InitialCondition"; }
  const char* getEmfName() { return "Initial Condition"; }
  const char* getSifName() { return SIF_INITIAL_CONDITION; }
  ecif_parameterType getParameterType() { return ECIF_INITIAL_CONDITION; }
  static void initClass(Model* model);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
