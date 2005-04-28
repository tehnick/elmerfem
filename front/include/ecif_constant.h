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
Module:     ecif_constant.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for physical constants paremeter.

************************************************************************/

#ifndef _ECIF_CONSTANT_
#define _ECIF_CONSTANT_

#include "ecif_parameter.h"


// ****** Constant parameter class ******
class Constant : public Parameter{
public:
  Constant();
  Constant(int pid);
  Constant(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Constant"; }
  const char* getArrayName() { return "Constant"; }
  const char* getEmfName() { return "Constant"; }
  const char* getSifName() { return "Constant"; }
  ecif_parameterType getParameterType() { return ECIF_CONSTANT; }
  static void initClass(Model* model);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
