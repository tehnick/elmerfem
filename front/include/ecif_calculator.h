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
Module:     ecif_solver.h
Language:   C++
Date:       24.01.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for calculator (a special solver without real equation)

************************************************************************/

#ifndef _ECIF_CALCULATOR_
#define _ECIF_CALCULATOR_

#include "ecif_parameter.h"


// ****** Calculator class ******
class Calculator : public Parameter{
public:
  Calculator();
  Calculator(int pid);
  Calculator(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Calculator"; }
  const char* getArrayName() { return "Calculator"; }
  const char* getEmfName() { return "Calculator"; }
  const char* getSifName() { return SIF_SOLVER; }
  ecif_parameterType getParameterType() { return ECIF_CALCULATOR; }
  static void initClass(Model* model);
  virtual ostream& output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
