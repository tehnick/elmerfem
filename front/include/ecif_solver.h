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
Date:       15.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class solver parameter

************************************************************************/

#ifndef _ECIF_SOLVER_
#define _ECIF_SOLVER_

#include "ecif_parameter.h"


// ****** Solver class ******
class Solver : public Parameter{
public:
  Solver();
  Solver(int pid);
  Solver(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Solver"; }
  const char* getArrayName() { return "Solver"; }
  const char* getEmfName() { return "Solver"; }
  const char* getSifName() { return SIF_SOLVER; }
  ecif_parameterType getParameterType() { return ECIF_SOLVER; }
  static void initClass(Model* model);
  virtual ostream& output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
