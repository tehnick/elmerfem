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
Module:     ecif_datafile.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for input and resul file parameter definition

************************************************************************/

#ifndef _ECIF_DATAFILE_
#define _ECIF_DATAFILE_

#include "ecif_parameter.h"


// ****** Datafile parameter class ******
class Datafile : public Parameter{
public:
  Datafile();
  Datafile(int pid);
  Datafile(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Datafile"; }
  const char* getArrayName() { return "Datafile"; }
  const char* getEmfName() { return "Datafile"; }
  const char* getSifName() { return "Datafile"; }
  ecif_parameterType getParameterType() { return ECIF_DATAFILE; }
  static void initClass(Model* model);
  virtual ostream& output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
