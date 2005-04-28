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
Module:     ecif_modelObject.h
Language:   C++
Date:       20.12.99
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Common base classes for model level objects

************************************************************************/

#ifndef _ECIF_MODELOBJECT_
#define _ECIF_MODELOBJECT_

#include "ecif_def.h"


// Absract base class for all model objects
class ModelObject {
public:
  ModelObject();
  ModelObject(int oid, enum objectType tp, int tg, char* nm);
  virtual ~ModelObject();
  virtual int Id() const { return id; }
  bool isActive() { return active; }
  bool objectIsOk() { return objectOk; }
  virtual enum objectType getObjectType() const { return otype; }
  virtual const char* getName() const { return name; }
  virtual bool hasName() const;
  virtual int Tag() const { return tag; }
  static void initClass(Model* mdl);
  virtual void setId(int oid) { id = oid; }
  virtual void setObjectType(enum objectType tp) { otype = tp; }
  virtual void setTag(int tg) { tag = tg; }
  virtual void setName(char* nm) { update_dyna_string(name, nm); }
  virtual void setActive(bool value) { active = value; }

protected:
  static Model* model;
  int id;
  bool objectOk;
  enum objectType otype;
  int tag;
  char* name;
  bool active;
};

#endif
