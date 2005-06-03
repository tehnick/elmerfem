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
Program:    ELMER Data base interface (EIO)
Author(s):  Harri Hakula 10.03.98
************************************************************************/

#ifndef EIOMODELMANAGER_H
#define EIOMODELMANAGER_H

#include "eio_config.h"

#include <typeinfo>
using namespace std;

#include <fstream>
#include <iostream>

class EIOModelManager
{
public:
  EIOModelManager();
  int createModel(const char *dir);
  int openModel(const char *dir);
  int closeModel();

  ~EIOModelManager();

  int openStream(fstream& fstr, const char *name, int mode);
  int closeStream(fstream& fstr);

  int makeDirectory(const char *dir);
  char *name() { return modelname; }
protected:
  char rundir[PATH_MAX];
  char modeldir[PATH_MAX];
  char modelname[PATH_MAX];

};

#endif  /* EIOMODELMANAGER_H */
