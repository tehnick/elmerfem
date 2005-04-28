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
Module:     ecif_coordinate.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Implementation

************************************************************************/

#include "ecif_coordinate.h"

//Initialize static class variables.
int Coordinate::last_id = 0;
Model* Coordinate::model = NULL;


// Constructors
Coordinate::Coordinate()
{
}


Coordinate::Coordinate(int pid) : Parameter(pid)
{
}

Coordinate::Coordinate(int pid, char* data_string, char* param_name)
{
  setData(pid, data_string, param_name);
}


void
Coordinate::initClass(Model* mdl)
{
  Coordinate::model = mdl;
  Coordinate::last_id = 0;
}


void
Coordinate::setName(char* param_name)
{
  Parameter::setName(param_name, "Coordinate");
}



