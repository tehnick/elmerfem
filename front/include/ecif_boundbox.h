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
Module:     ecif_boundbox.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   A Base class for geometry object's bounding box. 

************************************************************************/

#ifndef _ECIF_BOUNDBOX_
#define _ECIF_BOUNDBOX_

#include "ecif_def.h"


class BoundBox
{     
public:
  BoundBox();
  BoundBox(double min_value, double max_value);
  BoundBox(RangeVector range_values);
  bool contains(BoundBox* other_box);
  void extendByPoint(GcPoint* point);
  void extendByPoint(Point3 point);
  void extendByRange(RangeVector);
  void getBoxAxis(int crn1, int crn2, double* start, double* end1, double* end2);
  void getSize(double& dim_x, double& dim_y, double& dim_z);
  void getRangeVector(RangeVector rv);
  ostream& output(ostream& out);
  bool overlap(BoundBox* other_box);
  void restrictByRange(RangeVector);
  
private:
  double x1, x2, y1, y2, z1, z2;
  void setMaximumPair(double& c1, double& c2, double r1, double r2);
  void setMinimumPair(double& c1, double& c2, double r1, double r2);
} ;

#endif
