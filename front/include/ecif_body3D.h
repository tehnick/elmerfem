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
Module:     ecif_body3D.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Body3D class, derived from Body base class. 
            Handels 3D-bodies.
        
************************************************************************/
 
#ifndef _ECIF_BODY3D_
#define _ECIF_BODY3D_

#include "ecif_body.h"

class Body3D : public Body
{     
public:
  Body3D();
  Body3D(bodyGmtrType body_type, int ext_id, char* name, colorIndices color = DEFAULT_COLOR_INDEX);
  Body3D(ecif_Body_X& trx_body, bool add_default_layer = false);
  Body3D(bodyGmtrType body_type, int int_id, int ext_id, int nof_fem_elements, int* fem_elem_ids);
  int addAllPendingVertices(int layer);

protected:
  int calcDirection();
  bool check();
} ;

#endif
