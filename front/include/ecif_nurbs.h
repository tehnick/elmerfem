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
Module:     ecif_nurbs.h
Language:   C
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Decalrations for NURBS-funtions and utilities. 

************************************************************************/

#ifndef _ECIF_NURBS_
#define _ECIF_NURBS_

#include "ecif_def.h"


/*
Function calculates x,y values corresponding parameter
value u for a nurbs-curve.
*/
void nurbsCurveValue(struct ecif_NurbsCurve* curve_params, double u, Point3 result);

#endif
