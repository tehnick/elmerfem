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
Program:	ELMER Front 
Module:		ecif_nurbs.cpp
Language:	C++
Date:		01.10.98
Version:	1.00
Author(s):	Martti Verho
Revisions:	

Abstract:	Implementation

************************************************************************/
 
#include "ecif_geometry.h"
#include "ecif_nurbs.h"
 
 
// ************************
// Local utility functions.
// ************************


// B-spline basis function when p=0.
double
bspl_n0(double* knots, int interval, double u)
{
	if (u >= knots[interval] && u < knots[interval+1])
		return 1.0;
	else
		return 0.0;
}


// B-spline basis function, general case
// N_i,p(u) value for parameter value u.
double
bspl_np(int p, double* knots, int i, double u)
{
	// Elementary case (p=1)
	if (p == 1)
		return bspl_n0(knots, i, u);

	// General case, recursive call
	else {
		double du0 = knots[i+p] - knots[i];
		double du1 = knots[i+1+p] - knots[i+1];
		
		// Take care of the 0/0 special case 
		double w0 = (du0 == 0.0f)?1.0f:( (u-knots[i])/du0 );
		double w1 = (du1 == 0.0f)?1.0f:( (knots[i+1+p]-u)/du1 );

		return ( w0 * bspl_np(p-1, knots, i, u) +
					w1 * bspl_np(p-1, knots, i+1, u) );

	}
}


// Function calculates x,y,z values corresponding parameter
// value u for a b-spline curve.
// Based on basis-functions (bf)
void
bsplCurveValue_bf(struct ecif_NurbsCurve* uP, double u, Point3 result)
{
	// Seed for the result
	double x_val = 0.0;
	double y_val = 0.0;
	double z_val = 0.0;

		
	// Cumulate the contribution of all control points
	int i;
	for (i = 0; i < uP->nofCpoints; i++) {

		double b_val = bspl_np(1 + uP->degree, uP->knots, i, u);
    
		x_val = x_val + b_val * uP->cpoints[i][0];
		y_val = y_val + b_val * uP->cpoints[i][1];
		z_val = z_val + b_val * uP->cpoints[i][2];
	}
	
	result[0] = x_val;
	result[1] = y_val;
	result[2] = z_val;
}


// ******************
// Callable functions.
// ******************

// Function calculates x,y,z values corresponding parameter
// value u for a nurbs-curve.
// Currently only bspline-version !!!***!!!
void
nurbsCurveValue(struct ecif_NurbsCurve* curve_params, double u, Point3 result)
{
	bsplCurveValue_bf(curve_params, u, result);
}



