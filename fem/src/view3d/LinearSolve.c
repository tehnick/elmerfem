
/******************************************************************************
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
 *****************************************************************************/
/******************************************************************************
 *
 *
 *
 ******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 02 Jun 1997
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 *****************************************************************************/

#include "ViewFactors.h"

static double LinearSolveGather( Geometry_t *Geom,double UpdateValue,double *ViewFactorRowScale,double *Residual )
{
  GeometryList_t *Link=Geom->Link,*Diag=NULL;
  double Area = Geom->Area, B=0.0;

  while( Link )
    {
      if ( Geom != Link->Entry )
        {
	  UpdateValue += Link->ViewFactor*Link->Entry->M*Link->Entry->B;
        }
      else Diag = Link;

      Link = Link->Next;
    }

  if ( Geom->Flags & GEOMETRY_FLAG_LEAF )
    {
      double E=Geom->E,M=1.0;
      int N = Geom->N;

      B = E + UpdateValue*ViewFactorRowScale[N];

      if ( Diag ) M -= Diag->ViewFactor*Geom->M*ViewFactorRowScale[N];

      *Residual += ABS(B-M*Geom->B);
      B /= M;

      Geom->B = B;
      return Geom->B*Area;
    }
    
  B += LinearSolveGather( Geom->Left,UpdateValue,ViewFactorRowScale,Residual );
  B += LinearSolveGather( Geom->Right,UpdateValue,ViewFactorRowScale,Residual );

  Geom->B = B / Area;

  return B;
}

void LinearSolveGaussSeidel( Geometry_t *Geometry,int NGeom,double *ViewFactorRowScale )
{
  int i,iter,MAX_GAUSS_SEIDEL_ITER=200;
  double Residual,FirstResidual;
    
  double T,second();
  T = second();

  FirstResidual = 0.0;
  for( i=0; i<NGeom; i++ )
  {
      LinearSolveGather( &Geometry[i],0.0,ViewFactorRowScale,&FirstResidual );
  }

  for( iter=0; iter<MAX_GAUSS_SEIDEL_ITER; iter++ )
    {
      Residual = 0.0;
      for( i=0; i<NGeom; i++ )
	{
          LinearSolveGather( &Geometry[i],0.0,ViewFactorRowScale,&Residual );
	}
      if ( Residual < 1.0E-08 ) break;
      fprintf( stderr, "LINSOLVE TIME: %g,RES (first,latest): %g, %g, ITER: %d\n", second()-T,FirstResidual,Residual,iter );
    }
  fprintf( stderr, "LINSOLVE TIME: %g,RES (first,last): %g, %g, ITER: %d\n", second()-T,FirstResidual,Residual,iter );
}
