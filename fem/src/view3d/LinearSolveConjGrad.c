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

#include <ViewFactors.h>

static void LinearSolveResidual( Geometry_t *Geom,double UpdateValue,double *ViewFactorRowScale,double *R )
{
     GeometryList_t *Link = Geom->Link,*Diag=NULL;

     while( Link )
     {
         if ( Link->Entry != Geom )
         {
             UpdateValue += Link->ViewFactor*Link->Entry->B;
         } else Diag = Link;

         Link = Link->Next;
     }

     if ( Geom->Flags & GEOMETRY_FLAG_LEAF )
     {
         double Area = Geom->Area,B=Geom->B,M=Geom->M,E=Geom->E;
         int N = Geom->N;

         R[N] = E*M + UpdateValue*ViewFactorRowScale[N];

         if ( Diag )
         {
             R[N] -= (M-Diag->ViewFactor*ViewFactorRowScale[N])*B;
         }
         else
         {
             R[N] -= M*B;
         }

         R[N] *= Area;
         Geom->P = R[N];

         return;
     }
    
     LinearSolveResidual( Geom->Left,UpdateValue,ViewFactorRowScale,R );
     LinearSolveResidual( Geom->Right,UpdateValue,ViewFactorRowScale,R );
}

static void LinearSolveGather( Geometry_t *Geom,double UpdateValue,double *ViewFactorRowScale,double *x,double *Ax )
{
     GeometryList_t *Link = Geom->Link,*Diag=NULL;

     while( Link )
     {
         if ( Link->Entry != Geom )
         {
             UpdateValue += Link->ViewFactor*Link->Entry->P;
         } else Diag = Link;

         Link = Link->Next;
     }

     if ( Geom->Flags & GEOMETRY_FLAG_LEAF )
     {
         double Area = Geom->Area,P=Geom->P,M=Geom->M;
         int N = Geom->N;

         Ax[N] = -UpdateValue*ViewFactorRowScale[N];

         if ( Diag )
         {
             Ax[N] += (M-Diag->ViewFactor*ViewFactorRowScale[N])*P;
         }
         else
         {
             Ax[N] += M*P;
         }

         Ax[N] *= Area;
         x[N] = P;

         return;
     }
    
     LinearSolveGather( Geom->Left,UpdateValue,ViewFactorRowScale,x,Ax );
     LinearSolveGather( Geom->Right,UpdateValue,ViewFactorRowScale,x,Ax );
}

static double LinearSolveUpdateB( Geometry_t *Geom,double Alpha,double *Norm )
{
    double B=0.0,Area = Geom->Area;

    if ( Geom->Flags & GEOMETRY_FLAG_LEAF )
    {
        Geom->B += Alpha*Geom->P;
        *Norm += Geom->B*Geom->B;

        return Geom->B*Area;
    }

    B += LinearSolveUpdateB( Geom->Left,Alpha,Norm );
    B += LinearSolveUpdateB( Geom->Right,Alpha,Norm );

    Geom->B = B / Area;

    return B;
}

static double LinearSolveUpdateRP( Geometry_t *Geom,double Alpha,double Beta,double *R,double *AP )
{
    double P=0.0,Area = Geom->Area;
    int N = Geom->N;

    if ( Geom->Flags & GEOMETRY_FLAG_LEAF )
    {
        R[N] -= Alpha*AP[N];
        Geom->P = R[N] + Beta*Geom->P;

        return Geom->P*Area;
    }

    P += LinearSolveUpdateRP( Geom->Left,Alpha,Beta,R,AP );
    P += LinearSolveUpdateRP( Geom->Right,Alpha,Beta,R,AP );

    Geom->P = P / Area;

    return P;
}

void LinearSolveConjugateGradient(Geometry_t Geometry[],int NGeom,int N,double *ViewFactorRowScale)
{
    double T,second(),Alpha,Beta,XNorm,FNorm,RNorm,R1Norm,s,*R,*P,*AP;
    int i,j,k,iter,CE,n=0,MAX_CONJ_GRAD_ITER=200;

    FILE *fp;

    R  = (double *)calloc( N,sizeof(double) );
    P  = (double *)calloc( N,sizeof(double) );
    AP = (double *)calloc( N,sizeof(double) );

T = second();

    for( i=0; i<NGeom; i++ ) LinearSolveResidual( &Geometry[i],0.0,ViewFactorRowScale,R );
    for( i=0; i<NGeom; i++ ) LinearSolveUpdateB( &Geometry[i],0.0,&XNorm );
    for( i=0; i<NGeom; i++ ) LinearSolveUpdateRP( &Geometry[i],0.0,0.0,R,AP );

    FNorm = 0.0;
    for( i=0; i<N; i++ ) FNorm += R[i]*R[i];

    for( iter=0; iter<MAX_CONJ_GRAD_ITER; iter++ )
    {
        for( i=0; i<NGeom; i++ )
        {
            LinearSolveGather( &Geometry[i],0.0,ViewFactorRowScale,P,AP );
        }

        RNorm = R1Norm = Alpha = Beta = 0.0;

        for( i=0; i<N; i++ ) RNorm += R[i]*R[i];
        for( i=0; i<N; i++ ) Alpha += P[i]*AP[i];
        Alpha = RNorm/Alpha;

        for( i=0; i<N; i++ )
        {
            s = R[i]-Alpha*AP[i];
            R1Norm += s*s;
        }
        Beta  = R1Norm/RNorm;
/*
 *      fprintf( stderr, "ITERATION: %d, RES: %g,%g,%g,%g\n", iter,R1Norm,Alpha,Beta,RNorm );
 */
        XNorm = 0.0;
        for( i=0; i<NGeom; i++ ) LinearSolveUpdateB( &Geometry[i],Alpha,&XNorm );
        for( i=0; i<NGeom; i++ ) LinearSolveUpdateRP( &Geometry[i],Alpha,Beta,R,AP );
        if ( R1Norm < 1.0E-24 /*|| R1Norm/XNorm < 1.0E-16*/ ) break;
    }

    fprintf( stderr, "LINSOLVE TIME: %g,Res (first,last,x):%g, %g, %g, ITER: %d\n", second()-T,FNorm,R1Norm,XNorm,iter );
}
