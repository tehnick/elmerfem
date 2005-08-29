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

#define MODULE_MAIN

/******************************************************************************

View factor computation.

Juha Ruokolainen/CSC - 24 Aug 1995

******************************************************************************/

#include <ViewFactors.h>
#include "../../config.h"

#if defined(WIN32) || defined(MINGW32) 
double drand48()
{
    return rand()/(1.0*(1<<15));
}
#endif


extern double ShapeFunctionMatrix3[3][3],ShapeFunctionMatrix4[4][4];


static int MaxLev;
/*******************************************************************************

Compute viewfactor from hierarchy

24 Aug 1995

*******************************************************************************/
static double ComputeViewFactorValue( Geometry_t *Geom,int Level )
{
     double S=0.0, Area=Geom->Area;

     GeometryList_t *Link = Geom->Link;

     while( Link )
     {
        S += Area * Link->ViewFactor;
        Link = Link->Next;
     }

     if ( !(Geom->Flags & GEOMETRY_FLAG_LEAF) )
     {
        S += ComputeViewFactorValue( Geom->Left,Level+1 );
        S += ComputeViewFactorValue( Geom->Right,Level+1 );
     } else
       MaxLev = MAX(MaxLev,Level);

     return S;
}

static void PrintMesh( Geometry_t *Geom )
{
   static FILE *nfp=NULL,*efp=NULL;
   static int nodes;

   if ( !Geom->Left )
   {
     if ( !nfp )
     {
        nfp = (FILE *)fopen( "nodes", "w" );
        efp = (FILE *)fopen( "elems", "w" );
     }

     if ( Geom->GeometryType == GEOMETRY_TRIANGLE )
     {
         fprintf( nfp, "%g %g %g\n", 
            TriangleValue(0.0,0.0,Geom->Triangle->PolyFactors[0]),
            TriangleValue(0.0,0.0,Geom->Triangle->PolyFactors[1]),
            TriangleValue(0.0,0.0,Geom->Triangle->PolyFactors[2]) );

         fprintf( nfp, "%g %g %g\n", 
            TriangleValue(1.0,0.0,Geom->Triangle->PolyFactors[0]),
            TriangleValue(1.0,0.0,Geom->Triangle->PolyFactors[1]),
            TriangleValue(1.0,0.0,Geom->Triangle->PolyFactors[2]) );

         fprintf( nfp, "%g %g %g\n", 
            TriangleValue(0.0,1.0,Geom->Triangle->PolyFactors[0]),
            TriangleValue(0.0,1.0,Geom->Triangle->PolyFactors[1]),
            TriangleValue(0.0,1.0,Geom->Triangle->PolyFactors[2]) );

        fprintf( efp, "1 303 %d %d %d\n",nodes,nodes+1,nodes+2 );
        nodes += 3;
     } else {
         fprintf( nfp, "%g %g %g\n", 
            BiLinearValue(0.0,0.0,Geom->BiLinear->PolyFactors[0]),
            BiLinearValue(0.0,0.0,Geom->BiLinear->PolyFactors[1]),
            BiLinearValue(0.0,0.0,Geom->BiLinear->PolyFactors[2]) );

         fprintf( nfp, "%g %g %g\n", 
            BiLinearValue(1.0,0.0,Geom->BiLinear->PolyFactors[0]),
            BiLinearValue(1.0,0.0,Geom->BiLinear->PolyFactors[1]),
            BiLinearValue(1.0,0.0,Geom->BiLinear->PolyFactors[2]) );

         fprintf( nfp, "%g %g %g\n", 
            BiLinearValue(1.0,1.0,Geom->BiLinear->PolyFactors[0]),
            BiLinearValue(1.0,1.0,Geom->BiLinear->PolyFactors[1]),
            BiLinearValue(1.0,1.0,Geom->BiLinear->PolyFactors[2]) );

         fprintf( nfp, "%g %g %g\n", 
            BiLinearValue(0.0,1.0,Geom->BiLinear->PolyFactors[0]),
            BiLinearValue(0.0,1.0,Geom->BiLinear->PolyFactors[1]),
            BiLinearValue(0.0,1.0,Geom->BiLinear->PolyFactors[2]) );

        fprintf( efp, "1 404 %d %d %d %d\n",nodes,nodes+1,nodes+2,nodes+3 );
        nodes += 4;
     }
   } else {
      PrintMesh( Geom->Left );
      PrintMesh( Geom->Right );
   }
}

/*******************************************************************************
*******************************************************************************/
static void FreeLinks( Geometry_t *Geom )
{
     GeometryList_t *Link=Geom->Link, *Link1;

     while( Link )
     {
        Link1 = Link->Next;
        free(Link);
        Link = Link1;
     }
     Geom->Link = NULL;

     if ( Geom->Flags & GEOMETRY_FLAG_LEAF ) 
     {
        Geom->Flags &= ~GEOMETRY_FLAG_LEAF;
        return;
     }

     if ( Geom->Left )  FreeLinks( Geom->Left );
     if ( Geom->Right ) FreeLinks( Geom->Right );
}


/*******************************************************************************
*******************************************************************************/
static void FreeChilds( Geometry_t *Geom )
{
    if ( !Geom ) return;

     FreeChilds( Geom->Left );
     FreeChilds( Geom->Right );

    free( Geom );
}



/*******************************************************************************

Compute viewfactors for elements of the model, and solve for Gebhardt factors,
or radiosity, if requested.

24 Aug 1995

*******************************************************************************/
static void IntegrateFromGeometry(int N,double *Factors)
{
    double T,s,F,Fmin=DBL_MAX,Fmax=-DBL_MAX,Favg=0.0,*RowSums,Fact;
    int i,j,k,Imin,Imax;

    GeometryList_t *Link;

    for( i=0; i<N; i++ )
    {
        Elements[i].Area = (*AreaCompute[Elements[i].GeometryType])(&Elements[i]);
        Elements[i].Flags |= GEOMETRY_FLAG_LEAF;
    }

    RowSums = (double *)calloc( N,sizeof(double) );

    MaxLev = 0;
    k = 0;
    for( i=0; i<N; i++ )
    {
         if ( Elements[i].Area<1.0e-10 ) continue;

         fprintf( stderr, "row = % 4d of %d: ",i+1,N );
         for( j=i; j<N; j++ )
         { 
            if ( Elements[j].Area<1.0e-10 ) continue;

            FreeLinks( &Elements[i] );
            FreeLinks( &Elements[j] );

            Elements[i].Flags |= GEOMETRY_FLAG_LEAF;
            Elements[j].Flags |= GEOMETRY_FLAG_LEAF;

            (*ViewFactorCompute[Elements[i].GeometryType])( &Elements[i],&Elements[j],0,0 );
  
            Fact = ComputeViewFactorValue( &Elements[i],0 );
            Factors[i*N+j] = Fact / Elements[i].Area;
            Factors[j*N+i] = Fact / Elements[j].Area;
  
#if 0
            if ( Factors[j] != 0.0 ) fprintf( stdout, "%d %d %g\n", i+1,j+1, Factors[j]);
#endif
         }

         fflush( stdout );

#if 0
         PrintMesh( &Elements[i] );
#endif
         FreeChilds( Elements[i].Left );
         Elements[i].Left = NULL;

         FreeChilds( Elements[i].Right );
         Elements[i].Right = NULL;

         RowSums[i] += Factors[i*N+i];
         for( j=i+1; j<N; j++ )
         {
           if ( Elements[j].Area < 1.0e-10 ) continue;

           RowSums[i] += Factors[i*N+j];
           RowSums[j] += Factors[j*N+i];
         }
         s = RowSums[i];
         if ( s < Fmin )
         {
            Fmin = s;
            Imin = i+1;
         }
         if ( s > Fmax )
         {
            Fmax = s;
            Imax = i+1;
         }
         Favg += s;
         k++;

         fprintf( stderr, "sum=%-4.2f, (min(%d)=%-4.2f, max(%d)=%-4.2f, avg=%-4.2f)\n", 
                        s,Imin,Fmin,Imax,Fmax,Favg/k );
    }

    free( RowSums );

#if 0
    for( i=0;i<N; i++ )
    {
      fprintf( stdout, "%g %g %g\n",
         BiLinearValue(0.0,0.0,Elements[i].BiLinear->PolyFactors[0]),
            BiLinearValue(0.0,0.0,Elements[i].BiLinear->PolyFactors[1]),
                BiLinearValue(0.0,0.0,Elements[i].BiLinear->PolyFactors[2]));      

            fprintf( stdout, "%g %g %g\n",
         BiLinearValue(1.0,0.0,Elements[i].BiLinear->PolyFactors[0]),
            BiLinearValue(1.0,0.0,Elements[i].BiLinear->PolyFactors[1]),
                BiLinearValue(1.0,0.0,Elements[i].BiLinear->PolyFactors[2]));

                        fprintf( stdout, "%g %g %g\n",
         BiLinearValue(1.0,1.0,Elements[i].BiLinear->PolyFactors[0]),
            BiLinearValue(1.0,1.0,Elements[i].BiLinear->PolyFactors[1]),
            BiLinearValue(1.0,1.0,Elements[i].BiLinear->PolyFactors[2]));
                        
                  fprintf( stdout, "%g %g %g\n",
         BiLinearValue(0.0,1.0,Elements[i].BiLinear->PolyFactors[0]),
            BiLinearValue(0.0,1.0,Elements[i].BiLinear->PolyFactors[1]),
                BiLinearValue(0.0,1.0,Elements[i].BiLinear->PolyFactors[2]));

    }

   for( i=0; i<N; i++ )
       fprintf( stdout, "1 404 %d %d %d %d\n", 4*i+0,4*i+1,4*i+2,4*i+3 );
          i=123;

    for( j=0; j<N; j++ )
      for( k=0; k<4; k++ )
        fprintf( stdout, "%g\n", Factors[i*N+j] );
#endif
}

void MakeViewFactorMatrix(int N,double *Factors,int NInteg,int NInteg3)
{
    double T[32],S[32];
    long int i,j,k,n;

    n = sqrt( NInteg ) + 0.5;

    switch( n )
    {
      case 1:
        T[0] = 0.0;
        S[0] = 2.0;

        N_Integ = 1;
      break;

      case 2:
        T[0] = -0.577350269189625;
        T[1] =  0.577350269189625;

        S[0] = 1.000000000000000;
        S[1] = 1.000000000000000;

        N_Integ = 2;
      break;

      case 4:
        T[0] = -0.861136311594052;
        T[1] = -0.339981043584856;
        T[2] =  0.339981043584856;
        T[3] =  0.861136311594052;

        S[0] =  0.347854845137454;
        S[1] =  0.652145154862546;
        S[2] =  0.652145154862546;
        S[3] =  0.347854845137454;

        N_Integ = 4;
      break;

      case 7:
        T[0] = -0.949107912342759;
        T[1] = -0.741531185599394;
        T[2] = -0.405845151377397;
        T[3] =  0.000000000000000;
        T[4] =  0.405845151377397;
        T[5] =  0.741531185599394;
        T[6] =  0.949107912342759;

        S[0] =  0.129484966168870;
        S[1] =  0.279705391489277;
        S[2] =  0.381830050505119;
        S[3] =  0.417959183673469;
        S[4] =  0.381830050505119;
        S[5] =  0.279705391489277;
        S[6] =  0.129484966168870;

        N_Integ = 7;
      break;
     }

     k = 0;
     for( i=0; i<N_Integ; i++ )
     {
        for( j=0; j<N_Integ; j++,k++ )
        {
/* FOR -1,1 */
           U_Integ[k] = T[i];
           V_Integ[k] = T[j];
           S_Integ[k] = S[i]*S[j];

/* FOR 0-1 */
           U_Integ[k] = 0.5*(T[i]+1.0);
           V_Integ[k] = 0.5*(T[j]+1.0);
           S_Integ[k] = S[i]*S[j]/4.0;
       }
       U_Integ1d[i] = 0.5*(T[i]+1.0);
       S_Integ1d[i] = 0.5*(S[i]+1.0);
    }
    N_Integ1d = N_Integ;
    N_Integ = k;


    switch( NInteg3 )
    {
      case 1:
         N_Integ3 = 1;

        U_Integ3[0] = 1.0/3.0;
        V_Integ3[0] = 1.0/3.0;
        S_Integ3[0] = 1.0/2.0;
      break;

      case 3:
        N_Integ3 = 3;

        U_Integ3[0] = 1.0/6.0;
        U_Integ3[1] = 2.0/3.0;
        U_Integ3[2] = 1.0/6.0;

        V_Integ3[0] = 1.0/6.0;
        V_Integ3[1] = 1.0/6.0;
        V_Integ3[2] = 2.0/3.0;

        S_Integ3[0] = 1.0/6.0;
        S_Integ3[1] = 1.0/6.0;
        S_Integ3[2] = 1.0/6.0;
      break;

      case 6:
        N_Integ3 = 6;

        U_Integ3[0] = 0.091576213509771;
        U_Integ3[1] = 0.816847572980459;
        U_Integ3[2] = 0.091576213509771;
        U_Integ3[3] = 0.445948490915965;
        U_Integ3[4] = 0.108103018168070;
        U_Integ3[5] = 0.445948490915965;

        V_Integ3[0] = 0.091576213509771;
        V_Integ3[1] = 0.091576213509771;
        V_Integ3[2] = 0.816847572980459;
        V_Integ3[3] = 0.445948490915965;
        V_Integ3[4] = 0.445948490915965;
        V_Integ3[5] = 0.108103018168070;

        S_Integ3[0] = 0.109951743655322 / 2;
        S_Integ3[1] = 0.109951743655322 / 2;
        S_Integ3[2] = 0.109951743655322 / 2;
        S_Integ3[3] = 0.223381589678011 / 2;
        S_Integ3[4] = 0.223381589678011 / 2;
        S_Integ3[5] = 0.223381589678011 / 2;
      break;
    }

    IntegrateFromGeometry( N,Factors );
}

void InitGeometryTypes()
{
    InitRayTracer( RayEPS );

    IntegrateDiffToArea[GEOMETRY_TRIANGLE]    = TriangleIntegrateDiffToArea;
    IntegrateDiffToArea[GEOMETRY_BILINEAR]    = BiLinearIntegrateDiffToArea;
    IntegrateDiffToArea[GEOMETRY_BICUBIC]     = BiCubicIntegrateDiffToArea;
    IntegrateDiffToArea[GEOMETRY_BIQUADRATIC] = BiQuadraticIntegrateDiffToArea;

    Subdivide[GEOMETRY_TRIANGLE]              = TriangleSubdivide;
    Subdivide[GEOMETRY_BILINEAR]              = BiLinearSubdivide;
    Subdivide[GEOMETRY_BICUBIC]               = BiCubicSubdivide;
    Subdivide[GEOMETRY_BIQUADRATIC]           = BiQuadraticSubdivide;

    AreaCompute[GEOMETRY_TRIANGLE]            = TriangleArea;
    AreaCompute[GEOMETRY_BILINEAR]            = BiLinearArea;
    AreaCompute[GEOMETRY_BICUBIC]             = BiCubicArea;
    AreaCompute[GEOMETRY_BIQUADRATIC]         = BiQuadraticArea;

    ViewFactorCompute[GEOMETRY_TRIANGLE]      = TriangleComputeViewFactors;
    ViewFactorCompute[GEOMETRY_BILINEAR]      = BiLinearComputeViewFactors;
    ViewFactorCompute[GEOMETRY_BICUBIC]       = BiCubicComputeViewFactors;
    ViewFactorCompute[GEOMETRY_BIQUADRATIC]   = BiQuadraticComputeViewFactors;
}


void STDCALLBULL FC_FUNC(viewfactors3d,VIEWFACTORS3D)
  ( int *N, int *Topo, int *Type, double *Coord, double *Normals, double *Factors,
    double *Feps, double *Aeps, double *Reps,int *NInteg,int *NInteg3 )
{
   int i,j,k,l,n;

   AreaEPS   = *Aeps; 
   RayEPS    = *Reps; 
   FactorEPS = *Feps; 

   elm_4node_quad_shape_functions(  ShapeFunctionMatrix4 );
                                                                                                                                     
   ShapeFunctionMatrix3[0][0] =  1.0;
   ShapeFunctionMatrix3[0][1] = -1.0;
   ShapeFunctionMatrix3[0][2] = -1.0;
                                                                                                                                     
   ShapeFunctionMatrix3[1][0] =  0.0;
   ShapeFunctionMatrix3[1][1] =  1.0;
   ShapeFunctionMatrix3[1][2] =  0.0;
                                                                                                                                     
   ShapeFunctionMatrix3[2][0] =  0.0;
   ShapeFunctionMatrix3[2][1] =  0.0;
   ShapeFunctionMatrix3[2][2] =  1.0;


   Elements = (Geometry_t *)calloc( *N,sizeof(Geometry_t) );

   for( i=0; i<*N; i++ )
   {
     if ( Type[i] == 404 )
     {
        Elements[i].GeometryType = GEOMETRY_BILINEAR;
        Elements[i].BiLinear = (BiLinear_t *)calloc( sizeof(BiLinear_t),1 );

        for( j=0; j<4; j++ )
        {
           for( k=0; k<4; k++ )
           for( n=0; n<3; n++ )
           {
              l = 3*Topo[4*i+k]+n;
              Elements[i].BiLinear->PolyFactors[n][j]   += ShapeFunctionMatrix4[k][j]*Coord[l];
              Elements[i].BiLinear->PolyFactors[n+3][j] += ShapeFunctionMatrix4[k][j]*Normals[3*i+n];
            }
         }
      } else if ( Type[i] == 303 )
      {
        Elements[i].GeometryType = GEOMETRY_TRIANGLE;
        Elements[i].Triangle = (Triangle_t *)calloc( sizeof(Triangle_t),1 );

        for( j=0; j<3; j++ )
        {
           for( k=0; k<3; k++ )
           for( n=0; n<3; n++ )
           {
              l = 3*Topo[4*i+k] + n;
              Elements[i].Triangle->PolyFactors[n][j]   += ShapeFunctionMatrix3[k][j]*Coord[l];
              Elements[i].Triangle->PolyFactors[n+3][j] += ShapeFunctionMatrix3[k][j]*Normals[3*i+n];
            }
        }
      }
   }

   InitGeometryTypes();
   InitVolumeBounds( 1, *N, Elements );
   MakeViewFactorMatrix( *N,Factors,*NInteg,*NInteg3 );
}


#if 0
void ReadParams()
{
    FILE *fp = fopen( "params.dat","r" );

    fscanf( fp, "%lf %lf %lf", &FactorEPS, &AreaEPS, &RayEPS );

    fclose( fp );
}

void main( int argc,char **argv)
{
    double *Factors;
    int i,j,N;

    ReadParams();

    InitGeometryTypes();
    MakeTestModelLinear();

    InitVolumeBounds( 1,NElements,Elements );

    Factors = calloc( NElements,sizeof(double) );
    MakeViewFactorMatrix( NElements,Factors,4,3 );
}
#endif
