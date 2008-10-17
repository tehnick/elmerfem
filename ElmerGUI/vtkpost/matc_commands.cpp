#ifdef MATC
#include <stdio.h>
#include "mc.h"
#include "vtkpost.h"

extern "C" VARIABLE *var_temp_new(int,int,int);
extern VtkPost *vtkp;

extern "C" VARIABLE *com_grad(VARIABLE *in)
{
   VARIABLE *out; 
   int n=vtkp->NofNodes(),nsteps=NCOL(in)/n;

   out = var_temp_new(TYPE_DOUBLE,3,NCOL(in));
   if ( nsteps==1 ) {
     vtkp->grad(MATR(in), MATR(out) );
   } else {
     double *outf = (double *)malloc(3*n*sizeof(double) );
     for( int i=0; i<nsteps; i++ )
     {
       vtkp->grad( &M(in,0,i*n),outf );
       for( int j=0; j<n; j++ )
       {
         M(out,0,i*n+j) = outf[j];
         M(out,1,i*n+j) = outf[j+n];
         M(out,2,i*n+j) = outf[j+2*n];
       }
     }
     free(outf);
   }
   return out;
}

extern "C" VARIABLE *com_div(VARIABLE *in)
{
   VARIABLE *out; 
   int n=vtkp->NofNodes(),nsteps=NCOL(in)/n;

   out = var_temp_new(TYPE_DOUBLE,1,NCOL(in));
   if ( nsteps==1 ) {
     vtkp->div(MATR(in), MATR(out) );
   } else {
     double *inf = (double *)malloc(3*n*sizeof(double) );
     for( int i=0; i<nsteps; i++ )
     {
       for( int j=0; j<n; j++ )
       {
         inf[j]=M(in,0,i*n+j);
         inf[j+n]=M(in,1,i*n+j);
         inf[j+2*n]=M(in,2,i*n+j);
       }
       vtkp->div(inf,&M(out,0,i*n));
     }
     free(inf);
   }
   return out;
}

extern "C" VARIABLE *com_curl(VARIABLE *in)
{
   VARIABLE *out; 
   int n=vtkp->NofNodes(),nsteps=NCOL(in)/n;

   out = var_temp_new(TYPE_DOUBLE,3,NCOL(in));
   if ( nsteps==1 ) {
     vtkp->div(MATR(in), MATR(out) );
   } else {
     double *inf  = (double *)malloc(3*n*sizeof(double));
     double *outf = (double *)malloc(3*n*sizeof(double));
     for( int i=0; i<nsteps; i++ )
     {
       for( int j=0; j<n; j++ )
       {
         inf[j]=M(in,0,i*n+j);
         inf[j+n]=M(in,1,i*n+j);
         inf[j+2*n]=M(in,2,i*n+j);
       }
       vtkp->curl(inf,outf);
       for( int j=0; j<n; j++ )
       {
         M(out,0,i*n+j) = outf[j];
         M(out,1,i*n+j) = outf[j+n];
         M(out,2,i*n+j) = outf[j+2*n];
       }
     }
     free(inf); free(outf);
   }
   return out;
}
#endif




