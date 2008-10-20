#ifdef MATC
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
     int nsize=n*sizeof(double);
     double *outf = (double *)malloc(3*nsize);
     for( int i=0; i<nsteps; i++ )
     {
       vtkp->grad( &M(in,0,i*n),outf );

       memcpy( &M(out,0,i*n), &outf[0], nsize );
       memcpy( &M(out,1,i*n), &outf[n], nsize );
       memcpy( &M(out,2,i*n), &outf[2*n], nsize );
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
     int nsize=n*sizeof(double);
     double *inf = (double *)malloc(3*nsize);
     for( int i=0; i<nsteps; i++ )
     {
       memcpy( &inf[0], &M(in,0,i*n), nsize );
       memcpy( &inf[n], &M(in,1,i*n), nsize );
       memcpy( &inf[2*n], &M(in,2,i*n), nsize );

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
     int nsize=n*sizeof(double);
     double *inf  = (double *)malloc(3*nsize);
     double *outf = (double *)malloc(3*nsize);
     for( int i=0; i<nsteps; i++ )
     {
       memcpy( &inf[0], &M(in,0,i*n), nsize);
       memcpy( &inf[n], &M(in,1,i*n), nsize);
       memcpy( &inf[2*n], &M(in,2,i*n), nsize);

       vtkp->curl(inf,outf);

       memcpy( &M(out,0,i*n), &outf[0], nsize );
       memcpy( &M(out,1,i*n), &outf[n], nsize );
       memcpy( &M(out,2,i*n), &outf[2*n], nsize );
     }
     free(inf); free(outf);
   }
   return out;
}
#endif
