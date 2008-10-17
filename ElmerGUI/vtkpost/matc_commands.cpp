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
   for( int i=0; i<nsteps; i++ )
     vtkp->grad(&M(in,0,i*n),&M(out,0,3*i*n));

   return out;
}

extern "C" VARIABLE *com_div(VARIABLE *in)
{
   VARIABLE *out; 
   int n=vtkp->NofNodes(),nsteps=NCOL(in)/n;

   out = var_temp_new(TYPE_DOUBLE,1,NCOL(in));
   for( int i=0; i<nsteps; i++ )
     vtkp->div(&M(in,0,3*i*n),&M(out,0,i*n));
   return out;
}

extern "C" VARIABLE *com_curl(VARIABLE *in)
{
   VARIABLE *out; 
   int n=vtkp->NofNodes(),nsteps=NCOL(in)/n;

   out = var_temp_new(TYPE_DOUBLE,3,NCOL(in));
   for( int i=0; i<nsteps; i++ )
     vtkp->curl(&M(in,0,3*i*n),&M(out,0,3*i*n));
   return out;
}
#endif
