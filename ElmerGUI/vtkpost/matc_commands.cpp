#ifdef MATC
#include <stdio.h>
#include "../src/mainwindow.h"
#include <mc.h>
#include "vtkpost.h"

extern "C" VARIABLE *var_temp_new(int,int,int);
extern VtkPost *vtkp;

extern "C" VARIABLE *com_grad(VARIABLE *in)
{
   VARIABLE *out; 
   out = var_temp_new(TYPE_DOUBLE,3,NCOL(in));
   vtkp->grad(MATR(in),MATR(out));

   return out;
}
#endif
