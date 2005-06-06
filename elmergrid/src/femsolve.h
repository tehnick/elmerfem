/* femsolve.h */
/* This module includes the LU-decomposition algorithms for dense 
   linear matrices. There are also routines for symmetrisizing and 
   normalizing matrices. These are needed to correct the 
   discretization errors of the view factor calculations. */

void NRindexx(int n,double *arr,int *indx);
void NRgaussj(Real **a,int n,Real **b,int m);
void NRludcmp(Real **a, int n, int *indx, Real *d);
void NRlubksb(Real **a, int n, int *indx, Real b[]);
void NRcholdc(Real **a, int n, Real p[]);
void NRcholsl(Real **a, int n, Real p[], Real b[], Real x[]);
void Symmetrize(Real **vf,int sides);
void Normalize(Real **vf, const Real *b,int sides);
