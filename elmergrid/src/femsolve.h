/* femsolve.h */
/* This module includes the LU-decomposition algorithms for dense 
   linear matrices. There are also routines for symmetrisizing and 
   normalizing matrices. These are needed to correct the 
   discretization errors of the view factor calculations. */

void SortIndex(int n,double *arr,int *indx);
void ludcmp(Real **a, int n, int *indx, Real *d);
void lubksb(Real **a, int n, int *indx, Real b[]);
void Symmetrize(Real **vf,int sides);
void Normalize(Real **vf, const Real *b,int sides);
