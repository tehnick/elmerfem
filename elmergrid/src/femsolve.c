/*  
   ElmerGrid - A simple mesh generation and manipulation utility  
   Copyright (C) 1995- , CSC - Scientific Computing Ltd.   

   Author: Peter Råback
   Email: Peter.Raback@csc.fi
   Address: CSC - Scientific Computing Ltd.
            Keilaranta 14
            02101 Espoo, Finland

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

/* -------------------------------:  femsolve.c  :----------------------------

   This module is for solving a dense system of linear equations. For sparse systems
   a special sparse matrix library should be used.
*/


#include <math.h>
#include <stdio.h>
#include "common.h"
#include "nrutil.h"
#include "femdef.h"
#include "femsolve.h"


/* LU Decomposition */
#define TINY 1.0e-20;

void ludcmp(Real **a, int n, int *indx, Real *d)
{
  int i,imax,j,k;
  Real big,dum,sum,temp;
  Real *vv;

  vv=Rvector(1,n);
  *d=1.0;

  for(i=1;i<=n;i++) {
    big=0.0;
    for(j=1;j<=n;j++)
      if ((temp=fabs(a[i][j])) > big) big=temp;
    if (big == 0.0) nrerror("Singular matrix in routine ludcmp.");
    vv[i]=1.0/big;
  }
  for (j=1;j<=n;j++) {
    for (i=1;i<j;i++) {
      sum=a[i][j];
      for (k=1;k<i;k++) sum -= a[i][k]*a[k][j];
      a[i][j]=sum;
    }
    big=0.0;
    for (i=j;i<=n;i++) {
      sum=a[i][j];
      for (k=1;k<j;k++) sum -= a[i][k]*a[k][j];
      a[i][j]=sum;
      if( (dum=vv[i]*fabs(sum)) >= big ) {
        big=dum;
	imax=i;
      }
    }
    if (j != imax) {
      for (k=1;k<=n;k++) {
	dum=a[imax][k];
	a[imax][k]=a[j][k];
	a[j][k]=dum;
      }
      *d = -(*d);
      vv[imax]=vv[j];
    }
    indx[j]=imax;
    if (a[j][j] == 0.0) a[j][j]=TINY;
    if (j != n) {
      dum=1.0/(a[j][j]);
      for (i=j+1;i<=n;i++) a[i][j] *= dum;
    }
  }

  free_Rvector(vv,1,n);
}




void lubksb(Real **a, int n, int *indx, Real b[])
{
  int i,ii=0,ip,j;
  Real sum;

  for (i=1;i<=n;i++) {
    ip=indx[i];
    sum=b[ip];
    b[ip]=b[i];
    if (ii)
      for (j=ii; j<=i-1; j++) sum -= a[i][j]*b[j];
    else if (sum) ii=i;
    b[i]=sum;
  }
  for (i=n;i>=1;i--) {
    sum=b[i];
    for (j=i+1; j<=n; j++) sum -= a[i][j]*b[j];
    b[i]=sum/a[i][i];
  }
}


/*******************************************************************************/

void Symmetrize(Real **vf,int sides)
/* Symmetrize the matrix by replacing two elements that should be equal by their
   mean value. If the matrix is not symmetric, Symmetrize should be called before 
   the subroutine Normalize.
*/
{
  int i,j;
  Real temp;

  for (i=1;i<=sides;i++) 
    for (j=i+1;j<=sides;j++) {
      temp = .5*(vf[i][j] + vf[j][i]);
      vf[j][i] = vf[i][j] = temp;
    }
}


void Normalize(Real **vf, const Real *b,int sides)
/* Normalize a symmetric matrix F (vf) by solving a diagonal 
   matrix D (diag) so that the row sums in matrix DFD coincides 
   with those given in vector b. The Newton method 
   is used to solve the diagonal matrix D. 
*/
{
  /* Criteria for ending the iteration */
  int itmax = 10;
  Real eps = 1e-20;
  Real **jac,*rest,*diag,sum;
  int i,j,k,it;
  int *indx;
  Real evenodd;

  jac = Rmatrix(1,sides,1,sides);
  rest = Rvector(1,sides);
  diag = Rvector(1,sides);
  indx = Ivector(1,sides);

  for (i=1;i<=sides;i++) diag[i] = 1.;


  for(it=1;it<=itmax;it++) {

    /* Calculate the residual sum(DAD)-b. */
    for (i=1; i<=sides; i++) {
      sum = 0.;
      for (j=1; j<=sides; j++) 
        sum += diag[j] * vf[i][j];
      sum *= diag[i];
      rest[i] = sum - b[i];
    }
    sum = 0.;
    for (i=1; i<=sides; i++) 
      sum += rest[i]*rest[i]/b[i];
    sum /= sides;
    
    if(sum < eps) break;

    /* Make the Jacobian matrix. */
    for (i=1; i<=sides; i++) {
      jac[i][i] = 0.;
      for(j=1; j<=sides; j++)
	jac[i][j] = diag[i]*vf[i][j];
      for(k=1; k<=sides;k++)
        jac[i][i] += diag[k]*vf[i][k];
    }

    /* Solve the equation jac * x = rest using Numerical Recipes routines. */     
    ludcmp(jac,sides,indx,&evenodd);
    lubksb(jac,sides,indx,rest);

    /* New approximation. */
    for (i=1; i<=sides; i++)
      diag[i] -= rest[i];    
  } 

  /* Normalize the viewfactors. */
  for (i=1; i<=sides; i++)
    for (j=1; j<=sides; j++)
      vf[i][j] *= diag[i] * diag[j];

  free_Rmatrix(jac,1,sides,1,sides);
  free_Rvector(rest,1,sides);
  free_Rvector(diag,1,sides);
  free_Ivector(indx,1,sides);
}



/* Indexing algorithm, Creates an index table */
#define SWAPI(a,b) itemp=(a);(a)=(b);(b)=itemp;
#define M 7
#define NSTACK 50

void SortIndex(int n,double *arr,int *indx)
{
  int i,indxt,ir,itemp,j,k,l;
  int jstack,*istack;
  double a;

  ir = n;
  l = 1;
  jstack = 0;  
  istack = ivector(1,NSTACK);

  for(j=1;j<=n;j++) 
    indx[j] = j;

  for(;;) {
    if (ir-l < M) {
      for(j=l+1;j<=ir;j++) {
	indxt = indx[j];
	a = arr[indxt];
	for(i=j-1;i>=1;i--) {
	  if(arr[indx[i]] <= a) break;
	  indx[i+1] = indx[i];
	}
	indx[i+1] = indxt;
      }
      if(jstack == 0) break;
      ir = istack[jstack--];
      l = istack[jstack--];
    } 
    else {
      k = (l+ir) >>  1;
      SWAPI(indx[k],indx[l+1]);
      if(arr[indx[l+1]] > arr[indx[ir]]) {
	SWAPI(indx[l+1],indx[ir]);
      }
      if(arr[indx[l]] > arr[indx[ir]]) {
	SWAPI(indx[l],indx[ir]);
      }
      if(arr[indx[l+1]] > arr[indx[l]]) {
	SWAPI(indx[l+1],indx[l]);
      }
      i = l+1;
      j = ir;
      indxt = indx[l];
      a = arr[indxt];
      for(;;) {
	do i++; while(arr[indx[i]] < a);
	do j--; while(arr[indx[j]] > a);
	if(j < i) break;
	SWAPI(indx[i],indx[j]);
      }
      indx[l] = indx[j];
      indx[j] = indxt;
      jstack += 2;
      if(jstack > NSTACK) printf("NSTACK too small in SortIndex.");
      if(ir-i+1 >= j-l) {
	istack[jstack]   = ir;
	istack[jstack-1] = i;
	ir = j-1;
      } else {
	istack[jstack] = j-1;
	istack[jstack-1] = l;
	l = i;
      }
    }
  }
  free_ivector(istack,1,NSTACK);
}




