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

/* -----------------------------:  femfact.c  :---------------------------

   This module includes the subroutines for calculting the view factors 
   for a closure where the elements see one another. The view factors 
   form a N*N matrix, if there are N side elements all together. This 
   module is largely based on the work of Juha Katajamaki (Piikiteen 
   kasvatuista simuloivan ohjelman mikrotietokonesovellus, diplomityo, 
   TKK 1993). 
   */


#include <stdio.h>
#include <math.h>
#include "common.h"
#include "nrutil.h"
#include "femdef.h"
#include "femtypes.h"
#include "femknot.h"
#include "femsolve.h"
#include "femfact.h" 

#define DEBUG 0


/* Local variables of the module */
static Real r1,r2,z1,z2; 	/* surface being viewed */
static Real r3,r4,z3,z4; 	/* surface doing the viewing */
static Real r12,r34,z12,z34;    /* coordinates for the centerline */
static Real zd1,zd3,zd;         /* differences z1-z2, z3-z4, z12-z34 */
static Real rd1,rd3,rd;         /* r1-r2, r3-r4, r12-r34 */
static Real g1,g3,d1,d3,t,rratio; /* Just for help. */

/* Small numbers for various cases. These should 
   really be comparable to the mesh dimensions. */
static const Real eps = 1.e-7, eps2 = 1.e-16;
static const Real delta = 1.e-6;  /* the largest difference of cosines that causes an integration */
static int nsurf;                 /* the dimension of the viewfactor-matrix */



void ViewFactors(struct FemType *data,struct BoundaryType *bound,
		 int norm)
/* Calculates the view factors for a boundary given in the structure. 
   If the flag 'norm' is on, the factors are also symmetrisized and normalized. 
   errtoler is the maximum tolerated error. If the error is exceeded the 
   division is doubled until it's maxdiv at the most. 
   The view factors are saved to 'bound->vf'.
   */
{
  int sideelemtype,i,j,ii,jj,div1,div3,open;
  Real *a,**vf;         /* The side element areas */
  Real c1, c2;          /* The maximum and minumum of the direction cosine. */
  Real _r1,_r2,_r3,_r4,_z1,_z2,_z3,_z4;  /* the original coordinates that may be devided to parts */
  const int divinit = 4;	         /* minumum number of parts */
  Real rowsum,integr;
  Real length3,length1,_maxr1,_maxr3;
  Real maxrowsum=0.0,minrowsum=2.0;
  static int visited = 0;
  int ind[MAXNODESD1];

  Real dr1,dr3,dz1,dz3,n1[2],n3[2],zm1,zm3;
  int nosee;

  /* Note that the next ones might be included in the function call. */
  Real errtoler=1.0;
  int divmax = 20;

  nsurf = bound->nosides;
  a = bound->areas;
  vf = bound->vf;
  open = bound->open;
  visited++;


  for (i=1; i<=nsurf; i++) { 

    GetElementSide(bound->parent[i],bound->side[i],bound->normal[i],data,ind,&sideelemtype);

    _r3 = data->x[ind[0]];
    _r4 = data->x[ind[1]];
    _z3 = data->y[ind[0]];
    _z4 = data->y[ind[1]];
    _maxr3 = MAX(_r3,_r4);

    if(0) printf("i=%d i0=%d  i1=%d  r=%.3le dr=%.3le z=%.3le dz=%.3le side=%d\n",
	   i,ind[0],ind[1],_r4,_r4-_r3,_z4,_z4-_z3,bound->side[i]);

    if(_maxr3 < eps2) {
      printf("Error in subroutine ViewFactors:\n");
      printf("For side %d the rmax is %.3lg\n",i,_maxr3);
      printf("This makes it impossible to calculate the view factors!\n");      
      return;      
    }
  }
  
  
  for (i=1; i<=nsurf; i++) {

    /* The coordinates are ordered so that the outer normal 
       of the surface points always to the left. */

    GetElementSide(bound->parent[i],bound->side[i],bound->normal[i],data,ind,&sideelemtype);

    _r3 = data->x[ind[0]];
    _r4 = data->x[ind[1]];
    _z3 = data->y[ind[0]];
    _z4 = data->y[ind[1]];

    /* divisions of the viewing element */
    dr3 = _r4-_r3;
    dz3 = _z4-_z3;
    length3 = sqrt(dr3*dr3 + dz3*dz3);
    _maxr3 = MAX(_r3,_r4);
    if(_maxr3 > eps2) div3 = (int)(divinit*length3/_maxr3);
    div3 = MAX(divinit,div3);
    div3 = MIN(divmax,div3);
    
    do {
      rowsum = 0.;
      for (j=1; j<=nsurf; j++) {
	
	vf[j][i] = 0.;

	GetElementSide(bound->parent[j],bound->side[j],bound->normal[j],data,ind,&sideelemtype);

	_r1 = data->x[ind[0]];
	_r2 = data->x[ind[1]];
	_z1 = data->y[ind[0]];
	_z2 = data->y[ind[1]];

#if DEBUG
	if(visited == 0) {
	  printf("nr1 = %.3lg  nr3=%.3lg\n",n1[0],n3[0]);
	  printf("nz1 = %.3lg  nz3=%.3lg\n",n1[1],n3[1]);
	  printf("zm1 = %.3lg  zm3=%.3lg\n",zm1,zm3);
	  printf("test = %.3lg\n",(n1[1]-n3[1])*(zm1-zm3));
	}
#endif

        /* divisions of the viewed element */
	dr1 = _r2-_r1;
	dz1 = _z2-_z1;
        length1 = sqrt(dr1*dr1 + dz1*dz1);
        _maxr1 = MAX(_r1,_r2);

        if(_maxr1 > eps2) div1 = (int)(divinit*length1/_maxr1);
        div1 = MAX(divinit,div1);
        div1 = MIN(divmax,div1);
	
        if(length1/div1 > _maxr1/4.) 
          div1 = (int)(4.*length1/_maxr1);


#if 0
	n1[0] = -dz1/length1;  
	n1[1] = dr1/length1;   
	zm1 = 0.5*(_z1+_z2);

	n3[0] = -dz3/length3;  
	n3[1] = dr3/length3;   
	zm3 = 0.5*(_z3+_z4);

	nosee = FALSE;
	/* Both elements are looking outwards. */ 
	if((n1[0] >= 0.0)  &&  (n3[0] >= 0.0))  {
	  /* Are the same element */
	  if(i == j) 
	    nosee = TRUE;
	  if(zm1-zm3 > 0.0  &&  n1[1]-n3[1] > -1.0e-3)
	    nosee = TRUE;
	  if(zm1-zm3 < 0.0  &&  n1[1]-n3[1] < 1.0e-3)
	    nosee = TRUE;
	}

	if(nosee) continue;
#endif

	for (ii=0; ii<div3; ii++) {
	  r3 = _r3 + (_r4 - _r3) * ii/div3;
	  r4 = _r3 + (_r4 - _r3) * (ii+1.)/div3;
	  z3 = _z3 + (_z4 - _z3) * ii/div3;
	  z4 = _z3 + (_z4 - _z3) * (ii+1.)/div3;
	  
	  r34 = .5*(r3+r4);
	  if(r34 < eps) continue;
	  z34 = .5*(z3+z4);
	  zd3 = z3-z4;
	  rd3 = r3-r4;

	  for (jj=0; jj<div1; jj++) {
	    r1 = _r1 + (_r2 - _r1) * jj/div1;
	    r2 = _r1 + (_r2 - _r1) * (jj+1.)/div1;
	    z1 = _z1 + (_z2 - _z1) * jj/div1;
	    z2 = _z1 + (_z2 - _z1) * (jj+1.)/div1;
	    
	    r12 = .5*(r1+r2);
	    if (r12 < eps) continue;
	    
	    if (r1 < eps) r1 = eps;
	    if (r2 < eps) r2 = eps;
	    if (r3 < eps) r3 = eps;
	    if (r4 < eps) r4 = eps; 
	    
	    zd1 = z1-z2;
	    rd1 = r1-r2; 
	    z12 = .5*(z1+z2);
	    zd = z12-z34;
	    rd = r12-r34;
	    
	    /* Check that the angle is under 90 degrees. */
	    if (InitialInterval(&c1,&c2)) { 		
	      /* Calculate a single view factor */
	      integr = ViewIntegral(data,bound,c1,c2,1); 

	      vf[j][i] += 4. * integr;
	      /* Factor 4. is due to axisymmetry (2pi), mirror symmetry (2) and 
		 constant 1/pi. */
	    }	
	  }
	}		   
	rowsum += vf[j][i];
      }
      rowsum /= a[i];
    } while (fabs(1.-rowsum) >= errtoler && !open);

    /* The rowsum should be one if the calculation is exact. */

    if(rowsum > maxrowsum) maxrowsum = rowsum;
    if(rowsum < minrowsum) minrowsum = rowsum;

    if(nsurf>=200 && i%50==0) 
      printf("View factors:  row: %d / %d  rowsum = %.2le\n",
	     i,nsurf,rowsum); 

  } /* for i */
#if DEBUG 
  SaveRealMatrix(vf,1,nsurf,1,nsurf,"vf.dat");
#endif

  printf("Calculated %d view factors with rowsums in interval [%.3lg, %.3lg].\n",
	 nsurf*nsurf,minrowsum,maxrowsum);
  if(0) SaveRealMatrix(vf,1,nsurf,1,nsurf,"vfi.dat");

#if DEBUG
    printf("Symmetrizing view factors.\n");
#endif
    Symmetrize(vf,nsurf);

  if(norm && !open) {
#if DEBUG
    printf("Normalizing view factors.\n");
#endif
    Normalize(vf,a,nsurf);
  }

  /* Devide with the particular area. */
  for (i=1; i<=nsurf; i++) 
    for (j=1; j<=nsurf; j++)  {
      vf[j][i] /= a[j];
    }
#if DEBUG
  SaveRealMatrix(vf,1,nsurf,1,nsurf,"vf.dat");
#endif
}




int InitialInterval(Real *c1, Real *c2)
/*  Gives tha upper and lower boundaries for the cosines to be calculted. 
    The boundaries are calculted from the requirement that the angle between 
    the normals of the surfaces and the line connecting the surfaces must be 
    less than 90 degrees. If the elements don't see each other at all 
    FALSE is returned. 
    */
{
  Real cc1, cc3; 
  
  *c1 = -1.; *c2 = 1.;
  if ( fabs(zd1) > eps ) {
    cc1 = (- zd * rd1 + r12 * zd1) / (r34 * zd1);        
    if ( fabs(zd3) > eps ) {
      cc3 = (zd * rd3 + r34 * zd3) / (r12 * zd3);        
      if (zd1 > 0.) {
	if (zd3 > 0.) *c1 = MAX(cc1, cc3);
	else { *c1 = cc1; *c2 = cc3; }
      } else {
	if (zd3 < 0.) *c2 = MIN(cc1, cc3);
	else { *c1 = cc3; *c2 = cc1; }
      }
    } else {
      if ( SGN(rd3) && SGN(rd3) == -SGN(zd) ) {
        if (zd1 > 0.) *c1 = cc1;
        else *c2 = cc1;
      } else { *c1 = 1.; *c2 = -1.; } /* Empty */
    }
  } else {
    if ( fabs(zd3) > eps ) {
      cc3 = (zd * rd3 + r34 * zd3) / (r12 * zd3);
      if ( SGN(rd1) && SGN(rd1) == SGN(zd) ) {
	if (zd3 > 0.) *c1 = cc3;
	else *c2 = cc3;
      } else { *c1 = 1.; *c2 = -1.; } /* Empty */
    } else {
      if ( !SGN(rd1) || SGN(rd1) != SGN(zd) || SGN(rd1) != -SGN(rd3) )
        { *c1 = 1.; *c2 = -1.; }  /* Else = [-1, 1] */
    }
  }
  
  /* to certify that no division by zero takes place */
  *c1 = MAX(-1.+eps, *c1); 
  *c2 = MIN(1.-eps, *c2);
  
  if (*c2 - *c1 < eps) return FALSE;
  return TRUE;
}




Real ViewIntegral(struct FemType *data,struct BoundaryType *bound,
		  Real c1, Real c2, int k)
/* This function calcultes the view factors for a pair of elements, the coordinates
   of which are in variables r1, r2, z1, z2, r3, r4, z3, z4. The shading of elements
   is taking into account. If the shading devides the integration into two parts 
   recursive calls are used. Variable 'k' is the first index of a surface that
   has not yet been tested for shading. Originally it's one and in recursive calls
   something else.
   */
{
  static Real r5, r6, z5, z6;    /* surface doing the shading */
  static Real zd5, t1, tt1, t2, tt2, t0;
  Real cc1,cc2,integr;
  int ind[MAXNODESD1],sideelemtype;

  rratio = r34/r12;
  
  while (k <= nsurf) {

    GetElementSide(bound->parent[k],bound->side[k],bound->normal[k],data,ind,&sideelemtype);

    r5 = data->x[ind[0]];
    r6 = data->x[ind[1]];
    z5 = data->y[ind[0]];
    z6 = data->y[ind[1]];
    k++;
    if (r5+r6 < eps) continue;
    
    /* shading of one piece [cc1, cc2]: */
    /* 1) element that shades is a disk */
    if ( fabs(zd5 = z5-z6) < eps ) {
      if ( fabs(zd) < eps ) continue;
      t1 = (z12-z5)/zd; tt1 = 1.-t1;
      if (t1 < eps || tt1 < eps) continue;
      t = rratio * t1/tt1;
      cc1 = .5*(r5*r5/(r12*r34*t1*tt1) - t - 1./t);
      cc2 = .5*(r6*r6/(r12*r34*t1*tt1) - t - 1./t);
      if (cc1 > cc2) { t = cc1; cc1 = cc2; cc2 = t; }
    } 
    
    /* 2) element that shades is a frustrum or a cylinder */
    else {
      /* the shaded part in z-direction ([t1, t2]) */
      if ( fabs(zd) < eps ) {
	/* the connecting line is horizontal, thus the shading is complete or non-existing */
	if ( (z12-z5 < eps && z12-z6 > eps) || (z12-z6 < eps && z12-z5 > eps) ) 
	  { t1 = 0.; t2 = 1; }
	else continue;
      } 
      else {
	t1 = (z12-z5)/zd; 
	t2 = t1 + zd5/zd;
	if (t1 > t2) 
	  { t = t1; t1 = t2; t2 = t; }
      }
      
      if (!IntervalIsect(0., 1., t1, t2, &t1, &t2)) continue;
      tt1 = 1.-t1; 
      tt2 = 1.-t2;
      
      /* what values does the cosine get in the interval [t1, t2] */
      cc1 = 1.; cc2 = -1.;
      g1 = (r5 * (z12-z6) - r6 * (z12-z5)) / (r12 * zd5);
      g3 = (r5 * (z34-z6) - r6 * (z34-z5)) / (r34 * zd5);
      d1 = g1*g1 - 1; 
      d3 = g3*g3 - 1;  
      /* the sign indicates in which side of the frustrum the viewing and viewed points are */
      
      /* investigate the end points of the interval */
      ExaminePoint (t1, &cc1, &cc2);
      ExaminePoint (t2, &cc1, &cc2);
      /* if both points are outside the frustrum, investigate the point where the derivative
	 vanishes, if it's on the interval [t1,t2] */
      if (d1 <= -eps && d3 <= -eps) {
	t0 = 1. / (1. + sqrt(rratio * d3/d1));
	if (t0 - t1 > eps && t2 - t0 > eps)
	  ExaminePoint(t0, &cc1, &cc2);
      }
      if (cc1 > cc2) cc1 = cc2; /* case of round offs */
    }
    
    /* Calculate the differences of intervals [c1,c2] and [cc1,cc2] and take it as the 
       next interval to be considered. If the interval has two parts apply recursive
       calls. */
    if (IntervalIsect(c1, c2, cc1, cc2, &cc1, &cc2)) {
      if (cc1 - c1 < delta)
	if (c2 - cc2 < delta) return 0.;
	else c1 = cc2;
      else if (c2 - cc2 < delta) c2 = cc1;
    }
  }

  integr = Integrate(c1,c2);
    
  return(integr);
}



int IntervalIsect(Real x1, Real x2, Real y1, Real y2, Real *z1, Real *z2)
/* Calculate the combination (and) of intervals [x1,x2] and [y1,y2]. 
   If it's almost zero return false. 
   */
{
  *z1 = x1; *z2 = x2;
  if (x2 - y1 < eps) return FALSE;
  if (y1 - x1 > eps) *z1 = y1;
  if (y2 - x1 < eps) return FALSE;
  if (x2 - y2 > eps) *z2 = y2;
  
  return(*z2 - *z1 >= eps);
}


void ExaminePoint (Real x, Real *mi, Real *ma)
/* A function to investigate the shading. It evaluates at a given point the function f(t)
   which describes the values of cosines for which the surfaces sees one another. Global
   variables 'g1','g3','d1','d3' and 'rratio' are assumed to be predetermined.
   'x'	- the argument of the function to be evalueted
   'mi'	- the minimum of the function so far, which is updated if f(x) is smaller
   'mi'	- the maximum of the function so far, which is updated if f(x) is larger
   */
{
  Real y;
  if (x > eps) {
    if (1.-x > eps) {
      t = rratio*x/(1.-x);
      y = .5*(d1/t + d3*t) + g1*g3;
    } 
    else if ( fabs(d3) < eps ) y = g1*g3;
    else y = SGN(d3);
  } 
  else if ( fabs(d1) < eps ) y = g1*g3;
  else y = SGN(d1);
  if (y > *ma) *ma = y;
  if (y < *mi) *mi = y;
}



Real Integrate(Real c1, Real c2)
/* Calculate the integral over 3 variables. The Stokes theorem is used to 
   eliminate one variable. The two variables left are the one which moves 
   at the centerline of the viewing surface and another one on the boundary 
   of the viewed element. The integration over the viewing surface is done 
   numerically. c1 and c2 are the intervals of the cosine. The coordinates 
   r1,r2,z1,z2,r3,r4,z3,z4 are assumed to be predetermined.
   */
{
  /* Different quadratures on interval [0,1] may be used, but the first 
     and last point may not be end points. */
  
#if 0
  static const Real qp[] = { 1e-6, .25, .5, .75, 1.-1e-6 },
  w[] = { 1./12., 1./3., 1./6., 1./3., 1./12. };
  static const int nqp = 5;
#endif
#if 0
  static const Real qp[] = { 0.211324865, 0.788675134 },
  w[] = { .5, .5 };  
  static const int nqp = 2;
#endif
#if 1
  /* 3-point Gaussian quadrature */
  static const Real qp[] = { 0.112701665, 0.5, 0.887298334 },
  w[] = { 0.277777777, 0.444444444, 0.277777777 };
  static const int nqp = 3;
#endif
  
  int i;
  Real c,z,r,h,hh1,hh2,g1,g2,gg1,gg2,value,integral;
  Real d1,d2,e1,e2,f1,f2;
  Real zrd,a1,a2,b1,b2,s1,s2,cs,cd;
  
  c = zd1*zd1 + rd1*rd1;
  if (c < eps2) return(0.);
  /* The area is only a narrow line. This certifies that there are no division by 0. */
  
  zrd = r2*z1-r1*z2;
  a1 = rd3*r1, a2 = rd3*r2;
  b1 = zd3*z1, b2 = zd3*z2; 
  s1 = sqrt(1. - c1*c1), s2 = sqrt(1. - c2*c2);
  /* Note that the indices in sin and cos are opposite to the usual convention. */
  cs = (1.+c1)*(1.+c2), cd = (1.-c1)*(1.-c2);
  
  integral = 0.;
  
  for (i=0; i<nqp; i++) {
    /* Set the viewing point (r,z) */
    z = z3 - qp[i] * zd3;  /*  qp is the integral variable */
    r = r3 - qp[i] * rd3;
    
    /* These are used in the calculation. */
    e1 = (z1-z)*(z1-z) + r1*r1 + r*r;
    f1 = (z2-z)*(z2-z) + r2*r2 + r*r;
    hh1 = 2*r1*r;
    hh2 = 2*r2*r;
    g1 = - e1 / hh1;
    g2 = - f1 / hh2;
    e2 = e1 - c1*hh1;
    f2 = f1 - c1*hh2;
    e1 -= c2*hh1;
    f1 -= c2*hh2;
    h = zd3*z + rd3*r;
    gg1 = (g1+c2)*(g1+c1);
    gg2 = (g2+c2)*(g2+c1);
    
    /* the value of the integral over a sector with a fixed viewing point */
    value = (-.5 * (a1 + (h-b1)*g1) / sqrt(g1*g1-1) ) *
      acos( .5 * ( (1.-g1) * sqrt(cd/gg1) - 
		  (1.+g1) * sqrt(cs/gg1) ) );
    value -= (-.5 * (a2 + (h-b2)*g2) / sqrt(g2*g2-1) ) *
      acos( .5 * ( (1.-g2) * sqrt(cd/gg2) - 
		  (1.+g2) * sqrt(cs/gg2) ) );
    value += .25 * (b1-b2) * acos(c1*c2 + s1*s2);
    
    /*integral over a line */
    gg1 = e1+f1-c; gg2 = e2+f2-c;
    hh1 = 4*e1*f1; hh2 = 4*e2*f2;
    d1 = hh1 - gg1*gg1; d2 = hh2 - gg2*gg2;
    h = r * (rd1*h + zrd*zd3);
    value -= h * (s1 / sqrt(d2)) * acos( gg2 / sqrt(hh2) );
    value += h * (s2 / sqrt(d1)) * acos( gg1 / sqrt(hh1) );
    
    integral += w[i] * value;
  }
  
  return(integral);
}

