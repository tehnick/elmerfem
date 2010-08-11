/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUI helpers                                                         *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <iostream>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "helpers.h"

Helpers :: Helpers()
{
}

Helpers :: ~Helpers()
{
}

//====================================================================
//                             Normalize
//====================================================================

void Helpers::normalize(double *a)
{
  double b;

  b = vlen(a);
  a[0] /= b;
  a[1] /= b;
  a[2] /= b;
}

//====================================================================
//                              Length
//====================================================================

double Helpers::vlen(double *a)
{
  return sqrt(a[0]*a[0] + a[1]*a[1] + a[2]*a[2]);
}

//====================================================================
//                           Cross product
//====================================================================

void Helpers::crossProduct(double *a, double *b, double *c)
{
  c[0] = a[1]*b[2] - a[2]*b[1];
  c[1] = a[2]*b[0] - a[0]*b[2];
  c[2] = a[0]*b[1] - a[1]*b[0];
}

//====================================================================
//            Invert 4x4 matrix (for visualiztion only)
//====================================================================
void Helpers::invertMatrix(const double *a, double *inva)
{
#define E4(A, i, j) (A)[4*i+j]
#define E8(A, i, j) (A)[8*i+j]

  int i, j, k;
  double c[32];
  double cik, ckk;

  // Initialize:
  //-------------
  memset(c, 0, 32*sizeof(double));

  for(i = 0; i < 4; i++) {
    E8(c, i, i + 4) = 1.0;

    for(j = 0; j < 4; j++)
      E8(c, i, j) = E4(a, i, j);
  }

  // Eliminate:
  //------------
  for(k = 0; k < 4; k++) {
    ckk = E8(c, k, k);

    if(ckk == 0.0) return;

    for(j = 0; j < 8; j++)
      E8(c, k, j) /= ckk;

    for(i = 0; i < 4; i++) {
      if(i == k) continue;

      cik = E8(c, i, k);

      for(j = 0; j < 8; j++)
	E8(c, i, j) -= cik * E8(c, k, j);
    }
  }

  // Result:
  //---------
  for(i = 0; i < 4; i++) {
    for(j = 0; j < 4; j++)
      E4(inva, i, j) = E8(c, i, j + 4);
  }

#undef E8
#undef E4
}
