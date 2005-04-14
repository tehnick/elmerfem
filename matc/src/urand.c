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
 ******************************************************************************/

/*******************************************************************************
 *
 *     Random number generator.
 *
 *******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02101 Espoo, Finland
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 30 May 1996
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 ******************************************************************************/
/***********************************************************************
|
|  URAND.C - Last Edited 6. 8. 1988
|
***********************************************************************/

/*======================================================================
|Syntax of the manual pages:
|
|FUNCTION NAME(...) params ...
|
$  usage of the function and type of the parameters
?  explane the effects of the function
=  return value and the type of value if not of type int
@  globals effected directly by this routine
!  current known bugs or limitations
&  functions called by this function
~  these functions may interest you as an alternative function or
|  because they control this function somehow
^=====================================================================*/


/*
 * $Id: urand.c,v 1.2 1998/08/01 12:34:57 jpr Exp $ 
 *
 * $Log: urand.c,v $
 * Revision 1.2  1998/08/01 12:34:57  jpr
 *
 * Added Id, started Log.
 * 
 *
 */

#include "matc.h"

double s, halfm; int m2 = 0, itwo = 2, ia, ic, m, mic;

double urand(iy) int *iy;
/*======================================================================
?  urand is a uniform random number generator based  on  theory  and
|  suggestions  given  in  d.e. knuth (1969),  vol  2.   the integer  iy
|  should be initialized to an arbitrary integer prior to the first call
|  to urand.  the calling program should  not  alter  the  value  of  iy
|  between  subsequent calls to urand.  values of urand will be returned
|  in the interval (0,1).
|  see forsythe, malcolm and moler (1977).
^=====================================================================*/
{
  
  if (m2 == 0)
  {
    
    /* if first entry, compute machine integer word length */
    
    m = 1;
    do
    {
      m2 = m;
      m = itwo * m2;
    } while(m > m2);
    halfm = m2;
    
    /* compute multiplier and increment for linear congruential method */
    
    ia = 8 * (int)(halfm * atan(1.0) / 8.00) + 5;
    ic = 2*(int)(halfm * (0.50 - sqrt(3.0) / 6.0)) + 1;
    mic = (m2 - ic) + m2;
    
    /* s is the scale factor for converting to floating point */
    
    s = 0.5 / halfm;
    
  }
  
  /* compute next random number */
  
  *iy = *iy * ia;
  
  /*
    the following statement is for computers which do not allow
    integer overflow on addition
    */
  
  if (*iy > mic) *iy = (*iy - m2) - m2;
  
  *iy = *iy + ic;
  
  /*
    the following statement is for computers where the
    word length for addition is greater than for multiplication
    */
  
  if (*iy / 2 > m2) *iy = (*iy - m2) - m2;
  
  /*
    the following statement is for computers where integer
    overflow affects the sign bit
    */
  
  if (*iy < 0) *iy = (*iy + m2) + m2;

  return *iy * s;
}
