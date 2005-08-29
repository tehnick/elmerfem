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
 *****************************************************************************/

/******************************************************************************
 *
 *
 *
 ******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 02 Jun 1997
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 *****************************************************************************/

#include "../../config.h"

#if defined(MINGW32) | defined(WIN32) 

double second() 
{ 
  return 0.0; 
}


#else
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#include <sys/time.h>

static struct tms t;

static struct timeval tp;
static struct timezone tzp;

#ifndef HZ
#define HZ CLK_TCK
#endif

double second( )
{
    times( &t );
    return (double)t.tms_utime / (double)HZ;

    gettimeofday(&tp,&tzp);
    return tp.tv_sec + tp.tv_usec/1.0e6;
}
#endif
