/*******************************************************************************
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
 * Main include file of ElmerPost. Mainly includes other include files ;-)
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
 *                       Date: 26 Sep 1995
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 ******************************************************************************/

#ifdef WIN32
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>

#include <malloc.h>
#include <math.h>

#ifndef WIN32
#include <termio.h>
#endif
#include <sys/types.h>

#include <signal.h>

#include <elmer/matc.h>

#include "tk/tk.h"

#ifdef MODULE_MAIN
#define EXT 
#else
#define EXT extern
#endif

#ifndef MIN
#define MIN(x,y) ( (x)>(y) ? (y) : (x) )
#endif

#ifndef MAX
#define MAX(x,y) ( (x)>(y) ? (x) : (y) )
#endif

#ifndef ABS
#define ABS(x) ( (x)>(0) ? (x) : (-(x)) )
#endif

#define FALSE 0
#define TRUE  1

#ifndef DBL_MAX
#define DBL_MAX            1.79769313486231570e+308
#endif

#ifndef M_PI
#define M_PI (3.1415926535897931)
#endif

typedef unsigned char logical_t;

typedef struct
{
    char *name;
    double *f;
    double min,max;
} scalar_t;

typedef struct
{
    char *name;
    double *f;
    double min[3],max[3];
} vector_t;

typedef struct
{
    int VolumeSides;
    int SurfaceSides;
    int StereoMode;
    int OutputPS, FitToPagePS;
    double StereoTran,StereoRot;
} global_options_t;

#ifdef MODULE_MAIN
  global_options_t GlobalOptions = { FALSE,TRUE,FALSE,FALSE,TRUE,0.03,5.00 };
#else
  extern global_options_t GlobalOptions;
#endif

extern double RealTime(),CPUTime();

#include "geometry.h"
#include "elements/elements.h"
#include "graphics/graphics.h"
#include "visuals/visual.h"
#include "objects/objects.h"
#include "camera/camera.h"

EXT unsigned int epMouseDown,epMouseDownTakesTooLong;
EXT double GraphicsAspect;
EXT unsigned int GraphicsXSize,GraphicsYSize;

#ifndef WIN32
EXT XFontStruct *CurrentXFont;
#endif

EXT int BreakLoop;

#ifdef MODULE_MAIN
  void (*user_hook_before_all)()    = NULL;
  void (*user_hook_after_all)()     = NULL;

  void (*user_hook_camera_before)() = NULL;
  void (*user_hook_camera_after)()  = NULL;

  void (*user_hook_object_before)() = NULL;
  void (*user_hook_object_after)()  = NULL;
#else
  extern void (*user_hook_before_all)();
  extern void (*user_hook_after_all)();

  extern void (*user_hook_camera_before)();
  extern void (*user_hook_camera_after)();

  extern void (*user_hook_object_before)();
  extern void (*user_hook_object_after)();
#endif
