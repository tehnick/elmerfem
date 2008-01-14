// fttext.c
//
// Module for displaying text using freetype2 fonts with ElmerPost. Based on libFTGL.
//
// Compile e.g. as follows:
//
//   Linux: gcc -shared -Wall -fPIC -I/usr/include/FTGL -I/usr/include/freetype2 -I/usr/include/tcl8.4 -O -o fttext.so fontstuff.cpp fttext.c -lGL -lGLU -ltcl8.4 -lfreetype -lftgl_pic
//
// Copy the shared library into $ELMER_POST_HOME/modules and ruin ElmerPost
//
// Usage:
//
//   Elmer-Post: fttext X Y String Size R G B
//
// Here X and Y must be within -1...1. Arguments R, G and B are optional.
//
// Written by: ML, 13. Jan. 2008

#if defined(WIN32) || defined(win32)
#include <windows.h>
#endif

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <GL/gl.h>
#include <tcl.h>

#if !(defined(WIN32) || defined(win32))
#include <errno.h>
extern int errno;
#endif

extern void (*user_hook_before_all)();
extern void (*user_hook_after_all)();

#define TXTLEN 256
static char txt[TXTLEN];
static double x, y, r, g, b;
static int fontsize;

void ftstuff() {
  fontstuff( x, y, r, g, b, fontsize ,txt );
}

static int Fttext( ClientData cl, Tcl_Interp *interp, int argc, char **argv ) {

  user_hook_before_all = ftstuff;

  // Defaults:
  //----------
  x = -0.45;
  y = -0.15;
  strcpy( txt, "text" );
  fontsize = 144;
  r = 1.0;
  g = 1.0;
  b = 1.0;

  // User defined arguments:
  //------------------------
  if( argc > 1 )
    x = atof(argv[1]);

  if( argc > 2 )
    y = atof(argv[2]);

  if( argc > 3 ) 
    strcpy( txt, argv[3] );

  if( argc > 4 )
    fontsize = atoi(argv[4]);

  if( argc > 5 )
    r = atof(argv[5]);

  if( argc > 6 )
    g = atof(argv[6]);

  if( argc > 7 )
    b = atof(argv[7]);

  return TCL_OK;
}

#if defined(WIN32) || defined(win32)
__declspec(dllexport)
#endif

int Fttext_Init( Tcl_Interp *interp ) {
  Tcl_CreateCommand( interp, "fttext", (Tcl_CmdProc *)Fttext,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  return TCL_OK;
}
