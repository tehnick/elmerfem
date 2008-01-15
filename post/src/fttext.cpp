//-----------------------------------------------------------------------------
// Filename:    fttext.cpp
// Description: Provides FTGL text rendering functionality for ElmerPost
// Usage:       fttext string [x y]
//              ftfont font [size r g b]
//              display
// Requires:    ttf-fonts installed in $ELMER_POST_HOME/fonts/TrueType
// Written by:  Mikko Lyly
// Date:        15. Jan 2008
//-----------------------------------------------------------------------------
#if defined(FTGL)

#include <stdlib.h>
#include <stdio.h>
#include <GL/gl.h>
#include <tcl.h>

#include "FTGLExtrdFont.h"
#include "FTGLOutlineFont.h"
#include "FTGLPolygonFont.h"
#include "FTGLTextureFont.h"
#include "FTGLPixmapFont.h"
#include "FTGLBitmapFont.h"

#if defined(WIN32) || defined(win32)
#include <windows.h>
#endif

#define FTGLCHARLEN 1024

typedef struct {
  char txt[FTGLCHARLEN];
  double x, y;
  char ttf[FTGLCHARLEN];
  char current_ttf[FTGLCHARLEN];
  int size;
  double r, g, b;
  int init_ok;
  char ttffile[FTGLCHARLEN];
  FTGLPixmapFont *Font;
} ftgl_t;

static ftgl_t ftgl;

extern "C" void (*user_hook_before_all)();

static void FtInit() {
  strcpy(ftgl.txt, "");
  ftgl.x = -0.9;
  ftgl.y = -0.9;
  strcpy(ftgl.ttf, "FreeSans");
  ftgl.size = 40;
  ftgl.r = 1.0;
  ftgl.g = 1.0;
  ftgl.b = 1.0;
  ftgl.init_ok = 1;
  return;
}

extern "C" void FtRender() {
  unsigned int r = strlen(ftgl.current_ttf);
  unsigned int s = strlen(ftgl.ttf);
  unsigned int t = strcmp(ftgl.ttf, ftgl.current_ttf);

  if( (r!=s) || (t!=0)) {
    char *elmer_post_home = getenv("ELMER_POST_HOME");
    fprintf(stdout, "fttext: getenv: ELMER_POST_HOME=%s\n", 
	    elmer_post_home);
    fflush(stdout);
    
#if defined(WIN32) || defined(win32)
    sprintf(ftgl.ttffile, "%s\\fonts\\TrueType\\%s.ttf",
	    elmer_post_home, ftgl.ttf);
#else
    sprintf(ftgl.ttffile, "%s/fonts/TrueType/%s.ttf",
	    elmer_post_home, ftgl.ttf);
#endif

    fprintf(stdout, "fttext: load: %s\n", ftgl.ttffile);
    fflush(stdout);
    
    delete ftgl.Font;
    ftgl.Font = new FTGLPixmapFont(ftgl.ttffile);

    if(ftgl.Font->Error()) {
      fprintf(stderr, "fttext: load font failed\n");
      fflush(stderr);
      return;
    }
    memset(ftgl.current_ttf, 0, FTGLCHARLEN);
    strncpy(ftgl.current_ttf, ftgl.ttf, strlen(ftgl.ttf));
  }
  
  if(!ftgl.Font) {
    fprintf(stderr, "fttext: no font loaded\n");
    fflush(stderr);
    return;
  }

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glColor3f(ftgl.r, ftgl.g, ftgl.b);
  glRasterPos3f(ftgl.x, ftgl.y, 0.0);
  
  ftgl.Font->FaceSize(ftgl.size);
  ftgl.Font->Render(ftgl.txt);

  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();

  return;
}

extern "C" int FtFont(ClientData cl, Tcl_Interp *interp, 
		      int argc, char **argv) {
  if(!ftgl.init_ok)
    FtInit();
  
  if(argc > 1) 
    strcpy( ftgl.ttf, argv[1] );

  if(argc > 2)
    ftgl.size = atoi(argv[2]);

  if(argc > 3)
    ftgl.r = atof(argv[3]);

  if(argc > 4)
    ftgl.g = atof(argv[4]);

  if(argc > 5)
    ftgl.b = atof(argv[5]);
  
  Tcl_Eval(interp, "display");

  return TCL_OK;
}

extern "C" int FtText(ClientData cl, Tcl_Interp *interp, 
		      int argc, char **argv) {
  if(!ftgl.init_ok)
    FtInit();
  
  strcpy(ftgl.txt, "");
  ftgl.x = -0.9;
  ftgl.y = -0.9;
  
  if(argc > 1) 
    strcpy( ftgl.txt, argv[1] );
  
  if(argc > 2)
    ftgl.x = atof(argv[2]);
  
  if(argc > 3)
    ftgl.y = atof(argv[3]);
  
  user_hook_before_all = FtRender;

  Tcl_Eval(interp, "display");
  
  return TCL_OK;
}

#endif
