//-----------------------------------------------------------------------------------
// Filename:    fttext.cpp
// Description: Provides FTGL text rendering functionality for ElmerPost
// Usage:       fttext string [x y]
//              ftfont font [size r g b]
// Compilation: export CFLAGS="-DFGL -I/usr/include/freetype2 -I/usr/include/FTGL"
//              export CXXFLAGS="-DFGL -I/usr/include/freetype2 -I/usr/include/FTGL"
//              export LIBS="-lfreetype -lftgl -liconv"
// Written by:  Mikko Lyly
// Date:        15. Jan 2008
//-----------------------------------------------------------------------------------
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

#include <iconv.h>

#define FTGLSTRLEN 1024

typedef struct {
  char txt[FTGLSTRLEN];
  double x, y;
  char ttf[FTGLSTRLEN];
  char current_ttf[FTGLSTRLEN];
  int size;
  double r, g, b;
  int init_ok;
  char ttffile[FTGLSTRLEN];
  FTGLPixmapFont *Font;
} ftgl_t;

typedef struct {
  iconv_t cd;
  char inbuf[FTGLSTRLEN];
  char outbuf[FTGLSTRLEN];
  size_t insize;
  size_t outsize;
} ic_t;

static ftgl_t ftgl;
static ic_t ic;

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
  ic.cd = iconv_open("ISO-8859-15", "UTF-8");
  return;
}

extern "C" void FtRender() {
  unsigned int r = strlen(ftgl.current_ttf);
  unsigned int s = strlen(ftgl.ttf);
  int t = strcmp(ftgl.ttf, ftgl.current_ttf);

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
      fprintf(stderr, "fttext: error: load font failed!\n");
      fflush(stderr);
      return;
    }
    memset(ftgl.current_ttf, 0, FTGLSTRLEN);
    strncpy(ftgl.current_ttf, ftgl.ttf, strlen(ftgl.ttf));
  }
  
  if(!ftgl.Font) {
    fprintf(stderr, "fttext: error: no font loaded!\n");
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
  
  memset(ic.inbuf, 0, FTGLSTRLEN);
  memset(ic.outbuf, 0, FTGLSTRLEN);

  strcpy(ftgl.txt, "");
  ftgl.x = -0.9;
  ftgl.y = -0.9;
  
  if(argc > 1) 
    strcpy( ic.inbuf, argv[1] );
  
  if(argc > 2)
    ftgl.x = atof(argv[2]);
  
  if(argc > 3)
    ftgl.y = atof(argv[3]);
  
  // TCL uses internally UTF-8. We want UTF-8859-15 for FTGL:
  //---------------------------------------------------------
  ic.insize = strlen(ic.inbuf);
  ic.outsize = FTGLSTRLEN;
  const char *inptr = ic.inbuf;
  char *outptr = ic.outbuf;
#if defined(WIN32) || defined(win32)
  iconv(ic.cd, (const char**) &inptr, &ic.insize, &outptr, &ic.outsize);
#else
  iconv(ic.cd, (char**) &inptr, &ic.insize, &outptr, &ic.outsize);
#endif
  strcpy(ftgl.txt, ic.outbuf );

  user_hook_before_all = FtRender;

  Tcl_Eval(interp, "display");
  
  return TCL_OK;
}

#endif
