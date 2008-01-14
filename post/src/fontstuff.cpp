#include <stdlib.h>
#include <stdio.h>
#include <GL/gl.h>
#include <GL/glu.h>

#include "FTGLExtrdFont.h"
#include "FTGLOutlineFont.h"
#include "FTGLPolygonFont.h"
#include "FTGLTextureFont.h"
#include "FTGLPixmapFont.h"
#include "FTGLBitmapFont.h"

#if defined(WIN32) || defined(win32)
#define FONT_FILE "C:\\WINDOWS\\Fonts\\arial.ttf"
#else
#define FONT_FILE "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
#endif

static char* fontfile = FONT_FILE;
static FTGLPixmapFont* Font;
static int FirstTime = 1;

extern "C" void fontstuff( double x, double y, 
			   double r, double g, double b, 
			   int fontsize, char *txt ) {
  
  if( FirstTime ) {
    fprintf( stdout, "Opening font %s\n", fontfile );
    fflush( stdout );
    Font = new FTGLPixmapFont( fontfile );    
    if( Font->Error() )
      {
	fprintf( stderr, "Failed opening font!!!\n" );
	fflush( stderr );
	return;
      }
    FirstTime = 0;
  }
  
  glMatrixMode( GL_MODELVIEW );
  glPushMatrix();
  glLoadIdentity();
  glMatrixMode( GL_PROJECTION );
  glPushMatrix();
  glLoadIdentity();

  glColor3f( r, g, b );
  glRasterPos3f( x, y, 0.0 );
  
  Font->FaceSize( fontsize );
  Font->Render( txt );

  glPopMatrix();
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix();

  return;
}
