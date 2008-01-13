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

#define FONT_FILE "/usr/share/fonts/truetype/freefont/FreeSans.ttf"

static char* fontfile = FONT_FILE;
static FTGLBitmapFont* Font;
static int FirstTime = 1;

extern "C" void fontstuff(double,double,double,double,double,int,char*);

void fontstuff(double x, double y, double r, double g, double b, 
	       int fontsize, char *txt) {
  
  if( FirstTime ) {
    fprintf( stdout, "Opening font %s\n", fontfile );
    fflush( stdout );
    Font = new FTGLBitmapFont( fontfile );    
    if( Font->Error() )
      {
	fprintf( stderr, "Failed opening %s", fontfile );
	fflush( stderr );
	return;
      }
    FirstTime = 0;
  }
  
  Font->FaceSize( fontsize );
  
  glMatrixMode( GL_MODELVIEW );
  glPushMatrix();
  glLoadIdentity();

  glMatrixMode( GL_PROJECTION );
  glPushMatrix();
  glLoadIdentity();

  glRasterPos3f( x, y, 0.0 );
  
  glDisable( GL_LIGHTING );
  glDisable( GL_TEXTURE_1D );

  glColor3f( r, g, b );
  Font->Render( txt );

  glEnable( GL_TEXTURE_1D );
  glEnable( GL_LIGHTING );

  glPopMatrix();
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix();

  return;
}
