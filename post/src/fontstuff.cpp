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

static FTGLPixmapFont *Font;
static int FirstTime = 1;

extern "C" void fontstuff( double x, double y, 
			   double r, double g, double b, 
			   int fontsize, char *txt ) {
  
  static char *elmer_post_home;
  static char fontfile[2048];
  
  if( FirstTime ) {
    elmer_post_home = getenv( "ELMER_POST_HOME" );
    fprintf( stdout, "getenv: ELMER_POST_HOME=%s\n", elmer_post_home );
#if defined(WIN32)
    sprintf( fontfile, "%s\\lib\\FreeSans.ttf", elmer_post_home );
#else
    sprintf( fontfile, "%s/lib/FreeSans.ttf", elmer_post_home );
#endif
    fprintf( stdout, "Load font: %s\n", fontfile );
    fflush( stdout );
    Font = new FTGLPixmapFont( fontfile );    
    if( Font->Error() )
      {
	fprintf( stderr, "Load font failed!\n" );
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
