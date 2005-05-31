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
 * Includes for X & OpenGL.
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
 * $Id: graphics.h,v 1.4 2001/06/29 12:00:05 jpr Exp $
 *
 * $Log: graphics.h,v $
 * Revision 1.4  2001/06/29 12:00:05  jpr
 * *** empty log message ***
 *
 * Revision 1.3  2001/06/13 07:55:53  jpr
 * *** empty log message ***
 *
 * Revision 1.2  1998/07/31 13:36:55  jpr
 *
 * Added id, started log.
 *
 *
 ******************************************************************************/

#include <X11/X.h>
#if 0
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/extensions/XInput.h>
#endif
#include <X11/keysymdef.h>

#include "../glaux/glaux.h"

#include <GL/gl.h>

/*
 *  Material for lightning computations
 */
typedef struct material_s
{
    struct material_s *Next;

    char *Name;

    int Changed;

    float Shininess;
    float Diffuse[4],Specular[4];
} material_t;

/*
 * 
 */
typedef struct rgb_s
{
    unsigned char r,g,b;
} rgb_t;

typedef struct colormap_s
{
    struct colormap_s *Next;

    char *Name;

    int Changed;

    rgb_t *Values; 
    int NumberOfEntries;
} colormap_t;

#ifdef MODULE_GRAPHICS

material_t DefaultMaterial =
{
   NULL, NULL, TRUE, 20.0, { 0.8,0.8,0.8,1.0 }, { 0.0,0.0,0.0,1.0 }
};
material_t DefaultEdgeMaterial =
{
   NULL, NULL, TRUE, 20.0, { 0.8,0.8,0.8,1.0 }, { 0.0,0.0,0.0,1.0 }
};
material_t def_mat =
{
   NULL, NULL, TRUE, 20.0, { 0.8,0.8,0.8,1.0 }, { 0.0,0.0,0.0,1.0 }
};

colormap_t DefaultColorMap =
{
    NULL, NULL, TRUE, NULL, 0
};
colormap_t def_map =
{
    NULL, NULL, TRUE, NULL, 0
};

colormap_t *ArrowColorMap   = &DefaultColorMap, *MeshColorMap = &DefaultColorMap,
           *ContourColorMap = &DefaultColorMap, *IsoSurfaceColorMap = &DefaultColorMap,
           *SphereColorMap  = &DefaultColorMap, *ParticleColorMap = &DefaultColorMap;

material_t *ArrowMaterial   = &DefaultMaterial, *MeshMaterial = &DefaultMaterial,
           *ContourMaterial = &DefaultMaterial, *IsoSurfaceMaterial = &DefaultMaterial,
           *SphereMaterial  = &DefaultMaterial, *ParticleMaterial = &DefaultMaterial;

#else

extern colormap_t DefaultColorMap,def_map;

extern colormap_t *ArrowColorMap,*MeshColorMap,*ContourColorMap,
                  *IsoSurfaceColorMap,*SphereColorMap,*ParticleColorMap;

extern material_t DefaultMaterial,DefaultEdgeMaterial,def_mat;
extern material_t *ArrowMaterial,*MeshMaterial,*ContourMaterial,
                  *IsoSurfaceMaterial,*SphereMaterial,*ParticleMaterial;
#endif


/*
 * Try keeping direct softcalls to OpenGL in graphics module...
 */
#define GRA_FILL      (GL_FILL)
#define GRA_LINE      (GL_LINE)

#define GRA_POINTS    (GL_POINTS)
#define GRA_LINES     (GL_LINES)
#define GRA_QUADS     (GL_QUADS)
#define GRA_TRIANGLES (GL_TRIANGLES)

#define gra_end() glEnd()
#define gra_begin( Mode ) glBegin( Mode )

#define gra_lon()  glEnable( GL_LIGHTING )
#define gra_loff() glDisable( GL_LIGHTING )

#define gra_beg_lines() { glDisable( GL_LIGHTING ); glBegin( GL_LINES ); }
#define gra_end_lines() { glEnd(); glEnable( GL_LIGHTING ); }

#define gra_line_width(a) glLineWidth(a)

#define gra_push_matrix() glPushMatrix()
#define gra_pop_matrix()  glPopMatrix()
#define gra_mult_matrix(Matrix) glMultMatrixd( (GLdouble *)Matrix )
#define gra_load_identity() glLoadIdentity()

#define gra_look_at(fx,fy,fz,tx,ty,tz,ux,uy,uz) gluLookAt(fx,fy,fz,tx,ty,tz,ux,uy,uz)

#define gra_polygon_mode( Mode ) glPolygonMode( GL_FRONT_AND_BACK, Mode )


