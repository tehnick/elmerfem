/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUI glwidget                                                        *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <QtGui>
#include <QtOpenGL>
#include <QWheelEvent>
#include <QKeyEvent>
#include <math.h>
#include <iostream>
#include <stdio.h>
#include "glwidget.h"
#include "mainwindow.h"

using namespace std;

// Construct glWidget...
//-----------------------------------------------------------------------------
GLWidget::GLWidget(QWidget *parent)
  : QGLWidget(parent)
{
  backgroundColor = Qt::white;
  surfaceColor = Qt::cyan;
  edgeColor = Qt::green;
  surfaceMeshColor = Qt::black;
  sharpEdgeColor = Qt::black;

  stateFlatShade = true;
  stateDrawSurfaceMesh = true;
  stateDrawVolumeMesh = false;
  stateDrawSharpEdges = true;
  stateDrawSurfaceElements = true;
  stateDrawEdgeElements = true;
  stateDrawCoordinates = false;
  stateDrawSurfaceNumbers = false;
  stateDrawEdgeNumbers = false;
  stateDrawNodeNumbers = false;
  stateDrawBoundaryIndex = false;
  stateDrawBodyIndex = false;
  stateBcColors = false;
  stateBodyColors = false;

  currentlySelectedBody = -1;

  lists = 0;

  drawScale = 1.0;
  drawTranslate[0] = 0.0;
  drawTranslate[1] = 0.0;
  drawTranslate[2] = 0.0;

  mesh = NULL;

  helpers = new Helpers;
  meshutils = new Meshutils;

  ctrlPressed = false;
  shiftPressed = false;
  altPressed = false;

  quadratic = gluNewQuadric();	// for coordinate axis
}



// dtor...
//-----------------------------------------------------------------------------
GLWidget::~GLWidget()
{
  makeCurrent();
  for(int i=0; i < (int)lists; i++) {
    list_t *l = &list[i];
    glDeleteLists(l->object, 1);
  }

  delete helpers;
  delete meshutils;
}



// Min size hint...
//-----------------------------------------------------------------------------
QSize GLWidget::minimumSizeHint() const
{
  return QSize(64, 64);
}


// Default size...
//-----------------------------------------------------------------------------
QSize GLWidget::sizeHint() const
{
  return QSize(720, 576);
}

// Init GL...
//-----------------------------------------------------------------------------
void GLWidget::initializeGL()
{
  cout << "Initialize GL" << endl;
  cout << "Vendor: " << glGetString(GL_VENDOR) << endl;
  cout << "Renderer: " << glGetString(GL_RENDERER) << endl;
  cout << "Version: " << glGetString(GL_VERSION) << endl;
  cout.flush();

  static GLfloat light_ambient[]  = {0.2, 0.2, 0.2, 1.0};
  static GLfloat light_diffuse[]  = {0.6, 0.6, 0.6, 1.0};
  static GLfloat light_specular[] = {1.0, 1.0, 1.0, 1.0};
  static GLfloat light_position[] = {0.0, 0.0, -5.0, 0.0};

  static GLfloat mat_ambient[]    = {0.2, 0.2, 0.2, 1.0};
  static GLfloat mat_diffuse[]    = {1.0, 1.0, 1.0, 1.0};
  static GLfloat mat_specular[]   = {0.9, 0.9, 0.9, 1.0};
  static GLfloat high_shininess[] = {20.0};

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 1);
  glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 1.0);
  glEnable(GL_LIGHTING);

  // glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER,1.0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);
  glEnable(GL_LIGHT0);  
   
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, high_shininess);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  glDepthRange(-10.0, 10.0);

  glShadeModel(GL_SMOOTH);
  // glEnable(GL_LINE_SMOOTH);

  glEnable(GL_NORMALIZE);

  qglClearColor(backgroundColor);
}



// Paint event...
//-----------------------------------------------------------------------------
void GLWidget::paintGL()
{
  float xabs[3], xrel[3];

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  if(lists) {
    for(int i=0; i<(int)lists; i++) {
      list_t *l = &list[i];

      if(l->visible) {
	glPushName(i);

	if((l->type == SURFACEMESHLIST) && stateDrawSurfaceMesh) {
	  
	  // translate slightly towards viewer
	  glMatrixMode(GL_PROJECTION);
	  glPushMatrix();
	  glTranslated(0, 0, 0.01);
	  glCallList(l->object); 
	  glPopMatrix();
	  glMatrixMode(GL_MODELVIEW);
	  
	} else if((l->type == VOLUMEMESHLIST) && stateDrawVolumeMesh) {
	  
	  // translate slightly towards viewer
	  glMatrixMode(GL_PROJECTION);
	  glPushMatrix();
	  glTranslated(0, 0, 0.01);
	  glCallList(l->object); 
	  glPopMatrix();
	  glMatrixMode(GL_MODELVIEW);
	  
	} else if ((l->type == SHARPEDGELIST) && stateDrawSharpEdges) {

	  // translate slightly towards viewer
	  glMatrixMode(GL_PROJECTION);
	  glPushMatrix();
	  glTranslated(0, 0, 0.01);
	  glCallList(l->object); 
	  glPopMatrix();
	  glMatrixMode(GL_MODELVIEW);

	} else if((l->type == EDGELIST) && stateDrawEdgeElements ) {
	  
	  // translate slightly towards viewer
	  glMatrixMode(GL_PROJECTION);
	  glPushMatrix();
	  glTranslated(0, 0, 0.02);
	  glCallList(l->object); 
	  glPopMatrix();
	  glMatrixMode(GL_MODELVIEW);

	} else if((l->type == SURFACELIST) && stateDrawSurfaceElements ) {

	  glCallList(l->object); 

	} else {
	  
	  glCallList(l->object); 
	  
	}

	glPopName();
      }
    }
  }

  if(stateDrawCoordinates) {
    // push a dummy name
    glPushName(0xffffffff);
    drawCoordinates();
    glPopName();
  }

  if(mesh) {
    if(stateDrawSurfaceNumbers) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glTranslated(0, 0, 0.1);
      glColor3d(0.5, 0, 0);
      
      for(int i=0; i < mesh->surfaces; i++) {
	surface_t *surface = &mesh->surface[i];
	int nodes = surface->code / 100;

	xabs[0] = xabs[1] = xabs[2] = 0.0;

	for(int j=0; j < nodes;j++) {
	  int ind = surface->node[j];
	  xabs[0] = xabs[0] + mesh->node[ind].x[0];
	  xabs[1] = xabs[1] + mesh->node[ind].x[1];
	  xabs[2] = xabs[2] + mesh->node[ind].x[2];
	}
	xrel[0] = (xabs[0]/nodes - drawTranslate[0]) / drawScale;
	xrel[1] = (xabs[1]/nodes - drawTranslate[1]) / drawScale;
	xrel[2] = (xabs[2]/nodes - drawTranslate[2]) / drawScale;
	
	renderText(xrel[0], xrel[1], xrel[2], QString::number(i+1) );
      }
       glPopMatrix();
       glMatrixMode(GL_MODELVIEW);
    }       
    
    if(stateDrawEdgeNumbers) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glTranslated(0, 0, 0.1);
      glColor3d(0.0, 0.5, 0);
      
      for(int i=0; i < mesh->edges; i++) {
	edge_t *edge = &mesh->edge[i];
	int nodes = edge->code / 100;
	
	xabs[0] = xabs[1] = xabs[2] = 0.0;
	
	for(int j=0; j < nodes;j++) {
	  int ind = edge->node[j];
	  xabs[0] = xabs[0] + mesh->node[ind].x[0];
	  xabs[1] = xabs[1] + mesh->node[ind].x[1];
	  xabs[2] = xabs[2] + mesh->node[ind].x[2];
	}
	xrel[0] = (xabs[0]/nodes - drawTranslate[0]) / drawScale;
	xrel[1] = (xabs[1]/nodes - drawTranslate[1]) / drawScale;
	xrel[2] = (xabs[2]/nodes - drawTranslate[2]) / drawScale;
	
	renderText(xrel[0], xrel[1], xrel[2], QString::number(i+1) );
      }
      glPopMatrix();
      glMatrixMode(GL_MODELVIEW);
    }       
    
    if(stateDrawNodeNumbers) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glTranslated(0, 0, 0.1);
      glColor3d(0, 0, 0.5);

       for(int i=0; i < mesh->nodes; i++) {
	 xabs[0] = mesh->node[i].x[0];
	 xabs[1] = mesh->node[i].x[1];
	 xabs[2] = mesh->node[i].x[2];
	 
	 xrel[0] = (xabs[0] - drawTranslate[0]) / drawScale;
	 xrel[1] = (xabs[1] - drawTranslate[1]) / drawScale;
	 xrel[2] = (xabs[2] - drawTranslate[2]) / drawScale;
	 
	 renderText(xrel[0], xrel[1], xrel[2], QString::number(i+1) );
       }
       glPopMatrix();
       glMatrixMode(GL_MODELVIEW);
    }

    if(stateDrawBoundaryIndex || stateDrawBodyIndex) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glTranslated(0, 0, 0.1);
      glColor3d(0.5, 0, 0);

      for(int i=0; i < mesh->edges; i++) {
	edge_t *edge = &mesh->edge[i];
	int nodes = edge->code / 100;
	
	xabs[0] = xabs[1] = xabs[2] = 0.0;
	
	for(int j=0; j < nodes;j++) {
	  int ind = edge->node[j];
	  xabs[0] = xabs[0] + mesh->node[ind].x[0];
	  xabs[1] = xabs[1] + mesh->node[ind].x[1];
	  xabs[2] = xabs[2] + mesh->node[ind].x[2];
	}
	xrel[0] = (xabs[0]/nodes - drawTranslate[0]) / drawScale;
	xrel[1] = (xabs[1]/nodes - drawTranslate[1]) / drawScale;
	xrel[2] = (xabs[2]/nodes - drawTranslate[2]) / drawScale;
	
	if(stateDrawBoundaryIndex && (edge->nature == PDE_BOUNDARY))
	  renderText(xrel[0], xrel[1], xrel[2], QString::number(edge->index) );

	if(stateDrawBodyIndex && (edge->nature == PDE_BULK))
	  renderText(xrel[0], xrel[1], xrel[2], QString::number(edge->index) );
      }
      
      for(int i=0; i < mesh->surfaces; i++) {
	surface_t *surface = &mesh->surface[i];
	int nodes = surface->code / 100;

	xabs[0] = xabs[1] = xabs[2] = 0.0;
	
	for(int j=0; j < nodes; j++) {
	  int ind = surface->node[j];
	  xabs[0] = xabs[0] + mesh->node[ind].x[0];
	  xabs[1] = xabs[1] + mesh->node[ind].x[1];
	  xabs[2] = xabs[2] + mesh->node[ind].x[2];
	}
	xrel[0] = (xabs[0]/nodes - drawTranslate[0]) / drawScale;
	xrel[1] = (xabs[1]/nodes - drawTranslate[1]) / drawScale;
	xrel[2] = (xabs[2]/nodes - drawTranslate[2]) / drawScale;
	
	if(stateDrawBoundaryIndex && (surface->nature == PDE_BOUNDARY))
	  renderText(xrel[0], xrel[1], xrel[2], QString::number(surface->index) );

	if(stateDrawBodyIndex && (surface->nature == PDE_BULK))
	  renderText(xrel[0], xrel[1], xrel[2], QString::number(surface->index) );

	// case 3d:
	if(stateDrawBodyIndex && (surface->nature == PDE_BOUNDARY)) {
	  for(int i = 0; i < surface->elements; i++) {
	    int j = surface->element[i];
	    if(j >= 0) {
	      element_t *element = &mesh->element[j];
	      renderText(xrel[0], xrel[1], xrel[2], QString::number(element->index) );
	    }
	  }
	}

      }

      glPopMatrix();
      glMatrixMode(GL_MODELVIEW);
    }
  }
}



// Resize window...
//-----------------------------------------------------------------------------
void GLWidget::resizeGL(int width, int height)
{
  double top = 1.0;
  double bottom = -1.0;
  double left = -(double)width / (double)height;
  double right = (double)width / (double)height;
  double _near = -10.0;
  double _far = 10.0;

  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(left, right, bottom, top, _near, _far);
  glMatrixMode(GL_MODELVIEW);
}


// Focus in event...
//-----------------------------------------------------------------------------
void GLWidget::focusInEvent(QFocusEvent*)
{
  // Should we check the key pressed status here?
}


// Key pressed...
//-----------------------------------------------------------------------------
void GLWidget::keyPressEvent(QKeyEvent *event)
{
  if(event->key() == Qt::Key_Control)
    ctrlPressed = true;

  if(event->key() == Qt::Key_Shift)
    shiftPressed = true;

  if((event->key() == Qt::Key_Alt) || (event->key() == Qt::Key_AltGr))
    altPressed = true;
}


// Key released...
//-----------------------------------------------------------------------------
void GLWidget::keyReleaseEvent(QKeyEvent *event)
{
  if(event->key() == Qt::Key_Control)
    ctrlPressed = false;

  if(event->key() == Qt::Key_Shift)
    shiftPressed = false;

  if(event->key() == Qt::Key_Alt)
    altPressed = false;
}



// Mouse button clicked...
//-----------------------------------------------------------------------------
void GLWidget::mousePressEvent(QMouseEvent *event)
{
  lastPos = event->pos();
  setFocus();  // for tracing keyboard events
}



// Mouse wheel rotates...
//-----------------------------------------------------------------------------
void GLWidget::wheelEvent(QWheelEvent *event)
{
  double s = exp(-(double)(event->delta())*0.001);
  glScaled(s, s, s);
  updateGL();
  lastPos = event->pos();
  getMatrix();
}



// Mouse moves...
//-----------------------------------------------------------------------------
void GLWidget::mouseMoveEvent(QMouseEvent *event)
{
  GLint viewport[4];
  glGetIntegerv(GL_VIEWPORT, viewport);

  int dx = event->x() - lastPos.x();
  int dy = event->y() - lastPos.y();

  dy = -dy;
  
  if ((event->buttons() & Qt::MidButton) ||
      ((event->buttons() & Qt::LeftButton) && 
       (event->buttons() & Qt::RightButton)) ) {

    // Scale:
    double s = exp(-dy*0.01);
    glScaled(s, s, s);
    updateGL();

  } else if (event->buttons() & Qt::LeftButton) {
    
    // Rotation:
    double ax = -(double)dy;
    double ay =  (double)dx;
    double az = 0.0;

    if ( event->buttons() & Qt::RightButton ) {
       az = ay;
       ay = 0;
    }

    double s = 180.0*sqrt(ax*ax+ay*ay+az*az)/(double)(viewport[3]+1);
    double bx = invmatrix[0]*ax + invmatrix[4]*ay + invmatrix[8]*az;
    double by = invmatrix[1]*ax + invmatrix[5]*ay + invmatrix[9]*az;
    double bz = invmatrix[2]*ax + invmatrix[6]*ay + invmatrix[10]*az;
    glRotated(s, bx, by, bz);
    updateGL();

  } else if (event->buttons() & Qt::RightButton) {

    // Translation:
    double s = 2.0/(double)(viewport[3]+1);
    double ax = s*dx;
    double ay = s*dy;
    double az = 0.0;
    glLoadIdentity();
    glTranslated(ax, ay, az);
    glMultMatrixd(matrix);
    updateGL();
  }

  lastPos = event->pos();
  getMatrix();
}



// Mouse button double clicked...
//-----------------------------------------------------------------------------
void GLWidget::mouseDoubleClickEvent(QMouseEvent *event)
{
  if(lists == 0) 
    return;

  static list_t dummylist;
  static GLuint buffer[1024];
  const int bufferSize = sizeof(buffer)/sizeof(GLuint);
  
  GLint viewport[4];
  GLdouble projection[16];

  GLint hits;
  GLint i, j;

  updateGL();
  
  glSelectBuffer(bufferSize, buffer);
  glRenderMode(GL_SELECT);
  glInitNames();

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_PROJECTION_MATRIX, projection);
  glLoadIdentity();
  
  GLdouble x = event->x();
  GLdouble y = (double)viewport[3]-event->y()-1;
  
  GLdouble deltaX = 3.0;
  GLdouble deltaY = 3.0;
  
  gluPickMatrix(x, y, deltaX, deltaY, viewport);
  glMultMatrixd(projection); 
  
  glMatrixMode(GL_MODELVIEW);

  // updateGL();
  paintGL();

  hits = glRenderMode(GL_RENDER);
  
  GLuint smallestz = 0xffffffff;
  GLuint nearest = 0xffffffff;

  if(hits != 0) {
    for (i=0, j=0; i<hits; i++) {
      GLuint minz = buffer[j+1];
      GLuint resultz = buffer[j+3];
      
      if(minz < smallestz) {
	nearest = resultz;
	smallestz = minz;
      }

      j += 3 + buffer[j];
    }
  }
  
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);

  // highlight the selected boundary:
  if(nearest != 0xffffffff) {
    list_t *l = &list[nearest];

    // skip sharp edge lists
    if(l->type == SHARPEDGELIST) 
      return;
    
    // substitute surfacemeshlist with the parent surfacelist:
    if(l->type == SURFACEMESHLIST)
      l = &list[l->parent];

    // if not ctrl pressed, rebuild all selected lists except this one:
    if(!ctrlPressed) {
      for(i=0; i < lists; i++) {
	list_t *l2 = &list[i];
	if(l2->selected && (l2->index != l->index)) {
	  glDeleteLists(l2->object, 1);
	  l2->selected = false;	  
	  if(l2->type == SURFACELIST) {
            for( int j=0; j<mesh->surfaces; j++ ) {
              surface_t *surf=&mesh->surface[j];
              if ( surf->index == l2->index )
                surf->selected=l2->selected;
            }
	    l2->object = generateSurfaceList(l2->index, surfaceColor); // cyan
	  } else if(l2->type == EDGELIST) {
            for( int j=0; j<mesh->edges; j++ ) {
              edge_t *edge=&mesh->edge[j];
              if ( edge->index == l2->index )
                edge->selected=l2->selected;
            }
	    l2->object = generateEdgeList(l2->index, edgeColor); // green
	  }
	}
      }
    }

    // Toggle selection:
    l->selected = !l->selected;
    
    glDeleteLists(l->object, 1);

    // Highlight current selection:
    if(l->type == SURFACELIST) {
      if(l->selected) {
	l->object = generateSurfaceList(l->index, Qt::red); // red
      } else {
	l->object = generateSurfaceList(l->index, surfaceColor); // cyan
      }

      for( int i=0; i<mesh->surfaces; i++ ) {
        surface_t *surf = &mesh->surface[i];
        if ( surf->index == l->index ) surf->selected=l->selected;
      }

    } else if(l->type == EDGELIST) {
      if(l->selected) {
	l->object = generateEdgeList(l->index, Qt::red); // red
      } else {
	l->object = generateEdgeList(l->index, edgeColor); // green
      }
      for( int i=0; i<mesh->edges; i++ ) {
        edge_t *edge = &mesh->edge[i];
        if ( edge->index == l->index ) edge->selected=l->selected;
      }
    }

    // body selection:
    //----------------
    currentlySelectedBody = -1;
    if(shiftPressed || bodyEditActive) {

      // determine the max bulk index
      int MAX_BULK_INDEX = -1;

      for(int i = 0; i < mesh->elements; i++) {
	element_t *elem = &mesh->element[i];
	if(elem->nature != PDE_BULK)
	  break;
	if(elem->index > MAX_BULK_INDEX)
	  MAX_BULK_INDEX = elem->index;
      }

      for(int i = 0; i < mesh->surfaces; i++) {
	surface_t *surf = &mesh->surface[i];
	if(surf->nature != PDE_BULK)
	  break;
	if(surf->index > MAX_BULK_INDEX)
	  MAX_BULK_INDEX = surf->index;
      }

      for(int i = 0; i < mesh->edges; i++) {
	edge_t *edge = &mesh->edge[i];
	if(edge->nature != PDE_BULK)
	  break;
	if(edge->index > MAX_BULK_INDEX)
	  MAX_BULK_INDEX = edge->index;
      }
      
      MAX_BULK_INDEX++;
      if(MAX_BULK_INDEX == 0) {
	cout << "Error in body selection: There are no legal body indiced from which to choose" << endl;
	cout.flush();
	goto body_selection_finished;
      }

      // allocate temp arrays:
      bool *tmp1 = new bool[MAX_BULK_INDEX];
      bool *tmp2 = new bool[MAX_BULK_INDEX];
      for(int i = 0; i < MAX_BULK_INDEX; i++) {
	tmp1[i] = true;
	tmp2[i] = false;
      }
      
      // check if the selected lists uniquely determine a bulk body:
      for(int i = 0; i < lists; i++) {
	list_t *l2 = &list[i];

	if(l2->selected && (l2->nature == PDE_BULK)) {
	  for(int j = 0; j < MAX_BULK_INDEX; j++) {
	    if(j != l2->index)
	      tmp1[j] = false;
	  }
	}
	
	if(l2->selected && (l2->nature == PDE_BOUNDARY) && 
	   (l2->type == SURFACELIST)) {	  
	  for(int j = 0; j < mesh->surfaces; j++) {
	    surface_t *surf = &mesh->surface[j];	    
	    if(surf->index == l2->index) {
	      for(int k = 0; k < surf->elements; k++) {
		int l = surf->element[k];
		if(l < 0) 
		  break;
		element_t *elem = &mesh->element[l];
		if((elem->index < 0) || (elem->index >= MAX_BULK_INDEX))
		  break;
		tmp2[elem->index] = true;
	      }
	      for(int k = 0; k < MAX_BULK_INDEX; k++) {
		tmp1[k] &= tmp2[k];
		tmp2[k] = false;
	      }
	    }
	  }
	}
      }

      // array "tmp1" should contain only one entry with value "true"
      int count = 0;
      int found = -1;
      for(int i = 0; i < MAX_BULK_INDEX; i++) {
	if( tmp1[i] ) {
	  count++;
	  found = i;
	}
      }
      
      if((count == 1) && (found >= 0))
	currentlySelectedBody = found;
      
      delete [] tmp1;
      delete [] tmp2;
    }
  body_selection_finished:
    
    // Emit result to mainwindow:
    emit(signalBoundarySelected(l));

  } else {

    // Emit "nothing selected":
    dummylist.nature = -1;
    dummylist.type = -1;
    dummylist.index = -1;
    emit(signalBoundarySelected(&dummylist));

  }

  updateGL();
}



// Get current matrix and its inverse...
//-----------------------------------------------------------------------------
void GLWidget::getMatrix()
{
  glGetDoublev(GL_MODELVIEW_MATRIX, matrix);
  helpers->invertMatrix(matrix, invmatrix);
}



// Rebuild lists...
//-----------------------------------------------------------------------------
void GLWidget::rebuildLists()
{
  double *bb = meshutils->boundingBox(mesh);
  
  drawTranslate[0] = bb[6]; // x-center
  drawTranslate[1] = bb[7]; // y-center
  drawTranslate[2] = bb[8]; // z-center
  drawScale = bb[9];        // scaling

  delete [] bb;
 
  if(lists) {
    for(int i=0; i < (int)lists; i++) {
      list_t *l = &list[i];
      glDeleteLists(l->object, 1);
    }
    lists = 0;
  }
  
  lists = makeLists();

  updateGL();
}

// Compose GL surface lists...
//-----------------------------------------------------------------------------
void GLWidget::rebuildSurfaceLists()
{
  for( int i=0; i<lists; i++ )
  {
     list_t *l = &list[i];
     if ( l->type == SURFACELIST )
     {
       glDeleteLists( l->object,1 );
       if(l->selected) {
 	 l->object = generateSurfaceList(l->index, Qt::red); // red
       } else {
 	 l->object = generateSurfaceList(l->index, surfaceColor); // cyan
       }
     }
  }
}

// Compose GL edge lists...
//-----------------------------------------------------------------------------
void GLWidget::rebuildEdgeLists()
{
  for( int i=0; i<lists; i++ )
  {
     list_t *l = &list[i];
     if ( l->type == EDGELIST )
     {
       glDeleteLists( l->object,1 );
       if(l->selected) {
 	 l->object = generateEdgeList(l->index, Qt::red); // red
       } else {
 	 l->object = generateEdgeList(l->index, edgeColor); // green
       }
     }
  }
}



// Compose GL object lists...
//-----------------------------------------------------------------------------
GLuint GLWidget::makeLists()
{
  int i;

  if(mesh == NULL) {
    lists = 0;
    return 0;
  }

  // The rule for composing lists to display is the following:
  //---------------------------------------------------------------------------
  // - All surface elements with index >= 0 will be drawn - one list/index
  //   (list->type = SURFACELIST)
  // - For each surface element list, one auxiliary list will be drawn
  //   (list->type = SURFACEMESHLIST)
  // - All edge elements with index >= 0 will be drawn - one list/index
  //   (list->type = EDGELIST)
  // - All point elements with index >= 0 will be drawn - one list/index
  //   (list->type = POINTLIST)
  // - A list of sharp edges will always be drawn (even if it is empty)
  //---------------------------------------------------------------------------
  
  // Simultaneously, construct hash for mapping bosy & boundary incides:
  boundaryMap.clear();
  bodyMap.clear();
  int boundaryCount = 0;
  int bodyCount = 0;

  // Scan volume elements to determine the number of mat. ind. (just for hash):
  //---------------------------------------------------------------------------
  int *element_nature = new int[mesh->elements];

  for(i=0; i < mesh->elements; i++)
    element_nature[i] = 0;
  
  for(i=0; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];
    if(element->index >= 0)  // accept also index 0
      element_nature[element->index] = element->nature;
  }    
  
  for(i=0; i < mesh->elements; i++) {
    if(element_nature[i] == PDE_BULK) {
      bodyMap.insert(i, bodyCount);
      bodyCount++;
    }
  }  

  delete [] element_nature;
  
  // Scan surface elements to determine the number of bcs / mat. indices:
  //---------------------------------------------------------------------------
  int surface_bcs = 0;
  int *surface_nature = new int[mesh->surfaces];
  
  for(i=0; i < mesh->surfaces; i++)
    surface_nature[i] = 0;
  
  for(i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if(surface->index > 0)
      surface_nature[surface->index] = surface->nature;
  }    
  
  for(i=0; i < mesh->surfaces; i++) {
    if(surface_nature[i] > 0) {
      surface_bcs++;
      if(surface_nature[i] == PDE_BULK)
	bodyMap.insert(i, bodyCount++);
      if(surface_nature[i] == PDE_BOUNDARY)
	boundaryMap.insert(i, boundaryCount++);
    }
  }  
  
  cout << "Bcs / materials on surface elements: " << surface_bcs << endl;
  cout.flush();

  // Scan edge elements to determine the number of bcs / mat. indices:
  //---------------------------------------------------------------------------
  int edge_bcs = 0;
  int *edge_nature = new int[mesh->edges];
  
  for(i=0; i < mesh->edges; i++)
    edge_nature[i] = 0;
  
  for(i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if(edge->index > 0)
      edge_nature[edge->index] = edge->nature;
  }    
  
  for(i=0; i < mesh->edges; i++) {
    if(edge_nature[i] > 0) {
      edge_bcs++;
      if(edge_nature[i] == PDE_BULK)
	bodyMap.insert(i, bodyCount++);
      if(edge_nature[i] == PDE_BOUNDARY)
	boundaryMap.insert(i, boundaryCount++);
    }
  }  
  
  cout << "Bcs / materials on edge elements: " << edge_bcs << endl;  
  cout.flush();

  // Scan point elements to determine the number of bcs / mat. indices:
  //---------------------------------------------------------------------------
  int point_bcs = 0;
  int *point_nature = new int[mesh->points];

  // TODO

  cout << "Bcs / materials on point elements: " << point_bcs << endl;  
  cout.flush();

  // Generate lists:
  //---------------------------------------------------------------------------
  lists = 0;
  lists += surface_bcs;   // surface elements
  lists += surface_bcs;   // surface mesh lines (child of surf.elems.)
  lists += edge_bcs;      // edge elements
  lists += point_bcs;     // point elements
  lists += 1;             // sharp edges on surfaces
  lists += 1;             // volume mesh (visual only)
  
  list = new list_t[lists];
  int current_index = 0;

  cout << "Generating " << lists << " lists to display" << endl;
  cout.flush();

  // Surface lists:
  for(i=0; i < mesh->surfaces; i++) {
    mesh->surface[i].selected = false;
    if(surface_nature[i] > 0) {

      // triangles & quads:
      list_t *l = &list[current_index++];
      l->nature = surface_nature[i];
      l->type = SURFACELIST;
      l->index = i;
      l->object = generateSurfaceList(l->index, surfaceColor); // cyan
      l->child = current_index;
      l->parent = -1;
      l->selected = false;
      l->visible = stateDrawSurfaceElements;

      // edges of surface elements (just for visual):
      l = &list[current_index++];
      l->nature = PDE_UNKNOWN;
      l->type = SURFACEMESHLIST;
      l->index = i;
      l->object = generateSurfaceMeshList(l->index, surfaceMeshColor); // black
      l->child = -1;
      l->parent = current_index-2;
      l->selected = false;
      l->visible = stateDrawSurfaceMesh;
    }
  }
  
  // Edge lists (only PDE_BOUNDARY):
  for(i=0; i < mesh->edges; i++) {
    mesh->edge[i].selected = false;
    if(edge_nature[i] == PDE_BOUNDARY) {
      list_t *l = &list[current_index++];
      l->nature = edge_nature[i]; 
      l->type = EDGELIST;
      l->index = i;
      l->object = generateEdgeList(l->index, edgeColor); // green
      l->child = -1;
      l->parent = -1;
      l->selected = false;
      l->visible = stateDrawEdgeElements;
    }
  }

  // Point lists: TODO

  // Sharp edges (just for visual):
  list_t *l = &list[current_index++];
  l->nature = PDE_UNKNOWN;
  l->type = SHARPEDGELIST;
  l->index = -1;
  l->object = generateSharpEdgeList(sharpEdgeColor); // black
  l->child = -1;
  l->parent = -1;
  l->selected = false;
  l->visible = stateDrawSharpEdges;


  // Volume mesh (visual only)
  l = &list[current_index++];
  l->nature = PDE_UNKNOWN;
  l->type = VOLUMEMESHLIST;
  l->index = -1;
  l->object = generateVolumeMeshList(Qt::black); // black
  l->child = -1;
  l->parent = -1;
  l->selected = false;
  l->visible = stateDrawVolumeMesh;


  delete [] surface_nature;
  delete [] edge_nature;
  delete [] point_nature;
  
  updateGL();
  getMatrix();

  return lists;
}


// Generate volume mesh list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateVolumeMeshList(QColor qColor)
{
  double R = qColor.red() / 255.0;
  double G = qColor.green() / 255.0;
  double B = qColor.blue() / 255.0;

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);

  glBegin(GL_LINES);

  for(int i = 1; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];    

    glColor3d(R, G, B);

    // only tetras at the moment:
    if(element->code == 504) {
      node_t *n0 = &mesh->node[ element->node[0] ];
      node_t *n1 = &mesh->node[ element->node[1] ];
      node_t *n2 = &mesh->node[ element->node[2] ];
      node_t *n3 = &mesh->node[ element->node[3] ];

      double x0 = ( n0->x[0] - drawTranslate[0] ) / drawScale;
      double y0 = ( n0->x[1] - drawTranslate[1] ) / drawScale;
      double z0 = ( n0->x[2] - drawTranslate[2] ) / drawScale;

      double x1 = ( n1->x[0] - drawTranslate[0] ) / drawScale;
      double y1 = ( n1->x[1] - drawTranslate[1] ) / drawScale;
      double z1 = ( n1->x[2] - drawTranslate[2] ) / drawScale;

      double x2 = ( n2->x[0] - drawTranslate[0] ) / drawScale;
      double y2 = ( n2->x[1] - drawTranslate[1] ) / drawScale;
      double z2 = ( n2->x[2] - drawTranslate[2] ) / drawScale;

      double x3 = ( n3->x[0] - drawTranslate[0] ) / drawScale;
      double y3 = ( n3->x[1] - drawTranslate[1] ) / drawScale;
      double z3 = ( n3->x[2] - drawTranslate[2] ) / drawScale;

      glVertex3d(x0, y0, z0);
      glVertex3d(x1, y1, z1);

      glVertex3d(x0, y0, z0);
      glVertex3d(x2, y2, z2);

      glVertex3d(x0, y0, z0);
      glVertex3d(x3, y3, z3);

      glVertex3d(x1, y1, z1);
      glVertex3d(x2, y2, z2);

      glVertex3d(x1, y1, z1);
      glVertex3d(x3, y3, z3);

      glVertex3d(x2, y2, z2);
      glVertex3d(x3, y3, z3);
    }
  }

  glEnd();

  glEndList();

  return current;
}



// Generate surface list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateSurfaceList(int index, QColor qColor)
{
  double x0[3], x1[3], x2[3], x3[3];

  double R = qColor.red() / 255.0;
  double G = qColor.green() / 255.0;
  double B = qColor.blue() / 255.0;

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);

  // Draw triangles:
  //-----------------
  glBegin(GL_TRIANGLES);

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index) && ((int)(surface->code/100) == 3)) {

      glColor3d(R, G, B);

      if(stateBcColors && (surface->nature == PDE_BOUNDARY)) {
	glColor3d(0.5 + 0.5 * sin(1 * index),
		  0.5 + 0.5 * cos(2 * index),
		  0.5 + 0.5 * cos(3 * index));
      } 
	
      if(stateBodyColors) {
	int bodyIndex = surface->index;
	if(surface->nature == PDE_BOUNDARY) {
	  int parentIndex = surface->element[0];
	  element_t *parent = &mesh->element[parentIndex];
	  bodyIndex = parent->index;
	}
	glColor3d(0.5 + 0.5 * sin(1 * bodyIndex),
		  0.5 + 0.5 * cos(2 * bodyIndex),
		  0.5 + 0.5 * cos(3 * bodyIndex));
      } 
	
      glNormal3dv(surface->normal); 
      
      int n0 = surface->node[0];
      int n1 = surface->node[1];
      int n2 = surface->node[2];
      
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
      
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
      
      x2[0] = (mesh->node[n2].x[0] - drawTranslate[0]) / drawScale;
      x2[1] = (mesh->node[n2].x[1] - drawTranslate[1]) / drawScale;
      x2[2] = (mesh->node[n2].x[2] - drawTranslate[2]) / drawScale;
      
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[0]); 
      glVertex3dv(x0);
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[1]); 
      glVertex3dv(x1);
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[2]); 
      glVertex3dv(x2);
    }
  }

  glEnd();

  // Draw quads:
  //------------
  glBegin(GL_QUADS);
  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index) && ((int)(surface->code/100) == 4)) {

      glColor3d(R, G, B);

      if(stateBcColors && (surface->nature == PDE_BOUNDARY)) {
	glColor3d(0.5 + 0.5 * sin(1 * index),
		  0.5 + 0.5 * cos(2 * index),
		  0.5 + 0.5 * cos(3 * index));
      }

      if(stateBodyColors) {
	int bodyIndex = surface->index;
	if(surface->nature == PDE_BOUNDARY) {
	  int parentIndex = surface->element[0];
	  element_t *parent = &mesh->element[parentIndex];
	  bodyIndex = parent->index;
	}
	glColor3d(0.5 + 0.5 * sin(1 * bodyIndex),
		  0.5 + 0.5 * cos(2 * bodyIndex),
		  0.5 + 0.5 * cos(3 * bodyIndex));
      } 

      glNormal3dv(surface->normal); 
      
      int n0 = surface->node[0];
      int n1 = surface->node[1];
      int n2 = surface->node[2];
      int n3 = surface->node[3];
      
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
      
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
      
      x2[0] = (mesh->node[n2].x[0] - drawTranslate[0]) / drawScale;
      x2[1] = (mesh->node[n2].x[1] - drawTranslate[1]) / drawScale;
      x2[2] = (mesh->node[n2].x[2] - drawTranslate[2]) / drawScale;
      
      x3[0] = (mesh->node[n3].x[0] - drawTranslate[0]) / drawScale;
      x3[1] = (mesh->node[n3].x[1] - drawTranslate[1]) / drawScale;
      x3[2] = (mesh->node[n3].x[2] - drawTranslate[2]) / drawScale;
      
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[0]); 
      glVertex3dv(x0);
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[1]); 
      glVertex3dv(x1);
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[2]); 
      glVertex3dv(x2);
      if ( !stateFlatShade ) glNormal3dv(surface->vertex_normals[3]); 
      glVertex3dv(x3);      
    }
  }


  glEnd();

  glEndList();

#if 0

  // Draw indexes:
  //------------
  
  cout << "draw indexes" << endl; 

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index)) {
      int nodes = surface->code/100;

      x0[0] = x0[1] = x0[2] = 0.0;
      for(int j=0;j<nodes;j++) {
	int ind = surface->node[j];
	x0[0] = x0[0] + mesh->node[ind].x[0];
	x0[1] = x0[1] + mesh->node[ind].x[1];
	x0[2] = x0[2] + mesh->node[ind].x[2];
      }
      x1[0] = (x0[0]/nodes - drawTranslate[0]) / drawScale;
      x1[1] = (x0[1]/nodes - drawTranslate[1]) / drawScale;
      x1[2] = (x0[2]/nodes - drawTranslate[2]) / drawScale;

      glColor3d(1, 1, 1);
      renderText(x1[0], x1[1], x1[2], "c");
    }
    renderText(0.0, 0.0, 0.0, "A");
  }
  
#endif

  return current;
}





// Generate surface edge list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateSurfaceMeshList(int index, QColor qColor)
{
  double x0[3], x1[3], x2[3], x3[3];

  double R = qColor.red() / 255.0;
  double G = qColor.green() / 255.0;
  double B = qColor.blue() / 255.0;

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);
 
  // Draw lines:
  //------------
  glLineWidth(1.0);
  glDisable(GL_LIGHTING);
  glColor3d(R, G, B);
  glBegin(GL_LINES);
  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    
    if((surface->index == index) && ((int)(surface->code/100) == 3)) {
      int n0 = surface->node[0];
      int n1 = surface->node[1];
      int n2 = surface->node[2];
      
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
      
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
      
      x2[0] = (mesh->node[n2].x[0] - drawTranslate[0]) / drawScale;
      x2[1] = (mesh->node[n2].x[1] - drawTranslate[1]) / drawScale;
      x2[2] = (mesh->node[n2].x[2] - drawTranslate[2]) / drawScale;
      
      glVertex3dv(x0);
      glVertex3dv(x1);

      glVertex3dv(x1);
      glVertex3dv(x2);

      glVertex3dv(x2);
      glVertex3dv(x0);
    }
  }
  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index) && ((int)(surface->code/100) == 4)) {
      int n0 = surface->node[0];
      int n1 = surface->node[1];
      int n2 = surface->node[2];
      int n3 = surface->node[3];
      
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
      
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
      
      x2[0] = (mesh->node[n2].x[0] - drawTranslate[0]) / drawScale;
      x2[1] = (mesh->node[n2].x[1] - drawTranslate[1]) / drawScale;
      x2[2] = (mesh->node[n2].x[2] - drawTranslate[2]) / drawScale;
      
      x3[0] = (mesh->node[n3].x[0] - drawTranslate[0]) / drawScale;
      x3[1] = (mesh->node[n3].x[1] - drawTranslate[1]) / drawScale;
      x3[2] = (mesh->node[n3].x[2] - drawTranslate[2]) / drawScale;
      
      glVertex3dv(x0);
      glVertex3dv(x1);

      glVertex3dv(x1);
      glVertex3dv(x2);

      glVertex3dv(x2);
      glVertex3dv(x3);

      glVertex3dv(x3);
      glVertex3dv(x0);      
    }
  }
  glEnd();

  glEnable(GL_LIGHTING);
  glEndList();
  
  return current;
}


// Generate edge list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateEdgeList(int index, QColor qColor)
{
  double x0[3], x1[3];

  double R = qColor.red() / 255.0;
  double G = qColor.green() / 255.0;
  double B = qColor.blue() / 255.0;

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);
  glColor3d(R, G, B);  
  glLineWidth(4.0);
  glDisable(GL_LIGHTING);
  glBegin(GL_LINES);

  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];

    if(edge->index == index) {
      int n0 = edge->node[0];
      int n1 = edge->node[1];
	
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
	
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
	
      glVertex3dv(x0);
      glVertex3dv(x1);
    }
  }
  
  glEnd();
  glEnable(GL_LIGHTING);  
  glEndList();
  
  return current;
}



// Generate sharp edge list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateSharpEdgeList(QColor qColor)
{
  double x0[3], x1[3];

  double R = qColor.red() / 255.0;
  double G = qColor.green() / 255.0;
  double B = qColor.blue() / 255.0;

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);

  glColor3d(R, G, B);  
  glLineWidth(1.0);
  glDisable(GL_LIGHTING);
  glBegin(GL_LINES);
  
  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];

    if(edge->sharp_edge) {
      int n0 = edge->node[0];
      int n1 = edge->node[1];
	
      x0[0] = (mesh->node[n0].x[0] - drawTranslate[0]) / drawScale;
      x0[1] = (mesh->node[n0].x[1] - drawTranslate[1]) / drawScale;
      x0[2] = (mesh->node[n0].x[2] - drawTranslate[2]) / drawScale;
	
      x1[0] = (mesh->node[n1].x[0] - drawTranslate[0]) / drawScale;
      x1[1] = (mesh->node[n1].x[1] - drawTranslate[1]) / drawScale;
      x1[2] = (mesh->node[n1].x[2] - drawTranslate[2]) / drawScale;
	
      glVertex3dv(x0);
      glVertex3dv(x1);
    }
  }
  
  glEnd();
  glEnable(GL_LIGHTING);  
  glEndList();
  
  return current;
}


// Draw coordinates:
//-----------------------------------------------------------------------------
void GLWidget::drawCoordinates()
{
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();

  glTranslated(-0.8, -0.8, 5.0);

  glMatrixMode(GL_MODELVIEW);

  // z-axis
  glColor3d(0, 0, 1);
  gluCylinder(quadratic, 0.02, 0.0, 0.2, 8, 8);  
  renderText(0.0, 0.0, 0.25, "Z");

  // x-axis
  glColor3d(1, 0, 0);
  glRotated(90, 0, 1, 0);
  gluCylinder(quadratic, 0.02, 0.0, 0.2, 8, 8);  
  renderText(0.0, 0.0, 0.25, "X");
  glRotated(-90, 0, 1, 0);

  // y-axis
  glColor3d(0, 1, 0);
  glRotated(-90, 1, 0, 0);
  gluCylinder(quadratic, 0.02, 0.0, 0.2, 8, 8);  
  renderText(0.0, 0.0, 0.25, "Y");
  glRotated(90, 1, 0, 0);
  
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

  glMatrixMode(GL_MODELVIEW);

  return;
}
