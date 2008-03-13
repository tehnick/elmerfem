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
  backgroundColor = QColor::fromRgb(255, 255, 255, 255);
  lists = 0;
  drawScale = 1.0;
  drawTranslate[0] = 0.0;
  drawTranslate[1] = 0.0;
  drawTranslate[2] = 0.0;
  mesh = NULL;

  helpers = new Helpers;
  meshutils = new Meshutils;
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
  static GLfloat light_position[] = {1.0,-2.0, 2.0, 0.0};

  static GLfloat mat_ambient[]    = {0.2, 0.2, 0.2, 1.0};
  static GLfloat mat_diffuse[]    = {0.6, 0.6, 0.6, 1.0};
  static GLfloat mat_specular[]   = {0.9, 0.9, 0.9, 1.0};
  static GLfloat high_shininess[] = {100.0};

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 1);
  glEnable(GL_LIGHTING);

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
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  if(lists) {
    for(int i=0; i<(int)lists; i++) {
      list_t *l = &list[i];

      if(l->visible) {
	glPushName(i);
	
	if(l->type == SURFACEEDGELIST) {
	  
	  // translate slightly towards viewer
	  glMatrixMode(GL_PROJECTION);
	  glPushMatrix();
	  glTranslated(0, 0, 0.01);
	  glCallList(l->object); 
	  glPopMatrix();
	  glMatrixMode(GL_MODELVIEW);

	} else {

	  glCallList(l->object); 

	}

	glPopName();
      }
    }
  }
}


// Resize window...
//-----------------------------------------------------------------------------
void GLWidget::resizeGL(int width, int height)
{
  double _top = 1.0;
  double _bottom = -1.0;
  double _left = -(double)width / (double)height;
  double _right = (double)width / (double)height;
  double _near = -10.0;
  double _far = 10.0;

  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(_left, _right, _top, _bottom, _near, _far);
  glMatrixMode(GL_MODELVIEW);
}



// Key pressed...
//-----------------------------------------------------------------------------
void GLWidget::keyPressEvent(QKeyEvent *event)
{
  ctrlPressed = true;
}


// Key released...
//-----------------------------------------------------------------------------
void GLWidget::keyReleaseEvent(QKeyEvent *event)
{
  ctrlPressed = false;
}



// Mouse button clicked...
//-----------------------------------------------------------------------------
void GLWidget::mousePressEvent(QMouseEvent *event)
{
  lastPos = event->pos();
  setFocus();
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
  
  if (event->buttons() & Qt::LeftButton) {
    
    // Rotation:
    double ax = -(double)dy;
    double ay = (double)dx;
    double az = 0.0;
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

  } else if (event->buttons() & Qt::MidButton) {

    // Scale:
    double s = exp(dy*0.01);
    glScaled(s, s, s);
    updateGL();
  }

  lastPos = event->pos();
  getMatrix();
}



// Mouse button double clicked...
//-----------------------------------------------------------------------------
void GLWidget::mouseDoubleClickEvent(QMouseEvent *event)
{
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
  
  GLdouble delX = 3.0;
  GLdouble delY = 3.0;
  
  gluPickMatrix(x, y, delX, delY, viewport);
  glMultMatrixd(projection);
  
  glMatrixMode(GL_MODELVIEW);
  updateGL();
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

  // Highlight the selected boundary:
  if(nearest != 0xffffffff) {
    list_t *l = &list[nearest];

    // substitute surfaceedgelists with the parent surfacelist:
    if(l->type == SURFACEEDGELIST)
      l = &list[l->parent];

    // if not ctrl pressed, clear all except this one:
    if(!ctrlPressed) {
      for(i=0; i < lists; i++) {
	list_t *l2 = &list[i];
	if(l2->selected && (l2->index != l->index)) {
	  glDeleteLists(l2->object, 1);
	  l2->selected = false;	  
	  if(l2->type == SURFACELIST) {
	    l2->object = generateSurfaceList(l2->index, 0, 1, 1); // cyan
	  } else if(l->type == EDGELIST) {
	    l2->object = generateEdgeList(l2->index, 0, 1, 0); // green
	  }
	}
      }
    }

    // Emit result to mainwindow:
    emit(signalBoundarySelected(l));
    
    // Toggle selection:
    l->selected = !l->selected;
    
    glDeleteLists(l->object, 1);

    // Highlight current selection:
    if(l->type == SURFACELIST) {
      if(l->selected) {
	l->object = generateSurfaceList(l->index, 1, 0, 0); // red
      } else {
	l->object = generateSurfaceList(l->index, 0, 1, 1); // cyan
      }

    } else if(l->type == EDGELIST) {
      if(l->selected) {
	l->object = generateEdgeList(l->index, 1, 0, 0); // red
      } else {
	l->object = generateEdgeList(l->index, 0, 1, 0); // green
      }
    }
    
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



// Rebuild boundary lists...
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
  //   (list->type = SURFACEEDGELIST)
  // - All edge elements with index >= 0 will be drawn - one list/index
  //   (list->type = EDGELIST)
  // - All point elements with index >= 0 will be drawn - one list/index
  //   (list->type = POINTLIST)
  //---------------------------------------------------------------------------
  

  // Scan surface elements to determine the number of bcs:
  //---------------------------------------------------------------------------
  int surface_bcs = 0;
  int *surface_tmp = new int[mesh->surfaces];
  
  for(i=0; i < mesh->surfaces; i++)
    surface_tmp[i] = 0;
  
  for(i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if(surface->index > 0)
      surface_tmp[surface->index]++;
  }    
  
  for(i=0; i < mesh->surfaces; i++) {
    if(surface_tmp[i] > 0)
      surface_bcs++;
  }  
  
  cout << "Bcs / materials on surface elements: " << surface_bcs << endl;

  // Scan edge elements to determine the number of bcs:
  //---------------------------------------------------------------------------
  int edge_bcs = 0;
  int *edge_tmp = new int[mesh->edges];
  
  for(i=0; i < mesh->edges; i++)
    edge_tmp[i] = 0;
  
  for(i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if(edge->index > 0)
      edge_tmp[edge->index]++;
  }    
  
  for(i=0; i < mesh->edges; i++) {
    if(edge_tmp[i] > 0)
      edge_bcs++;
  }  
  
  cout << "Bcs / materials on edge elements: " << edge_bcs << endl;  

  // Scan edge elements to determine the number of bcs:
  //---------------------------------------------------------------------------
  int point_bcs = 0;
  int *point_tmp = new int[mesh->points];

  // TODO

  cout << "Bcs / materials on point elements: " << point_bcs << endl;  

  // Generate lists:
  //---------------------------------------------------------------------------
  lists = 2*surface_bcs + edge_bcs + point_bcs;
  list = new list_t[lists];
  int current_index = 0;

  cout << "Generating " << lists << " lists to display" << endl;
  cout.flush();

  // Surface lists:
  for(i=0; i < mesh->surfaces; i++) {
    if(surface_tmp[i] > 0) {

      // triangles & quads:
      list_t *l = &list[current_index++];
      l->nature = PDE_BOUNDARY;   // fix this as this is not always the case
      l->type = SURFACELIST;
      l->index = i;
      l->object = generateSurfaceList(l->index, 0, 1, 1);
      l->child = current_index;
      l->parent = -1;
      l->selected = false;
      l->visible = true;

      // edges of surface elements (just for visual outlook):
      l = &list[current_index++];
      l->nature = PDE_UNKNOWN;
      l->type = SURFACEEDGELIST;
      l->index = i;
      l->object = generateSurfaceEdgeList(l->index, 0, 0, 0);
      l->child = -1;
      l->parent = current_index-2;
      l->selected = false;
      l->visible = true;
    }
  }
  
  // Edge lists:
  for(i=0; i < mesh->edges; i++) {
    if(edge_tmp[i] > 0) {
      list_t *l = &list[current_index++];
      l->nature = PDE_BOUNDARY;   // fix this as this is not always the case
      l->type = EDGELIST;
      l->index = i;
      l->object = generateEdgeList(l->index, 0, 1, 0);
      l->child = -1;
      l->parent = -1;
      l->selected = false;
      l->visible = true;
    }
  }

  delete [] surface_tmp;
  delete [] edge_tmp;
  delete [] point_tmp;
  
  updateGL();
  getMatrix();

  return lists;
}




// Generate surface list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateSurfaceList(int index, double R, double G, double B)
{
  double x0[3], x1[3], x2[3], x3[3];

  GLuint current = glGenLists(1);
  glNewList(current, GL_COMPILE);

  // Draw triangles:
  //-----------------
  glBegin(GL_TRIANGLES);
  glColor3d(R, G, B);      

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index) && (surface->code == 303)) {
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
      
      glVertex3dv(x0);
      glVertex3dv(x1);
      glVertex3dv(x2);
    }
  }

  glEnd();

  // Draw quads:
  //------------
  glBegin(GL_QUADS);
  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    if((surface->index == index) && (surface->code == 404)) {
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
      
      glVertex3dv(x0);
      glVertex3dv(x1);
      glVertex3dv(x2);
      glVertex3dv(x3);      
    }
  }

  glEnd();

  glEndList();
  
  return current;
}





// Generate surface edge list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateSurfaceEdgeList(int index, double R, double G, double B)
{
  double x0[3], x1[3], x2[3], x3[3];

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
    
    if((surface->index == index) && (surface->code == 303)) {
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

    if((surface->index == index) && (surface->code == 404)) {      
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
GLuint GLWidget::generateEdgeList(int index, double R, double G, double B)
{
  double x0[3], x1[3];

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
