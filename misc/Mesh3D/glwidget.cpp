#include <QtGui>
#include <QtOpenGL>
#include <QWheelEvent>
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
  objects = 0;
  firstList = 0;
  lastList = 0;
  colorMapEntries = 0;
  drawScale = 1.0;
  drawTranslate[0] = 0.0;
  drawTranslate[1] = 0.0;
  drawTranslate[2] = 0.0;
  mesh = NULL;
}



// dtor...
//-----------------------------------------------------------------------------
GLWidget::~GLWidget()
{
  makeCurrent();
  glDeleteLists(firstList, objects);
  delete [] colorMap;
  clearMesh();
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
  static GLfloat light_ambient[]  = {0.0, 0.0, 0.0, 1.0};
  static GLfloat light_diffuse[]  = {1.0, 1.0, 1.0, 1.0};
  static GLfloat light_specular[] = {1.0, 1.0, 1.0, 1.0};
  static GLfloat light_position[] = {0.0, 0.0, 5.0, 1.0};
  
  static GLfloat mat_ambient[]    = {0.7, 0.7, 0.7, 1.0};
  static GLfloat mat_diffuse[]    = {0.8, 0.8, 0.8, 1.0};
  static GLfloat mat_specular[]   = {1.0, 1.0, 1.0, 1.0};
  static GLfloat high_shininess[] = {100.0};
  
  glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);
  //glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,1);
  
  glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
  glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
  glMaterialfv(GL_FRONT, GL_SHININESS, high_shininess);
  
  // glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glDepthFunc(GL_LESS);
  glEnable(GL_DEPTH_TEST);
  //glEnable(GL_NORMALIZE);
  glEnable(GL_COLOR_MATERIAL);
  //glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_LINE_SMOOTH);

  // Set up colormap:
  glEnable(GL_TEXTURE_1D);
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, &colorMapEntries);

  cout << "Initialize GL" << endl;
  cout << "Vendor: " << glGetString(GL_VENDOR) << endl;
  cout << "Renderer: " << glGetString(GL_RENDERER) << endl;
  cout << "GL Version: " << glGetString(GL_VERSION) << endl;
  cout << "Colormap entries: " << colorMapEntries << endl;
  cout.flush();

  // delete  [] colorMap;
  colorMap = new unsigned char[3*colorMapEntries];
  clearColorMap();


  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB, colorMapEntries,
	       0, GL_RGB, GL_UNSIGNED_BYTE, colorMap);

  glBindTexture(GL_TEXTURE_1D, 0);

  qglClearColor(backgroundColor);
}



// Make virgin colormap...
//-----------------------------------------------------------------------------
void GLWidget::clearColorMap()
{
  unsigned char *cm = colorMap;

  // First n-1 entries are cyan
  for(int i=0; i<colorMapEntries-1; i++) {
    *cm++ = 0;
    *cm++ = 255;
    *cm++ = 255;
  }

  // Last is black
  *cm++ = 0;
  *cm++ = 0;
  *cm++ = 0;
}


// Paint event...
//-----------------------------------------------------------------------------
void GLWidget::paintGL()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  if(objects) {
    for(GLuint i=firstList; i<=lastList; i++) {
      glPushName(i);
      glCallList(i); 
      glPopName();
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



// Mouse button clicked...
//-----------------------------------------------------------------------------
void GLWidget::mousePressEvent(QMouseEvent *event)
{
  lastPos = event->pos();
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
  static GLuint buffer[1024];
  const int bufferSize = sizeof(buffer)/sizeof(GLuint);
  
  GLint viewport[4];
  GLdouble projection[16];

  GLint hits;
  GLint i, j;
  
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
  if(hits!=0) {
    for (i=0,j=0; i<hits; i++) {
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

  // Highlight the selected boundary by redefining the colormap:
  clearColorMap();

  if(nearest != 0xffffffff) {

    // Emit to MainWidow:
    emit(selectedBoundary(nearest)); 

    colorMap[3*nearest] = 255;
    colorMap[3*nearest+1] = 0;
    colorMap[3*nearest+2] = 0;
  } else {

    // Nothing selected:
    emit(selectedBoundary(-1)); 
  }

  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB, colorMapEntries,
	       0, GL_RGB, GL_UNSIGNED_BYTE, colorMap);

  glBindTexture(GL_TEXTURE_1D, 0);
  
  updateGL();
}



// Get current matrix and its inverse...
//-----------------------------------------------------------------------------
void GLWidget::getMatrix()
{
  glGetDoublev(GL_MODELVIEW_MATRIX, matrix);
  helpers.invertMatrix(matrix, invmatrix);
}



// Compose GL object lists...
//-----------------------------------------------------------------------------
GLuint GLWidget::makeObjects()
{
  int i, j, boundaryconditions;
  double x0[3], x1[3], x2[3];
  boundaryelement_t *boundaryelement;

  // First, scan boundary elements to determine the biggest index:
  boundaryconditions = 0;
  for(i=0; i < mesh->boundaryelements; i++) {
    boundaryelement = &mesh->boundaryelement[i];

    if(boundaryelement->index > boundaryconditions)
      boundaryconditions = boundaryelement->index;
  }

  boundaryconditions++;

  firstList = 0;
  lastList = 0;

  for(j=1; j<boundaryconditions; j++) {
    GLuint list = glGenLists(1);
    glNewList(list, GL_COMPILE);

    //cout << "Generating list: " << list << "\n";
    //cout.flush();

    if(j==1)
      firstList = list;

    if(j==(boundaryconditions-1))
      lastList = list;
    
    glBegin(GL_TRIANGLES);

    // Triangles:
    for(i=0; i < mesh->boundaryelements; i++) {
      boundaryelement = &mesh->boundaryelement[i];

      if(boundaryelement->index == j) {
	
	double colorValue = (double)(j)/(double)(colorMapEntries);
	glTexCoord1d(colorValue);

	glNormal3dv(boundaryelement->normal); 
	
	int vertex0 = boundaryelement->vertex[0];
	int vertex1 = boundaryelement->vertex[1];
	int vertex2 = boundaryelement->vertex[2];
	
	x0[0] = (mesh->node[vertex0].x[0] - drawTranslate[0]) / drawScale;
	x0[1] = (mesh->node[vertex0].x[1] - drawTranslate[1]) / drawScale;
	x0[2] = (mesh->node[vertex0].x[2] - drawTranslate[2]) / drawScale;

	x1[0] = (mesh->node[vertex1].x[0] - drawTranslate[0]) / drawScale;
	x1[1] = (mesh->node[vertex1].x[1] - drawTranslate[1]) / drawScale;
	x1[2] = (mesh->node[vertex1].x[2] - drawTranslate[2]) / drawScale;

	x2[0] = (mesh->node[vertex2].x[0] - drawTranslate[0]) / drawScale;
	x2[1] = (mesh->node[vertex2].x[1] - drawTranslate[1]) / drawScale;
	x2[2] = (mesh->node[vertex2].x[2] - drawTranslate[2]) / drawScale;

	glVertex3dv(x0);
	glVertex3dv(x1);
	glVertex3dv(x2);
      }
    }
    glEnd();
    
    glBegin(GL_LINES);
    glLineWidth(1.0);
    
    // Lines:
    for(i=0; i < mesh->boundaryelements; i++) {
      boundaryelement = &mesh->boundaryelement[i];

      if(boundaryelement->index == j) {

	glTexCoord1d(1.0); // last = 1.0 = black

	glNormal3dv(boundaryelement->normal); 

	int vertex0 = boundaryelement->vertex[0];
	int vertex1 = boundaryelement->vertex[1];
	int vertex2 = boundaryelement->vertex[2];

	x0[0] = (mesh->node[vertex0].x[0] - drawTranslate[0]) / drawScale;
	x0[1] = (mesh->node[vertex0].x[1] - drawTranslate[1]) / drawScale;
	x0[2] = (mesh->node[vertex0].x[2] - drawTranslate[2]) / drawScale;
	
	x1[0] = (mesh->node[vertex1].x[0] - drawTranslate[0]) / drawScale;
	x1[1] = (mesh->node[vertex1].x[1] - drawTranslate[1]) / drawScale;
	x1[2] = (mesh->node[vertex1].x[2] - drawTranslate[2]) / drawScale;
	
	x2[0] = (mesh->node[vertex2].x[0] - drawTranslate[0]) / drawScale;
	x2[1] = (mesh->node[vertex2].x[1] - drawTranslate[1]) / drawScale;
	x2[2] = (mesh->node[vertex2].x[2] - drawTranslate[2]) / drawScale;
	
	glVertex3dv(x0);
	glVertex3dv(x1);
	
	glVertex3dv(x1);
	glVertex3dv(x2);
	
	glVertex3dv(x2);
	glVertex3dv(x0);
      }
    }
    
    glEnd();
  
    glEndList();

  }

  return lastList - firstList + 1;
}



// Delete mesh...
//-----------------------------------------------------------------------------
void GLWidget::clearMesh()
{
  if(mesh != (mesh_t*)NULL) {
    if(mesh->element != (element_t*)NULL) 
      delete [] mesh->element;
    
    if(mesh->boundaryelement != (boundaryelement_t*)NULL) 
      delete [] mesh->boundaryelement;
    
    if(mesh->edge != (edge_t*)NULL) 
      delete [] mesh->edge;
    
    if(mesh->node != (node_t*)NULL)
      delete [] mesh->node;
    
    delete [] mesh;
  }
}

// Find edges for boundary elements...
//-----------------------------------------------------------------------------
void GLWidget::findBoundaryElementEdges(mesh_t *mesh)
{
#define PARENT_UNKNOWN -1
#define PARENT_MORETHANTWO -2

  typedef struct {
    int node;
    int parent[2];
    void *next;
  } hash_t;

  int keys = mesh->nodes;

  hash_t *hash = new hash_t[keys];

  for(int i=0; i<keys; i++) {
    hash[i].node = -1;
    hash[i].parent[0] = PARENT_UNKNOWN;
    hash[i].parent[1] = PARENT_UNKNOWN;
    hash[i].next = NULL;
  }

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *be = &mesh->boundaryelement[i];

    int v0 = be->vertex[0];
    int v1 = be->vertex[1];
    int v2 = be->vertex[2];
    
    // edge 0-1
    int m = (v0<v1) ? v0 : v1;
    int n = (v0<v1) ? v1 : v0;
    
    hash_t *h = &hash[m];
    bool found = false;

    while(h->node > -1) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }

    if(!found) {
      h->node = n;
      h->next = new hash_t;
      h = (hash_t*)h->next;
      h->node = -1;
      h->next = NULL;
    }

    // edge 1-2
    m = (v1<v2) ? v1 : v2;
    n = (v1<v2) ? v2 : v1;

    h = &hash[m];
    found = false;

    while(h->node > -1) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }

    if(!found) {
      h->node = n;
      h->next = new hash_t;
      h = (hash_t*)h->next;
      h->node = -1;
      h->next = NULL;
    }

    // edge 2-0
    m = (v2<v0) ? v2 : v0;
    n = (v2<v0) ? v0 : v2;

    h = &hash[m];
    found = false;

    while(h->node > -1) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }

    if(!found) {
      h->node = n;
      h->next = new hash_t;
      h = (hash_t*)h->next;
      h->node = -1;
      h->next = NULL;
    }
  }


  // count edges:
  int edges = 0;
  for(int i=0; i<keys; i++) {
    hash_t *h = &hash[i];
    while(h->node > -1) {
      edges++;
      h = (hash_t*)h->next;
    }
  }

  cout << "Found " << edges << " edges" << endl;

  mesh->edges = edges;
  mesh->edge = new edge_t[edges];

  edges = 0;
  for(int i=0; i<keys; i++) {
    hash_t *h = &hash[i];
    while(h->node > -1) {
      mesh->edge[edges].vertex[0] = i;
      mesh->edge[edges].vertex[1] = h->node;
      edges++;
      h = (hash_t*)h->next;
    }
  }

  // is this sufficient?
  delete [] hash;
}
