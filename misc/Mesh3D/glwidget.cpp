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
  objects = 0;
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
  for(int i=0; i<(int)objects; i++)
    glDeleteLists(glListMap[i], 1);
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
  cout << "GL Version: " << glGetString(GL_VERSION) << endl;
  cout.flush();

  static GLfloat light_ambient[]  = {0.2, 0.2, 0.2, 1.0};
  static GLfloat light_diffuse[]  = {0.8, 0.8, 0.8, 1.0};
  static GLfloat light_specular[] = {1.0, 1.0, 1.0, 1.0};
  static GLfloat light_position[] = {1.0,-2.0, 2.0, 0.0};

  static GLfloat mat_ambient[]    = {0.2, 0.2, 0.2, 1.0};
  static GLfloat mat_diffuse[]    = {0.8, 0.8, 0.8, 1.0};
  static GLfloat mat_specular[]   = {0.8, 0.8, 0.8, 1.0};
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
  glEnable(GL_LINE_SMOOTH);

  glEnable(GL_NORMALIZE);

  qglClearColor(backgroundColor);
}



// Paint event...
//-----------------------------------------------------------------------------
void GLWidget::paintGL()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  if(objects) {
    for(int i=0; i<(int)objects; i++) {
      if(glActiveList[i]) {
	glPushName(i);
	glCallList(glListMap[i]); 
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

  // Clear the previous selections:
  if(!ctrlPressed) {
    for(int i=0; i<sizeofGlMaps; i++) {
      if(glSelected[i]) {
	glDeleteLists(glListMap[i], 1);
	glSelected[i] = false;
	GLuint current = generateBoundaryList(glBcMap[i],0,1,1);
	glListMap[i] = current;
      }
    }
  }

  // Highlight the selected boundary:
  if(nearest != 0xffffffff) {

    // Emit result to mainwindow:
    emit(signalBoundarySelected(glBcMap[nearest])); 

    // Highlight current selection:
    glDeleteLists(glListMap[nearest], 1);
    glSelected[nearest] = true;
    GLuint current = generateBoundaryList(glBcMap[nearest],1,0,0);
    glListMap[nearest] = current;

  } else {

    // Emit "nothing selected":
    emit(signalBoundarySelected(-1)); 
  }

  updateGL();
}



// Get current matrix and its inverse...
//-----------------------------------------------------------------------------
void GLWidget::getMatrix()
{
  glGetDoublev(GL_MODELVIEW_MATRIX, matrix);
  helpers.invertMatrix(matrix, invmatrix);
}


// Rebuild boundary lists...
//-----------------------------------------------------------------------------
void GLWidget::rebuildBoundaryLists()
{
  double *bb = meshutils.boundingBox(mesh);
  
  drawTranslate[0] = bb[6]; // x-center
  drawTranslate[1] = bb[7]; // y-center
  drawTranslate[2] = bb[8]; // z-center
  drawScale = bb[9];         // scaling

  delete [] bb;
 
  if(objects) {
    for(int i=0; i < (int)objects; i++)
      glDeleteLists(glListMap[i], 1);
    objects = 0;
  }
  
  objects = makeObjects();

  updateGL();
}



// Compose GL object lists...
//-----------------------------------------------------------------------------
GLuint GLWidget::makeObjects()
{
  int i;
  boundaryelement_t *boundaryelement;

  if(mesh == NULL) {
    objects = 0;
    return 0;
  }


  // First, scan boundary elements to determine the number of bcs:
  int *bctable = new int[mesh->boundaryelements];
  for(i=0; i < mesh->boundaryelements; i++)
    bctable[i] = 0;

  for(i=0; i < mesh->boundaryelements; i++) {
    boundaryelement = &mesh->boundaryelement[i];
    if(boundaryelement->index > 0)
      bctable[boundaryelement->index]++;
  }    
  
  int bcs = 0;
  for(i=0; i < mesh->boundaryelements; i++) {
    if(bctable[i] > 0)
      bcs++;
  }  

  cout << "Boundary parts: " << bcs << endl;
  
  sizeofGlMaps = bcs;
  glBcMap = new int[bcs];
  glListMap = new GLuint[bcs];
  glSelected = new bool[bcs];
  glActiveList = new bool[bcs];

  bcs = 0;
  for(i=0; i < mesh->boundaryelements; i++) {
    if(bctable[i] > 0) {
      glBcMap[bcs] = i;
      glListMap[bcs] = 0;
      glSelected[bcs] = false;
      glActiveList[bcs] = true;
      bcs++;
    }
  }
  
  for(i=0; i<bcs; i++) {
    GLuint currentList = generateBoundaryList(glBcMap[i],0,1,1);
    glListMap[i] = currentList;
  }

  
  delete [] bctable; 

  updateGL();
  getMatrix();

  return bcs;
}

// Generate boundary list...
//-----------------------------------------------------------------------------
GLuint GLWidget::generateBoundaryList(int index, double R, double G, double B)
{
  int i;
  double x0[3], x1[3], x2[3], x3[3];
  boundaryelement_t *boundaryelement;

  GLuint list = glGenLists(1);
  glNewList(list, GL_COMPILE);

  for(i=0; i < mesh->boundaryelements; i++) {
    boundaryelement = &mesh->boundaryelement[i];

    if(boundaryelement->index == index && boundaryelement->code == 303) {
      
      glNormal3dv(boundaryelement->normal); 

      if(boundaryelement->code == 303) {
	glBegin(GL_TRIANGLES);
	
	glColor3d(R,G,B);

	int n0 = boundaryelement->node[0];
	int n1 = boundaryelement->node[1];
	int n2 = boundaryelement->node[2];
	
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

	glEnd();

	glBegin(GL_LINES);
	glLineWidth(1.0);
	glColor3d(0,0,0);

	glVertex3dv(x0);
	glVertex3dv(x1);

	glVertex3dv(x1);
	glVertex3dv(x2);

	glVertex3dv(x2);
	glVertex3dv(x0);
	
	glEnd();
      }

      if(boundaryelement->code == 404) {
	glBegin(GL_QUADS);
	
	glColor3d(R,G,B);
	
	int n0 = boundaryelement->node[0];
	int n1 = boundaryelement->node[1];
	int n2 = boundaryelement->node[2];
	int n3 = boundaryelement->node[3];
	
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

	glEnd();

	glBegin(GL_LINES);
	glLineWidth(1.0);
	glColor3d(0,0,0);

	glVertex3dv(x0);
	glVertex3dv(x1);

	glVertex3dv(x1);
	glVertex3dv(x2);
	
	glVertex3dv(x2);
	glVertex3dv(x3);
	
	glVertex3dv(x3);
	glVertex3dv(x0);
	
	glEnd();
      }
    }
  }
  
  glEndList();
  
  return list;
}
