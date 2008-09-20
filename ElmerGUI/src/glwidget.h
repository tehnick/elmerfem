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

#ifndef GLWIDGET_H
#define GLWIDGET_H

enum ListTypes {
  POINTLIST,
  EDGELIST,
  SURFACELIST,
  SURFACEMESHLIST,
  SHARPEDGELIST,
  VOLUMEMESHLIST,
  UNKNOWNLIST
};

#include <QGLWidget>
#include <QHash>
#include <QGLPixelBuffer>
#include <QTimer>
#include "helpers.h"
#include "meshutils.h"

#define DUMMY_NAME 0xffffffff

class list_t {
 public:
  list_t();
  ~list_t();

  int nature;        // PDE_UNKNOWN, PDE_BOUNDARY, PDE_BULK, ...
  int type;          // POINTLIST, EDGELIST, SURFACELIST, ...
  int index;         // Boundary condition as defined in input file
  GLuint object;     // GL list index as returned by glGenLists()
  int child;         // Index to the child list (-1 = no child)
  int parent;        // Index to the parent list (-1 = no parent)
  bool selected;     // Currently selected?
  bool visible;      // Currently visible?
};

class GLWidget : public QGLWidget
{
  Q_OBJECT
    
public:
  GLWidget(QWidget *parent = 0);
  ~GLWidget();
  
  QSize minimumSizeHint() const;
  QSize sizeHint() const;
  
  mesh_t *mesh;
  
  int lists;
  list_t *list;

  void rebuildLists();
  void rebuildSurfaceLists(void);
  void rebuildEdgeLists(void);

  bool toggleCoordinates();
  void enableIndicator(bool);
  
  // public state variables:
  bool stateFlatShade;
  bool stateDrawSurfaceMesh;
  bool stateDrawVolumeMesh;
  bool stateDrawSharpEdges;
  bool stateDrawSurfaceElements;
  bool stateDrawEdgeElements;
  bool stateDrawCoordinates;
  bool stateDrawSurfaceNumbers;
  bool stateDrawEdgeNumbers;
  bool stateDrawNodeNumbers;
  bool stateDrawBoundaryIndex;
  bool stateDrawBodyIndex;
  bool stateBcColors;
  bool stateBodyColors;
  bool ctrlPressed;
  bool shiftPressed;
  bool altPressed;
  bool bodyEditActive;
  bool stateUseBgImage;
  bool stateStretchBgImage;
  bool stateAlignRightBgImage;
  bool stateDrawIndicator;
  QString bgImageFileName;
  int currentlySelectedBody;
  QColor backgroundColor;
  QColor surfaceColor;
  QColor edgeColor;
  QColor surfaceMeshColor;
  QColor sharpEdgeColor;

  QHash<int, int> boundaryMap;
  QHash<int, int> bodyMap;

public slots:

signals:
  void signalBoundarySelected(list_t*);
  void escPressed();

protected:
  void initializeGL();
  void paintGL();
  void resizeGL(int, int);
  
  void focusInEvent(QFocusEvent*);
  void mouseDoubleClickEvent(QMouseEvent*);
  void mousePressEvent(QMouseEvent*);
  void mouseMoveEvent(QMouseEvent*);
  void wheelEvent(QWheelEvent*);
  void keyPressEvent(QKeyEvent*);
  void keyReleaseEvent(QKeyEvent*);
  
private:
  Helpers *helpers;
  Meshutils *meshutils;

  GLuint makeLists();
  
  GLdouble matrix[16];
  GLdouble invmatrix[16];
  void getMatrix();
  
  QPoint lastPos;
  
  GLuint generateSurfaceList(int, QColor);
  GLuint generateSurfaceMeshList(int, QColor);
  GLuint generateVolumeMeshList(QColor);
  GLuint generateEdgeList(int, QColor);
  GLuint generateSharpEdgeList(QColor);
  
  GLUquadricObj *quadric_axis;
  void drawCoordinates();

  GLUquadricObj *quadric_indicator;
  void drawIndicator();

  double drawTranslate[3];
  double drawScale;

  int bgSizeX;
  int bgSizeY;
  GLuint bgTexture;
  void drawBgImage();

  void changeNormalDirection(double*, double*);
  void drawSphere(int, int, float);
  QTimer *indicatorTimer;
  float indicatorColor;

private slots:
  void updateIndicator();

};

#endif
