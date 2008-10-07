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
 *  ElmerGUI vtkpost                                                         *
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

#ifndef VTKPOST_H
#define VTKPOST_H

#include <QMainWindow>
#include <QHash>
#include "QVTKWidget.h"
#include "src/meshtype.h"

class vtkRenderer;
class vtkActor;
class vtkScalarBarActor;
class vtkPolyDataMapper;
class vtkTextActor;

// EpNode:
//========
class EpNode
{
 public:
  EpNode();
  ~EpNode();

  double x[3];
};

// EpElement:
//===========
class EpElement
{
 public:
  EpElement();
  ~EpElement();

  QString groupName;
  int code;
  int indexes;
  int *index;
};

// EpMesh:
//=========
class EpMesh
{
 public:
  EpMesh();
  ~EpMesh();

  int epNodes;
  EpNode *epNode;

  int epElements;
  EpElement *epElement;
};

// ScalarField:
//=============
class ScalarField
{
 public:
  ScalarField();
  ~ScalarField();

  QAction *menuAction;
  QString name;
  int values;
  double *value;
  double minVal;
  double maxVal;
};

// VtkPost:
//==========
class VtkPost : public QMainWindow
{
  Q_OBJECT

public:
  VtkPost(QWidget *parent = 0);
  ~VtkPost();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  bool readPostFile(QString);

signals:

private slots:
  void exitSlot();
  void drawScalarSlot(QAction*);
  void redrawSlot();
  void groupChangedSlot(QAction*);
  void drawWireframeSlot();
  void drawColorBarSlot();
  void drawFieldNameSlot();

private:
  QMenu *fileMenu;
  QMenu *editMenu;
  QMenu *editGroupsMenu;
  QMenu *viewMenu;
  QMenu *viewScalarMenu;

  QAction *exitAct;
  QAction *redrawAct;
  QAction *drawWireframeAct;
  QAction *drawColorBarAct;
  QAction *drawFieldNameAct;

  void createActions();
  void createMenus();
  void createToolbars();
  void createStatusBar();

  ScalarField* addScalarField(QString, int);

  EpMesh *epMesh;

  QString postFileName;
  bool postFileRead;

  int scalarFields;
  ScalarField *scalarField;
  QAction *currentScalarFieldAction;

  QHash<QString, QAction*> groupActionHash;

  QVTKWidget *qvtkWidget;
  vtkRenderer *renderer;

  vtkActor *scalarFieldActor;
  vtkActor *wireframeActor;
  vtkScalarBarActor *colorBarActor;
  vtkTextActor *fieldNameActor;

  vtkPolyDataMapper *scalarFieldMapper;
};

#endif // VTKPOST_H
