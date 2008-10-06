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

#include <QtGui>
#include <iostream>
#include "vtkpost.h"

#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCamera.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>

using namespace std;

// EpNode:
//========
EpNode::EpNode()
{
  x[0] = 0.0;
  x[1] = 0.0;
  x[2] = 0.0;
}

EpNode::~EpNode()
{
}

// EpElement:
//===========
EpElement::EpElement()
{
  groupName = "";
  code = 0;
  indexes = 0;
  index = NULL;
}

EpElement::~EpElement()
{
  delete [] index;
}

// EpMesh:
//=========
EpMesh::EpMesh()
{
  epNodes = 0;
  epNode = NULL;

  epElements = 0;
  epElement = NULL;
}

EpMesh::~EpMesh()
{
  delete [] epNode;
  delete [] epElement;
}

// ScalarField:
//==============
ScalarField::ScalarField()
{
  menuAction = NULL;
  name = "";
  values = 0;
  value = NULL;
  minVal = +9.9e99;
  maxVal = -9.9e99;
}

ScalarField::~ScalarField()
{
  delete [] value;
}

// VtkPost:
//==========
VtkPost::VtkPost(QWidget *parent)
  : QMainWindow(parent)
{
  // Initialize:
  //------------
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  setWindowTitle("VTK widget...");

  createActions();
  createMenus();
  createToolbars();
  createStatusBar();
  
  epMesh = new EpMesh;

  postFileName = "";
  scalarFields = 0;
  scalarField = new ScalarField[100]; // fixed max.

  // Central widget:
  //----------------
  qvtkWidget = new QVTKWidget;
  setCentralWidget(qvtkWidget);

  // VTK interaction:
  //------------------
  scalarRenderer = vtkRenderer::New();
  scalarRenderer->SetBackground(1, 1, 1);
  qvtkWidget->GetRenderWindow()->AddRenderer(scalarRenderer);
}

VtkPost::~VtkPost()
{
}

QSize VtkPost::minimumSizeHint() const
{
  return QSize(64, 64);
}


QSize VtkPost::sizeHint() const
{
  return QSize(480, 640);
}

void VtkPost::createActions()
{
  // File menu:
  //-----------
  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip("Quit VTK widget");
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));
}

void VtkPost::createMenus()
{
  // File menu:
  //-----------
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // View menu:
  //-----------
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewScalarMenu = new QMenu(tr("Scalar"));
  viewMenu->addMenu(viewScalarMenu);
  connect(viewScalarMenu, SIGNAL(triggered(QAction*)), this, SLOT(drawScalarSlot(QAction*)));
}

void VtkPost::createToolbars()
{
}

void VtkPost::createStatusBar()
{
}

void VtkPost::setPostFileName(QString qString)
{
  this->postFileName = qString;
  cout << "Post file set to: " << qString.toAscii().data() << endl;
}


// Read in results:
//----------------------------------------------------------------------
bool VtkPost::readPostFile()
{
#define GET_TXT_STREAM                               \
  QString tmpLine = post.readLine().trimmed();       \
  while(tmpLine.isEmpty() || (tmpLine.at(0) == '#')) \
    tmpLine = post.readLine();                       \
    QTextStream txtStream(&tmpLine);

  QFile postFile(postFileName);

  if(!postFile.open(QIODevice::ReadOnly | QIODevice::Text))
    return false;

  cout << "Reading in post file" << endl;
  
  QTextStream post(&postFile);

  // Read nodes, elements, timesteps, and components:
  //--------------------------------------------------
  GET_TXT_STREAM

  int nodes, elements, timesteps, components;

  txtStream >> nodes >> elements >> components >> timesteps;

  cout << "Post: nodes: " << nodes << endl;
  cout << "Post: elements: " << elements << endl;
  cout << "Post: components: " << components << endl;
  cout << "Post: timesteps: " << timesteps << endl;

  // Read field names & set up menu entries:
  //----------------------------------------
  ScalarField *sf = NULL;

  for(int i = 0; i < components; i++) {
    QString fieldType, fieldName;
    txtStream >> fieldType >> fieldName;

    fieldType.replace(":", "");
    fieldType = fieldType.trimmed();
    fieldName = fieldName.trimmed();

    cout << "Field type: " << fieldType.toAscii().data() << endl;
    cout << "      name: " << fieldName.toAscii().data() << endl;

    if(fieldType == "scalar") {
      sf = &scalarField[scalarFields++];
      sf->menuAction = new QAction(fieldName, this);
      sf->menuAction->setCheckable(true);
      sf->name = fieldName;
      sf->values = nodes;
      sf->value = new double[nodes];
      viewScalarMenu->addAction(sf->menuAction);
    }

    if(fieldType == "vector") {
      sf = &scalarField[scalarFields++];
      sf->menuAction = new QAction(fieldName + ".1", this);
      sf->menuAction->setCheckable(true);
      sf->name = fieldName + ".1";    
      sf->values = nodes;
      sf->value = new double[nodes];
      viewScalarMenu->addAction(sf->menuAction);

      sf = &scalarField[scalarFields++];
      sf->menuAction = new QAction(fieldName + ".2", this);
      sf->menuAction->setCheckable(true);
      sf->name = fieldName + ".2";    
      sf->values = nodes;
      sf->value = new double[nodes];
      viewScalarMenu->addAction(sf->menuAction);

      sf = &scalarField[scalarFields++];
      sf->menuAction = new QAction(fieldName + ".3", this);
      sf->menuAction->setCheckable(true);
      sf->name = fieldName + ".3";    
      sf->values = nodes;
      sf->value = new double[nodes];
      viewScalarMenu->addAction(sf->menuAction);

      i += 2;
    }
  }

  // Nodes:
  //========
  epMesh->epNodes = nodes;
  epMesh->epNode = new EpNode[nodes];
  
  for(int i = 0; i < nodes; i++) {
    EpNode *epn = &epMesh->epNode[i];
    
    GET_TXT_STREAM

    for(int j = 0; j < 3; j++) 
      txtStream >> epn->x[j];
    
  }

  // Elements:
  //==========
  epMesh->epElements = elements;
  epMesh->epElement = new EpElement[elements];

  for(int i = 0; i < elements; i++) {
    EpElement *epe = &epMesh->epElement[i];
    
    GET_TXT_STREAM
    
    txtStream >> epe->groupName >> epe->code;
    
    epe->indexes = epe->code % 100;
    epe->index = new int[epe->indexes];
    
    for(int j = 0; j < epe->indexes; j++)
      txtStream >> epe->index[j];
  }

  // Data:
  //=======
  for(int i = 0; i < nodes; i++) {
    
    GET_TXT_STREAM

    for(int j = 0; j < scalarFields; j++) {
      sf = &scalarField[j];
      
      txtStream >> sf->value[i];
      
      if(sf->value[i] > sf->maxVal)
	sf->maxVal = sf->value[i];
      
      if(sf->value[i] < sf->minVal)
	sf->minVal = sf->value[i];
    }
  }
  
  postFile.close();
  return true;
}


// Exit VTK widget:
//----------------------------------------------------------------------
void VtkPost::exitSlot()
{
  close();
}

// Draw surface mesh:
//----------------------------------------------------------------------
void VtkPost::drawScalarSlot(QAction *qAction)
{
  if(epMesh == NULL)
    return;

  if(epMesh->epNodes == 0)
    return;

  if(epMesh->epElements == 0)
    return;

  // Check which action triggred drawing:
  //-------------------------------------
  int index = -1;

  ScalarField *sf = NULL;

  for(int i = 0; i < scalarFields; i++) {
    sf = &scalarField[i];
    
    if(sf->menuAction == qAction) {
      index = i;

      // Toggle rendering (Clear the scalar renderer and return):
      //---------------------------------------------------------
      if(!sf->menuAction->isChecked()) {
	qvtkWidget->GetRenderWindow()->RemoveRenderer(scalarRenderer);
	scalarRenderer = vtkRenderer::New();
	scalarRenderer->SetBackground(1, 1, 1);
	qvtkWidget->GetRenderWindow()->AddRenderer(scalarRenderer);
	return;
      }
    }

    sf->menuAction->setChecked(false);
  }

  if(index < 0)
    return;

  sf = &scalarField[index];
  sf->menuAction->setChecked(true);

  cout << "Displaying: " << sf->name.toAscii().data() << endl;
  cout << "      Min.: " << sf->minVal << endl;
  cout << "      Max.: " << sf->maxVal << endl;

  // Draw:
  //------
  vtkPolyData *surf = vtkPolyData::New();

  // Points:
  //------------
  vtkPoints *points = vtkPoints::New();
  for(int i = 0; i < epMesh->epNodes; i++) {
    EpNode *epn = &epMesh->epNode[i];
    points->InsertPoint(i, epn->x);
  }
  surf->SetPoints(points);
  points->Delete();

  // Polygons:
  //----------
  vtkCellArray *polys = vtkCellArray::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement *epe = &epMesh->epElement[i];
    polys->InsertNextCell(epe->indexes, epe->index);
  }
  surf->SetPolys(polys);
  polys->Delete();

  // Scalars:
  //---------
  vtkFloatArray *scalars = vtkFloatArray::New();
  for(int i = 0; i < epMesh->epNodes; i++) {
    double fieldValue = sf->value[i];
    scalars->InsertTuple1(i, fieldValue);
  }
  surf->GetPointData()->SetScalars(scalars);
  scalars->Delete();

  // Mapper:
  //--------
  vtkPolyDataMapper *surfMapper = vtkPolyDataMapper::New();
  surfMapper->SetInput(surf);
  surfMapper->SetScalarRange(sf->minVal, sf->maxVal);

  // Actor:
  //--------
  vtkActor *surfActor = vtkActor::New();
  surfActor->SetMapper(surfMapper);
  
  // Renderer:
  //----------
  scalarRenderer->AddActor(surfActor);
  scalarRenderer->ResetCamera();
  scalarRenderer->GetRenderWindow()->Render();

  // Clean up:
  //-----------
  surfActor->Delete();
  surfMapper->Delete();
  surf->Delete();
}
