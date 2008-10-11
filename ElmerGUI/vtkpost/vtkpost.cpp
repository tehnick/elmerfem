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
#include <vtkLookupTable.h>
#include <vtkProperty.h>
#include <vtkPolyDataMapper2D.h>
#include <vtkScalarBarActor.h>
#include <vtkTextMapper.h>
#include <vtkScalarBarActor.h>
#include <vtkTextActor.h>
#include <vtkTextProperty.h>
#include <vtkTetra.h>
#include <vtkTriangle.h>
#include <vtkQuad.h>
#include <vtkLine.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataSetMapper.h>
#include <vtkContourFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkOutlineFilter.h>
#include <vtkCleanPolyData.h>
#include <vtkExtractEdges.h>
#include <vtkFeatureEdges.h>
#include <vtkGeometryFilter.h>

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
  delete menuAction;
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
  setWindowTitle("VTK widget");

  createActions();
  createMenus();
  createToolbars();
  createStatusBar();

  // VTK:
  //-----
  volumeGrid = vtkUnstructuredGrid::New();
  surfaceGrid = vtkUnstructuredGrid::New();
  lineGrid = vtkUnstructuredGrid::New();
  isoContourActor = vtkActor::New();
  isoSurfaceActor = vtkActor::New();
  scalarFieldActor = vtkActor::New();
  wireframeActor = vtkActor::New();
  colorBarActor = vtkScalarBarActor::New();
  fieldNameActor = vtkTextActor::New();
  featureEdgeActor = vtkActor::New();

  // User interfaces:
  //-----------------
  isoContour = new IsoContour;
  connect(isoContour, SIGNAL(drawIsoContourSignal()), this, SLOT(drawIsoContourSlot()));

  isoSurface = new IsoSurface;
  connect(isoSurface, SIGNAL(drawIsoSurfaceSignal()), this, SLOT(drawIsoSurfaceSlot()));

  // Ep-data:
  //----------
  epMesh = new EpMesh;
  postFileName = "";
  postFileRead = false;
  scalarFields = 0;
  scalarField = new ScalarField[100]; // fixed max.
  currentScalarFieldIndex = 0;
  currentScalarFieldName = "";

  // Central widget:
  //----------------
  qvtkWidget = new QVTKWidget;
  setCentralWidget(qvtkWidget);

  // VTK interaction:
  //------------------
  renderer = vtkRenderer::New();
  renderer->SetBackground(1, 1, 1);
  qvtkWidget->GetRenderWindow()->AddRenderer(renderer);
  renderer->GetRenderWindow()->Render();
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
  return QSize(640, 480);
}

void VtkPost::createActions()
{
  // File menu:
  //-----------
  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip("Quit VTK widget");
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));

  // View menu:
  //------------
  drawWireframeAct = new QAction(QIcon(""), tr("Surface wireframe"), this);
  drawWireframeAct->setStatusTip("Draw surface wireframe");
  drawWireframeAct->setCheckable(true);
  drawWireframeAct->setChecked(false);
  connect(drawWireframeAct, SIGNAL(triggered()), this, SLOT(drawWireframeSlot()));

  drawFeatureEdgesAct = new QAction(QIcon(""), tr("Feature edges"), this);
  drawFeatureEdgesAct->setStatusTip("Draw feature edges");
  drawFeatureEdgesAct->setCheckable(true);
  drawFeatureEdgesAct->setChecked(true);
  connect(drawFeatureEdgesAct, SIGNAL(triggered()), this, SLOT(drawFeatureEdgesSlot()));

  drawColorBarAct = new QAction(QIcon(""), tr("Color bar"), this);
  drawColorBarAct->setStatusTip("Draw color bar");
  drawColorBarAct->setCheckable(true);
  drawColorBarAct->setChecked(false);
  connect(drawColorBarAct, SIGNAL(triggered()), this, SLOT(drawColorBarSlot()));

  drawFieldNameAct = new QAction(QIcon(""), tr("Field name"), this);
  drawFieldNameAct->setStatusTip("Draw field name");
  drawFieldNameAct->setCheckable(true);
  drawFieldNameAct->setChecked(false);
  connect(drawFieldNameAct, SIGNAL(triggered()), this, SLOT(drawFieldNameSlot()));

  drawIsoContourAct = new QAction(QIcon(""), tr("Iso contours"), this);
  drawIsoContourAct->setStatusTip("Draw iso contours (2d)");
  drawIsoContourAct->setCheckable(true);
  drawIsoContourAct->setChecked(false);
  connect(drawIsoContourAct, SIGNAL(triggered()), this, SLOT(showIsoContourDialogSlot()));

  drawIsoSurfaceAct = new QAction(QIcon(""), tr("Iso surfaces"), this);
  drawIsoSurfaceAct->setStatusTip("Draw iso surfaces (3d)");
  drawIsoSurfaceAct->setCheckable(true);
  drawIsoSurfaceAct->setChecked(false);
  connect(drawIsoSurfaceAct, SIGNAL(triggered()), this, SLOT(showIsoSurfaceDialogSlot()));

  redrawAct = new QAction(QIcon(""), tr("Reset"), this);
  redrawAct->setShortcut(tr("Ctrl+R"));
  redrawAct->setStatusTip("Reset view");
  connect(redrawAct, SIGNAL(triggered()), this, SLOT(redrawSlot()));
}

void VtkPost::createMenus()
{
  // File menu:
  //-----------
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // Edit menu:
  //-----------
  editMenu = menuBar()->addMenu(tr("&Edit"));
  editGroupsMenu = new QMenu(tr("Groups"));
  editMenu->addMenu(editGroupsMenu);

  // View menu:
  //-----------
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawWireframeAct);
  viewMenu->addAction(drawFeatureEdgesAct);
  viewMenu->addSeparator();
  viewScalarMenu = new QMenu(tr("Scalar"));
  viewMenu->addMenu(viewScalarMenu);
  connect(viewScalarMenu, SIGNAL(triggered(QAction*)), this, SLOT(drawScalarOnSurfaceSlot(QAction*)));
  viewMenu->addSeparator();
  viewMenu->addAction(drawIsoContourAct);
  viewMenu->addAction(drawIsoSurfaceAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawColorBarAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawFieldNameAct);
  viewMenu->addSeparator();
  viewMenu->addAction(redrawAct);
}

void VtkPost::createToolbars()
{
}

void VtkPost::createStatusBar()
{
}



// Read in ep-results:
//----------------------------------------------------------------------
bool VtkPost::readPostFile(QString postFileName)
{
#define GET_TXT_STREAM                               \
  QString tmpLine = post.readLine().trimmed();       \
  while(tmpLine.isEmpty() || (tmpLine.at(0) == '#')) \
    tmpLine = post.readLine();                       \
  QTextStream txtStream(&tmpLine);

  // Open the post file:
  //=====================
  this->postFileName = postFileName;
  this->postFileRead = false;

  QFile postFile(postFileName);

  if(!postFile.open(QIODevice::ReadOnly | QIODevice::Text))
    return false;

  cout << "Loading ep-file" << endl;
  
  QTextStream post(&postFile);

  // Read in nodes, elements, timesteps, and scalar components:
  //-----------------------------------------------------------
  GET_TXT_STREAM

  int nodes, elements, timesteps, components;

  txtStream >> nodes >> elements >> components >> timesteps;

  cout << "Nodes: " << nodes << endl;
  cout << "Elements: " << elements << endl;
  cout << "Scalar components: " << components << endl;
  cout << "Timesteps: " << timesteps << endl;

  // Read field names & set up menu actions:
  //=========================================
  if(epMesh->epNode) delete [] epMesh->epNode;
  if(epMesh->epElement) delete [] epMesh->epElement;

  for(int i = 0; i < scalarFields; i++ ) {
     ScalarField *sf = &scalarField[i];
     if(sf->value) delete [] sf->value;
  }

  scalarFields = 0;
  viewScalarMenu->clear();

  // Add the null field:
  //--------------------
  ScalarField *nullField = addScalarField("Null", nodes);
  nullField->minVal = 0.0;
  nullField->maxVal = 0.0;

  // Add the scalar fields:
  //-----------------------
  for(int i = 0; i < components; i++) {
    QString fieldType, fieldName;
    txtStream >> fieldType >> fieldName;

    fieldType.replace(":", "");
    fieldType = fieldType.trimmed();
    fieldName = fieldName.trimmed();

    cout << "Field type: " << fieldType.toAscii().data() << endl;
    cout << "Field name: " << fieldName.toAscii().data() << endl;

    if(fieldType == "scalar")
      addScalarField(fieldName, nodes);

    if(fieldType == "vector") {
      addScalarField(fieldName + "_x", nodes);
      addScalarField(fieldName + "_y", nodes);
      addScalarField(fieldName + "_z", nodes);
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

  // Add nodes to scalar field variables:
  //-------------------------------------
  ScalarField *sf = NULL;

  sf = addScalarField("Nodes_x", nodes);
  for(int i = 0; i < nodes; i++) {
    sf->value[i] = epMesh->epNode[i].x[0];
    if(sf->value[i] > sf->maxVal) sf->maxVal = sf->value[i];
    if(sf->value[i] < sf->minVal) sf->minVal = sf->value[i];
  }

  sf = addScalarField("Nodes_y", nodes);
  for(int i = 0; i < nodes; i++) {
    sf->value[i] = epMesh->epNode[i].x[1];
    if(sf->value[i] > sf->maxVal) sf->maxVal = sf->value[i];
    if(sf->value[i] < sf->minVal) sf->minVal = sf->value[i];
  }

  sf = addScalarField("Nodes_z", nodes);
  for(int i = 0; i < nodes; i++) {
    sf->value[i] = epMesh->epNode[i].x[2];
    if(sf->value[i] > sf->maxVal) sf->maxVal = sf->value[i];
    if(sf->value[i] < sf->minVal) sf->minVal = sf->value[i];
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

    for(int j = 0; j < scalarFields - 4; j++) { // - 4 = no nodes, no null field
      sf = &scalarField[j + 1];                 // + 1 = skip null field
      
      txtStream >> sf->value[i];
      
      if(sf->value[i] > sf->maxVal)
	sf->maxVal = sf->value[i];
      
      if(sf->value[i] < sf->minVal)
	sf->minVal = sf->value[i];
    }
  }
  
  postFile.close();

  // Set up the group edit menu:
  //=============================
  groupActionHash.clear();
  editGroupsMenu->clear();

  for(int i = 0; i < elements; i++) {
    EpElement *epe = &epMesh->epElement[i];

    QString groupName = epe->groupName;
    
    if(groupActionHash.contains(groupName))
      continue;

    QAction *groupAction = new QAction(groupName, this);
    groupAction->setCheckable(true);
    groupAction->setChecked(true);
    editGroupsMenu->addAction(groupAction);
    groupActionHash.insert(groupName, groupAction);
  }

  this->postFileRead = true;
  groupChangedSlot(NULL);

  connect(editGroupsMenu, SIGNAL(triggered(QAction*)), this, SLOT(groupChangedSlot(QAction*)));

  redrawSlot();
  return true;
}


// Add a scalar field:
//----------------------------------------------------------------------
ScalarField* VtkPost::addScalarField(QString fieldName, int nodes)
{
  ScalarField *sf = &scalarField[scalarFields++];

  sf->menuAction = new QAction(fieldName, this);
  sf->menuAction->setCheckable(true);
  sf->name = fieldName;
  sf->values = nodes;
  sf->value = new double[nodes];

  for(int i = 0; i < nodes; i++) 
    sf->value[i] = 0.0;

  sf->minVal = +9.9e30;
  sf->maxVal = -9.9e30;

  viewScalarMenu->addAction(sf->menuAction);

  if(scalarFields == 1) {
    sf->menuAction->setChecked(true);
    currentScalarFieldIndex = 0;
    currentScalarFieldName = sf->name;
  } else {
    sf->menuAction->setChecked(false);
  } 

  return sf;
}



// Close the widget:
//----------------------------------------------------------------------
void VtkPost::exitSlot()
{
  close();
}



// Group selection changed:
//----------------------------------------------------------------------
void VtkPost::groupChangedSlot(QAction *groupAction)
{
  // Status of groupAction has changed: regenerate grids
  //-----------------------------------------------------
  volumeGrid->Reset();
  surfaceGrid->Reset();
  lineGrid->Reset();

  // Points:
  //---------
  vtkPoints *points = vtkPoints::New();
  points->SetNumberOfPoints(epMesh->epNodes);
  for(int i = 0; i < epMesh->epNodes; i++) {
    EpNode *epn = &epMesh->epNode[i];
    points->InsertPoint(i, epn->x);
  }
  volumeGrid->SetPoints(points);
  surfaceGrid->SetPoints(points);
  lineGrid->SetPoints(points);
  points->Delete();

  // Volume grid:
  //---------------
  vtkTetra *tetra = vtkTetra::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement *epe = &epMesh->epElement[i];

    if(epe->code == 504) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 4; j++)
	tetra->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	volumeGrid->InsertNextCell(tetra->GetCellType(), tetra->GetPointIds());
    }

  }
  tetra->Delete();

  // Surface grid:
  //---------------
  vtkTriangle *tria = vtkTriangle::New();
  vtkQuad *quad = vtkQuad::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement *epe = &epMesh->epElement[i];

    if(epe->code == 303) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 3; j++)
	tria->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	surfaceGrid->InsertNextCell(tria->GetCellType(), tria->GetPointIds());
    }

    if(epe->code == 404) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 4; j++)
	quad->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	surfaceGrid->InsertNextCell(quad->GetCellType(), quad->GetPointIds());
    }

  }
  tria->Delete();
  quad->Delete();

  // Line grid:
  //---------------
  vtkLine *line = vtkLine::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement *epe = &epMesh->epElement[i];

    if(epe->code == 202) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 3; j++)
	line->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	lineGrid->InsertNextCell(line->GetCellType(), line->GetPointIds());
    }

  }
  line->Delete();

  redrawSlot();
}



// Redraw:
//----------------------------------------------------------------------
void VtkPost::redrawSlot()
{  
  drawWireframeSlot();
  drawFeatureEdgesSlot();
  drawScalarOnSurfaceSlot(NULL);
  drawColorBarSlot();
  drawFieldNameSlot();
  drawIsoContourSlot();
  drawIsoSurfaceSlot();
  renderer->ResetCamera();
  qvtkWidget->GetRenderWindow()->Render();
}



// Draw field name:
//----------------------------------------------------------------------
void VtkPost::drawFieldNameSlot()
{
  renderer->RemoveActor(fieldNameActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawFieldNameAct->isChecked()) return;
  if(currentScalarFieldName.isEmpty()) return;

  // Draw field name (scalar field):
  //--------------------------------
  QString fieldName = currentScalarFieldName;

  if(fieldName.isEmpty()) return;
  
  fieldNameActor->SetDisplayPosition(15, 15);
  fieldNameActor->SetInput(fieldName.toAscii().data());
  fieldNameActor->GetTextProperty()->SetFontSize(24);
  fieldNameActor->GetTextProperty()->SetFontFamilyToArial();
  fieldNameActor->GetTextProperty()->BoldOn();
  fieldNameActor->GetTextProperty()->ItalicOn();
  // fieldNameActor->GetTextProperty()->ShadowOn();
  fieldNameActor->GetTextProperty()->SetColor(0, 0, 1);

  renderer->AddActor2D(fieldNameActor);

  qvtkWidget->GetRenderWindow()->Render();
}



// Draw color bar:
//----------------------------------------------------------------------
void VtkPost::drawColorBarSlot()
{
  renderer->RemoveActor(colorBarActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawColorBarAct->isChecked()) return;

  // Draw color bar:
  //----------------
  vtkTextMapper *tMapper = vtkTextMapper::New();
  colorBarActor->SetMapper(tMapper);

  // is this ok?
  colorBarActor->GetLabelTextProperty()->SetFontSize(16);
  colorBarActor->GetLabelTextProperty()->SetFontFamilyToArial();
  colorBarActor->GetLabelTextProperty()->BoldOn();
  colorBarActor->GetLabelTextProperty()->ItalicOn();
  // colorBarActor->GetLabelTextProperty()->ShadowOn();
  colorBarActor->GetLabelTextProperty()->SetColor(0, 0, 1);
  
  colorBarActor->GetTitleTextProperty()->SetFontSize(16);
  colorBarActor->GetTitleTextProperty()->SetFontFamilyToArial();
  colorBarActor->GetTitleTextProperty()->BoldOn();
  colorBarActor->GetTitleTextProperty()->ItalicOn();
  // colorBarActor->GetTitleTextProperty()->ShadowOn();
  colorBarActor->GetTitleTextProperty()->SetColor(0, 0, 1);
  
  colorBarActor->SetTitle(currentScalarFieldName.toAscii().data());

  renderer->AddActor(colorBarActor);
  
  qvtkWidget->GetRenderWindow()->Render();

  tMapper->Delete();
}



// Draw surface wireframe:
//----------------------------------------------------------------------
void VtkPost::drawWireframeSlot()
{
  renderer->RemoveActor(wireframeActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawWireframeAct->isChecked()) return;

  vtkExtractEdges *edges = vtkExtractEdges::New();
  edges->SetInput(surfaceGrid);

  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(edges->GetOutputPort());
  mapper->ScalarVisibilityOff();
  mapper->SetResolveCoincidentTopologyToPolygonOffset();

  wireframeActor->GetProperty()->SetColor(0, 0, 0);
  wireframeActor->SetMapper(mapper);

  renderer->AddActor(wireframeActor);

  qvtkWidget->GetRenderWindow()->Render();

  mapper->Delete();
  edges->Delete();
}


// Draw feature edges:
//----------------------------------------------------------------------
void VtkPost::drawFeatureEdgesSlot()
{
  renderer->RemoveActor(featureEdgeActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawFeatureEdgesAct->isChecked()) return;
  
  // Convert from vtkUnstructuredGrid to vtkPolyData:
  vtkGeometryFilter *filter = vtkGeometryFilter::New();
  filter->SetInput(surfaceGrid);
  filter->GetOutput()->ReleaseDataFlagOn();

  vtkFeatureEdges *edges = vtkFeatureEdges::New();
  edges->SetInputConnection(filter->GetOutputPort());
  edges->SetFeatureAngle(20.0);
  edges->BoundaryEdgesOn();
  edges->ManifoldEdgesOn();
  edges->NonManifoldEdgesOn();

  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection(edges->GetOutputPort());
  mapper->ScalarVisibilityOff();
  mapper->SetResolveCoincidentTopologyToPolygonOffset();

  featureEdgeActor->GetProperty()->SetColor(0, 0, 0);
  featureEdgeActor->SetMapper(mapper);

  renderer->AddActor(featureEdgeActor);
  qvtkWidget->GetRenderWindow()->Render();
  
  filter->Delete();
  edges->Delete();
  mapper->Delete();
}


// Draw scalar field on surface:
//----------------------------------------------------------------------
void VtkPost::drawScalarOnSurfaceSlot(QAction *triggeredAction)
{
  renderer->RemoveActor(scalarFieldActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;

  // Check which scalar menu action triggred drawing:
  //-------------------------------------------------
  int index = -1;
  ScalarField *sf = NULL;
  bool shouldReturn = false;

  for(int i = 0; i < scalarFields; i++) {
    sf = &scalarField[i];
    
    if(sf->menuAction == triggeredAction) {
      index = i;

      // Check if we simply want to clear the view:
      //-------------------------------------------
      if(!sf->menuAction->isChecked())
	shouldReturn = true;
      
    } else {

      // Set all other scalar menu actions unchecked:
      //---------------------------------------------
      sf->menuAction->setChecked(false);
    }
  }

  if(shouldReturn) return;
  if(index < 0) index = currentScalarFieldIndex;

  sf = &scalarField[index];
  sf->menuAction->setChecked(true);

  // Scalars:
  //---------
  vtkFloatArray *scalars = vtkFloatArray::New();
  scalars->SetNumberOfComponents(1);
  scalars->SetNumberOfTuples(sf->values);

  for(int i = 0; i < sf->values; i++)
    scalars->SetComponent(i, 0, sf->value[i]);

  surfaceGrid->GetPointData()->SetScalars(scalars);

  // Mapper:
  //--------
  vtkDataSetMapper *scalarFieldMapper = vtkDataSetMapper::New();
  scalarFieldMapper->SetInput(surfaceGrid);
  scalarFieldMapper->SetScalarRange(sf->minVal, sf->maxVal);
  scalarFieldMapper->SetResolveCoincidentTopologyToPolygonOffset();
  colorBarActor->SetLookupTable(scalarFieldMapper->GetLookupTable());

  // Actor:
  //-------
  scalarFieldActor->SetMapper(scalarFieldMapper);

  // Renderer:
  //-----------
  renderer->AddActor(scalarFieldActor);

  // Update color bar && field name:
  //---------------------------------
  currentScalarFieldIndex = index;
  currentScalarFieldName = sf->name;
  drawColorBarSlot();
  drawFieldNameSlot();  

  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //-----------
  scalars->Delete();
  scalarFieldMapper->Delete();
}



// Draw iso surfaces (3D):
//----------------------------------------------------------------------
void VtkPost::showIsoSurfaceDialogSlot()
{
  if(drawIsoSurfaceAct->isChecked()) {
    // setup
    isoSurface->populateWidgets(scalarField, scalarFields);
    isoSurface->show();
  } else {
    // remove
    isoSurface->close();
    drawIsoSurfaceSlot();
  }
}

void VtkPost::drawIsoSurfaceSlot()
{
  renderer->RemoveActor(isoSurfaceActor);
  
  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawIsoSurfaceAct->isChecked()) return;

  // Data from UI:
  //--------------
  int contourIndex = isoSurface->ui.contoursCombo->currentIndex();
  QString contourName = isoSurface->ui.contoursCombo->currentText();
  int contours = isoSurface->ui.contoursSpin->value() + 1;
  double contourMinVal = isoSurface->ui.contoursMinEdit->text().toDouble();
  double contourMaxVal = isoSurface->ui.contoursMaxEdit->text().toDouble();
  bool useNormals = isoSurface->ui.normalsCheck->isChecked();
  int colorIndex = isoSurface->ui.colorCombo->currentIndex();
  QString colorName = isoSurface->ui.colorCombo->currentText();
  double colorMinVal = isoSurface->ui.colorMinEdit->text().toDouble();
  double colorMaxVal = isoSurface->ui.colorMaxEdit->text().toDouble();

  if(contourName == "Null") return;

  // Scalars:
  //----------
  vtkFloatArray *contourArray = vtkFloatArray::New();
  ScalarField *sf = &scalarField[contourIndex];
  contourArray->SetNumberOfComponents(1);
  contourArray->SetNumberOfTuples(sf->values);
  contourArray->SetName("Contour");
  for(int i = 0; i < sf->values; i++)
    contourArray->SetComponent(i, 0, sf->value[i]);
  volumeGrid->GetPointData()->SetScalars(contourArray);

  // Isosurfaces && normals:
  //--------------------------
  vtkContourFilter *iso = vtkContourFilter::New();
  iso->SetInput(volumeGrid);
  iso->ComputeScalarsOn();
  iso->GenerateValues(contours, contourMinVal, contourMaxVal);

  vtkPolyDataNormals *normals;
  if(useNormals) {
    normals = vtkPolyDataNormals::New();
    normals->SetInputConnection(iso->GetOutputPort());
    normals->SetFeatureAngle(45);
  }

  // Mapper:
  //--------
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();

  if(useNormals) {
    mapper->SetInputConnection(normals->GetOutputPort());
  } else {
    mapper->SetInputConnection(iso->GetOutputPort());
  }

  vtkFloatArray *colorArray = vtkFloatArray::New();
  sf = &scalarField[colorIndex];
  colorArray->SetName("Color");
  colorArray->SetNumberOfComponents(1);
  colorArray->SetNumberOfTuples(sf->values);
  for(int i = 0; i < sf->values; i++)
    colorArray->SetComponent(i, 0, sf->value[i]);
  volumeGrid->GetPointData()->AddArray(colorArray);

  mapper->ScalarVisibilityOn();
  mapper->SelectColorArray("Color");
  mapper->SetScalarModeToUsePointFieldData();
  mapper->SetScalarRange(colorMinVal, colorMaxVal);

  colorBarActor->SetLookupTable(mapper->GetLookupTable());
  
  // Actor:
  //-------
  isoSurfaceActor->SetMapper(mapper);

  // Renderer:
  //-----------
  renderer->AddActor(isoSurfaceActor);

  // Redraw text && colorbar:
  //--------------------------
  currentScalarFieldIndex = colorIndex;
  currentScalarFieldName = colorName;
  drawColorBarSlot();
  drawFieldNameSlot();  

  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //----------
  contourArray->Delete();
  colorArray->Delete();
  // remove colorArray from volume grid?
  iso->Delete();
  if(useNormals) normals->Delete();
  mapper->Delete();
}



// Draw iso contours (2D):
//----------------------------------------------------------------------
void VtkPost::showIsoContourDialogSlot()
{
  if(drawIsoContourAct->isChecked()) {
    // setup
    isoContour->populateWidgets(scalarField, scalarFields);
    isoContour->show();
  } else {
    // remove
    isoContour->close();
    drawIsoContourSlot();
  }
}

void VtkPost::drawIsoContourSlot()
{
  renderer->RemoveActor(isoContourActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawIsoContourAct->isChecked()) return;

  // Data from UI:
  //--------------
  int contourIndex = isoContour->ui.contoursCombo->currentIndex();
  QString contourName = isoContour->ui.contoursCombo->currentText();
  int contours = isoContour->ui.contoursSpin->value() + 1;
  int lineWidth = isoContour->ui.lineWidthSpin->value();
  double contourMinVal = isoContour->ui.contoursMinEdit->text().toDouble();
  double contourMaxVal = isoContour->ui.contoursMaxEdit->text().toDouble();
  int colorIndex = isoContour->ui.colorCombo->currentIndex();
  QString colorName = isoContour->ui.colorCombo->currentText();
  double colorMinVal = isoContour->ui.colorMinEdit->text().toDouble();
  double colorMaxVal = isoContour->ui.colorMaxEdit->text().toDouble();

  if(contourName == "Null")
    return;

  // Scalars:
  //----------
  vtkFloatArray *contourArray = vtkFloatArray::New();
  ScalarField *sf = &scalarField[contourIndex];
  contourArray->SetNumberOfComponents(1);
  contourArray->SetNumberOfTuples(sf->values);
  contourArray->SetName("Contour");
  for(int i = 0; i < sf->values; i++)
    contourArray->SetComponent(i, 0, sf->value[i]);
  surfaceGrid->GetPointData()->SetScalars(contourArray);

  // Isosurfaces && normals:
  //--------------------------
  vtkContourFilter *iso = vtkContourFilter::New();
  iso->SetInput(surfaceGrid);
  iso->ComputeScalarsOn();
  iso->GenerateValues(contours, contourMinVal, contourMaxVal);

  // Mapper:
  //--------
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(iso->GetOutputPort());
  mapper->ScalarVisibilityOn();

  vtkFloatArray *colorArray = vtkFloatArray::New();
  sf = &scalarField[colorIndex];
  colorArray->SetName("Color");
  colorArray->SetNumberOfComponents(1);
  colorArray->SetNumberOfTuples(sf->values);
  for(int i = 0; i < sf->values; i++)
    colorArray->SetComponent(i, 0, sf->value[i]);
  surfaceGrid->GetPointData()->AddArray(colorArray);

  mapper->SelectColorArray("Color");
  mapper->SetScalarModeToUsePointFieldData();
  mapper->SetScalarRange(colorMinVal, colorMaxVal);

  colorBarActor->SetLookupTable(mapper->GetLookupTable());
  
  // Actor:
  //-------
  isoContourActor->SetMapper(mapper);
  isoContourActor->GetProperty()->SetLineWidth(lineWidth);

  // Renderer:
  //-----------
  renderer->AddActor(isoContourActor);

  // Redraw text && colorbar:
  //--------------------------
  currentScalarFieldIndex = colorIndex;
  currentScalarFieldName = colorName;
  drawColorBarSlot();
  drawFieldNameSlot();  
  
  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //----------
  contourArray->Delete();
  colorArray->Delete();
  // remove colorArray from volume grid?
  iso->Delete();
  mapper->Delete();
}
