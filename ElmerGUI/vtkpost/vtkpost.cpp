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
#include <vtkLine.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataSetMapper.h>
#include <vtkContourFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkOutlineFilter.h>

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
  scalarFieldActor = vtkActor::New();
  wireframeActor = vtkActor::New();
  colorBarActor = vtkScalarBarActor::New();
  fieldNameActor = vtkTextActor::New();
  
  // User interfaces:
  //-----------------
  isoContours = new IsoContours;
  connect(isoContours, SIGNAL(drawIsoContourSignal()), this, SLOT(drawIsoContourSlot()));

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
  drawWireframeAct = new QAction(QIcon(""), tr("Wireframe"), this);
  drawWireframeAct->setStatusTip("Draw wireframe");
  drawWireframeAct->setCheckable(true);
  drawWireframeAct->setChecked(true);
  connect(drawWireframeAct, SIGNAL(triggered()), this, SLOT(drawWireframeSlot()));

  drawColorBarAct = new QAction(QIcon(""), tr("Color bar"), this);
  drawColorBarAct->setStatusTip("Draw color bar");
  drawColorBarAct->setCheckable(true);
  drawColorBarAct->setChecked(true);
  connect(drawColorBarAct, SIGNAL(triggered()), this, SLOT(drawColorBarSlot()));

  drawFieldNameAct = new QAction(QIcon(""), tr("Field name"), this);
  drawFieldNameAct->setStatusTip("Draw field name");
  drawFieldNameAct->setCheckable(true);
  drawFieldNameAct->setChecked(true);
  connect(drawFieldNameAct, SIGNAL(triggered()), this, SLOT(drawFieldNameSlot()));

  drawIsoContourAct = new QAction(QIcon(""), tr("Isocontours (test)"), this);
  drawIsoContourAct->setStatusTip("Draw isocontours");
  drawIsoContourAct->setCheckable(true);
  drawIsoContourAct->setChecked(false);
  connect(drawIsoContourAct, SIGNAL(triggered()), this, SLOT(showIsoContourDialogSlot()));

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
  viewMenu->addSeparator();
  viewScalarMenu = new QMenu(tr("Scalar"));
  viewMenu->addMenu(viewScalarMenu);
  connect(viewScalarMenu, SIGNAL(triggered(QAction*)), this, SLOT(drawScalarSlot(QAction*)));
  viewMenu->addSeparator();
  viewMenu->addAction(drawIsoContourAct);
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
  if ( epMesh->epNode ) delete [] epMesh->epNode;
  if ( epMesh->epElement ) delete [] epMesh->epElement;

  for( int i=0;i<scalarFields; i++ ) {
     ScalarField *sf = &scalarField[i];
     if ( sf->value ) delete [] sf->value;
  }

  scalarFields = 0;
  viewScalarMenu->clear();
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

    for(int j = 0; j < scalarFields - 3; j++) { // - 3 = no nodes
      sf = &scalarField[j];
      
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
  sf->minVal = +9.9e99;
  sf->maxVal = -9.9e99;
  viewScalarMenu->addAction(sf->menuAction);
  if ( scalarFields==1 ) {
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

  }
  tria->Delete();

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
  drawScalarSlot(NULL);
  drawColorBarSlot();
  drawFieldNameSlot();
  renderer->ResetCamera();
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

  // is this ok?
  colorBarActor->GetLabelTextProperty()->SetFontSize(16);
  colorBarActor->GetLabelTextProperty()->SetFontFamilyToArial();
  colorBarActor->GetLabelTextProperty()->BoldOn();
  colorBarActor->GetLabelTextProperty()->ItalicOn();
  // colorBarActor->GetLabelTextProperty()->ShadowOn();
  colorBarActor->GetLabelTextProperty()->SetColor(0, 0, 1);
  colorBarActor->SetMapper(tMapper);

  renderer->AddActor(colorBarActor);
  
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


  // ?????
  // Now all this should be replaced by displaying the edges
  // from the surfaceGrid. No need to regenerate new structures.

  // Draw the wireframe mesh:
  //-------------------------
  vtkPolyData *wireframe = vtkPolyData::New();



  // Points:
  //--------
  vtkPoints *points = vtkPoints::New();
  for(int i = 0; i < epMesh->epNodes; i++) {
    EpNode *epn = &epMesh->epNode[i];
    points->InsertPoint(i, epn->x);
  }
  wireframe->SetPoints(points);
  points->Delete();

  // Line segments:
  //---------------
  int n[2];
  vtkCellArray *segments = vtkCellArray::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement *epe = &epMesh->epElement[i];
    QString groupName = epe->groupName;

    if(groupName.isEmpty())
      continue;

    QAction *groupAction = groupActionHash.value(groupName);

    if(groupAction == NULL)
      continue;

    if(epe->code == 303) {
      if(groupAction->isChecked()) {
	n[0] = epe->index[0];
	n[1] = epe->index[1];
	segments->InsertNextCell(2, n);
	
	n[0] = epe->index[1];
	n[1] = epe->index[2];
	segments->InsertNextCell(2, n);
	
	n[0] = epe->index[2];
	n[1] = epe->index[0];
	segments->InsertNextCell(2, n);
      }
    }

    if(epe->code == 404) {
      if(groupAction->isChecked()) {
	n[0] = epe->index[0];
	n[1] = epe->index[1];
	segments->InsertNextCell(2, n);
	
	n[0] = epe->index[1];
	n[1] = epe->index[2];
	segments->InsertNextCell(2, n);
	
	n[0] = epe->index[2];
	n[1] = epe->index[3];
	segments->InsertNextCell(2, n);

	n[0] = epe->index[3];
	n[1] = epe->index[0];
	segments->InsertNextCell(2, n);
      }
    }
  }
  wireframe->SetLines(segments);
  segments->Delete();

  // Scalars:
  //---------
  vtkFloatArray *scalars = vtkFloatArray::New();
  for(int i = 0; i < epMesh->epNodes; i++)
    scalars->InsertTuple1(i, 0.0);
  wireframe->GetPointData()->SetScalars(scalars);
  scalars->Delete();

  // Lookuptable for black-and-white colors:
  //----------------------------------------
  vtkLookupTable *lut = vtkLookupTable::New();
  lut->SetNumberOfTableValues(3);
  lut->SetTableRange(0.0, 1.0);
  lut->SetTableValue(0, 0.0, 0.0, 0.0);
  lut->SetTableValue(1, 1.0, 1.0, 1.0);
  lut->Build();

  // Mapper:
  //--------
  vtkPolyDataMapper *wireframeMapper = vtkPolyDataMapper::New();
  wireframeMapper->SetInput(wireframe);
  wireframeMapper->SetLookupTable(lut);

  // Actor:
  //-------
  wireframeActor->SetMapper(wireframeMapper);

  // Renderer:
  //----------
  renderer->AddActor(wireframeActor);

  // wireframeActor->GetProperty()->SetLineWidth(1.5);

  // Clean up:
  //-----------
  lut->Delete();
  wireframeMapper->Delete();
  wireframe->Delete();
}





// Draw scalar field:
//----------------------------------------------------------------------
void VtkPost::drawScalarSlot(QAction *triggeredAction)
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
  for(int i = 0; i < epMesh->epNodes; i++) {
    double fieldValue = sf->value[i];
    scalars->InsertTuple1(i, fieldValue);
  }
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

  // Clean up:
  //-----------
  scalars->Delete();
  scalarFieldMapper->Delete();
}





// Draw isocontours:
//----------------------------------------------------------------------
void VtkPost::showIsoContourDialogSlot()
{
  isoContours->populateWidgets(scalarField, scalarFields);
  isoContours->show();
}

void VtkPost::drawIsoContourSlot()
{
  renderer->RemoveActor(isoContourActor);

  if(epMesh == NULL) return;
  if(epMesh->epNodes < 1) return;
  if(epMesh->epElements < 1) return;
  if(!drawIsoContourAct->isChecked()) return;

  // Scalars:
  //----------
  int index = isoContours->ui.variableCombo->currentIndex();
  QString name = isoContours->ui.variableCombo->currentText();
  int contours = isoContours->ui.contoursSpin->value() + 1;
  double minVal = isoContours->ui.contoursMinEdit->text().toDouble();
  double maxVal = isoContours->ui.contoursMaxEdit->text().toDouble();

  ScalarField *sf = &scalarField[index];
  vtkFloatArray *scalars = vtkFloatArray::New();
  for(int i = 0; i < epMesh->epNodes; i++) {
    double fieldValue = sf->value[i];
    scalars->InsertTuple1(i, fieldValue);
  }
  volumeGrid->GetPointData()->SetScalars(scalars);

  // Isosourface:
  //--------------
  vtkContourFilter *iso = vtkContourFilter::New();
  iso->SetInput(volumeGrid);
  iso->GenerateValues(contours, minVal, maxVal);

  vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
  normals->SetInputConnection(iso->GetOutputPort());
  normals->SetFeatureAngle(45);

  // Mapper:
  //--------
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(normals->GetOutputPort());
  mapper->ScalarVisibilityOn();
  mapper->SetScalarRange(sf->minVal, sf->maxVal);
  colorBarActor->SetLookupTable(mapper->GetLookupTable());

  // Actor:
  //-------
  isoContourActor->SetMapper(mapper);

  // Renderer:
  //-----------
  renderer->AddActor(isoContourActor);

  // Redraw text && colorbar:
  //--------------------------
  currentScalarFieldIndex = index;
  currentScalarFieldName = name;
  drawColorBarSlot();
  drawFieldNameSlot();  

  // Clean up:
  //----------
  scalars->Delete();
  iso->Delete();
  normals->Delete();
  mapper->Delete();
}
