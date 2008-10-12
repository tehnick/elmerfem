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

#include "epmesh.h"
#include "vtkpost.h"
#include "surface.h"
#include "isocontour.h"
#include "isosurface.h"
#include "colorbar.h"
#include "preferences.h"

#include <QVTKWidget.h>

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
  surface = new Surface(this);
  connect(surface, SIGNAL(drawSurfaceSignal()), this, SLOT(drawSurfaceSlot()));

  isoContour = new IsoContour(this);
  connect(isoContour, SIGNAL(drawIsoContourSignal()), this, SLOT(drawIsoContourSlot()));

  isoSurface = new IsoSurface(this);
  connect(isoSurface, SIGNAL(drawIsoSurfaceSignal()), this, SLOT(drawIsoSurfaceSlot()));

  colorBar = new ColorBar(this);
  connect(colorBar, SIGNAL(drawColorBarSignal()), this, SLOT(drawColorBarSlot()));

  preferences = new Preferences(this);
  connect(preferences, SIGNAL(redrawSignal()), this, SLOT(redrawSlot()));

  // Ep-data:
  //----------
  epMesh = new EpMesh;
  postFileName = "";
  postFileRead = false;
  scalarFields = 0;
  scalarField = new ScalarField[100]; // fixed max.

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
  drawWireframeAct = new QAction(QIcon(""), tr("Mesh edges"), this);
  drawWireframeAct->setStatusTip("Draw mesh edges");
  drawWireframeAct->setCheckable(true);
  drawWireframeAct->setChecked(false);
  connect(drawWireframeAct, SIGNAL(triggered()), this, SLOT(drawWireframeSlot()));
  connect(drawWireframeAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawFeatureEdgesAct = new QAction(QIcon(""), tr("Feature edges"), this);
  drawFeatureEdgesAct->setStatusTip("Draw feature edges");
  drawFeatureEdgesAct->setCheckable(true);
  drawFeatureEdgesAct->setChecked(true);
  connect(drawFeatureEdgesAct, SIGNAL(triggered()), this, SLOT(drawFeatureEdgesSlot()));
  connect(drawFeatureEdgesAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawColorBarAct = new QAction(QIcon(""), tr("Colorbar"), this);
  drawColorBarAct->setStatusTip("Draw color bar");
  drawColorBarAct->setCheckable(true);
  drawColorBarAct->setChecked(false);
  connect(drawColorBarAct, SIGNAL(triggered()), this, SLOT(showColorBarDialogSlot()));
  connect(drawColorBarAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawFieldNameAct = new QAction(QIcon(""), tr("Field name"), this);
  drawFieldNameAct->setStatusTip("Draw field name");
  drawFieldNameAct->setCheckable(true);
  drawFieldNameAct->setChecked(false);
  connect(drawFieldNameAct, SIGNAL(triggered()), this, SLOT(drawFieldNameSlot()));
  connect(drawFieldNameAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawSurfaceAct = new QAction(QIcon(""), tr("Surfaces"), this);
  drawSurfaceAct->setStatusTip("Draw scalars on surfaces");
  drawSurfaceAct->setCheckable(true);
  drawSurfaceAct->setChecked(false);
  connect(drawSurfaceAct, SIGNAL(triggered()), this, SLOT(drawSurfaceDialogSlot()));
  connect(drawSurfaceAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawIsoContourAct = new QAction(QIcon(""), tr("Isocontours"), this);
  drawIsoContourAct->setStatusTip("Draw isocontours (2d)");
  drawIsoContourAct->setCheckable(true);
  drawIsoContourAct->setChecked(false);
  connect(drawIsoContourAct, SIGNAL(triggered()), this, SLOT(showIsoContourDialogSlot()));
  connect(drawIsoContourAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawIsoSurfaceAct = new QAction(QIcon(""), tr("Isosurfaces"), this);
  drawIsoSurfaceAct->setStatusTip("Draw isosurfaces (3d)");
  drawIsoSurfaceAct->setCheckable(true);
  drawIsoSurfaceAct->setChecked(false);
  connect(drawIsoSurfaceAct, SIGNAL(triggered()), this, SLOT(showIsoSurfaceDialogSlot()));
  connect(drawIsoSurfaceAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  redrawAct = new QAction(QIcon(""), tr("Redraw"), this);
  redrawAct->setShortcut(tr("Ctrl+R"));
  redrawAct->setStatusTip("Redraw");
  connect(redrawAct, SIGNAL(triggered()), this, SLOT(redrawSlot()));

  preferencesAct = new QAction(QIcon(""), tr("Preferences"), this);
  preferencesAct->setStatusTip("Show preferences");
  connect(preferencesAct, SIGNAL(triggered()), this, SLOT(preferencesSlot()));
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
  viewMenu->addAction(drawSurfaceAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawIsoContourAct);
  viewMenu->addAction(drawIsoSurfaceAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawColorBarAct);
  viewMenu->addSeparator();
  // viewMenu->addAction(drawFieldNameAct);
  // viewMenu->addSeparator();
  viewMenu->addAction(preferencesAct);
  viewMenu->addSeparator();
  viewMenu->addAction(redrawAct);
}

void VtkPost::createToolbars()
{
  viewToolBar = addToolBar(tr("View"));
  viewToolBar->addAction(drawSurfaceAct);
  viewToolBar->addAction(drawIsoContourAct);
  viewToolBar->addAction(drawIsoSurfaceAct);
  viewToolBar->addAction(drawColorBarAct);
  viewToolBar->addSeparator();
  viewToolBar->addAction(preferencesAct);
  viewToolBar->addSeparator();
  viewToolBar->addAction(redrawAct);
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

  // Populate the widgets in user interface dialogs:
  //-------------------------------------------------
  surface->populateWidgets(scalarField, scalarFields);
  isoSurface->populateWidgets(scalarField, scalarFields);
  isoContour->populateWidgets(scalarField, scalarFields);
  surface->populateWidgets(scalarField, scalarFields);
  colorBar->populateWidgets();

  this->postFileRead = true;
  groupChangedSlot(NULL);
  connect(editGroupsMenu, SIGNAL(triggered(QAction*)), this, SLOT(groupChangedSlot(QAction*)));

  // Set the null field active:
  //---------------------------
  drawSurfaceAct->setChecked(true);
  redrawSlot();
  
  return true;
}


// Add a scalar field:
//----------------------------------------------------------------------
ScalarField* VtkPost::addScalarField(QString fieldName, int nodes)
{
  ScalarField *sf = &scalarField[scalarFields++];
  sf->name = fieldName;
  sf->values = nodes;
  sf->value = new double[nodes];

  for(int i = 0; i < nodes; i++) 
    sf->value[i] = 0.0;

  sf->minVal = +9.9e99;
  sf->maxVal = -9.9e99;

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
  volumeGrid->Delete();
  surfaceGrid->Delete();
  lineGrid->Delete();

  volumeGrid = vtkUnstructuredGrid::New();
  surfaceGrid = vtkUnstructuredGrid::New();
  lineGrid = vtkUnstructuredGrid::New();

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
      
      if(groupAction->isChecked()) {
	volumeGrid->InsertNextCell(tetra->GetCellType(), tetra->GetPointIds());
      }
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
      
      if(groupAction->isChecked()) {
	surfaceGrid->InsertNextCell(tria->GetCellType(), tria->GetPointIds());
      }
    }

    if(epe->code == 404) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 4; j++)
	quad->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked()) {
	surfaceGrid->InsertNextCell(quad->GetCellType(), quad->GetPointIds());
      }
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
      
      if(groupAction->isChecked()) {
	lineGrid->InsertNextCell(line->GetCellType(), line->GetPointIds());
      }
    }

  }
  line->Delete();

  redrawSlot();
}


// Show preferences dialog:
//----------------------------------------------------------------------
void VtkPost::preferencesSlot()
{
  preferences->show();
}

// Maybe redraw:
//----------------------------------------------------------------------
void VtkPost::maybeRedrawSlot(bool value)
{
  if(!value) redrawSlot();
}


// Redraw:
//----------------------------------------------------------------------
void VtkPost::redrawSlot()
{  
  drawWireframeSlot();
  drawFeatureEdgesSlot();
  drawSurfaceSlot();
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
  if(!drawFieldNameAct->isChecked()) return;

  return;

#if 0

  // Draw field name (scalar field):
  //--------------------------------
  // QString fieldName = currentScalarFieldName;
  // if(fieldName.isEmpty()) return;
  
  fieldNameActor->SetDisplayPosition(15, 15);
  fieldNameActor->SetInput(fieldName.toAscii().data());
  fieldNameActor->GetTextProperty()->SetFontSize(24);
  fieldNameActor->GetTextProperty()->SetFontFamilyToArial();
  fieldNameActor->GetTextProperty()->BoldOn();
  fieldNameActor->GetTextProperty()->ItalicOn();
  fieldNameActor->GetTextProperty()->SetColor(0, 0, 1);

  renderer->AddActor2D(fieldNameActor);

  qvtkWidget->GetRenderWindow()->Render();
#endif
}



// Draw color bar:
//----------------------------------------------------------------------
void VtkPost::showColorBarDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();

  if(drawColorBarAct->isChecked()) {
    colorBar->show();
  } else {
    colorBar->close();
    drawColorBarSlot();
  }
}

void VtkPost::drawColorBarSlot()
{
  renderer->RemoveActor(colorBarActor);
  if(!drawColorBarAct->isChecked()) return;

  // Draw color bar:
  //----------------
  vtkTextMapper *tMapper = vtkTextMapper::New();
  colorBarActor->SetMapper(tMapper);

  QString actorName = colorBar->ui.colorCombo->currentText().trimmed();

  if(actorName.isEmpty()) return;

  QString fieldName = "";

  vtkScalarsToColors *lut = NULL;

  if(actorName == "Surface") {
    fieldName = currentSurfaceName;
    if(fieldName.isEmpty()) return;
    lut = scalarFieldActor->GetMapper()->GetLookupTable();
  }

  if(actorName == "Isocontour") {
    fieldName = currentIsoContourName;
    if(fieldName.isEmpty()) return;
    lut = isoContourActor->GetMapper()->GetLookupTable();
  }

  if(actorName == "Isosurface") {
    fieldName = currentIsoSurfaceName;
    if(fieldName.isEmpty()) return;
    lut = isoSurfaceActor->GetMapper()->GetLookupTable();
  }

  if(!lut) return;

  colorBarActor->SetLookupTable(lut);

  bool horizontal = colorBar->ui.horizontalRButton->isChecked();
  bool annotate = colorBar->ui.annotateBox->isChecked();
  int labels = colorBar->ui.labelsSpin->value();
  double width = colorBar->ui.widthEdit->text().toDouble();
  double height = colorBar->ui.heightEdit->text().toDouble();

  if(width < 0.01) width = 0.01;
  if(width > 0.99) width = 0.99;
  if(height < 0.01) height = 0.01;
  if(height > 0.99) height = 0.99;

  colorBarActor->SetPosition(0.05, 0.05);

  if(horizontal) {
    colorBarActor->SetOrientationToHorizontal();
    colorBarActor->SetWidth(height);
    colorBarActor->SetHeight(width);
  } else {
    colorBarActor->SetOrientationToVertical();
    colorBarActor->SetWidth(width);
    colorBarActor->SetHeight(height);
  }
  
  colorBarActor->SetNumberOfLabels(labels);

  colorBarActor->GetLabelTextProperty()->SetFontSize(16);
  colorBarActor->GetLabelTextProperty()->SetFontFamilyToArial();
  colorBarActor->GetLabelTextProperty()->BoldOn();
  colorBarActor->GetLabelTextProperty()->ItalicOn();
  colorBarActor->GetLabelTextProperty()->SetColor(0, 0, 1);
  
  colorBarActor->GetTitleTextProperty()->SetFontSize(16);
  colorBarActor->GetTitleTextProperty()->SetFontFamilyToArial();
  colorBarActor->GetTitleTextProperty()->BoldOn();
  colorBarActor->GetTitleTextProperty()->ItalicOn();
  colorBarActor->GetTitleTextProperty()->SetColor(0, 0, 1);
  
  if(annotate) {
    colorBarActor->SetTitle(fieldName.toAscii().data());
  } else {
    colorBarActor->SetTitle("");
  }

  renderer->AddActor(colorBarActor);
  qvtkWidget->GetRenderWindow()->Render();

  tMapper->Delete();
}



// Draw surface wireframe:
//----------------------------------------------------------------------
void VtkPost::drawWireframeSlot()
{
  renderer->RemoveActor(wireframeActor);
  if(!drawWireframeAct->isChecked()) return;

  bool useSurfaceMesh = preferences->ui.meshEdgesSurface->isChecked();
  int lineWidth = preferences->ui.meshLineWidth->value();

  vtkExtractEdges *edges = vtkExtractEdges::New();
  if(useSurfaceMesh) {
    edges->SetInput(surfaceGrid);
  } else {
    edges->SetInput(volumeGrid);
  }

  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(edges->GetOutputPort());
  mapper->ScalarVisibilityOff();
  mapper->SetResolveCoincidentTopologyToPolygonOffset();

  wireframeActor->GetProperty()->SetLineWidth(lineWidth);
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
  if(!drawFeatureEdgesAct->isChecked()) return;

  bool useSurfaceGrid = preferences->ui.surfaceRButton->isChecked();
  int featureAngle = preferences->ui.angleSpin->value();
  int lineWidth = preferences->ui.lineWidthSpin->value();
  
  // Convert from vtkUnstructuredGrid to vtkPolyData:
  vtkGeometryFilter *filter = vtkGeometryFilter::New();
  if(useSurfaceGrid) {
    filter->SetInput(surfaceGrid);
  } else {
    filter->SetInput(volumeGrid);
  }
  filter->GetOutput()->ReleaseDataFlagOn();

  vtkFeatureEdges *edges = vtkFeatureEdges::New();
  edges->SetInputConnection(filter->GetOutputPort());
  edges->SetFeatureAngle(featureAngle);
  edges->BoundaryEdgesOn();
  edges->ManifoldEdgesOn();
  edges->NonManifoldEdgesOn();

  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection(edges->GetOutputPort());
  mapper->ScalarVisibilityOff();
  mapper->SetResolveCoincidentTopologyToPolygonOffset();

  featureEdgeActor->GetProperty()->SetLineWidth(lineWidth);
  featureEdgeActor->GetProperty()->SetColor(0, 0, 0);
  featureEdgeActor->SetMapper(mapper);

  renderer->AddActor(featureEdgeActor);
  qvtkWidget->GetRenderWindow()->Render();
  
  filter->Delete();
  edges->Delete();
  mapper->Delete();
}

// Draw surfaces:
//----------------------------------------------------------------------
void VtkPost::drawSurfaceDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();

  if(drawSurfaceAct->isChecked()) {
    surface->show();
  } else {
    surface->close();
    drawSurfaceSlot();
  }
}


// Draw surfaces:
//----------------------------------------------------------------------
void VtkPost::drawSurfaceSlot()
{
  renderer->RemoveActor(scalarFieldActor);
  if(!drawSurfaceAct->isChecked()) return;

  // Data from UI:
  //--------------
  int surfaceIndex = surface->ui.surfaceCombo->currentIndex();
  QString surfaceName = surface->ui.surfaceCombo->currentText();
  double minVal = surface->ui.minEdit->text().toDouble();
  double maxVal = surface->ui.maxEdit->text().toDouble();

  // Scalars:
  //---------
  surfaceGrid->GetPointData()->RemoveArray("Surface");
  vtkFloatArray *scalars = vtkFloatArray::New();
  ScalarField *sf = &scalarField[surfaceIndex];
  scalars->SetNumberOfComponents(1);
  scalars->SetNumberOfTuples(sf->values);
  scalars->SetName("Surface");
  for(int i = 0; i < sf->values; i++)
    scalars->SetComponent(i, 0, sf->value[i]);  
  surfaceGrid->GetPointData()->AddArray(scalars);

  // Mapper:
  //--------
  vtkDataSetMapper *scalarFieldMapper = vtkDataSetMapper::New();
  scalarFieldMapper->SetInput(surfaceGrid);
  scalarFieldMapper->SetScalarModeToUsePointFieldData();
  scalarFieldMapper->SelectColorArray("Surface");
  scalarFieldMapper->ScalarVisibilityOn();
  scalarFieldMapper->SetScalarRange(sf->minVal, sf->maxVal);
  scalarFieldMapper->SetResolveCoincidentTopologyToPolygonOffset();

  // Actor:
  //-------
  scalarFieldActor->SetMapper(scalarFieldMapper);

  // Renderer:
  //-----------
  renderer->AddActor(scalarFieldActor);

  // Update color bar && field name:
  //---------------------------------
  currentSurfaceName = sf->name;
  drawColorBarSlot();
  drawFieldNameSlot();  

  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //-----------
  scalars->Delete();
  scalarFieldMapper->Delete();
}



// Draw isosurfaces (3D):
//----------------------------------------------------------------------
void VtkPost::showIsoSurfaceDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();

  if(drawIsoSurfaceAct->isChecked()) {
    isoSurface->show();
  } else {
    isoSurface->close();
    drawIsoSurfaceSlot();
  }
}

void VtkPost::drawIsoSurfaceSlot()
{
  renderer->RemoveActor(isoSurfaceActor);
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
  volumeGrid->GetPointData()->RemoveArray("IsoSurface");
  vtkFloatArray *contourArray = vtkFloatArray::New();
  ScalarField *sf = &scalarField[contourIndex];
  contourArray->SetNumberOfComponents(1);
  contourArray->SetNumberOfTuples(sf->values);
  contourArray->SetName("IsoSurface");
  for(int i = 0; i < sf->values; i++)
    contourArray->SetComponent(i, 0, sf->value[i]);
  volumeGrid->GetPointData()->AddArray(contourArray);

  volumeGrid->GetPointData()->RemoveArray("IsoSurfaceColor");
  vtkFloatArray *colorArray = vtkFloatArray::New();
  sf = &scalarField[colorIndex];
  colorArray->SetName("IsoSurfaceColor");
  colorArray->SetNumberOfComponents(1);
  colorArray->SetNumberOfTuples(sf->values);
  for(int i = 0; i < sf->values; i++)
    colorArray->SetComponent(i, 0, sf->value[i]);
  volumeGrid->GetPointData()->AddArray(colorArray);

  // Isosurfaces && normals:
  //--------------------------
  vtkContourFilter *iso = vtkContourFilter::New();
  volumeGrid->GetPointData()->SetActiveScalars("IsoSurface");
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

  mapper->ScalarVisibilityOn();
  mapper->SelectColorArray("IsoSurfaceColor");
  mapper->SetScalarModeToUsePointFieldData();
  mapper->SetScalarRange(colorMinVal, colorMaxVal);

  // Actor:
  //-------
  isoSurfaceActor->SetMapper(mapper);

  // Renderer:
  //-----------
  renderer->AddActor(isoSurfaceActor);

  // Redraw text && colorbar:
  //--------------------------
  currentIsoSurfaceName = colorName;
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
  qvtkWidget->GetRenderWindow()->Render();

  if(drawIsoContourAct->isChecked()) {
    isoContour->show();
  } else {
    isoContour->close();
    drawIsoContourSlot();
  }
}

void VtkPost::drawIsoContourSlot()
{
  renderer->RemoveActor(isoContourActor);
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

  if(contourName == "Null") return;

  // Scalars:
  //----------
  surfaceGrid->GetPointData()->RemoveArray("IsoContour");
  vtkFloatArray *contourArray = vtkFloatArray::New();
  ScalarField *sf = &scalarField[contourIndex];
  contourArray->SetNumberOfComponents(1);
  contourArray->SetNumberOfTuples(sf->values);
  contourArray->SetName("IsoContour");
  for(int i = 0; i < sf->values; i++)
    contourArray->SetComponent(i, 0, sf->value[i]);
  surfaceGrid->GetPointData()->AddArray(contourArray);

  surfaceGrid->GetPointData()->RemoveArray("IsoContourColor");
  vtkFloatArray *colorArray = vtkFloatArray::New();
  sf = &scalarField[colorIndex];
  colorArray->SetName("IsoContourColor");
  colorArray->SetNumberOfComponents(1);
  colorArray->SetNumberOfTuples(sf->values);
  for(int i = 0; i < sf->values; i++)
    colorArray->SetComponent(i, 0, sf->value[i]);
  surfaceGrid->GetPointData()->AddArray(colorArray);

  // Isocontours:
  //--------------
  vtkContourFilter *iso = vtkContourFilter::New();
  surfaceGrid->GetPointData()->SetActiveScalars("IsoContour");
  iso->SetInput(surfaceGrid);
  iso->ComputeScalarsOn();
  iso->GenerateValues(contours, contourMinVal, contourMaxVal);

  // Mapper:
  //--------
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(iso->GetOutputPort());
  mapper->ScalarVisibilityOn();
  mapper->SelectColorArray("IsoContourColor");
  mapper->SetScalarModeToUsePointFieldData();
  mapper->SetScalarRange(colorMinVal, colorMaxVal);

  // Actor:
  //-------
  isoContourActor->SetMapper(mapper);
  isoContourActor->GetProperty()->SetLineWidth(lineWidth);

  // Renderer:
  //-----------
  renderer->AddActor(isoContourActor);

  // Redraw text && colorbar:
  //--------------------------
  currentIsoContourName = colorName;
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
