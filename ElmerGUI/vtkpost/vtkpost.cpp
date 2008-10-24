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
#include "vector.h"
#include "streamline.h"
#include "timestep.h"
#include "axes.h"
#include "featureedge.h"
#include "meshpoint.h"
#include "meshedge.h"

#ifdef MATC
#include "matc.h"
#endif

#include <QVTKWidget.h>
#include <vtkLookupTable.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCamera.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkProperty.h>
#include <vtkScalarBarActor.h>
#include <vtkTetra.h>
#include <vtkHexahedron.h>
#include <vtkTriangle.h>
#include <vtkQuad.h>
#include <vtkLine.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataSetMapper.h>
#include <vtkWindowToImageFilter.h>
#include <vtkPNGWriter.h>
#include <vtkCellDerivatives.h>
#include <vtkCellDataToPointData.h>
#include <vtkPlane.h>
#include <vtkCellPicker.h>
#include <vtkCallbackCommand.h>
#include <vtkAbstractPicker.h>
#include <vtkObject.h>
#include <vtkCommand.h>
#include <vtkFollower.h>

using namespace std;

// Pick event handler (place cursor on screen && press 'p' to pick):
//-------------------------------------------------------------------
static void pickEventHandler(vtkObject* caller, unsigned long eid, 
			     void* clientdata, void* calldata)
{
  VtkPost* vtkPost = reinterpret_cast<VtkPost*>(clientdata);
  vtkRenderer* renderer = vtkPost->GetRenderer();
  vtkActor* pickedPointActor = vtkPost->GetPickedPointActor();
  QVTKWidget* qvtkWidget = vtkPost->GetQVTKWidget();
  vtkAbstractPicker* picker = qvtkWidget->GetInteractor()->GetPicker();
  vtkCellPicker* cellPicker = vtkCellPicker::SafeDownCast(picker);

  int cellId = cellPicker->GetCellId();
  double* pickPos = vtkPost->GetCurrentPickPosition();
  cellPicker->GetPickPosition(pickPos);
  vtkPost->SetCurrentPickPosition(pickPos);

  if(cellId < 0) {
    renderer->RemoveActor(pickedPointActor);

  } else {
    vtkDataSetMapper* mapper = vtkDataSetMapper::New();
    vtkUnstructuredGrid* cross = vtkUnstructuredGrid::New();
    vtkPoints* points = vtkPoints::New();
    vtkLine* line = vtkLine::New();
    double l = vtkPost->GetLength() / 15.0;

    points->SetNumberOfPoints(6);
    points->InsertPoint(0, +l,  0,  0);
    points->InsertPoint(1,  0, +l,  0);
    points->InsertPoint(2,  0,  0, +l);
    points->InsertPoint(3, -l,  0,  0);    
    points->InsertPoint(4,  0, -l,  0);    
    points->InsertPoint(5,  0,  0, -l);
    cross->SetPoints(points);

    line->GetPointIds()->SetId(0, 0);
    line->GetPointIds()->SetId(1, 3);
    cross->InsertNextCell(line->GetCellType(), line->GetPointIds());

    line->GetPointIds()->SetId(0, 1);
    line->GetPointIds()->SetId(1, 4);
    cross->InsertNextCell(line->GetCellType(), line->GetPointIds());

    line->GetPointIds()->SetId(0, 2);
    line->GetPointIds()->SetId(1, 5);
    cross->InsertNextCell(line->GetCellType(), line->GetPointIds());

    mapper->SetInput(cross);

    pickedPointActor->SetMapper(mapper);
    pickedPointActor->SetPosition(pickPos);
    pickedPointActor->GetProperty()->SetColor(1, 0, 0);

    renderer->AddActor(pickedPointActor);

    mapper->Delete();
    cross->Delete();
    points->Delete();
    line->Delete();
  }
}

// Class VtkPost:
//-----------------------------------------------------------------
VtkPost::VtkPost(QWidget *parent)
  : QMainWindow(parent)
{
  // Initialize:
  //------------
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  setWindowTitle("ElmerGUI postprocessor");

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
  surfaceActor = vtkActor::New();
  meshEdgeActor = vtkActor::New();
  meshPointActor = vtkActor::New();
  colorBarActor = vtkScalarBarActor::New();
  featureEdgeActor = vtkActor::New();
  vectorActor = vtkActor::New();
  streamLineActor = vtkActor::New();
  axesActor = vtkActor::New();
  axesXTextActor = vtkFollower::New();
  axesYTextActor = vtkFollower::New();
  axesZTextActor = vtkFollower::New();
  pickedPointActor = vtkActor::New();

  // Default color map (from blue to red):
  //--------------------------------------
  currentLut = vtkLookupTable::New();
  currentLut->SetHueRange(0.6667, 0);
  currentLut->SetNumberOfColors(128);
  currentLut->Build();

  // Clip plane:
  //-------------
  clipPlane = vtkPlane::New();

  // User interfaces, widgets, and draw routines:
  //---------------------------------------------
  surface = new Surface(this);
  connect(surface, SIGNAL(drawSurfaceSignal()), this, SLOT(drawSurfaceSlot()));
  connect(surface, SIGNAL(hideSurfaceSignal()), this, SLOT(hideSurfaceSlot()));

  vector = new Vector(this);
  connect(vector, SIGNAL(drawVectorSignal()), this, SLOT(drawVectorSlot()));
  connect(vector, SIGNAL(hideVectorSignal()), this, SLOT(hideVectorSlot()));
  
  isoContour = new IsoContour(this);
  connect(isoContour, SIGNAL(drawIsoContourSignal()), this, SLOT(drawIsoContourSlot()));
  connect(isoContour, SIGNAL(hideIsoContourSignal()), this, SLOT(hideIsoContourSlot()));

  isoSurface = new IsoSurface(this);
  connect(isoSurface, SIGNAL(drawIsoSurfaceSignal()), this, SLOT(drawIsoSurfaceSlot()));
  connect(isoSurface, SIGNAL(hideIsoSurfaceSignal()), this, SLOT(hideIsoSurfaceSlot()));

  colorBar = new ColorBar(this);
  connect(colorBar, SIGNAL(drawColorBarSignal()), this, SLOT(drawColorBarSlot()));
  connect(colorBar, SIGNAL(hideColorBarSignal()), this, SLOT(hideColorBarSlot()));

  streamLine = new StreamLine(this);
  connect(streamLine, SIGNAL(drawStreamLineSignal()), this, SLOT(drawStreamLineSlot()));
  connect(streamLine, SIGNAL(hideStreamLineSignal()), this, SLOT(hideStreamLineSlot()));

  preferences = new Preferences(this);
  connect(preferences, SIGNAL(redrawSignal()), this, SLOT(redrawSlot()));

  timeStep = new TimeStep(this);
  connect(timeStep, SIGNAL(timeStepChangedSignal()), this, SLOT(timeStepChangedSlot()));
  connect(this, SIGNAL(canProceedWithNextSignal(vtkRenderWindow*)), timeStep, SLOT(canProceedWithNextSlot(vtkRenderWindow*)));

  axes = new Axes(this);
  featureEdge = new FeatureEdge(this);
  meshPoint = new MeshPoint(this);
  meshEdge = new MeshEdge(this);

#ifdef MATC
  matc = new Matc(this);
  connect(matc->ui.mcEdit, SIGNAL(returnPressed()), this, SLOT(domatcSlot()));
  connect(matc->ui.mcHistory, SIGNAL(selectionChanged()), this, SLOT(matcCutPasteSlot()));
#endif

  // Ep-data:
  //----------
  epMesh = new EpMesh;
  postFileName = "";
  postFileRead = false;
  scalarFields = 0;
  scalarField = new ScalarField[MAX_SCALARS];

  // Central widget:
  //----------------
  qvtkWidget = new QVTKWidget(this);
  setCentralWidget(qvtkWidget);

  // VTK interaction:
  //------------------
  renderer = vtkRenderer::New();
  renderer->SetBackground(1, 1, 1);
  qvtkWidget->GetRenderWindow()->AddRenderer(renderer);
  renderer->GetRenderWindow()->Render();

  // Create a cell picker and set the callback & observer:
  //------------------------------------------------------
  vtkCellPicker* cellPicker = vtkCellPicker::New();
  qvtkWidget->GetInteractor()->SetPicker(cellPicker);
  cellPicker->Delete();

  vtkCallbackCommand* cbc = vtkCallbackCommand::New();
  cbc->SetClientData(this);
  cbc->SetCallback(pickEventHandler);
  vtkAbstractPicker* picker = qvtkWidget->GetInteractor()->GetPicker();
  picker->AddObserver(vtkCommand::EndPickEvent, cbc);
  cbc->Delete();
}

VtkPost::~VtkPost()
{
}

void VtkPost::createActions()
{
  // File menu:
  //-----------
  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip("Quit VTK widget");
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));

  savePictureAct = new QAction(QIcon(""), tr("Save picture as..."), this);
  savePictureAct->setStatusTip("Save picture in file");
  connect(savePictureAct, SIGNAL(triggered()), this, SLOT(savePictureSlot()));

  // View menu:
  //------------
  drawMeshPointAct = new QAction(QIcon(""), tr("Mesh points"), this);
  drawMeshPointAct->setStatusTip("Draw mesh points");
  drawMeshPointAct->setCheckable(true);
  drawMeshPointAct->setChecked(false);
  connect(drawMeshPointAct, SIGNAL(triggered()), this, SLOT(drawMeshPointSlot()));
  connect(drawMeshPointAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawMeshEdgeAct = new QAction(QIcon(""), tr("Mesh edges"), this);
  drawMeshEdgeAct->setStatusTip("Draw mesh edges");
  drawMeshEdgeAct->setCheckable(true);
  drawMeshEdgeAct->setChecked(false);
  connect(drawMeshEdgeAct, SIGNAL(triggered()), this, SLOT(drawMeshEdgeSlot()));
  connect(drawMeshEdgeAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawFeatureEdgesAct = new QAction(QIcon(""), tr("Feature edges"), this);
  drawFeatureEdgesAct->setStatusTip("Draw feature edges");
  drawFeatureEdgesAct->setCheckable(true);
  drawFeatureEdgesAct->setChecked(true);
  connect(drawFeatureEdgesAct, SIGNAL(triggered()), this, SLOT(drawFeatureEdgesSlot()));
  connect(drawFeatureEdgesAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawAxesAct = new QAction(QIcon(""), tr("Coordinate axes"), this);
  drawAxesAct->setStatusTip("Draw cordinate axes");
  drawAxesAct->setCheckable(true);
  drawAxesAct->setChecked(false);
  connect(drawAxesAct, SIGNAL(triggered()), this, SLOT(drawAxesSlot()));

  drawColorBarAct = new QAction(QIcon(""), tr("Colorbar"), this);
  drawColorBarAct->setStatusTip("Draw color bar");
  drawColorBarAct->setCheckable(true);
  drawColorBarAct->setChecked(false);
  connect(drawColorBarAct, SIGNAL(triggered()), this, SLOT(showColorBarDialogSlot()));
  connect(drawColorBarAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawSurfaceAct = new QAction(QIcon(""), tr("Surfaces"), this);
  drawSurfaceAct->setStatusTip("Draw scalar fields on surfaces");
  drawSurfaceAct->setCheckable(true);
  drawSurfaceAct->setChecked(false);
  connect(drawSurfaceAct, SIGNAL(triggered()), this, SLOT(showSurfaceDialogSlot()));
  connect(drawSurfaceAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  drawVectorAct = new QAction(QIcon(""), tr("Vectors"), this);
  drawVectorAct->setStatusTip("Visualize vector fields by arrows");
  drawVectorAct->setCheckable(true);
  drawVectorAct->setChecked(false);
  connect(drawVectorAct, SIGNAL(triggered()), this, SLOT(showVectorDialogSlot()));
  connect(drawVectorAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

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

  drawStreamLineAct = new QAction(QIcon(""), tr("Streamlines"), this);
  drawStreamLineAct->setStatusTip("Draw stream lines");
  drawStreamLineAct->setCheckable(true);
  drawStreamLineAct->setChecked(false);
  connect(drawStreamLineAct, SIGNAL(triggered()), this, SLOT(showStreamLineDialogSlot()));
  connect(drawStreamLineAct, SIGNAL(toggled(bool)), this, SLOT(maybeRedrawSlot(bool)));

  redrawAct = new QAction(QIcon(""), tr("Redraw"), this);
  redrawAct->setShortcut(tr("Ctrl+R"));
  redrawAct->setStatusTip("Redraw");
  connect(redrawAct, SIGNAL(triggered()), this, SLOT(redrawSlot()));

  fitToWindowAct = new QAction(QIcon(""), tr("Fit to window"), this);
  fitToWindowAct->setStatusTip("Fit model to window");
  connect(fitToWindowAct, SIGNAL(triggered()), this, SLOT(fitToWindowSlot()));

  preferencesAct = new QAction(QIcon(""), tr("Preferences"), this);
  preferencesAct->setStatusTip("Show preferences");
  connect(preferencesAct, SIGNAL(triggered()), this, SLOT(showPreferencesDialogSlot()));

  // Edit menu:
  //------------
#ifdef MATC
  matcAct = new QAction(QIcon(""), tr("Matc..."), this);
  matcAct->setStatusTip("Matc window");
  connect(matcAct, SIGNAL(triggered()), this, SLOT(matcOpenSlot()));
#endif

  regenerateGridsAct = new QAction(QIcon(""), tr("Regenerate all..."), this);
  regenerateGridsAct->setStatusTip("Regerate all meshes");
  connect(regenerateGridsAct, SIGNAL(triggered()), this, SLOT(regenerateGridsSlot()));

  timeStepAct = new QAction(QIcon(""), tr("Time step control"), this);
  timeStepAct->setStatusTip("Time step control");
  connect(timeStepAct, SIGNAL(triggered()), this, SLOT(showTimeStepDialogSlot()));

}

void VtkPost::createMenus()
{
  // File menu:
  //-----------
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(savePictureAct);
  fileMenu->addSeparator();
  fileMenu->addAction(exitAct);

  // Edit menu:
  //-----------
  editMenu = menuBar()->addMenu(tr("&Edit"));
  editGroupsMenu = new QMenu(tr("Groups"));
  editMenu->addMenu(editGroupsMenu);
  editMenu->addSeparator();
  editMenu->addAction(timeStepAct);
#ifdef MATC
  editMenu->addSeparator();
  editMenu->addAction( matcAct );
#endif

  // View menu:
  //-----------
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawMeshPointAct);
  viewMenu->addAction(drawMeshEdgeAct);
  viewMenu->addAction(drawFeatureEdgesAct);
  viewMenu->addAction(drawAxesAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawSurfaceAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawIsoContourAct);
  viewMenu->addAction(drawIsoSurfaceAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawVectorAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawColorBarAct);
  viewMenu->addSeparator();
  viewMenu->addAction(drawStreamLineAct);
  viewMenu->addSeparator();
  viewMenu->addAction(preferencesAct);
  viewMenu->addSeparator();
  viewMenu->addAction(fitToWindowAct);
  viewMenu->addAction(redrawAct);
}

void VtkPost::createToolbars()
{
  viewToolBar = addToolBar(tr("View"));
  viewToolBar->addAction(drawSurfaceAct);
  viewToolBar->addAction(drawVectorAct);
  viewToolBar->addAction(drawIsoContourAct);
  viewToolBar->addAction(drawIsoSurfaceAct);
  viewToolBar->addAction(drawStreamLineAct);
  viewToolBar->addSeparator();
  viewToolBar->addAction(drawColorBarAct);
  viewToolBar->addSeparator();
  viewToolBar->addAction(preferencesAct);
  viewToolBar->addSeparator();
  viewToolBar->addAction(redrawAct);
}

void VtkPost::createStatusBar()
{
}

#ifdef MATC
void VtkPost::matcOpenSlot()
{
  matc->show();
}

void VtkPost::matcCutPasteSlot()
{
  matc->ui.mcHistory->copy();
  matc->ui.mcEdit->clear();
  matc->ui.mcEdit->paste();
}

void VtkPost::domatcSlot()
{
  matc->domatc(this);
  populateWidgetsSlot();
}
#endif

void VtkPost::minMax(ScalarField *sf)
{
   sf->minVal =  9e99;
   sf->maxVal = -9e99;
   for( int i=0; i<sf->values; i++ ) {
     if ( sf->minVal>sf->value[i] ) sf->minVal=sf->value[i];
     if ( sf->maxVal<sf->value[i] ) sf->maxVal=sf->value[i];
   }
}

// Populate widgets in user interface dialogs:
//----------------------------------------------------------------------
void VtkPost::populateWidgetsSlot()
{
  surface->populateWidgets(this);
  vector->populateWidgets(this);
  isoContour->populateWidgets(this);
  isoSurface->populateWidgets(this);
  streamLine->populateWidgets(this);
  colorBar->populateWidgets(this);
}

// Save picture:
//----------------------------------------------------------------------
void VtkPost::savePictureSlot()
{
  QString fileName = QFileDialog::getSaveFileName(this,
	 tr("Save picture"), "", tr("Picture files (*.png)"));
  
  if(fileName.isEmpty()) {
    cout << "File name is empty" << endl;
    return;
  }

  vtkWindowToImageFilter* image = vtkWindowToImageFilter::New();

  image->SetInput(qvtkWidget->GetRenderWindow());
  image->Update();

  vtkPNGWriter* writer = vtkPNGWriter::New();

  writer->SetInputConnection(image->GetOutputPort());
  writer->SetFileName(fileName.toAscii().data());

  qvtkWidget->GetRenderWindow()->Render();
  writer->Write();

  writer->Delete();
  image->Delete();
}

// Read in data:
//----------------------------------------------------------------------
bool VtkPost::readPostFile(QString postFileName)
{
  QString tmpLine;
  QTextStream txtStream;

#define GET_TXT_STREAM                               \
  tmpLine = post.readLine().trimmed();               \
  while(tmpLine.isEmpty() || (tmpLine.at(0) == '#')) \
    tmpLine = post.readLine().trimmed();             \
  txtStream.setString(&tmpLine);

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

  cout << "Ep file header says:" << endl;
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
#ifdef MATC
     QByteArray nm = sf->name.trimmed().toAscii();
     var_delete( nm.data() );
#else
     if(sf->value) free(sf->value);
#endif
  }

  scalarFields = 0;

  // Add the null field:
  //--------------------
  QString fieldName = "Null";
  ScalarField* nullField = addScalarField(fieldName, nodes*timesteps, NULL);
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
      addScalarField(fieldName, nodes*timesteps, NULL);

    if(fieldType == "vector") {
      addVectorField(fieldName, nodes*timesteps);
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

  // Add nodes to field variables:
  //-------------------------------
  addVectorField("nodes", nodes);
  int index = -1;
  for(int i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    if(sf->name == "nodes_x") {
      index = i;
      break;
    }
  }
  ScalarField *sfx = &scalarField[index+0];
  ScalarField *sfy = &scalarField[index+1];
  ScalarField *sfz = &scalarField[index+2];
  for( int i=0; i < nodes; i++ )
  {
    sfx->value[i] = epMesh->epNode[i].x[0];
    sfy->value[i] = epMesh->epNode[i].x[1];
    sfz->value[i] = epMesh->epNode[i].x[2];
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
    
    for(int j = 0; j < epe->indexes; j++) {
      QString tmpString = "";
      txtStream >> tmpString;
      if(tmpString.isEmpty()) {

	GET_TXT_STREAM

        txtStream >> tmpString;
      }
      epe->index[j] = tmpString.toInt();
    }
  }

  // Data:
  //=======
  ScalarField *sf;
  int i;
  for(i = 0; i < nodes * timesteps; i++) {

    if ( post.atEnd() ) break;
    GET_TXT_STREAM

    for(int j = 0; j < scalarFields-4; j++) { // - 4 = no nodes, no null field
      sf = &scalarField[j+1];                 // + 1 = skip null field
      txtStream >> sf->value[i];
    }
  }

  int real_timesteps = i/nodes;
  cout << real_timesteps << " timesteps read in." << endl;

  // Initial min & max values:
  //============================
  int ifield=0, size;
  while( ifield<scalarFields )
  {
    ScalarField *sf = &scalarField[ifield];

    int sf_timesteps = sf->values/nodes;
    if ( real_timesteps < sf_timesteps )
    {
      sf->values = real_timesteps*nodes;
#ifdef MATC
      QString name=sf->name;
      int n = sf->name.indexOf("_x");
      if ( n>0 ) 
      {
        name = sf->name.mid(0,n);
        size=3*sf->values;

        QString cmd = name+"="+name+"(0:2,0:"+QString::number(size-1)+")";
        mtc_domath(cmd.toAscii().data());
        VARIABLE *var = var_check(name.toAscii().data());

        sf = &scalarField[ifield];
        sf->value = &M(var,0,0);
        minMax(sf);

        ifield++;
        sf = &scalarField[ifield];
        sf->value = &M(var,1,0);
        sf->values = real_timesteps*nodes;
        minMax(sf);

        ifield++;
        sf = &scalarField[ifield];
        sf->value = &M(var,2,0);
        sf->values = real_timesteps*nodes;
        minMax(sf);
      } else {
        size=sf->values*sizeof(double);

        VARIABLE *var = var_check(name.toAscii().data());
        sf->value = (double *)ALLOC_PTR(realloc(
              ALLOC_LST(sf->value), ALLOC_SIZE(size)) );
        MATR(var) = sf->value;
        NCOL(var) = sf->values;
        minMax(sf);
      }
#else
      size = sf->values*sizeof(double);
      sf->value = (double *)realloc(sf->value,size);
      minMax(sf);
#endif
    } else {
      minMax(sf);
    }
    ifield++;
  }

  timesteps = real_timesteps;
  timeStep->maxSteps = timesteps;
  timeStep->ui.start->setValue(1);
  timeStep->ui.stop->setValue(timesteps);
  
  postFile.close();

  // Set up the group edit menu:
  //=============================
  groupActionHash.clear();
  editGroupsMenu->clear();

  for(int i = 0; i < elements; i++) {
    EpElement* epe = &epMesh->epElement[i];

    QString groupName = epe->groupName;
    
    if(groupActionHash.contains(groupName))
      continue;

    QAction* groupAction = new QAction(groupName, this);
    groupAction->setCheckable(true);
    groupAction->setChecked(true);
    editGroupsMenu->addAction(groupAction);
    groupActionHash.insert(groupName, groupAction);
  }

  // Populate the widgets in user interface dialogs:
  //-------------------------------------------------
  populateWidgetsSlot();

  this->postFileRead = true;

  groupChangedSlot(NULL);
  connect(editGroupsMenu, SIGNAL(triggered(QAction*)), this, SLOT(groupChangedSlot(QAction*)));
  
  editGroupsMenu->addSeparator();
  editGroupsMenu->addAction(regenerateGridsAct);

  // Set the null field active:
  //---------------------------
  drawSurfaceAct->setChecked(true);
  redrawSlot();

  renderer->ResetCamera();
  
  return true;
}

void VtkPost::addVectorField(QString fieldName, int values)
{
   
#ifdef MATC
    QByteArray nm=fieldName.trimmed().toAscii();

    char *name = (char *)malloc( nm.count()+1 );
    strcpy(name,nm.data());

    VARIABLE *var = var_check(name);
    if ( !var || NROW(var) != 3 || NCOL(var) != values )
      var = var_new( name, TYPE_DOUBLE, 3, values );
    free(name);

   addScalarField(fieldName+"_x", values, &M(var,0,0));
   addScalarField(fieldName+"_y", values, &M(var,1,0));
   addScalarField(fieldName+"_z", values, &M(var,2,0));
#else
   addScalarField(fieldName+"_x", values, NULL);
   addScalarField(fieldName+"_y", values, NULL);
   addScalarField(fieldName+"_z", values, NULL);
#endif
}

// Add a scalar field:
//----------------------------------------------------------------------
ScalarField* VtkPost::addScalarField(QString fieldName, int values, double *value)
{
  if(scalarFields >= MAX_SCALARS) {
    cout << "Max. scalar limit exceeded!" << endl;
    return NULL;
  }

  ScalarField *sf = &scalarField[scalarFields++];
  sf->name = fieldName;
  sf->values = values;
  sf->value = value;
 
  if ( !sf->value ) {
#ifdef MATC
    QByteArray nm=fieldName.trimmed().toAscii();

    char *name = (char *)malloc( nm.count()+1 );
    strcpy(name,nm.data());
    VARIABLE *var = var_check(name);
    if ( !var || NROW(var)!=1 || NCOL(var) != values )
      var = var_new( name, TYPE_DOUBLE, 1, values );
    sf->value = MATR(var);
    free(name);
#else
    sf->value = (double *)calloc(values,sizeof(double));
#endif
  }

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
void VtkPost::regenerateGridsSlot()
{
  groupChangedSlot(NULL);
}

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
  int index = -1;
  for(int i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    if(sf->name == "nodes_x") {
      index = i;
      break;
    }
  }
  
  if((index < 0) || (index + 2 > scalarFields - 1)) return;

  double x[3];
  ScalarField *sfx = &scalarField[index+0];
  ScalarField *sfy = &scalarField[index+1];
  ScalarField *sfz = &scalarField[index+2];
  
  vtkPoints *points = vtkPoints::New();
  points->SetNumberOfPoints(epMesh->epNodes);

  for(int i = 0; i < epMesh->epNodes; i++) {
    x[0] = sfx->value[i];
    x[1] = sfy->value[i];
    x[2] = sfz->value[i];
    points->InsertPoint(i, x);
  }
  volumeGrid->SetPoints(points);
  surfaceGrid->SetPoints(points);
  lineGrid->SetPoints(points);
  points->Delete();

  // Volume grid:
  //---------------
  vtkTetra *tetra = vtkTetra::New();
  vtkHexahedron *hexa = vtkHexahedron::New();

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
    
    if(epe->code == 808) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;
      
      QAction *groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 8; j++)
	hexa->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	volumeGrid->InsertNextCell(hexa->GetCellType(), hexa->GetPointIds());
    }
  }
  tetra->Delete();
  hexa->Delete();

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

// Show preferences dialog:
//----------------------------------------------------------------------
void VtkPost::showPreferencesDialogSlot()
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
#ifdef MATC
   VARIABLE *tvar = var_check((char *)"t");
   if (!tvar) tvar=var_new((char *)"t", TYPE_DOUBLE,1,1 );
   M(tvar,0,0) = (double)timeStep->ui.timeStep->value();

   QString dosome = timeStep->ui.doBefore->text();
   matc->ui.mcEdit->clear();
   matc->ui.mcEdit->insert(dosome);
   matc->domatc(this);
#endif

  drawMeshPointSlot();
  drawMeshEdgeSlot();
  drawFeatureEdgesSlot();
  drawSurfaceSlot();
  drawVectorSlot();
  drawIsoContourSlot();
  drawIsoSurfaceSlot();
  drawStreamLineSlot();
  drawColorBarSlot();
  drawAxesSlot();

  vtkRenderWindow *renderWindow = qvtkWidget->GetRenderWindow();
  renderWindow->Render();

  // Check if the "Stop" button of time stepping loop has been pressed:
  QCoreApplication::processEvents();

  emit(canProceedWithNextSignal(renderWindow));
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

void VtkPost::hideColorBarSlot()
{
  drawColorBarAct->setChecked(false);
  drawColorBarSlot();
}

void VtkPost::drawColorBarSlot()
{
  renderer->RemoveActor(colorBarActor);
  if(!drawColorBarAct->isChecked()) return;
  colorBar->draw(this);
  renderer->AddActor(colorBarActor);
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw mesh points:
//----------------------------------------------------------------------
void VtkPost::drawMeshPointSlot()
{
  renderer->RemoveActor(meshPointActor);
  if(!drawMeshPointAct->isChecked()) return;
  meshPoint->draw(this, preferences);
  renderer->AddActor(meshPointActor);
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw mesh edges:
//----------------------------------------------------------------------
void VtkPost::drawMeshEdgeSlot()
{
  renderer->RemoveActor(meshEdgeActor);
  if(!drawMeshEdgeAct->isChecked()) return;
  meshEdge->draw(this, preferences);
  renderer->AddActor(meshEdgeActor);
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw feature edges:
//----------------------------------------------------------------------
void VtkPost::drawFeatureEdgesSlot()
{
  renderer->RemoveActor(featureEdgeActor);
  if(!drawFeatureEdgesAct->isChecked()) return;
  featureEdge->draw(this, preferences);
  renderer->AddActor(featureEdgeActor);
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw stream lines:
//----------------------------------------------------------------------
void VtkPost::showStreamLineDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();
  
  if(drawStreamLineAct->isChecked()) {
    streamLine->show();
  } else {
    streamLine->close();
    drawStreamLineSlot();
  }
}

void VtkPost::hideStreamLineSlot()
{
  drawStreamLineAct->setChecked(false);
  drawStreamLineSlot();
}

void VtkPost::drawStreamLineSlot()
{
  renderer->RemoveActor(streamLineActor);
  if(!drawStreamLineAct->isChecked()) return;
  streamLine->draw(this, timeStep);
  renderer->AddActor(streamLineActor);
  drawColorBarSlot();
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw vectors:
//----------------------------------------------------------------------
void VtkPost::showVectorDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();

  if(drawVectorAct->isChecked()) {
    vector->show();
  } else {
    vector->close();
    drawVectorSlot();
  }
}

void VtkPost::hideVectorSlot()
{
  drawVectorAct->setChecked(false);
  drawVectorSlot();
}

void VtkPost::drawVectorSlot()
{
  renderer->RemoveActor(vectorActor);
  if(!drawVectorAct->isChecked()) return;
  vector->draw(this, timeStep);
  renderer->AddActor(vectorActor);
  drawColorBarSlot();
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw surfaces:
//----------------------------------------------------------------------
void VtkPost::showSurfaceDialogSlot()
{
  qvtkWidget->GetRenderWindow()->Render();

  if(drawSurfaceAct->isChecked()) {
    surface->show();
  } else {
    surface->close();
    drawSurfaceSlot();
  }
}

void VtkPost::hideSurfaceSlot()
{
  drawSurfaceAct->setChecked(false);
  drawSurfaceSlot();
}

void VtkPost::drawSurfaceSlot()
{
  renderer->RemoveActor(surfaceActor);
  if(!drawSurfaceAct->isChecked()) return;
  surface->draw(this, timeStep);
  renderer->AddActor(surfaceActor);
  drawColorBarSlot();
  qvtkWidget->GetRenderWindow()->Render();
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

void VtkPost::hideIsoContourSlot()
{
  drawIsoContourAct->setChecked(false);
  drawIsoContourSlot();
}

void VtkPost::drawIsoContourSlot()
{
  renderer->RemoveActor(isoContourActor);
  if(!drawIsoContourAct->isChecked()) return;
  isoContour->draw(this, timeStep);
  renderer->AddActor(isoContourActor);
  drawColorBarSlot();  
  qvtkWidget->GetRenderWindow()->Render();
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

void VtkPost::hideIsoSurfaceSlot()
{
  drawIsoSurfaceAct->setChecked(false);
  drawIsoSurfaceSlot();
}

void VtkPost::drawIsoSurfaceSlot()
{
  renderer->RemoveActor(isoSurfaceActor);
  if(!drawIsoSurfaceAct->isChecked()) return;
  isoSurface->draw(this, timeStep);
  renderer->AddActor(isoSurfaceActor);
  drawColorBarSlot();
  qvtkWidget->GetRenderWindow()->Render();
}

// Draw axes:
//----------------------------------------------------------------------
void VtkPost::drawAxesSlot()
{
  renderer->RemoveActor(axesActor);
  renderer->RemoveActor(axesXTextActor);
  renderer->RemoveActor(axesYTextActor);
  renderer->RemoveActor(axesZTextActor);
  if(!drawAxesAct->isChecked()) return;
  axes->draw(this);
  renderer->AddActor(axesActor);
  renderer->AddActor(axesXTextActor);
  renderer->AddActor(axesYTextActor);
  renderer->AddActor(axesZTextActor);
  qvtkWidget->GetRenderWindow()->Render();
}


// Time step control:
//----------------------------------------------------------------------
void VtkPost::showTimeStepDialogSlot()
{
  timeStep->show();
}

void VtkPost::timeStepChangedSlot()
{
  redrawSlot();
}

// Fit to window:
//----------------------------------------------------------------------
void VtkPost::fitToWindowSlot()
{
  renderer->ResetCamera();  
}

// Other public methods:
//----------------------------------------------------------------------
QVTKWidget* VtkPost::GetQVTKWidget()
{
  return qvtkWidget;
}

vtkRenderer* VtkPost::GetRenderer()
{
  return renderer;
}

vtkActor* VtkPost::GetSurfaceActor()
{
  return surfaceActor;
}

vtkActor* VtkPost::GetVectorActor()
{
  return vectorActor;
}

vtkActor* VtkPost::GetIsoContourActor()
{
  return isoContourActor;
}

vtkActor* VtkPost::GetIsoSurfaceActor()
{
  return isoSurfaceActor;
}

vtkActor* VtkPost::GetStreamLineActor()
{
  return streamLineActor;
}

vtkScalarBarActor* VtkPost::GetColorBarActor()
{
  return colorBarActor;
}

vtkActor* VtkPost::GetPickedPointActor()
{
  return pickedPointActor;
}

vtkActor* VtkPost::GetAxesActor()
{
  return axesActor;
}

vtkActor* VtkPost::GetFeatureEdgeActor()
{
  return featureEdgeActor;
}

vtkActor* VtkPost::GetMeshPointActor()
{
  return meshPointActor;
}

vtkActor* VtkPost::GetMeshEdgeActor()
{
  return meshEdgeActor;
}

vtkFollower* VtkPost::GetAxesXTextActor()
{
  return axesXTextActor;
}

vtkFollower* VtkPost::GetAxesYTextActor()
{
  return axesYTextActor;
}

vtkFollower* VtkPost::GetAxesZTextActor()
{
  return axesZTextActor;
}

double VtkPost::GetLength()
{
  double volumeLength = volumeGrid->GetLength();
  double surfaceLength = surfaceGrid->GetLength();
  double lineLength = lineGrid->GetLength();

  double length = volumeLength;
  if(surfaceLength > length) length = surfaceLength;
  if(lineLength > length) length = lineLength;

  return length;
}

vtkUnstructuredGrid* VtkPost::GetLineGrid()
{
  return lineGrid;
}

vtkUnstructuredGrid* VtkPost::GetSurfaceGrid()
{
  return surfaceGrid;
}

vtkUnstructuredGrid* VtkPost::GetVolumeGrid()
{
  return volumeGrid;
}

vtkPlane* VtkPost::GetClipPlane()
{
  double px = preferences->ui.clipPointX->text().toDouble();
  double py = preferences->ui.clipPointY->text().toDouble();
  double pz = preferences->ui.clipPointZ->text().toDouble();
  double nx = preferences->ui.clipNormalX->text().toDouble();
  double ny = preferences->ui.clipNormalY->text().toDouble();
  double nz = preferences->ui.clipNormalZ->text().toDouble();

  this->clipPlane->SetOrigin(px, py, pz);
  this->clipPlane->SetNormal(nx, ny, nz);

  return clipPlane;
}

vtkLookupTable* VtkPost::GetCurrentLut()
{
  return currentLut;
}

QString VtkPost::GetCurrentSurfaceName()
{
  return currentSurfaceName;
}

QString VtkPost::GetCurrentVectorName()
{
  return currentVectorName;
}

QString VtkPost::GetCurrentIsoContourName()
{
  return currentIsoContourName;
}

QString VtkPost::GetCurrentIsoSurfaceName()
{
  return currentIsoSurfaceName;
}

QString VtkPost::GetCurrentStreamLineName()
{
  return currentStreamLineName;
}

void VtkPost::SetCurrentSurfaceName(QString name)
{
  currentSurfaceName = name;
}

void VtkPost::SetCurrentVectorName(QString name)
{
  currentVectorName = name;
}

void VtkPost::SetCurrentIsoContourName(QString name)
{
  currentIsoContourName = name;
}

void VtkPost::SetCurrentIsoSurfaceName(QString name)
{
  currentIsoSurfaceName = name;
}

void VtkPost::SetCurrentStreamLineName(QString name)
{
  currentStreamLineName = name;
}

int VtkPost::GetScalarFields()
{
  return scalarFields;
}

void VtkPost::SetScalarFields(int n)
{
  scalarFields = n;
}

ScalarField* VtkPost::GetScalarField()
{
  return scalarField;
}

EpMesh* VtkPost::GetEpMesh()
{
  return epMesh;
}

double* VtkPost::GetCurrentPickPosition()
{
  return &currentPickPosition[0];
}

void VtkPost::SetCurrentPickPosition(double *p)
{
  currentPickPosition[0] = p[0];
  currentPickPosition[1] = p[1];
  currentPickPosition[2] = p[2];
}

int VtkPost::NofNodes()
{
   return epMesh->epNodes;
}

QSize VtkPost::minimumSizeHint() const
{
  return QSize(64, 64);
}

QSize VtkPost::sizeHint() const
{
  return QSize(640, 480);
}
