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
#include "readepfile.h"
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
#include <vtkPropPicker.h>
#include <vtkCallbackCommand.h>
#include <vtkAbstractPicker.h>
#include <vtkObject.h>
#include <vtkCommand.h>
#include <vtkFollower.h>
#include <vtkImplicitPlaneWidget.h>

using namespace std;

// Interaction event handler (press 'i' to interact):
//-------------------------------------------------------------------
static void iEventHandler(vtkObject* caller, unsigned long eid, 
			  void* clientdata, void* calldata)
{
  VtkPost* vtkPost = reinterpret_cast<VtkPost*>(clientdata);
  vtkImplicitPlaneWidget* planeWidget = vtkPost->GetPlaneWidget();

  vtkPost->SetClipPlaneOrigin(planeWidget->GetOrigin());
  vtkPost->SetClipPlaneNormal(planeWidget->GetNormal());
}

// Pick event handler (press 'p' to pick):
//-------------------------------------------------------------------
static void pEventHandler(vtkObject* caller, unsigned long eid, 
			  void* clientdata, void* calldata)
{
  VtkPost* vtkPost = reinterpret_cast<VtkPost*>(clientdata);
  vtkRenderer* renderer = vtkPost->GetRenderer();
  vtkActor* pickedPointActor = vtkPost->GetPickedPointActor();
  QVTKWidget* qvtkWidget = vtkPost->GetQVTKWidget();
  vtkAbstractPicker* picker = qvtkWidget->GetInteractor()->GetPicker();
  vtkPropPicker* propPicker = vtkPropPicker::SafeDownCast(picker);

  vtkActor* actor = propPicker->GetActor();
  double* pickPos = vtkPost->GetCurrentPickPosition();
  propPicker->GetPickPosition(pickPos);
  vtkPost->SetCurrentPickPosition(pickPos);

  if(!actor) {
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

  readEpFile = new ReadEpFile(this);
  connect(readEpFile, SIGNAL(readPostFileSignal(QString)), this, SLOT(ReadPostFile(QString)));

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
  vtkPropPicker* propPicker = vtkPropPicker::New();
  qvtkWidget->GetInteractor()->SetPicker(propPicker);
  propPicker->Delete();

  vtkCallbackCommand* cbcPick = vtkCallbackCommand::New();
  cbcPick->SetClientData(this);
  cbcPick->SetCallback(pEventHandler);

  vtkAbstractPicker* picker = qvtkWidget->GetInteractor()->GetPicker();
  picker->AddObserver(vtkCommand::EndPickEvent, cbcPick);
  cbcPick->Delete();

  // Create the clip plane & implicit plane widget:
  //------------------------------------------------
  clipPlane = vtkPlane::New();

  vtkCallbackCommand* cbcPlane = vtkCallbackCommand::New();
  cbcPlane->SetClientData(this);
  cbcPlane->SetCallback(iEventHandler);

  planeWidget = vtkImplicitPlaneWidget::New();
  planeWidget->SetInteractor(qvtkWidget->GetInteractor());
  planeWidget->AddObserver(vtkCommand::InteractionEvent, cbcPlane);
  cbcPlane->Delete();

  SetClipPlaneOrigin(planeWidget->GetOrigin());
  SetClipPlaneNormal(planeWidget->GetNormal());

  // Python bindings:
  //-----------------
#ifdef PYTHONQT
  PythonQt::init(PythonQt::IgnoreSiteModule | PythonQt::RedirectStdOut);
  mainContext = PythonQt::self()->getMainModule();
  mainContext.addObject("egp", this);
  console = new PythonQtScriptingConsole(NULL, mainContext);
  console->setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  console->setWindowTitle("ElmerGUI PythonQt");
  console->resize(400, 300);
#endif
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

  reloadPostAct = new QAction(QIcon(""), tr("Reload"), this);
  reloadPostAct->setStatusTip("Reloads input file");
  connect(reloadPostAct, SIGNAL(triggered()), this, SLOT(reloadPostSlot()));

  readEpFileAct = new QAction(QIcon(":/icons/document-open.png"), tr("Open..."), this);
  readEpFileAct->setShortcut(tr("Ctrl+O"));
  readEpFileAct->setStatusTip("Read input file");
  connect(readEpFileAct, SIGNAL(triggered()), this, SLOT(readEpFileSlot()));

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

  clipAllAct = new QAction(QIcon(""), tr("Clip all"), this);
  clipAllAct->setShortcut(tr("Ctrl+C"));
  clipAllAct->setStatusTip("Apply clip plane to all actors");
  clipAllAct->setCheckable(true);
  clipAllAct->setChecked(false);
  connect(clipAllAct, SIGNAL(toggled(bool)), this, SLOT(clipAllToggledSlot(bool)));

  resetModelViewAct = new QAction(QIcon(""), tr("Reset model view"), this);
  resetModelViewAct->setStatusTip("Reset model view");
  connect(resetModelViewAct, SIGNAL(triggered()), this, SLOT(resetModelViewSlot()));

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

#ifdef PYTHONQT
  showPythonQtConsoleAct = new QAction(QIcon(""), tr("PythonQt console..."), this);
  showPythonQtConsoleAct->setStatusTip("Show/hide PythonQt console");
  connect(showPythonQtConsoleAct, SIGNAL(triggered()), this, SLOT(showPythonQtConsoleSlot()));
#endif

  // Help menu:
  //-----------
  showHelpAct = new QAction(QIcon(":/icons/help-about.png"), tr("Help..."), this);
  showHelpAct->setStatusTip("Show help dialog");
  connect(showHelpAct, SIGNAL(triggered()), this, SLOT(showHelpSlot()));
}

void VtkPost::createMenus()
{
  // File menu:
  //-----------
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(readEpFileAct);
  fileMenu->addAction(reloadPostAct);
  fileMenu->addSeparator();
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

#ifdef PYTHONQT
  editMenu->addSeparator();
  editMenu->addAction(showPythonQtConsoleAct);
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
  viewMenu->addAction(clipAllAct);
  viewMenu->addSeparator();
  viewMenu->addAction(fitToWindowAct);
  viewMenu->addAction(resetModelViewAct);
  viewMenu->addAction(redrawAct);

  // Help menu:
  //-----------
  helpMenu = menuBar()->addMenu(tr("&Help"));
  helpMenu->addAction(showHelpAct);
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

#ifdef PYTHONQT
void VtkPost::showPythonQtConsoleSlot()
{
  console->show();
}
#endif

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

// Read input file (dialog):
//----------------------------------------------------------------------
void VtkPost::readEpFileSlot()
{
  readEpFile->show();
}

// Reload results:
//----------------------------------------------------------------------
void VtkPost::reloadPostSlot()
{
  if(postFileName.isEmpty()) {
    cout << "Unable to open ep-file. File name is empty." << endl;
    return;
  }

  bool surfaceVisible = drawSurfaceAct->isChecked();

  if(!ReadPostFile(postFileName))
    cout << "Reloading results from current ep-file failed." << endl;

  drawSurfaceAct->setChecked(surfaceVisible);
  
  redrawSlot();
}

// Get one line from post text stream:
//----------------------------------------------------------------------
void VtkPost::getPostLineStream(QTextStream* postStream)
{
  // postLine and postLineStream are private for VtkPost
  postLine = postStream->readLine().trimmed();
  while(postLine.isEmpty() || (postLine.at(0) == '#'))
    postLine = postStream->readLine().trimmed();
  postLineStream.setString(&postLine);
}

// Read in data:
//----------------------------------------------------------------------
bool VtkPost::ReadPostFile(QString postFileName)
{
  // Open the post file:
  //=====================
  this->postFileName = postFileName;
  this->postFileRead = false;

  QFile postFile(postFileName);

  if(!postFile.open(QIODevice::ReadOnly | QIODevice::Text))
    return false;

  cout << "Loading ep-file" << endl;

  readEpFile->ui.applyButton->setEnabled(false);
  readEpFile->ui.cancelButton->setEnabled(false);
  readEpFile->ui.okButton->setEnabled(false);
  readEpFile->setWindowTitle("Reading...");
  readEpFile->repaint();
  
  QTextStream postStream(&postFile);

  // Read in nodes, elements, timesteps, and scalar components:
  //-----------------------------------------------------------
  int nodes, elements, timesteps, components;

  getPostLineStream(&postStream);

  postLineStream >> nodes >> elements >> components >> timesteps;

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
    postLineStream >> fieldType >> fieldName;

    fieldType.replace(":", "");
    fieldType = fieldType.trimmed();
    fieldName = fieldName.trimmed();

    cout << fieldType.toAscii().data() << ": ";
    cout << fieldName.toAscii().data() << endl;

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

    getPostLineStream(&postStream);

    for(int j = 0; j < 3; j++) 
      postLineStream >> epn->x[j];
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

    getPostLineStream(&postStream);    

    postLineStream >> epe->groupName >> epe->code;
    
    epe->indexes = epe->code % 100;
    epe->index = new int[epe->indexes];
    
    for(int j = 0; j < epe->indexes; j++) {
      QString tmpString = "";
      postLineStream >> tmpString;
      if(tmpString.isEmpty()) {
	getPostLineStream(&postStream);
        postLineStream >> tmpString;
      }
      epe->index[j] = tmpString.toInt();
    }
  }

  // Data:
  //=======
  int start = readEpFile->ui.start->value() - 1;
  int end = readEpFile->ui.end->value() - 1;

  // skip values before start:
  for(int i = 0; i < nodes * start; i++) {
    if(postStream.atEnd()) break;
    getPostLineStream(&postStream);
  }

  ScalarField *sf;
  int i;
  for(i = 0; i < nodes * (end - start + 1); i++) {
    if(postStream.atEnd()) break;
    getPostLineStream(&postStream);

    for(int j = 0; j < scalarFields-4; j++) { // - 4 = no nodes, no null field
      sf = &scalarField[j+1];                 // + 1 = skip null field
      postLineStream >> sf->value[i];
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

        QString cmd = name+"="+name+"(0:2,0:"+QString::number(sf->values-1)+")";
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

  renderer->ResetCamera();
  
  readEpFile->ui.fileName->setText(postFileName);
  readEpFile->readHeader();
  readEpFile->ui.applyButton->setEnabled(true);
  readEpFile->ui.cancelButton->setEnabled(true);
  readEpFile->ui.okButton->setEnabled(true);
  readEpFile->setWindowTitle("Read input file");
  readEpFile->repaint();

  redrawSlot();

  renderer->GetActiveCamera()->GetPosition(initialCameraPosition);
  initialCameraRoll = renderer->GetActiveCamera()->GetRoll();

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

void VtkPost::groupChangedSlot(QAction* groupAction)
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
    ScalarField* sf = &scalarField[i];
    if(sf->name == "nodes_x") {
      index = i;
      break;
    }
  }
  
  if((index < 0) || (index + 2 > scalarFields - 1)) return;

  double x[3];
  ScalarField* sfx = &scalarField[index+0];
  ScalarField* sfy = &scalarField[index+1];
  ScalarField* sfz = &scalarField[index+2];
  
  vtkPoints* points = vtkPoints::New();
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
  vtkTetra* tetra = vtkTetra::New();
  vtkHexahedron* hexa = vtkHexahedron::New();

  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement* epe = &epMesh->epElement[i];

    if(epe->code == 504) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction* groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 4; j++)
	tetra->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	volumeGrid->InsertNextCell(tetra->GetCellType(), tetra->GetPointIds());
    }
    
    if(epe->code == 808) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;
      
      QAction* groupAction = groupActionHash.value(groupName);
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
  vtkTriangle* tria = vtkTriangle::New();
  vtkQuad* quad = vtkQuad::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement* epe = &epMesh->epElement[i];

    if(epe->code == 303) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction* groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 3; j++)
	tria->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	surfaceGrid->InsertNextCell(tria->GetCellType(), tria->GetPointIds());
    }

    if(epe->code == 404) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction* groupAction = groupActionHash.value(groupName);
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
  vtkLine* line = vtkLine::New();
  for(int i = 0; i < epMesh->epElements; i++) {
    EpElement* epe = &epMesh->epElement[i];

    if(epe->code == 202) {
      QString groupName = epe->groupName;
      if(groupName.isEmpty()) continue;

      QAction* groupAction = groupActionHash.value(groupName);
      if(groupAction == NULL) continue;
      
      for(int j = 0; j < 3; j++)
	line->GetPointIds()->SetId(j, epe->index[j]);
      
      if(groupAction->isChecked())
	lineGrid->InsertNextCell(line->GetCellType(), line->GetPointIds());
    }
  }
  line->Delete();

  if(timeStep->ui.regenerateBeforeDrawing->isChecked()) return;

  redrawSlot();

  // Place the implicit plane widget:
  //---------------------------------
  double bounds[6];
  GetBounds(bounds);

  double origin[3];
  origin[0] = (bounds[0] + bounds[1]) / 2.0;
  origin[1] = (bounds[2] + bounds[3]) / 2.0;
  origin[2] = (bounds[4] + bounds[5]) / 2.0;

  planeWidget->SetPlaceFactor(1.5);
  planeWidget->PlaceWidget(bounds);
  planeWidget->SetOrigin(origin);
  planeWidget->GetEdgesProperty()->SetColor(0, 0, 0);
  planeWidget->GetPlaneProperty()->SetColor(1, 0, 0);
  planeWidget->GetPlaneProperty()->SetOpacity(0.2);
  planeWidget->GetSelectedPlaneProperty()->SetColor(0, 1, 0);
  planeWidget->GetSelectedPlaneProperty()->SetOpacity(0.1);

  SetClipPlaneOrigin(planeWidget->GetOrigin());
  SetClipPlaneNormal(planeWidget->GetNormal());
}

// Show preferences dialog:
//----------------------------------------------------------------------
void VtkPost::showPreferencesDialogSlot()
{
  if(!postFileRead) return;
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
  if(!postFileRead) return;

#ifdef MATC
   VARIABLE *tvar = var_check((char *)"t");
   if (!tvar) tvar=var_new((char *)"t", TYPE_DOUBLE,1,1 );
   M(tvar,0,0) = (double)timeStep->ui.timeStep->value();

   QString dosome = timeStep->ui.doBefore->text();
   matc->ui.mcEdit->clear();
   matc->ui.mcEdit->insert(dosome);
   matc->domatc(this);
#endif

   if(timeStep->ui.regenerateBeforeDrawing->isChecked())
     regenerateGridsSlot();

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

  planeWidget->GetEdgesProperty()->SetColor(0, 0, 0);
  planeWidget->GetOutlineProperty()->SetColor(0, 0, 0);
  planeWidget->GetNormalProperty()->SetColor(1, 0, 0);  
}

// Draw color bar:
//----------------------------------------------------------------------
void VtkPost::showColorBarDialogSlot()
{
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
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
  if(!postFileRead) return;
  renderer->ResetCamera();  
}

// Reset model view:
//----------------------------------------------------------------------
void VtkPost::resetModelViewSlot()
{
  if(!postFileRead) return;
  SetInitialCameraPosition();
}

// Clip all -action toggled:
//----------------------------------------------------------------------
void VtkPost::clipAllToggledSlot(bool)
{
  redrawSlot();
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

void VtkPost::GetBounds(double* bounds)
{
  volumeGrid->GetBounds(bounds);
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

vtkImplicitPlaneWidget* VtkPost::GetPlaneWidget()
{
  return planeWidget;
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

void VtkPost::SetClipPlaneOrigin(double* origin)
{
  clipPlane->SetOrigin(origin);
  preferences->ui.clipPointX->setText(QString::number(origin[0]));
  preferences->ui.clipPointY->setText(QString::number(origin[1]));
  preferences->ui.clipPointZ->setText(QString::number(origin[2]));
}

void VtkPost::SetClipPlaneNormal(double* normal)
{
  clipPlane->SetNormal(normal);
  preferences->ui.clipNormalX->setText(QString::number(normal[0]));
  preferences->ui.clipNormalY->setText(QString::number(normal[1]));
  preferences->ui.clipNormalZ->setText(QString::number(normal[2]));
}

void VtkPost::SetClipPlaneOx(double x)
{
  preferences->ui.clipPointX->setText(QString::number(x));
  GetClipPlane();
}

void VtkPost::SetClipPlaneOy(double y)
{
  preferences->ui.clipPointY->setText(QString::number(y));
  GetClipPlane();
}

void VtkPost::SetClipPlaneOz(double z)
{
  preferences->ui.clipPointZ->setText(QString::number(z));  
  GetClipPlane();
}

void VtkPost::SetClipPlaneNx(double x)
{
  preferences->ui.clipNormalX->setText(QString::number(x));  
  GetClipPlane();
}

void VtkPost::SetClipPlaneNy(double y)
{
  preferences->ui.clipNormalY->setText(QString::number(y));
  GetClipPlane();
}

void VtkPost::SetClipPlaneNz(double z)
{
  preferences->ui.clipNormalZ->setText(QString::number(z));  
  GetClipPlane();
}

bool VtkPost::GetClipAll()
{
  return clipAllAct->isChecked();
}

void VtkPost::SetClipAll(bool clip)
{
  clipAllAct->setChecked(clip);
}

vtkLookupTable* VtkPost::GetCurrentLut()
{
  return currentLut;
}

QString VtkPost::GetCurrentSurfaceName()
{
  return surface->ui.surfaceCombo->currentText();
}

QString VtkPost::GetCurrentVectorName()
{
  return vector->ui.vectorCombo->currentText();
}

QString VtkPost::GetCurrentVectorColorName()
{
  return vector->ui.colorCombo->currentText();
}

QString VtkPost::GetCurrentIsoContourName()
{
  return isoContour->ui.contoursCombo->currentText();
}

QString VtkPost::GetCurrentIsoContourColorName()
{
  return isoContour->ui.colorCombo->currentText();
}

QString VtkPost::GetCurrentIsoSurfaceName()
{
  return isoSurface->ui.contoursCombo->currentText();
}

QString VtkPost::GetCurrentIsoSurfaceColorName()
{
  return isoSurface->ui.colorCombo->currentText();
}

QString VtkPost::GetCurrentStreamLineName()
{
  return streamLine->ui.vectorCombo->currentText();
}

QString VtkPost::GetCurrentStreamLineColorName()
{
  return streamLine->ui.colorCombo->currentText();
}

bool VtkPost::SetCurrentSurfaceName(QString name)
{
  for(int i = 0; i < surface->ui.surfaceCombo->count(); i++) {
    if(surface->ui.surfaceCombo->itemText(i) == name) {
      surface->ui.surfaceCombo->setCurrentIndex(i);
      currentSurfaceName = name;
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentVectorName(QString name)
{
  for(int i = 0; i < vector->ui.vectorCombo->count(); i++) {
    if(vector->ui.vectorCombo->itemText(i) == name) {
      vector->ui.vectorCombo->setCurrentIndex(i);
      currentVectorName = name;
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentVectorColorName(QString name)
{
  for(int i = 0; i < vector->ui.colorCombo->count(); i++) {
    if(vector->ui.colorCombo->itemText(i) == name) {
      vector->ui.colorCombo->setCurrentIndex(i);
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentIsoContourName(QString name)
{
  for(int i = 0; i < isoContour->ui.contoursCombo->count(); i++) {
    if(isoContour->ui.contoursCombo->itemText(i) == name) {
      isoContour->ui.contoursCombo->setCurrentIndex(i);
      currentIsoContourName = name;
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentIsoContourColorName(QString name)
{
  for(int i = 0; i < isoContour->ui.colorCombo->count(); i++) {
    if(isoContour->ui.colorCombo->itemText(i) == name) {
      isoContour->ui.colorCombo->setCurrentIndex(i);
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentIsoSurfaceName(QString name)
{
  for(int i = 0; i < isoSurface->ui.contoursCombo->count(); i++) {
    if(isoSurface->ui.contoursCombo->itemText(i) == name) {
      isoSurface->ui.contoursCombo->setCurrentIndex(i);
      currentIsoSurfaceName = name;
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentIsoSurfaceColorName(QString name)
{
  for(int i = 0; i < isoSurface->ui.colorCombo->count(); i++) {
    if(isoSurface->ui.colorCombo->itemText(i) == name) {
      isoSurface->ui.colorCombo->setCurrentIndex(i);
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentStreamLineName(QString name)
{
  for(int i = 0; i < streamLine->ui.vectorCombo->count(); i++) {
    if(streamLine->ui.vectorCombo->itemText(i) == name) {
      streamLine->ui.vectorCombo->setCurrentIndex(i);
      currentStreamLineName = name;
      return true;
    }
  }
  return false;
}

bool VtkPost::SetCurrentStreamLineColorName(QString name)
{
  for(int i = 0; i < streamLine->ui.colorCombo->count(); i++) {
    if(streamLine->ui.colorCombo->itemText(i) == name) {
      streamLine->ui.colorCombo->setCurrentIndex(i);
      return true;
    }
  }
  return false;
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

Preferences* VtkPost::GetPreferences()
{
  return preferences;
}

QSize VtkPost::minimumSizeHint() const
{
  return QSize(64, 64);
}

QSize VtkPost::sizeHint() const
{
  return QSize(640, 480);
}

void VtkPost::showHelpSlot()
{
  QMessageBox::about(this, tr("ElmerGUI postprocessor (beta)"),
		     tr("Press 'p' to pick a point\n"
                        "Press 'i' to show/hide the interactive plane widget\n"
			"Press 'w' to show the results in wireframe mode\n"
			"Press 's' to show the results in surface mode"));
}

void VtkPost::SetSurfaces(bool b)
{
  drawSurfaceAct->setChecked(b);
  drawSurfaceSlot();
}

void VtkPost::SetVectors(bool b)
{
  drawVectorAct->setChecked(b);
  drawVectorSlot();
}

void VtkPost::SetIsoContours(bool b)
{
  drawIsoContourAct->setChecked(b);
  drawIsoContourSlot();
}

void VtkPost::SetIsoSurfaces(bool b)
{
  drawIsoSurfaceAct->setChecked(b);
  drawIsoSurfaceSlot();
}

void VtkPost::SetStreamLines(bool b)
{
  drawStreamLineAct->setChecked(b);
  drawStreamLineSlot();
}

void VtkPost::SetColorBar(bool b)
{
  drawColorBarAct->setChecked(b);
  drawColorBarSlot();
}

void VtkPost::SetMeshPoints(bool b)
{
  drawMeshPointAct->setChecked(b);
  drawMeshPointSlot();
}

void VtkPost::SetMeshEdges(bool b)
{
  drawMeshEdgeAct->setChecked(b);
  drawMeshEdgeSlot();
}

void VtkPost::SetFeatureEdges(bool b)
{
  drawFeatureEdgesAct->setChecked(b);
  drawFeatureEdgesSlot();
}

void VtkPost::SetAxes(bool b)
{
  drawAxesAct->setChecked(b);
  drawAxesSlot();
  qvtkWidget->GetRenderWindow()->Render();
}

void VtkPost::SetPostFileStart(int n)
{
  readEpFile->ui.start->setValue(n);
}

void VtkPost::SetPostFileEnd(int n)
{
  readEpFile->ui.end->setValue(n);
}

void VtkPost::Redraw()
{
  this->redrawSlot();
}

void VtkPost::SetFeatureAngle(int angle)
{
  preferences->ui.angleSpin->setValue(angle);
  this->drawFeatureEdgesSlot();
}

void VtkPost::Render()
{
  qvtkWidget->GetRenderWindow()->Render();
}

double VtkPost::GetCameraDistance()
{
  return renderer->GetActiveCamera()->GetDistance();
}

void VtkPost::SetCameraDistance(double f)
{
  renderer->GetActiveCamera()->SetDistance(f);
}

double VtkPost::GetCameraPositionX()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  return x;
}

double VtkPost::GetCameraPositionY()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  return y;
}

double VtkPost::GetCameraPositionZ()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  return z;
}

void VtkPost::SetCameraPositionX(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  renderer->GetActiveCamera()->SetPosition(f, y, z);
}

void VtkPost::SetCameraPositionY(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  renderer->GetActiveCamera()->SetPosition(x, f, z);
}

void VtkPost::SetCameraPositionZ(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetPosition(x, y, z);
  renderer->GetActiveCamera()->SetPosition(x, y, f);
}

double VtkPost::GetCameraFocalPointX()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  return x;
}

double VtkPost::GetCameraFocalPointY()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  return y;
}

double VtkPost::GetCameraFocalPointZ()
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  return z;
}

void VtkPost::SetCameraFocalPointX(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  renderer->GetActiveCamera()->SetFocalPoint(f, y, z);
}

void VtkPost::SetCameraFocalPointY(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  renderer->GetActiveCamera()->SetFocalPoint(x, f, z);
}

void VtkPost::SetCameraFocalPointZ(double f)
{
  double x, y, z;
  renderer->GetActiveCamera()->GetFocalPoint(x, y, z);
  renderer->GetActiveCamera()->SetFocalPoint(x, y, f);
}

void VtkPost::SetCameraDolly(double f)
{
  renderer->GetActiveCamera()->Dolly(f);
}

void VtkPost::SetCameraRoll(double f)
{
  renderer->GetActiveCamera()->Roll(f);
}

void VtkPost::SetCameraAzimuth(double f)
{
  renderer->GetActiveCamera()->Azimuth(f);
}

void VtkPost::SetCameraElevation(double f)
{
  renderer->GetActiveCamera()->Elevation(f);
}

void VtkPost::SetCameraPitch(double f)
{
  renderer->GetActiveCamera()->Pitch(f);
}

void VtkPost::SetCameraZoom(double f)
{
  renderer->GetActiveCamera()->Zoom(f);
}

void VtkPost::SetCameraYaw(double f)
{
  renderer->GetActiveCamera()->Yaw(f);
}

void VtkPost::SetInitialCameraPosition()
{
  renderer->ResetCamera();  
  renderer->GetActiveCamera()->SetPosition(initialCameraPosition[0], 
					   initialCameraPosition[1], 
					   initialCameraPosition[2]);
  renderer->GetActiveCamera()->SetRoll(initialCameraRoll);
  renderer->GetRenderWindow()->Render();
}
