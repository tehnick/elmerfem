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

#include <QVTKWidget.h>

#include <vtkActor.h>
#include <vtkActor2D.h>
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
#include <vtkHexahedron.h>
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
#include <vtkGlyph3D.h>
#include <vtkArrowSource.h>
#include <vtkWindowToImageFilter.h>
#include <vtkPNGWriter.h>
#include <vtkCellDerivatives.h>
#include <vtkCellDataToPointData.h>
#include <vtkSphereSource.h>
#include <vtkCylinderSource.h>
#include <vtkStreamLine.h>
#include <vtkRungeKutta4.h>
#include <vtkPointSource.h>
#include <vtkLineSource.h>
#include <vtkRibbonFilter.h>

#ifdef MATC
#include "matc.h"
#include <mc.h>
extern "C" void matc_commands();
extern "C" VARIABLE *com_grad(VARIABLE *);
extern "C" VARIABLE *var_new(char *,int,int,int);
extern "C" VARIABLE *var_check(char *);
extern "C" VARIABLE *var_temp_new(int,int,int);
extern "C" void var_delete(char *);
extern "C" char *mtc_domath(const char *);
extern "C" void mtc_init(FILE *,FILE *,FILE *);
extern "C" void com_init(char *,int,int,VARIABLE *(*)(VARIABLE *),int,int,char*);
#endif

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
  surfaceActor = vtkActor::New();
  meshEdgeActor = vtkActor::New();
  meshPointActor = vtkActor::New();
  colorBarActor = vtkScalarBarActor::New();
  featureEdgeActor = vtkActor::New();
  vectorActor = vtkActor::New();
  streamLineActor = vtkActor::New();

  // User interfaces:
  //-----------------
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

#ifdef MATC
  matc = new Matc(this);
  connect(matc->ui.mcEdit, SIGNAL(returnPressed()), this, SLOT(domatcSlot()));
  connect(matc->ui.mcHistory, SIGNAL(selectionChanged()),this,SLOT(matcCutPasteSlot()));
  mtc_init( NULL, stdout, stderr ); 
  QString elmerGuiHome = getenv("ELMERGUI_HOME");
  QString mcIniLoad = "source(\"" + elmerGuiHome.replace("\\", "/") + "/edf/mc.ini\")";
  mtc_domath( mcIniLoad.toAscii().data() );
  com_init( "grad", FALSE, FALSE, com_grad, 1, 1,
            "r = grad(f): compute gradient of a scalar variable f.\n") ;
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

  preferencesAct = new QAction(QIcon(""), tr("Preferences"), this);
  preferencesAct->setStatusTip("Show preferences");
  connect(preferencesAct, SIGNAL(triggered()), this, SLOT(preferencesSlot()));

  // Edit menu:
  //------------
#ifdef MATC
  matcAct = new QAction(QIcon(""), tr("Matc..."), this);
  matcAct->setStatusTip("Matc window");
  connect(matcAct, SIGNAL(triggered()), this, SLOT(matcOpenSlot()));
#endif

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
#ifdef MATC
  editMenu->addAction( matcAct );
#endif

  // View menu:
  //-----------
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawMeshPointAct);
  viewMenu->addAction(drawMeshEdgeAct);
  viewMenu->addAction(drawFeatureEdgesAct);
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

void VtkPost::grad(double *in, double *out)
{
   vtkFloatArray *s = vtkFloatArray::New();
   s->SetNumberOfComponents(1);
   s->SetNumberOfTuples(epMesh->epNodes);
   for( int i=0;i<epMesh->epNodes; i++ )
      s->SetValue(i,in[i] );

   vtkCellDerivatives *cd = vtkCellDerivatives::New();
   if ( volumeGrid->GetNumberOfCells() >0 ) {
     volumeGrid->GetPointData()->SetScalars(s);
     cd->SetInput(volumeGrid);
   } else {
     surfaceGrid->GetPointData()->SetScalars(s);
     cd->SetInput(surfaceGrid);
   }
   cd->SetVectorModeToComputeGradient();
   cd->Update();

   vtkCellDataToPointData *nd = vtkCellDataToPointData::New();
   nd->SetInput(cd->GetOutput());
   nd->Update();

   vtkDataArray *da = nd->GetOutput()->GetPointData()->GetVectors();
   int ncomp = da->GetNumberOfComponents();
   for( int i=0; i<epMesh->epNodes; i++ )
     for( int j=0; j<ncomp; j++ )
        out[epMesh->epNodes*j+i] = da->GetComponent(i,j);

   cd->Delete();
   nd->Delete();
   s->Delete(); 
}

void VtkPost::domatcSlot()
{
   char *ptr;
   LIST *lst;
   int i;
   VARIABLE *var;

   QString cmd=matc->ui.mcEdit->text().trimmed();

   matc->ui.mcEdit->clear();

   ptr=mtc_domath(cmd.toAscii().data());
   matc->ui.mcHistory->append(cmd);
   if ( ptr ) matc->ui.mcOutput->append(ptr);

   // int needs_update = false;
   for( lst = listheaders[VARIABLES].next; lst; lst = NEXT(lst))
   {
     var = (VARIABLE *)lst;
     if ( !NAME(var) || NCOL(var)!=epMesh->epNodes ) continue;

     int found = false,n;
     for( int i=0; i<scalarFields; i++ )
     {
        ScalarField *sf = &scalarField[i]; 
        if ( sf->name == NAME(var) )
        {
           found = true;
           if ( sf->value != MATR(var) )
           {
             free(sf->value);
             sf->value = MATR(var);
           }
           sf->minVal =  1e99;
           sf->maxVal = -1e99;
           for(int j=0; j<epMesh->epNodes; j++) {
             if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
             if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
           }
           break;
        } else if ( n=sf->name.indexOf("_x")>0 ) {
           if ( sf->name.left(n)==NAME(var) ) {
             found = true;
             if ( sf->value != &M(var,0,0) )
             {
               free(sf->value);
               sf->value = &M(var,0,0);
             }
             sf->minVal =  1e99;
             sf->maxVal = -1e99;
             for(int j=0; j<epMesh->epNodes; j++) {
               if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
               if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
             }
             break;
           }
        } else if ( n=sf->name.indexOf("_y")>0 ) {
           if ( sf->name.left(n)==NAME(var) ) {
             found = true;
             if ( sf->value != &M(var,1,0) )
             {
               free(sf->value);
               sf->value = &M(var,1,0);
             }
             sf->minVal =  1e99;
             sf->maxVal = -1e99;
             for(int j=0; j<epMesh->epNodes; j++) {
               if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
               if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
             }
             break;
           }
        } else if ( n=sf->name.indexOf("_z")>0 ) {
           if ( sf->name.left(n)==NAME(var) ) {
             found = true;
             if ( sf->value != &M(var,2,0) )
             {
               free(sf->value);
               sf->value = &M(var,2,0);
             }
             sf->minVal =  1e99;
             sf->maxVal = -1e99;
             for(int j=0; j<epMesh->epNodes; j++) {
               if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
               if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
             }
             break;
           }
        }
     }

     if ( !found ) 
     {
        if ( NROW(var) == 1 ) {
          ScalarField *sf = addScalarField( NAME(var),epMesh->epNodes,NULL );
          sf->minVal =  1e99;
          sf->maxVal = -1e99;
          for(int j = 0; j<epMesh->epNodes; j++) {
             if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
             if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
          }
        } else if ( NROW(var) == 3 ) {
          QString qs = NAME(var);
          ScalarField *sf = addScalarField( qs+"_x",epMesh->epNodes, &M(var,0,0) );
          sf->minVal =  1e99;
          sf->maxVal = -1e99;
          for(int j = 0; j<epMesh->epNodes; j++) {
             if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
             if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
          }
          sf = addScalarField( qs+"_y",epMesh->epNodes, &M(var,1,0) );
          sf->minVal =  1e99;
          sf->maxVal = -1e99;
          for(int j = 0; j<epMesh->epNodes; j++) {
             if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
             if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
          }
          sf = addScalarField( qs+"_z",epMesh->epNodes, &M(var,2,0) );
          sf->minVal =  1e99;
          sf->maxVal = -1e99;
          for(int j = 0; j<epMesh->epNodes; j++) {
             if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
             if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
          }
        }
        // needs_update = true;
     }
   }


   int count=0,n;
   for( int i=0; i<scalarFields; i++ )
   {
      ScalarField *sf = &scalarField[i]; 
      for( lst = listheaders[VARIABLES].next; lst; lst = NEXT(lst))
      {
        var = (VARIABLE *)lst;
        if ( !NAME(var) || NCOL(var) != epMesh->epNodes ) continue;

        if ( sf->name == NAME(var) )
        {
          if ( count != i ) scalarField[count]=*sf;
          count++;
          break;
        } else if ( n=sf->name.indexOf("_x")>0 ) {
          if ( sf->name.left(n) == NAME(var) )
          {
            if ( count != i ) scalarField[count]=*sf;
            count++;
          }
        } else if ( n=sf->name.indexOf("_y")>0 ) {
          if ( sf->name.left(n) == NAME(var) )
          {
            if ( count != i ) scalarField[count]=*sf;
            count++;
          }
        } else if ( n=sf->name.indexOf("_z")>0 ) {
          if ( sf->name.left(n) == NAME(var) )
          {
            if ( count != i ) scalarField[count]=*sf;
            count++;
            break;
          }
        }
      }
   }
   if ( count<scalarFields ) {
     // needs_update = true;
     scalarFields = count;
   }
   
   // Populate widgets in user interface dialogs:
   //---------------------------------------------
   vector->populateWidgets(scalarField, scalarFields);
   surface->populateWidgets(scalarField, scalarFields);
   isoSurface->populateWidgets(scalarField, scalarFields);
   isoContour->populateWidgets(scalarField, scalarFields);
   surface->populateWidgets(scalarField, scalarFields);
   streamLine->populateWidgets(scalarField, scalarFields);
   colorBar->populateWidgets();
}
#endif


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

  vtkWindowToImageFilter *image = vtkWindowToImageFilter::New();
  vtkPNGWriter *writer = vtkPNGWriter::New();

  image->SetInput(qvtkWidget->GetRenderWindow());
  image->Update();

  writer->SetInputConnection(image->GetOutputPort());
  writer->SetFileName(fileName.toAscii().data());

  qvtkWidget->GetRenderWindow()->Render();
  writer->Write();

  image->Delete();
  writer->Delete();
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
  ScalarField *nullField = addScalarField(fieldName, nodes, NULL);
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
      addScalarField(fieldName, nodes, NULL);

    if(fieldType == "vector") {
      addScalarField(fieldName + "_x", nodes, NULL);
      addScalarField(fieldName + "_y", nodes, NULL);
      addScalarField(fieldName + "_z", nodes, NULL);
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

  fieldName = "Nodes_x";
  sf = addScalarField(fieldName, nodes, NULL);
  for(int i = 0; i < nodes; i++) {
    sf->value[i] = epMesh->epNode[i].x[0];
    if(sf->value[i] > sf->maxVal) sf->maxVal = sf->value[i];
    if(sf->value[i] < sf->minVal) sf->minVal = sf->value[i];
  }

  fieldName = "Nodes_y";
  sf = addScalarField(fieldName, nodes, NULL);
  for(int i = 0; i < nodes; i++) {
    sf->value[i] = epMesh->epNode[i].x[1];
    if(sf->value[i] > sf->maxVal) sf->maxVal = sf->value[i];
    if(sf->value[i] < sf->minVal) sf->minVal = sf->value[i];
  }

  fieldName = "Nodes_z";
  sf = addScalarField(fieldName, nodes,NULL);
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
  vector->populateWidgets(scalarField, scalarFields);
  surface->populateWidgets(scalarField, scalarFields);
  isoSurface->populateWidgets(scalarField, scalarFields);
  isoContour->populateWidgets(scalarField, scalarFields);
  surface->populateWidgets(scalarField, scalarFields);
  streamLine->populateWidgets(scalarField, scalarFields);
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
ScalarField* VtkPost::addScalarField(QString fieldName, int nodes, double *value)
{
  if(scalarFields >= MAX_SCALARS) {
    cout << "Max. scalar limit exceeded!" << endl;
    return NULL;
  }

  ScalarField *sf = &scalarField[scalarFields++];
  sf->name = fieldName;
  sf->values = nodes;
  sf->value = value;
 
  if ( !sf->value ) {
#ifdef MATC
    QByteArray nm=fieldName.trimmed().toAscii();

    char *name = (char *)malloc( nm.count()+1 );
    strcpy(name,nm.data());
    VARIABLE *var = var_check(name);
    if ( !var || NCOL(var) != nodes )
      var = var_new( name, TYPE_DOUBLE, 1, nodes );
    sf->value = MATR(var);
    free(name);
#else
    sf->value = (double *)calloc(nodes,sizeof(double));
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
  drawMeshPointSlot();
  drawMeshEdgeSlot();
  drawFeatureEdgesSlot();
  drawSurfaceSlot();
  drawVectorSlot();
  drawIsoContourSlot();
  drawIsoSurfaceSlot();
  drawStreamLineSlot();
  drawColorBarSlot();

  renderer->ResetCamera();

  qvtkWidget->GetRenderWindow()->Render();
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
    lut = surfaceActor->GetMapper()->GetLookupTable();
  }

  if(actorName == "Vector") {
    fieldName = currentVectorName;
    if(fieldName.isEmpty()) return;
    lut = vectorActor->GetMapper()->GetLookupTable();
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
  if(width > 1.00) width = 1.00;
  if(height < 0.01) height = 0.01;
  if(height > 1.00) height = 1.00;

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


// Draw node points (labeled with node index):
//----------------------------------------------------------------------
void VtkPost::drawMeshPointSlot()
{
  renderer->RemoveActor(meshPointActor);
  if(!drawMeshPointAct->isChecked()) return;
  
  double length = surfaceGrid->GetLength();
  int pointQuality = preferences->ui.pointQuality->value();
  int pointSize = preferences->ui.pointSize->value();
  bool useSurfaceGrid = preferences->ui.meshPointsSurface->isChecked();

  vtkSphereSource *sphere = vtkSphereSource::New();
  sphere->SetRadius((double)pointSize * length / 2000.0);
  sphere->SetThetaResolution(pointQuality);
  sphere->SetPhiResolution(pointQuality);

  vtkGlyph3D *glyph = vtkGlyph3D::New();

  if(useSurfaceGrid) {
    glyph->SetInput(surfaceGrid);
  } else {
    glyph->SetInput(volumeGrid);
  }

  glyph->SetSourceConnection(sphere->GetOutputPort());

  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection(glyph->GetOutputPort());

  meshPointActor->SetMapper(mapper);
  renderer->AddActor(meshPointActor);

  qvtkWidget->GetRenderWindow()->Render();

  glyph->Delete();
  sphere->Delete();
  mapper->Delete();
}


// Draw mesh edges:
//----------------------------------------------------------------------
void VtkPost::drawMeshEdgeSlot()
{
  renderer->RemoveActor(meshEdgeActor);
  if(!drawMeshEdgeAct->isChecked()) return;

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
  // mapper->ImmediateModeRenderingOn();

  meshEdgeActor->GetProperty()->SetLineWidth(lineWidth);
  meshEdgeActor->GetProperty()->SetColor(0, 0, 0);
  meshEdgeActor->SetMapper(mapper);

  renderer->AddActor(meshEdgeActor);

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
  // mapper->ImmediateModeRenderingOn();

  featureEdgeActor->GetProperty()->SetLineWidth(lineWidth);
  featureEdgeActor->GetProperty()->SetColor(0, 0, 0);
  featureEdgeActor->SetMapper(mapper);

  renderer->AddActor(featureEdgeActor);
  qvtkWidget->GetRenderWindow()->Render();
  
  filter->Delete();
  edges->Delete();
  mapper->Delete();
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
    drawVectorSlot();
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

  QString vectorName = streamLine->ui.vectorCombo->currentText();

  if(vectorName.isEmpty()) return;

  int i, j, index = -1;
  for(i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    QString name = sf->name;
    if((j = name.indexOf("_x")) >= 0) {
      if(vectorName == name.mid(0, j)) {
	index = i;
	break;
      }
    }
  }

  if(index < 0) return;

  // UI data:
  //----------
  int colorIndex = streamLine->ui.colorCombo->currentIndex();
  double minVal = streamLine->ui.minVal->text().toDouble();
  double maxVal = streamLine->ui.maxVal->text().toDouble();
  double startX = streamLine->ui.startX->text().toDouble();
  double startY = streamLine->ui.startY->text().toDouble();
  double startZ = streamLine->ui.startZ->text().toDouble();
  double endX = streamLine->ui.endX->text().toDouble();
  double endY = streamLine->ui.endY->text().toDouble();
  double endZ = streamLine->ui.endZ->text().toDouble();
  double centerX = streamLine->ui.centerX->text().toDouble();
  double centerY = streamLine->ui.centerY->text().toDouble();
  double centerZ = streamLine->ui.centerZ->text().toDouble();
  double radius = streamLine->ui.radius->text().toDouble();
  int lines = streamLine->ui.lines->value();
  int points = streamLine->ui.points->value();
  double propagationTime = streamLine->ui.propagationTime->text().toDouble();
  double stepLength = streamLine->ui.stepLength->text().toDouble();
  double integStepLength = streamLine->ui.integStepLength->text().toDouble();
  bool drawRibbon = streamLine->ui.drawRibbon->isChecked();
  int ribbonWidth = streamLine->ui.ribbonWidth->value();
  int lineWidth = streamLine->ui.lineWidth->text().toInt();
  bool drawRake = streamLine->ui.rake->isChecked();
  int rakeWidth = streamLine->ui.rakeWidth->value();
  bool useSurfaceGrid = streamLine->ui.useSurfaceGrid->isChecked();
  int threads = streamLine->ui.threads->value();
  bool rakeSource = streamLine->ui.rakeSource->isChecked();
  
  // Choose the grid:
  //------------------
  vtkUnstructuredGrid *grid = NULL;
  if(useSurfaceGrid)
    grid = surfaceGrid;
  else
    grid = volumeGrid;

  // Vector data:
  //-------------
  grid->GetPointData()->RemoveArray("VectorData");
  vtkFloatArray *vectorData = vtkFloatArray::New();
  ScalarField *sf_x = &scalarField[index + 0];
  ScalarField *sf_y = &scalarField[index + 1];
  ScalarField *sf_z = &scalarField[index + 2];
  vectorData->SetNumberOfComponents(3);
  vectorData->SetNumberOfTuples(sf_x->values);
  vectorData->SetName("VectorData");
  for(int i = 0; i < sf_x->values; i++) {
    double val_x  = sf_x->value[i];
    double val_y  = sf_y->value[i];
    double val_z  = sf_z->value[i];
    vectorData->SetComponent(i, 0, val_x); 
    vectorData->SetComponent(i, 1, val_y); 
    vectorData->SetComponent(i, 2, val_z); 
  }
  grid->GetPointData()->AddArray(vectorData);

  // Color data:
  //-------------
  grid->GetPointData()->RemoveArray("StreamLineColor");
  ScalarField *sf = &scalarField[colorIndex];
  vtkFloatArray *vectorColor = vtkFloatArray::New();
  vectorColor->SetNumberOfComponents(1);
  vectorColor->SetNumberOfTuples(sf->values);
  vectorColor->SetName("StreamLineColor");
  for(int i = 0; i < sf->values; i++) 
    vectorColor->SetComponent(i, 0, sf->value[i]); 
  grid->GetPointData()->AddArray(vectorColor);

  // Stream line:
  //-------------
  grid->GetPointData()->SetActiveVectors("VectorData");
  grid->GetPointData()->SetActiveScalars("StreamLineColor");

  vtkPointSource *point = vtkPointSource::New();
  vtkLineSource *rake = vtkLineSource::New();
  if(rakeSource) {
    rake->SetPoint1(startX, startY, startZ);
    rake->SetPoint2(endX, endY, endZ);
    rake->SetResolution(lines);
  } else {
    point->SetCenter(centerX, centerY, centerZ);
    point->SetRadius(radius);
    point->SetNumberOfPoints(points);
  }

  vtkStreamLine *streamer = vtkStreamLine::New();
  vtkRungeKutta4 *integrator = vtkRungeKutta4::New();
  streamer->SetInput(grid);
  if(rakeSource) {
    streamer->SetSource(rake->GetOutput());
  } else {
    streamer->SetSource(point->GetOutput());
  }
  streamer->SetIntegrator(integrator);
  streamer->SetMaximumPropagationTime(propagationTime);
  streamer->SetIntegrationStepLength(integStepLength);
  streamer->SetIntegrationDirectionToForward();
  streamer->SetStepLength(stepLength);
  streamer->SetNumberOfThreads(threads);
  
  vtkRibbonFilter *ribbon = vtkRibbonFilter::New();
  if(drawRibbon) {
    double length = grid->GetLength();
    ribbon->SetInputConnection(streamer->GetOutputPort());
    ribbon->SetWidth(ribbonWidth * length / 1000.0);
    ribbon->SetWidthFactor(5);
  }

  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->ScalarVisibilityOn();
  mapper->SetScalarRange(minVal, maxVal);

  if(drawRibbon) {
    mapper->SetInputConnection(ribbon->GetOutputPort());
  } else {
    mapper->SetInputConnection(streamer->GetOutputPort());
  }

  mapper->SetColorModeToMapScalars();
  streamLineActor->SetMapper(mapper);

  if(!drawRibbon)
    streamLineActor->GetProperty()->SetLineWidth(lineWidth);

  renderer->AddActor(streamLineActor);

  rake->Delete();
  point->Delete();
  vectorData->Delete();
  vectorColor->Delete();
  integrator->Delete();
  streamer->Delete();
  ribbon->Delete();
  mapper->Delete();

  // Draw rake
  //-----------
  if(drawRake) {
    // todo
  }

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

  QString vectorName = vector->ui.vectorCombo->currentText();

  if(vectorName.isEmpty()) return;

  int i, j, index = -1;
  for(i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    QString name = sf->name;
    if((j = name.indexOf("_x")) >= 0) {
      if(vectorName == name.mid(0, j)) {
	index = i;
	break;
      }
    }
  }

  if(index < 0) return;

  // UI data:
  //----------
  int colorIndex = vector->ui.colorCombo->currentIndex();
  QString colorName = vector->ui.colorCombo->currentText();
  double minVal = vector->ui.minVal->text().toDouble();
  double maxVal = vector->ui.maxVal->text().toDouble();
  int quality = vector->ui.qualitySpin->value();
  int scaleMultiplier = vector->ui.scaleSpin->value();
  bool scaleByMagnitude = vector->ui.scaleByMagnitude->isChecked();

  // Vector data:
  //-------------
  volumeGrid->GetPointData()->RemoveArray("VectorData");
  vtkFloatArray *vectorData = vtkFloatArray::New();
  ScalarField *sf_x = &scalarField[index + 0];
  ScalarField *sf_y = &scalarField[index + 1];
  ScalarField *sf_z = &scalarField[index + 2];
  vectorData->SetNumberOfComponents(3);
  vectorData->SetNumberOfTuples(sf_x->values);
  vectorData->SetName("VectorData");
  double scaleFactor = 0.0;
  for(int i = 0; i < sf_x->values; i++) {
    double val_x  = sf_x->value[i];
    double val_y  = sf_y->value[i];
    double val_z  = sf_z->value[i];
    double absval = sqrt(val_x*val_x + val_y*val_y + val_z*val_z);
    if(absval > scaleFactor) scaleFactor = absval;
    vectorData->SetComponent(i, 0, val_x); 
    vectorData->SetComponent(i, 1, val_y); 
    vectorData->SetComponent(i, 2, val_z); 
  }
  volumeGrid->GetPointData()->AddArray(vectorData);

  // Size of volume grid:
  //---------------------
  double length = volumeGrid->GetLength();
  if(scaleByMagnitude)
    scaleFactor = scaleFactor * 100.0 / length;

  // Color data:
  //-------------
  volumeGrid->GetPointData()->RemoveArray("VectorColor");
  ScalarField *sf = &scalarField[colorIndex];
  vtkFloatArray *vectorColor = vtkFloatArray::New();
  vectorColor->SetNumberOfComponents(1);
  vectorColor->SetNumberOfTuples(sf->values);
  vectorColor->SetName("VectorColor");
  for(int i = 0; i < sf->values; i++) 
    vectorColor->SetComponent(i, 0, sf->value[i]); 
  volumeGrid->GetPointData()->AddArray(vectorColor);

  // Glyphs:
  //---------
  volumeGrid->GetPointData()->SetActiveVectors("VectorData"); 
  vtkGlyph3D *glyph = vtkGlyph3D::New();
  vtkArrowSource *arrow = vtkArrowSource::New();
  arrow->SetTipResolution(quality);
  arrow->SetShaftResolution(quality);
  glyph->SetInput(volumeGrid);
  glyph->SetSourceConnection(arrow->GetOutputPort());
  glyph->SetVectorModeToUseVector();

  if(scaleByMagnitude) {
    glyph->SetScaleFactor(scaleMultiplier / scaleFactor);
    glyph->SetScaleModeToScaleByVector();
  } else {
    glyph->SetScaleFactor(scaleMultiplier * length  / 100.0);
    glyph->ScalingOn();
  }
  glyph->SetColorModeToColorByScale();
  
  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection(glyph->GetOutputPort());
  mapper->SetScalarModeToUsePointFieldData();
  mapper->ScalarVisibilityOn();
  mapper->SetScalarRange(minVal, maxVal);
  mapper->SelectColorArray("VectorColor");
  // mapper->ImmediateModeRenderingOn();

  vectorActor->SetMapper(mapper);
  renderer->AddActor(vectorActor);

  // Update color bar && field name:
  //---------------------------------
  currentVectorName = colorName;
  drawColorBarSlot();

  qvtkWidget->GetRenderWindow()->Render();

  mapper->Delete();
  arrow->Delete();
  glyph->Delete();
  vectorData->Delete();
  vectorColor->Delete();
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

  // Data from UI:
  //--------------
  int surfaceIndex = surface->ui.surfaceCombo->currentIndex();
  QString surfaceName = surface->ui.surfaceCombo->currentText();
  double minVal = surface->ui.minEdit->text().toDouble();
  double maxVal = surface->ui.maxEdit->text().toDouble();
  bool useNormals = surface->ui.useNormals->isChecked();
  int featureAngle = surface->ui.featureAngle->value();
  double opacity = surface->ui.opacitySpin->value() / 100.0;

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

  // Normals:
  //---------
  vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
  vtkGeometryFilter *filter = vtkGeometryFilter::New();

  if(useNormals) {
    // Convert from vtkUnstructuredGrid to vtkPolyData
    filter->SetInput(surfaceGrid);
    filter->GetOutput()->ReleaseDataFlagOn();
    normals->SetInputConnection(filter->GetOutputPort());
    normals->SetFeatureAngle(featureAngle);
  }

  // Mapper:
  //--------
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();

  if(useNormals) {
    mapper->SetInputConnection(normals->GetOutputPort());
  } else {
    mapper->SetInput(surfaceGrid);
  }

  mapper->SetScalarModeToUsePointFieldData();
  mapper->SelectColorArray("Surface");
  mapper->ScalarVisibilityOn();
  mapper->SetScalarRange(minVal, maxVal);
  mapper->SetResolveCoincidentTopologyToPolygonOffset();
  // mapper->ImmediateModeRenderingOn();

  // Actor & renderer:
  //------------------
  surfaceActor->SetMapper(mapper);
  surfaceActor->GetProperty()->SetOpacity(opacity);
  renderer->AddActor(surfaceActor);

  // Update color bar && field name:
  //---------------------------------
  currentSurfaceName = sf->name;
  drawColorBarSlot();

  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //-----------
  normals->Delete();
  filter->Delete();
  scalars->Delete();
  mapper->Delete();
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
  int featureAngle = isoSurface->ui.featureAngle->value();
  double opacity = isoSurface->ui.opacitySpin->value() / 100.0;

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

  vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
  if(useNormals) {
    normals->SetInputConnection(iso->GetOutputPort());
    normals->SetFeatureAngle(featureAngle);
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
  // mapper->ImmediateModeRenderingOn();

  // Actor && renderer:
  //-------------------
  isoSurfaceActor->SetMapper(mapper);
  isoSurfaceActor->GetProperty()->SetOpacity(opacity);
  renderer->AddActor(isoSurfaceActor);

  // Redraw colorbar:
  //------------------
  currentIsoSurfaceName = colorName;
  drawColorBarSlot();

  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //----------
  contourArray->Delete();
  colorArray->Delete();
  iso->Delete();
  normals->Delete();
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

void VtkPost::hideIsoContourSlot()
{
  drawIsoContourAct->setChecked(false);
  drawIsoContourSlot();
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
  // mapper->ImmediateModeRenderingOn();

  // Actor & renderer:
  //-------------------
  isoContourActor->SetMapper(mapper);
  isoContourActor->GetProperty()->SetLineWidth(lineWidth);
  renderer->AddActor(isoContourActor);

  // Redraw colorbar:
  //------------------
  currentIsoContourName = colorName;
  drawColorBarSlot();
  
  qvtkWidget->GetRenderWindow()->Render();

  // Clean up:
  //----------
  contourArray->Delete();
  colorArray->Delete();
  iso->Delete();
  mapper->Delete();
}
