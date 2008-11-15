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
 *  ElmerGUI cadview                                                         *
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

#include "cadview.h"

#include <QVTKWidget.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkActor.h>
#include <vtkPoints.h>
#include <vtkTriangle.h>
#include <vtkDataSetMapper.h>
#include <vtkPolyDataNormals.h>
#include <vtkFeatureEdges.h>
#include <vtkProperty.h>
#include <vtkPropPicker.h>
#include <vtkCallbackCommand.h>
#include <vtkPolyDataMapper.h>
#include <vtkAppendPolyData.h>
#include <vtkFloatArray.h>
#include <vtkCleanPolyData.h>

#include <BRep_Builder.hxx>
#include <TopoDS_Shape.hxx>
#include <BRepTools.hxx>
#include <TopTools_HSequenceOfShape.hxx>
#include <BRepMesh.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS_Face.hxx>
#include <TopoDS.hxx>
#include <BRep_Tool.hxx>
#include <Poly_Triangulation.hxx>
#include <GeomLProp_SLProps.hxx>
#include <STEPControl_Reader.hxx>
#include <Bnd_Box.hxx>
#include <BRepBndLib.hxx>

using namespace std;

static void pickEventHandler(vtkObject* caller, unsigned long eid, 
			     void* clientdata, void* calldata)
{
  CadView* cadView = reinterpret_cast<CadView*>(clientdata);
  QVTKWidget* qvtkWidget = cadView->GetQVTKWidget();
  vtkAbstractPicker* picker = qvtkWidget->GetInteractor()->GetPicker();
  vtkPropPicker* propPicker = vtkPropPicker::SafeDownCast(picker);
  vtkActor* actor = propPicker->GetActor();

  if(actor) {
    vtkProperty* p = actor->GetProperty();

    double color[3];
    p->GetColor(color);

    // Toggle color:
    //--------------
    if(color[0] < 0.5) {
      p->SetColor(1, 0, 0);
    } else {
      p->SetColor(0, 1, 1);
    }
  }
}

CadView::CadView(QWidget *parent)
  : QMainWindow(parent)
{
  setWindowTitle("ElmerGUI geometry viewer");
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  resize(800, 600);

  createActions();
  createMenus();

  qVTKWidget = new QVTKWidget(this);
  setCentralWidget(qVTKWidget);

  renderer = vtkRenderer::New();
  renderer->SetBackground(1, 1, 1);
  qVTKWidget->GetRenderWindow()->AddRenderer(renderer);
  renderer->GetRenderWindow()->Render();

  vtkPropPicker* propPicker = vtkPropPicker::New();
  vtkCallbackCommand* cbcPick = vtkCallbackCommand::New();
  qVTKWidget->GetInteractor()->SetPicker(propPicker);
  cbcPick->SetClientData(this);
  cbcPick->SetCallback(pickEventHandler);
  qVTKWidget->GetInteractor()->GetPicker()->AddObserver(vtkCommand::PickEvent, cbcPick);
  propPicker->Delete();
  cbcPick->Delete();

  stlSurfaceData = vtkAppendPolyData::New();
  stlEdgeData = vtkAppendPolyData::New();

  modelLength = 0.0;
  numberOfFaces = 0;
}

CadView::~CadView()
{
}

QSize CadView::minimumSizeHint() const
{
  return QSize(64, 64);
}

QSize CadView::sizeHint() const
{
  return QSize(720, 576);
}

void CadView::createActions()
{
  exitAct = new QAction(QIcon(""), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(closeSlot()));
}

void CadView::createMenus()
{
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);
}

void CadView::closeSlot()
{
  close();
}

bool CadView::readFile(QString fileName)
{
  double deflection = 0.0005;
  double featureAngle = 30.0;

  if(stlSurfaceData->GetOutput()->GetNumberOfPoints() > 0)
    stlSurfaceData->Delete();

  if(stlEdgeData->GetOutput()->GetNumberOfPoints() > 0)
    stlEdgeData->Delete();

  stlSurfaceData = vtkAppendPolyData::New();
  stlEdgeData = vtkAppendPolyData::New();

  if(fileName.isEmpty()) {
    cout << "File name is empty" << endl;
    return false;
  }

  QFileInfo fileInfo(fileName);
  QString fileSuffix = fileInfo.suffix().toLower();
  
  TopoDS_Shape shape;

  if(fileSuffix == "brep")
    shape = readBrep(fileName);

  if((fileSuffix == "step") || (fileSuffix == "stp"))
    shape = readStep(fileName);
  
  if(shape.IsNull())
    return false;

  clearScreen();

  // Compute bounding box:
  //----------------------
  Bnd_Box boundingBox;
  double min[3], max[3];
  
  BRepBndLib::Add(shape, boundingBox);
  boundingBox.Get(min[0], min[1], min[2], max[0], max[1], max[2]);

  cout << "Bounding box: "
       << "[ " << min[0] << ", " << min[1] << ", " << min[2] << "] x "
       << "[ " << max[0] << ", " << max[1] << ", " << max[2] << "]" << endl;

  double length = sqrt((max[2]-min[2])*(max[2]-min[2])
		       +(max[1]-min[1])*(max[1]-min[1]) 
		       +(max[0]-min[0])*(max[0]-min[0]));

  deflection *= length; // use relative deflection
  
  // Construct model data and draw surfaces:
  //-----------------------------------------
  BRepMesh::Mesh(shape, deflection);

  numberOfFaces = 0;
  TopExp_Explorer expFace(shape, TopAbs_FACE);
  for(expFace; expFace.More(); expFace.Next()) {
    TopoDS_Face Face = TopoDS::Face(expFace.Current());

    TopLoc_Location Location;
    Handle(Poly_Triangulation) Triangulation = BRep_Tool::Triangulation(Face, Location);
    if(Triangulation.IsNull()) continue;

    numberOfFaces++;

    const Poly_Array1OfTriangle& Triangles = Triangulation->Triangles();
    const TColgp_Array1OfPnt& Nodes = Triangulation->Nodes();

    int nofTriangles = Triangulation->NbTriangles();
    int nofNodes = Triangulation->NbNodes();

    int n0, n1, n2;
    vtkPolyData* partGrid = vtkPolyData::New();
    vtkTriangle* triangle = vtkTriangle::New();
    partGrid->Allocate(nofTriangles, nofTriangles);

    for(int i = Triangles.Lower(); i <= Triangles.Upper(); i++) {
      Triangles(i).Get(n0, n1, n2);

      if(Face.Orientation() != TopAbs_FORWARD) {
	int tmp = n2; n2 = n1; n1 = tmp;
      }

      triangle->GetPointIds()->SetId(0, n0 - Nodes.Lower());
      triangle->GetPointIds()->SetId(1, n1 - Nodes.Lower());
      triangle->GetPointIds()->SetId(2, n2 - Nodes.Lower());

      partGrid->InsertNextCell(triangle->GetCellType(),
			       triangle->GetPointIds());
    }

    double x[3];
    vtkPoints* partPoints = vtkPoints::New();
    for(int i = Nodes.Lower(); i <= Nodes.Upper(); i++) {
      x[0] = Nodes(i).X(); x[1] = Nodes(i).Y(); x[2] = Nodes(i).Z();
      partPoints->InsertPoint(i - Nodes.Lower(), x);
    }

    partGrid->SetPoints(partPoints);

    // Draw part:
    //-----------
    vtkPolyDataNormals* partNormals = vtkPolyDataNormals::New();
    partNormals->SetInput(partGrid);
    partNormals->SetFeatureAngle(featureAngle);
    
    vtkDataSetMapper* partMapper = vtkDataSetMapper::New();
    partMapper->SetInputConnection(partNormals->GetOutputPort());
    partMapper->ScalarVisibilityOff();
    
    vtkActor* partActor = vtkActor::New();
    partActor->SetPickable(1);
    partActor->GetProperty()->SetColor(0, 1, 1);
    partActor->SetMapper(partMapper);
    renderer->AddActor(partActor);

    vtkFeatureEdges* partFeature = vtkFeatureEdges::New();
    partFeature->SetInput(partGrid);
    partFeature->SetFeatureAngle(featureAngle);
    partFeature->FeatureEdgesOn();
    partFeature->BoundaryEdgesOn();
    partFeature->NonManifoldEdgesOn();
    partFeature->ManifoldEdgesOff();

    vtkDataSetMapper *partFeatureMapper = vtkDataSetMapper::New();
    partFeatureMapper->SetInputConnection(partFeature->GetOutputPort());
    partFeatureMapper->SetResolveCoincidentTopologyToPolygonOffset();
    partFeatureMapper->ScalarVisibilityOff();
    
    vtkActor* partFeatureActor = vtkActor::New();
    partFeatureActor->SetPickable(0);
    partFeatureActor->GetProperty()->SetColor(0, 0, 0);
    partFeatureActor->SetMapper(partFeatureMapper);
    renderer->AddActor(partFeatureActor);

    // Add triangles and edges to STL structures:
    //--------------------------------------------
    stlSurfaceData->AddInput(partGrid);
    stlEdgeData->AddInput(partFeature->GetOutput());

    // Clean up:
    //----------
    partFeatureActor->Delete();
    partFeatureMapper->Delete();
    partFeature->Delete();
    partActor->Delete();
    partNormals->Delete();
    partMapper->Delete();
    partGrid->Delete();
    partPoints->Delete();
    triangle->Delete();
  }

  stlSurfaceData->Update();
  stlEdgeData->Update();
  modelLength = stlSurfaceData->GetOutput()->GetLength();
  cout << "StlSurfaceData: points: " << stlSurfaceData->GetOutput()->GetNumberOfPoints() << endl;
  cout << "StlSurfaceData: cells: " << stlSurfaceData->GetOutput()->GetNumberOfCells() << endl;
  cout << "StlEdgeData: lines: " << stlEdgeData->GetOutput()->GetNumberOfLines() << endl;
  cout << "StlModelData: length: " << modelLength << endl;

  // Draw:
  //------
  qVTKWidget->GetRenderWindow()->Render();
  renderer->ResetCamera();

  QCoreApplication::processEvents();

  return true;
}

void CadView::generateSTLSlot()
{
  double meshMinSize = 0.005 * modelLength;
  double meshMaxSize = 0.1 * modelLength;
  double meshFineness = 0.5;
  bool restrictBySTL = true;

  // Add STL triangles to geometry:
  //--------------------------------
  vtkCleanPolyData* stlSurface = vtkCleanPolyData::New();
  stlSurface->PointMergingOn();
  stlSurface->SetInput(stlSurfaceData->GetOutput());
  stlSurface->Update();

  double p0[3], p1[3], p2[3];
  for(int i = 0; i < stlSurface->GetOutput()->GetNumberOfCells(); i++) {
    vtkCell* cell = stlSurface->GetOutput()->GetCell(i);
    int nofCellPoints = cell->GetNumberOfPoints();
    vtkPoints* cellPoints = cell->GetPoints();

    if(nofCellPoints == 3) {
      cellPoints->GetPoint(0, p0);
      cellPoints->GetPoint(1, p1);
      cellPoints->GetPoint(2, p2);
      nglib::Ng_STL_AddTriangle(geom, p0, p1, p2, NULL);
    }
  }

  // Add STL edges to geometry:
  //----------------------------
  vtkCleanPolyData* stlEdge = vtkCleanPolyData::New();
  stlEdge->PointMergingOn();
  stlEdge->SetInput(stlEdgeData->GetOutput());
  stlEdge->Update();

  for(int i = 0; i < stlEdge->GetOutput()->GetNumberOfCells(); i++) {
    vtkCell* cell = stlEdge->GetOutput()->GetCell(i);
    int nofCellPoints = cell->GetNumberOfPoints();
    vtkPoints* cellPoints = cell->GetPoints();

    if(nofCellPoints == 2) {
      cellPoints->GetPoint(0, p0);
      cellPoints->GetPoint(1, p1);
      nglib::Ng_STL_AddEdge(geom, p0, p1);
    }
  }

  // Init STL geometry:
  //--------------------
  nglib::Ng_STL_InitSTLGeometry(geom);

  // Generate edges:
  //-----------------
  nglib::Ng_STL_MakeEdges(geom, mesh, mp);
  
  // Global mesh size restrictions:
  //--------------------------------
  // nglib::Ng_RestrictMeshSizeGlobal(mesh, meshMaxSize);
  
  // Local mesh size restrictions:
  //-------------------------------
  if(restrictBySTL)    
    restrictMeshSizeLocal(mesh, stlSurface->GetOutput(), meshMaxSize, meshMinSize);
}

QVTKWidget* CadView::GetQVTKWidget()
{
  return this->qVTKWidget;
}

void CadView::clearScreen()
{
  cout << "Clear screen" << endl;
  vtkActorCollection* actors = renderer->GetActors();
  vtkActor* lastActor = actors->GetLastActor();
  while(lastActor != NULL) {
    renderer->RemoveActor(lastActor);
    lastActor = actors->GetLastActor();
  }
}

TopoDS_Shape CadView::readBrep(QString fileName)
{
  TopoDS_Shape shape;
  BRep_Builder builder;
  Standard_Boolean result;

  result = BRepTools::Read(shape, fileName.toAscii().data(), builder);    

  if(!result)
    cout << "Read brep failed" << endl;
  
  return shape;
}

TopoDS_Shape CadView::readStep(QString fileName)
{
  TopoDS_Shape shape;
  Handle_TopTools_HSequenceOfShape shapes;
  STEPControl_Reader stepReader;
  IFSelect_ReturnStatus status;

  status = stepReader.ReadFile(fileName.toAscii().data());
  
  if(status == IFSelect_RetDone) {	  
    bool failsonly = false;
    stepReader.PrintCheckLoad(failsonly, IFSelect_ItemsByEntity);
    
    int nbr = stepReader.NbRootsForTransfer();
    stepReader.PrintCheckTransfer(failsonly, IFSelect_ItemsByEntity);
    
    for(Standard_Integer n = 1; n <= nbr; n++) {
      bool ok = stepReader.TransferRoot(n);
      int nbs = stepReader.NbShapes();
      
      if(nbs > 0) {
	shapes = new TopTools_HSequenceOfShape();
	for(int i = 1; i <= nbs; i++) {
	  cout << "Added shape: " << i << endl;
	  shape = stepReader.Shape(i);
	  shapes->Append(shape);
	}
      }
    }
  }

  return shape;
}

void CadView::restrictMeshSizeLocal(nglib::Ng_Mesh* mesh, vtkPolyData* stlData,
				    double meshMaxSize, double meshMinSize)
{
  int n0, n1, n2;
  double h, h0, h1, h2;
  double p0[3], p1[3], p2[3], t[3];
  vtkFloatArray* mshSize = vtkFloatArray::New();
  mshSize->SetNumberOfComponents(1);
  mshSize->SetNumberOfTuples(stlData->GetNumberOfPoints());
  
  for(int i = 0; i < stlData->GetNumberOfPoints(); i++) 
    mshSize->SetComponent(i, 0, meshMaxSize);

  for(int i = 0; i < stlData->GetNumberOfCells(); i++) {
    vtkCell* cell = stlData->GetCell(i);
    int nofCellPoints = cell->GetNumberOfPoints();
    vtkPoints* cellPoints = cell->GetPoints();
    
    if(nofCellPoints == 3) {
      n0 = cell->GetPointId(0);
      n1 = cell->GetPointId(1);
      n2 = cell->GetPointId(2);
      
      h0 = mshSize->GetComponent(n0, 0);
      h1 = mshSize->GetComponent(n1, 0);
      h2 = mshSize->GetComponent(n2, 0);
      
      cellPoints->GetPoint(0, p0);
      cellPoints->GetPoint(1, p1);
      cellPoints->GetPoint(2, p2);
      
      t[0] = p1[0]-p0[0]; t[1] = p1[1]-p0[1]; t[2] = p1[2]-p0[2];
      h = sqrt(t[0]*t[0] + t[1]*t[1] + t[2]*t[2]);
      if(h < meshMinSize) h = meshMinSize;
      if(h < h1) mshSize->SetComponent(n1, 0, h);
      if(h < h0) mshSize->SetComponent(n0, 0, h);
      
      t[0] = p2[0]-p0[0]; t[1] = p2[1]-p0[1]; t[2] = p2[2]-p0[2];
      h = sqrt(t[0]*t[0] + t[1]*t[1] + t[2]*t[2]);
      if(h < meshMinSize) h = meshMinSize;
      if(h < h2) mshSize->SetComponent(n2, 0, h);
      if(h < h0) mshSize->SetComponent(n0, 0, h);
      
      t[0] = p2[0]-p1[0]; t[1] = p2[1]-p1[1]; t[2] = p2[2]-p1[2];
      h = sqrt(t[0]*t[0] + t[1]*t[1] + t[2]*t[2]);
      if(h < meshMinSize) h = meshMinSize;
      if(h < h2) mshSize->SetComponent(n2, 0, h);
      if(h < h1) mshSize->SetComponent(n1, 0, h);
    }
  }  

  for(int i = 0; i < stlData->GetNumberOfPoints(); i++) {
    h = mshSize->GetComponent(i, 0);
    if(h < meshMinSize) h = meshMinSize;
    if(h > meshMaxSize) h = meshMaxSize;
    stlData->GetPoint(i, p0);
    nglib::Ng_RestrictMeshSizePoint(mesh, p0, h);
  }
  
  mshSize->Delete();
}

void CadView::generateSTL()
{
  this->generateSTLSlot();
}

void CadView::setMesh(nglib::Ng_Mesh* mesh)
{
  this->mesh = mesh;
}

void CadView::setGeom(nglib::Ng_STL_Geometry* geom)
{
  this->geom = geom;
}

void CadView::setMp(nglib::Ng_Meshing_Parameters* mp)
{
  this->mp = mp;
}
