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

  mesh = NULL;

  // Central widget:
  //----------------
  qvtkWidget = new QVTKWidget;
  setCentralWidget(qvtkWidget);

  // VTK interaction:
  //------------------
  renderer = vtkRenderer::New();
  renderer->SetBackground(1, 1, 1);
  qvtkWidget->GetRenderWindow()->AddRenderer(renderer);
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
  // File menu
  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip("Quit VTK widget");
  connect(exitAct, SIGNAL(triggered()), this, SLOT(exitSlot()));

  // View menu
  drawSurfaceMeshAct = new QAction(QIcon(""), tr("Surface mesh"), this);
  drawSurfaceMeshAct->setStatusTip("Draw surface mesh");
  connect(drawSurfaceMeshAct, SIGNAL(triggered()), this, SLOT(drawSurfaceMeshSlot()));
}

void VtkPost::createMenus()
{
  // File menu
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // View menu
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawSurfaceMeshAct);
}

void VtkPost::createToolbars()
{
}

void VtkPost::createStatusBar()
{
}

void VtkPost::setMesh(mesh_t *mesh)
{
  this->mesh = mesh;
}

// Exit VTK widget:
//----------------------------------------------------------------------
void VtkPost::exitSlot()
{
  close();
}

// Draw surface mesh:
//----------------------------------------------------------------------
void VtkPost::drawSurfaceMeshSlot()
{
  if(!mesh)
    return;

  vtkPolyData *surf = vtkPolyData::New();

  // Points:
  //------------
  vtkPoints *points = vtkPoints::New();
  for(int i = 0; i < mesh->nodes; i++) {
    node_t *n = &mesh->node[i];
    points->InsertPoint(i, n->x);
  }
  surf->SetPoints(points);
  points->Delete();

  // Polygons:
  //----------
  vtkCellArray *polys = vtkCellArray::New();
  for(int i = 0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    if(s->nature == PDE_BOUNDARY)
      polys->InsertNextCell(s->nodes, s->node);
  }
  surf->SetPolys(polys);
  polys->Delete();

  // Scalars:
  //---------
  vtkFloatArray *scalars = vtkFloatArray::New();
  for(int i = 0; i < mesh->nodes; i++) {
    scalars->InsertTuple1(i, i); // random color
  }
  surf->GetPointData()->SetScalars(scalars);
  scalars->Delete();

  // Mapper:
  //--------
  vtkPolyDataMapper *surfMapper = vtkPolyDataMapper::New();
  surfMapper->SetInput(surf);
  surfMapper->SetScalarRange(0, mesh->nodes - 1);

  // Actor:
  //--------
  vtkActor *surfActor = vtkActor::New();
  surfActor->SetMapper(surfMapper);
  
  // Renderer:
  //----------
  renderer->AddActor(surfActor);
  renderer->ResetCamera();
  renderer->GetRenderWindow()->Render();

  // Clean up:
  //-----------
  surfActor->Delete();
  surfMapper->Delete();
  surf->Delete();
}
