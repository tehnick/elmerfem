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
#include <vtkCylinderSource.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkCamera.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>

using namespace std;

VtkPost::VtkPost(QWidget *parent)
  : QMainWindow(parent)
{
  // Initialize
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  setWindowTitle("VTK widget...");

  createActions();
  createMenus();
  createToolbars();
  createStatusBar();

  // Central widget
  qvtkWidget = new QVTKWidget;
  setCentralWidget(qvtkWidget);

  // VTK interaction
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
  drawCylinderAct = new QAction(QIcon(""), tr("Cylinder"), this);
  drawCylinderAct->setStatusTip("Draw cylinder");
  connect(drawCylinderAct, SIGNAL(triggered()), this, SLOT(drawCylinderSlot()));

  drawCubeAct = new QAction(QIcon(""), tr("Cube"), this);
  drawCubeAct->setStatusTip("Draw cube");
  connect(drawCubeAct, SIGNAL(triggered()), this, SLOT(drawCubeSlot()));
}

void VtkPost::createMenus()
{
  // File menu
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // View menu
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawCylinderAct);
  viewMenu->addAction(drawCubeAct);
}

void VtkPost::createToolbars()
{
}

void VtkPost::createStatusBar()
{
}

void VtkPost::exitSlot()
{
  close();
}

// The following is modified from the VTK documentation:

void VtkPost::drawCylinderSlot()
{
  // Geometry
  vtkCylinderSource *source = vtkCylinderSource::New();

  // Mapper
  vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
  mapper->ImmediateModeRenderingOn();
  mapper->SetInputConnection(source->GetOutputPort());

  // Actor in scene
  vtkActor *actor = vtkActor::New();
  actor->SetMapper(mapper);

  // Add Actor to renderer
  renderer->AddActor(actor);

  // Reset camera
  renderer->ResetCamera();

  // Render
  renderer->GetRenderWindow()->Render();

  // Clean up
  actor->Delete();
  mapper->Delete();
  source->Delete();
}

void VtkPost::drawCubeSlot()
{
  int i;

  static float x[8][3]={{0,0,0}, {1,0,0}, {1,1,0}, {0,1,0},
                        {0,0,1}, {1,0,1}, {1,1,1}, {0,1,1}};

  static vtkIdType pts[6][4]={{0,1,2,3}, {4,5,6,7}, {0,1,5,4},
			      {1,2,6,5}, {2,3,7,6}, {3,0,4,7}};
  
  // We'll create the building blocks of polydata including data attributes.
  vtkPolyData *cube = vtkPolyData::New();
  vtkPoints *points = vtkPoints::New();
  vtkCellArray *polys = vtkCellArray::New();
  vtkFloatArray *scalars = vtkFloatArray::New();

  // Load the point, cell, and data attributes.
  for(i=0; i<8; i++)
    points->InsertPoint(i, x[i]);

  for(i=0; i<6; i++)
    polys->InsertNextCell(4, pts[i]);

  for(i=0; i<8; i++)
    scalars->InsertTuple1(i, i);

  // We now assign the pieces to the vtkPolyData.
  cube->SetPoints(points);
  points->Delete();
  cube->SetPolys(polys);
  polys->Delete();
  cube->GetPointData()->SetScalars(scalars);
  scalars->Delete();

  // Now we'll look at it.
  vtkPolyDataMapper *cubeMapper = vtkPolyDataMapper::New();
  cubeMapper->SetInput(cube);
  cubeMapper->SetScalarRange(0, 7);
  vtkActor *cubeActor = vtkActor::New();
  cubeActor->SetMapper(cubeMapper);
  
  // The usual rendering stuff.
  renderer->AddActor(cubeActor);
  renderer->ResetCamera();
  renderer->GetRenderWindow()->Render();

  // Clean up
  cube->Delete();
  cubeMapper->Delete();
  cubeActor->Delete();
}
