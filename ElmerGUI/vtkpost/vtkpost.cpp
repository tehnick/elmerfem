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
#include <vtkCylinderSource.h>
#include <vtkPolyDataMapper.h>

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
  ren = vtkRenderer::New();
  qvtkWidget->GetRenderWindow()->AddRenderer(ren);
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
}

void VtkPost::createMenus()
{
  // File menu
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // View menu
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawCylinderAct);
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

// The following is modified from the SimpleView example of VTK documentation:

void VtkPost::drawCylinderSlot()
{
  // Geometry
  source = vtkCylinderSource::New();

  // Mapper
  mapper = vtkPolyDataMapper::New();
  mapper->ImmediateModeRenderingOn();
  mapper->SetInputConnection(source->GetOutputPort());

  // Actor in scene
  actor = vtkActor::New();
  actor->SetMapper(mapper);

  // Add Actor to renderer
  ren->AddActor(actor);

  // Reset camera
  ren->ResetCamera();

  // Render
  ren->GetRenderWindow()->Render();
}
