/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland    *
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
 *  ElmerGUI TwodView                                                        *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/
#include <QAction>
#include <QIcon>
#include <QMenu>
#include <QMenuBar>
#include <QStatusBar>
#include <QFileDialog>
#include <QMessageBox>
#include <QDockWidget>
#include <iostream>

#include "twodview.h"
#include "renderarea.h"
#include "curveeditor.h"

using namespace std;

TwodView::TwodView(QWidget *parent)
  : QMainWindow(parent)
{
  renderArea = new RenderArea(this);
  setCentralWidget(renderArea);

  connect(renderArea, SIGNAL(statusMessage(QString)), this, SLOT(statusMessage(QString)));

  curveEditor = new CurveEditor;
  QDockWidget *dockWidget = new QDockWidget("Editor", this);
  dockWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  dockWidget->setWidget(curveEditor);  
  addDockWidget(Qt::RightDockWidgetArea, dockWidget); 

  renderArea->setCurveEditor(curveEditor);

  createActions();
  createMenus();
  createStatusBar();
  
  setWindowTitle("ElmerGUI 2D model view");
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  resize(620, 400);
}

TwodView::~TwodView()
{
}

void TwodView::createActions()
{
  openAction = new QAction(QIcon(""), tr("&Open..."), this);
  openAction->setShortcut(tr("Ctrl+O"));
  connect(openAction, SIGNAL(triggered()), this, SLOT(openSlot()));

  quitAction = new QAction(QIcon(""), tr("&Quit"), this);
  quitAction->setShortcut(tr("Ctrl+Q"));
  connect(quitAction, SIGNAL(triggered()), this, SLOT(close()));

  fitAction =  new QAction(QIcon(""), tr("&Fit to window"), this);
  fitAction->setShortcut(tr("Ctrl+F"));
  connect(fitAction, SIGNAL(triggered()), renderArea, SLOT(fitSlot()));

  drawPointsAction = new QAction(QIcon(""), tr("Draw points"), this);
  drawPointsAction->setCheckable(true);
  connect(drawPointsAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawPointsSlot(bool)));
  drawPointsAction->setChecked(true);

  drawSplinesAction = new QAction(QIcon(""), tr("Draw splines"), this);
  drawSplinesAction->setCheckable(true);
  connect(drawSplinesAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawSplinesSlot(bool)));
  drawSplinesAction->setChecked(true);

  drawTangentsAction = new QAction(QIcon(""), tr("Draw tangents"), this);
  drawTangentsAction->setCheckable(true);
  connect(drawTangentsAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawTangentsSlot(bool)));
  drawTangentsAction->setChecked(true);

  drawPointNumbersAction = new QAction(QIcon(""), tr("Point numbers"), this);
  drawPointNumbersAction->setCheckable(true);
  connect(drawPointNumbersAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawPointNumbersSlot(bool)));
  drawPointNumbersAction->setChecked(true);

  drawSplineNumbersAction = new QAction(QIcon(""), tr("Spline numbers"), this);
  drawSplineNumbersAction->setCheckable(true);
  connect(drawSplineNumbersAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawSplineNumbersSlot(bool)));
  drawSplineNumbersAction->setChecked(true);

  drawMaterialNumbersAction = new QAction(QIcon(""), tr("Material numbers"), this);
  drawMaterialNumbersAction->setCheckable(true);
  connect(drawMaterialNumbersAction, SIGNAL(toggled(bool)), renderArea, SLOT(drawMaterialNumbersSlot(bool)));
  drawMaterialNumbersAction->setChecked(true);

  helpAction =  new QAction(QIcon(""), tr("&Help"), this);
  helpAction->setShortcut(tr("Ctrl+H"));
  connect(helpAction, SIGNAL(triggered()), this, SLOT(helpSlot()));
}

void TwodView::createMenus()
{
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(openAction);
  fileMenu->addSeparator();
  fileMenu->addAction(quitAction);

  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(drawPointsAction);
  viewMenu->addAction(drawSplinesAction);
  viewMenu->addAction(drawTangentsAction);
  viewMenu->addSeparator();
  viewMenu->addAction(drawPointNumbersAction);
  viewMenu->addAction(drawSplineNumbersAction);
  viewMenu->addAction(drawMaterialNumbersAction);
  viewMenu->addSeparator();
  viewMenu->addAction(fitAction);

  helpMenu = menuBar()->addMenu(tr("&Help"));
  helpMenu->addAction(helpAction);
}

void TwodView::createStatusBar()
{
  statusBar()->showMessage(tr("Ready"));
}

void TwodView::statusMessage(QString message)
{
  statusBar()->showMessage(message);
}

void TwodView::openSlot()
{
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open file"), "", tr("Geometry Input Files (*.in2d)"));

  if(fileName.isEmpty())
    return;

  renderArea->readSlot(fileName);
  renderArea->fitSlot();
}

void TwodView::helpSlot()
{
  QMessageBox::information(this, tr("Information"), 
			   tr("Mouse controls:\n"
			      "   Left-click to move points\n"
			      "   Right-click to pan\n"
			      "   Rotate wheel to zoom\n\n"
			      "Supported formats:\n"
			      "   splinecurves2dv2"));
}
