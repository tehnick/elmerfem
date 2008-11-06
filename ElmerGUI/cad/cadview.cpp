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

using namespace std;

//-----------------------------------------------------------------------------
CadView::CadView(QWidget *parent)
  : QMainWindow(parent)
{
  setWindowTitle("ElmerGUI cad model viewer");
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
  
  qoccViewerContext  = new QoccViewerContext();
  qoccViewWidget = new QoccViewWidget(qoccViewerContext->getContext(), this);
  this->setCentralWidget(qoccViewWidget);
  
  createActions();
  createMenus();

  cout << "Cad model view window (qocc) set up" << endl;
  cout.flush();  
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
  // File menu:
  //-----------
  exitAct = new QAction(QIcon(":/icons/application-exit.png"), tr("&Quit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

  // View menu:
  //-----------
  fitToWindowAct = new QAction(QIcon(""), tr("&Fit to window"), this);
  fitToWindowAct->setShortcut(tr("Ctrl+F"));
  connect(fitToWindowAct, SIGNAL(triggered()), this, SLOT(fitToWindowSlot()));

  // Help menu:
  //-----------
  helpAct = new QAction(QIcon(""), tr("&Help"), this);
  helpAct->setShortcut(tr("Ctrl+H"));
  connect(helpAct, SIGNAL(triggered()), this, SLOT(helpSlot()));
}


void CadView::createMenus()
{
  // File menu:
  //------------
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(exitAct);

  // View menu:
  //-----------
  viewMenu = menuBar()->addMenu(tr("&View"));
  viewMenu->addAction(fitToWindowAct);

  // Help menu:
  //------------
  helpMenu = menuBar()->addMenu(tr("&Help"));
  helpMenu->addAction(helpAct);
}

void CadView::helpSlot()
{
  QMessageBox::about(this,tr("ElmerGUI cad model viewer (beta)"),
		     tr("Use \"View->Fit to window\" to show the model using the whole screen\n"
			"Hold down crtl and use mouse to rotate/pan/zoom"));
}

void CadView::fitToWindow()
{
  qoccViewWidget->fitAll();
}


void CadView::fitToWindowSlot()
{
  this->fitToWindow();
}


void CadView::drawModel()
{
  qoccViewerContext->deleteAllObjects();
  const Handle_AIS_InteractiveContext& ic = qoccViewWidget->getContext();
  
  for(int i = 1; i <= shapes->Length(); i++) {
    Handle(AIS_Shape) anAISShape = new AIS_Shape(shapes->Value(i));
    ic->SetMaterial(anAISShape, Graphic3d_NOM_GOLD);
    // ic->SetMaterial(anAISShape, Graphic3d_NOM_DEFAULT);
    ic->SetColor(anAISShape, Quantity_NOC_RED);
    ic->SetDisplayMode(anAISShape, 1, Standard_False);
    ic->Display(anAISShape, Standard_False);
  }
  ic->UpdateCurrentViewer();
  qoccViewerContext->gridOff();
}


bool CadView::convertToSTL(QString fileName, QString fileSuffix)
{
  TopoDS_Shape shape;
  BRep_Builder builder;
  TopoDS_Compound res;
  Standard_Boolean result;
  STEPControl_Reader stepReader;
  StlAPI_Writer writer;
  IFSelect_ReturnStatus status;
  bool failsonly, ok;
  int nbs, nbr;
  
  if( (fileSuffix == "brep") ||
      (fileSuffix == "step") ||
      (fileSuffix == "stp") ) {
    
    // QApplication::setOverrideCursor(Qt::WaitCursor);
    
    // Read in BREP:
    //---------------
    if(fileSuffix == "brep") {
      
      result = BRepTools::Read(shape, fileName.toAscii().data(), builder);
      
      if(result) {
	shapes = new TopTools_HSequenceOfShape();
	shapes->Append(shape);
      }
    }
    
    // Read in STEP:
    //---------------
    if( (fileSuffix == "step") || 
	(fileSuffix == "stp") ) {
      
      status = stepReader.ReadFile(fileName.toAscii().data());
      
      if(status == IFSelect_RetDone) {	  
	
	// OCC62: Interface_TraceFile::SetDefault();
	failsonly = false;
	stepReader.PrintCheckLoad(failsonly, IFSelect_ItemsByEntity);
	
	nbr = stepReader.NbRootsForTransfer();
	stepReader.PrintCheckTransfer(failsonly, IFSelect_ItemsByEntity);
	
	for(Standard_Integer n = 1; n <= nbr; n++) {
	  
	  ok = stepReader.TransferRoot(n);
	  nbs = stepReader.NbShapes();
	  
	  if(nbs > 0) {
	    shapes = new TopTools_HSequenceOfShape();
	    for(int i = 1; i <= nbs; i++) {
	      shape = stepReader.Shape(i);
	      shapes->Append(shape);
	    }
	  }
	}
      }
    }
    
    if(shapes.IsNull() || shapes->IsEmpty()) {
      cout << "Failed to import cad file" << endl;
      cout.flush();
      return false;
    }
    
    // Write STL:
    //------------
    QString fileNameSTL = fileName + ".stl";
    
    builder.MakeCompound(res);
    
    for(int i = 1; i <= shapes->Length(); i++) {
      shape = shapes->Value(i);
      if(shape.IsNull()) {
	cout << "Failed to import cad file" << endl;
	cout.flush();
	return false;
      }
      builder.Add(res, shape);
    }
    
    writer.Write(res, fileNameSTL.toAscii().data());

    // QApplication::restoreOverrideCursor();

  } else {
    
    return false;
  }

  return true;
}
