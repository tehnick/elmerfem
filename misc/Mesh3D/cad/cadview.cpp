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
  setWindowTitle("ElmerGUI cad model view (qocc)");

#ifdef OCC62
  myVC  = new QoccViewerContext();
  myOCC = new QoccViewWidget(myVC->getContext(), this);

  this->setCentralWidget(myOCC);
#endif

  cout << "Cad model view window set up" << endl;
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


void CadView::drawModel()
{
#ifdef OCC62
    myVC->deleteAllObjects();
    const Handle_AIS_InteractiveContext& ic = myOCC->getContext();
      
    for(int i = 1; i <= shapes->Length(); i++) {
		Handle(AIS_Shape) anAISShape = new AIS_Shape(shapes->Value(i));
		ic->SetMaterial(anAISShape, Graphic3d_NOM_GOLD);
		ic->SetColor(anAISShape, Quantity_NOC_RED);
		ic->SetDisplayMode(anAISShape, 1, Standard_False);
		ic->Display(anAISShape, Standard_False);
    }
    ic->UpdateCurrentViewer();
    myVC->gridOff();
#endif
}
