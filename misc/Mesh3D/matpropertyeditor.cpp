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
 *  ELMER/Mesh3D matpropertyeditor                                           *
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
#include "matpropertyeditor.h"

using namespace std;

MATPropertyEditor::MATPropertyEditor(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);
  
  menuAction = NULL;

  connect(ui.acceptEquation, SIGNAL(clicked()), this, SLOT(acceptButtonClicked()));
  connect(ui.deleteEquation, SIGNAL(clicked()), this, SLOT(deleteButtonClicked()));
}

MATPropertyEditor::~MATPropertyEditor()
{
}

void MATPropertyEditor::acceptButtonClicked()
{
  cout << "Accept" << endl;
  cout.flush();

  emit(signalMatEditorFinished(0, myId));
}


void MATPropertyEditor::deleteButtonClicked()
{
  cout << "Delete" << endl;
  cout.flush();
  
  emit(signalMatEditorFinished(1, myId));
}

void MATPropertyEditor::startEdit(int id)
{
  myId = id;
  this->show();
}

void MATPropertyEditor::defaultSettings()
{
}
