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
 *  ELMER/Mesh3D propertyeditor                                              *
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
#include "propertyeditor.h"

using namespace std;

PropertyEditor::PropertyEditor(QWidget *parent)
  : QWidget(parent)
{
  setWindowFlags(Qt::Window);

  textEdit = new QTextEdit;
  textEdit->setLineWrapMode(QTextEdit::NoWrap);

  connect(textEdit, SIGNAL(textChanged()), this, SLOT(propertiesChanged()));

  clearButton = new QPushButton(tr("Clear"));
  connect(clearButton, SIGNAL(clicked()), this, SLOT(clearProperties()));

  closeButton = new QPushButton(tr("Close"));
  connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));
  
  QVBoxLayout *layout = new QVBoxLayout;
  layout->addWidget(textEdit);
  layout->addWidget(clearButton);
  layout->addWidget(closeButton);
  setLayout(layout);
  
  setWindowTitle(tr("Property editor"));
}

PropertyEditor::~PropertyEditor()
{
}

QSize PropertyEditor::minimumSizeHint() const
{
  return QSize(64, 64);
}


QSize PropertyEditor::sizeHint() const
{
  return QSize(400, 100);
}


void PropertyEditor::editProperties(int bcIndex)
{
  if(bcIndex >= MAX_BCS) {
    cout << "propertyeditor: index " << bcIndex << " too large" << endl;
    cout << "Increase MAX_BCS and recompile" << endl;
    cout.flush();
    exit(0);
  }

  this->bcIndex = bcIndex;

  QString qs = bcPropertyTable[bcIndex];
  textEdit->clear();
  textEdit->append(qs);

  this->show();
}


void PropertyEditor::propertiesChanged()
{
  bcPropertyTable[bcIndex] = textEdit->toPlainText();
}


void PropertyEditor::clearProperties()
{
  textEdit->clear();
}
