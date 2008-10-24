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
 *  ElmerGUI readfile                                                        *
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
#include "readfile.h"

using namespace std;

ReadFile::ReadFile(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.browseButton, SIGNAL(clicked()), this, SLOT(browseButtonClickedSlot()));
  connect(ui.applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClickedSlot()));
  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(cancelButtonClickedSlot()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClickedSlot()));
  connect(ui.allButton, SIGNAL(clicked()), this, SLOT(allButtonClickedSlot()));

  ui.nodesEdit->setEnabled(false);
  ui.elementsEdit->setEnabled(false);
  ui.timestepsEdit->setEnabled(false);
  ui.dofsEdit->setEnabled(false);

  setWindowTitle("Read input file");
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

ReadFile::~ReadFile()
{
}

void ReadFile::browseButtonClickedSlot()
{
  QString fileName = QFileDialog::getOpenFileName(this, tr("Select input file"), "", tr("Ep files (*.ep)"));

  ui.fileName->setText(fileName.trimmed());

  readHeader();
}

void ReadFile::applyButtonClickedSlot()
{
  QString fileName = ui.fileName->text().trimmed();
  
  if(fileName.isEmpty()) return;

  int start = ui.start->value();
  int end = ui.end->value();
  int maxSteps = ui.timestepsEdit->text().toInt();

  if(end > maxSteps) {
    end = maxSteps;
    ui.end->setValue(maxSteps);
  }

  if(start > end) {
    start = end;
    ui.start->setValue(start);
  }
  
  repaint();

  emit(readPostFileSignal(fileName));
}

void ReadFile::cancelButtonClickedSlot()
{
  close();
}

void ReadFile::okButtonClickedSlot()
{
  applyButtonClickedSlot();
  cancelButtonClickedSlot();
}

void ReadFile::readHeader()
{ 
  QString fileName = ui.fileName->text().trimmed();

  QFile postFile(fileName);
  
  if(!postFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
    ui.fileName->setText("");
    return;
  }

  QTextStream post(&postFile);

  QTextStream txtStream;
  QString tmpLine = post.readLine().trimmed();
  while(tmpLine.isEmpty() || (tmpLine.at(0) == '#'))
    tmpLine = post.readLine().trimmed();
  txtStream.setString(&tmpLine);

  int nodes, elements, timesteps, components;
  txtStream >> nodes >> elements >> components >> timesteps;

  postFile.close();

  ui.nodesEdit->setText(QString::number(nodes));
  ui.elementsEdit->setText(QString::number(elements));
  ui.timestepsEdit->setText(QString::number(timesteps));
  ui.dofsEdit->setText(QString::number(components));
}

void ReadFile::allButtonClickedSlot()
{
  ui.start->setValue(1);
  ui.end->setValue(ui.timestepsEdit->text().toInt());

  repaint();
}
