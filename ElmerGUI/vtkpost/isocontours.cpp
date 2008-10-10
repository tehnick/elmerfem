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
 *  ElmerGUI isocontours                                                     *
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
#include "isocontours.h"
#include "vtkpost.h"

using namespace std;

IsoContours::IsoContours(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(close()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  connect(ui.contoursCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(contoursSelectionChanged(int)));
  connect(ui.colorCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(colorSelectionChanged(int)));

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

IsoContours::~IsoContours()
{
}

void IsoContours::okButtonClicked()
{
  emit(drawIsoContourSignal());
}

void IsoContours::populateWidgets(ScalarField *scalarField, int n)
{
  this->scalarField = scalarField;
  this->scalarFields = n;

  ui.contoursCombo->clear();
  ui.colorCombo->clear();

  for(int i = 0; i < n; i++) {
    ScalarField *sf = &scalarField[i];
    ui.contoursCombo->addItem(sf->name);
    ui.colorCombo->addItem(sf->name);
  }

  ui.contoursMinEdit->setText(QString::number(scalarField->minVal));
  ui.contoursMaxEdit->setText(QString::number(scalarField->maxVal));
  ui.colorMinEdit->setText(QString::number(scalarField->minVal));
  ui.colorMaxEdit->setText(QString::number(scalarField->maxVal));
  ui.colorCombo->clear();
  for(int i = 0; i < n; i++) {
    ScalarField *sf = &scalarField[i];
    ui.colorCombo->addItem(sf->name);
  }
}

void IsoContours::contoursSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  ui.contoursMinEdit->setText(QString::number(sf->minVal));
  ui.contoursMaxEdit->setText(QString::number(sf->maxVal));
}

void IsoContours::colorSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  ui.colorMinEdit->setText(QString::number(sf->minVal));
  ui.colorMaxEdit->setText(QString::number(sf->maxVal));
}
