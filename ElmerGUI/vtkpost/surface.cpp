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
 *  ElmerGUI surface                                                         *
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
#include "epmesh.h"
#include "vtkpost.h"
#include "surface.h"

using namespace std;

Surface::Surface(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(cancelButtonClicked()));
  connect(ui.applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  connect(ui.surfaceCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(surfaceSelectionChanged(int)));
  connect(ui.keepLimits, SIGNAL(stateChanged(int)), this, SLOT(keepLimitsSlot(int)));

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

Surface::~Surface()
{
}

void Surface::cancelButtonClicked()
{
  emit(hideSurfaceSignal());
  close();
}

void Surface::applyButtonClicked()
{
  emit(drawSurfaceSignal());
}

void Surface::okButtonClicked()
{
  emit(drawSurfaceSignal());
  close();
}

void Surface::populateWidgets(ScalarField *scalarField, int n)
{
  this->scalarField = scalarField;
  this->scalarFields = n;

  ui.surfaceCombo->clear();

  for(int i = 0; i < n; i++) {
    ScalarField *sf = &scalarField[i];
    ui.surfaceCombo->addItem(sf->name);
  }

  surfaceSelectionChanged(ui.surfaceCombo->currentIndex());
}

void Surface::surfaceSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  if(!ui.keepLimits->isChecked()) {
    ui.minEdit->setText(QString::number(sf->minVal));
    ui.maxEdit->setText(QString::number(sf->maxVal));
  }
}

void Surface::keepLimitsSlot(int state)
{
  if(state == 0)
    surfaceSelectionChanged(ui.surfaceCombo->currentIndex());
}
