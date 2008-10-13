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
 *  ElmerGUI isosurface                                                      *
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
#include "isosurface.h"

using namespace std;

IsoSurface::IsoSurface(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(cancelButtonClicked()));
  connect(ui.applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  connect(ui.contoursCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(contoursSelectionChanged(int)));
  connect(ui.colorCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(colorSelectionChanged(int)));

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

IsoSurface::~IsoSurface()
{
}

void IsoSurface::applyButtonClicked()
{
  emit(drawIsoSurfaceSignal());
}

void IsoSurface::cancelButtonClicked()
{
  emit(hideIsoSurfaceSignal());
  close();
}

void IsoSurface::okButtonClicked()
{
  emit(drawIsoSurfaceSignal());
  close();
}

void IsoSurface::populateWidgets(ScalarField *scalarField, int n)
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
}

void IsoSurface::contoursSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  ui.contoursMinEdit->setText(QString::number(sf->minVal));
  ui.contoursMaxEdit->setText(QString::number(sf->maxVal));
}

void IsoSurface::colorSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  ui.colorMinEdit->setText(QString::number(sf->minVal));
  ui.colorMaxEdit->setText(QString::number(sf->maxVal));
}
