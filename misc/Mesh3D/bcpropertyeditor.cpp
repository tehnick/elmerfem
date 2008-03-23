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
#include "bcpropertyeditor.h"

using namespace std;

BCPropertyEditor::BCPropertyEditor(QWidget *parent)
  : QDialog(parent)
{
  maxindex = MAX_BCS;

  bcEditActive = false;
  heatEquationActive = true;
  linearElasticityActive = true;

  for(int i = 0; i < MAX_BCS; i++)
    bcProperty[i].defined = false;

  ui.setupUi(this);

  connect(ui.temperatureEdit,   SIGNAL(textChanged(const QString&)), this, SLOT(temperatureChanged(const QString&)));
  connect(ui.heatFluxEdit,      SIGNAL(textChanged(const QString&)), this, SLOT(heatFluxChanged(const QString&)));
  connect(ui.displacement1Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement1Changed(const QString&)));
  connect(ui.displacement2Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement2Changed(const QString&)));
  connect(ui.displacement3Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement3Changed(const QString&)));
}

BCPropertyEditor::~BCPropertyEditor()
{
}

void BCPropertyEditor::editProperties(int bcIndex)
{
  this->bcIndex = bcIndex;

  updateActiveSheets();

  bcProperty_t *bc = &bcProperty[bcIndex];

  ui.temperatureEdit   -> setText(bc->temperature);
  ui.heatFluxEdit      -> setText(bc->heatFlux);
  ui.displacement1Edit -> setText(bc->displacement1);
  ui.displacement2Edit -> setText(bc->displacement2);
  ui.displacement3Edit -> setText(bc->displacement3);

  this->show();
}

void BCPropertyEditor::updateActiveSheets()
{
  if(heatEquationActive) 
    ui.heatEquation->setEnabled(true);
  else
    ui.heatEquation->setEnabled(false);
  
  if(linearElasticityActive)
    ui.linearElasticity->setEnabled(true);
  else
    ui.linearElasticity->setEnabled(false);
}

void BCPropertyEditor::temperatureChanged(const QString& qs)
{
  bcProperty_t *bc = &bcProperty[bcIndex];
  bc->defined = true;
  bc->temperature = qs;
}

void BCPropertyEditor::heatFluxChanged(const QString& qs)
{
  bcProperty_t *bc = &bcProperty[bcIndex];
  bc->defined = true;
  bc->heatFlux = qs;
}

void BCPropertyEditor::displacement1Changed(const QString& qs)
{
  bcProperty_t *bc = &bcProperty[bcIndex];
  bc->defined = true;
  bc->displacement1 = qs;
}

void BCPropertyEditor::displacement2Changed(const QString& qs)
{
  bcProperty_t *bc = &bcProperty[bcIndex];
  bc->defined = true;
  bc->displacement2 = qs;
}

void BCPropertyEditor::displacement3Changed(const QString& qs)
{
  bcProperty_t *bc = &bcProperty[bcIndex];
  bc->defined = true;
  bc->displacement3 = qs;
}
