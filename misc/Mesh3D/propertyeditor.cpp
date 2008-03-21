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
  : QDialog(parent)
{
  heatEquationActive = false;
  linearElasticityActive = false;

  for(int i = 0; i < MAX_BCS; i++)
    bcProperty[i].defined = false;

  maxindex = MAX_BCS;

  ui.setupUi(this);

  connect(ui.temperatureEdit,   SIGNAL(textChanged(const QString&)), this, SLOT(temperatureChanged(const QString&)));
  connect(ui.heatFluxEdit,      SIGNAL(textChanged(const QString&)), this, SLOT(heatFluxChanged(const QString&)));
  connect(ui.displacement1Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement1Changed(const QString&)));
  connect(ui.displacement2Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement2Changed(const QString&)));
  connect(ui.displacement3Edit, SIGNAL(textChanged(const QString&)), this, SLOT(displacement3Changed(const QString&)));
}

PropertyEditor::~PropertyEditor()
{
}

void PropertyEditor::editProperties(int bcIndex)
{
  this->bcIndex = bcIndex;

  if(heatEquationActive) 
    ui.heatEquation->setEnabled(true);
  else
    ui.heatEquation->setEnabled(false);

  if(linearElasticityActive)
    ui.linearElasticity->setEnabled(true);
  else
    ui.linearElasticity->setEnabled(false);

  bcProperty_t *bp = &bcProperty[bcIndex];

  ui.temperatureEdit   -> setText(bp->temperature);
  ui.heatFluxEdit      -> setText(bp->heatFlux);
  ui.displacement1Edit -> setText(bp->displacement1);
  ui.displacement2Edit -> setText(bp->displacement2);
  ui.displacement3Edit -> setText(bp->displacement3);

  this->show();
}

void PropertyEditor::temperatureChanged(const QString& qs)
{
  bcProperty_t *bp = &bcProperty[bcIndex];
  bp->defined = true;
  bp->temperature = qs;
}

void PropertyEditor::heatFluxChanged(const QString& qs)
{
  bcProperty_t *bp = &bcProperty[bcIndex];
  bp->defined = true;
  bp->heatFlux = qs;
}

void PropertyEditor::displacement1Changed(const QString& qs)
{
  bcProperty_t *bp = &bcProperty[bcIndex];
  bp->defined = true;
  bp->displacement1 = qs;
}

void PropertyEditor::displacement2Changed(const QString& qs)
{
  bcProperty_t *bp = &bcProperty[bcIndex];
  bp->defined = true;
  bp->displacement2 = qs;
}

void PropertyEditor::displacement3Changed(const QString& qs)
{
  bcProperty_t *bp = &bcProperty[bcIndex];
  bp->defined = true;
  bp->displacement3 = qs;
}
