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
 *  ELMER/Mesh3D pdepropertyeditor                                           *
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
#include "pdepropertyeditor.h"

using namespace std;

PDEPropertyEditor::PDEPropertyEditor(QWidget *parent)
  : QDialog(parent)
{
  equations = 0;
  equation = new equation_t[MAX_EQUATIONS];

  ui.setupUi(this);

  connect(ui.acceptEquation, SIGNAL(clicked()), this, SLOT(acceptButtonClicked()));
  connect(ui.deleteEquation, SIGNAL(clicked()), this, SLOT(deleteButtonClicked()));

}

PDEPropertyEditor::~PDEPropertyEditor()
{
  delete [] equation;
}

void PDEPropertyEditor::acceptButtonClicked()
{
  cout << "Accept" << endl;
  cout.flush();

  equation_t *eq = &equation[equations++];

  heatEquation_t he = eq->heatEquation;
  he.active = ui.heatEquationActive->checkState();
  he.convectionNone = ui.heatEquationConvectionNone->isChecked();
  he.convectionConstant = ui.heatEquationConvectionConstant->isChecked();
  he.convectionComputed = ui.heatEquationConvectionComputed->isChecked();
  he.phaseChangeSpatial1 = ui.heatEquationPCMNone->isChecked();
  he.phaseChangeSpatial2 = ui.heatEquationPCMSpatial1->isChecked();
  he.phaseChangeTemporal = ui.heatEquationPCMSpatial2->isChecked();
  he.latentHeatRelease = ui.heatEquationPCMLatentHeatRelease->checkState();

  linearElasticity_t le = eq->linearElasticity;
  le.active = ui.heatEquationActive->checkState();
  le.planeStress = ui.linearElasticityPlaneStress->checkState();

  navierStokes_t ns = eq->navierStokes;
  ns.active = ui.navierStokesActive->checkState();
  ns.turbulenceModelNone = ui.navierStokesTurbulenceModelNone->isChecked();
  ns.turbulenceModelKE = ui.navierStokesTurbulenceModelKE->isChecked();
  const QString &keClip = ui.navierStokesKEClipEdit->text();
  ns.keClip = keClip.toDouble();

  advectionDiffusion_t ad = eq->advectionDiffusion;
  ad.active = ui.advectionDiffusionActive->checkState();
  ad.convectionNone = ui.advectionDiffusionConvectionNone->isChecked();
  ad.convectionConstant = ui.advectionDiffusionConvectionConstant->isChecked();
  ad.convectionComputed = ui.advectionDiffusionConvectionComputed->isChecked();
  
  helmholtzEquation_t h = eq->helmholtzEquation;
  h.active = ui.helmholtzEquationActive->checkState();  
  const QString &angularFrequency = ui.helmholtzEquationAngularFrequencyEdit->text();
  h.angularFrequency = angularFrequency.toDouble();

  close();
}


void PDEPropertyEditor::deleteButtonClicked()
{
  cout << "Delete" << endl;
  cout.flush();

  close();
}
