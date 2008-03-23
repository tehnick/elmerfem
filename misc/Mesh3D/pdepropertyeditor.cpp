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
  ui.setupUi(this);
 
  solverParameterEditor = new SolverParameterEditor[MAX_SOLVERS];

  menuAction = NULL;

  connect(ui.acceptEquation, SIGNAL(clicked()), 
	  this, SLOT(acceptButtonClicked()));

  connect(ui.deleteEquation, SIGNAL(clicked()), 
	  this, SLOT(deleteButtonClicked()));

  connect(ui.solverParametersButton, SIGNAL(clicked()),
	  this, SLOT(editNumericalMethods()));
}

PDEPropertyEditor::~PDEPropertyEditor()
{
}

void PDEPropertyEditor::acceptButtonClicked()
{
  cout << "Accept" << endl;
  cout.flush();

  emit(signalPdeEditorFinished(0, myId));
}


void PDEPropertyEditor::deleteButtonClicked()
{
  cout << "Delete" << endl;
  cout.flush();
  
  emit(signalPdeEditorFinished(1, myId));
}

void PDEPropertyEditor::startEdit(int id)
{
  myId = id;
  this->show();
}

void PDEPropertyEditor::defaultSettings()
{
  Qt::CheckState uc = Qt::Unchecked;

  ui.heatEquationActive->setCheckState(uc);
  ui.linearElasticityActive->setCheckState(uc);
  ui.navierStokesActive->setCheckState(uc);
  ui.heatEquationActive->setCheckState(uc);
  ui.advectionDiffusionActive->setCheckState(uc);
  ui.helmholtzEquationActive->setCheckState(uc);

  ui.pdeTabs->setCurrentIndex(0);

  ui.heatEquationConvectionNone->setChecked(true);
  ui.heatEquationPCMNone->setChecked(true);
  ui.heatEquationPCMLatentHeatRelease->setChecked(uc);

  ui.linearElasticityPlaneStress->setChecked(uc);

  ui.navierStokesCalculateHydrostaticPressure->setChecked(uc);
  ui.navierStokesTurbulenceModelNone->setChecked(true);
  ui.navierStokesKEClipEdit->setText("1.0e-6");

  ui.advectionDiffusionConvectionNone->setChecked(true);

  ui.helmholtzEquationAngularFrequencyEdit->setText("");
}

void PDEPropertyEditor::editNumericalMethods()
{
  const QString &equationName = ui.equationNameEdit->text();
    
  // determine which solver is currently selected in the Tab:
  int currentIndex = ui.pdeTabs->currentIndex();
  const QString &solverName = ui.pdeTabs->tabText(currentIndex);

  SolverParameterEditor *spe = &solverParameterEditor[currentIndex];
  spe->setWindowTitle("Solver control for " + solverName 
		      + " (" + equationName + ")");
  spe->show();
}
