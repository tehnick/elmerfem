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
 *  ElmerGUI generalsetup                                                    *
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
#include "generalsetup.h"

using namespace std;

GeneralSetup::GeneralSetup(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.acceptButton, SIGNAL(clicked()), 
	  this, SLOT(acceptButtonClicked()));

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

GeneralSetup::~GeneralSetup()
{
}

void GeneralSetup::acceptButtonClicked()
{
  this->close();
}

void GeneralSetup::appendToProjectDoc(QDomDocument *projectDoc)
{
  // General setup block:
  //======================
  QDomElement gs = projectDoc->createElement("generalsetup");
  projectDoc->documentElement().appendChild(gs);
  
  // Header block:
  //---------------
  QDomElement header = projectDoc->createElement("header");
  gs.appendChild(header);

  QDomElement checkKeywordsWarn = projectDoc->createElement("checkkeywordswarn");
  bool value = ui.checkKeywordsWarn->isChecked();
  QDomText checkKeywordsWarnValue = projectDoc->createTextNode(QString::number(value));
  checkKeywordsWarn.appendChild(checkKeywordsWarnValue);
  header.appendChild(checkKeywordsWarn);
  
  QDomElement meshDB1 = projectDoc->createElement("meshdb1");
  QDomText meshDB1Value = projectDoc->createTextNode(ui.meshDBEdit1->text().trimmed());
  meshDB1.appendChild(meshDB1Value);
  header.appendChild(meshDB1);

  QDomElement meshDB2 = projectDoc->createElement("meshdb2");
  QDomText meshDB2Value = projectDoc->createTextNode(ui.meshDBEdit2->text().trimmed());
  meshDB2.appendChild(meshDB2Value);
  header.appendChild(meshDB2);

  QDomElement incPath = projectDoc->createElement("includepath");
  QDomText incPathValue = projectDoc->createTextNode(ui.includePathEdit->text().trimmed());
  incPath.appendChild(incPathValue);
  header.appendChild(incPath);

  QDomElement resDir = projectDoc->createElement("resultsdirectory");
  QDomText resDirValue = projectDoc->createTextNode(ui.resultsDirectoryEdit->text().trimmed());
  resDir.appendChild(resDirValue);
  header.appendChild(resDir);

  // Simulation block:
  //------------------
  QDomElement simulation = projectDoc->createElement("simulation");
  gs.appendChild(simulation);

  QDomElement maxOutLevel = projectDoc->createElement("maxoutputlevel");
  QDomText maxOutLevelValue = projectDoc->createTextNode(QString::number(ui.maxOutputLevelCombo->currentIndex()));
  maxOutLevel.appendChild(maxOutLevelValue);
  simulation.appendChild(maxOutLevel);

  QDomElement coordinateSystem = projectDoc->createElement("coordinatesystem");
  QDomText coordinateSystemValue = projectDoc->createTextNode(QString::number(ui.coordinateSystemCombo->currentIndex()));
  coordinateSystem.appendChild(coordinateSystemValue);
  simulation.appendChild(coordinateSystem);

  QDomElement coordinateMapping = projectDoc->createElement("coordinatemapping");
  QDomText coordinateMappingValue = projectDoc->createTextNode(ui.coordinateMappingEdit->text().trimmed());
  coordinateMapping.appendChild(coordinateMappingValue);
  simulation.appendChild(coordinateMapping);

  QDomElement simulationType = projectDoc->createElement("simulationtype");
  QDomText simulationTypeValue = projectDoc->createTextNode(QString::number(ui.simulationTypeCombo->currentIndex()));
  simulationType.appendChild(simulationTypeValue);
  simulation.appendChild(simulationType);

  QDomElement outputIntervals = projectDoc->createElement("outputintervals");
  QDomText outputIntervalsValue = projectDoc->createTextNode(ui.outputIntervalsEdit->text().trimmed());
  outputIntervals.appendChild(outputIntervalsValue);
  simulation.appendChild(outputIntervals);

  QDomElement ssMaxIter = projectDoc->createElement("steadystatemaxiter");
  QDomText ssMaxIterValue = projectDoc->createTextNode(ui.steadyStateMaxIterEdit->text().trimmed());
  ssMaxIter.appendChild(ssMaxIterValue);
  simulation.appendChild(ssMaxIter);

  QDomElement tsMethod = projectDoc->createElement("timesteppipngmethod");
  QDomText tsMethodValue = projectDoc->createTextNode(QString::number(ui.timesteppingMethodCombo->currentIndex()));
  tsMethod.appendChild(tsMethodValue);
  simulation.appendChild(tsMethod);

  QDomElement bdfOrder = projectDoc->createElement("bdforder");
  QDomText bdfOrderValue = projectDoc->createTextNode(QString::number(ui.bdfOrderCombo->currentIndex()));
  bdfOrder.appendChild(bdfOrderValue);
  simulation.appendChild(bdfOrder);

  QDomElement tsIntervals = projectDoc->createElement("timestepintervals");
  QDomText tsIntervalsValue = projectDoc->createTextNode(ui.timeStepIntervalsEdit->text().trimmed());
  tsIntervals.appendChild(tsIntervalsValue);
  simulation.appendChild(tsIntervals);

  QDomElement tsSizes = projectDoc->createElement("timestepsizes");
  QDomText tsSizesValue = projectDoc->createTextNode(ui.timestepSizesEdit->text().trimmed());
  tsSizes.appendChild(tsSizesValue);
  simulation.appendChild(tsSizes);

  QDomElement sif = projectDoc->createElement("solverinputfile");
  QDomText sifValue = projectDoc->createTextNode(ui.solverInputFileEdit->text().trimmed());
  sif.appendChild(sifValue);
  simulation.appendChild(sif);

  QDomElement ep = projectDoc->createElement("postfile");
  QDomText epValue = projectDoc->createTextNode(ui.postFileEdit->text().trimmed());
  ep.appendChild(epValue);
  simulation.appendChild(ep);

  // Constants block:
  //------------------
  QDomElement constants = projectDoc->createElement("constants");
  gs.appendChild(constants);

  QDomElement gravity = projectDoc->createElement("gravity");
  QDomText gravityValue = projectDoc->createTextNode(ui.gravityEdit->text().trimmed());
  gravity.appendChild(gravityValue);
  constants.appendChild(gravity);
  
  QDomElement sb = projectDoc->createElement("stefanboltzmann");
  QDomText sbValue = projectDoc->createTextNode(ui.stefanBoltzmannEdit->text().trimmed());
  sb.appendChild(sbValue);
  constants.appendChild(sb);
  
  QDomElement vc = projectDoc->createElement("vacuumpermittivity");
  QDomText vcValue = projectDoc->createTextNode(ui.vacuumPermittivityEdit->text().trimmed());
  vc.appendChild(vcValue);
  constants.appendChild(vc);
  
  QDomElement b = projectDoc->createElement("boltzmann");
  QDomText bValue = projectDoc->createTextNode(ui.boltzmannEdit->text().trimmed());
  b.appendChild(bValue);
  constants.appendChild(b);
  
  QDomElement uc = projectDoc->createElement("unitcharge");
  QDomText ucValue = projectDoc->createTextNode(ui.unitChargeEdit->text().trimmed());
  uc.appendChild(ucValue);
  constants.appendChild(uc);
}

void GeneralSetup::readFromProjectDoc(QDomDocument *projectDoc)
{
  QDomElement gs = projectDoc->documentElement().firstChildElement("generalsetup");

  // Header block:
  //---------------
  QDomElement header = gs.firstChildElement("header");

  QDomElement checkKeywordsWarn = header.firstChildElement("checkkeywordswarn");
  bool checkKeywordsWarnValue = (checkKeywordsWarn.text().toInt() > 0);
  ui.checkKeywordsWarn->setChecked(checkKeywordsWarnValue);

  QDomElement meshDB1 = header.firstChildElement("meshdb1");
  ui.meshDBEdit1->setText(meshDB1.text().trimmed());

  QDomElement meshDB2 = header.firstChildElement("meshdb2");
  ui.meshDBEdit2->setText(meshDB2.text().trimmed());

  QDomElement incPath = header.firstChildElement("includepath");
  ui.includePathEdit->setText(incPath.text().trimmed());

  QDomElement resDir = header.firstChildElement("resultsdirectory");
  ui.resultsDirectoryEdit->setText(resDir.text().trimmed());

  // Simulation block:
  //------------------
  QDomElement simulation = gs.firstChildElement("simulation");

  QDomElement maxOutLevel = simulation.firstChildElement("maxoutputlevel");
  ui.maxOutputLevelCombo->setCurrentIndex(maxOutLevel.text().trimmed().toInt());

  QDomElement coordinateSystem = simulation.firstChildElement("coordinatesystem");
  ui.coordinateSystemCombo->setCurrentIndex(coordinateSystem.text().trimmed().toInt());

  QDomElement coordinateMapping = simulation.firstChildElement("coordinatemapping");
  ui.coordinateMappingEdit->setText(coordinateMapping.text().trimmed());

  QDomElement simulationType = simulation.firstChildElement("simulationtype");
  ui.simulationTypeCombo->setCurrentIndex(simulationType.text().trimmed().toInt());

  QDomElement outputIntervals = simulation.firstChildElement("outputintervals");
  ui.outputIntervalsEdit->setText(outputIntervals.text().trimmed());

  QDomElement ssMaxIter = simulation.firstChildElement("steadystatemaxiter");
  ui.steadyStateMaxIterEdit->setText(ssMaxIter.text().trimmed());

  QDomElement tsMethod = simulation.firstChildElement("timesteppipngmethod");
  ui.timesteppingMethodCombo->setCurrentIndex(tsMethod.text().trimmed().toInt());

  QDomElement bdfOrder = simulation.firstChildElement("bdforder");
  ui.bdfOrderCombo->setCurrentIndex(bdfOrder.text().trimmed().toInt());

  QDomElement tsIntervals = simulation.firstChildElement("timestepintervals");
  ui.timeStepIntervalsEdit->setText(tsIntervals.text().trimmed());

  QDomElement tsSizes = simulation.firstChildElement("timestepsizes");
  ui.timestepSizesEdit->setText(tsSizes.text().trimmed());

  QDomElement sif = simulation.firstChildElement("solverinputfile");
  ui.solverInputFileEdit->setText(sif.text().trimmed());

  QDomElement ep = simulation.firstChildElement("postfile");
  ui.postFileEdit->setText(ep.text().trimmed());

  // Constants block:
  //------------------
  QDomElement constants = gs.firstChildElement("constants");

  QDomElement gravity = constants.firstChildElement("gravity");
  ui.gravityEdit->setText(gravity.text().trimmed());
  
  QDomElement sb = constants.firstChildElement("stefanboltzmann");
  ui.stefanBoltzmannEdit->setText(sb.text().trimmed());
  
  QDomElement vc = constants.firstChildElement("vacuumpermittivity");
  ui.vacuumPermittivityEdit->setText(vc.text().trimmed());
  
  QDomElement b = constants.firstChildElement("boltzmann");
  ui.boltzmannEdit->setText(b.text().trimmed());
  
  QDomElement uc = constants.firstChildElement("unitcharge");
  ui.unitChargeEdit->setText(uc.text().trimmed());
}
