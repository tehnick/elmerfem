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
 *  ElmerGUI timestep                                                        *
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
#include "timestep.h"

using namespace std;

TimeStep::TimeStep(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(cancelButtonClicked()));
  connect(ui.applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  connect(ui.loopButton, SIGNAL(clicked()), this, SLOT(loopButtonClicked()));

  maxSteps = 0;
  loopOn = false;

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

TimeStep::~TimeStep()
{
}

void TimeStep::cancelButtonClicked()
{
  close();
}

void TimeStep::okButtonClicked()
{
  emit(timeStepChangedSignal());
  close();
}

void TimeStep::applyButtonClicked()
{
  emit(timeStepChangedSignal());
}

void TimeStep::canProceedWithNext()
{
  if(!loopOn) return;

  int current = ui.timeStep->value();
  int stop = ui.stop->value();
  int increment = ui.increment->value();
  if(increment < 1) {
    increment = 1;
    ui.increment->setValue(increment);
  }
  if(stop > maxSteps) {
    stop = maxSteps;
    ui.stop->setValue(stop);
  }

  if(current > stop) {
    loopOn = false;
    ui.loopButton->setText("Loop");
    this->repaint();
  } else {
    ui.loopButton->setText("Stop");
    current += increment;
    if(current > stop) {
      loopOn = false;
      ui.loopButton->setText("Loop");
      this->repaint();
      return;
    }
    ui.timeStep->setValue(current);
    this->repaint();
    applyButtonClicked();
  }
}

void TimeStep::loopButtonClicked()
{
  if(loopOn) {
    loopOn = false;
    ui.loopButton->setText("Loop");
    this->repaint();
  } else {
    loopOn = true;
    ui.loopButton->setText("Stop");
    int start = ui.start->value();
    int stop = ui.stop->value();
    int increment = ui.increment->value();
    if(start < 1) {
      start = 1;
      ui.start->setValue(start);
    }
    if(start > maxSteps) {
      start = maxSteps;
      ui.start->setValue(start);
    }
    if(stop < 1) {
      stop = 1;
      ui.stop->setValue(stop);
    }
    if(stop > maxSteps) {
      stop = maxSteps;
      ui.stop->setValue(stop);
    }
    if(stop < start) {
      stop = start;
      ui.stop->setValue(stop);
    }
    if(increment < 1) {
      increment = 1;
      ui.increment->setValue(increment);
    }
    ui.timeStep->setValue(start);
    this->repaint();
    applyButtonClicked();
  }
}
