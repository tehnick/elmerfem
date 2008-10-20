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
 *  ElmerGUI vector                                                          *
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
#include "vector.h"
#include "timestep.h"

#include <vtkUnstructuredGrid.h>
#include <vtkPointData.h>
#include <vtkFloatArray.h>
#include <vtkGlyph3D.h>
#include <vtkArrowSource.h>
#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkActor.h>

using namespace std;

Vector::Vector(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.cancelButton, SIGNAL(clicked()), this, SLOT(cancelButtonClicked()));
  connect(ui.applyButton, SIGNAL(clicked()), this, SLOT(applyButtonClicked()));
  connect(ui.okButton, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  connect(ui.colorCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(colorSelectionChanged(int)));
  connect(ui.keepLimits, SIGNAL(stateChanged(int)), this, SLOT(keepLimitsSlot(int)));

  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

Vector::~Vector()
{
}

void Vector::applyButtonClicked()
{
  emit(drawVectorSignal());
}

void Vector::cancelButtonClicked()
{
  emit(hideVectorSignal());
  close();
}

void Vector::okButtonClicked()
{
  emit(drawVectorSignal());
  close();
}

void Vector::populateWidgets(VtkPost *vtkPost)
{
  this->scalarField = vtkPost->GetScalarField();
  this->scalarFields = vtkPost->GetScalarFields();

  ui.vectorCombo->clear();

  int index = -1;
  for(int i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    QString name = sf->name;
    if((index = name.indexOf("_x")) >= 0) {
      ui.vectorCombo->addItem(name.mid(0, index));
    }
  }

  QString name = ui.colorCombo->currentText();

  ui.colorCombo->clear();

  for(int i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    ui.colorCombo->addItem(sf->name);
  }

  for(int i = 0; i < ui.colorCombo->count(); i++) {
    if(ui.colorCombo->itemText(i) == name)
      ui.colorCombo->setCurrentIndex(i);
  }

  colorSelectionChanged(ui.colorCombo->currentIndex());
}

void Vector::colorSelectionChanged(int newIndex)
{
  ScalarField *sf = &this->scalarField[newIndex];
  if(!ui.keepLimits->isChecked()) {
    ui.minVal->setText(QString::number(sf->minVal));
    ui.maxVal->setText(QString::number(sf->maxVal));
  }
}

void Vector::keepLimitsSlot(int state)
{
  if(state == 0)
    colorSelectionChanged(ui.colorCombo->currentIndex());
}

void Vector::draw(VtkPost* vtkPost, TimeStep* timeStep)
{
  QString vectorName = ui.vectorCombo->currentText();

  if(vectorName.isEmpty()) return;

  int i, j, index = -1;
  for(i = 0; i < scalarFields; i++) {
    ScalarField *sf = &scalarField[i];
    QString name = sf->name;
    if((j = name.indexOf("_x")) >= 0) {
      if(vectorName == name.mid(0, j)) {
	index = i;
	break;
      }
    }
  }

  if(index < 0) return;

  int colorIndex = ui.colorCombo->currentIndex();
  QString colorName = ui.colorCombo->currentText();
  double minVal = ui.minVal->text().toDouble();
  double maxVal = ui.maxVal->text().toDouble();
  int quality = ui.qualitySpin->value();
  int scaleMultiplier = ui.scaleSpin->value();
  bool scaleByMagnitude = ui.scaleByMagnitude->isChecked();

  int step = timeStep->ui.timeStep->value();
  if(step > timeStep->maxSteps) step = timeStep->maxSteps;
  int offset = vtkPost->NofNodes() * (step - 1);

  // Vector data:
  //-------------
  vtkPost->GetVolumeGrid()->GetPointData()->RemoveArray("VectorData");
  vtkFloatArray *vectorData = vtkFloatArray::New();
  ScalarField* sf_x = &scalarField[index + 0];
  ScalarField* sf_y = &scalarField[index + 1];
  ScalarField* sf_z = &scalarField[index + 2];
  vectorData->SetNumberOfComponents(3);
  vectorData->SetNumberOfTuples(vtkPost->NofNodes());
  vectorData->SetName("VectorData");
  double scaleFactor = 0.0;
  for(int i = 0; i < vtkPost->NofNodes(); i++) {
    double val_x  = sf_x->value[i + offset];
    double val_y  = sf_y->value[i + offset];
    double val_z  = sf_z->value[i + offset];
    double absval = sqrt(val_x*val_x + val_y*val_y + val_z*val_z);
    if(absval > scaleFactor) scaleFactor = absval;
    vectorData->SetComponent(i, 0, val_x); 
    vectorData->SetComponent(i, 1, val_y); 
    vectorData->SetComponent(i, 2, val_z); 
  }
  vtkPost->GetVolumeGrid()->GetPointData()->AddArray(vectorData);

  // Size of volume grid:
  //---------------------
  double length = vtkPost->GetVolumeGrid()->GetLength();
  if(scaleByMagnitude)
    scaleFactor = scaleFactor * 100.0 / length;

  // Color data:
  //-------------
  vtkPost->GetVolumeGrid()->GetPointData()->RemoveArray("VectorColor");
  ScalarField* sf = &scalarField[colorIndex];
  vtkFloatArray *vectorColor = vtkFloatArray::New();
  vectorColor->SetNumberOfComponents(1);
  vectorColor->SetNumberOfTuples(vtkPost->NofNodes());
  vectorColor->SetName("VectorColor");
  for(int i = 0; i < vtkPost->NofNodes(); i++) 
    vectorColor->SetComponent(i, 0, sf->value[i + offset]); 
  vtkPost->GetVolumeGrid()->GetPointData()->AddArray(vectorColor);

  // Glyphs:
  //---------
  vtkPost->GetVolumeGrid()->GetPointData()->SetActiveVectors("VectorData"); 
  vtkGlyph3D* glyph = vtkGlyph3D::New();
  vtkArrowSource* arrow = vtkArrowSource::New();
  arrow->SetTipResolution(quality);
  arrow->SetShaftResolution(quality);
  glyph->SetInput(vtkPost->GetVolumeGrid());
  glyph->SetSourceConnection(arrow->GetOutputPort());
  glyph->SetVectorModeToUseVector();

  if(scaleByMagnitude) {
    glyph->SetScaleFactor(scaleMultiplier / scaleFactor);
    glyph->SetScaleModeToScaleByVector();
  } else {
    glyph->SetScaleFactor(scaleMultiplier * length  / 100.0);
    glyph->ScalingOn();
  }
  glyph->SetColorModeToColorByScale();
  
  vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection(glyph->GetOutputPort());
  mapper->SetScalarModeToUsePointFieldData();
  mapper->ScalarVisibilityOn();
  mapper->SetScalarRange(minVal, maxVal);
  mapper->SelectColorArray("VectorColor");
  mapper->SetLookupTable(vtkPost->GetCurrentLut());
  // mapper->ImmediateModeRenderingOn();

  vtkPost->GetVectorActor()->SetMapper(mapper);
  vtkPost->SetCurrentVectorName(colorName);

  mapper->Delete();
  arrow->Delete();
  glyph->Delete();
  vectorColor->Delete();
  vectorData->Delete();
}
