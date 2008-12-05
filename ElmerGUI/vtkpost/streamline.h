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
 *  ElmerGUI streamline                                                      *
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

#ifndef STREAMLINE_H
#define STREAMLINE_H

#include <QWidget>
#include "ui_streamline.h"

class ScalarField;
class VtkPost;
class TimeStep;

class StreamLine : public QDialog
{
  Q_OBJECT

public:
  StreamLine(QWidget *parent = 0);
  ~StreamLine();

  Ui::streamLineDialog ui;

  void populateWidgets(VtkPost*);
  void draw(VtkPost*, TimeStep*);

signals:
  void drawStreamLineSignal();
  void hideStreamLineSignal();

public slots:
  QString GetFieldName();
  QString GetColorName();
  bool SetFieldName(QString);
  bool SetColorName(QString);
  void SetMaxTime(double);
  void SetStepLength(double);
  void SetThreads(int);
  void SetIntegStepLength(double);
  void SetUseSurfaceMesh(bool);
  void SetUseVolumeMesh(bool);
  void SetIntegrateForwards(bool);
  void SetIntegrateBackwards(bool);
  void SetMinColorVal(double);
  void SetMaxColorVal(double);
  void SetKeepColorLimits(bool);
  void SetDrawLines(bool);
  void SetDrawRibbons(bool);
  void SetLineWidth(int);
  void SetRibbonWidth(int);
  void SetSphereSource(bool);
  void SetLineSource(bool);
  void SetPointSource(bool);
  void SetSphereSourceX(double);
  void SetSphereSourceY(double);
  void SetSphereSourceZ(double);
  void SetSphereSourceRadius(double);
  void SetSphereSourcePoints(int);
  void SetLineSourceStartX(double);
  void SetLineSourceStartY(double);
  void SetLineSourceStartZ(double);
  void SetLineSourceEndX(double);
  void SetLineSourceEndY(double);
  void SetLineSourceEndZ(double);
  void SetLineSourcePoints(int);

private slots:
  void cancelButtonClicked();
  void okButtonClicked();
  void applyButtonClicked();
  void colorSelectionChanged(int);
  void keepLimitsSlot(int);

private:
  ScalarField *scalarField;
  int scalarFields;

};

#endif // STREAMLINE_H
