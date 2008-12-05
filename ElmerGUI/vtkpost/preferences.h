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
 *  ElmerGUI preferences                                                     *
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

#ifndef PREFERENCES_H
#define PREFERENCES_H

#include <QWidget>
#include "ui_preferences.h"

class Preferences : public QDialog
{
  Q_OBJECT

public:
  Preferences(QWidget *parent = 0);
  ~Preferences();

  Ui::preferencesDialog ui;

signals:
  void redrawSignal();

public slots:
  void SetSurfaceMeshForPoints(bool);
  void SetVolumeMeshForPoints(bool);
  void SetPointSize(int);
  void SetPointQuality(int);
  void SetClipPlaneForPoints(bool);
  void SetSurfaceMeshForEdges(bool);
  void SetVolumeMeshForEdges(bool);
  void SetTubeFilterForEdges(bool);
  void SetClipPlaneForEdges(bool);
  void SetLineWidthForEdges(int);
  void SetTubeQualityForEdges(int);
  void SetTubeRadiusForEdges(int);
  void SetSurfaceMeshForFeatureEdges(bool);
  void SetVolumeMeshForFeatureEdges(bool);
  void SetTubeFilterForFeatureEdges(bool);
  void SetClipPlaneForFeatureEdges(bool);
  void SetDrawBoundaryEdges(bool);
  int GetFeatureAngle();
  void SetFeatureAngle(int);
  void SetLineWidthForFeatureEdges(int);
  void SetTubeQualityForFeatureEdges(int);
  void SetTubeRadiusForFeatureEdges(int);
  void SetClipPlaneOx(double);
  void SetClipPlaneOy(double);
  void SetClipPlaneOz(double);
  void SetClipPlaneNx(double);
  void SetClipPlaneNy(double);
  void SetClipPlaneNz(double);

private slots:
  void okButtonClicked();
  void cancelButtonClicked();
  void applyButtonClicked();

private:

};

#endif // PREFERENCES_H
