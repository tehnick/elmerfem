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
 *  ElmerGUI vtkpost                                                         *
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

#ifndef VTKPOST_H
#define VTKPOST_H

#include <QMainWindow>
#include <QHash>

class EpMesh;
class ScalarField;
class QVTKWidget;
class vtkRenderer;
class vtkActor;
class vtkScalarBarActor;
class vtkDataSetMapper;
class vtkTextActor;
class vtkUnstructuredGrid;
class IsoSurface;
class IsoContour;
class ColorBar;

class VtkPost : public QMainWindow
{
  Q_OBJECT

public:
  VtkPost(QWidget *parent = 0);
  ~VtkPost();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;
  bool readPostFile(QString);

signals:

public slots:

public slots:
  void drawIsoContourSlot();
  void drawIsoSurfaceSlot();

private slots:
  void exitSlot();
  void drawScalarOnSurfaceSlot(QAction*);
  void redrawSlot();
  void groupChangedSlot(QAction*);
  void drawWireframeSlot();
  void showColorBarDialogSlot();
  void drawColorBarSlot();
  void drawFieldNameSlot();
  void showIsoContourDialogSlot();
  void showIsoSurfaceDialogSlot();
  void drawFeatureEdgesSlot();

private:
  QMenu *fileMenu;
  QMenu *editMenu;
  QMenu *editGroupsMenu;
  QMenu *viewMenu;
  QMenu *viewScalarMenu;

  QAction *exitAct;
  QAction *redrawAct;
  QAction *drawWireframeAct;
  QAction *drawColorBarAct;
  QAction *drawFieldNameAct;
  QAction *drawIsoContourAct;
  QAction *drawIsoSurfaceAct;
  QAction *drawFeatureEdgesAct;

  void createActions();
  void createMenus();
  void createToolbars();
  void createStatusBar();

  EpMesh *epMesh;
  QString postFileName;
  bool postFileRead;
  int scalarFields;
  ScalarField *scalarField;

  // int currentScalarFieldIndex;
  // QString currentScalarFieldName;

  ScalarField* addScalarField(QString, int);

  QHash<QString, QAction*> groupActionHash;
  QHash<QString, QAction*> scalarActionHash;

  QVTKWidget *qvtkWidget;
  vtkRenderer *renderer;

  vtkUnstructuredGrid *volumeGrid;
  vtkUnstructuredGrid *surfaceGrid;
  vtkUnstructuredGrid *lineGrid;

  vtkActor *isoContourActor;
  vtkActor *isoSurfaceActor;
  vtkActor *scalarFieldActor;
  vtkActor *wireframeActor;
  vtkScalarBarActor *colorBarActor;
  vtkTextActor *fieldNameActor;
  vtkActor *featureEdgeActor;

  IsoContour *isoContour; // ui
  IsoSurface *isoSurface; // ui
  ColorBar *colorBar;     // ui

  QString currentScalarFieldName;
  QString currentIsoContourName;
  QString currentIsoSurfaceName;
};

#endif // VTKPOST_H
