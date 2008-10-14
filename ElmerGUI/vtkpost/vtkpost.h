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

#define MAX_SCALARS 100

#include <QMainWindow>
#include <QHash>

class EpMesh;
class ScalarField;
class QVTKWidget;
class vtkRenderer;
class vtkActor;
class vtkActor2D;
class vtkScalarBarActor;
class vtkDataSetMapper;
class vtkTextActor;
class vtkUnstructuredGrid;
class IsoSurface;
class IsoContour;
class ColorBar;
class Surface;
class Preferences;
class Vector;
class Matc;

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
#ifdef MATC
  void domatcSlot();
  void matcOpenSlot();
  void matcCutPasteSlot();
#endif

private slots:
  void exitSlot();
  void showSurfaceDialogSlot();
  void showVectorDialogSlot();
  void showIsoContourDialogSlot();
  void showIsoSurfaceDialogSlot();
  void showColorBarDialogSlot();
  void preferencesSlot();

  void drawMeshEdgeSlot();
  void drawFeatureEdgesSlot();
  void drawSurfaceSlot();
  void drawVectorSlot();
  void drawIsoContourSlot();
  void drawIsoSurfaceSlot();
  void drawColorBarSlot();
  void drawMeshPointSlot();

  void hideSurfaceSlot();
  void hideVectorSlot();
  void hideIsoContourSlot();
  void hideIsoSurfaceSlot();
  void hideColorBarSlot();

  void groupChangedSlot(QAction*);
  void maybeRedrawSlot(bool);
  void redrawSlot();

  void savePictureSlot();

private:
  QMenu *fileMenu;
  QMenu *editMenu;
  QMenu *editGroupsMenu;
  QMenu *viewMenu;

  QToolBar *viewToolBar;

  QAction *matcAct;
  QAction *exitAct;
  QAction *redrawAct;
  QAction *savePictureAct;
  QAction *preferencesAct;
  QAction *drawMeshEdgeAct;
  QAction *drawColorBarAct;
  QAction *drawIsoContourAct;
  QAction *drawSurfaceAct;
  QAction *drawVectorAct;
  QAction *drawIsoSurfaceAct;
  QAction *drawFeatureEdgesAct;
  QAction *drawMeshPointAct;

  void createActions();
  void createMenus();
  void createToolbars();
  void createStatusBar();

  EpMesh *epMesh;
  QString postFileName;
  bool postFileRead;
  int scalarFields;
  ScalarField *scalarField;

  ScalarField* addScalarField(QString, int);

  QHash<QString, QAction*> groupActionHash;

  QVTKWidget *qvtkWidget;
  vtkRenderer *renderer;

  vtkUnstructuredGrid *volumeGrid;
  vtkUnstructuredGrid *surfaceGrid;
  vtkUnstructuredGrid *lineGrid;
  
  vtkActor2D *meshPointActor;
  vtkActor *meshEdgeActor;
  vtkActor *featureEdgeActor;
  vtkActor *surfaceActor;
  vtkActor *vectorActor;
  vtkActor *isoContourActor;
  vtkActor *isoSurfaceActor;
  vtkScalarBarActor *colorBarActor;

  Surface *surface;         // ui
  Vector *vector;           // ui
  IsoContour *isoContour;   // ui
  IsoSurface *isoSurface;   // ui
  ColorBar *colorBar;       // ui
  Preferences *preferences; // ui
  Matc *matc;               // ui

  QString currentSurfaceName;
  QString currentVectorName;
  QString currentIsoContourName;
  QString currentIsoSurfaceName;

};

#endif // VTKPOST_H
