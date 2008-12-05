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
#include <QTextStream>

#ifdef PYTHONQT
#include <PythonQt.h>
#include <gui/PythonQtScriptingConsole.h>
#endif

class EpMesh;
class ScalarField;
class QVTKWidget;
class vtkRenderer;
class vtkRenderWindow;
class vtkActor;
class vtkFollower;
class vtkScalarBarActor;
class vtkDataSetMapper;
class vtkTextActor;
class vtkUnstructuredGrid;
class vtkLookupTable;
class vtkPlane;
class vtkAxes;
class vtkImplicitPlaneWidget;
class vtkCamera;
class IsoSurface;
class IsoContour;
class ColorBar;
class Surface;
class Preferences;
class Vector;
class ReadEpFile;
class StreamLine;
class TimeStep;
class Axes;
class FeatureEdge;
class MeshPoint;
class MeshEdge;
class Matc;

class VtkPost : public QMainWindow
{
  Q_OBJECT

public:
  VtkPost(QWidget *parent = 0);
  ~VtkPost();

  QSize minimumSizeHint() const;
  QSize sizeHint() const;

  QVTKWidget* GetQVTKWidget();
  vtkRenderer* GetRenderer();
  vtkActor* GetSurfaceActor();
  vtkActor* GetVectorActor();
  vtkActor* GetIsoContourActor();
  vtkActor* GetIsoSurfaceActor();
  vtkActor* GetStreamLineActor();
  vtkActor* GetAxesActor();
  vtkFollower* GetAxesXTextActor();
  vtkFollower* GetAxesYTextActor();
  vtkFollower* GetAxesZTextActor();
  vtkScalarBarActor* GetColorBarActor();
  vtkActor* GetFeatureEdgeActor();
  vtkActor* GetPickedPointActor();
  vtkActor* GetMeshPointActor();
  vtkActor* GetMeshEdgeActor();
  vtkUnstructuredGrid* GetLineGrid();
  vtkUnstructuredGrid* GetSurfaceGrid();
  vtkUnstructuredGrid* GetVolumeGrid();
  vtkPlane* GetClipPlane();
  vtkImplicitPlaneWidget* GetPlaneWidget();
  vtkLookupTable* GetCurrentLut();
  ScalarField* GetScalarField();
  EpMesh* GetEpMesh();
  Preferences* GetPreferences();

  void minMax(ScalarField *);
  ScalarField* addScalarField(QString, int, double *);
  void SetClipPlaneOrigin(double*);
  void SetClipPlaneNormal(double*);
  void GetBounds(double*);
  double* GetCurrentPickPosition();
  void SetCurrentPickPosition(double*);
  int GetScalarFields();
  void SetScalarFields(int);
  QString GetCurrentSurfaceName();
  void SetCurrentSurfaceName(QString);
  QString GetCurrentVectorName();
  QString GetCurrentVectorColorName();
  void SetCurrentVectorName(QString);
  void SetCurrentVectorColorName(QString);
  QString GetCurrentIsoContourName();
  QString GetCurrentIsoContourColorName();
  void SetCurrentIsoContourName(QString);
  void SetCurrentIsoContourColorName(QString);
  QString GetCurrentIsoSurfaceName();
  QString GetCurrentIsoSurfaceColorName();
  void SetCurrentIsoSurfaceName(QString);
  void SetCurrentIsoSurfaceColorName(QString);
  QString GetCurrentStreamLineName();
  QString GetCurrentStreamLineColorName();
  void SetCurrentStreamLineName(QString);
  void SetCurrentStreamLineColorName(QString);

signals:
  void canProceedWithNextSignal(vtkRenderWindow*);

public slots:
  void redrawSlot();

#ifdef MATC
  bool MatcCmd(QString);
  void domatcSlot();
  void matcOpenSlot();
  void matcCutPasteSlot();
#endif

  // Python bindings:
  void SetPostFileStart(int);
  void SetPostFileEnd(int);
  bool ReadPostFile(QString);
  void Redraw();
  void Render();
  void SetSurfaces(bool);
  void SetVectors(bool);
  void SetIsoContours(bool);
  void SetIsoSurfaces(bool);
  void SetStreamLines(bool);
  void SetColorBar(bool);
  void SetMeshPoints(bool);
  void SetMeshEdges(bool);
  void SetFeatureEdges(bool);
  void SetAxes(bool);
  bool GetClipAll();
  void SetClipAll(bool);
  void SetClipPlaneOx(double);
  void SetClipPlaneOy(double);
  void SetClipPlaneOz(double);
  void SetClipPlaneNx(double);
  void SetClipPlaneNy(double);
  void SetClipPlaneNz(double);
  double GetCameraDistance();
  void SetCameraDistance(double);
  double GetCameraPositionX();
  double GetCameraPositionY();
  double GetCameraPositionZ();
  void SetCameraPositionX(double);
  void SetCameraPositionY(double);
  void SetCameraPositionZ(double);
  double GetCameraFocalPointX();
  double GetCameraFocalPointY();
  double GetCameraFocalPointZ();
  void SetCameraFocalPointX(double);
  void SetCameraFocalPointY(double);
  void SetCameraFocalPointZ(double);
  void SetCameraDolly(double);
  void SetCameraRoll(double);
  void SetCameraAzimuth(double);
  void SetCameraYaw(double);
  void SetCameraElevation(double);
  void SetCameraPitch(double);
  void SetCameraZoom(double);
  void SetInitialCameraPosition();
  double GetLength();
  int NofNodes();
  bool SavePngFile(QString);

private slots:
  void exitSlot();
  void readEpFileSlot();
  void showSurfaceDialogSlot();
  void showVectorDialogSlot();
  void showIsoContourDialogSlot();
  void showIsoSurfaceDialogSlot();
  void showColorBarDialogSlot();
  void showStreamLineDialogSlot();
  void showTimeStepDialogSlot();
  void showPreferencesDialogSlot();

  void drawMeshPointSlot();
  void drawMeshEdgeSlot();
  void drawFeatureEdgesSlot();
  void drawSurfaceSlot();
  void drawVectorSlot();
  void drawIsoContourSlot();
  void drawIsoSurfaceSlot();
  void drawColorBarSlot();
  void drawStreamLineSlot();
  void drawAxesSlot();

  void hideSurfaceSlot();
  void hideVectorSlot();
  void hideIsoContourSlot();
  void hideIsoSurfaceSlot();
  void hideColorBarSlot();
  void hideStreamLineSlot();

  void groupChangedSlot(QAction*);
  void regenerateGridsSlot();
  void maybeRedrawSlot(bool);
  void populateWidgetsSlot();
  void fitToWindowSlot();
  void resetModelViewSlot();
  void clipAllToggledSlot(bool);

  void savePictureSlot();
  void timeStepChangedSlot();
  void reloadPostSlot();

  void showHelpSlot();

#ifdef PYTHONQT
  void showPythonQtConsoleSlot();
#endif

private:
  QMenu *fileMenu;
  QMenu *editMenu;
  QMenu *editGroupsMenu;
  QMenu *viewMenu;
  QMenu *helpMenu;

  QToolBar *viewToolBar;

  QAction *regenerateGridsAct;
  QAction *matcAct;
  QAction *exitAct;
  QAction *redrawAct;
  QAction *savePictureAct;
  QAction *preferencesAct;
  QAction *drawMeshPointAct;
  QAction *drawMeshEdgeAct;
  QAction *drawFeatureEdgesAct;
  QAction *drawSurfaceAct;
  QAction *drawVectorAct;
  QAction *drawIsoContourAct;
  QAction *drawIsoSurfaceAct;
  QAction *drawColorBarAct;
  QAction *drawStreamLineAct;
  QAction *timeStepAct;
  QAction *fitToWindowAct;
  QAction *resetModelViewAct;
  QAction *drawAxesAct;
  QAction *reloadPostAct;
  QAction *readEpFileAct;
  QAction *clipAllAct;
  QAction *showHelpAct;

  void createActions();
  void createMenus();
  void createToolbars();
  void createStatusBar();

  EpMesh* epMesh;
  QString postFileName;
  bool postFileRead;
  int scalarFields;
  ScalarField* scalarField;

  void addVectorField(QString, int);
  void getPostLineStream(QTextStream*);

  QHash<QString, QAction*> groupActionHash;

  QVTKWidget* qvtkWidget;

  vtkRenderer* renderer;
  vtkUnstructuredGrid* volumeGrid;
  vtkUnstructuredGrid* surfaceGrid;
  vtkUnstructuredGrid* lineGrid;
  vtkLookupTable *currentLut;
  vtkPlane* clipPlane;
  vtkActor* meshPointActor;
  vtkActor* meshEdgeActor;
  vtkActor* featureEdgeActor;
  vtkActor* surfaceActor;
  vtkActor* vectorActor;
  vtkActor* isoContourActor;
  vtkActor* isoSurfaceActor;
  vtkActor* streamLineActor;
  vtkActor* axesActor;
  vtkFollower* axesXTextActor;
  vtkFollower* axesYTextActor;
  vtkFollower* axesZTextActor;
  vtkScalarBarActor* colorBarActor;
  vtkActor* pickedPointActor;
  vtkImplicitPlaneWidget* planeWidget;
  double initialCameraPosition[3];
  double initialCameraRoll;

  Surface* surface;         // ui
  Vector* vector;           // ui
  IsoContour* isoContour;   // ui
  IsoSurface* isoSurface;   // ui
  StreamLine* streamLine;   // ui
  ColorBar* colorBar;       // ui
  Preferences* preferences; // ui
  Matc* matc;               // ui
  TimeStep* timeStep;       // ui
  Axes* axes;               // ui
  FeatureEdge* featureEdge; // ui
  MeshPoint* meshPoint;     // ui
  MeshEdge* meshEdge;       // ui
  ReadEpFile* readEpFile;   // ui

  QString currentSurfaceName;
  QString currentVectorName;
  QString currentVectorColorName;
  QString currentIsoContourName;
  QString currentIsoContourColorName;
  QString currentIsoSurfaceName;
  QString currentIsoSurfaceColorName;
  QString currentStreamLineName;
  QString currentStreamLineColorName;
  double currentPickPosition[3];

  // Post file input:
  QString postLine;
  QTextStream postLineStream;

#ifdef PYTHONQT
  QAction *showPythonQtConsoleAct;
  PythonQtObjectPtr mainContext;
  PythonQtScriptingConsole *console;
#endif  
};

#endif // VTKPOST_H
