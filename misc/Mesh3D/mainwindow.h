#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#define MAX_EQUATIONS 10
#define MAX_MATERIALS 10
// MAX_BCS defined in "bcpropertyeditor.h"
// MAX_BODIES defined in "bodypropertyeditor.h"

#include <QMainWindow>
#include <QProcess>
#include <QAction>
#include <QDomDocument>
#include "plugins/tetlib_api.h"
#include "plugins/nglib_api.h"
#include "plugins/elmergrid_api.h"
#include "glwidget.h"
#include "meshingthread.h"
#include "sifwindow.h"
#include "meshcontrol.h"
#include "boundarydivision.h"
#include "meshutils.h"
#include "bcpropertyeditor.h"
#include "pdepropertyeditor.h"
#include "matpropertyeditor.h"
#include "bodypropertyeditor.h"
#include "summaryeditor.h"
#include "sifgenerator.h"
#include "generalsetup.h"
#include "edfeditor.h"
#include "dynamiceditor.h"

class QAction;
class QMenu;
class GLWidget;

class MainWindow : public QMainWindow
{
  Q_OBJECT
    
public:
  MainWindow();
  ~MainWindow();

private slots:
  void openSlot();                // File -> Open...
  void loadSlot();                // File -> Load...
  void saveSlot();                // File -> Save...
  void saveAsSlot();              // File -> Save As...
  void closeMainWindowSlot();     // File -> exit
  void modelSetupSlot();          // Model -> Setup...
  void addEquationSlot();         // Model -> Equation...
  void addMaterialSlot();         // Model -> Material...
  void bodyEditSlot();            // Model -> Set body properties
  void bcEditSlot();              // Model -> Set boundary conditions
  void modelSummarySlot();        // Model -> Summary...
  void dummyEditorSlot();         // Model -> Dummy editor...
  void generateSifSlot();         // Edit -> Generate sif
  void showsifSlot();             // Edit -> Solver input file...
  void editDefinitionsSlot();     // Edit -> Definitions...
  void meshcontrolSlot();         // Mesh -> Control...
  void remeshSlot();              // Mesh -> Remesh
  void stopMeshingSlot();         // Mesh -> Kill generator
  void surfaceDivideSlot();       // Mesh -> Divide surface...
  void surfaceUnifySlot();        // Mesh -> Unify surface
  void edgeUnifySlot();           // Mesh -> Unify edge
  void edgeDivideSlot();          // Mesh -> Divide edge...
  void hidesurfacemeshSlot();     // View -> Surface mesh
  void hidesharpedgesSlot();      // View -> Sharp edges
  void viewCoordinatesSlot();     // View -> Coordinates
  void selectAllSurfacesSlot();   // View -> Select all surfaces
  void selectAllEdgesSlot();      // View -> Select all edges
  void hideselectedSlot();        // View -> Hide/show selected
  void showallSlot();             // View -> Show all
  void resetSlot();               // View -> Reset model view
  void flatShadeSlot();           // View -> Shade model -> flat
  void smoothShadeSlot();         // View -> Shade model -> smooth
  void runsolverSlot();           // Solver -> Run solver
  void killsolverSlot();          // Solver -> Kill solver
  void resultsSlot();             // Solver -> Post process
  void killresultsSlot();         // Solver -> Kill post process
  void showaboutSlot();           // Help -> About...

  void meshOkSlot();                  // signal emitted by meshingThread
  void boundarySelectedSlot(list_t*); // signal emitted by glWidget
  void doDivideSurfaceSlot(double);   // signal emitted by boundaryDivide
  void doDivideEdgeSlot(double);      // signal emitted by boundaryDivide
  void postProcessFinishedSlot(int);  // signal emitted by postProcess
  void solverStdoutSlot();            // solver's stdout redirection
  void solverStderrSlot();            // solver's stderr redirection
  void solverFinishedSlot(int);       // signal emitted by solver process
  void pdeEditorFinishedSlot(int, int);  // signal emitted by pde editor
  void matEditorFinishedSlot(int, int);  // signal emitted by mat editor
  void equationSelectedSlot(QAction*);   // item selected from Equation menu
  void materialSelectedSlot(QAction*);   // item selected from Material menu

private:
  GLWidget *glWidget;             // central gl widget
  SifWindow *sifWindow;           // sif text editor
  MeshControl *meshControl;       // mesh generator control
  BoundaryDivide *boundaryDivide; // boundary division control
  Meshutils *meshutils;           // mesh manipulation utilities  
  MeshingThread *meshingThread;   // meshing thread
  SifWindow *solverLogWindow;     // Solver log
  SifGenerator *sifGenerator;     // SIF generator
  EdfEditor *edfEditor;           // Edf editor

  void createActions();
  void createMenus();
  void createToolBars();
  void createStatusBar();
  void applyOperations();
    
  QMenu *fileMenu;                // File menu
  QMenu *modelMenu;               // Model menu
  QMenu *equationMenu;            // Model -> Equation menu
  QMenu *materialMenu;            // Model -> Material menu
  QMenu *editMenu;                // Edit menu
  QMenu *viewMenu;                // View menu
  QMenu *shadeMenu;               // View -> Shade model menu
  QMenu *meshMenu;                // Mesh menu
  QMenu *solverMenu;              // Solver menu
  QMenu *helpMenu;                // Help menu

  QToolBar *fileToolBar;          // File toolbar
  QToolBar *editToolBar;          // Edit toolbar
  QToolBar *meshToolBar;          // Mesh toolbar
  QToolBar *solverToolBar;        // Solver toolbar

  QAction *openAct;               // File -> Open...
  QAction *loadAct;               // File -> Load...
  QAction *saveAct;               // File -> Save...
  QAction *saveAsAct;             // File -> Save As...
  QAction *exitAct;               // File -> Exit
  QAction *modelSetupAct;         // Model -> Setup...
  QAction *addEquationAct;        // Model -> Equation...
  QAction *addMaterialAct;        // Model -> Material...
  QAction *bodyEditAct;           // Model -> Set body properties
  QAction *bcEditAct;             // Model -> Set boundary conditions
  QAction *modelSummaryAct;       // Model -> Summary...
  QAction *dummyEditorAct;        // Model -> Dummy editor...
  QAction *generateSifAct;        // Edit -> Generate sif
  QAction *showsifAct;            // Edit -> Edit SIF...
  QAction *editDefinitionsAct;    // Edit -> Edit SIF...
  QAction *hidesurfacemeshAct;    // View -> Show surface mesh
  QAction *hidesharpedgesAct;     // View -> Show sharp edges
  QAction *viewCoordinatesAct;    // View -> Show sharp edges
  QAction *selectAllSurfacesAct;  // View -> Select all surfaces
  QAction *selectAllEdgesAct;     // View -> Select all edges
  QAction *hideselectedAct;       // View -> Show selected
  QAction *flatShadeAct;          // View -> Shade model -> Flat
  QAction *smoothShadeAct;        // View -> Shade model -> Smooth
  QAction *showallAct;            // View -> Show all
  QAction *resetAct;              // View -> Reset model view
  QAction *meshcontrolAct;        // Mesh -> Control...
  QAction *remeshAct;             // Mesh -> Remesh
  QAction *stopMeshingAct;        // Mesh -> Kill generator
  QAction *surfaceDivideAct;      // Mesh -> Divide surface...
  QAction *surfaceUnifyAct;       // Mesh -> Unify surface
  QAction *edgeDivideAct;         // Mesh -> Divide edges...
  QAction *edgeUnifyAct;          // Mesh -> Unify edge
  QAction *runsolverAct;          // Solver -> Run solver
  QAction *killsolverAct;         // Solver -> Kill solver
  QAction *resultsAct;            // Solver -> Post process
  QAction *killresultsAct;        // Solver -> Kill post process
  QAction *aboutAct;              // Help -> About...

  // property editors etc.:
  GeneralSetup *generalSetup;
  PDEPropertyEditor *pdePropertyEditor;
  DynamicEditor *matPropertyEditor;
  DynamicEditor *bcPropertyEditor;
  BodyPropertyEditor *bodyPropertyEditor;
  SummaryEditor *summaryEditor;

  DynamicEditor *dynamicEditorSimulation;
  DynamicEditor *dynamicEditorConstants;
  DynamicEditor *dynamicEditorMaterial;
  DynamicEditor *dynamicEditorSolver;
  DynamicEditor *dynamicEditorBC;
  DynamicEditor *dynamicEditorIC;
  DynamicEditor *dynamicEditorEquation;
  DynamicEditor *dynamicEditorBodyForce;

  // elmer definitions:
  QDomDocument *elmerDefs;

  // images:
  QIcon iconChecked;
  QIcon iconEmpty;

  // tetlib:
  bool tetlibPresent;
  TetlibAPI *tetlibAPI;
  tetgenio *in;
  tetgenio *out;
  QString tetlibControlString;
  bool tetlibInputOk;
  
  // nglib:
  bool nglibPresent;
  NglibAPI *nglibAPI;
  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters *mp;
  bool nglibInputOk;

  // elmergrid:
  ElmergridAPI *elmergridAPI;

  // solver and post processor:
  QProcess *solver;
  QProcess *post;
  
  // utility functions:
  void readInputFile(QString);
  void loadElmerMesh(QString);
  void saveElmerMesh(QString);
  void makeElmerMeshFromTetlib();
  void makeElmerMeshFromNglib();
  void logMessage(QString);
  void synchronizeMenuToState();
  void loadDefinitions();

  // state variables
  int activeGenerator;
  bool bcEditActive;
  bool bodyEditActive;
};

#endif
