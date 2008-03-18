#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "plugins/tetlib_api.h"
#include "plugins/nglib_api.h"
#include "plugins/elmergrid_api.h"
#include "glwidget.h"
#include "meshingthread.h"
#include "sifwindow.h"
#include "meshcontrol.h"
#include "boundarydivision.h"
#include "meshutils.h"

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
  void showsifSlot();             // Edit -> Solver input file...
  void makeSteadyHeatSifSlot();   // Edit -> Steady heat conduction...
  void makeLinElastSifSlot();     // Edit -> Steady heat conduction...
  void meshcontrolSlot();         // Mesh -> Control...
  void remeshSlot();              // Mesh -> Remesh...
  void boundarydivideSlot();      // Mesh -> Divide boundary...
  void boundaryunifySlot();       // Mesh -> Unify boundary...
  void edgedivideSlot();          // Mesh -> Divide edges...
  void hidesurfacemeshSlot();     // View -> Surface mesh...
  void hidesharpedgesSlot();      // View -> Sharp edges...
  void hideselectedSlot();        // View -> Hide selected...
  void showallSlot();             // View -> Show all...
  void resetSlot();               // View -> Reset model view...
  void flatShadeSlot();           // View -> Shade model -> flat
  void smoothShadeSlot();         // View -> Shade model -> smooth
  void runsolverSlot();           // Solver -> run
  void showaboutSlot();           // Help -> About...


  void closeMainWindowSlot();     // Close MainWindow

  void meshOkSlot();              // signal emitted by meshingThread
  void boundarySelectedSlot(list_t*); // signal emitted by glWidget
  void doDivisionSlot(double);    // signal emitted by boundaryDivide
  
private:
  GLWidget *glWidget;             // central gl widget
  SifWindow *sifWindow;           // sif text editor
  MeshControl *meshControl;       // mesh generator control
  BoundaryDivide *boundaryDivide; // boundary division control
  MeshingThread *meshingThread;   // meshing thread
  Meshutils *meshutils;           // mesh manipulation utilities  

  void createActions();
  void createMenus();
  void createToolBars();
  void createStatusBar();
    
  QMenu *fileMenu;                // File menu
  QMenu *editMenu;                // Edit menu
  QMenu *viewMenu;                // View menu
  QMenu *meshMenu;                // Mesh menu
  QMenu *helpMenu;                // Help menu
  QMenu *shadeMenu;               // Mesh -> Shade model...
  QMenu *solverMenu;              // Solver menu

  QToolBar *fileToolBar;          // File toolbar
  QToolBar *editToolBar;          // Edit toolbar
  QToolBar *meshToolBar;          // Mesh toolbar

  QAction *openAct;               // File -> Open...
  QAction *loadAct;               // File -> Load...
  QAction *saveAct;               // File -> Save...
  QAction *exitAct;               // File -> Exit...
  QAction *showsifAct;            // Edit -> Solver input file...
  QAction *steadyHeatSifAct;      // Edit -> Steady heat conduction...
  QAction *linElastSifAct;        // Edit -> Linear elasticity...
  QAction *meshcontrolAct;        // Mesh -> Control...
  QAction *remeshAct;             // Mesh -> Remesh...
  QAction *boundarydivideAct;     // Mesh -> Divide boundary...
  QAction *boundaryunifyAct;      // Mesh -> Unify boundary...
  QAction *edgedivideAct;         // Mesh -> Divide edges...
  QAction *hidesurfacemeshAct;    // View -> Show surface mesh...
  QAction *hidesharpedgesAct;     // View -> Show sharp edges...
  QAction *hideselectedAct;       // View -> Show selected...
  QAction *flatShadeAct;          // View -> Shade model -> Flat
  QAction *smoothShadeAct;        // View -> Shade model -> Smooth
  QAction *showallAct;            // View -> Show all...
  QAction *resetAct;              // View -> Reset model view...
  QAction *runsolverAct;          // Solver -> Run
  QAction *aboutAct;              // Help -> About...

  int activeGenerator;            // Currently active generator

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

  // private functions:
  void readInputFile(QString);
  void loadElmerMesh(QString);
  void saveElmerMesh(QString);
  void makeElmerMeshFromTetlib();
  void makeElmerMeshFromNglib();
  void logMessage(QString);
  void makeSifBodyBlocks();
  void makeSifBoundaryBlocks(QString);
  void synchronizeMenuToState();
};

#endif
