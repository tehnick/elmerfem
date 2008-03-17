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
  void hidesurfacemeshSlot();     // Mesh -> Hide/Show surface mesh...
  void hidesharpedgesSlot();      // Mesh -> Hide/Show sharp edges...
  void hideselectedSlot();        // Mesh -> Hide selected...
  void showallSlot();             // Mesh -> Show all...
  void resetSlot();               // Mesh -> Reset...
  void shadeSlot();               // Mesh -> Reset...
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
  QMenu *meshMenu;                // Mesh menu
  QMenu *helpMenu;                // Help menu

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
  QAction *hidesurfacemeshAct;    // Mesh -> Hide/Show surface mesh...
  QAction *hidesharpedgesAct;     // Mesh -> Hide/Show sharp edges...
  QAction *hideselectedAct;       // Mesh -> Hide selected...
  QAction *showallAct;            // Mesh -> Show all...
  QAction *resetAct;              // Mesh -> Reset...
  QAction *shadeAct;              // Mesh -> Reset...
  QAction *aboutAct;              // Help -> About...

  int activeGenerator;            // Currently active generator

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
};

#endif
