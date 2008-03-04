#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001

#include <QMainWindow>
#include "plugins/tetlib_api.h"
#include "plugins/nglib_api.h"
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
  void showsifSlot();             // Edit -> Show Sif...
  void makeSteadyHeatSifSlot();   // Edit -> Steady heat...
  void meshcontrolSlot();         // Mesh -> Control...
  void boundarydivideSlot();      // Mesh -> Divide boundary...
  void remeshSlot();              // Mesh -> Remesh...
  void showaboutSlot();           // Help -> About...

  void meshOkSlot();              // signal emitted by meshingThread
  void boundarySelectedSlot(int); // signal emitted by glWidget
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

  QAction *openAct;               // File -> Open...
  QAction *loadAct;               // File -> Load...
  QAction *saveAct;               // File -> Save...
  QAction *exitAct;               // File -> Exit...
  QAction *showsifAct;            // Edit -> Show Sif...
  QAction *steadyHeatSifAct;      // Edit -> Steady heat...
  QAction *meshcontrolAct;        // Mesh -> Control...
  QAction *boundarydivideAct;     // Mesh -> Divide boundary...
  QAction *remeshAct;             // Mesh -> Remesh...
  QAction *aboutAct;              // Help -> About...

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
  
  // private functions:
  void readInputFile(QString);
  void loadElmerMesh(QString);
  void saveElmerMesh(QString);
  void makeElmerMeshFromTetlib();
  void makeElmerMeshFromNglib();
  void logMessage(QString);

};

#endif
