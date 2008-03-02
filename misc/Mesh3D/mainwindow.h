#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001

#include <QMainWindow>
#include "tetlib_api.h"
#include "nglib_api.h"
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
  void open();                // File -> Open...
  void save();                // File -> Save...
  void showsif();             // Edit -> Show Sif...
  void makeSteadyHeatSif();   // Edit -> Steady heat...
  void meshcontrol();         // Mesh -> Control...
  void boundarydivide();      // Mesh -> Divide boundary...
  void remesh();              // Mesh -> Remesh...
  void showabout();           // Help -> About...

  void meshOk();
  void boundarySelected(int);
  void slotDoDivision(double);
  
private:
  GLWidget *glWidget;
  SifWindow *sifWindow;
  MeshControl *meshControl;
  BoundaryDivide *boundaryDivide;

  void createActions();
  void createMenus();
  void createToolBars();
  void createStatusBar();
    
  QMenu *fileMenu;            // File menu
  QMenu *editMenu;            // Edit menu
  QMenu *meshMenu;            // Mesh menu
  QMenu *helpMenu;            // Help menu

  QToolBar *fileToolBar;      // File toolbar

  QAction *openAct;           // File -> Open...
  QAction *saveAct;           // File -> Save...
  QAction *exitAct;           // File -> Exit...
  QAction *showsifAct;        // Edit -> Show Sif...
  QAction *steadyHeatSifAct;  // Edit -> Steady heat...
  QAction *meshcontrolAct;    // Mesh -> Control...
  QAction *boundarydivideAct; // Mesh -> Divide boundary...
  QAction *remeshAct;         // Mesh -> Remesh...
  QAction *aboutAct;          // Help -> About...

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
  
  // meshing thread:
  MeshingThread *meshingThread;

  // mesh utilities:
  Meshutils *meshutils;
  
  // private functions:
  void readInputFile(QString fileName);
  void saveElmerMesh(QString dirName);
  void makeElmerMeshFromTetlib();
  void makeElmerMeshFromNglib();
  void logMessage(QString message);

};

#endif
