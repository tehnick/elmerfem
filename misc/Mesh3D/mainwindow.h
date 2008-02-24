#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001

#include <QMainWindow>
#include "glwidget.h"
#include "meshingthread.h"
#include "sifwindow.h"
#include "meshcontrol.h"

#include <tetgen.h>

namespace nglib {
#include <nglib.h>
}

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
  void remesh();              // Mesh -> Remesh...
  void meshcontrol();         // Mesh -> Control...
  void showabout();           // Help -> About...

  void meshOk();
  void boundarySelected(int boundary);
  
private:
  GLWidget *glWidget;
  SifWindow *sifWindow;
  MeshControl *meshControl;

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
  QAction *remeshAct;         // Mesh -> Remesh...
  QAction *meshcontrolAct;    // Mesh -> Control...
  QAction *aboutAct;          // Help -> About...

  QString tetlibControlString;
  tetgenio in;
  tetgenio out;
  bool tetlibInputOk;

  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters mp;
  bool nglibInputOk;

  MeshingThread meshingThread;
  
  void readInputFile(QString fileName);
  void saveElmerMesh(QString dirName);
  void makeElmerMeshFromTetlib();
  void makeElmerMeshFromNglib();
  void logMessage(QString message);

};

#endif
