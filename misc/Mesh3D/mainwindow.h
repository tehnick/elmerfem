#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "glwidget.h"
#include "meshingthread.h"
#include <tetgen.h>
#include "sifwindow.h"
#include "meshcontrol.h"

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

  QToolBar *fileToolBar;      // File toolbar

  QAction *openAct;           // File -> Open...
  QAction *saveAct;           // File -> Save...
  QAction *exitAct;           // File -> Exit...
  QAction *showsifAct;        // Edit -> Show Sif...
  QAction *steadyHeatSifAct;  // Edit -> Steady heat...
  QAction *remeshAct;         // Mesh -> Remesh...
  QAction *meshcontrolAct;    // Mesh -> Control...

  tetgenio in;
  tetgenio out;
  MeshingThread meshingThread;
  
  void readInputFile(QString fileName);
  void saveElmerMesh(QString dirName);
  void makeElmerMesh();
  void logMessage(QString message);

  QString tetgenControlString;
};

#endif
