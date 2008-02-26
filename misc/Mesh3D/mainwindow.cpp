#include <QtGui>
#include <QFile>
#include <iostream>
#include <fstream>

#include "mainwindow.h"
#include "glwidget.h"
#include "meshingthread.h"
#include "sifwindow.h"
#include "meshcontrol.h"



// Construct main window...
//-----------------------------------------------------------------------------
MainWindow::MainWindow()
{
  loadPlugins();

  glWidget = new GLWidget;
  setCentralWidget(glWidget);

  sifWindow = new SifWindow(this);

  meshControl = new MeshControl(this);

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  
  setWindowTitle(tr("Elmer Mesh3D (experimental)"));

  // glWidget emits (int) when a boundary is selected by double clicking:
  connect(glWidget, SIGNAL(selectedBoundary(int)), this, SLOT(boundarySelected(int)));

  // meshing thread emits (void) when the mesh generation is completed:
  connect(&meshingThread, SIGNAL(generatorFinished()), this, SLOT(meshOk()));

  nglibInputOk = false;
  tetlibInputOk = false;
}


// dtor...
//-----------------------------------------------------------------------------
MainWindow::~MainWindow()
{
}


// Load plugins...
//-----------------------------------------------------------------------------
void MainWindow::loadPlugins()
{
  tetlibPresent = false;

  std::cout << "Load libtet...";

#ifdef WIN32
  hTetlib = LoadLibrary(TEXT("./libtet.dll"));
#else
  hTetlib = dlopen("./libtet.so", RTLD_LAZY);  
#endif
  
  if(!hTetlib) {
    std::cout << "failed\n";
    std::cout << "tetlib functionality disabled\n";
    std::cout.flush();
    return;
  }

  std::cout << "done\n";
  std::cout.flush();
  
#ifdef WIN32
  ptetgenio = (tetgenio_t) GetProcAddress(hTetlib, "CreateObjectOfTetgenio");
#else
  ptetgenio = (tetgenio_t) dlsym(hTetlib, "CreateObjectOfTetgenio");  
#endif
  
  if(!ptetgenio) {
    std::cout << "Unable to get proc address for 'tetgenio'\n";
    std::cout.flush();
#ifndef WIN32
    dlclose(hTetlib);
#endif
    return;
  }
  
  in = (ptetgenio)();
  out = (ptetgenio)();  

  tetlibPresent = true;
}



// Create status bar...
//-----------------------------------------------------------------------------
void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}


// Create menus...
//-----------------------------------------------------------------------------
void MainWindow::createMenus()
{
  // File menu
  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(openAct);
  fileMenu->addAction(saveAct);
  fileMenu->addSeparator();
  fileMenu->addAction(exitAct);

  // Edit menu
  editMenu = menuBar()->addMenu(tr("&Edit"));
  editMenu->addAction(showsifAct);
  editMenu->addSeparator();
  editMenu->addAction(steadyHeatSifAct);

  // Mesh menu
  meshMenu = menuBar()->addMenu(tr("&Mesh"));
  meshMenu->addAction(meshcontrolAct);
  meshMenu->addAction(remeshAct);

  // Help menu
  helpMenu = menuBar()->addMenu(tr("&Help"));
  helpMenu->addAction(aboutAct);
}



// Create tool bars...
//-----------------------------------------------------------------------------
void MainWindow::createToolBars()
{
  // File toolbar
  fileToolBar = addToolBar(tr("&File"));
  fileToolBar->addAction(openAct);
  fileToolBar->addAction(saveAct);
  fileToolBar->addAction(exitAct);
}


// Create actions...
//-----------------------------------------------------------------------------
void MainWindow::createActions()
{
  // File -> Open file
  openAct = new QAction(QIcon("./icons/book_open.png"), tr("&Open..."), this);
  openAct->setShortcut(tr("Ctrl+O"));
  openAct->setStatusTip(tr("Open model input file"));
  connect(openAct, SIGNAL(triggered()), this, SLOT(open()));
  
  // File -> Save file
  saveAct = new QAction(QIcon("./icons/disk.png"), tr("&Save..."), this);
  saveAct->setShortcut(tr("Ctrl+S"));
  saveAct->setStatusTip(tr("Save mesh"));
  connect(saveAct, SIGNAL(triggered()), this, SLOT(save()));

  // File -> Exit
  exitAct = new QAction(QIcon("./icons/cancel.png"), tr("E&xit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip(tr("Exit"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

  // Edit -> Sif
  showsifAct = new QAction(QIcon(), tr("&Sif..."), this);
  showsifAct->setShortcut(tr("Ctrl+I"));
  showsifAct->setStatusTip(tr("Edit solver input file"));
  connect(showsifAct, SIGNAL(triggered()), this, SLOT(showsif()));

  // Edit -> Steady heat sif...
  steadyHeatSifAct = new QAction(QIcon(), tr("&Steady heat..."), this);
  steadyHeatSifAct->setStatusTip(tr("Sif skeleton for steady heat conduction"));
  connect(steadyHeatSifAct, SIGNAL(triggered()), this, SLOT(makeSteadyHeatSif()));

  // Mesh -> Control
  meshcontrolAct = new QAction(QIcon(), tr("&Control..."), this);
  meshcontrolAct->setShortcut(tr("Ctrl+M"));
  meshcontrolAct->setStatusTip(tr("Mesh control"));
  connect(meshcontrolAct, SIGNAL(triggered()), this, SLOT(meshcontrol()));

  // Mesh -> Remesh
  remeshAct = new QAction(QIcon(), tr("&Remesh..."), this);
  remeshAct->setShortcut(tr("Ctrl+R"));
  remeshAct->setStatusTip(tr("Remesh"));
  connect(remeshAct, SIGNAL(triggered()), this, SLOT(remesh()));

  // Help -> About
  aboutAct = new QAction(QIcon(), tr("&About..."), this);
  aboutAct->setShortcut(tr("Ctrl+A"));
  aboutAct->setStatusTip(tr("About the program"));
  connect(aboutAct, SIGNAL(triggered()), this, SLOT(showabout()));
}



// About dialog...
//-----------------------------------------------------------------------------
void MainWindow::showabout()
{
  QMessageBox::about(this, tr("About Mesh3D"),
		     tr("Mesh3D is a preprocessor for three dimensional "
			"modeling with Elmer finite element software. "
			"The program uses tetlib and nglib as tetrahedral "
			"Delaunay mesh generators:\n\n"
			"http://tetgen.berlios.de/\n"
			"http://www.hpfem.jku.at/netgen/\n"
			"http://www.csc.fi/elmer/\n\n"
			"Written by Mikko Lyly, 2008"));
}



// Mesh -> Control...
//-----------------------------------------------------------------------------
void MainWindow::meshcontrol()
{
  meshControl->show();
}



// Edit -> Sif...
//-----------------------------------------------------------------------------
void MainWindow::showsif()
{
  sifWindow->show();
}



// Log message...
//-----------------------------------------------------------------------------
void MainWindow::logMessage(QString message)
{
  std::cout << std::string(message.toAscii()) << std::endl;
  std::cout.flush();
}




// Mesh -> Remesh...
//-----------------------------------------------------------------------------
void MainWindow::remesh()
{
  if(meshControl->generatorType == GEN_TETLIB) {

    if(!tetlibPresent) {
      logMessage("tetlib functionality disabled");
      return;
    }

    if(!tetlibInputOk) {
      logMessage("Remesh: error: no input data for tetlib");
      return;
    }

    // must have "J" in control string:
    tetlibControlString = meshControl->tetlibControlString;

  } else if(meshControl->generatorType == GEN_NGLIB) {

    if(!nglibInputOk) {
      logMessage("Remesh: error: no input data for nglib");
      return;
    }

    char backgroundmesh[1024];
    sprintf(backgroundmesh, "%s",
	    (const char*)(meshControl->nglibBackgroundmesh.toAscii()));
    
    ngmesh = nglib::Ng_NewMesh();
    
    mp.maxh = meshControl->nglibMaxH.toDouble();
    mp.fineness = meshControl->nglibFineness.toDouble();
    mp.secondorder = 0;
    mp.meshsize_filename = backgroundmesh;
    
  } else {

    logMessage("Remesh: uknown generator type");
    return;

  }

  // Start meshing thread:
  int gt = meshControl->generatorType;
  meshingThread.generate(gt, tetlibControlString, in, out, ngmesh, nggeom, mp, hTetlib);

  logMessage("Mesh generation initiated");
  statusBar()->showMessage(tr("Generating mesh..."));
}




// Mesh is ready (signaled by MeshingThread::run):
//-----------------------------------------------------------------------------
void MainWindow::meshOk()
{
  logMessage("Mesh generation completed");

  if(meshControl->generatorType == GEN_TETLIB) {

    makeElmerMeshFromTetlib();

  } else if(meshControl->generatorType == GEN_NGLIB) {
    
    makeElmerMeshFromNglib();

  } else {
    
    logMessage("MeshOk: error: unknown mesh generator");

  }

  statusBar()->showMessage(tr("Ready"));
}




// File -> Open...
//-----------------------------------------------------------------------------
void MainWindow::open()
{
  QString fileName = QFileDialog::getOpenFileName(this);

  if (!fileName.isEmpty()) {
    
    QFileInfo fi(fileName);
    QString absolutePath = fi.absolutePath();
    QDir::setCurrent(absolutePath);
    
  } else {
    
    logMessage("Unable to open file: file name is empty");
    return;

  }
  
  readInputFile(fileName);
  remesh();
}



// File -> Save...
//-----------------------------------------------------------------------------
void MainWindow::save()
{
  if(glWidget->mesh==NULL) {
    logMessage("Unable to save mesh: no data");
    return;
  }

  QString dirName = QFileDialog::getExistingDirectory(this);

  if (!dirName.isEmpty()) {

    logMessage("Output directory " + dirName);

  } else {

    logMessage("Unable to save: directory undefined");
    return;

  }
  
  saveElmerMesh(dirName);
}




// Write out mesh files in elmer-format:
//-----------------------------------------------------------------------------
void MainWindow::saveElmerMesh(QString dirName)
{
  logMessage("Saving elmer mesh files");

  statusBar()->showMessage(tr("Saving..."));

  QFile file;
  QDir::setCurrent(dirName);

  mesh_t *mesh = glWidget->mesh;

  // Header:
  file.setFileName("mesh.header");
  file.open(QIODevice::WriteOnly);
  QTextStream header(&file);

  std::cout << "Saving " << mesh->nodes << " nodes\n";
  std::cout << "Saving " << mesh->elements << " elements\n";
  std::cout << "Saving " << mesh->boundaryelements << " boundary elements\n";
  std::cout.flush();

  header << mesh->nodes << " ";
  header << mesh->elements << " ";
  header << mesh->boundaryelements << "\n";

  header << "2\n";
  header << "303 " << mesh->boundaryelements << "\n";
  header << "504 " << mesh->elements << "\n";

  file.close();

  // Nodes:
  file.setFileName("mesh.nodes");
  file.open(QIODevice::WriteOnly);
  QTextStream nodes(&file);
  
  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    int index = node->index;

    nodes << i+1 << " " << index << " ";
    nodes << node->x[0] << " ";
    nodes << node->x[1] << " ";
    nodes << node->x[2] << "\n";
  }

  file.close();

  // Elements:
  file.setFileName("mesh.elements");
  file.open(QIODevice::WriteOnly);
  QTextStream elements(&file);
  
  for(int i=0; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    int index = element->index;

    if(index < 1)
      index = 1;

    elements << i+1 << " " << index << " 504 ";
    elements << element->vertex[0]+1 << " ";
    elements << element->vertex[1]+1 << " ";
    elements << element->vertex[2]+1 << " ";
    elements << element->vertex[3]+1 << "\n";
  }

  file.close();
  
  // Boundary elements:
  file.setFileName("mesh.boundary");
  file.open(QIODevice::WriteOnly);
  QTextStream boundary(&file);
  
  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    int parent1 = boundaryelement->parent[0];
    int parent2 = boundaryelement->parent[1];

    if(parent1 < 0)
      parent1 = 0;

    if(parent2 < 0)
      parent2 = 0;

    boundary << i+1 << " ";
    boundary << boundaryelement->index << " ";
    boundary << parent1 << " ";
    boundary << parent2 << " ";
    boundary << "303 ";
    boundary << boundaryelement->vertex[0]+1 << " ";
    boundary << boundaryelement->vertex[1]+1 << " ";
    boundary << boundaryelement->vertex[2]+1 << "\n";
  }

  file.close();

  // Sif:
  file.setFileName("skeleton.sif");
  file.open(QIODevice::WriteOnly);
  QTextStream sif(&file);

  QApplication::setOverrideCursor(Qt::WaitCursor);
  sif << sifWindow->textEdit->toPlainText();
  QApplication::restoreOverrideCursor();

  file.close();

  // ELMERSOLVER_STARTINFO:
  file.setFileName("ELMERSOLVER_STARTINFO");
  file.open(QIODevice::WriteOnly);
  QTextStream startinfo(&file);

  startinfo << "skeleton.sif\n1\n";

  file.close();

  statusBar()->showMessage(tr("Ready"));
}




// Boundady selected by double clicking (signaled by glWidget::select):
//-----------------------------------------------------------------------------
void MainWindow::boundarySelected(int boundary)
{
  if( boundary > -1 ) {

    QString qs = "Selected boundary " + QString::number(boundary);
    statusBar()->showMessage(qs);

  } else {

    QString qs = "Ready";;
    statusBar()->showMessage(qs);    

  }

  // Find the boundary condition block in sif:
  QTextEdit *textEdit = sifWindow->textEdit;
  QTextCursor cursor = textEdit->textCursor();

  textEdit->moveCursor(QTextCursor::Start);
  QString qs = "Target boundaries(1) = " + QString::number(boundary);
  bool found = textEdit->find(qs);

  // Select and highlight bc block:
  if(found) {
    textEdit->moveCursor(QTextCursor::Up);
    textEdit->moveCursor(QTextCursor::Up);
    textEdit->find("Boundary");
    
    cursor.movePosition(QTextCursor::StartOfWord, QTextCursor::KeepAnchor);
    textEdit->moveCursor(QTextCursor::Down, QTextCursor::KeepAnchor);
    textEdit->moveCursor(QTextCursor::Down, QTextCursor::KeepAnchor);
    textEdit->moveCursor(QTextCursor::Down, QTextCursor::KeepAnchor);
    textEdit->moveCursor(QTextCursor::Down, QTextCursor::KeepAnchor);
    cursor.select(QTextCursor::BlockUnderCursor);
  }
}




// Read input file and populate mesh generator's input structures:
//-----------------------------------------------------------------------------
void MainWindow::readInputFile(QString fileName)
{
  char cs[1024];

  QFileInfo fi(fileName);
  QString absolutePath = fi.absolutePath();
  QString baseName = fi.baseName();
  QString fileSuffix = fi.suffix();
  QString baseFileName = absolutePath + "/" + baseName;
  sprintf(cs, "%s", (const char*)(baseFileName.toAscii()));

  if(meshControl->generatorType==GEN_TETLIB) {

    if(!tetlibPresent) {
      logMessage("tetlib functionality disabled");
      return;
    }

    in->deinitialize();
    in->initialize();

    if(fileSuffix == "smesh" || fileSuffix=="poly") {
      in->load_poly(cs);
    } else if (fileSuffix == "stl") {
      in->load_stl(cs); 
    } else if (fileSuffix == "off") {
      in->load_off(cs); 
    } else if (fileSuffix == "ply") {
      in->load_ply(cs); 
    } else if (fileSuffix == "mesh") {
      in->load_medit(cs);
    } else {
      logMessage("Read input file: error: illegal file type for tetlib");
      tetlibInputOk = false;      
      return;
    }
        
    tetlibInputOk = true;

  } else if (meshControl->generatorType==GEN_NGLIB) {    

    if(fileSuffix != "stl") {
      logMessage("Read input file: error: illegan file type for nglib");
      nglibInputOk = false;      
      return;
    }

    nglib::Ng_Init();
    
    nggeom = nglib::Ng_STL_LoadGeometry((const char*)(fileName.toAscii()));
    
    if (!nggeom) {
      logMessage("Ng_STL_LoadGeometry failed");
      return;
    }
    
    int rv = nglib::Ng_STL_InitSTLGeometry(nggeom);
    std::cout << "InitSTLGeometry: NG_result=" << rv << std::endl;
    std::cout.flush();
    
    nglibInputOk = true;

  } else {

    logMessage("ReadInputFile: error: unknown mesh generator");
    return;

  }
}
  


// Populate elmer's mesh structure and make GL-objects (tetlib):
//-----------------------------------------------------------------------------
void MainWindow::makeElmerMeshFromTetlib()
{
  Helpers helpers;

  glWidget->clearMesh();
  glWidget->mesh = new mesh_t;
  mesh_t *mesh = glWidget->mesh;
  
  // Nodes:
  mesh->nodes = out->numberofpoints;
  mesh->node = new node_t[mesh->nodes];

  double xmin = +9e9;
  double xmax = -9e9;

  double ymin = +9e9;
  double ymax = -9e9;

  double zmin = +9e9;
  double zmax = -9e9;

  REAL *pointlist = out->pointlist;

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];
    
    node->x[0] = *pointlist++;
    node->x[1] = *pointlist++;
    node->x[2] = *pointlist++;

    node->index = -1; // default
    
    if(node->x[0] > xmax) 
      xmax = node->x[0];
    
    if(node->x[0] < xmin) 
      xmin = node->x[0];
    
    if(node->x[1] > ymax) 
      ymax = node->x[1];

    if(node->x[1] < ymin) 
      ymin = node->x[1];

    if(node->x[2] > zmax) 
      zmax = node->x[2];

    if(node->x[2] < zmin) 
      zmin = node->x[2];
  }

  double xmid = (xmax+xmin)/2.0;
  double ymid = (ymax+ymin)/2.0;
  double zmid = (zmax+zmin)/2.0;

  double xlen = (xmax-xmin)/2.0;
  double ylen = (ymax-ymin)/2.0;
  double zlen = (zmax-zmin)/2.0;

  double s = xlen;

  if(ylen > s)
    s = ylen;

  if(zlen > s)
    s = zlen;

  s *= 2.0;
  
  // Scale to fit unit cube:
  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    node->x[0] -= xmid;
    node->x[1] -= ymid;
    node->x[2] -= zmid;

    node->x[0] /= s;
    node->x[1] /= s;
    node->x[2] /= s;
  }

  // Edges:
  mesh->edges = 0;
  mesh->edge = new edge_t[0];

  // Boundary elements:
  mesh->boundaryelements = out->numberoftrifaces;
  mesh->boundaryelement = new boundaryelement_t[mesh->boundaryelements];

  int *trifacelist = out->trifacelist;
  int *adjtetlist = out->adjtetlist;

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    boundaryelement->index = 1; // default
    if(out->trifacemarkerlist != (int*)NULL)
      boundaryelement->index = out->trifacemarkerlist[i];

    boundaryelement->parent[0] = -1; // default
    boundaryelement->parent[1] = -1;
    // must have "nn" in control string:
    if(out->adjtetlist != (int*)NULL) {
      boundaryelement->parent[0] = *adjtetlist++;
      boundaryelement->parent[1] = *adjtetlist++;
    }

    int u = (*trifacelist++) - out->firstnumber;
    int v = (*trifacelist++) - out->firstnumber;
    int w = (*trifacelist++) - out->firstnumber;

    boundaryelement->vertex[0] = u;
    boundaryelement->vertex[1] = v;
    boundaryelement->vertex[2] = w;

    // Normal:
    static double a[3], b[3], c[3];

    a[0] = mesh->node[v].x[0] - mesh->node[u].x[0];
    a[1] = mesh->node[v].x[1] - mesh->node[u].x[1];
    a[2] = mesh->node[v].x[2] - mesh->node[u].x[2];

    b[0] = mesh->node[w].x[0] - mesh->node[u].x[0];
    b[1] = mesh->node[w].x[1] - mesh->node[u].x[1];
    b[2] = mesh->node[w].x[2] - mesh->node[u].x[2];

    helpers.crossProduct(a,b,c);
    helpers.normalize(c);

    boundaryelement->normal[0] = c[0];
    boundaryelement->normal[1] = c[1];
    boundaryelement->normal[2] = c[2];
  }

  // Elements:
  mesh->elements = out->numberoftetrahedra;
  mesh->element = new element_t[mesh->elements];

  int *tetrahedronlist = out->tetrahedronlist;
  REAL *attribute = out->tetrahedronattributelist;
  int na =  out->numberoftetrahedronattributes;

  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    element->vertex[0] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[1] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[2] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[3] = (*tetrahedronlist++) - out->firstnumber;

    element->index = 1; // default
    // must have "A" in control string:
    if(out->tetrahedronattributelist != (REAL*)NULL) 
      element->index = (int)attribute[na*(i+1)-1];
  }

  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(1, glWidget->objects);
    glWidget->objects = 0;
  }

  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();

  logMessage("Input file processed");
}


// Populate elmer's mesh structure and make GL-objects (nglib):
//-----------------------------------------------------------------------------
void MainWindow::makeElmerMeshFromNglib()
{
  Helpers helpers;
  
  glWidget->clearMesh();
  glWidget->mesh = new mesh_t;
  mesh_t *mesh = glWidget->mesh;
  
  // Nodes:
  mesh->nodes = nglib::Ng_GetNP(ngmesh);
  mesh->node = new node_t[mesh->nodes];
  
  double xmin = +9e9;
  double xmax = -9e9;

  double ymin = +9e9;
  double ymax = -9e9;

  double zmin = +9e9;
  double zmax = -9e9;

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    nglib::Ng_GetPoint(ngmesh, i+1, node->x);

    node->index = -1; // default

    if(node->x[0] > xmax) 
      xmax = node->x[0];
    
    if(node->x[0] < xmin) 
      xmin = node->x[0];
    
    if(node->x[1] > ymax) 
      ymax = node->x[1];

    if(node->x[1] < ymin) 
      ymin = node->x[1];

    if(node->x[2] > zmax) 
      zmax = node->x[2];

    if(node->x[2] < zmin) 
      zmin = node->x[2];
  }

  double xmid = (xmax+xmin)/2.0;
  double ymid = (ymax+ymin)/2.0;
  double zmid = (zmax+zmin)/2.0;

  double xlen = (xmax-xmin)/2.0;
  double ylen = (ymax-ymin)/2.0;
  double zlen = (zmax-zmin)/2.0;

  double s = xlen;

  if(ylen > s)
    s = ylen;

  if(zlen > s)
    s = zlen;

  s *= 2.0;
  
  // Scale to fit unit cube:
  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    node->x[0] -= xmid;
    node->x[1] -= ymid;
    node->x[2] -= zmid;

    node->x[0] /= s;
    node->x[1] /= s;
    node->x[2] /= s;
  }

  // Edges:
  mesh->edges = 0;
  mesh->edge = new edge_t[0];

  // Boundary elements:				       
  mesh->boundaryelements = nglib::Ng_GetNSE(ngmesh);
  mesh->boundaryelement = new boundaryelement_t[mesh->boundaryelements];

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    boundaryelement->index = 1; // default

    boundaryelement->parent[0] = -1; 
    boundaryelement->parent[1] = -1;

    nglib::Ng_GetSurfaceElement(ngmesh, i+1, boundaryelement->vertex);
    
    int u = --boundaryelement->vertex[0];
    int v = --boundaryelement->vertex[1];
    int w = --boundaryelement->vertex[2];

    // Normal:
    static double a[3], b[3], c[3];

    a[0] = mesh->node[v].x[0] - mesh->node[u].x[0];
    a[1] = mesh->node[v].x[1] - mesh->node[u].x[1];
    a[2] = mesh->node[v].x[2] - mesh->node[u].x[2];

    b[0] = mesh->node[w].x[0] - mesh->node[u].x[0];
    b[1] = mesh->node[w].x[1] - mesh->node[u].x[1];
    b[2] = mesh->node[w].x[2] - mesh->node[u].x[2];

    helpers.crossProduct(a,b,c);
    helpers.normalize(c);

    boundaryelement->normal[0] = c[0];
    boundaryelement->normal[1] = c[1];
    boundaryelement->normal[2] = c[2];
  }

  // Elements:
  mesh->elements = nglib:: Ng_GetNE(ngmesh);
  mesh->element = new element_t[mesh->elements];

  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    nglib::Ng_GetVolumeElement(ngmesh, i+1, element->vertex);
    
    element->vertex[0]--;
    element->vertex[1]--;
    element->vertex[2]--;
    element->vertex[3]--;

    element->index = 1; // default
  }

  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(1, glWidget->objects);
    glWidget->objects = 0;
  }

  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();

  logMessage("Input file processed");
}



// Make solver input file for steady heat conduction...
//-----------------------------------------------------------------------------
void MainWindow::makeSteadyHeatSif()
{
  if(glWidget->mesh==NULL) {
    logMessage("Unable to create sif: no mesh");
    return;
  }

  QTextEdit *textEdit = sifWindow->textEdit;

  textEdit->append("! Sif skeleton for steady heat conduction\n");

  textEdit->append("Header");
  textEdit->append("  CHECK KEYWORDS Warn");
  textEdit->append("  Mesh DB \".\" \".\"");
  textEdit->append("  Include Path \"\"");
  textEdit->append("  Results Directory \"\"");
  textEdit->append("End\n");
  
  textEdit->append("Simulation");
  textEdit->append("  Max Output Level = 4");
  textEdit->append("  Coordinate System = \"Cartesian 3D\"");
  textEdit->append("  Coordinate Mapping(3) = 1 2 3");
  textEdit->append("  Simulation Type = \"Steady State\"");
  textEdit->append("  Steady State Max Iterations = 1");
  textEdit->append("  Output Intervals = 1");
  textEdit->append("  Solver Input File = \"skeleton.sif\"");
  textEdit->append("  Post File = \"skeleton.ep\"");
  textEdit->append("End\n");

  textEdit->append("Constants");
  textEdit->append("  Gravity(4) = 0 -1 0 9.82");
  textEdit->append("  Stefan Boltzmann = 5.67e-08");
  textEdit->append("End\n");

  textEdit->append("Body 1");
  textEdit->append("  Name = \"Body1\"");
  textEdit->append("  Body Force = 1");
  textEdit->append("  Equation = 1");
  textEdit->append("  Material = 1");
  textEdit->append("End\n");

  textEdit->append("Equation 1");
  textEdit->append("  Name = \"Heat equation\"");
  textEdit->append("  Active Solvers(1) = 1");
  textEdit->append("End\n");

  textEdit->append("Solver 1");
  textEdit->append("  Exec Solver = \"Always\"");
  textEdit->append("  Equation = \"Heat Equation\"");
  textEdit->append("  Variable = \"Temperature\"");
  textEdit->append("  Variable Dofs = 1");
  textEdit->append("  Linear System Solver = \"Iterative\"");
  textEdit->append("  Linear System Iterative Method = \"BiCGStab\"");
  textEdit->append("  Linear System Max Iterations = 350");
  textEdit->append("  Linear System Convergence Tolerance = 1.0e-08");
  textEdit->append("  Linear System Abort Not Converged = True");
  textEdit->append("  Linear System Preconditioning = \"ILU0\"");
  textEdit->append("  Linear System Residual Output = 1");
  textEdit->append("  Nonlinear System Convergence Tolerance = 1.0e-08");
  textEdit->append("  Nonlinear System Max Iterations = 1");
  textEdit->append("  Steady State Convergence Tolerance = 1.0e-08");
  textEdit->append("End\n");

  textEdit->append("Material 1");
  textEdit->append("  Name = \"Material1\"");
  textEdit->append("  Density = 1");
  textEdit->append("  Heat Conductivity = 1");
  textEdit->append("End\n");

  textEdit->append("Body Force 1");
  textEdit->append("  Name = \"BodyForce1\"");
  textEdit->append("  Heat Source = 1");
  textEdit->append("End\n");


  // Boundary condition blocks:
  mesh_t *mesh = glWidget->mesh;

  int maxindex = 0;
  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];
    if(boundaryelement->index > maxindex)
      maxindex = boundaryelement->index;
  }
  maxindex++;

  bool *tmp = new bool[maxindex];

  for(int i=0; i<maxindex; i++) 
    tmp[i] = false;

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];
    tmp[boundaryelement->index] = true;
  }
  
  int j = 0;
  for(int i=1; i < maxindex; i++) {
    if(tmp[i]) {
      textEdit->append("Boundary condition " + QString::number(++j));
      textEdit->append("  Target boundaries(1) = " + QString::number(i));
      textEdit->append("!  Temperature = 0");
      textEdit->append("!  Heat flux = 0");
      textEdit->append("End\n");
    }
  }

  delete tmp;  
}
