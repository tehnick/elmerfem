#include <QtGui>
#include <QFile>
#include <iostream>
#include <fstream>
#include "mainwindow.h"

using namespace std;

// Construct main window...
//-----------------------------------------------------------------------------
MainWindow::MainWindow()
{
  // load tetlib
  tetlibAPI = new TetlibAPI;
  tetlibPresent = tetlibAPI->loadTetlib();
  this->in = tetlibAPI->in;
  this->out = tetlibAPI->out;
  
  // load nglib
  nglibAPI = new NglibAPI;
  nglibPresent = nglibAPI->loadNglib();
  this->mp = nglibAPI->mp;
  this->ngmesh = nglibAPI->ngmesh;
  this->nggeom = nglibAPI->nggeom;

  // widgets and helpers
  glWidget = new GLWidget;
  setCentralWidget(glWidget);
  sifWindow = new SifWindow(this);
  meshControl = new MeshControl(this);
  boundaryDivide = new BoundaryDivide(this);
  meshingThread = new MeshingThread;
  meshutils = new Meshutils;

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  
  // glWidget emits (int) when a boundary is selected by double clicking:
  connect(glWidget, SIGNAL(signalBoundarySelected(int)), this, SLOT(boundarySelectedSlot(int)));

  // meshingThread emits (void) when the mesh generation is completed:
  connect(meshingThread, SIGNAL(signalMeshOk()), this, SLOT(meshOkSlot()));

  // boundaryDivide emits (void) when "divide button" has been clicked:
  connect(boundaryDivide, SIGNAL(signalDoDivision(double)), this, SLOT(doDivisionSlot(double)));

  nglibInputOk = false;
  tetlibInputOk = false;

  setWindowTitle(tr("Elmer Mesh3D (experimental)"));
}


// dtor...
//-----------------------------------------------------------------------------
MainWindow::~MainWindow()
{
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
  fileMenu->addAction(loadAct);
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
  meshMenu->addAction(boundarydivideAct);
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
  fileToolBar->addAction(loadAct);
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
  connect(openAct, SIGNAL(triggered()), this, SLOT(openSlot()));
  
  // File -> Load mesh
  loadAct = new QAction(QIcon("./icons/cog.png"), tr("&Load..."), this);
  loadAct->setShortcut(tr("Ctrl+L"));
  loadAct->setStatusTip(tr("Load Elmer mesh files"));
  connect(loadAct, SIGNAL(triggered()), this, SLOT(loadSlot()));
  
  // File -> Save file
  saveAct = new QAction(QIcon("./icons/disk.png"), tr("&Save..."), this);
  saveAct->setShortcut(tr("Ctrl+S"));
  saveAct->setStatusTip(tr("Save mesh"));
  connect(saveAct, SIGNAL(triggered()), this, SLOT(saveSlot()));

  // File -> Exit
  exitAct = new QAction(QIcon("./icons/cancel.png"), tr("E&xit"), this);
  exitAct->setShortcut(tr("Ctrl+Q"));
  exitAct->setStatusTip(tr("Exit"));
  connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

  // Edit -> Sif
  showsifAct = new QAction(QIcon(), tr("&Sif..."), this);
  showsifAct->setShortcut(tr("Ctrl+I"));
  showsifAct->setStatusTip(tr("Edit solver input file"));
  connect(showsifAct, SIGNAL(triggered()), this, SLOT(showsifSlot()));

  // Edit -> Steady heat sif...
  steadyHeatSifAct = new QAction(QIcon(), tr("&Steady heat..."), this);
  steadyHeatSifAct->setStatusTip(tr("Sif skeleton for steady heat conduction"));
  connect(steadyHeatSifAct, SIGNAL(triggered()), this, SLOT(makeSteadyHeatSifSlot()));

  // Mesh -> Control
  meshcontrolAct = new QAction(QIcon(), tr("&Control..."), this);
  meshcontrolAct->setShortcut(tr("Ctrl+M"));
  meshcontrolAct->setStatusTip(tr("Mesh control"));
  connect(meshcontrolAct, SIGNAL(triggered()), this, SLOT(meshcontrolSlot()));

  // Mesh -> Divide boundary
  boundarydivideAct = new QAction(QIcon(), tr("&Divide boundary..."), this);
  boundarydivideAct->setShortcut(tr("Ctrl+D"));
  boundarydivideAct->setStatusTip(tr("Divide boundary by sharp edges"));
  connect(boundarydivideAct, SIGNAL(triggered()), this, SLOT(boundarydivideSlot()));

  // Mesh -> Remesh
  remeshAct = new QAction(QIcon(), tr("&Remesh..."), this);
  remeshAct->setShortcut(tr("Ctrl+R"));
  remeshAct->setStatusTip(tr("Remesh"));
  connect(remeshAct, SIGNAL(triggered()), this, SLOT(remeshSlot()));

  // Help -> About
  aboutAct = new QAction(QIcon(), tr("&About..."), this);
  aboutAct->setShortcut(tr("Ctrl+A"));
  aboutAct->setStatusTip(tr("About the program"));
  connect(aboutAct, SIGNAL(triggered()), this, SLOT(showaboutSlot()));
}



// About dialog...
//-----------------------------------------------------------------------------
void MainWindow::showaboutSlot()
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
void MainWindow::meshcontrolSlot()
{
  meshControl->tetlibPresent = this->tetlibPresent;
  meshControl->nglibPresent = this->nglibPresent;
  
  if(!tetlibPresent) {
    meshControl->tetlibPresent = false;
    meshControl->ui.nglibRadioButton->setChecked(true);
    meshControl->ui.tetlibRadioButton->setEnabled(false);
    meshControl->ui.tetlibStringEdit->setEnabled(false);
  }

  if(!nglibPresent) {
    meshControl->nglibPresent = false;
    meshControl->ui.tetlibRadioButton->setChecked(true);
    meshControl->ui.nglibRadioButton->setEnabled(false);
    meshControl->ui.nglibMaxHEdit->setEnabled(false);
    meshControl->ui.nglibFinenessEdit->setEnabled(false);
    meshControl->ui.nglibBgmeshEdit->setEnabled(false);
  }

  meshControl->show();
}



// Mesh -> Divide boundary...
//-----------------------------------------------------------------------------
void MainWindow::boundarydivideSlot()
{
  boundaryDivide->show();
}



// Make boundary division by sharp edges (signalled by boundaryDivide)...
//-----------------------------------------------------------------------------
void MainWindow::doDivisionSlot(double angle)
{
  if(glWidget->mesh == NULL) {
    logMessage("No mesh to divide");
    return;
  }
  
  meshutils->findSharpEdges(glWidget->mesh, angle);
  int parts = meshutils->divideBoundaryBySharpEdges(glWidget->mesh);

  QString qs = "Boundary divided into " + QString::number(parts) + " parts";
  statusBar()->showMessage(qs);
  
  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(glWidget->firstList, glWidget->objects);
    glWidget->objects = 0;
  }
  
  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();
}



// Edit -> Sif...
//-----------------------------------------------------------------------------
void MainWindow::showsifSlot()
{
  sifWindow->show();
}



// Log message...
//-----------------------------------------------------------------------------
void MainWindow::logMessage(QString message)
{
  cout << string(message.toAscii()) << endl;
  statusBar()->showMessage(message);
  cout.flush();
}




// Mesh -> Remesh...
//-----------------------------------------------------------------------------
void MainWindow::remeshSlot()
{
  if(meshControl->generatorType == GEN_TETLIB) {

    if(!tetlibPresent) {
      logMessage("tetlib functionality unavailable");
      return;
    }

    if(!tetlibInputOk) {
      logMessage("Remesh: error: no input data for tetlib");
      return;
    }

    // must have "J" in control string:
    tetlibControlString = meshControl->tetlibControlString;

  } else if(meshControl->generatorType == GEN_NGLIB) {

    if(!nglibPresent) {
      logMessage("nglib functionality unavailable");
      return;
    }

    if(!nglibInputOk) {
      logMessage("Remesh: error: no input data for nglib");
      return;
    }

    char backgroundmesh[1024];
    sprintf(backgroundmesh, "%s",
	    (const char*)(meshControl->nglibBackgroundmesh.toAscii()));
    
    ngmesh = nglibAPI->Ng_NewMesh();
    
    mp->maxh = meshControl->nglibMaxH.toDouble();
    mp->fineness = meshControl->nglibFineness.toDouble();
    mp->secondorder = 0;
    mp->meshsize_filename = backgroundmesh;
    
  } else {

    logMessage("Remesh: uknown generator type");
    return;

  }

  // Start meshing thread:
  int gt = meshControl->generatorType;
  meshingThread->generate(gt, tetlibControlString, tetlibAPI, ngmesh, nggeom, mp, nglibAPI);

  logMessage("Mesh generation initiated");
  statusBar()->showMessage(tr("Generating mesh..."));
}




// Mesh is ready (signaled by MeshingThread::run):
//-----------------------------------------------------------------------------
void MainWindow::meshOkSlot()
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
void MainWindow::openSlot()
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
  remeshSlot();
}


// File -> Load...
//-----------------------------------------------------------------------------
void MainWindow::loadSlot()
{
  QString dirName = QFileDialog::getExistingDirectory(this);

  if (!dirName.isEmpty()) {

    logMessage("Loading from directory " + dirName);

  } else {

    logMessage("Unable to load mesh: directory undefined");
    return;

  }
  
  loadElmerMesh(dirName);
}



// File -> Save...
//-----------------------------------------------------------------------------
void MainWindow::saveSlot()
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



// Load mesh files in elmer-format:
//-----------------------------------------------------------------------------
void MainWindow::loadElmerMesh(QString dirName)
{
  logMessage("Loading elmer mesh files");

  QFile file;
  QDir::setCurrent(dirName);

  // Header:
  file.setFileName("mesh.header");
  if(!file.exists()) {
    logMessage("mesh.header does not exist");
    return;
  }
  file.open(QIODevice::ReadOnly);
  QTextStream mesh_header(&file);

  int nodes, elements, boundaryelements, types, type, ntype;

  mesh_header >> nodes >> elements >> boundaryelements;
  mesh_header >> types;

  cout << "mesh.header:" << endl;
  cout << nodes << " " << elements << " " << boundaryelements << endl;
  cout << types << endl;
  
  // the the moment only triangles and tetrahedron are accepted:
  for(int i=0; i<types; i++) {
    mesh_header >> type >> ntype;
    cout << type << " " << ntype << endl;
    if( (type != 303) && (type != 504) ) {
      logMessage("Illegal element type (not implemented)");
      return;
    }
  }
  
  file.close();

  // Nodes:
  file.setFileName("mesh.nodes");
  if(!file.exists()) {
    logMessage("mesh.nodes does not exist");
    return;
  }
  file.open(QIODevice::ReadOnly);
  QTextStream mesh_node(&file);

  int number, index;
  double x, y, z;

  // now we need to allocate a new mesh:
  meshutils->clearMesh(glWidget->mesh);
  glWidget->mesh = new mesh_t;
  mesh_t *mesh = glWidget->mesh;
  
  mesh->nodes = nodes;
  mesh->node = new node_t[nodes];

  for(int i=0; i<nodes; i++) {
    node_t *node = &mesh->node[i];
    mesh_node >> number >> index >> x >> y >> z;
    node->x[0] = x;
    node->x[1] = y;
    node->x[2] = z;
    node->index = index;
  }

  file.close();  

  // Elements:
  file.setFileName("mesh.elements");
  if(!file.exists()) {
    logMessage("mesh.elements does not exist");
    meshutils->clearMesh(mesh);
    return;
  }
  file.open(QIODevice::ReadOnly);
  QTextStream mesh_elements(&file);

  mesh->elements = elements;
  mesh->element = new element_t[elements];

  int n0, n1, n2, n3;
  for(int i=0; i<elements; i++) {
    element_t *element = &mesh->element[i];
    mesh_elements >> number >> index >> type >> n0 >> n1 >> n2 >> n3;
    element->node[0] = n0-1;
    element->node[1] = n1-1;
    element->node[2] = n2-1;
    element->node[3] = n3-1;
    element->index = index;
  }

  file.close();

  // Boundary elements:
  file.setFileName("mesh.boundary");
  if(!file.exists()) {
    logMessage("mesh.boundary does not exist");
    meshutils->clearMesh(mesh);
    return;
  }
  file.open(QIODevice::ReadOnly);
  QTextStream mesh_boundary(&file);

  mesh->boundaryelements = boundaryelements;
  mesh->boundaryelement = new boundaryelement_t[boundaryelements];

  int parent0, parent1;
  for(int i=0; i<boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];
    mesh_boundary >> number >> index >> parent0 >> parent1 >> type >> n0 >> n1 >> n2;

    boundaryelement->node[0] = n0-1;
    boundaryelement->node[1] = n1-1;
    boundaryelement->node[2] = n2-1;
    boundaryelement->edge[0] = -1;
    boundaryelement->edge[1] = -1;
    boundaryelement->edge[2] = -1;
    boundaryelement->element[0] = parent0-1;
    boundaryelement->element[1] = parent1-1;
    boundaryelement->normal[0] = 0.0;
    boundaryelement->normal[1] = 0.0;
    boundaryelement->normal[2] = 0.0;
    boundaryelement->index = index;
  }

  file.close();

  // Finalize:
  logMessage("Ready");

  // Scaling factors for drawing:
  double *bb = meshutils->boundingBox(glWidget->mesh);

  glWidget->drawTranslate[0] = bb[6]; // x-center
  glWidget->drawTranslate[1] = bb[7]; // y-center
  glWidget->drawTranslate[2] = bb[8]; // z-center
  glWidget->drawScale = bb[9];         // scaling

  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(glWidget->firstList, glWidget->objects);
    glWidget->objects = 0;
  }
  
  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();
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

  cout << "Saving " << mesh->nodes << " nodes\n";
  cout << "Saving " << mesh->elements << " elements\n";
  cout << "Saving " << mesh->boundaryelements << " boundary elements\n";
  cout.flush();

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
    elements << element->node[0]+1 << " ";
    elements << element->node[1]+1 << " ";
    elements << element->node[2]+1 << " ";
    elements << element->node[3]+1 << "\n";
  }

  file.close();
  
  // Boundary elements:
  file.setFileName("mesh.boundary");
  file.open(QIODevice::WriteOnly);
  QTextStream boundary(&file);
  
  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    int e0 = boundaryelement->element[0] + 1;
    int e1 = boundaryelement->element[1] + 1;

    if(e0 < 1)
      e0 = 0;

    if(e1 < 1)
      e1 = 0;

    boundary << i+1 << " ";
    boundary << boundaryelement->index << " ";
    boundary << e0 << " ";
    boundary << e1 << " ";
    boundary << "303 ";
    boundary << boundaryelement->node[0]+1 << " ";
    boundary << boundaryelement->node[1]+1 << " ";
    boundary << boundaryelement->node[2]+1 << "\n";
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
void MainWindow::boundarySelectedSlot(int boundary)
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
      logMessage("tetlib functionality unavailable");
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

    if(!nglibPresent) {
      logMessage("nglib functionality unavailable");
      return;
    }

    if(fileSuffix != "stl") {
      logMessage("Read input file: error: illegal file type for nglib");
      nglibInputOk = false;      
      return;
    }

    nglibAPI->Ng_Init();
    
    nggeom = nglibAPI->Ng_STL_LoadGeometry((const char*)(fileName.toAscii()), 0);
    
    if (!nggeom) {
      logMessage("Ng_STL_LoadGeometry failed");
      return;
    }
    
    int rv = nglibAPI->Ng_STL_InitSTLGeometry(nggeom);
    cout << "InitSTLGeometry: NG_result=" << rv << endl;
    cout.flush();
    
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
  meshutils->clearMesh(glWidget->mesh);
  glWidget->mesh = tetlibAPI->createElmerMeshStructure();

  // Scaling factors for drawing:
  double *bb = meshutils->boundingBox(glWidget->mesh);

  glWidget->drawTranslate[0] = bb[6]; // x-center
  glWidget->drawTranslate[1] = bb[7]; // y-center
  glWidget->drawTranslate[2] = bb[8]; // z-center
  glWidget->drawScale = bb[9];         // scaling

  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(glWidget->firstList, glWidget->objects);
    glWidget->objects = 0;
  }
  
  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();

  delete [] bb;

  logMessage("Input file processed");
}


// Populate elmer's mesh structure and make GL-objects (nglib):
//-----------------------------------------------------------------------------
void MainWindow::makeElmerMeshFromNglib()
{
  meshutils->clearMesh(glWidget->mesh);
  nglibAPI->ngmesh = this->ngmesh;
  glWidget->mesh = nglibAPI->createElmerMeshStructure();

  // Scaling factors for drawing:
  double *bb = meshutils->boundingBox(glWidget->mesh);

  glWidget->drawTranslate[0] = bb[6]; // x-center
  glWidget->drawTranslate[1] = bb[7]; // y-center
  glWidget->drawTranslate[2] = bb[8]; // z-center
  glWidget->drawScale = bb[9];         // scaling

  // Delete old objects, if any:
  if(glWidget->objects) {
    glDeleteLists(glWidget->firstList, glWidget->objects);
    glWidget->objects = 0;
  }

  // Compose new GL-objects:
  glWidget->objects = glWidget->makeObjects();
  glWidget->updateGL();

  delete [] bb;

  logMessage("Input file processed");
}



// Make solver input file for steady heat conduction...
//-----------------------------------------------------------------------------
void MainWindow::makeSteadyHeatSifSlot()
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

  delete [] tmp;  
}
