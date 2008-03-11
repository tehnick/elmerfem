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

  // elmergrid
  elmergridAPI = new ElmergridAPI;

  // widgets and utilities
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
  meshMenu->addAction(remeshAct);
  meshMenu->addSeparator();
  meshMenu->addAction(boundarydivideAct);
  meshMenu->addAction(boundaryunifyAct);
  meshMenu->addSeparator();
  meshMenu->addAction(hideselectedAct);
  meshMenu->addAction(showallAct);

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

  // Mesh -> Remesh
  remeshAct = new QAction(QIcon(), tr("&Remesh..."), this);
  remeshAct->setShortcut(tr("Ctrl+R"));
  remeshAct->setStatusTip(tr("Remesh"));
  connect(remeshAct, SIGNAL(triggered()), this, SLOT(remeshSlot()));

  // Mesh -> Divide boundary
  boundarydivideAct = new QAction(QIcon(), tr("&Divide boundary..."), this);
  boundarydivideAct->setShortcut(tr("Ctrl+D"));
  boundarydivideAct->setStatusTip(tr("Divide boundary by sharp edges"));
  connect(boundarydivideAct, SIGNAL(triggered()), this, SLOT(boundarydivideSlot()));

  // Mesh -> Unify boundary
  boundaryunifyAct = new QAction(QIcon(), tr("&Unify boundary..."), this);
  boundaryunifyAct->setShortcut(tr("Ctrl+U"));
  boundaryunifyAct->setStatusTip(tr("Unify boundary (merge selected)"));
  connect(boundaryunifyAct, SIGNAL(triggered()), this, SLOT(boundaryunifySlot()));

  // Mesh -> Hide selected
  hideselectedAct = new QAction(QIcon(), tr("&Hide selected..."), this);
  //hideselectedAct->setShortcut(tr("Ctrl+H"));
  hideselectedAct->setStatusTip(tr("Hide selected boundaries"));
  connect(hideselectedAct, SIGNAL(triggered()), this, SLOT(hideselectedSlot()));

  // Mesh -> Show all
  showallAct = new QAction(QIcon(), tr("&Show all..."), this);
  //showallAct->setShortcut(tr("Ctrl+A"));
  showallAct->setStatusTip(tr("Show all boundaries"));
  connect(showallAct, SIGNAL(triggered()), this, SLOT(showallSlot()));

  // Help -> About
  aboutAct = new QAction(QIcon(), tr("&About..."), this);
  aboutAct->setShortcut(tr("Ctrl+A"));
  aboutAct->setStatusTip(tr("About the program"));
  connect(aboutAct, SIGNAL(triggered()), this, SLOT(showaboutSlot()));
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

  if(!tetlibPresent && !nglibPresent) 
    meshControl->ui.elmerGridRadioButton->setChecked(true);  

  meshControl->show();
}



// Mesh -> Divide boundary...
//-----------------------------------------------------------------------------
void MainWindow::boundarydivideSlot()
{
  boundaryDivide->show();
}



// Mesh -> unify boundary...
//-----------------------------------------------------------------------------
void MainWindow::boundaryunifySlot()
{
  mesh_t *mesh = glWidget->mesh;
  int lists = glWidget->lists;
  list_t *list = glWidget->list;

  if(mesh == NULL) {
    logMessage("No boundaries to unify");
    return;
  }
  
  int targetindex = -1;
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];
    if(l->selected) {
      if(targetindex < 0) {
	targetindex = l->index;
	break;
      }
    }
  }

  if(targetindex < 0) {
    logMessage("No boundaries selected");
    return;
  }
  
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];    
    if(l->selected) {
      for(int j=0; j < mesh->boundaryelements; j++) {
	boundaryelement_t *be = &mesh->boundaryelement[j];
	if(be->index == l->index) 
	  be->index = targetindex;
      }
    }
  }
  
  cout << "Selected boundary parts marked with index " << targetindex << endl;
  cout.flush();

  glWidget->rebuildLists();

  logMessage("Selected boundary parts unified");
}


// Mesh -> Hide selected...
//-----------------------------------------------------------------------------
void MainWindow::hideselectedSlot()
{
  mesh_t *mesh = glWidget->mesh;
  int lists = glWidget->lists;
  list_t *list = glWidget->list;

  if(mesh == NULL) {
    logMessage("There is nothing to hide");
    return;
  }
  
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];
    if(l->selected) 
      l->visible = false;
  }
  
  logMessage("Selected boundary parts hidden");
}


// Mesh -> Show all...
//-----------------------------------------------------------------------------
void MainWindow::showallSlot()
{
  int lists = glWidget->lists;
  list_t *list = glWidget->list;
  
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];
    l->visible = true;
  }

  logMessage("All boundary parts visible");
}



// Make boundary division by sharp edges (signalled by boundaryDivide)...
//-----------------------------------------------------------------------------
void MainWindow::doDivisionSlot(double angle)
{
  mesh_t *mesh = glWidget->mesh;

  if(mesh == NULL) {
    logMessage("No mesh to divide");
    return;
  }
  
  meshutils->findSharpEdges(mesh, angle);
  int parts = meshutils->divideBoundaryBySharpEdges(mesh);

  QString qs = "Boundary divided into " + QString::number(parts) + " parts";
  statusBar()->showMessage(qs);
  
  glWidget->rebuildLists();
}



// Edit -> Sif...
//-----------------------------------------------------------------------------
void MainWindow::showsifSlot()
{
  sifWindow->show();
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

  } else if(meshControl->generatorType == GEN_ELMERGRID) {

    // ***** ELMERGRID *****
    meshutils->clearMesh(glWidget->mesh);
    glWidget->mesh = new mesh_t;
    mesh_t *mesh = glWidget->mesh;
    

    logMessage("eg api start");
    elmergridAPI->createElmerMeshStructure(mesh);
    logMessage("eg api end");

    cout << "Nodes: " << mesh->nodes << endl;
    cout << "Elements: " << mesh->elements << endl;
    cout << "Boundaryelements: " << mesh->boundaryelements << endl;
    cout.flush();
    
    // this is just for the test case:
    cout << "find edges?" << endl;
    if(0) meshutils->findBoundaryElementEdges(mesh);
    cout << "find parents?" << endl;
    if(0) meshutils->findBoundaryElementParents(mesh);
    cout << "find normals?" << endl;
    if(0) meshutils->findBoundaryElementNormals(mesh);
    
    logMessage("Ready");
    glWidget->rebuildLists();
    
    cout << "back" << endl;

    return;
    
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

  // cout << "mesh.header:" << endl;
  // cout << nodes << " " << elements << " " << boundaryelements << endl;
  // cout << types << endl;

  int elements_zero_d = 0;
  int elements_one_d = 0;
  int elements_two_d = 0;
  int elements_three_d = 0;
  
  for(int i=0; i<types; i++) {
    mesh_header >> type >> ntype;
    //cout << type << " " << ntype << endl;
    
    switch(type/100) {
    case 1:
      elements_zero_d += ntype;
      break;
    case 2:
      elements_one_d += ntype;
      break;
    case 3:
    case 4:
      elements_two_d += ntype;
      break;
    case 5:
    case 8:
      elements_three_d += ntype;
      break;
    default:
      cout << "Unknown element family (possibly not implamented)" << endl;
      cout.flush();
      exit(0);
    }
  }
  
  file.close();

  cout << "Summary:" << endl;
  cout << "Nodes: " << nodes << endl;
  cout << "0d elements: " << elements_zero_d << endl;
  cout << "1d elements: " << elements_one_d << endl;
  cout << "2d elements: " << elements_two_d << endl;
  cout << "3d elements: " << elements_three_d << endl;
  cout.flush();




  // rework from here




  // allocate the new mesh:
  meshutils->clearMesh(glWidget->mesh);
  glWidget->mesh = new mesh_t;
  mesh_t *mesh = glWidget->mesh;
  
  mesh->nodes = nodes;
  mesh->node = new node_t[nodes];

  mesh->points = elements_zero_d;
  mesh->point = new point_t[mesh->points];

  mesh->edges = elements_one_d;
  mesh->edge = new edge_t[mesh->edges];

  mesh->boundaryelements = elements_two_d;
  mesh->boundaryelement = new boundaryelement_t[mesh->boundaryelements];

  mesh->elements = elements_three_d;
  mesh->element = new element_t[mesh->elements];

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

  int current_point = 0;
  int current_edge = 0;
  int current_boundaryelement = 0;
  int current_element = 0;

  for(int i=0; i<elements; i++) {
    mesh_elements >> number >> index >> type;

    switch(type/100) {
    case 1:
      break;

    case 2:
      break;

    case 3:
    case 4:
      boundaryelement_t *boundaryelement = &mesh->boundaryelement[current_boundaryelement++];
      boundaryelement->nature = BULK_ELEMENT;
      boundaryelement->index = index;
      boundaryelement->code = type;
      boundaryelement->nodes = boundaryelement->code % 100;
      boundaryelement->node = new int[boundaryelement->nodes];
      for(int j=0; j < boundaryelement->nodes; j++) {
	mesh_elements >> boundaryelement->node[j];
	boundaryelement->node[j] -= 1;
      }
      
      boundaryelement->elements = 0;
      boundaryelement->element = new int[2];
      boundaryelement->element[0] = -1;
      boundaryelement->element[1] = -1;

      break;

    case 5:
    case 8:
      element_t *element = &mesh->element[current_element++];
      element->nature = BULK_ELEMENT;
      element->index = index;
      element->code = type;
      element->nodes = element->code % 100;
      element->node = new int[element->nodes];
      for(int j=0; j < element->nodes; j++) {
	mesh_elements >> element->node[j];
	element->node[j] -= 1;
      }
      break;

    default:
      cout << "Unknown element type (possibly not implemented" << endl;
      cout.flush();
      exit(0);
      break;
    }

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

  int parent0, parent1;
  for(int i=0; i<boundaryelements; i++) {
    mesh_boundary >> number >> index >> parent0 >> parent1 >> type;

    switch(type/100) {
    case 1:
      break;

    case 2:
      edge_t *edge = &mesh->edge[current_edge++];
      edge->nature = BOUNDARY_ELEMENT;
      edge->index = index;
      edge->boundaryelements = 2;
      edge->boundaryelement = new int[edge->boundaryelements];
      edge->boundaryelement[0] = parent0-1;
      edge->boundaryelement[1] = parent1-1;
      edge->code = type;
      edge->nodes = edge->code % 100;
      edge->node = new int[edge->nodes];
      
      for(int j=0; j < edge->nodes; j++) {
	mesh_boundary >> edge->node[j];
	edge->node[j] -= 1;
      }

      break;

    case 3:
    case 4:
      boundaryelement_t *boundaryelement = &mesh->boundaryelement[current_boundaryelement++];
      boundaryelement->nature = BOUNDARY_ELEMENT;
      boundaryelement->index = index;
      boundaryelement->elements = 2;
      boundaryelement->element = new int[boundaryelement->elements];
      boundaryelement->element[0] = parent0-1;
      boundaryelement->element[1] = parent1-1;
      boundaryelement->code = type;
      boundaryelement->nodes = boundaryelement->code % 100;
      boundaryelement->node = new int[boundaryelement->nodes];
      
      for(int j=0; j < boundaryelement->nodes; j++) {
	mesh_boundary >> boundaryelement->node[j];
	boundaryelement->node[j] -= 1;
      }

      boundaryelement->edges = (int)(boundaryelement->code/100);      
      boundaryelement->edge = new int[boundaryelement->edges];

      for(int j=0; j<boundaryelement->edges; j++)
	boundaryelement->edge[j] = -1;
      
      break;

    case 5:
    case 8:
      // can't be boundary elements
      break;

    default:
      break;
    }
  }

  file.close();

  //?????
  // Edges:
  meshutils->findBoundaryElementEdges(mesh);
  meshutils->findBoundaryElementNormals(mesh);

  // Finalize:
  logMessage("Ready");

  glWidget->rebuildLists();
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
void MainWindow::boundarySelectedSlot(int index)
{
  QString qs;

  if(index < 0) {
    statusBar()->showMessage("Ready");    
    return;
  }

  list_t *l = &glWidget->list[index];
  
  if(l->type == BOUNDARYLIST) {
    qs = "Selected boundary " + QString::number(l->index);
  } else if(l->type == EDGELIST) {
    qs = "Selected edge " + QString::number(l->index);
  } else {
    qs = "Selected object " + QString::number(l->index) + " (type unknown)";
  }

  statusBar()->showMessage(qs);    
  
  // Find the boundary condition block in sif:
  QTextEdit *textEdit = sifWindow->textEdit;
  QTextCursor cursor = textEdit->textCursor();

  textEdit->moveCursor(QTextCursor::Start);
  qs = "Target boundaries(1) = " + QString::number(l->index);
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

  } else if (meshControl->generatorType==GEN_ELMERGRID) {    

    logMessage("nyt tarttis lukea inputfilee, kai" );
    
    int errstat = elmergridAPI->loadElmerMeshStructure((const char*)(fileName.toAscii()));
    
    if (errstat) {
      logMessage("loadElmerMeshStructure failed!");
      return;
    }
    return;

  } else {

    logMessage("ReadInputFile: error: unknown mesh generator");
    return;

  }
}
  


// Populate elmer's mesh structure and make GL-lists (tetlib):
//-----------------------------------------------------------------------------
void MainWindow::makeElmerMeshFromTetlib()
{
  meshutils->clearMesh(glWidget->mesh);
  glWidget->mesh = tetlibAPI->createElmerMeshStructure();

  glWidget->rebuildLists();

  logMessage("Input file processed");
}


// Populate elmer's mesh structure and make GL-lists (nglib):
//-----------------------------------------------------------------------------
void MainWindow::makeElmerMeshFromNglib()
{
  meshutils->clearMesh(glWidget->mesh);
  nglibAPI->ngmesh = this->ngmesh;
  glWidget->mesh = nglibAPI->createElmerMeshStructure();

  glWidget->rebuildLists();

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



// Log message...
//-----------------------------------------------------------------------------
void MainWindow::logMessage(QString message)
{
  cout << string(message.toAscii()) << endl;
  statusBar()->showMessage(message);
  cout.flush();
}
