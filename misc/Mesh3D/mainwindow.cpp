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
  meshControl->nglibPresent = nglibPresent;
  meshControl->tetlibPresent = tetlibPresent;
  meshControl->defaultControls();
  boundaryDivide = new BoundaryDivide(this);
  meshingThread = new MeshingThread;
  meshutils = new Meshutils;

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  
  // glWidget emits (list_t*) when a boundary is selected by double clicking:
  connect(glWidget, SIGNAL(signalBoundarySelected(list_t*)), this, SLOT(boundarySelectedSlot(list_t*)));

  // meshingThread emits (void) when the mesh generation is completed:
  connect(meshingThread, SIGNAL(signalMeshOk()), this, SLOT(meshOkSlot()));

  // boundaryDivide emits (double) when "divide button" has been clicked:
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
  meshMenu->addAction(hidesurfacemeshAct);
  meshMenu->addAction(hideselectedAct);
  meshMenu->addAction(showallAct);
  meshMenu->addAction(resetAct);

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

  // Mesh -> Hide/Show surface mesh
  hidesurfacemeshAct = new QAction(QIcon(), tr("&Hide/Show surface mesh..."), this);
  hidesurfacemeshAct->setStatusTip(tr("Hide/show surface mesh (do/do not outline surface elements)"));
  connect(hidesurfacemeshAct, SIGNAL(triggered()), this, SLOT(hidesurfacemeshSlot()));

  // Mesh -> Hide selected
  hideselectedAct = new QAction(QIcon(), tr("&Hide selected..."), this);
  hideselectedAct->setStatusTip(tr("Hide selected boundaries"));
  connect(hideselectedAct, SIGNAL(triggered()), this, SLOT(hideselectedSlot()));

  // Mesh -> Show all
  showallAct = new QAction(QIcon(), tr("&Show all..."), this);
  showallAct->setStatusTip(tr("Show all boundaries"));
  connect(showallAct, SIGNAL(triggered()), this, SLOT(showallSlot()));

  // Mesh -> Reset
  resetAct = new QAction(QIcon(), tr("&Reset model..."), this);
  resetAct->setStatusTip(tr("Reset model"));
  connect(resetAct, SIGNAL(triggered()), this, SLOT(resetSlot()));

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
      for(int j=0; j < mesh->surfaces; j++) {
	surface_t *s = &mesh->surface[j];
	if(s->index == l->index) 
	  s->index = targetindex;
      }
    }
  }
  
  cout << "Selected surfaces marked with index " << targetindex << endl;
  cout.flush();

  glWidget->rebuildLists();

  logMessage("Selected surfaces unified");
}


// Mesh -> Hide surface mesh...
//-----------------------------------------------------------------------------
void MainWindow::hidesurfacemeshSlot()
{
  mesh_t *mesh = glWidget->mesh;
  list_t *list = glWidget->list;
  int lists = glWidget->lists, vis;

  if(mesh == NULL) {
    logMessage("There is no surface mesh to hide/show");
    return;
  }
  
  vis = false;
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];
    if(l->type == SURFACEEDGELIST) 
    {
      l->visible = !l->visible;
      vis = l->visible;

      // do not set visible if the parent surface list is hidden
      int p = l->parent;
      if(p >= 0) {
	list_t *lp = &list[p];
	if(!lp->visible)
	  l->visible = false;
      }
    }
  }

 if ( !vis ) logMessage("Surface mesh hidden");
 else logMessage("Surface mesh shown");
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
    if(l->selected) {
      l->visible = false;

      // hide also all the child surfaceedgelist
      int c = l->child;
      if(c >= 0) {
	list_t *lc = &list[c];
	lc->visible = false;
      }
    }
  }
  
  logMessage("Selected objects hidden");
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

  logMessage("All objects visible");
}



// Mesh -> Reset...
//-----------------------------------------------------------------------------
void MainWindow::resetSlot()
{
  int lists = glWidget->lists;
  list_t *list = glWidget->list;
  
  for(int i=0; i<lists; i++) {
    list_t *l = &list[i];
    l->visible = true;
    l->selected = false;
  }

  glLoadIdentity();
  glWidget->rebuildLists();
  glWidget->updateGL();

  logMessage("Model reset");
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
    
    elmergridAPI->createElmerMeshStructure(mesh, meshControl->elmerGridControlString.toAscii());

    cout << "Nodes: " << mesh->nodes << endl;
    cout << "Elements: " << mesh->elements << endl;
    cout << "Surfaces: " << mesh->surfaces << endl;
    cout.flush();
    
    // this is just for the test case:
    cout << "find edges?" << endl;
    if(0) meshutils->findBoundaryElementEdges(mesh);
    cout << "find parents?" << endl;
    if(0) meshutils->findBoundaryElementParents(mesh);
 
    // hack for avoiding problems in 3d:
    cout << "***" << mesh->surfaces << endl;
    cout.flush();
    
    //meshutils->findBoundaryElementEdges(mesh);
    meshutils->findBoundaryElementNormals(mesh);
    glWidget->rebuildLists();

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

  int nodes, elements, surfaces, types, type, ntype;

  mesh_header >> nodes >> elements >> surfaces;
  mesh_header >> types;

  // cout << "mesh.header:" << endl;
  // cout << nodes << " " << elements << " " << surfaces << endl;
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

  mesh->surfaces = elements_two_d;
  mesh->surface = new surface_t[mesh->surfaces];

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
  int current_surface = 0;
  int current_element = 0;

  surface_t *surface = NULL;
  element_t *element = NULL;
  edge_t *edge = NULL;
  point_t *point = NULL;

  for(int i=0; i<elements; i++) {
    mesh_elements >> number >> index >> type;

    switch(type/100) {
    case 1:
      break;

    case 2:
      break;

    case 3:
    case 4:
      surface = &mesh->surface[current_surface++];
      surface->nature = PDE_BULK;
      surface->index = index;
      surface->code = type;
      surface->nodes = surface->code % 100;
      surface->node = new int[surface->nodes];
      for(int j=0; j < surface->nodes; j++) {
	mesh_elements >> surface->node[j];
	surface->node[j] -= 1;
      }
      
      surface->elements = 0;
      surface->element = new int[2];
      surface->element[0] = -1;
      surface->element[1] = -1;

      break;

    case 5:
    case 8:
      element = &mesh->element[current_element++];
      element->nature = PDE_BULK;
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
  for(int i=0; i<surfaces; i++) {
    mesh_boundary >> number >> index >> parent0 >> parent1 >> type;

    switch(type/100) {
    case 1:
      break;

    case 2:
      edge = &mesh->edge[current_edge++];
      edge->nature = PDE_BOUNDARY;
      edge->index = index;
      edge->surfaces = 2;
      edge->surface = new int[edge->surfaces];
      edge->surface[0] = parent0-1;
      edge->surface[1] = parent1-1;
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
      surface = &mesh->surface[current_surface++];
      surface->nature = PDE_BOUNDARY;
      surface->index = index;
      surface->elements = 2;
      surface->element = new int[surface->elements];
      surface->element[0] = parent0-1;
      surface->element[1] = parent1-1;
      surface->code = type;
      surface->nodes = surface->code % 100;
      surface->node = new int[surface->nodes];
      
      for(int j=0; j < surface->nodes; j++) {
	mesh_boundary >> surface->node[j];
	surface->node[j] -= 1;
      }

      surface->edges = (int)(surface->code/100);      
      surface->edge = new int[surface->edges];

      for(int j=0; j<surface->edges; j++)
	surface->edge[j] = -1;
      
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

  QDir dir(dirName);
  if ( !dir.exists() ) dir.mkdir(dirName);
  dir.setCurrent(dirName);

  QFile file;
  mesh_t *mesh = glWidget->mesh;
  
  // Elmer's elements codes are smaller than 1000
  int maxcode = 1000;
  int *bulk_by_type = new int[maxcode];
  int *boundary_by_type = new int[maxcode];

  for(int i=0; i<maxcode; i++) {
    bulk_by_type[i] = 0;
    boundary_by_type[i] = 0;
  }

  for(int i=0; i < mesh->elements; i++) {
    element_t *e = &mesh->element[i];

    if(e->nature == PDE_BULK) 
      bulk_by_type[e->code]++;

    if(e->nature == PDE_BOUNDARY)
      boundary_by_type[e->code]++;
  }

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];

    if(s->nature == PDE_BULK)
      bulk_by_type[s->code]++;

    if(s->nature == PDE_BOUNDARY)
      boundary_by_type[s->code]++;
  }

  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];

    if(e->nature == PDE_BULK)
      bulk_by_type[e->code]++;

    if(e->nature == PDE_BOUNDARY)
      boundary_by_type[e->code]++;
  }

  for(int i=0; i < mesh->points; i++) {
    point_t *p = &mesh->point[i];

    if(p->nature == PDE_BULK)
      bulk_by_type[p->code]++;

    if(p->nature == PDE_BOUNDARY)
      boundary_by_type[p->code]++;
  }

  int bulk_elements = 0;
  int boundary_elements = 0;
  int element_types = 0;

  for(int i=0; i<maxcode; i++) {
    bulk_elements += bulk_by_type[i];
    boundary_elements += boundary_by_type[i];

    if((bulk_by_type[i]>0) || (boundary_by_type[i]>0))
      element_types++;
  }

  // Header:
  file.setFileName("mesh.header");
  file.open(QIODevice::WriteOnly);
  QTextStream mesh_header(&file);

  cout << "Saving " << mesh->nodes << " nodes\n";
  cout << "Saving " << bulk_elements << " elements\n";
  cout << "Saving " << boundary_elements << " boundary elements\n";
  cout.flush();

  mesh_header << mesh->nodes << " ";
  mesh_header << bulk_elements << " ";
  mesh_header << boundary_elements << "\n";

  mesh_header << element_types << "\n";

  for(int i=0; i<maxcode; i++) {
    int j = bulk_by_type[i] + boundary_by_type[i];
    if(j > 0) 
      mesh_header << i << " " << j << "\n";
  }

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
  QTextStream mesh_element(&file);

  int current = 0;

  for(int i=0; i < mesh->elements; i++) {
    element_t *e = &mesh->element[i];
    int index = e->index;
    if(index < 1)
      index = 1;
    if(e->nature == PDE_BULK) {
      mesh_element << ++current << " ";
      mesh_element << index << " ";
      mesh_element << e->code << " ";
      for(int j=0; j < e->nodes; j++) 
	mesh_element << e->node[j]+1 << " ";
      mesh_element << "\n";
    }
  }

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    int index = s->index;
    if(index < 1)
      index = 1;
    if(s->nature == PDE_BULK) {
      mesh_element << ++current << " ";
      mesh_element << index << " ";
      mesh_element << s->code << " ";
      for(int j=0; j < s->nodes; j++) 
	mesh_element << s->node[j]+1 << " ";
      mesh_element << "\n";
    }
  }

  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];
    int index = e->index;
    if(index < 1)
      index = 1;
    if(e->nature == PDE_BULK) {
      mesh_element << ++current << " ";
      mesh_element << index << " ";
      mesh_element << e->code << " ";
      for(int j=0; j<e->nodes; j++) 
	mesh_element << e->node[j]+1 << " ";
      mesh_element << "\n";
    }
  }

  for(int i=0; i < mesh->points; i++) {
    point_t *p = &mesh->point[i];
    int index = p->index;
    if(index < 1)
      index = 1;
    if(p->nature == PDE_BULK) {
      mesh_element << ++current << " ";
      mesh_element << index << " ";
      mesh_element << p->code << " ";
      mesh_element << p->node << "\n";
    }
  }

  file.close();
  
  // Boundary elements:
  file.setFileName("mesh.boundary");
  file.open(QIODevice::WriteOnly);
  QTextStream mesh_boundary(&file);

  current = 0;

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    int e0 = s->element[0] + 1;
    int e1 = s->element[1] + 1;
    if(e0 < 0)
      e0 = 0;
    if(e1 < 0)
      e1 = 0;
    int index = s->index;
    if(index < 1)
      index = 1;
    if(s->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << index << " ";
      mesh_boundary << e0 << " " << e1 << " ";
      mesh_boundary << s->code << " ";
      for(int j=0; j < s->nodes; j++) 
	mesh_boundary << s->node[j]+1 << " ";
      mesh_boundary << "\n";
    }
  }

  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];
    int s0 = e->surface[0] + 1;
    int s1 = e->surface[1] + 1;
    if(s0 < 0)
      s0 = 0;
    if(s1 < 0)
      s1 = 0;
    int index = e->index;
    if(index < 1)
      index = 1;
    if(e->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << index << " ";
      mesh_boundary << s0 << " " << s1 << " ";
      mesh_boundary << e->code << " ";
      for(int j=0; j < e->nodes; j++) 
	mesh_boundary << e->node[j]+1 << " ";
      mesh_boundary << "\n";
    }
  }

  for(int i=0; i < mesh->points; i++) {
    point_t *p = &mesh->point[i];
    int index = p->index;
    if(index < 1)
      index = 1;
    // Todo: parents
    if(p->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << index << " ";
      mesh_boundary << -1 << " " << -1 << " ";
      mesh_boundary << p->code << " ";
      mesh_boundary << p->node+1 << "´\n";
    }
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

  delete [] bulk_by_type;
  delete [] boundary_by_type;

  statusBar()->showMessage(tr("Ready"));
}




// Boundady selected by double clicking (signaled by glWidget::select):
//-----------------------------------------------------------------------------
void MainWindow::boundarySelectedSlot(list_t *l)
{
  QString qs;

  if(l->index < 0) {
    statusBar()->showMessage("Ready");    
    return;
  }

  if(!l->selected) {
    if(l->type == SURFACELIST) {
      qs = "Selected surface " + QString::number(l->index);
    } else if(l->type == EDGELIST) {
      qs = "Selected edge " + QString::number(l->index);
    } else {
      qs = "Selected object " + QString::number(l->index) + " (type unknown)";
    }
  } else {
    if(l->type == SURFACELIST) {
      qs = "Unselected surface " + QString::number(l->index);
    } else if(l->type == EDGELIST) {
      qs = "Unselected edge " + QString::number(l->index);
    } else {
      qs = "Unselected object " + QString::number(l->index) + " (type unknown)";
    }
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

  textEdit->clear();
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

  // find out mesh domain ids:
  // -------------------------
  mesh_t *mesh = glWidget->mesh;
  char str[1024];

  int maxindex = 0;
  for(int i=0; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];
    if((element->nature == PDE_BULK) && (element->index > maxindex))
      maxindex = element->index;
  }

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if((surface->nature == PDE_BULK) && (surface->index > maxindex))
      maxindex = surface->index;
  }

  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if((edge->nature == PDE_BULK) && (edge->index > maxindex))
      maxindex = edge->index;
  }
  maxindex++;

  bool *body_tmp = new bool[maxindex];
  int  *body_id  = new  int[maxindex];

  for(int i=0; i<maxindex; i++) body_tmp[i] = false;

  maxindex = 0;
  for(int i=0; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];
    if(element->nature == PDE_BULK)
      if ( !body_tmp[element->index] ) {
        body_tmp[element->index] = true;
        body_id[maxindex++] = element->index;
      }
  }

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if(surface->nature == PDE_BULK)
      if ( !body_tmp[surface->index] ) {
        body_tmp[surface->index] = true;
        body_id[maxindex++] = surface->index;
      }
  }
  
  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if(edge->nature == PDE_BULK)
      if ( !body_tmp[edge->index] ) {
        body_tmp[edge->index] = true;
        body_id[maxindex++] = edge->index;
      }
  }
  if ( maxindex==0 ) { maxindex=1; body_id[0]=1; }

  textEdit->append("Body 1");
  textEdit->append("  Name = \"Body1\"");
  sprintf( str, "  Target Bodies(%d)=", maxindex );
  for( int i=0; i<maxindex; i++ ) {
     sprintf( str, "%s %d", str, max(body_id[i],1) );
  }
  delete [] body_tmp; delete [] body_id;

  textEdit->append(str);
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
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if((surface->nature == PDE_BOUNDARY) && (surface->index > maxindex))
      maxindex = surface->index;
  }

  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if((edge->nature == PDE_BOUNDARY) && (edge->index > maxindex))
      maxindex = edge->index;
  }
  maxindex++;

  bool *tmp = new bool[maxindex];

  for(int i=0; i<maxindex; i++) 
    tmp[i] = false;

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if(surface->nature == PDE_BOUNDARY)
      tmp[surface->index] = true;
  }
  
  for(int i=0; i < mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if(edge->nature == PDE_BOUNDARY)
      tmp[edge->index] = true;
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
