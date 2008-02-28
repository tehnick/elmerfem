#include <QtGui>
#include <iostream>
#include <stdio.h>
#include "meshingthread.h"

using namespace std;

MeshingThread::MeshingThread(QObject *parent)
  : QThread(parent)
{
  restart = false;
  abort = false;
}



MeshingThread::~MeshingThread()
{
  mutex.lock();
  abort = true;
  condition.wakeOne();
  mutex.unlock();
  wait();
}

void MeshingThread::generate(int generatorType, QString cs,
			     TetlibAPI *tetlibAPI,
			     nglib::Ng_Mesh *ngmesh,
			     nglib::Ng_STL_Geometry *nggeom,
			     nglib::Ng_Meshing_Parameters *mp,
			     NglibAPI *nglibAPI)
{
  QMutexLocker locker(&mutex);

  this->generatorType = generatorType;

  this->tetgenControlString = cs;
  this->tetlibAPI = tetlibAPI;
  this->in = tetlibAPI->in;
  this->out = tetlibAPI->out;
  this->delegate_tetrahedralize = tetlibAPI->delegate_tetrahedralize;

  this->nglibAPI = nglibAPI;
  this->ngmesh = ngmesh;
  this->nggeom = nggeom;
  this->mp = mp;

  if (!isRunning()) {
    start(LowPriority);
  } else {
    restart = true;
    condition.wakeOne();
  }
}

void MeshingThread::run()
{
  QString qs;
  char ss[1024];

  forever {
    mutex.lock();

    // Here, set values to variables that need mutex locked:
    // .....

    mutex.unlock();

    if(abort)
      return;

    if(generatorType==GEN_TETLIB) {
      
      cout << "Mesh generator: control string: " << string(tetgenControlString.toAscii()) << endl;
      cout << "Mesh generator: input points: " << in->numberofpoints << endl;
      cout.flush();
      
      out->deinitialize();
      out->initialize();    
      
      sprintf(ss, "%s", (const char*)(tetgenControlString.toAscii()));
      if(delegate_tetrahedralize)
	delegate_tetrahedralize(1, NULL, ss, in, out, NULL, NULL);      
      
      cout << "Mesh generator: nodes: " << out->numberofpoints << endl;
      cout << "Mesh generator: elements: " << out->numberoftetrahedra << endl;
      cout << "Mesh generator: boundary elements: " << out->numberoftrifaces << endl;
      cout.flush();
      
    } else if(generatorType==GEN_NGLIB) {
      
      int rv = nglibAPI->Ng_STL_MakeEdges(nggeom, ngmesh, mp);
      cout << "Make Edges: Ng_result=" << rv << endl;
      
      rv = nglibAPI->Ng_STL_GenerateSurfaceMesh(nggeom, ngmesh, mp);
      cout << "Generate Surface Mesh: Ng_result=" << rv << endl;
      
      rv = nglibAPI->Ng_GenerateVolumeMesh(ngmesh, mp);
      cout << "Generate Volume Mesh: Ng_result=" << rv << endl;
      
      int np = nglibAPI->Ng_GetNP(ngmesh);
      cout << "Meshing thtread: nodes: " << np << endl;
      
      int ne = nglibAPI->Ng_GetNE(ngmesh);
      cout << "Meshing thtread: elements: " << ne << endl;

      int nse = nglibAPI->Ng_GetNSE(ngmesh);
      cout << "Meshing thtread: boundary elements: " << nse << endl;      
      cout.flush();
      
    } else {
      
      cout << "Meshgen: unknown generator type\n";
      cout.flush();
      
    }
       
    // emit "ok" to the main thread:
    if(!restart)
      emit(generatorFinished());
    
    mutex.lock();
    
    if (!restart) 
      condition.wait(&mutex);
    
    restart = false;
    mutex.unlock();    
  }
}
