#include <QtGui>
#include <iostream>
#include <stdio.h>

#include "meshingthread.h"


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


void MeshingThread::generate(int generatorType,
			     QString cs, TetlibAPI *tetlibAPI,
			     nglib::Ng_Mesh *ngmesh,
			     nglib::Ng_STL_Geometry *nggeom,
			     nglib::Ng_Meshing_Parameters &mp)
  
{
  QMutexLocker locker(&mutex);

  this->generatorType = generatorType;

  this->tetgenControlString = cs;
  this->tetlibAPI = tetlibAPI;
  this->in = tetlibAPI->in;
  this->out = tetlibAPI->out;
  this->delegate_tetrahedralize = tetlibAPI->delegate_tetrahedralize;

  this->ngmesh = ngmesh;
  this->nggeom = nggeom;
  this->mp = &mp;

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
      
      std::cout << "Mesh generator: control string: " << std::string(tetgenControlString.toAscii()) << std::endl;
      std::cout << "Mesh generator: input points: " << in->numberofpoints << std::endl;
      std::cout.flush();
      
      out->deinitialize();
      out->initialize();    
      
      sprintf(ss, "%s", (const char*)(tetgenControlString.toAscii()));
      if(delegate_tetrahedralize)
	delegate_tetrahedralize(1, NULL, ss, in, out, NULL, NULL);      
      
      std::cout << "Mesh generator: nodes: " << out->numberofpoints << std::endl;
      std::cout << "Mesh generator: elements: " << out->numberoftetrahedra << std::endl;
      std::cout << "Mesh generator: boundary elements: " << out->numberoftrifaces << std::endl;
      std::cout.flush();
      
    } else if(generatorType==GEN_NGLIB) {
      
      int rv = nglib::Ng_STL_MakeEdges(nggeom, ngmesh, mp);
      std::cout << "Make Edges: Ng_result=" << rv << std::endl;
      
      rv = nglib::Ng_STL_GenerateSurfaceMesh(nggeom, ngmesh, mp);
      std::cout << "Generate Surface Mesh: Ng_result=" << rv << std::endl;
      
      rv = nglib::Ng_GenerateVolumeMesh(ngmesh, mp);
      std::cout << "Generate Volume Mesh: Ng_result=" << rv << std::endl;
      
      int np = nglib::Ng_GetNP(ngmesh);
      std::cout << "Meshing thtread: nodes: " << np << std::endl;
      
      int ne = nglib::Ng_GetNE(ngmesh);
      std::cout << "Meshing thtread: elements: " << ne << std::endl;

      int nse = nglib::Ng_GetNSE(ngmesh);
      std::cout << "Meshing thtread: boundary elements: " << nse << std::endl;      
      std::cout.flush();
      
    } else {
      
      std::cout << "Meshgen: unknown generator type\n";
      std::cout.flush();
      
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
