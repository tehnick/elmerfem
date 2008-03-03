#ifndef MESHINGTHREAD_H
#define MESHINGTHREAD_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001

#include <QMutex>
#include <QThread>
#include <QWaitCondition>

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "tetlib_api.h"
#include "nglib_api.h"

namespace nglib {
#include <nglib.h>
}

class MeshingThread : public QThread
{
  Q_OBJECT

public:
  MeshingThread(QObject *parent = 0);
  ~MeshingThread();

  void generate(int generatorType, QString cs, TetlibAPI *tetlibAPI, 
		nglib::Ng_Mesh *ngmesh, nglib::Ng_STL_Geometry *nggeom, 
		nglib::Ng_Meshing_Parameters *mp, NglibAPI *nglibAPI);

signals:
  void signalMeshOk();
  
protected:
  void run();
  
private:
  QMutex mutex;
  QWaitCondition condition;
  
  bool restart;
  bool abort;

  int generatorType;

  // tetlib:
  QString tetgenControlString;
  TetlibAPI *tetlibAPI;
  tetgenio *in;
  tetgenio *out;
  delegate_tetrahedralize_t delegate_tetrahedralize;

  // nglib:
  NglibAPI *nglibAPI;
  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters *mp;

};

#endif
