#ifndef MESHINGTHREAD_H
#define MESHINGTHREAD_H

#include <QMutex>
#include <QThread>
#include <QWaitCondition>

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "plugins/tetlib_api.h"
#include "plugins/nglib_api.h"

namespace nglib {
#include "plugins/nglib.h"
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

  void stopMeshing();

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
