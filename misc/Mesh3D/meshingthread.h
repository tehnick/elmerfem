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

#include "tetgen.h"

namespace nglib {
#include <nglib.h>
}

class MeshingThread : public QThread
{
  Q_OBJECT

public:
  MeshingThread(QObject *parent = 0);
  ~MeshingThread();

#ifdef WIN32
  void generate(int generatorType, QString cs, tetgenio *in, tetgenio *out, 
		nglib::Ng_Mesh *ngmesh, nglib::Ng_STL_Geometry *nggeom,
		nglib::Ng_Meshing_Parameters &mp, HINSTANCE hTetlib);
#else
  void generate(int generatorType, QString cs, tetgenio *in, tetgenio *out, 
		nglib::Ng_Mesh *ngmesh, nglib::Ng_STL_Geometry *nggeom,
		nglib::Ng_Meshing_Parameters &mp, void *hTetlib);
#endif
  
signals:
  void generatorFinished();
  
protected:
  void run();
  
private:
  QMutex mutex;
  QWaitCondition condition;
  
  bool restart;
  bool abort;

  int generatorType;

  // tetlib:
  typedef void (*delegate_tetrahedralize_t)(int, char *tetgenbehavior,
					    char*, tetgenio*, tetgenio*, 
					    tetgenio*, tetgenio*);
  QString tetgenControlString;
  tetgenio *in;
  tetgenio *out;
#ifdef WIN32
  HINSTANCE hTetlib;
#else
  void *hTetlib;
#endif
  delegate_tetrahedralize_t delegate_tetrahedralize;

  // nglib:
  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters *mp;

};

#endif
