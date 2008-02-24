#ifndef MESHINGTHREAD_H
#define MESHINGTHREAD_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001

#include <QMutex>
#include <QThread>
#include <QWaitCondition>

#include <tetgen.h>

namespace nglib {
#include <nglib.h>
}

class MeshingThread : public QThread
{
  Q_OBJECT

public:
  MeshingThread(QObject *parent = 0);
  ~MeshingThread();

  void generate(int generatorType, QString cs, tetgenio &in, tetgenio &out, 
		nglib::Ng_Mesh *ngmesh, nglib::Ng_STL_Geometry *nggeom,
		nglib::Ng_Meshing_Parameters &mp);
  
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

  QString tetgenControlString;
  tetgenio *in;
  tetgenio *out;

  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters *mp;

};

#endif
