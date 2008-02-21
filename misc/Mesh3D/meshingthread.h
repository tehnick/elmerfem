#ifndef MESHINGTHREAD_H
#define MESHINGTHREAD_H

#include <QMutex>
#include <QThread>
#include <QWaitCondition>
#include <tetgen.h>

class MeshingThread : public QThread
{
  Q_OBJECT

public:
  MeshingThread(QObject *parent = 0);
  ~MeshingThread();

  void generate(QString cs, tetgenio &in, tetgenio &out);
  
signals:
  void generatorFinished();
  
protected:
  void run();
  
private:
  QMutex mutex;
  QWaitCondition condition;
  
  bool restart;
  bool abort;
  
  tetgenio *in;
  tetgenio *out;

  QString tetgenControlString;
};

#endif
