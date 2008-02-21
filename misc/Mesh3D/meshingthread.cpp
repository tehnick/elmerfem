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


void MeshingThread::generate(QString cs, tetgenio &in, tetgenio &out)
{
  QMutexLocker locker(&mutex);

  this->tetgenControlString = cs;
  this->in = &in;
  this->out = &out;

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

    std::cout << "Mesh generator: control string: " << std::string(tetgenControlString.toAscii()) << std::endl;
    std::cout << "Mesh generator: input points: " << in->numberofpoints << std::endl;
    std::cout.flush();
    
    if(abort)
      return;
    
    // Must be cleared:
    delete [] out->trifacemarkerlist;

    out->initialize();    
    out->firstnumber = 1;
    
    // Call tetgen:
    sprintf(ss, "%s", (const char*)(tetgenControlString.toAscii()));
    tetrahedralize(ss, in, out);

    std::cout << "Mesh generator: nodes: " << out->numberofpoints << std::endl;
    std::cout << "Mesh generator: elements: " << out->numberoftetrahedra << std::endl;
    std::cout << "Mesh generator: boundary elements: " << out->numberoftrifaces << std::endl;
    std::cout.flush();

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
