#ifndef POSTPROCESSINGTHREAD_H
#define POSTPROCESSINGTHREAD_H

#include <QMutex>
#include <QThread>
#include <QWaitCondition>

class PostProcessingThread : public QThread
{
  Q_OBJECT

public:
  PostProcessingThread(QObject *parent = 0);
  ~PostProcessingThread();

  void startPostProcessing();

signals:
  void signalPostProcessingReady();
  
protected:
  void run();
  
private:
  QMutex mutex;
  QWaitCondition condition;
  
  bool restart;
  bool abort;
};

#endif // POSTPROCESSINGTHREAD_H
