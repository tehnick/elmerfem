#ifndef SOLVERTHREAD_H
#define SOLVERTHREAD_H

#include <QMutex>
#include <QThread>
#include <QWaitCondition>

class SolverThread : public QThread
{
  Q_OBJECT

public:
  SolverThread(QObject *parent = 0);
  ~SolverThread();

  void startSolver();

signals:
  void signalSolverReady();
  
protected:
  void run();
  
private:
  QMutex mutex;
  QWaitCondition condition;
  
  bool restart;
  bool abort;
};

#endif // SOLVERTHREAD_H
