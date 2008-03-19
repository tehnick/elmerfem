/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ELMER/Mesh3D SolverThread                                                *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <QtGui>
#include "solverthread.h"

#include <ext/stdio_filebuf.h>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <iostream>

using namespace std;

SolverThread::SolverThread(QObject *parent)
  : QThread(parent)
{
  restart = false;
  abort = false;
}



SolverThread::~SolverThread()
{
  mutex.lock();
  abort = true;
  condition.wakeOne();
  mutex.unlock();
  wait();
}

void SolverThread::startSolver(QTextEdit *te)
{
  this->te = te;

  QMutexLocker locker(&mutex);
  
  if (!isRunning()) {
    start(LowPriority);
  } else {
    restart = true;
    condition.wakeOne();
  }
}

void SolverThread::run()
{
  forever {
    mutex.lock();
    
    // Here, set values to variables that need mutex locked...
    
    mutex.unlock();

    if(abort)
      return;

    te->append("Ok start solving.");
    te->append("Logging to this textEdit is currently broken, though...");
    
    // open log file
    QFile logfile;
    logfile.setFileName("ElmerSolver.log");
    logfile.open(QIODevice::WriteOnly);
    QTextStream logstream(&logfile);

    // open process
    FILE *fp = popen("ElmerSolver", "r");

    // redirect stdout
    __gnu_cxx::stdio_filebuf<char> fb(fp, ios::in);
    istream f(&fb);
    
    // write to stdout && log
    QString qs;
    char c;
    for(c = f.get(); !f.eof(); c = f.get()) {

      // broken:
      //qs = c;
      //te->append(qs);

      logstream << c;
      cout << c;
      cout.flush();
    }

    // close process
    pclose(fp);

    // close log file
    logfile.close();

    // emit "ok"
    if(!restart)
      emit(signalSolverReady());
    
    mutex.lock();
    
    if (!restart) 
      condition.wait(&mutex);
    
    restart = false;
    mutex.unlock();    
  }
}
