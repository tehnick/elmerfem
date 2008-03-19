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
 *  ELMER/Mesh3D PostProcessingThread                                        *
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
#include <iostream>
#include <stdio.h>
#include "postprocessingthread.h"

using namespace std;

PostProcessingThread::PostProcessingThread(QObject *parent)
  : QThread(parent)
{
  restart = false;
  abort = false;
}



PostProcessingThread::~PostProcessingThread()
{
  mutex.lock();
  abort = true;
  condition.wakeOne();
  mutex.unlock();
  wait();
}

void PostProcessingThread::startPostProcessing()
{
  QMutexLocker locker(&mutex);
  
  if (!isRunning()) {
    start(LowPriority);
  } else {
    restart = true;
    condition.wakeOne();
  }
}

void PostProcessingThread::run()
{
  forever {
    mutex.lock();
    
    // Here, set values to variables that need mutex locked...
    
    mutex.unlock();
    
    if(abort)
      return;
    
    system( "ElmerPost \"readfile skeleton.ep;"
            "set ColorScaleColor Temperature;"
            "set DisplayStyle(ColorScale) 1;"
            "set MeshStyle 1;"
            "set MeshColor Temperature;"
            "set DisplayStyle(ColorMesh) 1;"
            "UpdateObject;\"" );
    
    if(!restart)
      emit(signalPostProcessingReady());
    
    mutex.lock();
    
    if (!restart) 
      condition.wait(&mutex);
    
    restart = false;
    mutex.unlock();    
  }
}
