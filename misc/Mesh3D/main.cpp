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
 *  ElmerGUI main                                                            *
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

#include <QApplication>
#include "mainwindow.h"

#ifdef __APPLE__

#include <mach-o/dyld.h>
#include <stdlib.h>
#define MAXPATHLENGTH 600

#endif

int main(int argc, char *argv[])
{
#ifdef __APPLE__
// we'll change ENVIRONMENT so that the Elmer binaries and libraries
// hidden wihtin the application bundle will be correctly found

  char executablePath[MAXPATHLENGTH] = {0};
  uint32_t len = MAXPATHLENGTH;
  if(! _NSGetExecutablePath( (char*) executablePath, &len)){
    // remove executable name from path:
    *(strrchr(executablePath,'/'))='\0';
    char *oldValue = 0, *newValue = 0;
    
    oldValue = getenv("PATH");
    asprintf(&newValue, "%s/../bin:%s",executablePath,oldValue);
    setenv("PATH",newValue,1);
    free(newValue);
    
    oldValue = getenv("DYLD_LIBRARY_PATH");
    asprintf(&newValue,"%s/../lib:%s",executablePath,oldValue);
    setenv("DYLD_LIBRARY_PATH",newValue,0);
    free(newValue);
    
    asprintf(&newValue,"%s/..",executablePath);        
    setenv("ELMER_HOME",newValue,0);
    free(newValue);
    
    asprintf(&newValue,"%s/../share/elmerpost",executablePath);        
    setenv("ELMER_POST_HOME",newValue,0);
    free(newValue);
    
    
#ifdef DEBUG
    printf("PATH = %s\nDYLD_LIBRARY_PATH=%s\nELMER_HOME=%s\n", 
	   getenv("PATH"), 
	   getenv("DYLD_LIBRARY_PATH"), 
	   getenv("ELMER_HOME"));
#endif
  }    
#endif
  
  //========================================================================

  QApplication app(argc, argv);
  MainWindow mainWindow;
  mainWindow.show();
  return app.exec();
}
