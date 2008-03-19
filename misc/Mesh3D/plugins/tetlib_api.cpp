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
 *  ELMER/Mesh3D tetlib_api                                                  *
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

#include <iostream>
#include "tetlib_api.h"

using namespace std;

TetlibAPI::TetlibAPI()
{
}


TetlibAPI::~TetlibAPI()
{
}


bool TetlibAPI::loadTetlib()
{
  cout << "Load tetlib...";

#ifdef WIN32
  hTetlib = LoadLibrary(TEXT("libtet.dll"));
#else
  hTetlib = dlopen("libtet.so", RTLD_LAZY);  
#endif
  
  if(!hTetlib) {
    cout << "unable to load library\n";
    cout << "tetlib functionality unavailable\n";
    cout.flush();
    return false;
  }

  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
#define DLSYMPROC GetProcAddress
#else
#define DLSYMPROC dlsym
#endif

  if(!(ptetgenio = (tetgenio_t) DLSYMPROC(hTetlib, "CreateObjectOfTetgenio")))
    {
      cout << "Unable to get proc address for 'tetgenio'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  in = (ptetgenio)();
  out = (ptetgenio)(); 

  if(!(delegate_tetrahedralize = (delegate_tetrahedralize_t) DLSYMPROC(hTetlib, "delegate_tetrahedralize")))
    {
      cout << "Unable to get proc address for 'delegate_tetrahedralize'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  return true;
}



// Populate elmer's mesh structure:
//-----------------------------------------------------------------------------
mesh_t* TetlibAPI::createElmerMeshStructure()
{
  Helpers helpers;
  Meshutils meshutils;

  // Create new mesh structure:
  mesh_t *mesh = new mesh_t;

  mesh->nodes = 0;
  mesh->points = 0;
  mesh->edges = 0;
  mesh->surfaces = 0;
  mesh->elements = 0;
  
  // Nodes:
  mesh->nodes = out->numberofpoints;
  mesh->node = new node_t[mesh->nodes];

  REAL *pointlist = out->pointlist;

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];
    
    node->x[0] = *pointlist++;
    node->x[1] = *pointlist++;
    node->x[2] = *pointlist++;

    node->index = -1; // default
  }

  // Elements:
  mesh->elements = out->numberoftetrahedra;
  mesh->element = new element_t[mesh->elements];

  int *tetrahedronlist = out->tetrahedronlist;
  REAL *attribute = out->tetrahedronattributelist;
  int na =  out->numberoftetrahedronattributes;
  
  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    element->nature = PDE_BULK;

    element->code = 504;

    element->nodes = 4;
    element->node = new int[4];
    
    element->node[0] = (*tetrahedronlist++) - out->firstnumber;
    element->node[1] = (*tetrahedronlist++) - out->firstnumber;
    element->node[2] = (*tetrahedronlist++) - out->firstnumber;
    element->node[3] = (*tetrahedronlist++) - out->firstnumber;
    
    element->index = 1; // default
    // must have "A" in control string:
    if(out->tetrahedronattributelist != (REAL*)NULL) 
      element->index = (int)attribute[na*(i+1)-1];
  }
  
  // Boundary elements:
  mesh->surfaces = out->numberoftrifaces;
  mesh->surface = new surface_t[mesh->surfaces];

  int *trifacelist = out->trifacelist;
  int *adjtetlist = out->adjtetlist;

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    surface->nature = PDE_BOUNDARY;

    surface->code = 303;

    surface->nodes = 3;
    surface->node = new int[3];

    surface->edges = 3;
    surface->edge = new int[3];

    surface->elements = 2;
    surface->element = new int[2];

    surface->index = 1; // default
    if(out->trifacemarkerlist != (int*)NULL)
      surface->index = out->trifacemarkerlist[i];

    surface->edge[0] = -1;
    surface->edge[1] = -1;
    surface->edge[2] = -1;
    
    surface->element[0] = -1; // default
    surface->element[1] = -1;
    // must have "nn" in control string:
    if(out->adjtetlist != (int*)NULL) {
      surface->element[0] = (*adjtetlist++) - out->firstnumber;
      surface->element[1] = (*adjtetlist++) - out->firstnumber;
    }

    int u = (*trifacelist++) - out->firstnumber;
    int v = (*trifacelist++) - out->firstnumber;
    int w = (*trifacelist++) - out->firstnumber;

    surface->node[0] = u;
    surface->node[1] = v;
    surface->node[2] = w;
  }

  // Edges:
  meshutils.findSurfaceElementEdges(mesh);
  meshutils.findSurfaceElementNormals(mesh);

  // Points:
  mesh->points = 0;
  // mesh->point == NULL;
  
  mesh->dim = 3;
  mesh->cdim = 3;

  return mesh;
}
