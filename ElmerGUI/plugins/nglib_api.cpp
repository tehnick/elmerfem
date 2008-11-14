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
 *  ELMER/Mesh3D nglib_api                                                   *
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
#include "nglib_api.h"

using namespace std;

NglibAPI::NglibAPI()
{
}


NglibAPI::~NglibAPI()
{
}

// Populate elmer's mesh structure:
//-----------------------------------------------------------------------------
mesh_t* NglibAPI::createElmerMeshStructure()
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
  mesh->nodes = nglib::Ng_GetNP(ngmesh);
  mesh->node = new node_t[mesh->nodes];

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    nglib::Ng_GetPoint(ngmesh, i+1, node->x);

    node->index = -1; // default
  }

  // Boundary elements:				       
  mesh->surfaces = nglib::Ng_GetNSE(ngmesh);
  mesh->surface = new surface_t[mesh->surfaces];

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];

    surface->nature = PDE_BOUNDARY;

    surface->code = 303;

    surface->nodes = 3;
    surface->node = new int[3];
    
    surface->edges = 3;
    surface->edge = new int[3];

    surface->index = 1; // default

    surface->edge[0] = -1;
    surface->edge[1] = -1;
    surface->edge[2] = -1;

    // data for surface->element is not available

    nglib::Ng_GetSurfaceElement(ngmesh, i+1, surface->node);
    
    surface->node[0]--;
    surface->node[1]--;
    surface->node[2]--;

    // swap orientation:
    int tmp = surface->node[1];
    surface->node[1] = surface->node[2];
    surface->node[2] = tmp;
  }


  // Elements:
  mesh->elements = nglib::Ng_GetNE(ngmesh);
  mesh->element = new element_t[mesh->elements];


  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    element->nature = PDE_BULK;

    element->code = 504;

    element->nodes = 4;
    element->node = new int[4];

    nglib::Ng_GetVolumeElement(ngmesh, i+1, element->node);
        
    element->node[0]--;
    element->node[1]--;
    element->node[2]--;
    element->node[3]--;

    element->index = 1; // default
  }

  // Find parents for surface elements:
  meshutils.findSurfaceElementParents(mesh);

  // Find edges for surface elements:
  meshutils.findSurfaceElementEdges(mesh);

  // Compute normals for boundary elements:
  meshutils.findSurfaceElementNormals(mesh);

  mesh->dim = 3;
  mesh->cdim = 3;

  return mesh;
}
