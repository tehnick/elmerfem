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
  //----------------------------
  mesh_t *mesh = new mesh_t;

  mesh->setNodes(0);
  mesh->setPoints(0);
  mesh->setEdges(0);
  mesh->setSurfaces(0);
  mesh->setElements(0);

  // Nodes:
  //--------
  mesh->setNodes(nglib::Ng_GetNP(ngmesh));
  mesh->newNodeArray(mesh->getNodes());

  for(int i=0; i < mesh->getNodes(); i++) {
    node_t *node = mesh->getNode(i);

    nglib::Ng_GetPoint(ngmesh, i+1, node->getXvec());

    node->setIndex(-1); // default
  }

  // Boundary elements:
  //--------------------
  mesh->setSurfaces(nglib::Ng_GetNSE(ngmesh));
  mesh->newSurfaceArray(mesh->getSurfaces());

  for(int i=0; i < mesh->getSurfaces(); i++) {
    surface_t *surface = mesh->getSurface(i);

    surface->setNature(PDE_BOUNDARY);
    surface->setCode(303);
    surface->setNodes(3);
    surface->newNodeIndexes(3);
    surface->setEdges(3);
    surface->newEdgeIndexes(3);

    int face = nglib::EG_GetSurfaceElementBCProperty(ngmesh, i+1);

    surface->setIndex(face);

    surface->setEdgeIndex(0, -1);
    surface->setEdgeIndex(1, -1);
    surface->setEdgeIndex(2, -1);

    // data for surface->element is not available

    nglib::Ng_GetSurfaceElement(ngmesh, i+1, surface->getNodeIndexes());
    
    surface->setNodeIndex(0, surface->getNodeIndex(0) - 1);
    surface->setNodeIndex(1, surface->getNodeIndex(1) - 1);
    surface->setNodeIndex(2, surface->getNodeIndex(2) - 1);

    // swap orientation:
    //------------------
    int tmp = surface->getNodeIndex(1);
    surface->setNodeIndex(1, surface->getNodeIndex(2));
    surface->setNodeIndex(2, tmp);
  }

  // Elements:
  //-----------
  mesh->setElements(nglib::Ng_GetNE(ngmesh));
  mesh->newElementArray(mesh->getElements()); 

  for(int i=0; i< mesh->getElements(); i++) {
    element_t *element = mesh->getElement(i);

    element->setNature(PDE_BULK);
    element->setCode(504);
    element->setNodes(4);
    element->newNodeIndexes(4);

    nglib::Ng_GetVolumeElement(ngmesh, i+1, element->getNodeIndexes());
    
    element->setNodeIndex(0, element->getNodeIndex(0) - 1);
    element->setNodeIndex(1, element->getNodeIndex(1) - 1);
    element->setNodeIndex(2, element->getNodeIndex(2) - 1);
    element->setNodeIndex(3, element->getNodeIndex(3) - 1);

    element->setIndex(1); // default
  }

  // Find parents for surface elements:
  //------------------------------------
  meshutils.findSurfaceElementParents(mesh);

  // Find edges for surface elements:
  //----------------------------------
  meshutils.findSurfaceElementEdges(mesh);

  // Compute normals for boundary elements:
  //---------------------------------------
  meshutils.findSurfaceElementNormals(mesh);

  mesh->setDim(3);
  mesh->setCdim(3);

  return mesh;
}
