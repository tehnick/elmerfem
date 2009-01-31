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

void NglibAPI::setDim(int ngDim)
{
  this->ngDim = ngDim;
}

int NglibAPI::getDim()
{
  return this->ngDim;
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

  bool twod = (ngDim == 2) ? true : false;
  
  // Nodes:
  //--------
  if(twod) {
    mesh->setNodes(nglib::Ng_GetNP_2D(ngmesh));
  } else {
    mesh->setNodes(nglib::Ng_GetNP(ngmesh));
  }

  mesh->newNodeArray(mesh->getNodes());

  for(int i = 0; i < mesh->getNodes(); i++) {
    node_t *node = mesh->getNode(i);

    if(twod) {
      double *x = node->getXvec();
      x[0] = 0; x[1] = 0; x[2] = 0;
      nglib::Ng_GetPoint_2D(ngmesh, i+1, x);
    } else {
      nglib::Ng_GetPoint(ngmesh, i+1, node->getXvec());
    }

    node->setIndex(-1); // default
  }

  // Boundary elements:
  //--------------------

  if(twod) {

    // 2D:
    //-----
    mesh->setEdges(nglib::Ng_GetNSeg_2D(ngmesh));
    mesh->newEdgeArray(mesh->getEdges());

    for(int i=0; i < mesh->getEdges(); i++) {
      edge_t *edge = mesh->getEdge(i);
      
      edge->setNature(PDE_BOUNDARY);
      edge->setCode(202);
      edge->setNodes(2);
      edge->newNodeIndexes(2);
      edge->setPoints(2);
      edge->newPointIndexes(2);

      int face = 1; // default

      edge->setIndex(face);      
      edge->setPointIndex(0, -1);
      edge->setPointIndex(1, -1);
      
      // data for edge->points is not available
      
      nglib::Ng_GetSegment_2D(ngmesh, i+1, edge->getNodeIndexes());
      
      edge->setNodeIndex(0, edge->getNodeIndex(0) - 1);
      edge->setNodeIndex(1, edge->getNodeIndex(1) - 1);
      
      // swap orientation:
      //------------------
      int tmp = edge->getNodeIndex(0);
      edge->setNodeIndex(0, edge->getNodeIndex(1));
      edge->setNodeIndex(1, tmp);
    }

  } else {

    // 3D:
    //-----
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
  }

  // Elements:
  //-----------
  if(twod) {

    // 2D:
    //-----
    mesh->setSurfaces(nglib::Ng_GetNE_2D(ngmesh));
    mesh->newSurfaceArray(mesh->getSurfaces()); 
    
    for(int i = 0; i < mesh->getSurfaces(); i++) {
      surface_t *surface = mesh->getSurface(i);
      
      surface->setNature(PDE_BULK);
      surface->setCode(303);
      surface->setNodes(3);
      surface->newNodeIndexes(3);
      
      nglib::Ng_GetElement_2D(ngmesh, i+1, surface->getNodeIndexes());
      
      surface->setNodeIndex(0, surface->getNodeIndex(0) - 1);
      surface->setNodeIndex(1, surface->getNodeIndex(1) - 1);
      surface->setNodeIndex(2, surface->getNodeIndex(2) - 1);
      
      surface->setIndex(1); // default
    }

  } else {

    // 3D:
    //-----
    mesh->setElements(nglib::Ng_GetNE(ngmesh));
    mesh->newElementArray(mesh->getElements()); 
    
    for(int i = 0; i < mesh->getElements(); i++) {
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
  }

  mesh->setDim(ngDim);
  mesh->setCdim(ngDim);

  return mesh;
}
