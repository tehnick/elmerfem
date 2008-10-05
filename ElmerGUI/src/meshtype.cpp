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
 *  ElmerGUI mesh_t (Elmer mesh structure)                                   *
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
#include <fstream>
#include "meshtype.h"
using namespace std;

mesh_t::mesh_t()
{
  setDefaults();
}


mesh_t::~mesh_t()
{
}

bool mesh_t::isUndefined()
{
  if((cdim < 0) || (dim < 0) || (nodes < 1))
    return true;

  return false;
}

void mesh_t::clear()
{
  delete [] element;
  delete [] surface;
  delete [] edge;
  delete [] point;
  delete [] node;
  
  setDefaults();
}

void mesh_t::setDefaults()
{
  cdim = -1;
  dim = -1;
  nodes = 0;
  node = 0;
  points = 0;
  point = 0;
  edges = 0;
  edge = 0;
  surfaces = 0;
  surface = 0;
  elements = 0;
  element = 0;
}

// Load Elmer mesh files and populate mesh structures
//---------------------------------------------------------------------------
bool mesh_t::load(char* dirName)
{
  char fileName[1024];
  ifstream mesh_header;
  ifstream mesh_nodes;
  ifstream mesh_elements;
  ifstream mesh_boundary;

  // Header:
  //--------
  sprintf(fileName, "%s/mesh.header", dirName);
  mesh_header.open(fileName);

  if(!mesh_header.is_open()) {
    cout << "Mesh: load: unable to open " << fileName << endl;
    return false;
  }

  int nodes, elements, boundaryelements, types, type, ntype;

  mesh_header >> nodes >> elements >> boundaryelements;
  mesh_header >> types;

  int elements_zero_d = 0;
  int elements_one_d = 0;
  int elements_two_d = 0;
  int elements_three_d = 0;
  
  for(int i = 0; i < types; i++) {
    mesh_header >> type >> ntype;
    
    switch(type/100) {
    case 1:
      elements_zero_d += ntype;
      break;
    case 2:
      elements_one_d += ntype;
      break;
    case 3:
    case 4:
      elements_two_d += ntype;
      break;
    case 5:
    case 6:
    case 7:
    case 8:
      elements_three_d += ntype;
      break;
    default:
      cout << "Unknown element family (possibly not implamented)" << endl;
      cout.flush();
      return false;
      // exit(0);
    }
  }
  
  cout << "Summary:" << endl;
  cout << "Nodes: " << nodes << endl;
  cout << "Point elements: " << elements_zero_d << endl;
  cout << "Edge elements: " << elements_one_d << endl;
  cout << "Surface elements: " << elements_two_d << endl;
  cout << "Volume elements: " << elements_three_d << endl;
  cout.flush();

  // Set mesh dimension:
  this->dim = 0;

  if(elements_one_d > 0)
    this->dim = 1;

  if(elements_two_d > 0)
    this->dim = 2;

  if(elements_three_d > 0)
    this->dim = 3;

  this->nodes = nodes;
  node = new node_t[nodes];

  this->points = elements_zero_d;
  point = new point_t[this->points];

  this->edges = elements_one_d;
  edge = new edge_t[this->edges];

  this->surfaces = elements_two_d;
  surface = new surface_t[this->surfaces];

  this->elements = elements_three_d;
  element = new element_t[this->elements];

  mesh_header.close();

  // Nodes:
  //-------
  sprintf(fileName, "%s/mesh.nodes", dirName);
  mesh_nodes.open(fileName);

  if(!mesh_nodes.is_open()) {
    cout << "Mesh: load: unable to open " << fileName << endl;
    return false;
  }

  int number, index;
  double x, y, z;

  for(int i = 0; i < nodes; i++) {
    node_t *node = &this->node[i];
    mesh_nodes >> number >> index >> x >> y >> z;
    node->x[0] = x;
    node->x[1] = y;
    node->x[2] = z;
    node->index = index;
  }

  mesh_nodes.close();  

  // Elements:
  //----------
  sprintf(fileName, "%s/mesh.elements", dirName);
  mesh_elements.open(fileName);

  if(!mesh_elements.is_open()) {
    cout << "Mesh: load: unable to open " << fileName << endl;
    return false;
  }

  int current_point = 0;
  int current_edge = 0;
  int current_surface = 0;
  int current_element = 0;

  point_t *point = NULL;
  edge_t *edge = NULL;
  surface_t *surface = NULL;
  element_t *element = NULL;

  for(int i = 0; i < elements; i++) {
    mesh_elements >> number >> index >> type;

    switch(type/100) {
    case 1:
      point = &this->point[current_point++];
      point->nature = PDE_BULK;
      point->index = index;
      point->code = type;
      point->nodes = point->code % 100;
      point->node = new int[point->nodes];
      for(int j = 0; j < point->nodes; j++) {
	mesh_elements >> point->node[j];
	point->node[j] -= 1;
      }
      point->edges = 2;
      point->edge = new int[point->edges];
      point->edge[0] = -1;
      point->edge[1] = -1;
      break;

    case 2:
      edge = &this->edge[current_edge++];
      edge->nature = PDE_BULK;
      edge->index = index;
      edge->code = type;
      edge->nodes = edge->code % 100;
      edge->node = new int[edge->nodes];
      for(int j = 0; j < edge->nodes; j++) {
	mesh_elements >> edge->node[j];
	edge->node[j] -= 1;
      }
      edge->surfaces = 0;
      edge->surface = new int[edge->surfaces];
      edge->surface[0] = -1;
      edge->surface[1] = -1;

      break;

    case 3:
    case 4:
      surface = &this->surface[current_surface++];
      surface->nature = PDE_BULK;
      surface->index = index;
      surface->code = type;
      surface->nodes = surface->code % 100;
      surface->node = new int[surface->nodes];
      for(int j = 0; j < surface->nodes; j++) {
	mesh_elements >> surface->node[j];
	surface->node[j] -= 1;
      }      
      surface->edges = (int)(surface->code/100);
      surface->edge = new int[surface->edges];
      for(int j = 0; j < surface->edges; j++)
	surface->edge[j] = -1;
      surface->elements = 2;
      surface->element = new int[surface->elements];
      surface->element[0] = -1;
      surface->element[1] = -1;

      break;

    case 5:
    case 6:
    case 7:
    case 8:
      element = &this->element[current_element++];
      element->nature = PDE_BULK;
      element->index = index;
      element->code = type;
      element->nodes = element->code % 100;
      element->node = new int[element->nodes];
      for(int j = 0; j < element->nodes; j++) {
	mesh_elements >> element->node[j];
	element->node[j] -= 1;
      }
      break;

    default:
      cout << "Unknown element type (possibly not implemented" << endl;
      cout.flush();
      return false;
      // exit(0);
    }
  }

  mesh_elements.close();

  // Boundary elements:
  //-------------------
  sprintf(fileName, "%s/mesh.boundary", dirName);
  mesh_boundary.open(fileName);

  if(!mesh_boundary.is_open()) {
    cout << "Mesh: load: unable to open " << fileName << endl;
    return false;
  }

  int parent0, parent1;

  for(int i = 0; i < boundaryelements; i++) {
    mesh_boundary >> number >> index >> parent0 >> parent1 >> type;

    switch(type/100) {
    case 1:
      point = &this->point[current_point++];
      point->nature = PDE_BOUNDARY;
      point->index = index;
      point->edges = 2;
      point->edge = new int[point->edges];
      point->edge[0] = parent0 - 1;
      point->edge[1] = parent0 - 1;
      point->code = type;
      point->nodes = point->code % 100;
      point->node = new int[point->nodes];
      for(int j = 0; j < point->nodes; j++) {
	mesh_elements >> point->node[j];
	point->node[j] -= 1;
      }
      break;

    case 2:
      edge = &this->edge[current_edge++];
      edge->nature = PDE_BOUNDARY;
      edge->index = index;
      edge->surfaces = 2;
      edge->surface = new int[edge->surfaces];
      edge->surface[0] = parent0 - 1;
      edge->surface[1] = parent1 - 1;
      edge->code = type;
      edge->nodes = edge->code % 100;
      edge->node = new int[edge->nodes];      
      for(int j = 0; j < edge->nodes; j++) {
	mesh_boundary >> edge->node[j];
	edge->node[j] -= 1;
      }

      break;

    case 3:
    case 4:
      surface = &this->surface[current_surface++];
      surface->nature = PDE_BOUNDARY;
      surface->index = index;
      surface->elements = 2;
      surface->element = new int[surface->elements];
      surface->element[0] = parent0 - 1;
      surface->element[1] = parent1 - 1;
      surface->code = type;
      surface->nodes = surface->code % 100;
      surface->node = new int[surface->nodes];
      for(int j = 0; j < surface->nodes; j++) {
	mesh_boundary >> surface->node[j];
	surface->node[j] -= 1;
      }
      surface->edges = (int)(surface->code/100);
      surface->edge = new int[surface->edges];
      for(int j = 0; j < surface->edges; j++)
	surface->edge[j] = -1;      
      
      break;

    case 5:
    case 6:
    case 7:
    case 8:
      // these can't be boundary elements
      break;

    default:
      break;
    }
  }

  mesh_boundary.close();

  this->boundingBox();

  return true;
}

// Save Elmer mesh files and populate mesh structures
//---------------------------------------------------------------------------
bool mesh_t::save(char *dirName)
{
  char fileName[1024];
  ofstream mesh_header;
  ofstream mesh_nodes;
  ofstream mesh_elements;
  ofstream mesh_boundary;

  // Elmer's elements codes are smaller than 1000
  int maxcode = 1000;
  int *bulk_by_type = new int[maxcode];
  int *boundary_by_type = new int[maxcode];
  
  for(int i = 0; i < maxcode; i++) {
    bulk_by_type[i] = 0;
    boundary_by_type[i] = 0;
  }
  
  for(int i = 0; i < elements; i++) {
    element_t *e = &element[i];
      
    if(e->nature == PDE_BULK) 
      bulk_by_type[e->code]++;
    
    if(e->nature == PDE_BOUNDARY)
      boundary_by_type[e->code]++;
  }
	
  for(int i = 0; i < surfaces; i++) {
    surface_t *s = &surface[i];

    if(s->nature == PDE_BULK)
      bulk_by_type[s->code]++;

    if(s->nature == PDE_BOUNDARY)
      boundary_by_type[s->code]++;
  }

  for(int i = 0; i < edges; i++) {
    edge_t *e = &edge[i];

    if(e->nature == PDE_BULK)
      bulk_by_type[e->code]++;

    if(e->nature == PDE_BOUNDARY)
      boundary_by_type[e->code]++;
  }

  for(int i = 0; i < points; i++) {
    point_t *p = &point[i];

    if(p->nature == PDE_BULK)
      bulk_by_type[p->code]++;

    if(p->nature == PDE_BOUNDARY)
      boundary_by_type[p->code]++;
  }

  int bulk_elements = 0;
  int boundary_elements = 0;
  int element_types = 0;
  
  for(int i = 0; i < maxcode; i++) {
    bulk_elements += bulk_by_type[i];
    boundary_elements += boundary_by_type[i];
    
    if((bulk_by_type[i] > 0) || (boundary_by_type[i] > 0))
      element_types++;
  }
  
  // Header:
  //---------
  sprintf(fileName, "%s/mesh.header", dirName);
  
  mesh_header.open(fileName);
  
  if(!mesh_header.is_open()) {
    cout << "Unable to open " << fileName << endl;
    return false;
  }
  
  cout << "Saving " << nodes << " nodes" << endl;
  cout << "Saving " << bulk_elements << " elements" << endl;
  cout << "Saving " << boundary_elements << " boundary elements" << endl;
  cout.flush();
  
  mesh_header << nodes << " ";
  mesh_header << bulk_elements << " ";
  mesh_header << boundary_elements << endl;
  mesh_header << element_types << endl;
  
  for(int i = 0; i < maxcode; i++) {
    int j = bulk_by_type[i] + boundary_by_type[i];
    if(j > 0) 
      mesh_header << i << " " << j << endl;
  }
  
  mesh_header.close();

  // Nodes:
  //--------
  sprintf(fileName, "%s/mesh.nodes", dirName);

  mesh_nodes.open(fileName);
  
  if(!mesh_nodes.is_open()) {
    cout << "Unable to open " << fileName << endl;
    return false;
  }

  for(int i = 0; i < this->nodes; i++) {
    node_t *node = &this->node[i];

    int ind = node->index;

    mesh_nodes << i+1 << " " << ind << " ";
    mesh_nodes << node->x[0] << " ";
    mesh_nodes << node->x[1] << " ";
    mesh_nodes << node->x[2] << endl;
  }

  mesh_nodes.close();

  
  // Elements:
  //----------
  sprintf(fileName, "%s/mesh.elements", dirName);

  mesh_elements.open(fileName);

  if(!mesh_elements.is_open()) {
    cout << "Unable to open " << fileName << endl;
    return false;
  }

  int current = 0;

  for(int i = 0; i < this->elements; i++) {
    element_t *e = &this->element[i];

    int ind = e->index;

    if(ind < 1)
      ind = 1;

    if(e->nature == PDE_BULK) {
      mesh_elements << ++current << " ";
      mesh_elements << ind << " ";
      mesh_elements << e->code << " ";

      for(int j = 0; j < e->nodes; j++) 
	mesh_elements << e->node[j] + 1 << " ";

      mesh_elements << endl;
    }
  }

  for(int i = 0; i < this->surfaces; i++) {
    surface_t *s = &this->surface[i];

    int ind = s->index;

    if(ind < 1)
      ind = 1;

    if(s->nature == PDE_BULK) {
      mesh_elements << ++current << " ";
      mesh_elements << ind << " ";
      mesh_elements << s->code << " ";

      for(int j = 0; j < s->nodes; j++) 
	mesh_elements << s->node[j]+1 << " ";

      mesh_elements << endl;
    }
  }

  for(int i = 0; i < this->edges; i++) {
    edge_t *e = &this->edge[i];

    int ind = e->index;

    if(ind < 1)
      ind = 1;

    if(e->nature == PDE_BULK) {
      mesh_elements << ++current << " ";
      mesh_elements << ind << " ";
      mesh_elements << e->code << " ";

      for(int j = 0; j < e->nodes; j++)
	mesh_elements << e->node[j]+1 << " ";

      mesh_elements << endl;
    }
  }

  for(int i = 0; i < this->points; i++) {
    point_t *p = &this->point[i];

    int ind = p->index;

    if(ind < 1)
      ind = 1;

    if(p->nature == PDE_BULK) {
      mesh_elements << ++current << " ";
      mesh_elements << ind << " ";
      mesh_elements << p->code << " ";

      for(int j = 0; j < p->nodes; j++)
	mesh_elements << p->node[j]+1 << " ";

      mesh_elements << endl;
    }
  }

  mesh_elements.close();

  // Boundary elements:
  //-------------------
  sprintf(fileName, "%s/mesh.boundary", dirName);

  mesh_boundary.open(fileName);

  if(!mesh_boundary.is_open()) {
    cout << "Unable to open " << fileName << endl;
    return false;
  }

  current = 0;

  for(int i = 0; i < this->surfaces; i++) {
    surface_t *s = &this->surface[i];

    int e0 = s->element[0] + 1;
    int e1 = s->element[1] + 1;

    if(e0 < 0)
      e0 = 0;

    if(e1 < 0)
      e1 = 0;

    int ind = s->index;

    if(ind < 1)
      ind = 1;

    if(s->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << ind << " ";
      mesh_boundary << e0 << " " << e1 << " ";
      mesh_boundary << s->code << " ";

      for(int j = 0; j < s->nodes; j++) 
	mesh_boundary << s->node[j]+1 << " ";

      mesh_boundary << endl;
    }
  }


  for(int i = 0; i < this->edges; i++) {
    edge_t *e = &this->edge[i];

    int s0 = e->surface[0] + 1;
    int s1 = e->surface[1] + 1;

    if(s0 < 0)
      s0 = 0;

    if(s1 < 0)
      s1 = 0;

    int ind = e->index;

    if(ind < 1)
      ind = 1;

    if(e->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << ind << " ";
      mesh_boundary << s0 << " " << s1 << " ";
      mesh_boundary << e->code << " ";

      for(int j = 0; j < e->nodes; j++) 
	mesh_boundary << e->node[j]+1 << " ";

      mesh_boundary << endl;
    }
  }

  for(int i = 0; i < this->points; i++) {
    point_t *p = &this->point[i];

    int e0 = p->edge[0] + 1;
    int e1 = p->edge[1] + 1;

    if(e0 < 0)
      e0 = 0;

    if(e1 < 0)
      e1 = 0;

    int ind = p->index;

    if(ind < 1)
      ind = 1;

    if(p->nature == PDE_BOUNDARY) {
      mesh_boundary << ++current << " ";
      mesh_boundary << ind << " ";
      mesh_boundary << e0 << " " << e1 << " ";
      mesh_boundary << p->code << " ";

      for(int j = 0; j < p->nodes; j++) 
	mesh_boundary << p->node[j]+1 << " ";

      mesh_boundary << endl;
    }
  }

  mesh_boundary.close();

  delete [] bulk_by_type;
  delete [] boundary_by_type;

  return true;
}

// Bounding box...
//-----------------------------------------------------------------------------
double* mesh_t::boundingBox()
{
  double *result = new double[10];

  double xmin = +9e9;
  double xmax = -9e9;

  double ymin = +9e9;
  double ymax = -9e9;

  double zmin = +9e9;
  double zmax = -9e9;

  for(int i=0; i < this->nodes; i++) {
    node_t *node = &this->node[i];
    
    if(node->x[0] > xmax) 
      xmax = node->x[0];
    
    if(node->x[0] < xmin) 
      xmin = node->x[0];
    
    if(node->x[1] > ymax) 
      ymax = node->x[1];

    if(node->x[1] < ymin) 
      ymin = node->x[1];

    if(node->x[2] > zmax) 
      zmax = node->x[2];

    if(node->x[2] < zmin) 
      zmin = node->x[2];
  }
  
  double xmid = (xmin + xmax)/2.0;
  double ymid = (ymin + ymax)/2.0;
  double zmid = (zmin + zmax)/2.0;

  double xlen = (xmax - xmin)/2.0;
  double ylen = (ymax - ymin)/2.0;
  double zlen = (zmax - zmin)/2.0;

  double s = xlen;

  if(ylen > s)
    s = ylen;
  
  if(zlen > s)
    s = zlen;
  
  s *= 1.1;

  bool sx = xmin==xmax;
  bool sy = ymin==ymax;
  bool sz = zmin==zmax;

  this->cdim = 3;

  if((sz && sy) || (sz && sx) || (sx && sy))
    this->cdim = 1;

  else if(sz || sy || sx)
    this->cdim = 2;

  result[0] = xmin;
  result[1] = xmax;
  result[2] = ymin;
  result[3] = ymax;
  result[4] = zmin;
  result[5] = zmax;

  result[6] = xmid;
  result[7] = ymid;
  result[8] = zmid;

  result[9] = s;

  return result;
}
