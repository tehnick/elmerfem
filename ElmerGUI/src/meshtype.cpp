#include "meshtype.h"

mesh_t::mesh_t()
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
