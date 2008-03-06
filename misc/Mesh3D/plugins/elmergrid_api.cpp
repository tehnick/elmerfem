#include <iostream>
#include "elmergrid_api.h"

using namespace std;

ElmergridAPI::ElmergridAPI()
{
  cout << "Constructing ElmergridAPI" << endl;
  cout.flush();
}


ElmergridAPI::~ElmergridAPI()
{
  cout << "Destructing ElmergridAPI" << endl;
  cout.flush();
}


mesh_t* ElmergridAPI::createElmerMeshStructure()
{
  mesh_t *mesh = new mesh_t;

  mesh->nodes = 1223;
  mesh->node = new node_t[mesh->nodes];
  
  cout << "ok, palautan nyt uuden verkon" << endl;
  cout.flush();

  return mesh;
}
