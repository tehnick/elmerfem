#include "maxlimits.h"

Limit::Limit()
{
  // Default limits:
  max_solvers = 10;
  max_equations = 10;
  max_materials = 10;
  max_bodyforces = 10;
  max_initialconditions = 10;
  max_bcs = 500;
  max_bodies = 100;
  max_boundaries = 500;
}

Limit::~Limit()
{
}

int Limit::maxEquations()
{
  return max_equations;
}

int Limit::maxMaterials()
{
  return max_materials;
}

int Limit::maxBodyforces()
{
  return max_bodyforces;
}

int Limit::maxInitialconditions()
{
  return max_initialconditions;
}

int Limit::maxBcs()
{
  return max_bcs;
}

int Limit::maxBodies()
{
  return max_bodies;
}

int Limit::maxBoundaries()
{
  return max_boundaries;
}

int Limit::maxSolvers()
{
  return max_solvers;
}

void Limit::setMaxEquations(int value)
{
  max_equations = value;
}

void Limit::setMaxMaterials(int value)
{
  max_materials = value;
}

void Limit::setMaxBodyforces(int value)
{
  max_bodyforces = value;
}

void Limit::setMaxInitialconditions(int value)
{
  max_initialconditions = value;
}

void Limit::setMaxBcs(int value)
{
  max_bcs = value;
}

void Limit::setMaxBodies(int value)
{
  max_bodies = value;
}

void Limit::setMaxBoundaries(int value)
{
  max_boundaries = value;
}

void Limit::setMaxSolvers(int value)
{
  max_solvers = value;
}
