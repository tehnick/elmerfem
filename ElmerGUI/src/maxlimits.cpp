#include "maxlimits.h"

Limit::Limit()
{
  maxEquations = MAX_EQUATIONS;
  maxMaterials = MAX_MATERIALS;
  maxBodyforces = MAX_BODYFORCES;
  maxInitialconditions = MAX_INITIALCONDITIONS;
  maxBcs = MAX_BCS;
  maxBodies = MAX_BODIES;
  maxBoundaries = MAX_BOUNDARIES;
  maxSolvers = MAX_SOLVERS;
}

Limit::~Limit()
{
}
