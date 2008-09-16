#ifndef MAXLIMITS_H
#define MAXLIMITS_H

#define MAX_EQUATIONS           10 // equations
#define MAX_SOLVERS             10 // solvers/equation
#define MAX_MATERIALS           10 // materials
#define MAX_BODYFORCES          10 // body forces
#define MAX_INITIALCONDITIONS   10 // initial conditions
#define MAX_BODIES             100 // bodies (as in input file & sif)
#define MAX_BOUNDARIES         500 // boundaries (as in input file)
#define MAX_BCS                500 // boundary conditions (as in sif)

class Limit {
 public:
  Limit();
  ~Limit();

  int maxEquations;
  int maxMaterials;
  int maxBodyforces;
  int maxInitialconditions;
  int maxBcs;
  int maxBodies;
  int maxBoundaries;
  int maxSolvers;
};

#endif // MAXLIMITS_H
