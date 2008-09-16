#ifndef MAXLIMITS_H
#define MAXLIMITS_H

class Limit {
 public:
  Limit();
  ~Limit();

  int maxEquations();
  int maxMaterials();
  int maxBodyforces();
  int maxInitialconditions();
  int maxBcs();
  int maxBodies();
  int maxBoundaries();
  int maxSolvers();

  void setMaxEquations(int);
  void setMaxMaterials(int);
  void setMaxBodyforces(int);
  void setMaxInitialconditions(int);
  void setMaxBcs(int);
  void setMaxBodies(int);
  void setMaxBoundaries(int);
  void setMaxSolvers(int);

 private:
  int max_equations;
  int max_materials;
  int max_bodyforces;
  int max_initialconditions;
  int max_bcs;
  int max_bodies;
  int max_boundaries;
  int max_solvers;
};

#endif // MAXLIMITS_H
