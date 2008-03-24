#ifndef GENERATESIF_H
#define GENERATESIF_H

#include <QTextEdit>

#include "meshtype.h"
#include "pdepropertyeditor.h"
#include "bcpropertyeditor.h"
#include "solverparameters.h"
#include "meshcontrol.h"
#include "ui_pdepropertyeditor.h"
#include "ui_bcpropertyeditor.h"
#include "ui_solverparameters.h"
#include "ui_meshcontrol.h"

#define HEAT_EQUATION       0
#define LINEAR_ELASTICITY   1
#define NAVIER_STOKES       2
#define ADVECTION_DIFFUSION 3
#define HELMHOLTZ_EQUATION  4

class GenerateSif  {
 public:
  GenerateSif();
  ~GenerateSif();

  mesh_t *mesh;
  QTextEdit *te;
  PDEPropertyEditor *pe;
  BCPropertyEditor *bcPropertyEditor;
  MeshControl *meshControl;
  int cdim;

  void makeHeaderBlock();
  void makeSimulationBlock();
  void makeConstantsBlock();
  void makeBodyBlocks();
  void makeEquationBlocks();
  void makeSolverBlocks();
  void makeMaterialBlocks();
  void makeBodyForceBlocks();
  void makeBoundaryBlocks();

 private:
  void parseProcedure(Ui::solverParameterEditor);
  void parseGeneralTab(Ui::solverParameterEditor);
  void parseSteadyStateTab(Ui::solverParameterEditor);
  void parseNonlinearSystemTab(Ui::solverParameterEditor);
  void parseLinearSystemTab(Ui::solverParameterEditor);
};

#endif // GENERATESIF_H
