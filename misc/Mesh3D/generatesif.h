#ifndef GENERATESIF_H
#define GENERATESIF_H

#include <QTextEdit>

#include "pdepropertyeditor.h"
#include "solverparameters.h"
#include "meshcontrol.h"
#include "ui_pdepropertyeditor.h"
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

  QTextEdit *te;
  PDEPropertyEditor *pe;
  MeshControl *meshControl;
  int cdim;

  void makeHeaderBlock();
  void makeSimulationBlock();
  void makeConstantsBlock();
  void makeEquationBlocks();
  void makeSolverBlocks();
  void makeMaterialBlocks();
  void makeBodyForceBlocks();

 private:
  void parseProcedure(Ui::solverParameterEditor, QTextEdit*);
  void parseGeneralTab(Ui::solverParameterEditor, QTextEdit*);
  void parseSteadyStateTab(Ui::solverParameterEditor, QTextEdit*);
  void parseNonlinearSystemTab(Ui::solverParameterEditor, QTextEdit*);
  void parseLinearSystemTab(Ui::solverParameterEditor, QTextEdit*);
};

#endif // GENERATESIF_H
