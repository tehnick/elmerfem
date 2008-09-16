#ifndef SIFGENERATOR_H
#define SIFGENERATOR_H

#include <QTextEdit>
#include <QHash>
#include <QScriptEngine>

#include "meshtype.h"
#include "maxlimits.h"
#include "generalsetup.h"
#include "boundarypropertyeditor.h"
#include "bodypropertyeditor.h"
#include "solverparameters.h"
#include "meshcontrol.h"
#include "dynamiceditor.h"

enum EquationTypes {
  HEAT_EQUATION,
  LINEAR_ELASTICITY,
  NAVIER_STOKES,
  ADVECTION_DIFFUSION,
  HELMHOLTZ_EQUATION 
};

class SifGenerator  {
 public:
  SifGenerator();
  ~SifGenerator();

  mesh_t *mesh;
  QTextEdit *te;

  QDomDocument *elmerDefs;
  GeneralSetup *generalSetup;
  DynamicEditor *equationEditor;
  DynamicEditor *materialEditor;
  DynamicEditor *bodyForceEditor;
  DynamicEditor *initialConditionEditor;
  DynamicEditor *boundaryConditionEditor;
  
  SolverParameterEditor *solverParameterEditor;

  BoundaryPropertyEditor *boundaryPropertyEditor;
  BodyPropertyEditor *bodyPropertyEditor;

  MeshControl *meshControl;
  int dim,cdim;

  void makeHeaderBlock();
  void makeSimulationBlock();
  void makeConstantsBlock();
  void makeBodyBlocks();
  void makeEquationBlocks();
  void makeSolverBlocks(QString);
  void makeMaterialBlocks();
  void makeBodyForceBlocks();
  void makeInitialConditionBlocks();
  void makeBoundaryBlocks();
  
  QHash<int, int> bodyMap;
  QHash<int, int> boundaryMap;

 private:
  void parseSolverSpecificTab(DynamicEditor *, QString);
  void parseGeneralTab(Ui::solverParameterEditor);
  void parseSteadyStateTab(Ui::solverParameterEditor);
  void parseNonlinearSystemTab(Ui::solverParameterEditor);
  void parseLinearSystemTab(Ui::solverParameterEditor);

  void addSifLine(const QString&, const QString&);
  void addSifLineBool(const QString&, bool);

  void handleBCLineEdit(QDomElement, QWidget*,int *);
  void handleLineEdit(QDomElement, QWidget*);
  void handleComboBox(QDomElement, QWidget*);
  void handleCheckBox(QDomElement, QWidget*);
};

#endif // SIFGENERATOR_H
