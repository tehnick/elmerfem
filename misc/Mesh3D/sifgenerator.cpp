#include "sifgenerator.h"
#include <iostream>

using namespace std;

SifGenerator::SifGenerator()
{
}

SifGenerator::~SifGenerator()
{
}

// Make Header-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeHeaderBlock()
{
  Ui::setupDialog ui = generalSetup->ui;

  te->append("Header");
  
  if(ui.checkKeywordsWarn->isChecked())
    te->append("  CHECK KEYWORDS Warn");

  const QString &qs1 = ui.meshDBEdit1->text().trimmed(); 
  const QString &qs2 = ui.meshDBEdit2->text().trimmed(); 
  const QString &qs3 = ui.includePathEdit->text().trimmed(); 
  const QString &qs4 = ui.resultsDirectoryEdit->text().trimmed(); 
  
  te->append("  Mesh DB \"" +  qs1 + "\" \"" + qs2 + "\"");
  te->append("  Include Path \"" + qs3 + "\"");
  te->append("  Results Directory \"" + qs4 + "\"");

  te->append("End\n");
}

// Make Simulation-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeSimulationBlock()
{
  Ui::setupDialog ui = generalSetup->ui;

  te->append("Simulation");

  addSifLine("  Max Output Level = ", 
	     ui.maxOutputLevelCombo->currentText().trimmed());
  addSifLine("  Coordinate System = ",
	     ui.coordinateSystemCombo->currentText().trimmed());
  addSifLine("  Coordinate Mapping(3) = ",
	     ui.coordinateMappingEdit->text().trimmed());
  addSifLine("  Simulation Type = ", 
	     ui.simulationTypeCombo->currentText().trimmed());
  addSifLine("  Steady State Max Iterations = ",
	     ui.steadyStateMaxIterEdit->text().trimmed());
  addSifLine("  Output Intervals = ",
	     ui.outputIntervalsEdit->text().trimmed());
  addSifLine("  Timestepping Method = ",
	     ui.timesteppingMethodCombo->currentText().trimmed());
  addSifLine("  BDF Order = ",
	     ui.bdfOrderCombo->currentText().trimmed());
  addSifLine("  Timestepping intervals = ",
	     ui.timeStepIntervalsEdit->text().trimmed());
  addSifLine("  Timestep Sizes = ",
	     ui.timestepSizesEdit->text().trimmed());
  addSifLine("  Solver Input File = ", 
	     ui.solverInputFileEdit->text().trimmed());
  addSifLine("  Post File = ", 
	     ui.postFileEdit->text().trimmed());

  te->append("End\n");
}

// Make Constants-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeConstantsBlock()
{
  Ui::setupDialog ui = generalSetup->ui;

  te->append("Constants");
  
  addSifLine("  Gravity(4) = ",
	     ui.gravityEdit->text().trimmed());
  addSifLine("  Stefan Boltzmann = ",
	     ui.stefanBoltzmannEdit->text().trimmed());
  addSifLine("  Permittivity of Vacuum = ",
	     ui.vacuumPermittivityEdit->text().trimmed());
  
  te->append("End\n");
}


// Make Body-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyBlocks()
{
  int i;

  int sifIndex = 0;

  for(int index = 0; index < bodyMap.count(); index++) {
    BodyPropertyEditor *bodyEdit = &bodyPropertyEditor[index];
    
    if(bodyEdit->touched) {
      te->append("Body " + QString::number(++sifIndex));

      int originalIndex = bodyMap.key(index);

      te->append("  Target Bodies(1) = " + QString::number(originalIndex));

      te->append("  Name = " + bodyEdit->ui.nameEdit->text().trimmed());

      i = bodyEdit->ui.equationCombo->currentIndex();
      if(i > -1)
	te->append("  Equation = " + QString::number(i+1));
      
      i = bodyEdit->ui.materialCombo->currentIndex();
      if(i > -1)
	te->append("  Material = " + QString::number(i+1));
      
      i = bodyEdit->ui.bodyForceCombo->currentIndex();

      if(i > -1)
	te->append("  Body Force = " + QString::number(i+1));
      
      i = bodyEdit->ui.initialConditionCombo->currentIndex();
      if(i > -1)
	te->append("  Initial condition = " + QString::number(i+1));
      
      te->append("End\n");      
    }
  }
}

// Make Equation-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeEquationBlocks()
{
  int sifIndex = 0;
  int sifSolver = 0;

  for(int index = 0; index < MAX_EQUATIONS; index++) {
    DynamicEditor *eqEditor = &equationEditor[index];
    
    if(eqEditor->menuAction != NULL) {      
      te->append("Equation " + QString::number(++sifIndex));
      
      QString name = eqEditor->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);

      QString solverString = "";
      int nofSolvers = 0;

      for(int i = 0; i < eqEditor->hash.count(); i++) {
	hash_entry_t entry = eqEditor->hash.values().at(i); 
	
	QWidget *widget = entry.widget;

        if ( widget->isEnabled() ) {
          QDomElement elem = entry.elem;
	  
	  // solver active?
	  QString key = eqEditor->hash.keys().at(i);
	  QStringList keyList = key.split("/");
	  
	  if((keyList.at(3).trimmed() == "Active") &&
	     (elem.attribute("Widget", "") == "CheckBox")) {
	    QCheckBox *checkBox = (QCheckBox*)widget;
	    if(checkBox->isChecked()) {
	      nofSolvers++; // for this eq.
	      sifSolver++;  // all solvers
	      solverString += " " + QString::number(sifSolver);
	    }
	  }

          if((elem.attribute("Widget", "") == "CheckBox") &&
	     (keyList.at(3).trimmed() != "Active")) 
	    handleCheckBox(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Edit")
	    handleLineEdit(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Combo")
	    handleComboBox(elem, widget);
        }
      }

      if(nofSolvers > 0)
	te->append("  Active Solvers(" 
		   + QString::number(nofSolvers) 
		   + ") =" + solverString);
      
      te->append("End\n");
    }
  }
}

// Make Solver-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeSolverBlocks()
{
#if 0
  PDEPropertyEditor *p = &pdePropertyEditor[0];
  
  int currentSolver = 0;
  
  // user interface of the equation property editor:
  Ui::equationEditor ui = p->ui;
  
  if(ui.heatEquationActive->isChecked()) {
    currentSolver++;
    // user interface of the solver parameter editor:
    Ui::solverParameterEditor ui = p->solverParameterEditor[HEAT_EQUATION].ui;
    te->append("Solver " + QString::number(currentSolver));
    te->append("  Equation = \"Heat Equation\"");
    te->append("  Variable = Temperature");
    te->append("  Variable Dofs = 1");
    parseProcedure(ui);
    parseGeneralTab(ui);
    parseSteadyStateTab(ui);
    parseNonlinearSystemTab(ui);
    parseLinearSystemTab(ui);
    // todo: add adaptivity & multigrid
    te->append("End\n");
  }
  
  if(ui.linearElasticityActive->isChecked()) {
    currentSolver++;
    // user interface of the solver parameter editor:
    Ui::solverParameterEditor ui = p->solverParameterEditor[LINEAR_ELASTICITY].ui;
    te->append("Solver " + QString::number(currentSolver));
    te->append("  Equation = \"Stress analysis\"");
    te->append("  Variable = Displacement");
    te->append("  Variable dofs = " + QString::number(cdim));
    parseProcedure(ui);
    parseGeneralTab(ui);
    parseSteadyStateTab(ui);
    parseNonlinearSystemTab(ui);
    parseLinearSystemTab(ui);
    // todo: add adaptivity & multigrid
    te->append("End\n");
  }
  
  if(ui.navierStokesActive->isChecked()) {
    currentSolver++;
    // user interface of the solver parameter editor:
    Ui::solverParameterEditor ui = p->solverParameterEditor[NAVIER_STOKES].ui;
    te->append("Solver " + QString::number(currentSolver));
    te->append("  Equation = \"Navier-Stokes\"");
    te->append("  Variable = Flow Solution");
    te->append("  Variable dofs = " + QString::number(cdim+1));
    parseProcedure(ui);
    parseGeneralTab(ui);
    parseSteadyStateTab(ui);
    parseNonlinearSystemTab(ui);
    parseLinearSystemTab(ui);
    // todo: add adaptivity & multigrid
    te->append("End\n");
  }
#endif
}


// Make Material-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeMaterialBlocks()
{
  int sifIndex = 0;

  for(int index = 0; index < MAX_MATERIALS; index++) {
    DynamicEditor *matEditor = &materialEditor[index];
    
    if(matEditor->menuAction != NULL) {      
      te->append("Material " + QString::number(++sifIndex));
      
      QString name = matEditor->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);
      
      for(int i = 0; i < matEditor->hash.count(); i++) {
	hash_entry_t entry = matEditor->hash.values().at(i); 
	
	QWidget *widget = entry.widget;

	QDomElement elem;
        if ( widget->isEnabled() ) {
          elem = entry.elem;
	  
          if(elem.attribute("Widget", "") == "CheckBox") 
	   handleCheckBox(elem, widget);
	
	 if(elem.attribute("Widget", "") == "Edit")
	   handleLineEdit(elem, widget);
	
	 if(elem.attribute("Widget", "") == "Combo")
	   handleComboBox(elem, widget);
        }
      }
      te->append("End\n");
    }
  }
}



// Make body force blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyForceBlocks()
{
  int sifIndex = 0;

  for(int index = 0; index < MAX_BODYFORCES; index++) {
    DynamicEditor *bfEdit = &bodyForceEditor[index];
    
    if(bfEdit->menuAction != NULL) { 
      te->append("Body Force " + QString::number(++sifIndex));
      
      QString name = bfEdit->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);
      
      for(int i = 0; i < bfEdit->hash.count(); i++) {
	hash_entry_t entry = bfEdit->hash.values().at(i); 
	
	QWidget *widget = entry.widget;

        if ( widget->isEnabled() ) {
          QDomElement elem = entry.elem;
	  
          if(elem.attribute("Widget", "") == "CheckBox") 
	    handleCheckBox(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Edit")
	    handleLineEdit(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Combo")
	    handleComboBox(elem, widget);
        }
      }
      te->append("End\n");
    }
  }
}


// Make initial condition blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeInitialConditionBlocks()
{
  int sifIndex = 0;
  
  for(int index = 0; index < MAX_INITIALCONDITIONS; index++) {
    DynamicEditor *icEdit = &initialConditionEditor[index];
    
    if(icEdit->menuAction != NULL) { 
      te->append("Initial Condition " + QString::number(++sifIndex));
      
      QString name = icEdit->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);
      
      for(int i = 0; i < icEdit->hash.count(); i++) {
	hash_entry_t entry = icEdit->hash.values().at(i); 
	
	QWidget *widget = entry.widget;
	
        if ( widget->isEnabled() ) {
          QDomElement elem = entry.elem;
	  
          if(elem.attribute("Widget", "") == "CheckBox") 
	    handleCheckBox(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Edit")
	    handleLineEdit(elem, widget);
	  
	  if(elem.attribute("Widget", "") == "Combo")
	    handleComboBox(elem, widget);
        }
      }
      te->append("End\n");
    }
  }
}






// Make boundary blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBoundaryBlocks()
{
  int sifIndex = 0;
  for(int index = 0; index < boundaryMap.count(); index++) {
    BoundaryPropertyEditor *bEdit = &boundaryPropertyEditor[index];

    if(bEdit->touched) {
      te->append("Boundary Condition " + QString::number(++sifIndex));

      int originalIndex = boundaryMap.key(index);

      te->append("  Target Boundaries(1) = " + QString::number(originalIndex));

      int i = bEdit->ui.boundaryConditionCombo->currentIndex();
      const QString &name = bEdit->ui.boundaryConditionCombo->currentText().trimmed();
      if(i > -1) 
	te->append("  Name = " +  name);

      // check which one of the dynamic editors has "name" typed in nameEdit:
      for(int j = 0; j < MAX_BCS; j++) {
	DynamicEditor *bc = &boundaryConditionEditor[j];
	if(bc->menuAction != NULL) {
	  if(bc->nameEdit->text().trimmed() == name) {
	    
	    // go through the hash of this dynamic editor:
	    //--------------------------------------------
	    for(int i = 0; i < bc->hash.count(); i++) {
	      hash_entry_t entry = bc->hash.values().at(i); 
	      
	      QWidget *widget = entry.widget;
	      
	      QDomElement elem;
	      if ( widget->isEnabled() ) {
		elem = entry.elem;
		
		if(elem.attribute("Widget", "") == "CheckBox") 
		  handleCheckBox(elem, widget);
		
		if(elem.attribute("Widget", "") == "Edit")
		  handleLineEdit(elem, widget);
		
		if(elem.attribute("Widget", "") == "Combo")
		  handleComboBox(elem, widget);
	      }
	    }
	  }
	}
      }      
      te->append("End\n");      
    }
  }
}


// Parse "Procedure fields" from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseProcedure(Ui::solverParameterEditor ui)
{
  if((ui.procedureFileEdit->text() == "") ||
     (ui.procedureFunctionEdit->text() == ""))
    return;

  te->append("  Procedure = \"" + ui.procedureFileEdit->text() + "\" "
	                 + "\"" + ui.procedureFunctionEdit->text() + "\"");
}



// Parse "Exec Solver" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseGeneralTab(Ui::solverParameterEditor ui)
{
  if(ui.execAlways->isChecked())
    te->append("  Exec Solver = Always");
  
  if(ui.execBeforeSimulation->isChecked())
    te->append("  Exec Solver = Before Simulation");
  
  if(ui.execAfterSimulation->isChecked())
    te->append("  Exec Solver = After Simulation");
  
  if(ui.execBeforeTimestep->isChecked())
    te->append("  Exec Solver = Before Timestep");
  
  if(ui.execAfterTimestep->isChecked())
    te->append("  Exec Solver = After Timestep");
  
  if(ui.execNever->isChecked())
    te->append("  Exec Solver = Never");
  
  addSifLineBool("  Stabilize = ", ui.stabilizeCheck->isChecked());
  addSifLineBool("  Bubbles = ", ui.bubblesCheck->isChecked());
  addSifLineBool("  Lumped Mass Matrix = ", ui.lumpedMassCheck->isChecked());
  addSifLineBool("  Optimize Bandwidth = ", ui.optimizeBandwidthCheck->isChecked());
}


// Parse "Steady state" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseSteadyStateTab(Ui::solverParameterEditor ui)
{
  if(ui.steadyStateConvergenceToleranceEdit->text() == "") {
    cout << "Steady state convergence tolerance is undefined - aborting" << endl;
    return;
  }
  
  addSifLine("  Steady State Convergence Tolerance = ",
	      ui.steadyStateConvergenceToleranceEdit->text());
}


// Parse "Nonlinear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseNonlinearSystemTab(Ui::solverParameterEditor ui)
{
  addSifLine("  Nonlinear System Convergence Tolerance = ",
	      ui.nonlinSystemConvergenceToleranceEdit->text());
  
  addSifLine("  Nonlinear System Max Iterations = ", 
	      ui.nonlinSystemMaxIterationEdit->text());
  
  addSifLine("  Nonlinear System Newton After Iterations = ",
	      ui.nonlinSystemNewtonAfterIterEdit->text());
  
  addSifLine("  Nonlinear System Newton After Tolerance = ", 
	      ui.nonlinSystemNewtonAfterTolEdit->text());
  
  addSifLine("  Nonlinear System Relaxation Factor = ", 
	      ui.nonlinSystemRelaxationFactorEdit->text());
}


// Parse "Linear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseLinearSystemTab(Ui::solverParameterEditor ui)
{
  if(ui.linearSystemSolverDirect->isChecked()) {
    
    addSifLine("  Linear System Solver = ", "Direct");
    
    addSifLine("  Linear System Direct Method = ",
		ui.linearSystemDirectMethod->currentText());
    
  } else if(ui.linearSystemSolverIterative->isChecked()) {
    
    addSifLine("  Linear System Solver = ", "Iterative");
    
    addSifLine("  Linear System Iterative Method = ",
		ui.linearSystemIterativeMethod->currentText());
    
    addSifLine("  Linear System Max Iterations = ", 
		ui.linearSystemMaxIterationsEdit->text());
    
    addSifLine("  Linear System Convergence Tolerance = ",
		ui.linearSystemConvergenceToleranceEdit->text());
    
    addSifLine("  Linear System Preconditioning = ",
		ui.linearSystemPreconditioning->currentText());
    
    addSifLine("  Linear System ILUT Tolerance = ",
		ui.linearSystemILUTToleranceEdit->text());
    
    addSifLineBool("  Linear System Abort Not Converged = ",
		ui.linearSystemAbortWhenNotConvergedCheck->isChecked());
    
    addSifLine("  Linear System Residual Output = ",
		ui.linearSystemResiduaOutputEdit->text());
    
    addSifLine("  Linear System Precondition Recompute = ",
		ui.linearSystemPreconditionRecomputeEdit->text());
    
  } else if(ui.linearSystemSolverMultigrid->isChecked()) {
    
    addSifLine("  Linear System Solver = ", "Multigrid");
    
    // TODO: rest
  }
}

//------------------------------------------------------------------------
//
//                         COMMON UTILITY FUNCTIONS
//
//------------------------------------------------------------------------

void SifGenerator::addSifLine(const QString &var, const QString &val)
{
  if(val != "")
    te->append(var + val);
}

void SifGenerator::addSifLineBool(const QString &var, bool val)
{
  if(val == true)
    te->append(var + "True");
  else
    te->append(var + "False");
}


void SifGenerator::handleLineEdit(QDomElement elem, QWidget *widget)
{
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if( name == "" )
    name= elem.firstChildElement("Name").text().trimmed();

  QLineEdit *lineEdit = (QLineEdit*)widget;
  QString value = lineEdit->text().trimmed();
  addSifLine("  " + name + " = ", value);
}

void SifGenerator::handleComboBox(QDomElement elem, QWidget *widget)
{  
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if( name == "" )
    name= elem.firstChildElement("Name").text().trimmed();

  QComboBox *comboBox = (QComboBox*)widget;
  QString value = comboBox->currentText().trimmed();

  if(value != "None")
    addSifLine("  " + name + " = ", value);
}

void SifGenerator::handleCheckBox(QDomElement elem, QWidget *widget)
{
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if( name == "" )
    name = elem.firstChildElement("Name").text().trimmed();
  
  QString def_val = elem.firstChildElement("DefaultValue").text().trimmed();
  if ( def_val == "" )
    def_val = "False";

  QCheckBox *checkBox = (QCheckBox*)widget;
  
  if(checkBox->isChecked()) {
    if ( def_val != "True" )
      te->append("  " + name + " = True");
  } else {
    if ( def_val != "False" )
      te->append("  " + name + " = False");
  }
}
