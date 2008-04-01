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

  te->append("! Sif skeleton for active equations\n");
  te->append("Header");
  
  if(ui.checkKeywordsWarn->isChecked())
    te->append("  CHECK KEYWORDS Warn");

  const QString &qs1 = ui.meshDBEdit1->text(); 
  const QString &qs2 = ui.meshDBEdit2->text(); 
  const QString &qs3 = ui.includePathEdit->text(); 
  const QString &qs4 = ui.resultsDirectoryEdit->text(); 

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
	      ui.maxOutputLevelCombo->currentText());
  addSifLine("  Coordinate System = ",
	      ui.coordinateSystemCombo->currentText());
  addSifLine("  Coordinate Mapping(3) = ",
	      ui.coordinateMappingEdit->text());
  addSifLine("  Simulation Type = ", 
	      ui.simulationTypeCombo->currentText());
  addSifLine("  Steady State Max Iterations = ",
	      ui.steadyStateMaxIterEdit->text());
  addSifLine("  Output Intervals = ",
	      ui.outputIntervalsEdit->text());
  addSifLine("  Timestepping Method = ",
	      ui.timesteppingMethodCombo->currentText());
  addSifLine("  BDF Order = ",
	      ui.bdfOrderCombo->currentText());
  addSifLine("  Timestepping intervals = ",
	      ui.timeStepIntervalsEdit->text());
  addSifLine("  Timestep Sizes = ",
	      ui.timestepSizesEdit->text());
  addSifLine("  Solver Input File = ", 
	      ui.solverInputFileEdit->text());
  addSifLine("  Post File = ", 
	      ui.postFileEdit->text());

  te->append("End\n");
}


// Make Constants-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeConstantsBlock()
{
  Ui::setupDialog ui = generalSetup->ui;

  te->append("Constants");

  addSifLine("  Gravity(4) = ",
	      ui.gravityEdit->text());
  addSifLine("  Stefan Boltzmann = ",
	      ui.stefanBoltzmannEdit->text());
  addSifLine("  Permittivity of Vacuum = ",
	      ui.vacuumPermittivityEdit->text());

  te->append("End\n");
}


// Make Body-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyBlocks()
{
  // find out mesh domain ids:
  // -------------------------
  char str[1024];
  int maxindex=-1;
  for( int i=0; i < mesh->elements; i++)
  {
    element_t *element=&mesh->element[i];
    if ( (element->nature == PDE_BULK) &&( element->index > maxindex) )
      maxindex = element->index;
  }

  for( int i = 0; i < mesh->surfaces; i++)
  {
    element_t *element=&mesh->surface[i];
    if ( (element->nature == PDE_BULK) && (element->index > maxindex) )
      maxindex = element->index;
  }

  for( int i = 0; i < mesh->edges; i++)
  {
    element_t *element=&mesh->edge[i];
    if ( (element->nature == PDE_BULK) && (element->index > maxindex) )
      maxindex = element->index;
  }
  
  for( int i = 0; i < mesh->points; i++)
  {
    element_t *element=&mesh->point[i];
    if ( (element->nature == PDE_BULK) && (element->index > maxindex) )
      maxindex = element->index;
  }
  maxindex++;
  
  if(maxindex == 0)
    return;

  bool *body_tmp = new bool[maxindex];
  int  *body_id  = new  int[maxindex];

  for(int i = 0; i < maxindex; i++)
    body_tmp[i] = false;

  maxindex = 0;

  for(int i = 0; i < mesh->elements; i++) {
    element_t *element = &mesh->element[i];
    
    if(element->nature == PDE_BULK)
      if ( !body_tmp[element->index] ) {
        body_tmp[element->index] = true;
        body_id[maxindex++] = element->index;
      }
  }

  for(int i = 0; i < mesh->surfaces; i++) {
    element_t *element = &mesh->surface[i];
    if(element->nature == PDE_BULK)
      if ( !body_tmp[element->index] ) {
        body_tmp[element->index] = true;
        body_id[maxindex++] = element->index;
      }
  }
  
  for(int i = 0; i < mesh->edges; i++) {
    element_t *element = &mesh->edge[i];
    if(element->nature == PDE_BULK)
      if ( !body_tmp[element->index] ) {
        body_tmp[element->index] = true;
        body_id[maxindex++] = element->index;
      }
  }

  for(int i = 0; i < mesh->points; i++) {
    element_t *element = &mesh->point[i];
    if(element->nature == PDE_BULK)
      if ( !body_tmp[element->index] ) {
        body_tmp[element->index] = true;
        body_id[maxindex++] = element->index;
      }
  }
  
  te->append("Body 1");
  te->append("  Name = \"Body1\"");
  sprintf( str, "  Target Bodies(%d) =", maxindex );
  for( int i=0; i < maxindex; i++ ) 
     sprintf( str, "%s %d", str, max(body_id[i],1) );

  delete [] body_tmp;
  delete [] body_id;

  // Assume only one body with index=1, at the moment:
  BodyPropertyEditor *bodyEdit = &bodyPropertyEditor[1];

  te->append(str);
  te->append("  Body Force = 1");
  //const QString &qs1 = bodyEdit->ui.equationCombo->currentText();
  int ind1 = bodyEdit->ui.equationCombo->currentIndex();
  //te->append("  Equation = " + qs1);
  te->append("  Equation = " + QString::number(ind1+1));
  //const QString &qs2 = bodyEdit->ui.materialCombo->currentText();
  int ind2 = bodyEdit->ui.materialCombo->currentIndex();
  //te->append("  Material = " + qs2);
  te->append("  Material = " + QString::number(ind2+1));
  te->append("End\n");
}


// Make Equation-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeEquationBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pdePropertyEditor[0];

  if(p->menuAction == NULL) {
    cout << "No active equation - aborting" << endl;
    cout.flush();
    return;
  }
  
  int nofSolvers = 0;

  Ui::equationEditor ui = p->ui;

  if(ui.heatEquationActive->isChecked())
    nofSolvers++;

  if(ui.linearElasticityActive->isChecked())
    nofSolvers++;

  if(ui.navierStokesActive->isChecked())
    nofSolvers++;

  if(ui.advectionDiffusionActive->isChecked())
    nofSolvers++;

  if(ui.helmholtzEquationActive->isChecked())
    nofSolvers++;

  if(nofSolvers == 0) {
    cout << "There are no active solvers - aborting" << endl;
    cout.flush();
    return;
  }

  te->append("Equation 1");
  QString qs = "  Active Solvers(" + QString::number(nofSolvers) + ") =";
  for(int i = 0; i < nofSolvers; i++) 
    qs.append(" " + QString::number(i+1));
  te->append(qs);
  te->append( "  Element = \"" +  meshControl->elementCodesString + "\"" );
  if(ui.heatEquationConvectionConstant->isChecked())
    te->append( "  Convection = Constant" );
  if(ui.heatEquationConvectionComputed->isChecked())
    te->append( "  Convection = Computed" );

  te->append("End\n");  
}


// Make Solver-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeSolverBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
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

}








// Make Material-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeMaterialBlocks()
{
  int sifIndex = 0;

  for(int index = 0; index < MAX_MATERIALS; index++) {
    DynamicEditor *matEditor = &matPropertyEditor[index];
    
    if(matEditor->menuAction != NULL) {      
      te->append("Material " + QString::number(++sifIndex));
      
      QString name = matEditor->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);
      
      for(int i = 0; i < matEditor->hash.count(); i++) {
	hash_entry_t entry = matEditor->hash.values().at(i); 
	
	QWidget *widget = entry.widget;
        if ( entry.widget->isEnabled() ) {
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

void SifGenerator::handleLineEdit(QDomElement elem, QWidget *widget)
{
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if ( name == "" )
    name= elem.firstChildElement("Name").text().trimmed();

  QLineEdit *lineEdit = (QLineEdit*)widget;
  QString value = lineEdit->text().trimmed();
  addSifLine("  " + name + " = ", value);
}

void SifGenerator::handleComboBox(QDomElement elem, QWidget *widget)
{
  
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if ( name == "" )
    name= elem.firstChildElement("Name").text().trimmed();

  QComboBox *comboBox = (QComboBox*)widget;
  QString value = comboBox->currentText().trimmed();

  if(value != "None")
    addSifLine("  " + name + " = ", value);
}

void SifGenerator::handleCheckBox(QDomElement elem, QWidget *widget)
{
  QString name    = elem.firstChildElement("SifName").text().trimmed();
  if ( name == "" )
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





// Make BodyForce-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyForceBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pdePropertyEditor[0];

  // TODO: add functionality wrt ui
  Ui::equationEditor ui = p->ui;

  te->append("Body Force 1");

  if(ui.heatEquationActive->isChecked())
    te->append("  Heat Source = 1");

  if(ui.linearElasticityActive->isChecked()) {
    if(cdim >= 1) 
      te->append("  Stress BodyForce 1 = 1");
    if(cdim >= 2) 
      te->append("  Stress BodyForce 2 = 0");
    if(cdim >= 3) 
      te->append("  Stress BodyForce 3 = 0");
  }
  
  te->append("End\n");
}


// Make Booundary-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBoundaryBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *eqEdit = &pdePropertyEditor[0];

  int j = 0;

  // TODO: replace MAX_BCS with an actual value
#if 0
  for(int i = 0; i < MAX_BCS; i++) {
    BCPropertyEditor *bcEdit = &bcPropertyEditor[i];
    Ui::bcPropertyDialog ui = bcEdit->ui;

    if(bcEdit->touched) {
      
      te->append("Boundary condition " + QString::number(++j));
      te->append("  Target boundaries(1) = " + QString::number(i));
      
      if(eqEdit->ui.heatEquationActive->isChecked()) {
	addSifLine("  Temperature = ", ui.temperatureEdit->text());
	addSifLine("  Heat Flux = ", ui.heatFluxEdit->text());
      }
      
      if(eqEdit->ui.linearElasticityActive->isChecked()) {
	addSifLine("  Displacement 1 = ", ui.displacement1Edit->text());
	addSifLine("  Displacement 2 = ", ui.displacement2Edit->text());
	addSifLine("  Displacement 3 = ", ui.displacement3Edit->text());
      }

      if(eqEdit->ui.navierStokesActive->isChecked()) {
	addSifLine("  Velocity 1 = ", ui.Velocity1Edit->text());
	addSifLine("  Velocity 2 = ", ui.Velocity2Edit->text());
	addSifLine("  Velocity 3 = ", ui.Velocity3Edit->text());
      }
      
      te->append("End\n");
    }
  }  
#endif
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
