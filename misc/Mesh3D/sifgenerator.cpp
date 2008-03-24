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
  te->append("! Sif skeleton for active equations\n");
  te->append("Header");
  te->append("  CHECK KEYWORDS Warn");
  te->append("  Mesh DB \".\" \".\"");
  te->append("  Include Path \"\"");
  te->append("  Results Directory \"\"");
  te->append("End\n");
}


// Make Simulation-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeSimulationBlock()
{
  te->append("Simulation");
  te->append("  Max Output Level = 4");
  te->append("  Coordinate System = \"Cartesian\"");
  te->append("  Coordinate Mapping(3) = 1 2 3");
  te->append("  Simulation Type = \"Steady State\"");
  te->append("  Steady State Max Iterations = 1");
  te->append("  Output Intervals = 1");
  te->append("  Solver Input File = \"skeleton.sif\"");
  te->append("  Post File = \"skeleton.ep\"");
  te->append("End\n");
}


// Make Constants-block:
//-----------------------------------------------------------------------------
void SifGenerator::makeConstantsBlock()
{
  te->append("Constants");
  te->append("  Gravity(4) = 0 -1 0 9.82");
  te->append("  Stefan Boltzmann = 5.67e-08");
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

  te->append(str);
  te->append("  Body Force = 1");
  te->append("  Equation = 1");
  te->append("  Material = 1");
  te->append("End\n");
}


// Make Equation-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeEquationBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pe[0];

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
    cout << "There are no active solvers - unable to continue with SIF" << endl;
    cout.flush();
    return;
  }

  te->append("Equation 1");
  QString qs = "  Active Solvers(" + QString::number(nofSolvers) + ") =";
  for(int i = 0; i < nofSolvers; i++) 
    qs.append(" " + QString::number(i+1));
  te->append(qs);
  te->append( "  Element = \"" +  meshControl->elementCodesString + "\"" );
  te->append("End\n");  
}


// Make Solver-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeSolverBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pe[0];

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
}



// Make Material-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeMaterialBlocks()
{
  // TODO: At the moment only "Material 1" is meaningful (index=0)
  MATPropertyEditor *m = &me[0];

  if(m->menuAction == NULL) {
    cout << "There is no material - aborting" << endl;
    return;
  }

  Ui::materialEditor ui = m->ui;

  if(ui.densityEdit->text() == "") {
    cout << "Undefined density - aborting" << endl;
    return;
  }
  
  te->append("Material 1");

  // Name
  addLineEdit("  Name = ", ui.materialNameEdit->text());

  // General parameters
  addLineEdit("  Density = ", ui.densityEdit->text());
  addLineEdit("  Reference Temperature = ", ui.referenceTemperatureEdit->text());
  addLineEdit("  Reference Temperature = ", ui.referencePressureEdit->text());
  addLineEdit("  Heat Expansion Coefficient = ", ui.heatExpansionCoefficientEdit->text());
  addLineEdit("  Heat Capacity = ", ui.heatCapacityEdit->text());
  addLineEdit("  Convection Velocity 1 = ", ui.convectionVelocity1Edit->text());
  addLineEdit("  Convection Velocity 2 = ", ui.convectionVelocity2Edit->text());
  addLineEdit("  Convection Velocity 3 = ", ui.convectionVelocity3Edit->text());

  // Heat equation
  addLineEdit("  Heat Conductivity = ", ui.heatEquationHeatConductivityEdit->text());
  addLineEdit("  Enthalpy = ", ui.heatEquationEnthalpyEdit->text());
  
  // Linear elasticity
  addLineEdit("  Youngs Modulus = ", ui.linearElasticityYoungsModulusEdit->text());
  addLineEdit("  Poisson Ratio = ", ui.linearElasticityPoissonRatioEdit->text());
  
  // Navier-Stokes
  addLineEdit("  Viscosity = ", ui.navierStokesViscosityEdit->text());
  addLineEdit("  Specific Heat Ratio = ", ui.navierStokesSpecificHeatEdit->text());
  
  if(ui.navierStokesIncompressibleButton->isChecked())
    te->append("  Compressibility Model = None");
  else
    te->append("  Compressibility Model = Perfect Gas Equation 1");
  
  // Advection-diffusion
  addLineEdit("  Species Diffusivity = ", ui.advectionDiffusionSpeciesDiffEdit->text());

  // Helmholtz equation
  addLineEdit("  Sound Speed = ", ui.helmholtzEquationSoundSpeedEdit->text());
  addLineEdit("  Damping = ", ui.helmholtzEquationDampingEdit->text());

  te->append("End\n");
}


// Make BodyForce-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyForceBlocks()
{
  // TODO: At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pe[0];

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
  PDEPropertyEditor *p = &pe[0];

  Ui::equationEditor ui = p->ui;

  int j = 0;

  for(int i = 1; i < bcPropertyEditor->maxindex; i++) {
    bcProperty_t *bp = &bcPropertyEditor->bcProperty[i];

    if(bp->defined) {

      te->append("Boundary condition " + QString::number(++j));
      te->append("  Target boundaries(1) = " + QString::number(i));

      if(ui.heatEquationActive->isChecked()) {
	addLineEdit("  Temperature = ", bp->temperature);
	addLineEdit("  Heat Flux = ", bp->heatFlux);
      }

      if(ui.linearElasticityActive->isChecked()) {
	addLineEdit("  Displacement 1 = ", bp->displacement1);
	addLineEdit("  Displacement 2 = ", bp->displacement2);
	addLineEdit("  Displacement 3 = ", bp->displacement3);
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

  te->append(ui.procedureFunctionEdit->text());    
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
  
  addLineBool("  Stabilize = ", ui.stabilizeCheck->isChecked());
  addLineBool("  Bubbles = ", ui.bubblesCheck->isChecked());
  addLineBool("  Lumped Mass Matrix = ", ui.lumpedMassCheck->isChecked());
  addLineBool("  Optimize Bandwidth = ", ui.optimizeBandwidthCheck->isChecked());
}


// Parse "Steady state" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseSteadyStateTab(Ui::solverParameterEditor ui)
{
  if(ui.steadyStateConvergenceToleranceEdit->text() == "") {
    cout << "Steady state convergence tolerance is undefined" << endl;
    return;
  }
  
  addLineEdit("  Steady State Convergence Tolerance = ",
	      ui.steadyStateConvergenceToleranceEdit->text());
}


// Parse "Nonlinear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseNonlinearSystemTab(Ui::solverParameterEditor ui)
{
  addLineEdit("  Nonlinear System Convergence Tolerance = ",
	      ui.nonlinSystemConvergenceToleranceEdit->text());
  
  addLineEdit("  Nonlinear System Max Iterations = ", 
	      ui.nonlinSystemMaxIterationEdit->text());
  
  addLineEdit("  Nonlinear System Newton After Iterations = ",
	      ui.nonlinSystemNewtonAfterIterEdit->text());
  
  addLineEdit("  Nonlinear System Newton After Tolerance = ", 
	      ui.nonlinSystemNewtonAfterTolEdit->text());
  
  addLineEdit("  Nonlinear System Relaxation Factor = ", 
	      ui.nonlinSystemRelaxationFactorEdit->text());
}


// Parse "Linear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseLinearSystemTab(Ui::solverParameterEditor ui)
{
  if(ui.linearSystemSolverDirect->isChecked()) {
    
    addLineEdit("  Linear System Solver = ", "Direct");
    
    addLineEdit("  Linear System Direct Method = ",
		ui.linearSystemDirectMethod->currentText());
    
  } else if(ui.linearSystemSolverIterative->isChecked()) {
    
    addLineEdit("  Linear System Solver = ", "Iterative");
    
    addLineEdit("  Linear System Iterative Method = ",
		ui.linearSystemIterativeMethod->currentText());
    
    addLineEdit("  Linear System Max Iterations = ", 
		ui.linearSystemMaxIterationsEdit->text());
    
    addLineEdit("  Linear System Convergence Tolerance = ",
		ui.linearSystemConvergenceToleranceEdit->text());
    
    addLineEdit("  Linear System Preconditioning = ",
		ui.linearSystemPreconditioning->currentText());
    
    addLineEdit("  Linear System ILUT Tolerance = ",
		ui.linearSystemILUTToleranceEdit->text());
    
    addLineBool("  Linear System Abort Not Converged = ",
		ui.linearSystemAbortWhenNotConvergedCheck->isChecked());
    
    addLineEdit("  Linear System Residual Output = ",
		ui.linearSystemResiduaOutputEdit->text());
    
    addLineEdit("  Linear System Precondition Recompute = ",
		ui.linearSystemPreconditionRecomputeEdit->text());
    
  } else if(ui.linearSystemSolverMultigrid->isChecked()) {
    
    addLineEdit("  Linear System Solver = ", "Multigrid");
    
    // TODO: rest
  }
}


void SifGenerator::addLineEdit(const QString &var, const QString &val)
{
  if(val != "")
    te->append(var + val);
}

void SifGenerator::addLineBool(const QString &var, bool val)
{
  if(val == true)
    te->append(var + "True");
  else
    te->append(var + "False");
}
