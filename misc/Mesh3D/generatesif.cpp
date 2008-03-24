#include "generatesif.h"
#include <iostream>

using namespace std;

GenerateSif::GenerateSif()
{
}

GenerateSif::~GenerateSif()
{
}


// Make Header-block:
//-----------------------------------------------------------------------------
void GenerateSif::makeHeaderBlock()
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
void GenerateSif::makeSimulationBlock()
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
void GenerateSif::makeConstantsBlock()
{
  te->append("Constants");
  te->append("  Gravity(4) = 0 -1 0 9.82");
  te->append("  Stefan Boltzmann = 5.67e-08");
  te->append("End\n");
}


// Make Equation-blocks:
//-----------------------------------------------------------------------------
void GenerateSif::makeEquationBlocks()
{
  if(pe->menuAction == NULL) {
    cout << "No active equation - aborting" << endl;
    cout.flush();
    return;
  }
  
  int nofSolvers = 0;

  Ui::equationEditor ui = pe->ui;

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
void GenerateSif::makeSolverBlocks()
{
  int currentSolver = 0;

  // user interface of the equation property editor:
  Ui::equationEditor ui = pe->ui;

  if(ui.heatEquationActive->isChecked()) {
    currentSolver++;
    // user interface of the solver parameter editor:
    Ui::solverParameterEditor ui = pe->solverParameterEditor[HEAT_EQUATION].ui;
    te->append("Solver " + QString::number(currentSolver));
    te->append("  Equation = \"Heat Equation\"");
    te->append("  Variable = Temperature");
    te->append("  Variable Dofs = 1");
    parseProcedure(ui, te);
    parseGeneralTab(ui, te);
    parseSteadyStateTab(ui, te);
    parseNonlinearSystemTab(ui, te);
    parseLinearSystemTab(ui, te);
    // todo: add adaptivity & multigrid
    te->append("End\n");
  }

  if(ui.linearElasticityActive->isChecked()) {
    currentSolver++;
    // user interface of the solver parameter editor:
    Ui::solverParameterEditor ui = pe->solverParameterEditor[LINEAR_ELASTICITY].ui;
    te->append("Solver " + QString::number(currentSolver));
    te->append("  Equation = \"Stress analysis\"");
    te->append("  Variable = Displacement");
    te->append("  Variable dofs = " + QString::number(cdim));
    parseProcedure(ui, te);
    parseGeneralTab(ui, te);
    parseSteadyStateTab(ui, te);
    parseNonlinearSystemTab(ui, te);
    parseLinearSystemTab(ui, te);
    // todo: add adaptivity & multigrid
    te->append("End\n");
  }
}



// Make Material-blocks:
//-----------------------------------------------------------------------------
void GenerateSif::makeMaterialBlocks()
{
  // TODO: add functionality wrt ui
  Ui::equationEditor ui = pe->ui;

  te->append("Material 1");
  te->append("  Name = \"Material1\"");
  te->append("  Density = 1");

  if(ui.heatEquationActive->isChecked())
    te->append("  Heat Conductivity = 1");

  if(ui.linearElasticityActive->isChecked()) {
    te->append("  Youngs modulus = 1");
    te->append("  Poisson ratio = 0.3");
  }

  te->append("End\n");
}


// Make BodyForce-blocks:
//-----------------------------------------------------------------------------
void GenerateSif::makeBodyForceBlocks()
{
  // TODO: add functionality wrt ui
  Ui::equationEditor ui = pe->ui;

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


// Parse "Procedure fields" from ui to sif:
//-----------------------------------------------------------------------------
void GenerateSif::parseProcedure(Ui::solverParameterEditor ui, QTextEdit *te)
{
  if((ui.procedureFileEdit->text() == "") && 
     (ui.procedureFunctionEdit->text() == ""))
    return;

  te->append("  Procedure = \"" + ui.procedureFileEdit->text() + "\" "
	                 + "\"" + ui.procedureFunctionEdit->text() + "\"");

  te->append(ui.procedureFunctionEdit->text());    
}



// Parse "Exec Solver" tab from ui to sif:
//-----------------------------------------------------------------------------
void GenerateSif::parseGeneralTab(Ui::solverParameterEditor ui, QTextEdit *te)
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

  if(ui.stabilizeCheck->isChecked())
    te->append("  Stabilize = True");
  else
    te->append("  Stabilize = False");

  if(ui.bubblesCheck->isChecked())
    te->append("  Bubbles = True");
  else
    te->append("  Bubbles = False");

  if(ui.lumpedMassCheck->isChecked())
    te->append("  Lumped Mass Matrix = True");
  else
    te->append("  Lumped Mass Matrix = False");

  if(ui.optimizeBandwidthCheck->isChecked())
    te->append("  Optimize Bandwidth = True");
  else
    te->append("  Optimize Bandwidth = False"); 
}


// Parse "Steady state" tab from ui to sif:
//-----------------------------------------------------------------------------
void GenerateSif::parseSteadyStateTab(Ui::solverParameterEditor ui, QTextEdit *te)
{
  te->append("  Steady State Convergence Tolerance = " 
	     + ui.steadyStateConvergenceToleranceEdit->text());
}


// Parse "Nonlinear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void GenerateSif::parseNonlinearSystemTab(Ui::solverParameterEditor ui, QTextEdit *te)
{
  te->append("  Nonlinear System Convergence Tolerance = " 
	     + ui.nonlinSystemConvergenceToleranceEdit->text());

  te->append("  Nonlinear System Max Iterations = " 
	     + ui.nonlinSystemMaxIterationEdit->text());

  te->append("  Nonlinear System Newton After Iterations = " 
	     + ui.nonlinSystemNewtonAfterIterEdit->text());

  te->append("  Nonlinear System Newton After Tolerance = " 
	     + ui.nonlinSystemNewtonAfterTolEdit->text());

  te->append("  Nonlinear System Relaxation Factor = " 
	     + ui.nonlinSystemRelaxationFactorEdit->text());
}


// Parse "Linear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void GenerateSif::parseLinearSystemTab(Ui::solverParameterEditor ui, QTextEdit *te)
{
  if(ui.linearSystemSolverDirect->isChecked()) {

    te->append("  Linear System Solver = Direct");

    te->append("  Linear System Direct Method = "
	       + ui.linearSystemDirectMethod->currentText());
    
  } else if(ui.linearSystemSolverIterative->isChecked()) {

    te->append("  Linear System Solver = Iterative");

    te->append("  Linear System Iterative Method = "
	       + ui.linearSystemIterativeMethod->currentText());

    te->append("  Linear System Max Iterations = " 
	       + ui.linearSystemMaxIterationsEdit->text());

    te->append("  Linear System Convergence Tolerance = " 
	       + ui.linearSystemConvergenceToleranceEdit->text());

    te->append("  Linear System Preconditioning = " 
	       + ui.linearSystemPreconditioning->currentText());

    te->append("  Linear System ILUT Tolerance = " 
	       + ui.linearSystemILUTToleranceEdit->text());
    
    if(ui.linearSystemAbortWhenNotConvergedCheck->isChecked())
      te->append("  Linear System Abort Not Converged = True");
    else
      te->append("  Linear System Abort Not Converged = False");

    te->append("  Linear System Residual Output = "
	       + ui.linearSystemResiduaOutputEdit->text());
    
    te->append("  Linear System Precondition Recompute = "
	       + ui.linearSystemPreconditionRecomputeEdit->text());

  } else if(ui.linearSystemSolverMultigrid->isChecked()) {
    
    te->append("  Linear System Solver = Multigrid");
    
    // TODO
  }
}
