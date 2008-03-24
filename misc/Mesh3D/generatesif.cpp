#include "generatesif.h"
#include <iostream>

using namespace std;

GenerateSif::GenerateSif()
{
}

GenerateSif::~GenerateSif()
{
}



// Make Solevr-blocks:
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
