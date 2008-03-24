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
  // At the moment only "Equation 1" is meaningful (index=0)
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
  // At the moment only "Equation 1" is meaningful (index=0)
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
  // At the moment only "Material 1" is meaningful (index=0)
  MATPropertyEditor *m = &me[0];

  if(m->menuAction == NULL) {
    cout << "There is no material defined - aborting" << endl;
    return;
  }

  Ui::materialEditor ui = m->ui;
  
  te->append("Material 1");
  te->append("  Name = " + ui.materialNameEdit->text());

  // General parameters
  te->append("  Density = " + ui.densityEdit->text());

  if(ui.heatEquationActive->isChecked()) {
    te->append("  Heat Capacity = " + ui.heatEquationHeatCapacityEdit->text());
    te->append("  Heat Conductivity = " + ui.heatEquationHeatConductivityEdit->text());
    te->append("  Enthalpy = " + ui.heatEquationEnthalpyEdit->text());
  }

  if(ui.linearElasticityActive->isChecked()) {
    te->append("  Heat Expansion Coefficient = " + ui.linearElasticityHeatExpansionEdit->text());
    te->append("  Reference Temperature = " + ui.linearElasticityReferenceTempEdit->text());
    te->append("  Youngs Modulus = " + ui.linearElasticityYoungsModulusEdit->text());
    te->append("  Poisson Ratio = " + ui.linearElasticityPoissonRatioEdit->text());
  }

  // TODO: rest of materials

  te->append("End\n");
}


// Make BodyForce-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyForceBlocks()
{
  // At the moment only "Equation 1" is meaningful (index=0)
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
  // At the moment only "Equation 1" is meaningful (index=0)
  PDEPropertyEditor *p = &pe[0];

  Ui::equationEditor ui = p->ui;

  int j = 0;

  QString qs = "";

  for(int i = 1; i < bcPropertyEditor->maxindex; i++) {
    bcProperty_t *bp = &bcPropertyEditor->bcProperty[i];

    if(bp->defined) {

      te->append("Boundary condition " + QString::number(++j));

      te->append("  Target boundaries(1) = " + QString::number(i));

      if(ui.heatEquationActive->isChecked()) {

	qs = bp->temperature;
	if(qs != "")
	  te->append("  Temperature = " + qs);
	
	qs = bp->heatFlux;
	if(qs != "")
	  te->append("  Heat Flux = " + qs);
      }

      if(ui.linearElasticityActive->isChecked()) {

	qs = bp->displacement1;
	if(qs != "")
	  te->append("  Displacement 1 = " + qs);
	
	qs = bp->displacement2;
	if(qs != "")
	  te->append("  Displacement 2 = " + qs);
	
	qs = bp->displacement3;
	if(qs != "")
	  te->append("  Displacement 3 = " + qs);
      }

      te->append("End\n");
    }
  }  
}


// Parse "Procedure fields" from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseProcedure(Ui::solverParameterEditor ui)
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
void SifGenerator::parseSteadyStateTab(Ui::solverParameterEditor ui)
{
  te->append("  Steady State Convergence Tolerance = " 
	     + ui.steadyStateConvergenceToleranceEdit->text());
}


// Parse "Nonlinear system" tab from ui to sif:
//-----------------------------------------------------------------------------
void SifGenerator::parseNonlinearSystemTab(Ui::solverParameterEditor ui)
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
void SifGenerator::parseLinearSystemTab(Ui::solverParameterEditor ui)
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
