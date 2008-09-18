/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUI sifgenerator                                                    *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/


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
  addSifLine("  Timestep intervals = ",
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
  addSifLine("  Boltzmann Constant = ",
	     ui.boltzmannEdit->text().trimmed());
  addSifLine("  Unit Charge = ",
	     ui.unitChargeEdit->text().trimmed());

  te->append("End\n");
}


// Make Body-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeBodyBlocks()
{
  int i;

  int sifIndex = 0, maxOriginalIndex=-1;

  for(int index = 0; index < bodyMap.count(); index++) {
    BodyPropertyEditor *bodyEdit = &bodyPropertyEditor[index];
    
    int originalIndex = bodyMap.key(index);
    maxOriginalIndex = max(maxOriginalIndex, originalIndex );

    if(bodyEdit->touched) {
      te->append("Body " + QString::number(++sifIndex));

      te->append("  Target Bodies(1) = " + QString::number(originalIndex));

      if ( bodyEdit->ui.nameEdit->text().trimmed() == "" )
        te->append("  Name = Body " + QString::number(sifIndex) );
      else
        te->append("  Name = " + bodyEdit->ui.nameEdit->text().trimmed());

      i = bodyEdit->ui.equationCombo->currentIndex();
      if(i > 0)
	te->append("  Equation = " + QString::number(i));
      // te->append("  Equation = " + QString::number(i+1));
      
      i = bodyEdit->ui.materialCombo->currentIndex();
      if(i > 0)
	te->append("  Material = " + QString::number(i));
      // te->append("  Material = " + QString::number(i+1));
      
      i = bodyEdit->ui.bodyForceCombo->currentIndex();
      if(i > 0)
	te->append("  Body Force = " + QString::number(i));
      // te->append("  Body Force = " + QString::number(i+1));
      
      i = bodyEdit->ui.initialConditionCombo->currentIndex();
      if(i > 0)
	te->append("  Initial condition = " + QString::number(i));
      // te->append("  Initial condition = " + QString::number(i+1));
      
      te->append("End\n");      
    }
  }

  for( int index=0; index < limit->maxBoundaries(); index++ )
  {
    BodyPropertyEditor *bodyEdit=boundaryPropertyEditor[index].bodyProperties;

    if(bodyEdit && bodyEdit->touched ) {
      te->append("Body " + QString::number(++sifIndex));

      boundaryPropertyEditor[index].bodyID = ++maxOriginalIndex;
      te->append("  Target Bodies(1) = " + QString::number(maxOriginalIndex));

      if ( bodyEdit->ui.nameEdit->text().trimmed() == "" )
        te->append("  Name = Body " + QString::number(sifIndex) );
      else
        te->append("  Name = " + bodyEdit->ui.nameEdit->text().trimmed());

      i = bodyEdit->ui.equationCombo->currentIndex();
      if(i > 0)
	te->append("  Equation = " + QString::number(i));
      // te->append("  Equation = " + QString::number(i+1));
      
      i = bodyEdit->ui.materialCombo->currentIndex();
      if(i > 0)
	te->append("  Material = " + QString::number(i));
      // te->append("  Material = " + QString::number(i+1));
      
      i = bodyEdit->ui.bodyForceCombo->currentIndex();
      if(i > 0)
	te->append("  Body Force = " + QString::number(i));
      // te->append("  Body Force = " + QString::number(i+1));
      
      i = bodyEdit->ui.initialConditionCombo->currentIndex();
      if(i > 0)
	te->append("  Initial condition = " + QString::number(i));
      // te->append("  Initial condition = " + QString::number(i+1));
      
      te->append("End\n");      
    }
  }
}



// Make Equation/Solver -blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeEquationBlocks()
{
  // enumerate solvers && write solver blocks:

  int solverNumber = 0;
  QHash<QString, int> numberForSolver;
  numberForSolver.clear();
  for(int index = 0; index < limit->maxEquations(); index++) {
    DynamicEditor *eqEditor = &equationEditor[index];
    if(eqEditor->menuAction != NULL) {
      for(int i = 0; i < eqEditor->hash.count(); i++) {
	hash_entry_t entry = eqEditor->hash.values().at(i); 
	QWidget *widget = entry.widget;
        if(widget->isEnabled()) {
	  QString key = eqEditor->hash.keys().at(i);
	  QStringList keySplitted = key.split("/");
	  QString solverName = keySplitted.at(1).trimmed();
	  QString labelName = keySplitted.at(3).trimmed();
          QDomElement elem = entry.elem;
	  if((labelName == "Active") &&
	     (elem.attribute("Widget", "") == "CheckBox")) {
	    QCheckBox *checkBox = (QCheckBox*)widget;
	    if(checkBox->isChecked()) {
	      if(!numberForSolver.contains(solverName)) {
		numberForSolver.insert(solverName, ++solverNumber);
		te->append("Solver " + QString::number(solverNumber));
		te->append("  Equation = " + solverName);
		makeSolverBlocks(solverName);
		te->append("End");
		te->append("");
	      }
	    }
	  }
	}
      }
    }
  }
  
  // generate equation blocks:
  int sifIndex = 0;
  for(int index = 0; index < limit->maxEquations(); index++) {
    DynamicEditor *eqEditor = &equationEditor[index];

    if(eqEditor->menuAction != NULL) {
      te->append("Equation " + QString::number(++sifIndex));
      
      QString name = eqEditor->nameEdit->text().trimmed();
      addSifLine("  Name = ", name);

      QString solverString = "";
      int nofSolvers = 0;
      int solverActive[limit->maxSolvers()];

      for( int i=0; i < limit->maxSolvers(); i++ )
        solverActive[i] = false;

      for(int i = 0; i < eqEditor->hash.count(); i++) {
	hash_entry_t entry = eqEditor->hash.values().at(i); 
	QWidget *widget = entry.widget;

        if(widget->isEnabled()) {
          QDomElement elem = entry.elem;
	  QString key = eqEditor->hash.keys().at(i);
	  QStringList keySplitted = key.split("/");	  
	  QString solverName = keySplitted.at(1).trimmed();
	  QString labelName = keySplitted.at(3).trimmed();

	  // solver active?
	  if((labelName == "Active") && (elem.attribute("Widget", "") == "CheckBox")) {
	    QCheckBox *checkBox = (QCheckBox*)widget;
	    if(checkBox->isChecked()) {
	      nofSolvers++;
	      solverNumber = numberForSolver.value(solverName);
              solverActive[solverNumber] = true;
              solverString += " " + QString::number(solverNumber);
	    }
	  }
        }
      }

      for(int i = 0; i < eqEditor->hash.count(); i++) {
	hash_entry_t entry = eqEditor->hash.values().at(i); 
	QWidget *widget = entry.widget;

        if(widget->isEnabled()) {
          QDomElement elem = entry.elem;
	  QString key = eqEditor->hash.keys().at(i);
	  QStringList keySplitted = key.split("/");	  
	  QString solverName = keySplitted.at(1).trimmed();
	  QString labelName = keySplitted.at(3).trimmed();

          solverNumber = numberForSolver.value(solverName);
          if ( !solverActive[solverNumber] ) continue;

          if((elem.attribute("Widget", "") == "CheckBox") &&
	     (labelName != "Active")) 
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

//-------------------------------------------------------------------------
void SifGenerator::makeSolverBlocks(QString solverName)
{
  SolverParameterEditor *spe, *tmp;
  Ui::solverParameterEditor ui;

  bool found = false;
  int current=-1;

  for(int i = 0; i < limit->maxSolvers(); i++) {
    spe = &solverParameterEditor[i];
    QString currentName = spe->solverName.trimmed();
    if(currentName == solverName) {
      found = true;
      current = i;
      break;
    }
  }

  if(!found) {
    tmp = new SolverParameterEditor;
  } else {
    tmp = spe;
  }

  if ( !tmp->generalOptions ) {
    tmp->generalOptions = new DynamicEditor;
    tmp->generalOptions->setupTabs(*elmerDefs, "Solver", current );
  }

  parseSolverSpecificTab(tmp->generalOptions, solverName);


  ui = tmp->ui; 
  parseGeneralTab(ui);
  parseSteadyStateTab(ui);
  parseNonlinearSystemTab(ui);
  parseLinearSystemTab(ui);
  // todo: add adaptivity & multigrid

  if(!found)
  {
    delete tmp->generalOptions;
    delete tmp;
  }
}



// Make Material-blocks:
//-----------------------------------------------------------------------------
void SifGenerator::makeMaterialBlocks()
{
  int sifIndex = 0;

  for(int index = 0; index < limit->maxMaterials(); index++) {
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

  for(int index = 0; index < limit->maxBodyforces(); index++) {
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
  
  for(int index = 0; index < limit->maxInitialconditions(); index++) {
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
  int boundaryBC[limit->maxBoundaries()];

  for(int index = 0; index < boundaryMap.count(); index++) {
    BoundaryPropertyEditor *bEdit = &boundaryPropertyEditor[index];
    if(bEdit->touched)
      boundaryBC[index] = ++sifIndex;
  }

  sifIndex = 0;
  for(int index = 0; index < boundaryMap.count(); index++) {
    BoundaryPropertyEditor *bEdit = &boundaryPropertyEditor[index];

    if(bEdit->touched) {
      te->append("Boundary Condition " + QString::number(++sifIndex));

      int originalIndex = boundaryMap.key(index);

      te->append("  Target Boundaries(1) = " + QString::number(originalIndex));

      if ( bEdit->bodyProperties ) {
        te->append("  Body id = " + QString::number(bEdit->bodyID) );
      }

      int i = bEdit->ui.boundaryConditionCombo->currentIndex();
      const QString &name = bEdit->ui.boundaryConditionCombo->currentText().trimmed();
      if(i > -1) 
	te->append("  Name = " +  name);

      // check which one of the dynamic editors has "name" typed in nameEdit:
      for(int j = 0; j < limit->maxBcs(); j++) {
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
		  handleBCLineEdit(elem, widget, boundaryBC);
		
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



// Parse "Solver specific tab"
//-----------------------------------------------------------------------------
void SifGenerator::parseSolverSpecificTab(DynamicEditor *solEditor, QString solverName)
{
  if ( !solEditor ) return;

  QScriptEngine engine; 

  QScriptValue dim_QSV  = QScriptValue(&engine,dim);
  engine.globalObject().setProperty( "dim", dim_QSV );

  QScriptValue cdim_QSV = QScriptValue(&engine,cdim);
  engine.globalObject().setProperty( "cdim", cdim_QSV );

  for(int i = 0; i < solEditor->hash.count(); i++) {
    hash_entry_t entry = solEditor->hash.values().at(i);


    QString key = solEditor->hash.keys().at(i);
    QStringList keySplitted = key.split("/");	  
    QString tabName   = keySplitted.at(1).trimmed();
    QString labelName = keySplitted.at(3).trimmed();

    if ( tabName != solverName ) continue;


    // variable names handled separately...
    // ------------------------------------
    if ( labelName=="Variable" || labelName.mid(0,17)=="Exported Variable" ) {
      if( entry.elem.attribute("Widget", "") != "Edit") continue;

      QLineEdit *l = (QLineEdit *)entry.widget;
      QString varName = l->text().trimmed();

      if ( varName == "" ) continue;

      int dofs=1;
      QStringList dofsplit = varName.split("[");
      if ( dofsplit.count()>1 ) {
        varName = dofsplit.at(0).trimmed() + "[";
        QString dof = dofsplit.at(1).trimmed();
        dof = dof.split("]").at(0).trimmed();

        dofsplit = dof.split(":");
        QString subVarName = dofsplit.at(0).trimmed();
        for( int i=1; i<dofsplit.count(); i++)
        {
          dof = dofsplit.at(i).trimmed();

          QStringList subDofSplit = dof.split(" ");
          QString subDof = subDofSplit.at(0).trimmed();

          dofs = engine.evaluate(subDof).toInt32();
          if (i>1) varName = varName + " ";
          varName = varName + subVarName + ":" + QString::number(dofs);

          if ( subDofSplit.count() > 1 )
            subVarName = subDofSplit.at(1).trimmed();
        }
        varName = varName + "]";
        addSifLine( "  " + labelName + " = ", varName );
      } else {
        dofsplit = varName.split("(");
        if ( dofsplit.count()>1 ) {
          varName = dofsplit.at(0).trimmed();
          QString dof = dofsplit.at(1).trimmed();
          dofsplit = dof.split(")");
          dof = dofsplit.at(0).trimmed();
          dofs = engine.evaluate(dof).toInt32();
        }
        if ( dofs <= 0 ) dofs = 1;
        addSifLine( "  "+labelName+" = -dofs ",  QString::number(dofs) + " " + varName );
      }
      continue;
    }

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

  if( ui.steadyStateConvergenceMeasureCombo->currentText().trimmed() != "Norm") 
    addSifLine("  Steady State Convergence Measure = ",
	       ui.steadyStateConvergenceMeasureCombo->currentText().trimmed());
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

  if( ui.nonlinSystemConvergenceMeasureCombo->currentText().trimmed() != "Norm") 
    addSifLine("  Nonlinear System Convergence Measure = ",
	       ui.nonlinSystemConvergenceMeasureCombo->currentText().trimmed());

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


void SifGenerator::handleBCLineEdit(QDomElement elem, QWidget *widget,int *boundaryBC)
{
  QString name = elem.firstChildElement("SifName").text().trimmed();
  if( name == "" )
    name= elem.firstChildElement("Name").text().trimmed();

  QLineEdit *lineEdit = (QLineEdit*)widget;
  QString value = lineEdit->text().trimmed();

  if ( name=="Periodic BC" && value != "" ) 
  {
     int val = value.toInt(); 
     val = boundaryMap.value(val);
     value=QString::number(boundaryBC[val]);
  }

  addSifLine("  " + name + " = ", value);
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
