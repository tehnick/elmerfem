/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland    *
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
 *  Address: CSC - IT Center for Science Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

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

  void setMesh(mesh_t*);
  void setTextEdit(QTextEdit*);
  void setDim(int);
  void setCdim(int);
  void setElmerDefs(QDomDocument*);
  void setGeneralSetup(GeneralSetup*);
  void setEquationEditor(QVector<DynamicEditor*>&);
  void setMaterialEditor(QVector<DynamicEditor*>&);
  void setBodyForceEditor(QVector<DynamicEditor*>&);
  void setInitialConditionEditor(QVector<DynamicEditor*>&);
  void setBoundaryConditionEditor(QVector<DynamicEditor*>&);
  void setSolverParameterEditor(QVector<SolverParameterEditor*>&);
  //?? void setBoundaryPropertyEditor(BoundaryPropertyEditor*);
  void setBoundaryPropertyEditor(QVector<BoundaryPropertyEditor*>&);
  void setBodyPropertyEditor(QVector<BodyPropertyEditor*>&);
  void setMeshControl(MeshControl*);
  void setLimit(Limit*);

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
  mesh_t* mesh;
  QTextEdit* te;
  int dim, cdim;
  QDomDocument* elmerDefs;
  GeneralSetup* generalSetup;
  QVector<DynamicEditor*> equationEditor;
  QVector<DynamicEditor*> materialEditor;
  QVector<DynamicEditor*> bodyForceEditor;
  QVector<DynamicEditor*> initialConditionEditor;
  QVector<DynamicEditor*> boundaryConditionEditor;
  QVector<SolverParameterEditor*> solverParameterEditor;
  QVector<BoundaryPropertyEditor*> boundaryPropertyEditor;
  QVector<BodyPropertyEditor*> bodyPropertyEditor;
  MeshControl* meshControl;
  Limit* limit;

  int  sort_index(int,int [],QString []);
  int  findHashValue(DynamicEditor*,QString,QString);
  bool parseSolverSpecificTab(DynamicEditor *, QString);
  void parseGeneralTab(Ui::solverParameterEditor);
  void parseSteadyStateTab(Ui::solverParameterEditor);
  void parseNonlinearSystemTab(Ui::solverParameterEditor);
  void parseLinearSystemTab(Ui::solverParameterEditor);
  void parseParallelTab(Ui::solverParameterEditor);

  void addSifLine(const QString&, const QString&);
  void addSifLineBool(const QString&, bool);

  void handleBCLineEdit(QDomElement, QWidget*,int *);
  void handleLineEdit(QDomElement, QWidget*);
  void handleComboBox(QDomElement, QWidget*);
  void handleCheckBox(QDomElement, QWidget*);
};

#endif // SIFGENERATOR_H
