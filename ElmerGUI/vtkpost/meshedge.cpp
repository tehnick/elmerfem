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
 *  ElmerGUI meshedge                                                        *
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

#include <QtGui>
#include <iostream>
#include "vtkpost.h"
#include "meshedge.h"
#include "preferences.h"

#include <vtkUnstructuredGrid.h>
#include <vtkExtractEdges.h>
#include <vtkDataSetMapper.h>
#include <vtkProperty.h>
#include <vtkActor.h>

using namespace std;

MeshEdge::MeshEdge(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  setWindowTitle("Mesh edges");
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));
}

MeshEdge::~MeshEdge()
{
}

void MeshEdge::draw(VtkPost* vtkPost, Preferences* preferences)
{
  bool useSurfaceGrid = preferences->ui.meshEdgesSurface->isChecked();
  int lineWidth = preferences->ui.meshLineWidth->value();

  vtkUnstructuredGrid *grid = NULL;

  if(useSurfaceGrid) {
    grid = vtkPost->GetSurfaceGrid();
  } else {
    grid = vtkPost->GetVolumeGrid();
  }

  if(!grid) return;

  if(grid->GetNumberOfCells() < 1) return;

  vtkExtractEdges *edges = vtkExtractEdges::New();
  edges->SetInput(grid);

  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(edges->GetOutputPort());
  mapper->ScalarVisibilityOff();
  mapper->SetResolveCoincidentTopologyToPolygonOffset();
  // mapper->ImmediateModeRenderingOn();

  vtkPost->GetMeshEdgeActor()->GetProperty()->SetLineWidth(lineWidth);
  vtkPost->GetMeshEdgeActor()->GetProperty()->SetColor(0, 0, 0);
  vtkPost->GetMeshEdgeActor()->SetMapper(mapper);

  mapper->Delete();
  edges->Delete();
}
