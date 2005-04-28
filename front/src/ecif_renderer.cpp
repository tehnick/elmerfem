/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , Center for Scientific Computing,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: Center for Scientific Computing
*                         Tietotie 6, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front
Module:     ecif_renderer.cpp
Language:   C++
Date:       22.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract: Implementation.

************************************************************************/

#include "ecif_control.h"
#include "ecif_model.h"
#include "ecif_renderer.h"
#include "ecif_userinterface.h"

//Initialize static class variables.
Control* Renderer::theControlCenter = NULL;
Model* Renderer::model = NULL;
bool Renderer::visible = false;
Timer* Renderer::doubleClickTimer = NULL;
Timer* Renderer::mouseMoveTimer = NULL;

RendererInfo Renderer::rendererInfo = {0};


// Finds if some axis priority is set
// and updates default-rotate axis accordingly
void
Renderer::getRotateAxis(int& default_axis)
{
  int axis = default_axis;

  // Change default_axis only if some
  // priority is set
  if (rotateAxisX) {
    axis = 0;
  }
  else if (rotateAxisY) {
    axis = 1;
  }
  else if (rotateAxisZ) {
    axis = 2;
  }

  default_axis = axis;
}

void
Renderer::initClass(Model* mdl)
{
  model = mdl;
}


void
Renderer::selectBoundary(int elem_id,
                         int body1_id, int layer1_id,
                         int body2_id, int layer2_id,
                         bool accept_body_change, bool update_gui)
{
  model->boundarySelected(this, elem_id,
                          body1_id, layer1_id,
                          body2_id, layer2_id,
                          accept_body_change, update_gui);
  refresh();
}


void
Renderer::selectBoundaries(int nof_elements, int* elem_ids,
                           int* body1_ids, int* layer1_ids,
                           int* body2_ids, int* layer2_ids,
                           bool accept_body_change, bool update_gui)
{
  for (int i = 0; i < nof_elements; i++) {
    model->boundarySelected(this, elem_ids[i],
                            body1_ids[i], layer1_ids[i],
                            body2_ids[i], layer2_ids[i],
                            accept_body_change, update_gui);
  }

  refresh();
}


void
Renderer::setDimension(ecif_modelDimension dimension)
{
  if ( dimension == ECIF_2D ) {
    is2D = true;
    is2DSimulation = true;

  } else {
    is2D = false;
    is2DSimulation = false;
  }
}


void
Renderer::setDrawBox(bool in_draw_mode)
{
  inBoxDrawMode = in_draw_mode;
}


void
Renderer::setDrawVector(bool in_draw_mode)
{
  inVectorDrawMode = in_draw_mode;
}


void
Renderer::setEditBoundaries(bool in_edit_mode)
{
  UserInterface* gui = theControlCenter->getGui();

  inMeshEditingMode = in_edit_mode;

  if (inMeshEditingMode) {
    gui->configureButtons("draw_target_bodies", 0);
  }
  else {
    gui->configureButtons("draw_target_bodies", 1);
    //model->resetMeshSelected();
    //refresh();
  }

}


// Set current rotate axis for the mouse movements
// in the screen.
// If all flags are false, then rotation
// on the screen is X,Y, otherwise the flag which
// is true, defines the axis for both x and y mouse movements
void
Renderer::setRotatePriorities(bool rot_x, bool rot_y, bool rot_z)
{
  rotateAxisX = rot_x;
  rotateAxisY = rot_y;
  rotateAxisZ = rot_z;
}


void
Renderer::setSimulationDimension(ecif_modelDimension simulation_dimension)
{
  if ( simulation_dimension == ECIF_2D ) {
    is2DSimulation = true;
  } else {
    is2DSimulation = false;
  }
}




