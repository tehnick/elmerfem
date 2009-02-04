/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: CSC - IT Center for Science Ltd.
*                         Keilaranta 14, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front
Module:     ecif_control.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   An class for system level control object.

************************************************************************/

#ifndef _ECIF_CONTROL_
#define _ECIF_CONTROL_

#include "ecif_def.h"
#include "ecif_model.h"
#include "ecif_renderer.h"
#include "ecif_userinterface.h"


struct emf_ObjectData_X;

class Control
{
public:
  Control(Hinst app_name, UserInterface* u);
  void activateUI();
  void copyParameters(char* emf_filename);
  Input* create_mesh_input( enum ecif_modelDimension m_dim, ifstream& in_file, char* mesh_filename);
  void deactivateUI();
  void displayModel();
  void Exit();
  bool getBreakValue(enum frontProcessType process);
  bool getColorName(int id, char* buffer) {return theModel->getColorName(id, buffer);}
  UserInterface* getGui() {return theUI;}
  void getCurrentTimestamp(char* buffer);
  Model* getModel() {return theModel;}
  ParameterFieldInfo* getParameterFieldInfo(char* key, bool conver_to_lower = true);
  Process* getProcess(int process_nbr);
  UserInterface* getUI() {return theUI;}
  Renderer* getRenderer() {return theRenderer;}
  void handleKeyAction(enum keyAction action);
  iostream& print_progress_info(iostream& strm, int nbr, int total_nbr, char* text);
  bool processExists(int process_nbr);
  bool processResume(int process_nbr);
  bool processSetPriorityLevel(int process_nbr, priorityLevel priority);
  bool processStart(Process* process);
  bool processStop(int process_nbr);
  bool processSuspend(int process_nbr);
  bool readCADFile(char* CAD_filename,
                   char* CAD_type = NULL,
                   ecif_modelDimension m_dim = ECIF_ND);
  bool readMeshFile(char* mesh_filename,
                    char* mesh_type = NULL,
                    bool create_new_model = true,
                    enum ecif_modelDimension m_dim = ECIF_ND);
  bool readModelFile(char* model_filename, bool load_mesh, bool is_batch = false);
  void rendererIsClosed();
  void setModelDimension(enum ecif_modelDimension model_dim);
  void saveElmerMeshFile(char* mesh_dir);
  void saveElmerPostMeshFile(char* out_filename);
  void saveFrontModelFile(char* out_filename);
  void saveMeshInputFile(char* out_filename);
  void saveSolverInputFile(char* out_filename);
  void saveThetisMeshFile(char* out_filename);
  void saveUserSettingsFile(char* out_filename);
  void selectBody(int bd_id, int lr_id, bool update_gui = true);
  void selectBoundary(int elem_id,
                      int body1_id, int layer1_id,
                      int body2_id, int layer2_id,
                      bool accept_body_change = false, bool update_gui = true);
  void selectBoundaries(int nof_elems, int* elem_ids,
                        int* body1_ids, int* lr1_ids,
                        int* body2_ids, int* lr2_ids,
                        bool accept_body_change = false, bool update_gui = true);
  void setBreakValue(enum frontProcessType process, bool value);
  void setGuiWindowTitle(char* case_name);
  void setRendererWindowTitle(char* case_name);
  void setWindowTitles(char* case_name);
  int unknownFieldMsg(emf_ObjectData_X* object_data, bool is_fatal);
  int unknownObjectMsg(emf_ObjectData_X* object_data, bool is_fatal);
  void update(int counter, int update_interval);
  void updateUI();
  bool write_ok(ofstream& out_file, char* out_filename = NULL, char* msg = NULL);

protected:
  void updatePreUI();
  void updatePostUI();

  void createRenderer(enum ecif_modelDimension m_dim, bool display_gmtr = true);

  Hinst appInstance;
  bool breakEgfInput;
  bool breakEgfOutput;
  bool breakEmfInput;
  bool breakEmfOutput;
  bool breakMeshInput;
  bool breakMeshOutput;
  Model* theModel;
  ProcessTable* processTable;
  Renderer* theRenderer;
  int resetRenderer;
  UserInterface* theUI;
};

#endif
