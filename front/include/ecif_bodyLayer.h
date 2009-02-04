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
Module:     ecif_bodyLayer.h
Language:   C++
Date:       01.01.03
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for model's bodiesLayer.
            BodyLayer are subparts of a body and they are
            used in meshing only

************************************************************************/

#ifndef _ECIF_BODYLAYER_
#define _ECIF_BODYLAYER_

#include "ecif_def.h"
#include "ecif_def_stl.h"
#include "ecif_def_trx.h"
#include "ecif_modelObject.h"


class BodyLayer : public ModelObject
{
friend class Control;
friend class Model;
public:
  BodyLayer();
  BodyLayer(ecif_BodyLayer_X& trx_bl);
  virtual ~BodyLayer();
  bool acceptsStructuredMesh();
  const Body* getBody();
  const int getBodyId() { return bodyId; }
  const int getBodyTag() { return bodyTag; }
  void getColor(Color4& clr);
  const int* getExcludedMeshIndices() { return excludedMeshIndices; }
  const int* getGridParameterMeshIndices() { return gridParameterMeshIndices; }
  int getGridParameterId(int mesh_index);
  const int* getGridParameterIds() { return gridParameterIds; }
  bool getMeshDensityValue(int mesh_index, char& type, double& value);
  int getMeshQuadGridN(int mesh_index, int element_id);
  int getNofExcludedMeshes() { return nofExcludedMeshes; }
  int getNofGridParameterIds() { return nofGridParameterIds; }
  int getNofMifLayers(const IdList* elem_loop_ids);
  int getNofMifLayerLoops(const IdList* elem_loop_ids);
  int getNofMifLayerLoops(int gmtr_index, const IdList* elem_loop_ids);
  enum bodyLayerType getLayerType() const {return type;}
  bool hasBody(int bd_id);
  void initName();
  static void initClass(Model* model);
  bool isExcludedFromMesh(int mesh_index);
  virtual bool isClosed() { return tplgType == CLOSED_LAYER; }
  virtual bool isOpen() { return tplgType == OPEN_LAYER; }
  virtual ostream& output_emf(ostream& out, short indent_size, short indent_level);
  virtual ostream& output_mif(ostream& out, int& next_mif_id, const IdList* elem_loop_ids);
  void setBodyId(int body_id);
  void setBodyTag(int body_tag);
  void setColorIndex(colorIndices color_index);
  void setExcludedMeshData(int nof_ids, int* excluded_mesh_indices);
  void setGridParameterData(int nof_ids, int* gids, int* mesh_indices);
  void setGridParameterIds(int nof_ids, int* gids);
  void setGridParameterMeshIndices(int nof_ids, int* mesh_indices);
  void setLayerType(enum bodyLayerType tp) {type = tp;}
  void setTplgType(bodyLayerTplgType value) { tplgType = value; };

protected:
  void init();

  static int last_tag;
  int bodyId;
  int bodyTag;
  Color4 color;
  colorIndices colorIndex;
  int* excludedMeshIndices;
  int* gridParameterMeshIndices;
  int* gridParameterIds;
  int nofExcludedMeshes;
  int nofGridParameterIds;
  enum bodyLayerType type;
  enum bodyLayerTplgType tplgType;
};

#endif
