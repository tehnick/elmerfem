/*******************************************************************************
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
 ******************************************************************************/

/*******************************************************************************
 *
 *     The character of the routines in this file.
 *
 *******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02101 Espoo, Finland
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 1 Oct 1995
 *
 * Modification history:
 *
 ******************************************************************************/

#ifdef MODULE_OBJECTS
#   define OBJ_EXT
#else
#   define OBJ_EXT extern
#endif

typedef double matrix_t[4][4];

typedef enum
{
    rot_pri_xyz,
    rot_pri_xzy,
    rot_pri_yxz,
    rot_pri_yzx,
    rot_pri_zxy,
    rot_pri_zyx,
    rot_pri_local,
    rot_pri_parent
} rot_pri_t;

typedef enum
{
    trn_pri_trs,
    trn_pri_tsr,
    trn_pri_rts,
    trn_pri_rst,
    trn_pri_str,
    trn_pri_srt
} trn_pri_t;

typedef struct transform_s
{
    struct transform_list_s *Children;
    struct transform_s *Parent;

    matrix_t Matrix;

    matrix_t RotMatrix;
    matrix_t TrnMatrix;
    matrix_t SclMatrix;

    double RotX,RotY,RotZ;
    double TrnX,TrnY,TrnZ;
    double SclX,SclY,SclZ;

    rot_pri_t RotationPriority;
    trn_pri_t TransformPriority;
} transform_t;

typedef struct transform_list_s
{
    struct transform_list_s *Next;
    transform_t *Entry;
} transform_list_t;

typedef struct object_s
{
    struct object_s *Next;
    int Id;
    char *Name;

    transform_t Transform;

    element_model_t *ElementModel;

    geometry_t *Geometry;

    int ClipPlane[6];
    double ClipEquation[6][4];

    struct visual_s *VisualList;
} object_t;

OBJ_EXT object_t VisualObject,RotObject,*CurrentObject;
OBJ_EXT double PiDiv180;
