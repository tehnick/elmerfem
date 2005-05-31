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
 * Visual class definitions.
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
 *                       Date: 27 Sep 1995
 *
 *
 * Modification history:
 *
 * 28 Sep 1995, modified visual_t and visual_type_t structures to make a list
 *              rather than an array of visual types.
 *
 ******************************************************************************/

#ifdef EXT
#undef EXT
#endif

#ifdef MODULE_VISUALS
#define EXT 
#else
#define EXT extern
#endif

#ifdef MODULE_VISUALS
    double TooLong1 = 0.30,TooLong2 = 0.5;
#else
    extern double TooLong1,TooLong2;
#endif

#define VIS_VISUAL_PARAM_LOGICAL 1
#define VIS_VISUAL_PARAM_INT     2
#define VIS_VISUAL_PARAM_FLOAT   4
#define VIS_VISUAL_PARAM_POINTER 8

typedef struct visual_param_s
{
   /*
    * Name string for the parameter
    */
   char *Name;

   /*
    * Format string for scanf when scanning parameter value
    */
   char *Format;
  
   /*
    * Parameter offset from structure beginnig
    */
   long int Offset;

   /*
    *  type of the parameter (one of VIS_VISUAL_PARAM_*)
    */
   int   ParamType;

   /*
    * Initial values for the parameters
    */
   int IntValue;
   double FloatValue;
   void *PointerValue; 
} visual_param_t;

typedef struct visual_type_s
{
    /*
     * Next visual type in the list
    */
    struct visual_type_s *Next;

    /*
     * One line description of the visual
     */
    char *VisualName;

    /*
     *  Alloc memory for parameters
     */
    void *(*AllocParams)();

    /*
     * Dispose a visual
     */
    void (*DeleteParams)(void *);

    /*
     * Realize a visual instance
     */
    int (*RealizeVisual)(geometry_t *,element_model_t *,void *,double);

    /*
     * Structure holding names and offsets of particular
     * visual types parameters
     */
    visual_param_t *VisualParams;
} visual_type_t;

/*
 *  visual type list (remove this)
 */
typedef struct visual_defs_s
{
    int NumberOfVisualTypes;
    visual_type_t *VisualTypes;
} visual_defs_t;

/*
 * Global variable holding start of the list of visual types
 */
EXT visual_defs_t VisualDefs;

typedef struct visual_s
{
    /*
     *  Next visual in list
     */
    struct visual_s *Next;

    /*
     * Name of the visual instance
     */
    char *Name;

   /*
    * this  is pointer to arrow_t,mesh_t etc. structures, see below
    */
    void *VisualParams;

    /*
     * Pointer to visual type structure holding the function pointers
     * acting on this type of visual 
     */

    visual_type_t *VisualType;
} visual_t;

typedef enum
{
    mesh_style_none,mesh_style_line,mesh_style_surf,mesh_style_line_and_surf
} mesh_style_t;

typedef enum
{
    arrow_style_stick,arrow_style_arrow
} arrow_style_t;

typedef enum
{
    particle_style_vector,particle_style_sphere
} particle_style_t;

typedef enum
{
    particle_integ_euler, particle_integ_runge_kutta
} particle_integ_method_t;

typedef enum
{
    particle_policy_fixed, particle_policy_adaptive
} particle_integ_policy_t;

