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
 * Definition of 6 node wedge element.
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
 *                       Date: 4 Oct 1995
 *
 ******************************************************************************/

#include "../elmerpost.h"
#include <elements.h>

/*
 * Six node wedge volume element.
 *
 *                  5-----------4    
 *                 /|          /|          w  v
 *                / |        /  |          | /
 *               /  |      /    |          |/
 *              /   2----/------1          ---u
 *             /   /    /     //
 *            /   /  ///   //
 *           /    //     //
 *          /  //     //
 *         3/  /   //
 *         |  /   //
 *         | / //
 *         |//
 *         0
 */

static double A[6][6],N[6][6];

static double NodeU[] = {  0.0, 1.0, 0.0, 0.0, 1.0, 0.0 };
static double NodeV[] = {  0.0, 0.0, 1.0, 0.0, 0.0, 1.0 };
static double NodeW[] = { -1.0,-1.0,-1.0, 1.0, 1.0, 1.0 };

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_triangulate
 *
 *     Purpose:     Triangulate an elment. The process also builds up an edge
 *                  table and adds new nodes to node table. The triangulation
 *                  and edge table is stored in geometry_t *geom-structure.
 *
 *     Parameters:
 *
 *         Input:   (geometry_t *) pointer to structure holding triangulation
 *                  (element_t  *) element to triangulate
 *
 *         Output:  (geometry_t *) structure is modified
 *
 *   Return value:  FALSE if malloc() fails, TRUE otherwise
 *
 ******************************************************************************/
static int elm_6node_wedge_triangulate( geometry_t *geom,element_t *wedge )
{
    element_t element;
    int i,j;


    if ( GlobalOptions.VolumeSides )
    {
       int topo[4];

       element.DisplayFlag = TRUE;
       element.Topology = topo;
       for( i=0; i<MAX_GROUP_IDS; i++ ) element.GroupIds[i] = wedge->GroupIds[i];
   
       for( i=0; i<3; i++ )
       {
           for( j=0; j<4; j++ )
           {
               element.Topology[j] = wedge->Topology[ElmWedgeFace[i][j]];
           }
           if ( !elm_4node_quad_triangulate( geom, &element, wedge ) ) return FALSE;
       }

       for( i=3; i<5; i++ )
       {
           for( j=0; j<3; j++ )
           {
               element.Topology[j] = wedge->Topology[ElmWedgeFace[i][j]];
           }
           if ( !elm_3node_triangle_triangulate( geom, &element, wedge ) ) return FALSE;
       }
    } else {
       if ( !geo_add_edge( geom, wedge->Topology[0], wedge->Topology[1],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[0], wedge->Topology[2],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[0], wedge->Topology[3],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[1], wedge->Topology[2],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[1], wedge->Topology[4],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[2], wedge->Topology[5],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[3], wedge->Topology[4],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[3], wedge->Topology[5],wedge ) ) return FALSE;
       if ( !geo_add_edge( geom, wedge->Topology[4], wedge->Topology[5],wedge ) ) return FALSE;
    }

    return TRUE;
}

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_shape_functions
 *
 *     Purpose:     Initialize element shape function array. Internal only.
 *
 *     Parameters:
 *
 *         Input:   Global (filewise) variables NodeU,NodeV,NodeW
 *
 *         Output:  Global (filewise) variable N[6][6], will contain
 *                  shape function coefficients
 *
 *   Return value:  void
 *   Return value:  void
 *
 ******************************************************************************/
static void elm_6node_wedge_shape_functions()
{
     double u,v,w;
     int i,j;

     for( i=0; i<6; i++ )
     {
         u = NodeU[i];
         v = NodeV[i];
         w = NodeW[i];

         A[i][0]   = 1;
         A[i][1]   = u;
         A[i][2]   = v;
         A[i][3]   = w;
         A[i][4]   = u*w;
         A[i][5]   = v*w;
     }

     lu_mtrinv( (double *)A,6 );

     for( i=0; i<6; i++ )
        for( j=0; j<6; j++ ) N[i][j] = A[j][i];
}

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_fvalue
 *
 *     Purpose:     return value of a quantity given on nodes at point (u,v)
 *                  Use trough (element_type_t *) structure.
 *
 *     Parameters:
 *
 *         Input:  (double *) quantity values at nodes 
 *                 (double u,double v) point where values are evaluated
 *
 *         Output:  none
 *
 *   Return value:  quantity value
 *
 ******************************************************************************/
static double elm_6node_wedge_fvalue(double *F,double u,double v,double w)
{
     double R=0.0,uw=u*w,vw=v*w;
     int i;

     for( i=0; i<6; i++ )
     {
         R += F[i]*(N[i][0]+
                    N[i][1]*u+
                    N[i][2]*v+
                    N[i][3]*w+
                    N[i][4]*uw+
                    N[i][5]*vw);
     }

     return R;
}

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_dndu_fvalue
 *
 *     Purpose:     return value of a first partial derivate in (v) of a
 *                  quantity given on nodes at point (u,v).
 *                  Use trough (element_type_t *) structure.
 *                 
 *
 *     Parameters:
 *
 *         Input:  (double *) quantity values at nodes 
 *                 (double u,double v,double w) point where values are evaluated
 *
 *         Output:  none
 *
 *   Return value:  quantity value
 *
 ******************************************************************************/
static double elm_6node_wedge_dndu_fvalue(double *F,double u,double v,double w)
{
     double R=0.0;
     int i;

     for( i=0; i<6; i++ )
     {
         R += F[i]*( N[i][1] + N[i][4]*w );
     }

     return R;
}

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_dndv_fvalue
 *
 *     Purpose:     return value of a first partial derivate in (v) of a
 *                  quantity given on nodes at point (u,v,w).
 *                  Use trough (element_type_t *) structure.
 *                 
 *
 *     Parameters:
 *
 *         Input:  (double *) quantity values at nodes 
 *                 (double u,double v,double w) point where values are evaluated
 *
 *         Output:  none
 *
 *   Return value:  quantity value
 *
 ******************************************************************************/
static double elm_6node_wedge_dndv_fvalue(double *F,double u,double v,double w)
{
     double R=0.0;
     int i;

     for( i=0; i<6; i++ )
     {
         R += F[i]*( N[i][2] + N[i][5]*w );
     }

     return R;
}

/*******************************************************************************
 *
 *     Name:        elm_6node_wedge_dndw_fvalue
 *
 *     Purpose:     return value of a first partial derivate in (w) of a
 *                  quantity given on nodes at point (u,v,w)
 *                  Use trough (element_type_t *) structure.
 *                 
 *
 *     Parameters:
 *
 *         Input:  (double *) quantity values at nodes 
 *                 (double u,double v,double w) point where values are evaluated
 *
 *         Output:  none
 *
 *   Return value:  quantity value
 *
 ******************************************************************************/
static double elm_6node_wedge_dndw_fvalue(double *F,double u,double v,double w)
{
     double R=0.0;
     int i;

     for( i=0; i<6; i++ )
     {
         R += F[i]*( N[i][3] + N[i][4]*u + N[i][5]*v );
     }

     return R;
}

/******************************************************************************
 *
 *     Name:        elm_6node_wedge_initialize
 *
 *     Purpose:     Register the element type
 *                  
 *     Parameters:
 *
 *         Input:  (char *) description of the element
 *                 (int)    numeric code for the element
 *
 *         Output:  Global list of element types is modfied
 *
 *   Return value:  malloc() success
 *
 ******************************************************************************/
int elm_6node_wedge_initialize()
{
     static char *Name = "ELM_6NODE_WEDGE";

     element_type_t ElementDef;

     elm_6node_wedge_shape_functions();

     ElementDef.ElementName = Name;
     ElementDef.ElementCode = 706;

     ElementDef.NumberOfNodes = 6;

     ElementDef.NodeU = NodeU;
     ElementDef.NodeV = NodeV;
     ElementDef.NodeW = NodeW;

     ElementDef.PartialU = (double (*)())elm_6node_wedge_dndu_fvalue;
     ElementDef.PartialV = (double (*)())elm_6node_wedge_dndv_fvalue;
     ElementDef.PartialW = (double (*)())elm_6node_wedge_dndw_fvalue;

     ElementDef.FunctionValue = (double (*)())elm_6node_wedge_fvalue;
     ElementDef.Triangulate   = (int (*)())elm_6node_wedge_triangulate;
     ElementDef.PointInside   = (int (*)())NULL;

     return elm_add_element_type( &ElementDef );
}
