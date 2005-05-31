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
 * Type & structure definitions for objects & geometry. This is really the
 * definition of the structure of ElmerPost.
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
 *                       Date: 26 Sep 1995
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 ******************************************************************************/

#ifdef MODULE_GEOMETRY
#   define GEO_EXT
#else
#   define GEO_EXT extern
#endif

#define GEO_TRIANGLE_BLOCK_SIZE 4096
#define GEO_VERTEX_BLOCK_SIZE   4096

#define FLOAT float

typedef struct group_s
{
    struct group_s *Next;
    int status,Open;
    char *Name;
} group_t;

/*
 * Triangle
 */
typedef struct
{
    int   v[3];    /* vertex pointers */
    FLOAT Fu[3];   /* triangle normal */
    FLOAT u[3][3]; /* vertex normals  */

    struct element_s *Element;

    int Count;
    logical_t Edge[3];  /* beginning of an edge flag */
} triangle_t;

/*
 * list of faces connected to a vertex
 */
typedef struct vertex_face_s
{
    int Face;
    struct vertex_face_s *Next;
} vertex_face_t;

/*
 *  vertex def's
 */
typedef struct vertex_s
{
    FLOAT x[3];
    vertex_face_t *Faces;
    logical_t ElementModelNode;
} vertex_t;

typedef struct
{
    FLOAT x[3],y[3],z[3];
    FLOAT u[3],v[3],w[3];
    FLOAT c[3],f[3];
} polygon_t;

typedef enum
{
    line_style_line,line_style_cylinder
} line_style_t;

typedef struct line_s
{
    float x[3];
    float y[3];
    float z[3];
    float f[3],c[3];
} line_t;

/*
 * Edges of elements are hold in an array of lists that are
 * indexed by smallest numbered vertex of a particular edge.
 */
typedef enum
{
    edge_style_all, edge_style_free
} edge_style_t;

typedef struct edge_list_s
{
    struct edge_list_s *Next;
    int Entry,Count;
    struct element_s *Element;
} edge_list_t;
    
typedef struct edge_s
{
     edge_list_t *EdgeList; 
} edge_t;

/*
 *  geometry def's
 */
typedef struct geometry_s
{
    struct Geometry_s *Next;
    char *Name;

    triangle_t *Triangles;
    int TriangleCount,MaxTriangleCount;

    vertex_t *Vertices;
    int VertexCount,MaxVertexCount;

    edge_t *Edges;

    double Scale;
    vertex_t MinMax[2];
} geometry_t;

GEO_EXT geometry_t Geometry;

typedef struct data_s
{
    int a;    
} data_t;
