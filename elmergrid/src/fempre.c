/*  
   ElmerGrid - A simple mesh generation and manipulation utility  
   Copyright (C) 1995- , CSC - Scientific Computing Ltd.   

   Author: Peter Råback
   Email: Peter.Raback@csc.fi
   Address: CSC - Scientific Computing Ltd.
            Keilaranta 14
            02101 Espoo, Finland

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

/****************************************************************************
*                                                                           *  
*                              Elmergrid                                    *
*                                                                           *
*  This program creates very easily a structured 2D meshes with BCs.        *
*  The element types include 4, 5, 8, 9, 12 and 16-node rectangles and      *
*  3, 6 and 10-node triangles. There is also limited 3D functionality       *
*  with 8, 20 and 27-node cubes and 6-node prisms.                          *
*                                                                           *
*  The program may also be used as a mesh import and export utility. It     *
*  is able to read several different formats and writes mainly Elmer input  *
*  and output formats. The meshes may also be given some simple operations. *
*                                                                           *
*  Note: this software was initially part of my first fem implementation    *
*  the Pirfem code, then later called Quickmesh, and finally renamed to     *
*  Elmergrid. The code has never been designed and with new features the    *
*  code has eventually become very dirty and does not present my view of    *
*  good programming.                                                        *
*                                                                           *
****************************************************************************/


#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "common.h"
#include "nrutil.h"
#include "femdef.h"
#include "femtypes.h"
#include "femmesh.h"
#include "femknot.h"
#include "feminfo.h"
#include "femelmer.h"
#include "femfilein.h"
#include "femfileout.h"
#include "femfact.h"


static void Instructions()
{
  printf("****************** Elmergrid ************************\n");
  printf("This program can create simple 2D structured meshes consisting of\n");
  printf("linear, quadratic or cubic rectangles or triangles. The meshes may\n");
  printf("also be extruded and revolved to create 3D forms. In addition many\n");
  printf("mesh formats may be imported into Elmer software. Some options have\n");
  printf("not been properly tested. Contact the author if you face problems.\n\n");

  printf("The program has two operation modes\n");
  printf("A) Command file mode which has the command file as the only argument\n");
  printf("   'ElmerGrid commandfile.eg'\n\n");

  printf("B) Inline mode which expects at least three input parameters\n");
  printf("   'ElmerGrid 1 3 test'\n\n");
  printf("The first parameter defines the input file format:\n");
  printf("1)  .grd      : Elmergrid file format\n");
  printf("2)  .mesh.*   : Elmer input format\n");
  printf("3)  .ep       : Elmer output format\n");
  printf("4)  .ansys    : Ansys input format\n");
  printf("5)  .inp      : Abaqus input format by Ideas\n");
  printf("6)  .fil      : Abaqus output format\n");
  printf("7)  .FDNEUT   : Gambit (Fidap) neutral file\n");
  printf("8)  .unv      : Universal mesh file format\n");
  if(0) printf("8)  .d        : Easymesh input format\n");
  printf("9)  .mphtxt   : Comsol Multiphysics mesh format\n");
  printf("10) .dat      : Fieldview format\n");
  printf("11) .node,.ele: Triangle 2D mesh format\n");
  printf("12) .mesh     : Medit mesh format\n");
  printf("13) .msh      : GID mesh format\n");
  printf("14) .msh      : Gmsh mesh format\n");
  printf("15) .ep.i     : Partitioned ElmerPost format\n");
#if 0
  printf("16) .unv      : Universal mesh file format\n");
  printf("17) .msh      : Nastran format\n");
#endif 

  printf("\nThe second parameter defines the output file format:\n");
  printf("1)  .grd      : ElmerGrid file format\n");
  printf("2)  .mesh.*   : ElmerSolver format (also partitioned .part format)\n");
  printf("3)  .ep       : ElmerPost format\n");
#if 0
  printf("5)  .inp      : Abaqus input format\n");
  printf("7)  .fidap    : Fidap format\n");
  if(0) printf("8)  .n .e .s  : Easymesh output format\n");
  printf("18) .ep       : Fastcap input format.\n");
#endif

  printf("\nThe third parameter is the name of the input file.\n");
  printf("If the file does not exist, an example with the same name is created.\n");
  printf("The default output file name is the same with a different suffix.\n\n");

  printf("There are several additional in-line parameters that are\n");
  printf("taken into account only when applicable to the given format.\n");

  printf("-out str             : name of the output file\n");
  printf("-in str              : name of a secondary input file\n");
  printf("-decimals            : number of decimals in the saved mesh (eg. 8)\n");
  printf("-triangles           : rectangles will be divided to triangles\n");
  printf("-merge real          : merges nodes that are close to each other\n");
  printf("-order real[3]       : reorder elements and nodes using c1*x+c2*y+c3*z\n");
  printf("-centralize          : set the center of the mesh to origin\n");
  printf("-scale real[3]       : scale the coordinates with vector real[3]\n");
  printf("-translate real[3]   : translate the nodes with vector real[3]\n");
  printf("-rotate real[3]      : rotate around the main axis with angles real[3]\n");
  printf("-clone int[3]        : make ideantilcal copies of the mesh\n");
  printf("-clonesize real[3]   : the size of the mesh to be cloned if larger to the original\n");
  printf("-mirror int[3]       : copy the mesh around the origin in coordinate directions\n");
  printf("-unite               : the meshes will be united\n");
  printf("-polar real          : map 2D mesh to a cylindrical shell with given radius\n");
  printf("-cylinder            : map 2D/3D cylindrical mesh to a cartesian mesh\n");
  printf("-reduce int[2]       : reduce element order at material interval [int1 int2]\n");
  printf("-increase            : increase element order from linear to quadratic\n");
  printf("-bcoffset int        : add an offset to the boundary conditions\n");
  printf("-discont int         : make the boundary to have secondary nodes\n");
  printf("-connect int         : make the boundary to have internal connection among its elements\n");
  printf("-removelowdim        : remove boundaries that are two ranks lower than highest dim\n");
  printf("-removeunused        : remove nodes that are not used in any element\n");
  printf("-bulkorder           : renumber materials types from 1 so that every number is used\n");
  printf("-boundorder          : renumber boundary types from 1 so that every number is used\n");
  printf("-autoclean           : this performs the united action of the three above\n");
  printf("-bulkbound int[3]    : set the union of materials [int1 int2] to be boundary int3\n");
  printf("-boundbound int[3]   : set the union of boundaries [int1 int2] to be boundary int3\n");
  printf("-bulktype int[3]     : set material types in interval [int1 int2] to type int3\n");
  printf("-boundtype int[3]    : set sidetypes in interval [int1 int2] to type int3\n");
  printf("-layer int[2] real[2]: make a boundary layer for given boundary\n");
  printf("-layermove int       : apply Jacobi filter int times to move the layered mesh\n");
  printf("-divlayer int[2] real[2]: make a boundary layer for given boundary\n");
  printf("-3d / -2d / -1d      : mesh is 3, 2 or 1-dimensional (applies to examples)\n");
  printf("-isoparam            : ensure that higher order elements are convex\n");
  printf("-nobound             : disable saving of boundary elements in ElmerPost format\n");

  printf("\nThe following keywords are related only to the parallel Elmer computations.\n");
  printf("-partition int[4]    : the mesh will be partitioned in main directions\n");
  printf("-partorder real[3]   : in the above method, the direction of the ordering\n");
#if PARTMETIS
  printf("-metis int[2]        : the mesh will be partitioned with Metis\n");
#endif
  printf("-halo                : create halo for the partitioning\n");
  printf("-indirect            : create indirect connections in the partitioning\n");
  printf("-periodic int[3]     : decleare the periodic coordinate directions for parallel meshes\n");
  printf("-partjoin int        : number of partitions in the data to be joined\n");
  printf("-saveinterval int[3] : the first, last and step for fusing parallel data\n");
  printf("-partorder real[3]   : in the above method, the direction of the ordering\n");

  if(0) printf("-names               : conserve name information where applicable\n");
#if 0
  printf("-map str             : file with mapping info for mesh-to-mesh interpolation\n");
#endif
#if 0
  /* This functionality has moved into the ElmerSolver */
  printf("-pelem int[3]        : p-elements of power int3 at interval [int1 int2]\n");
  printf("-belem int[3]        : set bubble dofs to int3 at interval [int1 int2]\n");
#endif

}


static void Goodbye()
{
  printf("\nThank you for using Elmergrid!\n");
  printf("Send bug reports and feature wishes to peter.raback@csc.fi\n");
  exit(0);
}



int main(int argc, char *argv[])
{
  int i,j,k,l,inmethod,outmethod,info,errorstat,sides;
  int nogrids,nomeshes,nofile,dim,elementsredone=0;
  int nodes3d,elements3d,showmem;
  Real mergeeps;
  char prefix[MAXFILESIZE];
  struct GridType *grids;
  struct CellType *cell[MAXCASES];
  struct FemType data[MAXCASES];
  struct BoundaryType *boundaries[MAXCASES];
  struct ElmergridType eg;
  long ii;

  showmem = TRUE;
  printf("\nStarting program Elmergrid\n");

  InitParameters(&eg);
  grids = (struct GridType*)malloc((size_t) (MAXCASES)*sizeof(struct GridType));     
  InitGrid(grids);
  info = TRUE;

  if(argc <= 1) {
    errorstat = LoadCommands(argv[1],&eg,grids,argc-1,info);     
    Instructions();
    if(errorstat) Goodbye();
  }
  if(argc == 2) {
    errorstat = LoadCommands(argv[1],&eg,grids,argc-1,info);     
    if(errorstat) Goodbye();
  }
  else if(argc < 4) {
    Instructions();
    Goodbye();
  } 
  else {
    errorstat = InlineParameters(&eg,argc,argv);
    if(errorstat) Goodbye();
  }


  if(!eg.outmethod || !eg.inmethod) {
    printf("Please define the input and output formats\n");
  }
  if(eg.inmethod != 1) {
    if(eg.outmethod == 1 || eg.outmethod == 8 || eg.outmethod == 9 || eg.outmethod == 10) {
      printf("input of type %d can't create output of type %d\n",
	     eg.inmethod,eg.outmethod);
      errorstat++;
      Goodbye();
    }
  }
#if 0
  if(eg.inmethod != 8 && eg.outmethod == 5) {
    printf("To write Easymesh format you need to read easymesh format!\n");
    errorstat++;
  }
#endif


  /**********************************/
  printf("\nElmergrid loading data:\n");
  printf(  "-----------------------\n");

  dim = eg.dim;
  nofile = 0;
  nomeshes = 0;
  nogrids = 0;
  inmethod = eg.inmethod;
  outmethod = eg.outmethod;


 read_another_file:    

  switch (inmethod) {

  case 1:        
    if(LoadElmergrid(&grids,&nogrids,eg.filesin[nofile],info) == 1) {   
      if(dim == 3) ExampleGrid3D(&grids,&nogrids,info);
      if(dim == 2) ExampleGrid2D(&grids,&nogrids,info);
      if(dim == 1) ExampleGrid1D(&grids,&nogrids,info);
      SaveElmergrid(grids,nogrids,eg.filesin[nofile],info); 
      printf("Because file %s didn't exist, it was created for you.\n",eg.filesin[nofile]);
      Goodbye();
    }
    LoadCommands(eg.filesin[nofile],&eg,grids,2,info); 
    break;

  case 2: 
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if(LoadElmerInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],info))
      Goodbye();
    nomeshes++;
    break;

  case 3: 
    if(LoadSolutionElmer(&(data[nofile]),TRUE,eg.filesin[nofile],info)) 
      Goodbye();
    nomeshes++;
    break;

  case 4:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    if(LoadAnsysInput(&(data[0]),boundaries[0],eg.filesin[nofile],info)) 
      Goodbye();
    nomeshes++;
    break;

  case 5: 
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if(LoadAbaqusInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE)) 
      Goodbye();
    nomeshes++;
    break;

  case 6:
    if(LoadAbaqusOutput(&(data[nofile]),eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;

  case 7:
    if(LoadFidapInput(&(data[nofile]),eg.filesin[nofile],TRUE))
      Goodbye();
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if(0 && !eg.usenames) data[nofile].boundarynamesexist = data[nofile].bodynamesexist = FALSE;
    ElementsToBoundaryConditions(&(data[nofile]),boundaries[nofile],TRUE);
    RenumberBoundaryTypes(&data[nofile],boundaries[nofile],TRUE,0,info);
  
    nomeshes++;
    break;

#if 0
  case 8: 
    InitializeKnots(&(data[nofile]));
    if( Easymesh(argc,argv,&data[nofile].noknots,
		 &data[nofile].noelements,&sides)) 
      Goodbye();	
    
    data[nofile].dim = 2;
    data[nofile].coordsystem = COORD_CART2;
    data[nofile].maxnodes = 3;
    
    AllocateKnots(&(data[nofile]));
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if(EasymeshCopy(&(data[nofile]),boundaries[nofile]))
      Goodbye();    
    nomeshes++;
    break;
#endif

 case 9:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
   
    if(LoadComsolMesh(&(data[nofile]),eg.filesin[nofile],info)) {
      printf("\n***********************************************************************************\n");
      printf("The reading of Comsol mesh file seems to have failed\n");
      printf("Trying out a previous version that requires the use of savemesh.m utility in Matlab\n");
      printf("The recommended way to export meshes from Femlab to Elmer is the .mphtxt format\n"); 
      printf("***********************************************************************************\n\n");

      if(LoadFemlabMesh(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],info)) 
	Goodbye();
    }
    ElementsToBoundaryConditions(&(data[nofile]),boundaries[nofile],TRUE);
    nomeshes++;
    break;

  case 10:
    if(LoadFieldviewInput(&(data[nofile]),eg.filesin[nofile],TRUE))
      Goodbye();
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	    
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    ElementsToBoundaryConditions(&(data[nofile]),boundaries[nofile],TRUE);
    nomeshes++;
    break;

  case 11:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadTriangleInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;

  case 12:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadMeditInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;

  case 13:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadGidInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;

  case 14:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadGmshInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;

  case 15: 
    if(info) printf("Partitioned solution is fused on-the-fly therefore no other operations may be performed.\n");
    FuseSolutionElmerPartitioned(eg.filesin[nofile],eg.filesout[nofile],eg.decimals,eg.partjoin,
				 eg.saveinterval[0],eg.saveinterval[1],eg.saveinterval[2],info);
    if(info) printf("Finishing with the fusion of partitioned Elmer solutions\n");
    Goodbye();
    break;

  case 8:
  case 16:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadUniversalMesh(&(data[nofile]),eg.filesin[nofile],TRUE))
      Goodbye();
    if(1) ElementsToBoundaryConditions(&(data[nofile]),boundaries[nofile],TRUE);
    nomeshes++;
    break;

  case 17:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
    if (LoadNastranInput(&(data[nofile]),boundaries[nofile],eg.filesin[nofile],TRUE))
      Goodbye();
    nomeshes++;
    break;


  case 18:
    boundaries[nofile] = (struct BoundaryType*)
      malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
    for(i=0;i<MAXBOUNDARIES;i++) {
      boundaries[nofile][i].created = FALSE; 
      boundaries[nofile][i].nosides = 0;
    }
   
    if(LoadCGsimMesh(&(data[nofile]),eg.filesin[nofile],info))
       Goodbye();
    nomeshes++;
    break;


  default:
    Instructions();
    Goodbye();
  }  

  nofile++;
  if(nofile < eg.nofilesin) {
    printf("\nElmergrid loading data from another file:\n");
    goto read_another_file;
  }

  /***********************************/


 redoelements:

  printf("\nElmergrid creating and manipulating meshes:\n");
  printf(  "-------------------------------------------\n");

  if(nogrids > nomeshes && outmethod != 1) { 

    nomeshes = nogrids;
    for(k=0;k<nogrids;k++) {

      CreateCells(&(grids[k]),&(cell[k]),info);  
      CreateKnots(&(grids[k]),cell[k],&(data[k]),0,0);

      boundaries[k] = (struct BoundaryType*)
	malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 

      for(j=0;j<MAXBOUNDARIES;j++) {
	boundaries[k][j].created = FALSE;
	boundaries[k][j].nosides = FALSE;
      }

      if(grids[k].noboundaries > 0) {
	for(j=0;j<grids[k].noboundaries;j++) {
	  if(grids[k].boundsolid[j] < 4) {
	    CreateBoundary(cell[k],&(data[k]),&(boundaries[k][j]),
			   grids[k].boundext[j],grids[k].boundint[j],
			   1,grids[k].boundtype[j]);  
	  } 
	  else { 
	    CreatePoints(cell[k],&(data[k]),&(boundaries[k][j]),
			 grids[k].boundext[j],grids[k].boundint[j],
			 grids[k].boundsolid[j],grids[k].boundtype[j]); 	    
	  }
	}
      }
    }
  }

  /* Make the discontinous boundary needed, for example, in poor thermal conduction */
  for(k=0;k<nomeshes;k++) {
    if(!eg.discont) {
      for(j=0;j<grids[k].noboundaries;j++) 
	if(grids[k].boundsolid[j] == 2) {
	  eg.discontbounds[eg.discont] = grids[k].boundtype[j];
	  eg.discont++;	  
	}
    }
    if(eg.discont) {
      for(i=1;i<=eg.discont;i++) 
	SetDiscontinuousBoundary(&(data[k]),boundaries[k],eg.discontbounds[i-1],2,info);
    }
  }

  /* Make a connected boundary (specific to Elmer format) needed in linear constraints */
  for(k=0;k<nomeshes;k++) 
    for(i=1;i<=eg.connect;i++) 
      SetConnectedBoundary(&(data[k]),boundaries[k],eg.connectbounds[i-1],i,info);
  
  /* Divide quadrilateral meshes into triangular meshes */
  for(k=0;k<nomeshes;k++) 
    if(nogrids && (eg.triangles || grids[k].triangles == TRUE)) {
      Real criticalangle;
      criticalangle = MAX(eg.triangleangle , grids[k].triangleangle);
      ElementsToTriangles(&data[k],boundaries[k],criticalangle,info);
    }

  /* Make a boundary layer with two different methods */
  if(eg.layers > 0) 
    for(k=0;k<nomeshes;k++) 
      CreateBoundaryLayer(&data[k],boundaries[k],eg.layers,
			  eg.layerbounds, eg.layernumber, eg.layerratios, eg.layerthickness,
			  eg.layerparents, eg.layermove, eg.layereps, info);
  else if(eg.layers < 0) 
    for(k=0;k<nomeshes;k++) 
      CreateBoundaryLayerDivide(&data[k],boundaries[k],abs(eg.layers),
				eg.layerbounds, eg.layernumber, eg.layerratios, eg.layerthickness,
				eg.layerparents, info);

  if(outmethod != 1 && dim != 2 && eg.dim != 2) { 
    j = MAX(nogrids,1);

    for(k=0;k<j;k++) {
      if(grids[k].dimension == 3 || grids[k].rotate) {

	boundaries[j] = (struct BoundaryType*)
	  malloc((size_t) (MAXBOUNDARIES)*sizeof(struct BoundaryType)); 	
	
	for(i=0;i<MAXBOUNDARIES;i++) 
	  boundaries[j][i].created = FALSE;

	CreateKnotsExtruded(&(data[k]),boundaries[k],&(grids[k]),
			    &(data[j]),boundaries[j],info);

	if(nogrids) {
	  elements3d = MAX(eg.elements3d, grids[k].wantedelems3d);
	  nodes3d = MAX(eg.nodes3d, grids[k].wantednodes3d);
	  
	  if(elements3d) {
	    if( abs(data[j].noelements - elements3d) / (1.0*elements3d) > 0.01 && elementsredone < 5 ) {
	      grids[k].wantedelems *= pow(1.0*elements3d / data[j].noelements, (2.0/3.0));
	      elementsredone++;
	    }
	    else elementsredone = 0;
	  }
	  else if(nodes3d) {
	    if( abs(data[j].noknots - nodes3d) / (1.0*nodes3d) > 0.01 && elementsredone < 5 ) {
	      grids[k].wantedelems *= pow(1.0*nodes3d / data[j].noknots, (2.0/3.0));
	      elementsredone++;
	    }
	    else elementsredone = 0;
	  }
	  
	  if(elementsredone) {
	    nomeshes = 0;
	    for(i=0;i < nogrids;i++) SetElementDivision(&(grids[i]),info);
	    
	    DestroyKnots(&data[j]);
	    DestroyKnots(&data[k]);
	    free(cell[k]);
	    
	    if(info) printf("Iteration %d of elements number targiting %d in 2D\n",
			    elementsredone,grids[k].wantedelems);
	    goto redoelements;
	  }
	}	

	data[k] = data[j];
	boundaries[k] = boundaries[j];
      }
    }
  }

  /* If the original mesh was given in polar coordinates make the transformation into cartesian ones */
  for(k=0;k<nomeshes;k++) {
    if(eg.polar || data[k].coordsystem == COORD_POLAR) {
      if(!eg.polar) eg.polarradius = grids[k].polarradius;
      PolarCoordinates(&data[k],eg.polarradius,info);
    }
  }

  /* If the original mesh was given in cylindrical coordinates make the transformation into cartesian ones */
  for(k=0;k<nomeshes;k++) {
    if(eg.cylinder || data[k].coordsystem == COORD_CYL) {
      CylinderCoordinates(&data[k],info);
    }
  }

  /* Unite meshes if there are several of them */
  if(eg.unitemeshes) {
    for(k=1;k<nomeshes;k++)
      UniteMeshes(&data[0],&data[k],boundaries[0],boundaries[k],info);
    nomeshes = nogrids = 1;
  }
  
  if(eg.clone[0] || eg.clone[1] || eg.clone[2]) {
    for(k=0;k<nomeshes;k++) {
      CloneMeshes(&data[k],boundaries[k],eg.clone,eg.clonesize,FALSE,info);
      mergeeps = fabs(eg.clonesize[0]+eg.clonesize[1]+eg.clonesize[2]) * 1.0e-8;
      MergeElements(&data[k],boundaries[k],eg.order,eg.corder,mergeeps,TRUE,TRUE);
    }
  }

  if(eg.mirror[0] || eg.mirror[1] || eg.mirror[2]) {
    for(k=0;k<nomeshes;k++) {
      MirrorMeshes(&data[k],boundaries[k],eg.mirror,FALSE,eg.clonesize,eg.mirrorbc,info);
      mergeeps = fabs(eg.clonesize[0]+eg.clonesize[1]+eg.clonesize[2]) * 1.0e-8;
      MergeElements(&data[k],boundaries[k],eg.order,eg.corder,mergeeps,FALSE,TRUE);
    }
  }

  /* Naming convection for the case of several meshes */
  if(nomeshes > 1) {
    strcpy(prefix,eg.filesout[0]);
    for(k=0;k<nomeshes;k++)
      sprintf(eg.filesout[k],"%s%d",prefix,k+1);
  }

  for(k=0;k<nomeshes;k++) {
    if(nogrids && grids[k].reduceordermatmax) {
      eg.reduce = TRUE;
      eg.reducemat1 = grids[k].reduceordermatmin;
      eg.reducemat2 = grids[k].reduceordermatmax;
    }
    if(eg.reduce) 
      ReduceElementOrder(&data[k],eg.reducemat1,eg.reducemat2);
  }

  for(k=0;k<nomeshes;k++) 
    if(eg.increase) IncreaseElementOrder(&data[k],TRUE);
 
  for(k=0;k<nomeshes;k++) {
    if(eg.merge) 
      MergeElements(&data[k],boundaries[k],eg.order,eg.corder,eg.cmerge,FALSE,TRUE);
    else if(eg.order == 3) 
      ReorderElementsMetis(&data[k],TRUE);
    else if(eg.order) 
      ReorderElements(&data[k],boundaries[k],eg.order,eg.corder,TRUE);
    
    if(eg.isoparam) 
      IsoparametricElements(&data[k],boundaries[k],TRUE,info);
  }  

  /* This is mainly here for historical reasons */
  for(k=0;k<nomeshes;k++) 
    if(eg.findsides) 
      SideToBulkElements(&data[k],boundaries[k],eg.sidebulk,FALSE,info);


  for(k=0;k<nomeshes;k++) {
    if(eg.bulkbounds || eg.boundbounds) {
      int *boundnodes,noboundnodes;
      boundnodes = Ivector(1,data[k].noknots);
      
      if(eg.bulkbounds) {
	for(l=0;l<eg.bulkbounds;l++) {
	  FindBulkBoundary(&data[k],eg.bulkbound[3*l],eg.bulkbound[3*l+1],
			   boundnodes,&noboundnodes,info);
	  FindNewBoundaries(&data[k],boundaries[k],boundnodes,eg.bulkbound[3*l+2],1,info);
	}
      }
      if(eg.boundbounds) {
	for(l=0;l<eg.boundbounds;l++) {	
	  FindBoundaryBoundary(&data[k],boundaries[k],eg.boundbound[3*l],eg.boundbound[3*l+1],
			       boundnodes,&noboundnodes,info);
	  FindNewBoundaries(&data[k],boundaries[k],boundnodes,eg.boundbound[3*l+2],2,info);
	}
      }
      free_Ivector(boundnodes,1,data[k].noknots);
    }

#if 0
    if(eg.bulkbounds || eg.boundbounds)
      SeparateCartesianBoundaries(&data[k],boundaries[k],info);
#endif
  }

  for(k=0;k<nomeshes;k++) 
    RotateTranslateScale(&data[k],&eg,info);

  if(eg.removelowdim) 
    for(k=0;k<nomeshes;k++)
      RemoveLowerDimensionalBoundaries(&data[k],boundaries[k],info);

  if(eg.removeunused) 
    for(k=0;k<nomeshes;k++)
      RemoveUnusedNodes(&data[k],info);

  if(eg.boundorder || eg.bcoffset) 
    for(k=0;k<nomeshes;k++) 
      RenumberBoundaryTypes(&data[k],boundaries[k],eg.boundorder,eg.bcoffset,info);

  if(eg.bulkorder) 
    for(k=0;k<nomeshes;k++) 
      RenumberMaterialTypes(&data[k],boundaries[k],info);

  if(eg.sidemappings) {
    int currenttype;
    
    for(l=0;l<eg.sidemappings;l++) 
      printf("Setting boundary types between %d and %d to %d\n",
	     eg.sidemap[3*l],eg.sidemap[3*l+1],eg.sidemap[3*l+2]);

    for(k=0;k<nomeshes;k++) {
      for(j=0;j < MAXBOUNDARIES;j++) {
	if(!boundaries[k][j].created) continue;
	
	for(i=1; i <= boundaries[k][j].nosides; i++) {
	  if(currenttype = boundaries[k][j].types[i]) {
	    for(l=0;l<eg.sidemappings;l++) {
	      if(currenttype >= eg.sidemap[3*l] && currenttype <= eg.sidemap[3*l+1]) {
		boundaries[k][j].types[i] = eg.sidemap[3*l+2];
		currenttype = -1;
	      }
	    }
	  }
	}
      }
    }
    if(info) printf("Renumbering boundary types finished\n");
  }
  
  if(eg.bulkmappings) {
    int currenttype;
    for(l=0;l<eg.bulkmappings;l++) 
      printf("Setting material types between %d and %d to %d\n",
	     eg.bulkmap[3*l],eg.bulkmap[3*l+1],eg.bulkmap[3*l+2]);
    for(k=0;k<nomeshes;k++) {
      for(j=1;j<=data[k].noelements;j++) {
	currenttype = data[k].material[j];
	for(l=0;l<eg.bulkmappings;l++) {
	  if(currenttype >= eg.bulkmap[3*l] && currenttype <= eg.bulkmap[3*l+1]) {
	    data[k].material[j] = eg.bulkmap[3*l+2];
	    currenttype = -1;
	  }
	}
      }
    }
    if(info) printf("Renumbering material indexes finished\n");
  }

  for(k=0;k<nomeshes;k++) {
    int noopt = 0;

    if(eg.partitions || eg.metis) 
      printf("\nElmergrid partitioning meshes:\n");
      printf(  "------------------------------\n");

    if(eg.partitions) {
      if(eg.partopt % 2 == 0) 
	PartitionSimpleElements(&data[k],eg.partdim,eg.periodicdim,eg.partorder,eg.partcorder,info);	
      else 
	PartitionSimpleNodes(&data[k],eg.partdim,eg.periodicdim,eg.partorder,eg.partcorder,info);	
      noopt = eg.partopt / 2;      
    }
#if PARTMETIS
    if(eg.metis) {
      if(eg.periodicdim[0] || eg.periodicdim[1] || eg.periodicdim[2]) 
	FindPeriodicNodes(&data[k],eg.periodicdim,info);
      if(eg.partopt % 5 <= 1) 
	PartitionMetisElements(&data[k],eg.metis,eg.partopt % 5,info);
      else
	PartitionMetisNodes(&data[k],eg.metis,eg.partopt % 5,info);      
      noopt = eg.partopt / 5;      
    }
#endif
    if(eg.partitions || eg.metis ) 
      OptimizePartitioning(&data[k],boundaries[k],noopt,info);
    if(data[k].periodicexist) {
      free_Ivector(data[k].periodic,1,data[k].noknots);
      data[k].periodicexist = FALSE;
    }
  }


  /********************************/
  printf("\nElmergrid saving data:\n");
  printf(  "----------------------\n");

  switch (outmethod) {
  case 1:
    SaveElmergrid(grids,nogrids,eg.filesout[0],info);
    break; 

  case 2:
    for(k=0;k<nomeshes;k++) {
      if(data[k].nopartitions > 1) 
	SaveElmerInputPartitioned(&data[k],boundaries[k],eg.filesout[k],eg.decimals,
				  eg.partitionhalo,eg.partitionindirect,info);
      else
	SaveElmerInput(&data[k],boundaries[k],eg.filesout[k],eg.decimals,info);
    }
    break;

  case 22:

    for(k=0;k<nomeshes;k++) {
      SaveElmerInputFemBem(&data[k],boundaries[k],eg.filesout[k],eg.decimals,info);
    }
    break;


  case 3:
      /* Create a variable so that when saving data in ElmerPost format there is something to visualize */
    for(k=0;k<nomeshes;k++) {
      if(data[k].variables == 0) {
	CreateVariable(&data[k],1,1,0.0,"Number",FALSE);
	for(i=1;i<=data[k].alldofs[1];i++)
	  data[k].dofs[1][i] = (Real)(i);	
      }
      SaveSolutionElmer(&data[k],boundaries[k],eg.saveboundaries ? MAXBOUNDARIES:0,
			eg.filesout[k],eg.decimals,info);
    }
    break;

  case 4:
    printf("The output number 4 still refers to ep-file but will become obsolite in time\n");
    printf("Rather use number 3 for ElmerPost output format\n");
    for(k=0;k<nomeshes;k++) {
      if(data[k].variables == 0) {
	CreateVariable(&data[k],1,1,0.0,"Number",FALSE);
	for(i=1;i<=data[k].alldofs[1];i++)
	  data[k].dofs[1][i] = (Real)(i);	      
      }
      SaveSolutionElmer(&data[k],boundaries[k],eg.saveboundaries ? MAXBOUNDARIES:0,
			eg.filesout[k],eg.decimals,info);
    }
    break;

  case 6:
    for(k=0;k<nomeshes;k++)
      SaveAbaqusInput(&data[k],eg.filesout[k],info); 
    break;
    
  case 7:
    for(k=0;k<nomeshes;k++)
      SaveFidapOutput(&data[k],eg.filesout[k],info,1,data[k].dofs[1]);
    break;

#if 0
  case 8:
    EasymeshSave();
    break;
#endif

  case 18:    
    for(k=0;k<nomeshes;k++) 
      SaveFastcapInput(&data[k],boundaries[k],eg.filesout[k],eg.decimals,info);
    break;


    
    /* Some obsolite special formats related to mapping, view factors etc. */
    
  case 101:
    for(k=0;k<nogrids;k++) {   
      for(i=0;i<grids[k].noboundaries;i++)
	if(boundaries[k][i].created == TRUE) {
	  sprintf(prefix,"%s%d",eg.filesout[k],i+1);
	  SaveBoundary(&data[k],&boundaries[k][i],prefix,info);
	}
    }
    break;
    
  case 102:
    for(k=0;k<nogrids;k++) {   
      for(i=0;i<grids[k].noboundaries;i++)
	if(boundaries[k][i].created == TRUE) {
	  sprintf(prefix,"%s%d",eg.filesout[k],i+1);
	  boundaries[k][i].vf = Rmatrix(1,boundaries[k][i].nosides,
					1,boundaries[k][i].nosides);
	  boundaries[k][i].vfcreated = TRUE;
	  SideAreas(&data[k],&boundaries[k][i]);
	  ViewFactors(&data[k],&boundaries[k][i],TRUE);      
	  SaveViewFactors(&data[k],&boundaries[k][i],prefix,info);
	}
    }
    break;

  case 103:
    if(nogrids <= 1) printf("No mapping possible for %d grid.\n",nogrids);
    for(k=0;k<nogrids-1;k++) {
      sprintf(prefix,"%s%dto%d",eg.filesout[0],k+1,k+2);
      SaveGridToGridMapping(cell[k],&(grids[k]),cell[k+1],&(grids[k+1]),prefix);
      sprintf(prefix,"%s%dto%d",eg.filesout[0],k+2,k+1);
      SaveGridToGridMapping(cell[k+1],&(grids[k+1]),cell[k],&(grids[k]),prefix);
    }
    break;

  case 104:
    if(LoadSolutionElmer(&(data[1]),FALSE,eg.filesin[1],info)) {
      printf("The reading of the input file %s was not succesfull\n",eg.filesin[1]);
      Goodbye();
    }
    ElmerToElmerMap(&(data[0]),&(data[1]),TRUE);
    sprintf(prefix,"%s%s%s",eg.filesin[1],"_",eg.filesin[0]);
    SaveSolutionElmer(&(data[1]),boundaries[0],0,prefix,eg.decimals,TRUE);
    break;

  case 105:
    if(LoadSolutionElmer(&(data[1]),FALSE,eg.filesout[1],info)) {
      printf("The reading of the input file %s was not succesfull\n",eg.filesout[1]);
      Goodbye();
    }
    if(ElmerToElmerMapQuick(&(data[0]),&(data[1]),eg.mapfile,info))
      Goodbye();
    sprintf(prefix,"%s%s%s",eg.filesin[1],"_",eg.filesin[0]);
    SaveSolutionElmer(&(data[1]),boundaries[0],0,prefix,eg.decimals,TRUE);
    break;



  default:
    Instructions();
    break;
  }    

  Goodbye();
  return(0);
}





