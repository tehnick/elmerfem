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

/* -------------------------------:  femelmer.c  :----------------------------
   This module includes interfaces for the other Elmer programs.
*/

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "nrutil.h"
#include "common.h"
#include "femdef.h"
#include "femtools.h"
#include "femtypes.h"
#include "femknot.h"
#include "femsolve.h"
#include "femelmer.h"
#include "../config.h"


#define getline fgets(line,MAXLINESIZE,in) 


int LoadSolutionElmer(struct FemType *data,int results,char *prefix,int info)
/* This procedure reads the solution in a form that is understood 
   by the programs Funcs and ElmerPost, created
   by Juha Ruokolainen at Center for Scientific Computing. 
   This procedure is not by far general.
   */
{
  int noknots,noelements,novctrs,elemcode,open;
  int timesteps,i,j,k,grp;
  Real r;
  FILE *in;
  char line[MAXLINESIZE],filename[MAXFILESIZE],text[MAXNAMESIZE];

  AddExtension(prefix,filename,"ep");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadSolutionElmer: The opening of the Elmer-file %s wasn't succesfull!\n",
	   filename);
    return(1);
  }
  else 
    printf("Loading Elmer data from %s\n",filename);

  InitializeKnots(data);

  getline;
  sscanf(line,"%d %d %d %d",&noknots,&noelements,&novctrs,&timesteps);

  data->dim = 3;
  data->maxnodes = MAXNODESD2;
  data->noknots = noknots;
  data->noelements = noelements;
  data->timesteps = timesteps;
  
  if(timesteps > 1) 
    printf("LoadSolutionElmer: The subroutine may crash with %d timesteps\n",
	   timesteps);
  if(timesteps < 1) timesteps = 1;
    
  if(info) printf("Allocating for %d knots and %d elements.\n",
		  noknots,noelements);
  AllocateKnots(data);

  if(results) {
    if(timesteps > 1) 
      data->times = Rvector(0,timesteps-1);
    for(i=1;i<=novctrs;i++) {
      sprintf(text,"var%d",i);
      CreateVariable(data,i,timesteps,0.0,text,FALSE);    
    }
  }

  if(info) printf("Reading %d coordinates.\n",noknots);
  for(i=1; i <= noknots; i++) {
    getline;
    sscanf(line,"%le %le %le",
	   &(data->x[i]),&(data->y[i]),&(data->z[i]));
  }

  if(info) printf("Reading %d element topologies.\n",noelements);

  grp = 0;
  open = FALSE;
  for(i=1; i <= noelements; i++) {
    fscanf(in,"%s",text);
    if(strstr(text,"#group")) {
      grp++;
      printf("Starting a new element group\n");
      fscanf(in,"%s",text);      
      fscanf(in,"%s",text);
      open = TRUE;
    }
    if(strstr(text,"#end")) {
      printf("Ending an element group\n");
      fscanf(in,"%s",text);      
      open = FALSE;
    }
    fscanf(in,"%d",&(data->elementtypes[i]));
    data->material[i] = grp;
    for(j=0;j< data->elementtypes[i]%100 ;j++) {
      k = fscanf(in,"%d",&(data->topology[i][j]));
      data->topology[i][j] += 1;
    }
  }
  if(open) {    
    do {
      fscanf(in,"%s",text);
    } while (!strstr(text,"#end"));
    fscanf(in,"%s",text);
    printf("Ending an element group\n");   
    open = FALSE;
  }

  if(results == 0) 
    return(0);

  if(info) printf("Reading %d degrees of freedom for %d knots.\n",
		  novctrs,noknots);
  if (timesteps<2) {
    for(i=1; i <= noknots; i++) 
      for(j=1;j <= novctrs;j++) 
	fscanf(in,"%le",&(data->dofs[j][i]));
  }
  else for(k=0;k<timesteps;k++) {
    i = fscanf(in,"%s",text);
    if(i < 0) goto end;
    fscanf(in,"%d",&i);
    fscanf(in,"%d",&j);
    fscanf(in,"%le",&r);

    if(0) printf("Loading steps i=%d  j=%d  k=%d  r=%.3lg\n",i,j,k,r);

    for(i=1; i <= noknots; i++) 
      for(j=1;j <= novctrs;j++) 
	fscanf(in,"%le",&(data->dofs[j][k*noknots+i]));
  }

end:
  data->timesteps = k+1;

  fclose(in);

  return(0);
}



int FuseSolutionElmerPartitioned(char *prefix,char *outfile,int decimals,int info)
#define MAXPAR 100
{
  int *noknots,*noelements,novctrs,elemcode,open;
  int totknots,totelements,sumknots,sumelements;
  int timesteps,i,j,k,step;
  int ind[MAXNODESD1];
  int nofiles;
  Real r, *res, x, y, z;
  FILE *in[MAXPAR],*out;
  char line[MAXLINESIZE],filename[MAXFILESIZE],text[MAXNAMESIZE],outstyle[MAXFILESIZE];
  char *cp;

  for(i=0;;i++) {
    sprintf(filename,"%s.ep.%d",prefix,i);
    if ((in[i] = fopen(filename,"r")) == NULL) break;
  }

  nofiles = i;

  if(nofiles < 2) {
    printf("Opening of partitioned data from file %s wasn't succesfull!\n",
	   filename);
    return(1);
  } else {
    if(info) printf("Loading Elmer results from %d partitions.\n",nofiles);
  }

  noknots = Ivector(0,nofiles-1);
  noelements = Ivector(0,nofiles-1);
 
  sumknots = 0;
  sumelements = 0;


  for(i=0;i<nofiles;i++) {
    fgets(line,MAXLINESIZE,in[i]);
    if(i==0) {
      cp = line;
      noknots[i] = next_int(&cp);
      noelements[i] = next_int(&cp);
      novctrs = next_int(&cp);
      timesteps = next_int(&cp);
    }
    else {
      sscanf(line,"%d %d",&noknots[i],&noelements[i]);
    }
    sumknots += noknots[i];
    sumelements += noelements[i];
  }
  totknots = sumknots;
  totelements = sumelements;
  res = Rvector(1,novctrs);

  if(info) printf("There are alltogether %d nodes and %d elements.\n",totknots,sumelements);


  AddExtension(outfile,filename,"ep");
  if(info) printf("Saving ElmerPost data to %s.\n",filename);  
  out = fopen(filename,"w");
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(3);
  }
  fprintf(out,"%d %d %d %d %s %s",totknots,totelements,novctrs+1,timesteps,"scalar: Partition",cp);

 
  if(info) printf("Reading and writing %d coordinates.\n",totknots);
  sprintf(outstyle,"%%.%dlg %%.%dlg %%.%dlg\n",decimals,decimals,decimals);

  for(j=0; j <= nofiles; j++) {
    for(i=1; i <= noknots[j]; i++) {
      do {
	fgets(line,MAXLINESIZE,in[j]);
      } while(line[0] == '#');

      sscanf(line,"%le %le %le",&x,&y,&z);
      fprintf(out,outstyle,x,y,z);
    }
  }

  if(info) printf("Reading and writing %d element topologies.\n",totelements);
  sumknots = 0;

  for(j=0; j <= nofiles; j++) {
    open = FALSE;
    for(i=1; i <= noelements[j]; i++) {
      do {
	fgets(line,MAXLINESIZE,in[j]);
      } while (line[0] == '#');

      sscanf(line,"%s",text);
      cp = strstr(line," ");

      elemcode = next_int(&cp);
      for(k=0;k< elemcode%100 ;k++) 
	ind[k] = next_int(&cp);

      if(elemcode == 102) elemcode = 101;

      fprintf(out,"%s %d",text,elemcode);
      for(k=0;k < elemcode%100 ;k++)       
	fprintf(out," %d",ind[k]+sumknots);
      fprintf(out,"\n");
    }
    sumknots += noknots[j];
  }

  if(info) printf("Reading and writing %d degrees of freedom.\n",novctrs);
  sprintf(outstyle,"%%.%dlg ",decimals);

  for(step = 1; step <= timesteps; step++) {
    for(k=0;k<nofiles;k++) 
      for(i=1; i <= noknots[k]; i++) {
	do {
	  fgets(line,MAXLINESIZE,in[k]);
	  if(k==0 && strstr(line,"#time")) 
	    fprintf(out,"%s",line);
	}
	while (line[0] == '#');

	cp = line;
	for(j=1;j <= novctrs;j++) 
	  res[j] = next_real(&cp);

	fprintf(out,"%d ",k+1);
	for(j=1;j <= novctrs;j++) 
	  fprintf(out,outstyle,res[j]);
	fprintf(out,"\n");
      }
  }


  for(i=0;i<nofiles;i++) 
    fclose(in[i]);
  fclose(out);

  if(info) printf("Successfully fused partitioned Elmer results\n");

  return(0);
}



static int FindParentSide(struct FemType *data,struct BoundaryType *bound,
			  int sideelem,int sideelemtype,int *sideind)
{
  int i,j,k,sideelemtype2,elemind,parent,normal;
  int elemsides,side,sidenodes,hit,noparent, bulknodes;
  int sideind2[MAXNODESD1];


  for(parent=1;parent<=2;parent++) {
    if(parent == 1) {
      elemind = bound->parent[sideelem];
      noparent = (parent < 1);
    }
    else
      elemind = bound->parent2[sideelem];

    if(elemind > 0) {
      elemsides = data->elementtypes[elemind] / 100;
      bulknodes = data->elementtypes[elemind] % 100;

      if(elemsides == 8) elemsides = 6;
      else if(elemsides == 6) elemsides = 5;
      else if(elemsides == 5) elemsides = 4;
      
      for(normal=1;normal >= -1;normal -= 2) {

	for(side=0;side<elemsides;side++) {

	  GetElementSide(elemind,side,normal,data,&sideind2[0],&sideelemtype2);

	  if(sideelemtype != sideelemtype2) 
	    printf("FindParentSide: somethings smells %d vs %d\n",
		   sideelemtype,sideelemtype2);
	  sidenodes = sideelemtype%100;

	  for(j=0;j<sidenodes;j++) {
	    hit = TRUE;
	    for(i=0;i<sidenodes;i++) 
	      if(sideind[(i+j)%sidenodes] != sideind2[i]) hit = FALSE;
	    

	    if(hit == TRUE) {
	      if(parent == 1) {
		bound->side[sideelem] = side;
		bound->normal[sideelem] = normal;
	      }
	      else {
		bound->side2[sideelem] = side;	      
	      }
	      goto skip;
	    }
	  }
	}
      }	

      
      /* this finding of sides does not guarantee that normals are oriented correctly */
      normal = 1;
 
      for(side=0;side < elemsides;side++) {

	GetElementSide(elemind,side,normal,data,&sideind2[0],&sideelemtype2);
	sidenodes = sideelemtype%100;

	if(sideelemtype != sideelemtype2) 
	  printf("b) FindParentSide: somethings smells %d vs %d\n",
		 sideelemtype,sideelemtype2);

	hit = 0;
	for(j=0;j<sidenodes;j++) 
	  for(i=0;i<sidenodes;i++) 
	    if(sideind[i] == sideind2[j]) hit++;
	if(hit == sidenodes) {
	  hit = TRUE;
	  if(parent == 1) {
	    bound->side[sideelem] = side;
	  }
	  else 
	    bound->side2[sideelem] = side;	      
	  goto skip;
	}
	
      }
    }

  skip:  
    if(!hit) {
      printf("FindParentSide: unsuccesfull (elemtype=%d elemsides=%d parent=%d)\n",
		    sideelemtype,elemsides,parent);

      printf("parents = %d %d\n",bound->parent[sideelem],bound->parent2[sideelem]);

      printf("sideind =");
      for(i=0;i<sideelemtype%100;i++)
      printf(" %d ",sideind[i]);
      printf("\n");

      printf("elemind =");
      for(i=0;i<elemsides;i++)
      printf(" %d ",data->topology[elemind][i]);
      printf("\n");      
    }

  }

  return(0);
}




int LoadElmerInput(struct FemType *data,struct BoundaryType *bound,
		   char *prefix,int info)
/* This procedure reads the mesh assuming ElmerSolver format.
   */
{
  int noknots,noelements,nosides,maxelemtype;
  int sideind[MAXNODESD1],tottypes,elementtype;
  int i,j,k,dummyint;
  FILE *in;
  char line[MAXLINESIZE],filename[MAXFILESIZE],directoryname[MAXFILESIZE];


  sprintf(directoryname,"%s",prefix);
  chdir(directoryname);

  if(info) printf("Loading mesh in ElmerSolver format from directory %s.\n",
		  directoryname);

  InitializeKnots(data);


  sprintf(filename,"%s","mesh.header");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadElmerInput: The opening of the header-file %s failed!\n",
	   filename);
    return(1);
  }
  else 
    printf("Loading Elmer header from %s\n",filename);

  getline;
  sscanf(line,"%d %d %d",&noknots,&noelements,&nosides);
  getline;
  sscanf(line,"%d",&tottypes);

  maxelemtype = 0;
  for(i=1;i<=tottypes;i++) {   
    getline;
    sscanf(line,"%d",&dummyint);
    if(dummyint > maxelemtype) maxelemtype = dummyint;
  }
  fclose(in);
  
  if(maxelemtype < 300) {
    data->dim = 1;
  }
  else if(maxelemtype < 500) {
    data->dim = 2;
  }
  else {
    data->dim = 3;
  }

  data->maxnodes = maxelemtype % 100;
  data->noknots = noknots;
  data->noelements = noelements;


  if(info) printf("Allocating for %d knots and %d elements.\n",
		  noknots,noelements);
  AllocateKnots(data);


  sprintf(filename,"%s","mesh.nodes");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadElmerInput: The opening of the nodes-file %s failed!\n",
	   filename);
    return(2);
  }
  else 
    printf("Loading %d Elmer nodes from %s\n",noknots,filename);

  for(i=1; i <= noknots; i++) {
    getline;
    sscanf(line,"%d %d %le %le %le",
	   &j, &dummyint, &(data->x[i]),&(data->y[i]),&(data->z[i]));
    if(j != i) printf("LoadElmerInput: nodes i=%d j=%d\n",i,j);
  }
  fclose(in);


  sprintf(filename,"%s","mesh.elements");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadElmerInput: The opening of the element-file %s failed!\n",
	   filename);
    return(3);
  }
  else 
    printf("Loading %d Elmer elements from %s\n",noelements,filename);

  for(i=1; i <= noelements; i++) {
    fscanf(in,"%d",&dummyint);
    if(i != dummyint) printf("LoadElmerInput: i=%d element=%d\n",
				 i,dummyint);
    fscanf(in,"%d",&(data->material[i]));
    fscanf(in,"%d",&(data->elementtypes[i]));
    for(j=0;j< data->elementtypes[i]%100 ;j++) 
      fscanf(in,"%d",&(data->topology[i][j]));
  }
  fclose(in);


  sprintf(filename,"%s","mesh.boundary");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadElmerInput: The opening of the boundary-file %s failed!\n",
	   filename);
    return(4);
  }
  else 
    printf("Loading Elmer boundaries from %s\n",filename);

  AllocateBoundary(bound,nosides);
  

  i = 0;
  for(k=1; k <= nosides; k++) {
    i++;
    fscanf(in,"%d",&dummyint);
#if 0
    if(k != dummyint) printf("LoadElmerInput: k=%d side=%d\n",k,dummyint);
#endif
    fscanf(in,"%d",&(bound->types[i]));
    fscanf(in,"%d",&(bound->parent[i]));
    fscanf(in,"%d",&(bound->parent2[i]));
    fscanf(in,"%d",&elementtype);
    for(j=0;j< elementtype%100 ;j++) 
      fscanf(in,"%d",&(sideind[j]));

    if(bound->parent[i] == 0 && bound->parent2[i] != 0) {
      bound->parent[i] = bound->parent2[i];
      bound->parent2[i] = 0;
    }

    if(bound->parent[i] > 0) {
      FindParentSide(data,bound,i,elementtype,sideind);
    }
    else {
#if 0
      printf("could not find parent for side %d with inds %d %d\n",
	     dummyint,sideind[0],sideind[1]);
      printf("eleminfo: parents %d %d type %d\n",
	     bound->parent[i],bound->parent2[i],bound->types[i]);   
#endif
      i--;
    }
  }
  bound->nosides = i;
  fclose(in); 

  chdir("..");

  return(0);
}




int SaveSolutionElmer(struct FemType *data,struct BoundaryType *bound,
		      int nobound,char *prefix,int decimals,int info)
/* This procedure saves the solution in a form that is understood 
   by the programs Funcs and ElmerPost, created
   by Juha Ruokolainen at Center for Scientific Computing. 
   */
{
  int noknots,noelements,bulkelems,novctrs,sideelems,sideelemtype,elemtype,boundtype;
  char filename[MAXFILESIZE],outstyle[MAXFILESIZE];
  int i,j,k,nodesd1,timesteps,nodesd2,fail;
  int ind[MAXNODESD1];
  FILE *out;
  
  if(!data->created) {
    printf("SaveSolutionElmer: You tried to save points that were never created.\n");
    return(1);
  }
  if(data->variables == 0) {
    printf("SaveSolutionElmer: there are no dofs to save!\n");
    return(2);
  }
  
  sideelems = 0;
  if(nobound) {
    for(i=0;i<nobound;i++)
      if(bound[i].created) sideelems += bound[i].nosides; 
  }

  noknots = data->noknots;
  bulkelems = data->noelements;
  if(nobound)
    noelements = bulkelems + sideelems;
  else
    noelements = bulkelems;
  timesteps = data->timesteps;
  if(timesteps < 1) timesteps = 1;

  novctrs = 0;
  for(i=0;i<MAXDOFS;i++) {
    if(data->edofs[i] == 1) novctrs += 1; 
    if(data->edofs[i] == 2) novctrs += 3; 
    if(data->edofs[i] == 3) novctrs += 3; 
  }

  AddExtension(prefix,filename,"ep");
  if(info) printf("Saving ElmerPost data to %s.\n",filename);  

  out = fopen(filename,"w");
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(3);
  }

  fprintf(out,"%d %d %d %d",noknots,noelements,novctrs,timesteps);

  for(i=0; i<MAXDOFS; i++) {
    if(data->edofs[i] == 1) 
      fprintf(out," scalar: %s",data->dofname[i]);
    else if(data->edofs[i] > 1) 
      fprintf(out," vector: %s",data->dofname[i]);
  }
  fprintf(out,"\n");

  if(info) printf("Saving %d node coordinates.\n",noknots);
  
  if(data->dim == 1) {
    sprintf(outstyle,"%%.%dlg 0.0 0.0\n",decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,data->x[i]);
  }
  else if(data->dim == 2) {
    sprintf(outstyle,"%%.%dlg %%.%dlg 0.0\n",decimals,decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,data->x[i],data->y[i]);
  }
  else if(data->dim == 3) {
    sprintf(outstyle,"%%.%dlg %%.%dlg %%.%dlg\n",decimals,decimals,decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,data->x[i],data->y[i],data->z[i]);      
  }

  printf("Saving %d bulk element topologies.\n",bulkelems);

  for(i=1;i<=bulkelems;i++) {
    elemtype = data->elementtypes[i];

    if(elemtype/100 > 4) 
      fprintf(out,"vol%d %d ",data->material[i],data->elementtypes[i]);      
    else if(elemtype/100 > 2) 
      fprintf(out,"surf%d %d ",data->material[i],data->elementtypes[i]);
    else if(elemtype/100 > 1) 
      fprintf(out,"line%d %d ",data->material[i],data->elementtypes[i]);
    else 
      fprintf(out,"pnt%d %d ",data->material[i],data->elementtypes[i]);

    nodesd2 = data->elementtypes[i]%100;
    for(j=0;j<nodesd2;j++) 
      fprintf(out,"%d ",data->topology[i][j]-1);
    fprintf(out,"\n");    
  }

  if(nobound) {
    printf("Saving %d side element topologies.\n",sideelems);
    for(j=0;j<nobound;j++) {
      if(bound[j].created == FALSE) continue;
      
      for(i=1;i<=bound[j].nosides;i++) {
#if 0
	printf("type=%d no=%d parent=%d side=%d elemtype=%d\n",
	       j,i,bound[j].parent[i],bound[j].side[i],data->elementtypes[bound[j].parent[i]]);
#endif
	GetElementSide(bound[j].parent[i],bound[j].side[i],bound[j].normal[i],data,ind,&sideelemtype); 

	boundtype = bound[j].types[i];

	if(sideelemtype/100 > 2) 
	  fprintf(out,"bcside%d %d ",boundtype,sideelemtype);
	else if(sideelemtype/100 > 1) 
	  fprintf(out,"bcline%d %d ",boundtype,sideelemtype);
	else 
	fprintf(out,"bcpnt%d %d ",boundtype,sideelemtype);

	nodesd1 = sideelemtype%100;
	for(k=0;k<nodesd1;k++)
	  fprintf(out,"%d ",ind[k]-1);
	fprintf(out,"\n");
      }
    }
  }

  printf("Saving %d degrees of freedom for each knot.\n",novctrs);
  for(k=0;k<timesteps;k++) {
    for(i=1;i<=noknots;i++){
      for(j=0;j<MAXDOFS;j++) {
	if(data->edofs[j] == 1) 
	  fprintf(out,"%.6lg ",data->dofs[j][k*noknots+i]);
	if(data->edofs[j] == 2) 
	  fprintf(out,"%.6lg %.6lg 0.0 ",
		  data->dofs[j][2*(k*noknots+i)-1],data->dofs[j][2*(k*noknots+i)]);
	if(data->edofs[j] == 3) 
	  fprintf(out,"%.6lg %.6lg %.6lg ",
		  data->dofs[j][3*(k*noknots+i)-2],
		  data->dofs[j][3*(k*noknots+i)-1],
		  data->dofs[j][3*(k*noknots+i)]);
      }
      fprintf(out,"\n");
    }
  }
  fclose(out);

  return(0);
}


int SaveElmerInput(struct FemType *data,
		   struct BoundaryType *bound,char *prefix,
		   int decimals,int ver,int info)
/* Saves the mesh in a form that may be used as input 
   in Elmer calculations. 
   */
#define MAXELEMENTTYPE 827
{
  int noknots,noelements,sumsides,elemtype,fail;
  int sideelemtype,nodesd1,nodesd2;
  int i,j,k,l,bulktypes[MAXELEMENTTYPE+1],sidetypes[MAXELEMENTTYPE+1];
  int alltypes[MAXELEMENTTYPE+1],tottypes;
  int ind[MAXNODESD1];
  FILE *out;
  char filename[MAXFILESIZE], outstyle[MAXFILESIZE];
  char directoryname[MAXFILESIZE];

  if(!data->created) {
    printf("You tried to save points that were never created.\n");
    return(1);
  }

  noelements = data->noelements;
  noknots = data->noknots;
  sumsides = 0;

  for(i=0;i<=MAXELEMENTTYPE;i++)
    alltypes[i] = bulktypes[i] = sidetypes[i] = 0;

  sprintf(directoryname,"%s",prefix);

  if(info) printf("Saving mesh in ElmerSolver format to directory %s.\n",
		  directoryname);

  fail = chdir(directoryname);
  if(fail) {
#ifdef MINGW32
    fail = mkdir(directoryname);
#else
    fail = mkdir(directoryname,0700);
#endif
    if(fail) {
      printf("Could not create a result directory!\n");
      return(1);
    }
    else {
      chdir(directoryname);
    }
  }
  else {
    printf("Reusing an existing directory\n");
  }

  sprintf(filename,"%s","mesh.nodes");
  out = fopen(filename,"w");

  if(info) printf("Saving %d coordinates to %s.\n",noknots,filename);  
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(2);
  }
  
  if(data->dim == 1) {
    sprintf(outstyle,"%%d %%d %%.%dlg 0.0 0.0\n",decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,i,-1,data->x[i],data->y[i]);
  }
  if(data->dim == 2) {
    sprintf(outstyle,"%%d %%d %%.%dlg %%.%dlg 0.0\n",decimals,decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,i,-1,data->x[i],data->y[i]);
  }
  else if(data->dim == 3) {
    sprintf(outstyle,"%%d %%d %%.%dlg %%.%dlg %%.%dlg\n",decimals,decimals,decimals);
    for(i=1; i <= noknots; i++) 
      fprintf(out,outstyle,i,-1,data->x[i],data->y[i],data->z[i]);    
  }

  fclose(out);

  sprintf(filename,"%s","mesh.elements");
  out = fopen(filename,"w");
  if(info) printf("Saving %d element topologies to %s.\n",noelements,filename);
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(3);
  }

  for(i=1;i<=noelements;i++) {
    elemtype = data->elementtypes[i];

    fprintf(out,"%d %d %d",i,data->material[i],elemtype);

    if(data->pelems) {
      j = data->pelemtypes[i];

      k = j; j=j/10; k=k-10*j;
      if(k!=1) fprintf(out,"n%d",k);

      k = j; j=j/10; k=k-10*j;
      if(k!=0) fprintf(out,"e%d",k);

      k = j; j=j/10; k=k-10*j;
      if(k!=0) fprintf(out,"f%d",k);

      k = j; j=j/10; k=k-10*j;
      if(k!=0) fprintf(out,"d%d",k);

      k = j; j=j/100; k=k-100*j;
      if(k!=0) fprintf(out,"b%d",k);

      k = j; j=j/100; k=k-100*j;
      if(k!=0) fprintf(out,"p%d",k);
    }
      
    bulktypes[elemtype] += 1;
    nodesd2 = elemtype%100;
    for(j=0;j < nodesd2;j++) 
      fprintf(out," %d",data->topology[i][j]);
    fprintf(out,"\n");          
  }
  fclose(out);


  sprintf(filename,"%s","mesh.boundary");
  out = fopen(filename,"w");
  if(info) printf("Saving boundary elements to %s.\n",filename);
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(4);
  }

  sumsides = 0;
  for(j=0;j < MAXBOUNDARIES;j++) {

    if(bound[j].created == FALSE) continue;
    if(bound[j].nosides == 0) continue;

    for(i=1; i <= bound[j].nosides; i++) {
      GetElementSide(bound[j].parent[i],bound[j].side[i],bound[j].normal[i],data,ind,&sideelemtype); 
      sumsides++;

      fprintf(out,"%d %d %d %d ",
	      sumsides,bound[j].types[i],bound[j].parent[i],bound[j].parent2[i]);
      fprintf(out,"%d",sideelemtype);

      sidetypes[sideelemtype] += 1;
      nodesd1 = sideelemtype%100;
      for(l=0;l<nodesd1;l++)
	fprintf(out," %d",ind[l]);
      fprintf(out,"\n");
    }

    /* Save Discontinuous boundaries */
    for(i=1; i <= bound[j].nosides; i++) {
      if(!bound[j].parent2[i] || !bound[j].discont[i]) continue;

      GetElementSide(bound[j].parent2[i],bound[j].side2[i],-bound[j].normal[i],data,ind,&sideelemtype); 
      sumsides++;

      fprintf(out,"%d %d %d %d ",
	      sumsides,bound[j].discont[i],bound[j].parent2[i],bound[j].parent[i]);
      fprintf(out,"%d ",sideelemtype);
      sidetypes[sideelemtype] += 1;
      nodesd1 = sideelemtype%100;
      for(l=0;l<nodesd1;l++)
	fprintf(out,"%d ",ind[l]);
      fprintf(out,"\n");

      /* Save additional connections that arise at the discontinous boundary */
      sideelemtype = 100 + 2 * nodesd1;
      sumsides++;
      fprintf(out,"%d 0 0 0 %d ",sumsides,sideelemtype);
      for(l=0;l<nodesd1;l++)
	fprintf(out,"%d ",ind[l]);
      GetElementSide(bound[j].parent[i],bound[j].side[i],bound[j].normal[i],data,ind,&sideelemtype);       
      for(l=0;l<nodesd1;l++)
	fprintf(out,"%d ",ind[l]);
      fprintf(out,"\n");      
    }
  }

  if(data->periodicexist) {
    int *indxper,periodictype;
    indxper = data->periodic;

    periodictype = 0;
    for(j=0;j < MAXBOUNDARIES;j++) 
      for(i=1; i <= bound[j].nosides; i++) 
	if(bound[j].types[i] > periodictype) periodictype = bound[j].types[i];
    periodictype++;

    k = 0;
 
    for(i=1; i <= data->noknots; i++) {
      j = indxper[i];      

      if(i != j) {
	k++;
	sumsides++;
	sideelemtype = 102;
	fprintf(out,"%d %d %d %d %d %d %d\n",
		sumsides,periodictype,0,0,sideelemtype,i,j);
	sidetypes[sideelemtype] += 1;
      }
    }
    if(info) printf("Added %d periodic boundary conditions to boundary %d and elementtype 102.\n",
		    k,periodictype);
  }


  fclose(out);

  tottypes = 0;
  for(i=0;i<=MAXELEMENTTYPE;i++) {
    alltypes[i] = bulktypes[i] + sidetypes[i];
    if(alltypes[i]) tottypes++;
  }

  sprintf(filename,"%s","mesh.header");
  out = fopen(filename,"w");
  printf("Saving header info to %s.\n",filename);  
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(4);
  }
  fprintf(out,"%-6d %-6d %-6d\n",
	  noknots,noelements,sumsides);
  fprintf(out,"%-6d\n",tottypes);
  for(i=0;i<=MAXELEMENTTYPE;i++) {
    if(alltypes[i]) 
      fprintf(out,"%-6d %-6d\n",i,bulktypes[i]+sidetypes[i]);
  }

  fclose(out);

  chdir("..");

  return(0);
}



int ElmerToElmerMapQuick(struct FemType *data1,struct FemType *data2,
			 char *mapfile,int info)
/* Requires that the mapping matrix is provided in a external file. */
{
  int i,j,k,l,idx,mink,maxk,unknowns;
  Real weight;
  FILE *out;

  if ((out = fopen(mapfile,"r")) == NULL) {
    printf("The opening of the mapping file %s wasn't succesfull!\n",
	   mapfile);
    return(1);
  }

  if(info) printf("Mapping results utilizing matrix in file %s.\n",mapfile);

  mink = MAXDOFS-1;
  maxk = 1;
  for(k=1;k<MAXDOFS;k++)
    if(data1->edofs[k]) {
      if(k < mink) mink = k;
      if(k > maxk) maxk = k;
      CreateVariable(data2,k,data1->edofs[k],
		     0.0,data1->dofname[k],FALSE);
    }

  for(j=1;j <= data2->noknots;j++) {
    fscanf(out,"%d",&idx);
    for(i=0;i<4;i++) {
      fscanf(out,"%d",&idx);
      fscanf(out,"%le",&weight);
      for(k=mink;k <= maxk;k++) 
	if(unknowns = data1->edofs[k]) {
	  for(l=1;l<=unknowns;l++)
	    data2->dofs[k][unknowns*(j-1)+l] += 
	      weight * data1->dofs[k][unknowns*(idx-1)+l];
	}
    }
  }
  return(0);
}


int ElmerToElmerMap(struct FemType *data1,struct FemType *data2,int info)
/* Maps Elmer results to another Elmer file. 
   Does not need the mapping a'priori. 
   */
{
  Real x1,x2,y1,y2;
  Real *xmin,*xmax;
  Real *ymin,*ymax;
  Real eta,xi;
  Real shapefunc1[MAXNODESD2],shapeder1[DIM*MAXNODESD2];
  Real coord1[MAXNODESD2],tiny;
  int elemno,ind1[MAXNODESD2];
  int *ymaxi;
  int i1,i2,j1,j2,hit,i,j,k,l;
  int mink,maxk,unknowns;
  int noelems1,noknots2;
  int material1;
  long tests;

  tests = 0;

  if(info) printf("Performing Elmer to Elmer mapping.\n");

  noelems1 = data1->noelements;
  noknots2 = data2->noknots;

  mink = MAXDOFS-1;
  maxk = 1;
  for(k=1;k<MAXDOFS;k++)
    if(data1->edofs[k]) {
      if(k < mink) mink = k;
      if(k > maxk) maxk = k;
      CreateVariable(data2,k,data1->edofs[k],
		     0.0,data1->dofname[k],FALSE);
    }

  xmin = Rvector(1,noelems1);
  xmax = Rvector(1,noelems1);
  ymin = Rvector(1,noelems1);
  ymax = Rvector(1,noelems1);

  ymaxi = ivector(1,noelems1);

  for(j1=1;j1<=noelems1;j1++) {
    xmax[j1] = xmin[j1] = data1->x[data1->topology[j1][0]];
    ymax[j1] = ymin[j1] = data1->y[data1->topology[j1][0]];

    for(i1=1;i1<4;i1++) {
      x1 = data1->x[data1->topology[j1][i1]];
      if (x1 < xmin[j1]) xmin[j1] = x1;
      if (x1 > xmax[j1]) xmax[j1] = x1;

      y1 = data1->y[data1->topology[j1][i1]];
      if (y1 < ymin[j1]) ymin[j1] = y1;
      if (y1 > ymax[j1]) ymax[j1] = y1;
    }
  }

  /* ymaxi must be ordered so that it points to the elements of 
     ymax in increasing order. In rectangular structures mesh 
     this is automatically the case. */
  if(info) printf("Ordering elements\n");
  for(j1=1;j1<=noelems1;j1++)
    ymaxi[j1] = j1;
#if 0
  /* This does not seem to function as intended. */
  indexx(noelems1,ymax,ymaxi);
#endif
  tiny = 1.0e-10*fabs(ymax[1]-ymax[noelems1]);

  j1 = 1;

  for(j2=1;j2<=noknots2;j2++) {

    x2 = data2->x[j2];
    y2 = data2->y[j2];

    /* Find first possible element using xmax */
    while(j1<noelems1 && ymax[ymaxi[j1]] < y2-tiny) 
      {j1++; tests++;} 
    while(j1>1 && ymax[ymaxi[j1]-1] > y2-tiny) 
      {j1--; tests++;}
    
  omstart:
    
    hit = FALSE;
    do {
      tests++;
      if(j1 > noelems1) break;
      elemno = ymaxi[j1];

      if(ymin[elemno] > y2+tiny) break;

      if(xmax[elemno] > x2-tiny  &&  xmin[elemno] < x2+tiny) 
	hit = TRUE;
      else 
	j1++;
      if(j1 > noelems1) break;
    }
    while (hit == FALSE);

    if(hit == FALSE) {
      if(j1 > noelems1) j1=noelems1;
      printf("No hits for element %d at (%.3lg,%.3lg)\n",j2,x2,y2);
      printf("j1 = %d  noelems1 = %d\n",j1,noelems1);
    }
    else {
      GetElementInfo(j1,data1,coord1,ind1,&material1);
      hit = GlobalToLocalD2(coord1,x2,y2,&xi,&eta);
      if(hit == FALSE) {
	j1++;
	goto omstart;
      }
      else {
	Squad404(&xi,&eta,shapefunc1,shapeder1);
	for(k=mink;k <= maxk;k++) 
	  if(unknowns = data1->edofs[k]) {
	    for(l=1;l<=unknowns;l++)
	      for(i=0;i<4;i++)
		data2->dofs[k][unknowns*(j2-1)+l] += 
		  shapefunc1[i] * data1->dofs[k][unknowns*(ind1[i]-1)+l];
	  }
      }
    }
  }
  if(info) printf("Mapped %d knots with %.3lg average trials.\n",
		  noknots2,(1.0*tests)/noknots2);

  return(0);
}




int PartitionSimple(struct FemType *data,int dimpart[],int dimper[],
		    int partorder, Real corder[],int info)
{
  int i,j,k,ind;
  int noknots, noelements,nonodes,elemsinpart,periodic;
  int partitions1, partitions2, partitions3,partitions;
  int *indx,*part1,*part2,*part3,*nopart,*inpart;
  Real xmax,xmin,ymax,ymin,zmax,zmin,arrange0;
  Real *arrange;
  Real x,y,z,cx,cy,cz,dx,dy,dz;
  
  partitions1 = dimpart[0];
  partitions2 = dimpart[1];
  partitions3 = dimpart[2];
  if(data->dim < 3) partitions3 = 1;
  partitions = partitions1 * partitions2 * partitions3;

  if(partitions1 < 2 && partitions2 < 2 && partitions3 < 2) {
    printf("No partitions to make!\n");
    return(1);
  }

  if(partitions >= data->noelements) {
    printf("There must be fever partitions than elements (%d vs %d)!\n",
	   partitions,data->noelements);
    return(2);
  }
    
  if(!data->partitionexist) {
    data->partitionexist = TRUE;
    data->elempart = Ivector(1,data->noelements);
    data->nodepart = Ivector(1,data->noknots);
    data->nopartitions = partitions;
  }
  inpart = data->elempart;

  periodic = data->periodicexist;
  if(periodic) {
    xmin = xmax = data->x[1];
    ymin = ymax = data->y[1];
    if(data->dim > 2) zmin = zmax = data->z[1];
    else zmin = zmax = 0.0;
    for(i=1;i<=data->noknots;i++) {
      if(xmin > data->x[i]) xmin = data->x[i];
      if(xmax < data->x[i]) xmax = data->x[i];
      if(ymin > data->y[i]) ymin = data->y[i];
      if(ymax < data->y[i]) ymax = data->y[i];
      if(data->dim > 2) {
	if(zmin > data->z[i]) zmin = data->z[i];
	if(zmax < data->z[i]) zmax = data->z[i];
      }
    }

    dx = xmax-xmin;
    dy = ymax-ymin;
    if(data->dim > 2) dz = zmax-zmin;
  }

  nopart = Ivector(1,partitions);
  noelements = data->noelements;
  noknots = data->noknots;

  if(info) printf("\nMaking a simple partitioning for %d elements in %d-dimensions.\n",
		  noelements,data->dim);

  arrange = Rvector(1,noelements);
  indx = Ivector(1,noelements);

  if(partorder) {
    cx = corder[0];
    cy = corder[1];
    cz = corder[2];    
  }
  else {
    cx = 1.0;
    cy = 0.01;
    cz = 0.0001;
  }

  z = 0.0;

  if(partitions1 > 1) {
    if(info) printf("Ordering 1st direction with (%.3lg*x + %.3lg*y + %.3lg*z)\n",cx,cy,cz);


    part1 = Ivector(1,noelements);

    if(periodic) arrange0 = cx * xmax + 0.5 * (cy*(ymin + ymax) + cz*(zmin + zmax));

    for(j=1;j<=noelements;j++) {

      nonodes = data->elementtypes[j]%100;
      x = y = z = 0.0;
      for(i=0;i<nonodes;i++) {
	k = data->topology[j][i];
	x += data->x[k];
	y += data->y[k];
	if(data->dim==3) z += data->z[k];
      }
      arrange[j] = (cx*x + cy*y + cz*z) / nonodes;
      if(periodic && dimper[0]) {
	arrange[j] += 0.5*arrange0/partitions1;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noelements,arrange,indx);
    
    for(i=1;i<=noelements;i++) 
      part1[indx[i]] = (i*partitions1-1)/noelements+1;
  } 
  else {
    part1 = Ivector(1,noelements);
    for(j=1;j<=noelements;j++) 
      part1[j] = 1;
  }


  /* Partition in the 2nd direction taking into account the 1st direction */
  if(partitions2 > 1 || partitions3 > 1) 
    part2 = Ivector(1,noelements);

  if(partitions2 > 1) {
    if(info) printf("Ordering in the 2nd direction.\n");

    if(periodic) arrange0 = cx * ymax + 0.5 * (-cy*(xmin + xmax) + cz*(zmin + zmax));

    for(j=1;j<=noelements;j++) {
      nonodes = data->elementtypes[j]%100;
      x = y = z = 0.0;
      for(i=0;i<nonodes;i++) {
	k = data->topology[j][i];
	x += data->x[k];
	y += data->y[k];
	if(data->dim==3) z += data->z[k];
      }
      arrange[j] = (-cy*x + cx*y + cz*z) / nonodes;
      if(dimper[1]) {
	arrange[j] += 0.5*arrange0/partitions2;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noelements,arrange,indx);
    
    for(i=1;i<=partitions;i++)
      nopart[i] = 0;
    
    elemsinpart = noelements / (partitions1*partitions2);
    for(i=1;i<=noelements;i++) {
      j = 0;
      ind = indx[i];
      do {
	j++;
	k = (part1[ind]-1) * partitions2 + j;
      }
      while(nopart[k] >= elemsinpart && j < partitions2);
      
      nopart[k] += 1;
      part2[ind] = j;
    }
  }  
  else if(partitions3 > 1) {
    for(j=1;j<=noelements;j++) 
      part2[j] = 1;
  }


  /* Partition in the 3rd direction taking into account the 1st and 2nd direction */
  if(partitions3 > 1) {
    if(info) printf("Ordering in the 3rd direction.\n");
    part3 = Ivector(1,noelements);

    if(periodic) arrange0 = cx * zmax + 0.5 * (-cz*(xmin + xmax) - cy*(ymin + ymax));

    for(j=1;j<=noelements;j++) {
      nonodes = data->elementtypes[j]%100;
      x = y = z = 0.0;
      for(i=0;i<nonodes;i++) {
	k = data->topology[j][i];
	x += data->x[k];
	y += data->y[k];
	if(data->dim==3) z += data->z[k];
      }
      arrange[j] = (-cz*x - cy*y + cx*z) / nonodes;
      if(dimper[2]) {
	arrange[j] += 0.5*arrange0/partitions3;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noelements,arrange,indx);

    for(i=1;i<=partitions;i++)
      nopart[i] = 0;
    
    elemsinpart = noelements / (partitions1*partitions2*partitions3);
    for(i=1;i<=noelements;i++) {
      j = 0;
      ind = indx[i];
      do {
	j++;
	k = (part1[ind]-1)*partitions2*partitions3 + (part2[ind]-1)*partitions3 + j;
      }
      while(nopart[k] >= elemsinpart && j < partitions3);
    
      nopart[k] += 1;
      part3[ind] = j;
    }
  }
  
  if(0) for(i=1;i<=noelements;i++) 
    printf("i=%d  part=%d\n",i,part3[ind]);

  /* Set the default partition for each element. */
  if(partitions3 > 1) { 
    for(i=1;i<=noelements;i++) 
      inpart[i] = (part1[i]-1)*partitions2*partitions3 + (part2[i]-1)*partitions3 + part3[i];
  }
  else if(partitions2 > 1) {
    for(i=1;i<=noelements;i++) 
      inpart[i] = (part1[i]-1)*partitions2 + part2[i];
  }    
  else {
    for(i=1;i<=noelements;i++) 
      inpart[i] = part1[i];
  }

  free_Rvector(arrange,1,noelements);
  free_Ivector(indx,1,noelements);
  free_Ivector(part1,1,noelements);
  if(partitions2 > 1) free_Ivector(part2,1,noelements);
  if(partitions3 > 1) free_Ivector(part3,1,noelements);

  if(info) printf("Succesfully made a simple partition\n");

  return(0);
}


int PartitionSimpleNodes(struct FemType *data,int dimpart[],int dimper[],
			 int partorder, Real corder[],int info)
{
  int i,j,k,ind,hit;
  int noknots, noelements,nonodes,nodesinpart,periodic;
  int partitions1, partitions2, partitions3,partitions;
  int *indx,*part1,*part2,*part3,*nopart,*inpart;
  Real xmax,xmin,ymax,ymin,zmax,zmin,arrange0;
  Real *arrange;
  Real x,y,z,cx,cy,cz,dx,dy,dz;
  
  partitions1 = dimpart[0];
  partitions2 = dimpart[1];
  partitions3 = dimpart[2];
  if(data->dim < 3) partitions3 = 1;
  partitions = partitions1 * partitions2 * partitions3;

  if(partitions1 < 2 && partitions2 < 2 && partitions3 < 2) {
    printf("No partitions to make!\n");
    return(1);
  }

  if(partitions >= data->noelements) {
    printf("There must be fever partitions than elements (%d vs %d)!\n",
	   partitions,data->noelements);
    return(2);
  }
    
  if(!data->partitionexist) {
    data->partitionexist = TRUE;
    data->elempart = Ivector(1,data->noelements);
    data->nodepart = Ivector(1,data->noknots);
    data->nopartitions = partitions;
  }
  inpart = data->nodepart;

  periodic = data->periodicexist;
  if(periodic) {
    xmin = xmax = data->x[1];
    ymin = ymax = data->y[1];
    if(data->dim > 2) zmin = zmax = data->z[1];
    else zmin = zmax = 0.0;
    for(i=1;i<=data->noknots;i++) {
      if(xmin > data->x[i]) xmin = data->x[i];
      if(xmax < data->x[i]) xmax = data->x[i];
      if(ymin > data->y[i]) ymin = data->y[i];
      if(ymax < data->y[i]) ymax = data->y[i];
      if(data->dim > 2) {
	if(zmin > data->z[i]) zmin = data->z[i];
	if(zmax < data->z[i]) zmax = data->z[i];
      }
    }

    dx = xmax-xmin;
    dy = ymax-ymin;
    if(data->dim > 2) dz = zmax-zmin;
  }

  nopart = Ivector(1,partitions);
  noelements = data->noelements;
  noknots = data->noknots;

  if(info) printf("\nMaking a simple partitioning for %d nodes in %d-dimensions.\n",
		  noknots,data->dim);

  arrange = Rvector(1,noknots);
  indx = Ivector(1,noknots);

  if(partorder) {
    cx = corder[0];
    cy = corder[1];
    cz = corder[2];    
  }
  else {
    cx = 1.0;
    cy = 0.01;
    cz = 0.0001;
  }

  z = 0.0;

  if(partitions1 > 1) {
    if(info) printf("Ordering 1st direction with (%.3lg*x + %.3lg*y + %.3lg*z)\n",cx,cy,cz);

    part1 = Ivector(1,noknots);

    if(periodic) arrange0 = cx * xmax + 0.5 * (cy*(ymin + ymax) + cz*(zmin + zmax));

    for(j=1;j<=noknots;j++) {
      k = data->topology[j][i];      
      x = data->x[j];
      y = data->y[j];
      if(data->dim==3) z = data->z[j];

      arrange[j] = cx*x + cy*y + cz*z;
      if(periodic && dimper[0]) {
	arrange[j] += 0.5*arrange0/partitions1;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noknots,arrange,indx);
    
    for(i=1;i<=noknots;i++) 
      part1[indx[i]] = (i*partitions1-1)/noknots+1;
  } 
  else {
    part1 = Ivector(1,noknots);
    for(j=1;j<=noknots;j++) 
      part1[j] = 1;
  }


  /* Partition in the 2nd direction taking into account the 1st direction */
  if(partitions2 > 1 || partitions3 > 1) 
    part2 = Ivector(1,noknots);

  if(partitions2 > 1) {
    if(info) printf("Ordering in the 2nd direction.\n");

    if(periodic) arrange0 = cx * ymax + 0.5 * (-cy*(xmin + xmax) + cz*(zmin + zmax));

    for(j=1;j<=noknots;j++) {
      x = data->x[j];
      y = data->y[j];
      if(data->dim==3) z = data->z[j];

      arrange[j] = (-cy*x + cx*y + cz*z);
      if(dimper[1]) {
	arrange[j] += 0.5*arrange0/partitions2;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noknots,arrange,indx);
    
    for(i=1;i<=partitions;i++)
      nopart[i] = 0;
    
    nodesinpart = noknots / (partitions1*partitions2);
    for(i=1;i<=noknots;i++) {
      j = 0;
      ind = indx[i];
      do {
	j++;
	k = (part1[ind]-1) * partitions2 + j;
      }
      while(nopart[k] >= nodesinpart && j < partitions2);
      
      nopart[k] += 1;
      part2[ind] = j;
    }
  }  
  else if(partitions3 > 1) {
    for(j=1;j<=noknots;j++) 
      part2[j] = 1;
  }


  /* Partition in the 3rd direction taking into account the 1st and 2nd direction */
  if(partitions3 > 1) {
    if(info) printf("Ordering in the 3rd direction.\n");
    part3 = Ivector(1,noknots);

    if(periodic) arrange0 = cx * zmax + 0.5 * (-cz*(xmin + xmax) - cy*(ymin + ymax));

    for(j=1;j<=noknots;j++) {
      x = data->x[j];
      y = data->y[j];
      if(data->dim==3) z = data->z[j];

      arrange[j] = -cz*x - cy*y + cx*z;
      if(dimper[2]) {
	arrange[j] += 0.5*arrange0/partitions3;
	if(arrange[j] > arrange0) arrange[j] -= arrange0;
      }
    }
    SortIndex(noknots,arrange,indx);

    for(i=1;i<=partitions;i++)
      nopart[i] = 0;
    
    nodesinpart = noknots / (partitions1*partitions2*partitions3);
    for(i=1;i<=noknots;i++) {
      j = 0;
      ind = indx[i];
      do {
	j++;
	k = (part1[ind]-1)*partitions2*partitions3 + (part2[ind]-1)*partitions3 + j;
      }
      while(nopart[k] >= nodesinpart && j < partitions3);
    
      nopart[k] += 1;
      part3[ind] = j;
    }
  }
  
  if(0) for(i=1;i<=noknots;i++) 
    printf("i=%d  part=%d\n",i,part3[ind]);

  /* Set the default partition for each node. */
  if(partitions3 > 1) { 
    for(i=1;i<=noknots;i++) 
      inpart[i] = (part1[i]-1)*partitions2*partitions3 + (part2[i]-1)*partitions3 + part3[i];
  }
  else if(partitions2 > 1) {
    for(i=1;i<=noknots;i++) 
      inpart[i] = (part1[i]-1)*partitions2 + part2[i];
  }    
  else {
    for(i=1;i<=noknots;i++) 
      inpart[i] = part1[i];
  }

  

  for(j=1;j<=noelements;j++) {
    nonodes = data->elementtypes[j]%100;
    
    /* First check whether the first node is majority */
    hit = 0;
    k = inpart[data->topology[j][0]];
    for(i=0;i<nonodes;i++) {
      ind = data->topology[j][i];
      if(k == inpart[ind]) hit++;
    }

    /* The go through all the candidates */
    if(2*hit >= nonodes) {
      data->elempart[j] = k;
    }
    else {
      for(i=1;i<=partitions;i++)
	nopart[i] = 0;
      for(i=0;i<nonodes;i++) {
	ind = inpart[data->topology[j][i]];
	nopart[ind] += 1;
      }
      hit = 0;
      for(i=1;i<=partitions;i++)
	if(nopart[i] > hit) {
	  hit = nopart[i];
	  data->elempart[j] = i;
	}
    }
  }

  /* Check that every element belongs to some partition */
  j=0;
  for(i=1;i<=data->noelements;i++)
    if(data->elempart[i] < 1 || data->elempart[i] > partitions) j++;
  if(j) printf("Bad partitioning: %d elements do not belong anywhere!\n",j);

   /* Check that every node belongs to some partition */
  k=0;
  for(i=1;i<=data->noknots;i++)
    if(data->nodepart[i] < 1 || data->nodepart[i] > partitions) k++;
  if(k) printf("Bad partitioning: %d nodes do not belong anywhere!\n",k);
    
  if(j+k) exit;

  if(0) printf("Deallocating vectors needed for reordering.\n");
  free_Rvector(arrange,1,noknots);
  free_Ivector(indx,1,noknots);
  free_Ivector(part1,1,noknots);
  if(partitions2 > 1) free_Ivector(part2,1,noknots);
  if(partitions3 > 1) free_Ivector(part3,1,noknots);

  if(info) printf("Succesfully made a simple partition of nodes\n");



  return(0);
}




#if PARTMETIS 
int PartitionMetis(struct FemType *data,int partitions,int info)
{
  int i,j,periodic, highorder, noelements, noknots, ne, nn, sides;
  int nodesd1, nodesd2, etype, numflag, nparts, edgecut;
  int *neededby,*metistopo;
  int *indxper,*inpart,*epart;

  if(info) printf("\nMaking a Metis partitioning for %d elements in %d-dimensions.\n",
		  data->noelements,data->dim);

  if(!data->partitionexist) {
    data->partitionexist = TRUE;
    data->elempart = Ivector(1,data->noelements);
    data->nodepart = Ivector(1,data->noknots);
    data->nopartitions = partitions;
  }
  inpart = data->elempart;

  /* Are there periodic boundaries */
  periodic = data->periodicexist;
  if(periodic) {
    if(info) printf("There seems to be peridic boundaries\n");
    indxper = data->periodic;
  }

  highorder = FALSE;
  noelements = data->noelements;
  noknots = data->noknots;
  
  ne = noelements;
  nn = noknots;

  sides = data->elementtypes[1]/100;
  for(i=1;i<=noelements;i++) {
    if(sides != data->elementtypes[i]/100) {
      printf("Metis partition requires that all the elements are of the same type!\n");
      data->partitionexist = FALSE;
      return(2);
    }
    if(sides == 3 && data->elementtypes[i]%100 > 3) highorder = TRUE;
    if(sides == 4 && data->elementtypes[i]%100 > 4) highorder = TRUE;
    if(sides == 5 && data->elementtypes[i]%100 > 4) highorder = TRUE;
    if(sides == 8 && data->elementtypes[i]%100 > 8) highorder = TRUE;
  }

  if(info && highorder) printf("There are at least some higher order elements\n");

  if(sides == 3) {
    if (info) printf("The mesh seems to consist of triangles\n");
    nodesd2 = 3;
    nodesd1 = 2;
    etype = 1;
  }
  else if(sides == 4)  {
    if(info) printf("The mesh seems to consist of quadrilaterals\n");
    nodesd2 = 4;
    nodesd1 = 2;
    etype = 4;
  }
  else if(sides == 5) {
    if(info) printf("The mesh seems to consist of tetrahedra\n");
    nodesd2 = 4;
    nodesd1 = 3;
    etype = 2;
  }
  else if(sides == 8) {
    if(info) printf("The mesh seems to consist of bricks\n");
    nodesd2 = 8;
    nodesd1 = 4;
    etype = 3;
  }

  /* perucliar length is because needed for two cases */ 
  neededby = Ivector(0,noknots);
  metistopo = Ivector(0,noelements*nodesd2-1);
  epart = Ivector(0,noelements-1);

  numflag = 0;
  nparts = partitions;
  
  for(i=1;i<=noknots;i++) 
    neededby[i] = 0;

  if(periodic) {
    for(i=1;i<=noelements;i++) 
      for(j=0;j<nodesd2;j++) 
	neededby[indxper[data->topology[i][j]]] = 1;
  }
  else {
    for(i=1;i<=noelements;i++) 
      for(j=0;j<nodesd2;j++) 
	neededby[data->topology[i][j]] = 1;
  }

  j = 0;
  for(i=1;i<=noknots;i++) 
    if(neededby[i]) 
      neededby[i] = ++j;
  nn = j;
    
  if(periodic) {
    for(i=0;i<noelements;i++) 
      for(j=0;j<nodesd2;j++) 
	metistopo[nodesd2*i+j] = neededby[indxper[data->topology[i+1][j]]]-1;
  }    
  else {
    for(i=0;i<noelements;i++) 
      for(j=0;j<nodesd2;j++) 
	metistopo[nodesd2*i+j] = neededby[data->topology[i+1][j]]-1;    
  }

  if(info) printf("Using %d nodes of %d possible nodes in the Metis graph\n",nn,noknots);


  METIS_PartMeshNodal(&ne,&nn,metistopo,&etype,
		      &numflag,&nparts,&edgecut,epart,neededby);

  /* Set the partition given by Metis for each element. */
  for(i=1;i<=noelements;i++) {
    inpart[i] = epart[i-1]+1;
    if(inpart[i] < 1 || inpart[i] > partitions) 
      printf("Invalid partition %d for element %d\n",inpart[i],i);
  }
  /* Set the partition given by Metis for each node. */
  for(i=1;i<=noknots;i++) {
    data->nodepart[i] = neededby[i-1]+1;
    if(data->nodepart[i] < 1 || data->nodepart[i] > partitions) 
      printf("Invalid partition %d for node %d\n",inpart[i],i);
  }

  free_Ivector(neededby,0,noknots);
  free_Ivector(metistopo,0,noelements*nodesd2-1);
  free_Ivector(epart,0,noelements-1);

  if(info) printf("Succesfully made a Metis partition\n");

  return(0);
}
#endif  



int OptimizePartitioning(struct FemType *data,struct BoundaryType *bound,
			 int info)
{
  int i,j,k,l,n,m,boundaryelems,noelements,partitions,ind,periodic,hit,hit2;
  int part1,part2,mam1,mam2,noknots,part,dshared,dshared0;
  int *elempart,*nodepart,*neededby,*neededtimes,*indxper,sharings;
  int nodesd2,maxneededtimes,*probnodes;
  int *nodesinpart,*elementsinpart,optimize;
  int **neededmatrix,*neededvector,**neededtable;
  Real *rpart;

  if(!data->partitionexist) {
    printf("OptimizePartitioning: this should be called only after partitioning\n");
    return(1);
  }

  optimize = TRUE;

  printf("Optimizing the partitioning for boundaries and load balancing\n");
  noknots = data->noknots;
  noelements = data->noelements;
  partitions = data->nopartitions;

  nodepart = data->nodepart;
  elempart = data->elempart;

  periodic = data->periodicexist;
  if(periodic) indxper = data->periodic;

  probnodes = Ivector(1,noknots);
  for(i=1;i<=noknots;i++)
    probnodes[i] = 0;
  
  elementsinpart = Ivector(1,partitions);
  nodesinpart = Ivector(1,partitions);
  for(i=1;i<=partitions;i++)
    nodesinpart[i] = elementsinpart[i] = 0;

  /* Set the secondary parent to be a parent also because we want all 
     internal BCs to be within the same partition. 
     Also set the nodes of the altered elements to be in the desired partition. */
  k = 0;
  do {
    k++;
    boundaryelems = 0;

    for(j=0;j < MAXBOUNDARIES;j++) {
      if(!bound[j].created) continue;
      for(i=1; i <= bound[j].nosides; i++) {
	if(bound[j].discont[i]) continue;

	mam1 = bound[j].parent[i];
	mam2 = bound[j].parent2[i];
	if(!mam1 || !mam2) continue;
	part1 = elempart[mam1];
	part2 = elempart[mam2];
	if(part1 != part2) {
	  
	  /* The first iterations check which parents is ruling */
	  if(k < 5) {
	    hit = hit2 = 0;
	    nodesd2 = data->elementtypes[mam1] % 100;
	    for(l=0;l < nodesd2;l++) {
	      ind = data->topology[mam1][l];
	      if(nodepart[ind] == part1) hit++;
	      if(nodepart[ind] == part2) hit2++;
	    }
	    nodesd2 = data->elementtypes[mam1] % 100;    
	    for(l=0;l < nodesd2;l++) {
	      ind = data->topology[mam2][l];
	      if(nodepart[ind] == part1) hit++;
	      if(nodepart[ind] == part2) hit2++;
	    }
	    
	  } else {
	    hit2 = 0;
	    hit = 1;
	  }   

	  /* Make the more ruling parent dominate the whole boundary */
	  if(hit > hit2) {
	    elempart[mam2] = part1;
	    boundaryelems++;	    
	    nodesd2 =  data->elementtypes[mam2] % 100;
	    for(l=0;l < nodesd2;l++) {
	      ind = data->topology[mam2][l];
	      nodepart[ind] = part1;
	    }
	  }	  
	  else {
	    elempart[mam1] = part2;
	    boundaryelems++;	    
	    nodesd2 =  data->elementtypes[mam1] % 100;
	    for(l=0;l < nodesd2;l++) {
	      ind = data->topology[mam1][l];
	      nodepart[ind] = part2;	    
	    }
	  }


	}
      }
    }

    printf("%d bulk elements with BCs removed from interface.\n",boundaryelems);
  } while(boundaryelems && k < 10);


  /* Check that every element belongs to some partition */
  j=0;
  for(i=1;i<=data->noelements;i++)
    if(elempart[i] < 1 || elempart[i] > partitions) j++;
  if(j) printf("Bad partitioning: %d elements do not belong anywhere!\n",j);



  neededtimes = Ivector(1,noknots);
  neededby = Ivector(1,noknots);

  for(i=1;i<=noknots;i++)
    neededby[i] = neededtimes[i] = 0;

  /* Compute to how many partitions the nodes may belong to */
  for(part=1;part<=partitions;part++) {
    for(i=1;i<=noelements;i++) {
      if(elempart[i] != part) continue;

      elementsinpart[part] += 1;

      nodesd2 =  data->elementtypes[i] % 100;
      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];
	if(periodic) ind = indxper[ind];

	if (neededby[ind] != part) {  
	  nodesinpart[part] += 1;
	  neededby[ind] = part;
	  neededtimes[ind] += 1;
	}
      }
    }
  }

  optimize = 1;


  maxneededtimes = 0;
  sharings = 0;
  for(i=1;i<=noknots;i++) {
    ind = i;
    if(periodic) ind = indxper[i];
    if(neededtimes[ind] > 1) sharings += 1;  /* neededtimes[ind] - 1; */
    if(maxneededtimes < neededtimes[ind]) maxneededtimes = neededtimes[ind];
  }
  if(info) {
    if(periodic) printf("Taking into account the periodic BCs\n");
    printf("Nodes belong to %d partitions in maximum\n",maxneededtimes);
    printf("There are %d shared nodes which is %.2lf %% of all nodes.\n",
	   sharings,(100.*sharings)/noknots);
  }
  
  
  /* Make a table showing to which all partitions the nodes belong to */
  neededtable = Imatrix(1,noknots,1,maxneededtimes);
  for(i=1;i<=noknots;i++) 
    for(j=1;j<=maxneededtimes;j++) 
      neededtable[i][j] = 0;
  
  for(i=1;i<=noknots;i++)
    neededby[i] = neededtimes[i] = 0;
  
  for(part=1;part<=partitions;part++) {
    for(i=1;i<=noelements;i++) {
      if(elempart[i] != part) continue;
      nodesd2 = data->elementtypes[i] % 100;
      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];
	
	if(periodic) ind = indxper[ind];
	if (neededby[ind] != part) {  
	  neededby[ind] = part;
	  neededtimes[ind] += 1;
	  neededtable[ind][neededtimes[ind]] = part; 
	}
      }
    }
  }
  
  /* Distribute the shared nodes as evenly as possible */
  neededmatrix = Imatrix(1,partitions,1,partitions);
  neededvector = Ivector(1,partitions);
  
  for(i=1;i<=partitions;i++)
    for(j=1;j<=partitions;j++)
      neededmatrix[i][j] = 0;
  
  for(i=1;i<=partitions;i++)
    neededvector[i] = 0;
      

  /* Make the initial distribution that points the ownerships, 
     take into account only the two first users  */  

#if 1
  for(i=1;i<=noknots;i++) {
    ind = i;
    if(periodic) ind = indxper[ind];
    
    if(neededtimes[ind] > 1) {
      j = neededtable[ind][1];
      k = neededtable[ind][2];
      if(neededmatrix[j][k] <= neededmatrix[k][j]) {
	neededby[ind] = j;
	neededmatrix[j][k] += 1;
	neededvector[j] += 1;
      }
      else {
	neededby[ind] = k;
	neededmatrix[k][j] += 1;
	neededvector[k] += 1;
      }
    }
  }
#else 
  for(i=1;i<=noknots;i++)
    neededby[i] = nodepart[i];

  for(i=1;i<=noknots;i++) {
    if(neededtimes[i] <= 1) continue;
    ind = i;
    if(periodic) ind = indxper[ind];
    for(j=1;j<=maxneededtimes;j++) 
      if(neededby[ind] == neededtable[ind][j]) {
	if(j > 2) {
	  neededtable[ind][j] = neededtable[ind][1];
	  neededtable[ind][1] = neededby[ind];
	}
      }
  }
  
  for(i=1;i<=noknots;i++) {
    ind = i;
    if(periodic) ind = indxper[ind];
    
    if(neededtimes[ind] > 1) {
      j = neededtable[ind][1];
      k = neededtable[ind][2];

      if(neededby[ind] == j) {
	neededmatrix[j][k] += 1;
	neededvector[j] += 1;
      }
      else if(neededby[ind] == k) {
	neededmatrix[k][j] += 1;
	neededvector[k] += 1;
      }
      else {
	printf("needed should be either of %d %d (%d)\n",j,k,neededby[ind]);
      }
    }
  }
#endif



optimizeownership:
  
  /* compute the first maximum deviation of shared nodes. */
  j = k = neededvector[1];
  for(i=1;i<=partitions;i++) {
    if(j < neededvector[i]) j = neededvector[i];
    if(k > neededvector[i]) k = neededvector[i];
  }
  dshared = j-k;
    
  if(info) printf("Maximum deviation in ownership %d\n",dshared);


  n = 0;
  do {
    n++;
    for(i=1;i<=noknots;i++) {
      ind = i;
      if(periodic) ind = indxper[ind];
      
      if(neededtimes[ind] > 1) {
	j = neededtable[ind][1];
	k = neededtable[ind][2];

	if(probnodes[ind]) continue;
	
	if(neededvector[j] < neededvector[k] && neededby[ind] == k) {
	  neededvector[j] += 1;
	  neededvector[k] -= 1;
	  neededby[ind] = j;
	}
	else if(neededvector[k] < neededvector[j] && neededby[ind] == j) {
	  neededvector[k] += 1;
	  neededvector[j] -= 1;
	  neededby[ind] = k;
	}
      }
    }
    
    j = k = neededvector[1];
    for(i=1;i<=partitions;i++) {
      if(j < neededvector[i]) j = neededvector[i];
      if(k > neededvector[i]) k = neededvector[i];
    }
    dshared0 = dshared;
    dshared = j-k;
    
  } while (dshared < dshared0 && n < 3);

  printf("Divided the shared nodes with %d heuristic iterations\n",n);
  
  /* Check the neededvector to be on the safe side */
  for(i=1;i<=noknots;i++) {
    k = 0;
    for(j=1;j<=neededtimes[i];j++)
      if(neededtable[i][j] == neededby[i]) k++;
    if(k>1) printf("Node %d is owned %d times by partition %d\n",i,k,neededby[i]);
  }

#if 0
  printf("Distribution of elements and nodes in the partitioning\n");
  printf("     %-10s %-10s %-10s %-10s\n","partition","elements","nodes","shared");
  for(i=1;i<=partitions;i++)
    printf("     %-10d %-10d %-10d %-10d\n",
	   i,elementsinpart[i],nodesinpart[i],neededvector[i]);
#endif


optimizesharing:

  if(info) printf("\nChecking for problematic sharings\n"); 
  m = 0;
  if(partitions > 2) do {
    int i1,i2,e1,e2,owners;
    int *elemparts;
    int **knows;

    m++;
    sharings = 0;
    e1 = e2 = 0;

    if(m == 1 && optimize == 1) {
      elemparts = Ivector(1,partitions);
      knows = Imatrix(1,partitions,1,partitions);
    }

    for(i=1;i<=noelements;i++) {

      for(j=1;j<=partitions;j++) elemparts[j] = 0;
      nodesd2 = data->elementtypes[i] % 100;

      /* Check the number of owners in an element */
      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];
	if(periodic) ind = indxper[ind];
	elemparts[neededby[ind]] = 1;
      }
      owners = 0;
      for(j=1;j<=partitions;j++)
	owners += elemparts[j]; 
 
      /* One strange owner is still ok. */
      if(owners - elemparts[elempart[i]] <= 1) continue;

      for(j=1;j<=partitions;j++) 
	for(k=1;k<=partitions;k++) 
	  knows[j][k] = 0;
      
      /* Check which partitions are related by a common node */
      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];
	if(periodic) ind = indxper[ind];

	for(l=1;l<=neededtimes[ind];l++)
	  for(k=l+1;k<=neededtimes[ind];k++) {
	    e1 = neededtable[ind][l];
	    e2 = neededtable[ind][k];
	    knows[e1][e2] = knows[e2][e1] = 1;
	  }
      }    

      /* Check if there are more complex relations:
	 i.e. two partitions are joined at an element but not at the same node. */
      hit = FALSE;
      for(j=1;j<=partitions;j++) {
	for(k=j+1;k<=partitions;k++) 
	  if(elemparts[j] && elemparts[k] && !knows[j][k]) {
	    if(info && hit) printf("Partitions %d and %d in element %d (%d owners) oddly and multiply related\n",
				   j,k,i,owners);
	    hit = TRUE;
	    i1 = j;
	    i2 = k;
	  }
      }

      if(hit) {
	e1 = e2 = 0;

	/* Count the number of nodes with wrong parents */
	for(j=0;j < nodesd2;j++) {
	  ind = data->topology[i][j];
	  if(periodic) ind = indxper[ind];

	  for(l=1;l<=neededtimes[ind];l++) {
	    if(neededtable[ind][l] == i1) e1++;
	    if(neededtable[ind][l] == i2) e2++;
	  }
	}

	/* Change the owner of those with less sharings */
	for(j=0;j < nodesd2;j++) {
	  ind = data->topology[i][j];
	  if(periodic) ind = indxper[ind];

	  for(l=1;l<=neededtimes[ind];l++) {
	    if(neededby[ind] == i1 && e1 < e2) {
	      probnodes[ind] += 1;
	      neededby[ind] = elempart[i];
	      neededvector[elempart[i]] += 1;
	      neededvector[i1] -= 1;
	    }
	    else if(neededby[ind] == i2) {
	      probnodes[ind] += 1;
	      neededby[ind] = elempart[i]; 
	      neededvector[elempart[i]] += 1;
	      neededvector[i2] -= 1;
	    }
	  }
	}
	
	sharings++;
      }
    }

#if 0
    if(info) printf("Changed the ownership of %d nodes\n",sharings);
#endif

  } while (sharings > 0 && m < 3);

  if(info) {
    if(sharings) printf("%d problematic sharings may still exist\n",sharings);
    else printf("There shouldn't be any more problematic sharings, knok, knok...\n");
  }

  /* This seems to work also iteratively */
  if(m+n>10 && optimize < 50) {
    optimize++;
    printf("\nPerforming ownership optimization round %d\n",optimize);
    goto optimizeownership;
  }

  printf("Distribution of elements and nodes after %d optimization rounds\n",optimize);
  printf("     %-10s %-10s %-10s %-10s\n","partition","elements","nodes","shared");
  for(i=1;i<=partitions;i++)
    printf("     %-10d %-10d %-10d %-10d\n",
	   i,elementsinpart[i],nodesinpart[i],neededvector[i]);


  /* Make a variable showing the owner partition */
  l = 0;
  do l++; while (data->edofs[l]);
  CreateVariable(data,l,1,0.0,"Partition",FALSE);      
  rpart = data->dofs[l];

  for(i=1;i<=noknots;i++) 
    rpart[i] = 1.0 * neededby[i];

  for(i=1;i<=noknots;i++) 
    nodepart[i] = neededby[i];
 

  free_Ivector(neededtimes,1,noknots);
  free_Ivector(elementsinpart,1,partitions);
  free_Ivector(nodesinpart,1,partitions);
  free_Imatrix(neededmatrix,1,partitions,1,partitions);
  free_Ivector(neededvector,1,partitions);
  free_Imatrix(neededtable,1,noknots,1,maxneededtimes);
  free_Ivector(probnodes,1,noknots);
  free_Ivector(neededby,1,noknots);
 
  if(info) printf("The partitioning was optimized.\n"); 
  return(0);
}


#define DEBUG 1
int SaveElmerInputPartitioned(struct FemType *data,struct BoundaryType *bound,
			      char *prefix,int decimals,int info)
/* Saves the mesh in a form that may be used as input 
   in Elmer calculations in parallel platforms. 
   */
{
  int noknots,noelements,sumsides,partitions;
  int nodesd2,nodesd1;
  int part,elemtype,sideelemtype,needednodes,neededtwice;
  int bulktypes[MAXELEMENTTYPE+1],sidetypes[MAXELEMENTTYPE+1],tottypes;
  int i,j,k,l,ind,ind2,sideind[MAXNODESD1],elemhit[MAXNODESD2];
  FILE *out,*out2;
  char filename[MAXFILESIZE],filename2[MAXFILESIZE],outstyle[MAXFILESIZE];
  char directoryname[MAXFILESIZE],subdirectoryname[MAXFILESIZE];
  int *neededby,*neededtimes,**neededtable,*elempart,*indxper;
  int *elementsinpart,*pairsinpart,*sidesinpart;
  int maxneededtimes,periodic,periodictype,bcneeded,trueparent,*ownerpart;

#if DEBUG
  int *usedelem;
  usedelem = Ivector(1,data->noelements);
  for(i=1;i<=data->noelements;i++)
    usedelem[i] = 0;
#endif

  if(!data->created) {
    printf("You tried to save points that were never created.\n");
    return(1);
  }

  partitions = data->nopartitions;
  if(!partitions) {
    printf("Tried to save partiotioned format without partitions!\n");
    return(2);
  }
  elempart = data->elempart;
  ownerpart = data->nodepart;

  /* Are there periodic boundaries */
  periodic = data->periodicexist;
  if(periodic) {
    if(info) printf("There seems to be peridic boundaries\n");
    indxper = data->periodic;
  }
 
  noelements = data->noelements;
  noknots = data->noknots;
  
  elementsinpart = Ivector(1,partitions);
  pairsinpart = Ivector(1,partitions);
  sidesinpart = Ivector(1,partitions);
  neededby = Ivector(1,noknots);
  neededtimes = Ivector(1,noknots);

  for(i=1;i<=noknots;i++)
    neededby[i] = neededtimes[i] = 0;


  sprintf(directoryname,"%s",prefix);
  sprintf(subdirectoryname,"%s.%d","partitioning",partitions);

#ifdef MINGW32
  mkdir(directoryname);
#else
  mkdir(directoryname,0700);
#endif
  chdir(directoryname);
#ifdef MINGW32
  mkdir(subdirectoryname);
#else
  mkdir(subdirectoryname,0700);
#endif

  chdir(subdirectoryname);

  if(info) printf("Saving mesh in ElmerSolver format to %d partitions in %s/%s.\n",
		  partitions,directoryname,subdirectoryname);

  for(i=1;i<=partitions;i++)
    elementsinpart[i] = pairsinpart[i] = sidesinpart[i] = 0;


  /*********** part.n.elements *********************/
  /* Save elements in all partitions and 
     memorize how many times the nodes are needed */
  for(part=1;part<=partitions;part++) {

    if(0) printf("partition %d of %d\n",part,partitions);

    sprintf(filename,"%s.%d.%s","part",part,"elements");
    out = fopen(filename,"w");

    for(i=1;i<=noelements;i++) {
      if(elempart[i] != part) continue;

      elementsinpart[part] += 1;
      elemtype = data->elementtypes[i];

      if(data->pelems) {
	if(data->pelemtypes[i] > 0) 
	  fprintf(out,"%d %d %dp%d ",i,data->material[i],elemtype,data->pelemtypes[i]);
	else 
	  fprintf(out,"%d %d %d ",i,data->material[i],elemtype);
      }
      else {
	fprintf(out,"%d %d %d ",i,data->material[i],elemtype);
      }
      nodesd2 = elemtype%100;

#if DEBUG
      if(elemtype < 303) printf("Invalid elementtype (%d)!\n",elemtype);
      if(usedelem[i]) printf("elem %d already used by partition %d\n",i,usedelem[i]);
      else usedelem[i] = part;
#endif

      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];
	fprintf(out,"%d ",ind);
	if(periodic) ind = indxper[ind];
	if (neededby[ind] != part) {  
	  neededby[ind] = part;
	  neededtimes[ind] += 1;
	}
      }
      fprintf(out,"\n");    
    }
    fclose(out);
  }

#if DEBUG
  for(i=1;i<=data->noelements;i++)
    if(usedelem[i] ==0 || usedelem[i] != elempart[i]) printf("element %d not saved in any partition\n",i);
#endif

  /* Make a table showing to which all partitions the nodes belong to */
  maxneededtimes = 0;
  for(i=1;i<=noknots;i++) {
    if(maxneededtimes < neededtimes[i]) maxneededtimes = neededtimes[i];
#if DEBUG
    if(neededtimes[i] < 1) printf("Node %d is not needed at all!\n",i);
#endif
  }

  printf("Nodes belong to %d partitions in maximum\n",maxneededtimes);
  
  neededtable = Imatrix(1,noknots,1,maxneededtimes);
  for(i=1;i<=noknots;i++) 
    for(j=1;j<=maxneededtimes;j++) 
      neededtable[i][j] = 0;
  
  for(i=1;i<=noknots;i++)
    neededby[i] = neededtimes[i] = 0;


  /* Make a table showing to what partitions a node belongs to */
  for(part=1;part<=partitions;part++) {
    for(i=1;i<=noelements;i++) {
      if(elempart[i] != part) continue;
      nodesd2 = data->elementtypes[i] % 100;
      for(j=0;j < nodesd2;j++) {
	ind = data->topology[i][j];

	if(periodic) ind = indxper[ind];
	if (neededby[ind] != part) {  
	  neededby[ind] = part;
	  neededtimes[ind] += 1;
	  neededtable[ind][neededtimes[ind]] = part; 
	}
      }
    }
  }


  /* The output format is the same for all partitions */
  if(data->dim == 2) 
    sprintf(outstyle,"%%d %%d %%.%dlg %%.%dlg 0.0\n",decimals,decimals);
  else 
    sprintf(outstyle,"%%d %%d %%.%dlg %%.%dlg %%.%dlg\n",decimals,decimals,decimals);


  /*********** part.n.nodes  and  part.n.shared *********************/
  for(part=1;part<=partitions;part++) {

    if(info) printf("\nSaving boundaries for partition %d\n",part);

    sprintf(filename,"%s.%d.%s","part",part,"nodes");
    out = fopen(filename,"w");

    sprintf(filename2,"%s.%d.%s","part",part,"shared");
    out2 = fopen(filename2,"w");

    for(i=0;i<=MAXELEMENTTYPE;i++)
      bulktypes[i] = sidetypes[i] = 0;

    for(i=1;i<=noelements;i++)
      if(elempart[i] == part) bulktypes[data->elementtypes[i]] += 1;

    needednodes = 0;
    neededtwice = 0;

    for(i=1; i <= noknots; i++) {
      ind = i;
      if(periodic) ind = indxper[ind];

      for(j=1;j<=neededtimes[i];j++) {
	if(neededtable[ind][j] == part) {
	  needednodes++;
	  if(data->dim == 2)
	    fprintf(out,outstyle,i,-1,data->x[i],data->y[i]);
	  else if(data->dim == 3)
	    fprintf(out,outstyle,i,-1,data->x[i],data->y[i],data->z[i]);	  
	  
	  if(neededtimes[ind] > 1) {
	    neededtwice++; 
	    fprintf(out2,"%d %d %d",i,neededtimes[ind],ownerpart[ind]);

	    for(k=1;k<=neededtimes[ind];k++) 
	      if(neededtable[ind][k] != ownerpart[ind]) fprintf(out2," %d",neededtable[ind][k]);
	    fprintf(out2,"\n");
	  }	     
	}
      }
    }
    fclose(out);
    fclose(out2);


    periodictype = 0;
    for(j=0;j < MAXBOUNDARIES;j++) 
      for(i=1; i <= bound[j].nosides; i++) 
	if(bound[j].types[i] > periodictype) periodictype = bound[j].types[i];
    periodictype++;
    if(info) printf("The hanging nodes boundary will be given index %d and elementtype 102.\n",periodictype);


   
  /*********** part.n.boundary *********************/
    sprintf(filename,"%s.%d.%s","part",part,"boundary");
    out = fopen(filename,"w");
    
    sumsides = 0;
    for(j=0;j < MAXBOUNDARIES;j++) {
      
      /* Normal boundary conditions */
      for(i=1; i <= bound[j].nosides; i++) {

	GetElementSide(bound[j].parent[i],bound[j].side[i],bound[j].normal[i],
		       data,sideind,&sideelemtype);
	nodesd1 = sideelemtype%100;

	bcneeded = 0;
	for(l=0;l<nodesd1;l++) {
	  ind = sideind[l];
	  if(periodic) ind = indxper[ind];
	  for(k=1;k<=neededtimes[ind];k++)
	    if(part == neededtable[ind][k]) bcneeded++;
	}

	if(bcneeded != nodesd1) continue;
	
	trueparent = (elempart[bound[j].parent[i]] == part);
	if(!trueparent && bound[j].discont[i] <= 1) {
	  if(bound[j].parent2[i]) 
	    trueparent = (elempart[bound[j].parent2[i]] == part);
	}	

	if(!trueparent) continue;

	sumsides++;	
	fprintf(out,"%d %d %d %d ",
		sumsides,bound[j].types[i],bound[j].parent[i],bound[j].parent2[i]);
	
	fprintf(out,"%d ",sideelemtype);
	sidetypes[sideelemtype] += 1;
	for(l=0;l<nodesd1;l++)
	  fprintf(out,"%d ",sideind[l]);
	fprintf(out,"\n");
      }

      /* The second side for discontinuous boundary conditions */
      for(i=1; i <= bound[j].nosides; i++) {
	if(!bound[j].parent2[i] || bound[j].discont[i] != 1) continue;

	GetElementSide(bound[j].parent2[i],bound[j].side2[i],-bound[j].normal[i],
		       data,sideind,&sideelemtype); 
	nodesd1 = sideelemtype%100;	

	bcneeded = 0;
	for(l=0;l<nodesd1;l++) {
	  ind = sideind[l];
	  if(periodic) ind = indxper[ind];
	  for(k=1;k<=neededtimes[ind];k++)
	    if(part == neededtable[ind][k]) bcneeded++;
	}
	if(bcneeded < nodesd1) continue;

	trueparent = (elempart[bound[j].parent2[i]] == part);
	if(!trueparent) continue;

	sumsides++;
	fprintf(out,"%d %d %d %d ",
		sumsides,bound[j].types[i],bound[j].parent2[i],bound[j].parent[i]);
	
	fprintf(out,"%d ",sideelemtype);
	sidetypes[sideelemtype] += 1;
	for(l=0;l<nodesd1;l++)
	  fprintf(out,"%d ",sideind[l]);
	fprintf(out,"\n");
      }
    }
   
      
    /* The periodic boundary conditions */
    if(periodic) {
      for(i=1; i <= data->noknots; i++) {
	ind = indxper[i]; 

	if(i != ind) {
	  bcneeded = 0;
	  for(k=1;k<=neededtimes[ind];k++)
	    if(part == neededtable[ind][k]) bcneeded++;
	  if(!bcneeded) continue;

	  sumsides++;
	  sideelemtype = 102;
	  fprintf(out,"%d %d %d %d %d %d %d\n",
		  sumsides,periodictype,0,0,sideelemtype,i,ind);
	  sidetypes[sideelemtype] += 1;
	}
      }
    }
    sidesinpart[part] = sumsides;


    /* Indirect couplings between dofs */
    {
      int maxsides,nodesides,maxnodeconnections,connectednodes,m;
      int **nodepairs,*nodeconnections,**indpairs;      

      l = 0;
      maxsides = 0;
      nodesides = 0;

  findindirect:

      /* First calculate the maximum number of additional sides */
      for(i=1;i<=noelements;i++) {
	if(elempart[i] == part) continue;
	
	elemtype = data->elementtypes[i];
	nodesd2 = elemtype%100;
	
	bcneeded = 0;
	for(j=0;j < nodesd2;j++) {
	  elemhit[j] = 0;
	  ind = data->topology[i][j];
	  for(k=1;k<=neededtimes[ind];k++) 
	    if(part == neededtable[ind][k]) elemhit[j] = TRUE;
	}
	for(j=0;j < nodesd2;j++) 
	  if(elemhit[j]) bcneeded++;

	if(bcneeded <= 1) continue;
	
	if(l == 0) {
	  maxsides += (bcneeded-1)*bcneeded/2;
	} 
	else {
	  for(j=0;j < nodesd2;j++) {	  
	    for(k=j+1;k < nodesd2;k++) {
	      if(elemhit[j] && elemhit[k]) {
		nodesides += 1;

		/* The minimum index always first */
		if(data->topology[i][j] <= data->topology[i][k]) {
		  nodepairs[nodesides][1] = data->topology[i][j];
		  nodepairs[nodesides][2] = data->topology[i][k];
		}
		else {
		  nodepairs[nodesides][1] = data->topology[i][k];
		  nodepairs[nodesides][2] = data->topology[i][j];		  
		}
	      }
	    }
	  }
	}
      }
      
      if(l == 0) {
	nodepairs = Imatrix(1,maxsides,1,2);
	for(i=1;i<=maxsides;i++)
	  nodepairs[i][1] = nodepairs[i][2] = 0;
	l++;
	goto findindirect;
      }
      if(info) printf("Number of non-element connections is %d\n",nodesides);
      
      
      nodeconnections = Ivector(1,noknots);
      for(i=1;i<=noknots;i++)
	nodeconnections[i] = 0;
      
      for(i=1;i<=nodesides;i++)
	nodeconnections[nodepairs[i][1]] += 1;
      
      maxnodeconnections = 0;
      for(i=1;i<=noknots;i++)
	maxnodeconnections = MAX(maxnodeconnections, nodeconnections[i]);     
      if(info) printf("Maximum number of node-to-node connections %d\n",maxnodeconnections);

      connectednodes = 0;
      for(i=1;i<=noknots;i++) {
	if(nodeconnections[i] > 0) {
	  connectednodes++;
	  nodeconnections[i] = connectednodes;
	}
      }
      if(info) printf("Number of nodes with non-element connections %d\n",connectednodes);

   
      indpairs = Imatrix(1,connectednodes,1,maxnodeconnections);
      for(i=1;i<=connectednodes;i++)
	for(j=1;j<=maxnodeconnections;j++)
	  indpairs[i][j] = 0;
      
      for(i=1;i<=nodesides;i++) {
	ind = nodeconnections[nodepairs[i][1]];
	for(j=1;j<=maxnodeconnections;j++) {
	  if(indpairs[ind][j] == 0) {
	    indpairs[ind][j] = i;	    
	    break;
	  }
	}
      }

      /* Remove dublicate connections */
      l = 0;
      for(i=1;i<=connectednodes;i++) {
	for(j=1;j<=maxnodeconnections;j++)
	  for(k=j+1;k<=maxnodeconnections;k++) {
	    ind = indpairs[i][j];
	    ind2 = indpairs[i][k];
	    if(!ind || !ind2) continue;
	    
	    if(!nodepairs[ind][1] || !nodepairs[ind][2]) continue;

	    if(nodepairs[ind][2] == nodepairs[ind2][2]) {
	      nodepairs[ind2][1] = nodepairs[ind2][2] = 0;
	      l++;
	    }
	  }
      }
      printf("Removed %d duplicate connections\n",l);

      
      /* Remove connections that already exist */
      m = 0;
      for(i=1;i<=noelements;i++) {
	if(elempart[i] != part) continue;
	
	elemtype = data->elementtypes[i];
	nodesd2 = elemtype%100;
	
	for(j=0;j < nodesd2;j++) {
	  ind = nodeconnections[data->topology[i][j]];
	  if(!ind) continue;
	  
	  for(k=0;k < nodesd2;k++) {
	    if(j==k) continue;
	    
	    for(l=1;l<=maxnodeconnections;l++) {
	      ind2 = indpairs[ind][l];
	      if(!ind2) break;

	      if(nodepairs[ind2][1] == data->topology[i][j] && nodepairs[ind2][2] == data->topology[i][k]) {
		nodepairs[ind2][1] = nodepairs[ind2][2] = 0;	    
		m++;
	      }
	    }
	  }
	}
      }
      printf("Removed %d connections that already exists in other elements\n",m);
      

      for(i=1; i <= nodesides; i++) {
	ind = nodepairs[i][1]; 
	ind2 = nodepairs[i][2];
	if(!ind || !ind2) continue;
	
	sumsides++;
	pairsinpart[part] += 1;

	sideelemtype = 102;
	fprintf(out,"%d %d %d %d %d %d %d\n",
		sumsides,periodictype,0,0,sideelemtype,ind,ind2);
	sidetypes[sideelemtype] += 1;
      }

      /* Finally free some extra space that was allocated */
      free_Imatrix(indpairs,1,connectednodes,1,maxnodeconnections);
      free_Imatrix(nodepairs,1,maxsides,1,2);

    }
    /* End of indirect couplings */



    fclose(out);

    tottypes = 0;
    for(i=0;i<=MAXELEMENTTYPE;i++) {
      if(bulktypes[i]) tottypes++;
      if(sidetypes[i]) tottypes++;
    }

    sprintf(filename,"%s.%d.%s","part",part,"header");
    out = fopen(filename,"w");
    fprintf(out,"%-6d %-6d %-6d\n",
	    needednodes,elementsinpart[part],sumsides);

    fprintf(out,"%-6d\n",tottypes);
    for(i=0;i<=MAXELEMENTTYPE;i++) 
      if(bulktypes[i]) 
	fprintf(out,"%-6d %-6d\n",i,bulktypes[i]);

    for(i=0;i<=MAXELEMENTTYPE;i++) 
      if(sidetypes[i]) 
	fprintf(out,"%-6d %-6d\n",i,sidetypes[i]);

    fprintf(out,"%-6d %-6d\n",neededtwice,0);
    fclose(out);

    if(info) {
      printf("\nSaved part %d with %d elements and %d nodes.\n",
	     part,elementsinpart[part],needednodes);
      printf("Saved %d boundary elements and %d node-to-node connections\n",
	     sidesinpart[part],pairsinpart[part]);
    }
  } /* of part */


  chdir("..");
  chdir("..");

  return(0);
}
