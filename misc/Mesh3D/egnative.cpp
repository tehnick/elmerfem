/*  
   ElmerGrid - A simple mesh generation and manipulation utility  
   Copyright (C) 1995- , CSC - Scientific Computing Ltd.   

   Author:  Peter Råback
   Email:   Peter.Raback@csc.fi
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

/* --------------------:  feminfo.c  :--------------------------

   These functions provide the user of the program information 
   about how the mesh was created and what the resulting sparse 
   matrix will be alike and also present the results of the 
   calculations in various ways. These subroutines don't affect 
   the operation and results of the program and can thus be 
   omitted if not needed. 
   */
   
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>

#include "egutils.h"
#include "egdef.h"
#include "egtypes.h"
#include "egnative.h"
#include "eggrid.h"
#include "egmesh.h"

int matcactive;

#if HAVE_MATC
char *mtc_domath(const char *);
void mtc_init(FILE * input, FILE * output, FILE * error);
#endif

int Getline(char *line1,FILE *io) 
{
  int i,isend;
  char line0[MAXLINESIZE],*charend,*matcpntr,*matcpntr0;

  for(i=0;i<MAXLINESIZE;i++) 
    line0[i] = ' ';

 newline:

  charend = fgets(line0,MAXLINESIZE,io);
  isend = (charend == NULL);

  if(isend) return(1);

  if(line0[0] == '#' || line0[0] == '%' || line0[0] == '!') goto newline;
  if(!matcactive && line0[0] == '*') goto newline;

#if HAVE_MATC
  if(matcactive) {
    matcpntr0 = strchr(line0,'$');
    if(matcpntr0) {
      matcpntr = mtc_domath(&matcpntr0[1]);
      if(matcpntr) {
	strcpy(matcpntr0, matcpntr);
	if(0) printf("A: %s\n%s\n",matcpntr0,matcpntr);
      }
    }
  }
#endif 

  if(strstr(line0,"subcell boundaries")) goto newline;
  if(strstr(line0,"material structure")) goto newline;
  if(strstr(line0,"mode")) goto newline;
  if(strstr(line0,"type")) goto newline;

  for(i=0;i<MAXLINESIZE;i++) 
    line1[i] = toupper(line0[i]);

  return(0);
}


int GetCommand(char *line1,char *line2,FILE *io) 
{
  int i,j,isend,empty;
  char line0[MAXLINESIZE],*charend,*matcpntr0,*matcpntr;

 newline:

  for(i=0;i<MAXLINESIZE;i++) 
    line2[i] = line1[i] = line0[i] = ' ';

  charend = fgets(line0,MAXLINESIZE,io);
  isend = (charend == NULL);

  if(isend) return(1);

  empty = TRUE;
  for(i=1;i<20;i++) if(line0[0] != ' ') empty = FALSE;
  if(empty) goto newline;

  if(line0[0] == '#' || line0[0] == '%' || line0[0] == '!' || line0[0] == '\n') goto newline;
  if(!matcactive && line0[0] == '*') goto newline;

#if HAVE_MATC
  if(matcactive) {
    matcpntr0 = strchr(line0,'$');
    if(matcpntr0) {
      matcpntr = mtc_domath(&matcpntr0[1]);
      if(matcpntr) {
	strcpy(matcpntr0, matcpntr);
	if(0) printf("B: %s\n%s\n",matcpntr0,matcpntr);
      }
      else {
	if(0) printf("B0: %s\n",matcpntr0);
	goto newline;
      }
    }
  }
#endif 

  j = 0;
  for(i=0;i<MAXLINESIZE;i++) {
    if(line0[i] == '=') {
      j = i;
      break;
    }
    line1[i] = toupper(line0[i]);
  }

  /* After these commands there will be no nextline even though there is no equality sign */
  if(strstr(line1,"END")) return(0);
  if(strstr(line1,"NEW MESH")) return(0);


  if(j) {
    for(i=j+1;i<MAXLINESIZE;i++) 
      line2[i-j-1] = line0[i];      
  }
  else {
  newline2:
    charend = fgets(line2,MAXLINESIZE,io);
    isend = (charend == NULL);
    if(isend) return(2);
    if(line2[0] == '#' || line2[0] == '%' || line2[0] == '!') goto newline2;
    if(!matcactive && line2[0] == '*') goto newline2;

#if HAVE_MATC
    if(matcactive) {
      matcpntr0 = strchr(line2,'$');
      if(matcpntr0) {
	matcpntr = mtc_domath(&matcpntr0[1]);
	if(matcpntr) {
	  strcpy(matcpntr0, matcpntr);
	  if(0) printf("C: %s\n%s\n",matcpntr0,matcpntr);
	}
      }
    }
#endif 
  }
  
  return(0);
}





int SaveElmergrid(struct GridType *grid,int nogrids,char *prefix,int info)
{
  int res,sameline,maxsameline;
  int i,j,dim;
  FILE *out;
  char filename[MAXFILESIZE];

  AddExtension(prefix,filename,"grd");
  out = fopen(filename,"w");
  dim = grid->dimension;
  if(grid->coordsystem == COORD_CART1) dim = 1;

  j = 0;
  sameline = TRUE;
  maxsameline = 6;
  if(grid->xcells > maxsameline) sameline = FALSE;
  if(dim >= 2 && grid->ycells > maxsameline) sameline = FALSE;
  if(dim >= 3 && grid->zcells > maxsameline) sameline = FALSE;
  
  fprintf(out,"#####  ElmerGrid input file for structured grid generation  ######\n");
  fprintf(out,"Version = 210903\n");

  fprintf(out,"Coordinate System = ");
  if(grid->coordsystem == COORD_AXIS)
    fprintf(out,"2D Axisymmetric\n");
  else if(grid->coordsystem == COORD_POLAR)
    fprintf(out,"2D Polar\n");
  else 
    fprintf(out,"Cartesian %dD\n",dim);
 
  fprintf(out,"Subcell Divisions in %dD = ",dim);
  if(dim >= 1) fprintf(out,"%d ",grid->xcells);
  if(dim >= 2) fprintf(out,"%d ",grid->ycells);
  if(dim >= 3) fprintf(out,"%d ",grid->zcells);
  fprintf(out,"\n");

  fprintf(out,"Subcell Limits 1 %s",sameline ? "= ":"\n  ");
  for(i=0;i <= grid->xcells;i++) 
    fprintf(out,"%.5lg ",grid->x[i]); 
  fprintf(out,"\n");
    
  if(dim >= 2) {
    fprintf(out,"Subcell Limits 2 %s",sameline ? "= ":"\n  ");
    for(i=0;i <= grid->ycells;i++) 
      fprintf(out,"%.5lg ",grid->y[i]); 
    fprintf(out,"\n");
  }
  
  if(dim >= 3) {
    fprintf(out,"Subcell Limits 3 %s",sameline ? "= ":"\n  ");
    for(i=0;i <= grid->zcells;i++) 
      fprintf(out,"%.5lg ",grid->z[i]); 
    fprintf(out,"\n");
  }  

  fprintf(out,"Material Structure in %dD\n",dim==1 ? 1:2);  
  for(j=grid->ycells;j>=1;j--) {
    fprintf(out,"  ");
    for(i=1;i<=grid->xcells;i++) 
      fprintf(out,"%-5d",grid->structure[j][i]);
    fprintf(out,"\n");
  }
  fprintf(out,"End\n");

  if(grid->mappings > 0) {
    fprintf(out,"Geometry Mappings\n");
    fprintf(out,"# mode  line  limits(2)   Np  params(Np)\n");
    for(i=0;i<grid->mappings;i++) {
      fprintf(out,"  %-5d %-5d %-7.5lg %-7.5lg %-3d ",
	      grid->mappingtype[i],grid->mappingline[i],
	      grid->mappinglimits[2*i],grid->mappinglimits[2*i+1],
	      grid->mappingpoints[i]);
      for(j=0;j<grid->mappingpoints[i];j++) 
	fprintf(out,"%.4lg ",grid->mappingparams[i][j]);
      fprintf(out,"\n");
    }
    fprintf(out,"End\n");
  }

  j = 0;
  if(grid[j].rotate) {
    fprintf(out,"Revolve Blocks = %d\n",grid[j].rotateblocks);
    fprintf(out,"Revolve Radius = %-8.3lg\n",grid[j].rotateradius2);
    if(fabs(grid[j].rotateimprove-1.0) > 1.0e-10)
      fprintf(out,"Revolve Improve = %-8.3lg\n",grid[j].rotateimprove);
    
  }
  if(grid[j].rotatecurve) {
    fprintf(out,"Revolve Curve Direct = %-8.3lg\n",grid[j].curvezet);
    fprintf(out,"Revolve Curve Radius = %-8.3lg\n",grid[j].curverad);
    fprintf(out,"Revolve Curve Angle = %-8.3lg\n",grid[j].curveangle);
  }

  if(grid[j].coordsystem == COORD_POLAR) {
    fprintf(out,"Polar Radius = %.3lg\n",grid[j].polarradius);
  } 

  for(j=0;j<nogrids;j++) {
    
    if(j>0) fprintf(out,"\nStart New Mesh\n");
  
    fprintf(out,"Materials Interval = %d %d\n",
	    grid[j].firstmaterial,grid[j].lastmaterial);
  
    if(dim == 3) {
      fprintf(out,"Extruded Structure\n");
      fprintf(out,"# %-8s %-8s %-8s\n","1stmat", "lastmat","newmat");
      for(i=1;i<=grid[j].zcells;i++) 
	fprintf(out,"  %-8d %-8d %-8d\n",
		grid[j].zfirstmaterial[i],grid[j].zlastmaterial[i],
		grid[j].zmaterial[i]); 
      fprintf(out,"End\n");    
    }

    if(grid[j].noboundaries > 0) {
      fprintf(out,"Boundary Definitions\n");
      fprintf(out,"# %-8s %-8s %-8s\n","type","out","int"); 
      for(i=0;i<grid[j].noboundaries;i++)
	fprintf(out,"  %-8d %-8d %-8d %-8d\n",
		grid[j].boundtype[i],grid[j].boundext[i],
		grid[j].boundint[i], grid[j].boundsolid[i]);
      fprintf(out,"End\n");
    }

    if(grid->numbering == NUMBER_XY)
      fprintf(out,"Numbering = Horizontal\n");
    if(grid->numbering == NUMBER_YX)
      fprintf(out,"Numbering = Vertical\n");
        
    fprintf(out,"Element Degree = %d\n",grid[j].elemorder);
    fprintf(out,"Element Innernodes = %s\n",grid[j].elemmidpoints ? "True" : "False");
    fprintf(out,"Triangles = %s\n",grid[j].triangles ? "True" : "False");
    if(grid[j].autoratio) 
      fprintf(out,"Surface Elements = %d\n",grid[j].wantedelems);
    if(dim == 2)
      fprintf(out,"Coordinate Ratios = %-8.3lg\n",grid[j].xyratio);
    if(dim == 3)
      fprintf(out,"Coordinate Ratios = %-8.3lg %-8.3lg\n",
	      grid[j].xyratio,grid[j].xzratio);
 
    fprintf(out,"Minimum Element Divisions = %d",grid[j].minxelems);
    if(dim >= 2) fprintf(out," %d",grid[j].minyelems);
    if(dim >= 3) fprintf(out," %d",grid[j].minzelems);
    fprintf(out,"\n");

    fprintf(out,"Element Ratios 1 %s",sameline ? "= ":"\n  ");
    for(i=1;i<=grid[j].xcells;i++) 
      fprintf(out,"%.3lg ",grid[j].xexpand[i]); 
    fprintf(out,"\n");
    if(dim >= 2) {
      fprintf(out,"Element Ratios 2 %s",sameline ? "= ":"\n  ");
      for(i=1;i<=grid[j].ycells;i++) 
	fprintf(out,"%.3lg ",grid[j].yexpand[i]); 
      fprintf(out,"\n");
    }
    if(dim >= 3) {
      fprintf(out,"Element Ratios 3 %s",sameline ? "= ":"\n  ");
      for(i=1;i<=grid[j].zcells;i++) 
	fprintf(out,"%.3lg ",grid[j].zexpand[i]); 
      fprintf(out,"\n");
    }

    if(grid[j].autoratio) {
      fprintf(out,"Element Densities 1 %s",sameline ? "= ":"\n  ");
      for(i=1;i<=grid[j].xcells;i++) 
	fprintf(out,"%.3lg ",grid[j].xdens[i]); 
      fprintf(out,"\n");
      if(dim >= 2) {
	fprintf(out,"Element Densities 2 %s",sameline ? "= ":"\n  ");
	for(i=1;i<=grid[j].ycells;i++) 
	  fprintf(out,"%.3lg ",grid[j].ydens[i]); 
	fprintf(out,"\n");
      }
      if(dim >= 3) {       
	fprintf(out,"Element Densities 3 %s",sameline ? "= ":"\n  ");
	for(i=1;i<=grid[j].zcells;i++) 
	  fprintf(out,"%.3lg ",grid[j].zdens[i]); 
	fprintf(out,"\n");
      }
    }
    else {
      fprintf(out,"Element Divisions 1 %s",sameline ? "= ":"\n  ");
      for(i=1;i<=grid[j].xcells;i++) 
	fprintf(out,"%d ",grid[j].xelems[i]); 
      fprintf(out,"\n");
      if(dim >= 2) {
	fprintf(out,"Element Divisions 2 %s",sameline ? "= ":"\n  ");
	for(i=1;i<=grid[j].ycells;i++) 
	  fprintf(out,"%d ",grid[j].yelems[i]); 
	fprintf(out,"\n");
      }
      if(dim >= 3) {       
	fprintf(out,"Element Divisions 3 %s",sameline ? "= ":"\n  ");
	for(i=1;i<=grid[j].zcells;i++) 
	  fprintf(out,"%d ",grid[j].zelems[i]); 
	fprintf(out,"\n");
      }
    }
    

  }

  if(info) printf("The Elmergrid input was saved to file %s.\n",filename);
  fclose(out);

  return(0);
}





int LoadElmergrid(struct GridType **grid,int *nogrids,char *prefix,int info) 
{
  char filename[MAXFILESIZE];
  char command[MAXLINESIZE],params[MAXLINESIZE];
  FILE *in;
  int i,j,k,l,error=0;
  struct GridType grid0;
  char *cp;
  int noknots,noelements,dim,axisymmetric;
  int elemcode,maxnodes,totelems,nogrids0,minmat,maxmat;
  long code;
  Real raid;

  AddExtension(prefix,filename,"grd");
  if ((in = fopen(filename,"r")) == NULL) {
    printf("LoadElmergrid: opening of the file '%s' wasn't succesfull !\n",filename);
    return(1);
  }

  if(info) printf("Loading the geometry from file '%s'.\n",filename);

  InitGrid(grid[*nogrids]);
  k = *nogrids;
  nogrids0 = *nogrids;

  noknots = 0;
  noelements = 0;
  dim = 0;
  axisymmetric = FALSE;
  elemcode = 0;
  maxnodes = 4;
  totelems = 0;

  matcactive = FALSE;

  for(;;) {
    if(GetCommand(command,params,in)) {
      if(0) printf("Reached the end of command file\n");
      goto end;
    }    

    /* Control information */
    if(strstr(command,"VERSION")) {
      sscanf(params,"%ld",&code);
      if(code == 210903) {
	if(info) printf("Loading ElmerGrid file version: %d\n",code);
      }
      else {
	printf("Unknown ElmerGrid file version: %d\n",code);
	return(2);
      }
      *nogrids += 1;
    }      

    else if(strstr(command,"MATC")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"FALSE")) 
	matcactive = FALSE;
      else {
#if HAVE_MATC
	matcactive = TRUE;
	mtc_init(NULL, stdout, stderr);
	strcpy(command, "format( 12 )");	
	mtc_domath(command);	 
	if(info) printf("MATC language activated with 12 digit accuracy.\n");	
#else
	printf("This version was compiled without MATC library.\n");		
#endif
      }
    }

    
    else if(strstr(command,"COORDINATE SYSTEM")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      grid[k]->dimension = 2;
      if(strstr(params,"CARTESIAN 1D")) {
	grid[k]->coordsystem = COORD_CART1;
	grid[k]->dimension = 1;
      }
      else if(strstr(params,"CARTESIAN 2D")) 
	grid[k]->coordsystem = COORD_CART2;
      else if(strstr(params,"AXISYMMETRIC")) 
	grid[k]->coordsystem = COORD_AXIS;
      else if(strstr(params,"POLAR"))
	grid[k]->coordsystem = COORD_POLAR;
      else if(strstr(params,"CARTESIAN 3D")) {
	grid[k]->coordsystem = COORD_CART3;
	grid[k]->dimension = 3;
      }
      else printf("Unknown coordinate system: %s\n",params);
      if(0) printf("Defining the coordinate system (%d-DIM).\n",grid[k]->dimension);
    }
    
    else if(strstr(command,"SUBCELL DIVISIONS")) {
      if(grid[k]->dimension == 1) {
	sscanf(params,"%d",&(*grid)[k].xcells);
	grid[k]->ycells = 1;	
      }
      else if(grid[k]->dimension == 2) 
	sscanf(params,"%d %d",&(*grid)[k].xcells,&(*grid)[k].ycells);
      else if(grid[k]->dimension == 3) 
	sscanf(params,"%d %d %d",&(*grid)[k].xcells,&(*grid)[k].ycells,&(*grid)[k].zcells);      
      if(grid[k]->xcells >= MAXCELLS || grid[k]->ycells >= MAXCELLS || grid[k]->zcells >= MAXCELLS) {
	printf("LoadElmergrid: Too many subcells [%d %d %d] vs. %d:\n",
	       grid[k]->xcells,grid[k]->ycells,grid[k]->zcells,MAXCELLS);
      }

      /* Initialize the default stucture with ones */
      for(j=grid[k]->ycells;j>=1;j--) 
	for(i=1;i<=grid[k]->xcells;i++) 
	  grid[k]->structure[j][i] = 1;
    }
    
    else if(strstr(command,"MINIMUM ELEMENT DIVISION")) {
      if(0) printf("Loading minimum number of elements\n");
      if((*grid)[k].dimension == 1) 
	sscanf(params,"%d",&(*grid)[k].minxelems);
      if((*grid)[k].dimension == 2) 
	sscanf(params,"%d %d",&(*grid)[k].minxelems,&(*grid)[k].minyelems);
      if((*grid)[k].dimension == 3) 
	sscanf(params,"%d %d %d",&(*grid)[k].minxelems,&(*grid)[k].minyelems,&(*grid)[k].minzelems);
    }      
    
    else if(strstr(command,"SUBCELL LIMITS 1")) {
      if(0) printf("Loading [%d] subcell limits in X-direction\n",grid[k]->xcells+1);
      cp = params;
      for(i=0;i<=grid[k]->xcells;i++) grid[k]->x[i] = next_real(&cp);
    }    
    else if(strstr(command,"SUBCELL LIMITS 2")) {
      if(0) printf("Loading [%d] subcell limits in Y-direction\n",grid[k]->ycells+1);
      cp = params;
      for(i=0;i<=grid[k]->ycells;i++) grid[k]->y[i] = next_real(&cp);
    }      
    else if(strstr(command,"SUBCELL LIMITS 3")) {
      if(0) printf("Loading [%d] subcell limits in Z-direction\n",grid[k]->zcells+1);
      cp = params;
      for(i=0;i<=grid[k]->zcells;i++) grid[k]->z[i] = next_real(&cp);
    }

    else if(strstr(command,"SUBCELL SIZES 1")) {
      if(0) printf("Loading [%d] subcell sizes in X-direction\n",grid[k]->xcells);
      cp = params;
      for(i=1;i<=grid[k]->xcells;i++) grid[k]->x[i] = next_real(&cp);
      for(i=1;i<=grid[k]->xcells;i++) grid[k]->x[i] = grid[k]->x[i-1] + grid[k]->x[i];
    }      
    else if(strstr(command,"SUBCELL SIZES 2")) {
      if(0) printf("Loading [%d] subcell sizes in Y-direction\n",grid[k]->ycells);
      cp = params;
      for(i=1;i<=grid[k]->ycells;i++) grid[k]->y[i] = next_real(&cp);
      for(i=1;i<=grid[k]->ycells;i++) grid[k]->y[i] = grid[k]->y[i-1] + grid[k]->y[i];
    }      
    else if(strstr(command,"SUBCELL SIZES 3")) {
      if(0) printf("Loading [%d] subcell sizes in Z-direction\n",grid[k]->zcells);
      cp = params;
      for(i=1;i<=grid[k]->zcells;i++) grid[k]->z[i] = next_real(&cp);
      for(i=1;i<=grid[k]->zcells;i++) grid[k]->z[i] = grid[k]->z[i-1] + grid[k]->z[i];
    }

    else if(strstr(command,"SUBCELL ORIGIN 1")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"CENTER")) {
	raid = 0.5 * (grid[k]->x[0] + grid[k]->x[grid[k]->xcells]);
      }
      else if(strstr(params,"LEFT") || strstr(params,"MIN") ) {
	raid = grid[k]->x[0];
      }
      else if(strstr(params,"RIGHT") || strstr(params,"MAX") ) {
	raid = grid[k]->x[grid[k]->xcells];
      }
      else {
	cp = params;
	raid = next_real(&cp);
      }
      for(i=0;i<=grid[k]->xcells;i++) grid[k]->x[i] -= raid;
    }
    else if(strstr(command,"SUBCELL ORIGIN 2")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"CENTER")) {
	raid = 0.5 * (grid[k]->y[0] + grid[k]->y[grid[k]->ycells]);
      }
      else if(strstr(params,"LEFT")) {
	raid = grid[k]->y[0];
      }
      else if(strstr(params,"RIGHT")) {
	raid = grid[k]->y[grid[k]->ycells];
      }
      else {
	cp = params;
	raid = next_real(&cp);
      }      
      for(i=0;i<=grid[k]->ycells;i++) grid[k]->y[i] -= raid;
    }
    else if(strstr(command,"SUBCELL ORIGIN 3")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"CENTER")) {
	raid = 0.5 * (grid[k]->z[0] + grid[k]->z[grid[k]->zcells]);
      }
      else if(strstr(params,"LEFT")) {
	raid = grid[k]->z[0];
      }
      else if(strstr(params,"RIGHT")) {
	raid = grid[k]->z[grid[k]->zcells];
      }
      else {
	cp = params;
	raid = next_real(&cp);
      }
      for(i=0;i<=grid[k]->zcells;i++) grid[k]->z[i] -= raid;      
    }

    else if(strstr(command,"MATERIAL STRUCTURE")) {
      if(0) printf("Loading material structure\n");

      /* Initialize the default stucture with zeros */
      for(j=grid[k]->ycells;j>=1;j--) 
	for(i=1;i<=grid[k]->xcells;i++) 
	  grid[k]->structure[j][i] = 0;
     
      for(j=grid[k]->ycells;j>=1;j--) {
	if(j < grid[k]->ycells) Getline(params,in);
	cp=params;
	for(i=1;i<=grid[k]->xcells;i++) 
	  grid[k]->structure[j][i] = next_int(&cp);
      }      
      minmat = maxmat = grid[k]->structure[1][1];
      for(j=grid[k]->ycells;j>=1;j--) 
	for(i=1;i<=grid[k]->xcells;i++) {
	  if(minmat > grid[k]->structure[j][i])
	    minmat = grid[k]->structure[j][i];
	  if(maxmat < grid[k]->structure[j][i])
	    maxmat = grid[k]->structure[j][i];
	}      
      if(minmat < 0) 
	printf("LoadElmergrid: please use positive material indices.\n");
      if(maxmat > MAXMATERIALS) 
	printf("LoadElmergrid: material indices larger to %d may create problems.\n",
	       MAXMATERIALS);
    }
    else if(strstr(command,"MATERIALS INTERVAL")) {
      sscanf(params,"%d %d",&(*grid)[k].firstmaterial,&(*grid)[k].lastmaterial);      
    }
     
    else if(strstr(command,"REVOLVE")) {
      if(strstr(command,"REVOLVE RADIUS")) {
	(*grid)[k].rotate = TRUE;
	sscanf(params,"%le",&(*grid)[k].rotateradius2);
      }
      else if(strstr(command,"REVOLVE BLOCKS")) {
	(*grid)[k].rotate = TRUE;
	sscanf(params,"%d",&(*grid)[k].rotateblocks);
      }
      else if(strstr(command,"REVOLVE IMPROVE")) {
	(*grid)[k].rotate = TRUE;
	sscanf(params,"%le",&(*grid)[k].rotateimprove);
      }
      else if(strstr(command,"REVOLVE RADIUS")) {
	sscanf(params,"%le",&(*grid)[k].polarradius);
      }
      else if(strstr(command,"REVOLVE CURVE DIRECT")) {
	(*grid)[k].rotatecurve = TRUE;
	sscanf(params,"%le",&(*grid)[k].curvezet);
      }
      else if(strstr(command,"REVOLVE CURVE RADIUS")) {
	(*grid)[k].rotatecurve = TRUE;
	sscanf(params,"%le",&(*grid)[k].curverad);
      }
      else if(strstr(command,"REVOLVE CURVE ANGLE")) {
	(*grid)[k].rotatecurve = TRUE;
	sscanf(params,"%le",&(*grid)[k].curveangle);
      }
    }

    else if(strstr(command,"REDUCE ORDER INTERVAL")) {
      sscanf(params,"%d%d",&(*grid)[k].reduceordermatmin,
	     &(*grid)[k].reduceordermatmax);
    }
    
    else if(strstr(command,"BOUNDARY DEFINITION")) {
      if(0) printf("Loading boundary conditions\n");
      
      for(i=0;i<MAXBOUNDARIES;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	sscanf(params,"%d %d %d %d",
	       &(*grid)[k].boundtype[i],&(*grid)[k].boundext[i],
	       &(*grid)[k].boundint[i],&(*grid)[k].boundsolid[i]);
      }  
      if(0) printf("Found %d boundaries\n",i);
      (*grid)[k].noboundaries = i;
    }
    
    else if(strstr(command,"LAYERED BOUNDARIES")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"TRUE")) (*grid)[k].layeredbc = 1;
      if(strstr(params,"FALSE")) (*grid)[k].layeredbc = 0;
    }
    
    else if(strstr(command,"NUMBERING")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"HORIZONATAL")) (*grid)[k].numbering = NUMBER_XY;
      if(strstr(params,"VERTICAL")) (*grid)[k].numbering = NUMBER_YX;
    }
    
    else if(strstr(command,"ELEMENT DEGREE")) {
      sscanf(params,"%d",&(*grid)[k].elemorder);
    }
    
    else if(strstr(command,"ELEMENT INNERNODES")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"TRUE")) (*grid)[k].elemmidpoints = TRUE;
      if(strstr(params,"FALSE")) (*grid)[k].elemmidpoints = FALSE;
    }
    else if(strstr(command,"ELEMENTTYPE") || strstr(command,"ELEMENTCODE")) {
      sscanf(params,"%d",&elemcode);
    }
    
    else if(strstr(command,"TRIANGLES CRITICAL ANGLE")) {
      sscanf(params,"%le",&(*grid)[k].triangleangle);      
    }
    else if(strstr(command,"TRIANGLES")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"TRUE")) (*grid)[k].triangles = TRUE;
      if(strstr(params,"FALSE")) (*grid)[k].triangles = FALSE;
    }
    
    else if(strstr(command,"PLANE ELEMENTS")) {
      sscanf(params,"%d",&(*grid)[k].wantedelems);
    }
    else if(strstr(command,"SURFACE ELEMENTS")) {
      sscanf(params,"%d",&(*grid)[k].wantedelems);
    }
    else if(strstr(command,"REFERENCE DENSITY")) {
      sscanf(params,"%le",&(*grid)[k].limitdx);
      (*grid)[k].autoratio = 3;     
    }
    else if(strstr(command,"VERIFY DENSITY")) {
      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      if(strstr(params,"TRUE")) (*grid)[k].limitdxverify = TRUE;
      if(strstr(params,"FALSE")) (*grid)[k].limitdxverify = FALSE;
    }
    else if(strstr(command,"COORDINATE RATIO")) {
      if((*grid)[k].dimension == 2) 
	sscanf(params,"%le",&(*grid)[k].xyratio);
      if((*grid)[k].dimension == 3) 
	sscanf(params,"%le %le",&(*grid)[k].xyratio,&(*grid)[k].xzratio);      
    }
    
    else if(strstr(command,"ELEMENT RATIOS 1")) {
      cp = params;
      for(i=1;i<=(*grid)[k].xcells;i++) (*grid)[k].xexpand[i] = next_real(&cp);
    }
    else if(strstr(command,"ELEMENT RATIOS 2")) {
      cp = params;
      for(i=1;i<=(*grid)[k].ycells;i++) (*grid)[k].yexpand[i] = next_real(&cp);
    }
    else if(strstr(command,"ELEMENT RATIOS 3")) {
      cp = params;
      for(i=1;i<=(*grid)[k].zcells;i++) (*grid)[k].zexpand[i] = next_real(&cp);
    }
    
    else if(strstr(command,"ELEMENT DENSITIES 1")) {
      cp = params;
      for(i=1;i<=(*grid)[k].xcells;i++) (*grid)[k].xdens[i] = next_real(&cp);
    }
    else if(strstr(command,"ELEMENT DENSITIES 2")) {
      cp = params;
      for(i=1;i<=(*grid)[k].ycells;i++) (*grid)[k].ydens[i] = next_real(&cp);
    }
    else if(strstr(command,"ELEMENT DENSITIES 3")) {
      cp = params;
      for(i=1;i<=(*grid)[k].zcells;i++) (*grid)[k].zdens[i] = next_real(&cp);
    }
    
    else if(strstr(command,"ELEMENT DIVISIONS 1")) {
      cp = params;
      for(i=1;i<=(*grid)[k].xcells;i++) (*grid)[k].xelems[i] = next_int(&cp);
      (*grid)[k].autoratio = 0;
    }
    else if(strstr(command,"ELEMENT DIVISIONS 2")) {
      cp = params;
      for(i=1;i<=(*grid)[k].ycells;i++) (*grid)[k].yelems[i] = next_int(&cp);
      (*grid)[k].autoratio = 0;
    }
    else if(strstr(command,"ELEMENT DIVISIONS 3")) {
      cp = params;
      for(i=1;i<=(*grid)[k].zcells;i++) (*grid)[k].zelems[i] = next_int(&cp);
      (*grid)[k].autoratio = 0;
    }
    
    else if(strstr(command,"EXTRUDED STRUCTURE")) {
      for(i=1;i<=(*grid)[k].zcells;i++) {
	if(i>1) Getline(params,in);
	sscanf(params,"%d %d %d\n",
	       &(*grid)[k].zfirstmaterial[i],&(*grid)[k].zlastmaterial[i],&(*grid)[k].zmaterial[i]); 
      }
    }
    
    else if(strstr(command,"GEOMETRY MAPPINGS")) {     
      if(k > 0) (*grid)[k].mappings = 0;

      for(i=0;i<MAXLINESIZE;i++) params[i] = toupper(params[i]);
      for(i=(*grid)[k].mappings;i<MAXMAPPINGS;i++) {
	if(i>(*grid)[k].mappings) Getline(params,in);

	if(strstr(params,"END")) break;
	cp=params; 
	(*grid)[k].mappingtype[i] = next_int(&cp);	
#if 0
	(*grid)[k].mappingtype[i] += 50*SGN((*grid)[k].mappingtype[i]);
#endif
	(*grid)[k].mappingline[i] = next_int(&cp);
	(*grid)[k].mappinglimits[2*i] = next_real(&cp);
	(*grid)[k].mappinglimits[2*i+1] = next_real(&cp);
	(*grid)[k].mappingpoints[i] = next_int(&cp);
	(*grid)[k].mappingparams[i] = Rvector(0,(*grid)[k].mappingpoints[i]);
	for(j=0;j<(*grid)[k].mappingpoints[i];j++) 
	  (*grid)[k].mappingparams[i][j] = next_real(&cp);
      }      
      if(0) printf("Loaded %d geometry mappings\n",i);
      (*grid)[k].mappings = i;      
    }

    else if(strstr(command,"END") ) {      
      if(0) printf("End of field\n");
    }
      
    else if(strstr(command,"START NEW MESH")) {
      if((*nogrids) >= MAXCASES) {
	printf("There are more grids than was allocated for!\n"); 
	printf("Ignoring meshes starting from %d\n.",(*nogrids)+1);
	goto end;
      }
      (*nogrids)++;
      if(0) printf("\nLoading element meshing no %d\n",*nogrids);
      k = *nogrids - 1;	           
      if(k > nogrids0) (*grid)[k] = (*grid)[k-1];	 
    }

    else {
      if(0) printf("Unknown command: %s",command);
    }
  }

end:

  if(0) printf("Found %d divisions for grid\n",*nogrids);
  
  for(k=nogrids0;k < (*nogrids) && k<MAXCASES;k++) {

    if(elemcode == 0) {
      if((*grid)[k].dimension == 1) {
	(*grid)[k].nonodes = (*grid)[k].elemorder + 1;
      }
      else if((*grid)[k].elemmidpoints == FALSE) {
	(*grid)[k].nonodes = 4 * (*grid)[k].elemorder;
      }					
      else {
	if((*grid)[k].elemorder == 2) (*grid)[k].nonodes = 9;
	if((*grid)[k].elemorder == 3) (*grid)[k].nonodes = 16;	
      }
    }
    else if(elemcode/100 == 2) {
      (*grid)[k].triangles = FALSE;      
      (*grid)[k].nonodes = elemcode%100;
    }
    else if(elemcode/100 == 4) {
      (*grid)[k].triangles = FALSE;      
      (*grid)[k].nonodes = elemcode%100;
    }
    else if(elemcode/100 == 3) {  
      (*grid)[k].triangles = TRUE;      
      if(elemcode%100 == 3)       (*grid)[k].nonodes = 4;
      else if(elemcode%100 == 6)  (*grid)[k].nonodes = 9;
      else if(elemcode%100 == 10) (*grid)[k].nonodes = 16;	
    }    
  }

  fclose(in);
  return(error);
}



void InitParameters(struct ElmergridType *eg)
{
  int i;
  
  eg->relh = 1.0;
  eg->inmethod = 0;
  eg->outmethod = 0;
  eg->silent = FALSE;
  eg->nofilesin = 1;
  eg->unitemeshes = FALSE;
  eg->triangles = FALSE;
  eg->triangleangle = 0.0;
  eg->rotate = FALSE;
  eg->polar = FALSE;
  eg->cylinder = FALSE;
  eg->usenames = FALSE;
  eg->layers = 0;
  eg->layereps = 0.0;
  eg->layermove = 0;
  eg->partitions = 0;
  eg->elements3d = 0;
  eg->nodes3d = 0;
  eg->metis = 0;
  eg->partitionhalo = FALSE;
  eg->partitionindirect = FALSE;
  eg->reduce = FALSE;
  eg->increase = FALSE;
  eg->translate = FALSE;
  eg->isoparam = FALSE;
  eg->removelowdim = FALSE;
  eg->removeunused = FALSE;
  eg->dim = 3;
  eg->center = FALSE;
  eg->scale = FALSE;
  eg->order = FALSE;
  eg->boundbounds = 0;
  eg->saveinterval[0] = eg->saveinterval[1] = eg->saveinterval[2] = 0;
  eg->bulkbounds = 0;
  eg->partorder = FALSE;
  eg->findsides = FALSE;
  eg->pelems = 0;
  eg->belems = 0;
  eg->saveboundaries = TRUE;
  eg->merge = FALSE;
  eg->bcoffset = FALSE;
  eg->periodic = 0;
  eg->periodicdim[0] = 0;
  eg->periodicdim[1] = 0;
  eg->periodicdim[2] = 0;
  eg->bulkorder = FALSE;
  eg->boundorder = FALSE;
  eg->sidemappings = 0;
  eg->bulkmappings = 0;
  eg->clone[0] = eg->clone[1] = eg->clone[2] = 0;
  eg->decimals = 12;
  eg->discont = 0;
  eg->connect = 0;
  eg->advancedmat = 0;
  
  for(i=0;i<MAXSIDEBULK;i++) 
    eg->sidebulk[i] = 0;
}






int LoadCommands(char *prefix,struct ElmergridType *eg,
		 struct GridType *grid, int mode,char *IOmethods[],int info) 
{
  char filename[MAXFILESIZE],command[MAXLINESIZE],params[MAXLINESIZE],*cp;

  FILE *in;
  int i,j,k,l,error=0;

  if( mode == 0) {  
    if (in = fopen("ELMERGRID_STARTINFO","r")) {
      fscanf(in,"%s",filename);
      fclose(in);
      printf("Using the file %s defined in ELMERGRID_STARTINFO\n",filename);
      if ((in = fopen(filename,"r")) == NULL) {
	printf("LoadCommands: opening of the file '%s' wasn't succesfull !\n",filename);
	return(1);
      }    
      else printf("Loading ElmerGrid commands from file '%s'.\n",filename);    
    }    
    else 
      return(2);
  }
  if(mode == 1) { 
    AddExtension(prefix,filename,"eg");
    if ((in = fopen(filename,"r")) == NULL) {
      printf("LoadCommands: opening of the file '%s' wasn't succesfull !\n",filename);
      return(3);
    }    
    if(info) printf("Loading ElmerGrid commands from file '%s'.\n",filename);    
  }
  else if(mode == 2) {
    AddExtension(prefix,filename,"grd");
    if ((in = fopen(filename,"r")) == NULL) {
      printf("LoadCommands: opening of the file '%s' wasn't succesfull !\n",filename);
      return(4);
    }    
    if(info) printf("Loading ElmerGrid commands from file '%s'.\n",filename);
  }



  for(;;) {

    if(GetCommand(command,params,in)) {
      if(0) printf("Reached the end of command file\n");
      goto end;
    }    

    /* If the mode is the command file mode read also the file information from the command file. */

    if(mode <= 1) {
      if(strstr(command,"INPUT FILE")) {
	sscanf(params,"%s",&eg->filesin[0]);
      }

      else if(strstr(command,"OUTPUT FILE")) {
	sscanf(params,"%s",&eg->filesout[0]);
      }

      else if(strstr(command,"INPUT MODE")) {
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	
	for(i=0;i<=MAXFORMATS;i++) {
	  if(strstr(params,IOmethods[i])) {
	    eg->inmethod = i;
	    break;
	  }
	}
	if(i>MAXFORMATS) sscanf(params,"%d",&eg->inmethod);
      }

      else if(strstr(command,"OUTPUT MODE")) {
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	
	/* Type of output file (fewer options) */
	for(i=1;i<=MAXFORMATS;i++) {
	  if(strstr(params,IOmethods[i])) {
	    eg->outmethod = i;
	    break;
	  }
	}
	if(i>MAXFORMATS) sscanf(params,"%d",&eg->outmethod);	
      }
    }    
    /* End of command file specific part */


    if(strstr(command,"DECIMALS")) {
      sscanf(params,"%d",&eg->decimals);
    }
    else if(strstr(command,"TRIANGLES CRITICAL ANGLE")) {
      sscanf(params,"%le",&eg->triangleangle);
    }
    else if(strstr(command,"TRIANGLES")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->triangles = TRUE;      
    }
    else if(strstr(command,"MERGE NODES")) {
      eg->merge = TRUE;
      sscanf(params,"%le",&eg->cmerge);
    }
    else if(strstr(command,"UNITE")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->unitemeshes = TRUE;      
    }
    else if(strstr(command,"ORDER NODES")) {
      eg->order = TRUE;
      if(eg->dim == 1) 
	sscanf(params,"%le",&eg->corder[0]);
      else if(eg->dim == 2) 
	sscanf(params,"%le%le",&eg->corder[0],&eg->corder[1]);
      else if(eg->dim == 3) 
	sscanf(params,"%le%le%le",&eg->corder[0],&eg->corder[1],&eg->corder[2]);
    }
    else if(strstr(command,"SCALE")) {
      eg->scale = TRUE;
      if(eg->dim == 1) 
	sscanf(params,"%le",&eg->cscale[0]);
      else if(eg->dim == 2) 
	sscanf(params,"%le%le",&eg->cscale[0],&eg->cscale[1]);
      else if(eg->dim == 3) 
	sscanf(params,"%le%le%le",&eg->cscale[0],&eg->cscale[1],&eg->cscale[2]);
    }
    else if(strstr(command,"CENTRALIZE")) {
      eg->center = TRUE;
    }
    else if(strstr(command,"TRANSLATE")) {
      eg->translate = TRUE;
      if(eg->dim == 1) 
	sscanf(params,"%le",&eg->ctranslate[0]);
      else if(eg->dim == 2) 
	sscanf(params,"%le%le",&eg->ctranslate[0],&eg->ctranslate[1]);
      else if(eg->dim == 3) 
	sscanf(params,"%le%le%le",&eg->ctranslate[0],&eg->ctranslate[1],&eg->ctranslate[2]);
    }
    else if(strstr(command,"ROTATE MESH")) {
      eg->rotate = TRUE;
      sscanf(params,"%le%le%le",&eg->crotate[0],&eg->crotate[1],&eg->crotate[2]);
    }
    else if(strstr(command,"CLONE")) {
      if(strstr(command,"CLONE SIZE")) {
	if(eg->dim == 1) 
	  sscanf(params,"%le",&eg->clonesize[0]);
	else if(eg->dim == 2) 
	  sscanf(params,"%le%le",&eg->clonesize[0],&eg->clonesize[1]);
	else if(eg->dim == 3) 
	  sscanf(params,"%le%le%le",&eg->clonesize[0],&eg->clonesize[1],&eg->clonesize[2]);	
      }
      else {
	if(eg->dim == 1) 
	  sscanf(params,"%d",&eg->clone[0]);
	else if(eg->dim == 2) 
	  sscanf(params,"%d%d",&eg->clone[0],&eg->clone[1]);
	else if(eg->dim == 3) 
	  sscanf(params,"%d%d%d",&eg->clone[0],&eg->clone[1],&eg->clone[2]);
      }
    }

    else if(strstr(command,"POLAR RADIUS")) {
      eg->polar = TRUE;
      sscanf(params,"%le",&eg->polarradius);
    }
    else if(strstr(command,"CYLINDER")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->cylinder = TRUE;      
    }
    else if(strstr(command,"REDUCE DEGREE")) {
      eg->reduce = TRUE;
      sscanf(params,"%d%d",&eg->reducemat1,&eg->reducemat2);
    }
    else if(strstr(command,"INCREASE DEGREE")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->increase = TRUE;      
    }
    else if(strstr(command,"ADVANCED ELEMENTS")) {
      printf("Loading advanced element definitions\n");
      
      for(i=0;i<MAXMATERIALS;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
		
	sscanf(params,"%d%d%d%d%d%d%d",
	       &eg->advancedelem[7*i],&eg->advancedelem[7*i+1],&eg->advancedelem[7*i+2],
	       &eg->advancedelem[7*i+3],&eg->advancedelem[7*i+4],&eg->advancedelem[7*i+5],
	       &eg->advancedelem[7*i+6]);
      }  
      eg->advancedmat = i;
      printf("Found %d definitions for advanced elements.\n",i);
    }
    else if(strstr(command,"POWER ELEMENTS")) {
      printf("Loading p-type element definitions\n");
      
      for(i=0;i<MAXMATERIALS;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	sscanf(params,"%d%d%d",
	       &eg->pelemmap[3*i],&eg->pelemmap[3*i+1],&eg->pelemmap[3*i+2]);
      }  
      eg->pelems = i;
      printf("Found %d definitions for p-elements.\n",i);
    }
    else if(strstr(command,"BUBBLE ELEMENTS")) {
      printf("Loading bubble element definitions\n");
      
      for(i=0;i<MAXMATERIALS;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	sscanf(params,"%d%d%d",
	       &eg->belemmap[3*i],&eg->belemmap[3*i+1],&eg->belemmap[3*i+2]);
      }  
      eg->belems = i;
      printf("Found %d definitions for bubble elements.\n",i);
    }
    else if(strstr(command,"METIS OPTION")) {
#if HAVE_METIS
      sscanf(params,"%d",&eg->partopt);
#else
      printf("This version of ElmerGrid was compiled without Metis library!\n");
#endif
    }
    else if(strstr(command,"METIS")) {
#if HAVE_METIS
      sscanf(params,"%d",&eg->metis);
#else
      printf("This version of ElmerGrid was compiled without Metis library!\n");
#endif
    }
    else if(strstr(command,"PARTITION ORDER")) {
      eg->partorder = 1;
      if(eg->dim == 2) sscanf(params,"%le%le",&eg->partcorder[0],&eg->partcorder[1]);
      if(eg->dim == 3) sscanf(params,"%le%le%le",&eg->partcorder[0],
			      &eg->partcorder[1],&eg->partcorder[2]);      
    }
    else if(strstr(command,"PARTITION")) {
      if(eg->dim == 2) sscanf(params,"%d%d",&eg->partdim[0],&eg->partdim[1]);
      if(eg->dim == 3) sscanf(params,"%d%d%d",&eg->partdim[0],&eg->partdim[1],&eg->partdim[2]);
      eg->partitions = 1;
      for(i=0;i<eg->dim;i++) {
	if(eg->partdim[i] < 1) eg->partdim[i] = 1;
	eg->partitions *= eg->partdim[i];
      }
    }
    else if(strstr(command,"PERIODIC")) {
      if(eg->dim == 2) sscanf(params,"%d%d",&eg->periodicdim[0],&eg->periodicdim[1]);
      if(eg->dim == 3) sscanf(params,"%d%d%d",&eg->periodicdim[0],
			      &eg->periodicdim[1],&eg->periodicdim[2]);
    }
    else if(strstr(command,"HALO")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->partitionhalo = TRUE;      
    }
    else if(strstr(command,"INDIRECT")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->partitionindirect = TRUE;      
    }
    else if(strstr(command,"BOUNDARY TYPE MAPPINGS")) {
      for(i=0;i<MAXMATERIALS;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->sidemap[3*i],&eg->sidemap[3*i+1],&eg->sidemap[3*i+2]);
      }
      printf("Found %d boundary type mappings\n",i);
      eg->sidemappings = i;
    }
    else if(strstr(command,"BULK TYPE MAPPINGS")) {
      for(i=0;i<MAXMATERIALS;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->bulkmap[3*i],&eg->bulkmap[3*i+1],&eg->bulkmap[3*i+2]);
      }
      printf("Found %d bulk type mappings\n",i);
      eg->bulkmappings = i;
    }
    else if(strstr(command,"BOUNDARY BOUNDARY")) {
      for(i=0;i<MAXBOUNDARIES;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->boundbound[3*i+2],&eg->boundbound[3*i],&eg->boundbound[3*i+1]);
      }
      printf("Found %d boundary boundary definitions\n",i);
      eg->boundbounds = i;
    }
    else if(strstr(command,"MATERIAL BOUNDARY")) {
      for(i=0;i<MAXBOUNDARIES;i++) {
	if(i>0) Getline(params,in);
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->bulkbound[3*i+2],&eg->bulkbound[3*i],&eg->bulkbound[3*i+1]);
      }
      printf("Found %d material boundary definitions\n",i);
      eg->bulkbounds = i;
    }

    else if(strstr(command,"RENUMBER BOUNDARY")) {
      for(i=0;i<MAXBOUNDARIES;i++) {
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->sidemap[3*i],&eg->sidemap[3*i+1],&eg->sidemap[3*i+2]);
      }
      printf("Found %d boundary mappings\n",i);
      eg->sidemappings = i;
    }
    else if(strstr(command,"RENUMBER MATERIAL")) {
      for(i=0;i<MAXBOUNDARIES;i++) {
	for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	if(strstr(params,"END")) break;
	cp = params;      
	sscanf(params,"%d%d%d",&eg->bulkmap[3*i],&eg->bulkmap[3*i+1],&eg->bulkmap[3*i+2]);
      }
      printf("Found %d material mappings\n",i);
      eg->bulkmappings = i;
    }

    else if(strstr(command,"BOUNDARY LAYER")) {
      if(strstr(command,"BOUNDARY LAYER MOVE")) {
	sscanf(params,"%d",&eg->layermove);
      }
      else if(strstr(command,"BOUNDARY LAYER EPSILON")) {
	sscanf(params,"%le",&eg->layereps);
      }
      else {
	for(i=0;i<MAXBOUNDARIES;i++) {
	  if(i>0) Getline(params,in);
	  for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
	  cp = params;      

	  if(strstr(params,"END") || strstr(params,"End") ) break;
	  eg->layerbounds[i] = next_int(&cp);
	  eg->layernumber[i] = next_int(&cp);
	  eg->layerthickness[i] = next_real(&cp);
	  eg->layerratios[i] = next_real(&cp);
	  eg->layerparents[i] = next_int(&cp);	  
	}
	printf("Found %d boundary layers\n",i);
	eg->layers = i;
      }
    }
    else if(strstr(command,"REMOVE LOWER DIMENSIONAL BOUNDARIES")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->removelowdim = TRUE; 
    }
    else if(strstr(command,"REMOVE UNUSED NODES")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->removeunused = TRUE; 
    }
    else if(strstr(command,"REORDER MATERIAL")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->bulkorder = TRUE; 
    }
    else if(strstr(command,"REORDER BOUNDARY")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->boundorder = TRUE; 
    }
    else if(strstr(command,"DIMENSION")) {
      sscanf(params,"%d",&eg->dim);
    }
    else if(strstr(command,"ISOPARAMETRIC")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->isoparam = TRUE;
    }
    else if(strstr(command,"NO BOUNDARY")) {
      for(j=0;j<MAXLINESIZE;j++) params[j] = toupper(params[j]);
      if(strstr(params,"TRUE")) eg->saveboundaries = FALSE;
    }
    else if(strstr(command,"EXTRUDED")) {
      grid->dimension = 3;

      if(strstr(command,"EXTRUDED DIVISIONS")) {
	sscanf(params,"%d",&grid->zcells);		
      }
      else if(strstr(command,"EXTRUDED LIMITS")) {
	cp = params;
	for(i=0;i<=grid->zcells;i++) grid->z[i] = next_real(&cp);
      }
      else if(strstr(command,"EXTRUDED ELEMENTS")) {
	cp = params;
	for(i=1;i<=grid->zcells;i++) grid->zelems[i] = next_int(&cp);
	grid->autoratio = FALSE;    
      }
      else if(strstr(command,"EXTRUDED RATIOS")) {
	cp = params;
	for(i=1;i<=grid->zcells;i++) grid->zexpand[i] = next_real(&cp);
      }
      else if(strstr(command,"EXTRUDED DENSITIES")) {
	cp = params;
	for(i=1;i<=grid->zcells;i++) grid->zdens[i] = next_real(&cp);
      }
      else if(strstr(command,"EXTRUDED STRUCTURE")) {
	for(i=1;i<= grid->zcells;i++) {
	  if(i>1) Getline(params,in);
	  sscanf(params,"%d %d %d\n",
		 &grid->zfirstmaterial[i],&grid->zlastmaterial[i],&grid->zmaterial[i]); 
	}
      }

    }
  }
  
end:
  if(0) printf("Read commands from a file\n");
  
  return(0);
}


int CreateElmerGridMesh(struct ElmergridType *eg,struct GridType *grid,
			struct FemType *data,struct BoundaryType *boundaries,
			Real relh,int info) {
  int errstat,i,j;  
  struct CellType *cell;
  
  SetElementDivision(grid,relh,info);
  
  cell = (struct CellType*)
    malloc((size_t) (grid->nocells+1)*sizeof(struct CellType)); 
  SetCellData(grid,cell,info);

  if(grid->dimension == 1) 
    SetCellKnots1D(grid,cell,info);
  else
    SetCellKnots(grid,cell,info);

  CreateKnots(grid,cell,data,0,0);

  if(grid->noboundaries > 0) {
    for(j=0;j<grid->noboundaries;j++) {
      if(grid->boundsolid[j] < 4) {
	CreateBoundary(cell,data,&(boundaries[j]),grid->boundext[j],grid->boundint[j],
		       1,grid->boundtype[j],info);  
      } 
      else { 
	CreatePoints(cell,data,&(boundaries[j]),grid->boundext[j],grid->boundint[j],
		     grid->boundsolid[j],grid->boundtype[j],info); 	    
      }
    }
  }
  else {
    CreateAllBoundaries(cell,data,boundaries,info);
  }

  free(cell);
}
