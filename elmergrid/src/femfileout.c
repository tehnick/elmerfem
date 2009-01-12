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

/* --------------------:  femfilein.c  :-------------------------- */

#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
/*#include <unistd.h>*/

#include "nrutil.h"
#include "common.h"
#include "femdef.h"
#include "femtools.h"
#include "femtypes.h"
#include "femknot.h"
#include "femfileout.h"


int SaveAbaqusInput(struct FemType *data,char *prefix,int info)
/* Saves the grid in a format that can be read by ABAQUS 
   program designed for sructural mechanics. 
   The elementtype is set to be that of thermal conduction. 
   */
{
  int noknots,noelements,nonodes;
  char filename[MAXFILESIZE];
  int i,j;
  FILE *out;

  noknots = data->noknots;
  noelements = data->noelements;
  nonodes = data->maxnodes;

  if(nonodes != 4 && nonodes != 8) {
    printf("SaveAbaqusInput: not designed for %d-node elements\n",nonodes);
    return(1);
  }

  AddExtension(prefix,filename,"inp");
  out = fopen(filename,"w");

  if(info) printf("Saving ABAQUS data to %s.\n",filename);  

  fprintf(out,"*HEADING\n");
  fprintf(out,"Abaqus input file creator by Peter.Raback@csc.fi\n");

  fprintf(out,"*NODE, SYSTEM=");
  if(data->coordsystem == COORD_CART2) fprintf(out,"R\n");
  else if(data->coordsystem == COORD_AXIS) fprintf(out,"C\n");
  else if(data->coordsystem == COORD_POLAR) fprintf(out,"P\n");

  for(i=1; i <= noknots; i++) 
    fprintf(out,"%8d, %12.4le, %12.4le, 0.0\n",i,data->x[i],data->y[i]);

  fprintf(out,"*ELEMENT,TYPE=");
  if(nonodes == 4) fprintf(out,"DC2D4 ");
  else if(nonodes == 8) fprintf(out,"DC2D8 ");
  else printf("SaveAbaqusInput: Not defined for %d-node elements\n",nonodes);

  fprintf(out,",ELSET=SETTI1\n");
  for(i=1;i<=noelements;i++) {
    fprintf(out,"%8d, ",i);
    for(j=1;j<nonodes;j++) 
      fprintf(out,"%6d, ",data->topology[i][j-1]);
    fprintf(out,"%6d\n",data->topology[i][nonodes-1]);
  }

  fprintf(out,"*SOLID SECTION, ELSET=SETTI1, MATERIAL=MAT1\n");

  fprintf(out,"*MATERIAL, NAME=MAT1\n");
  fprintf(out,"*CONDUCTIVITY\n");
  fprintf(out,"1.0\n");

  fprintf(out,"*RESTART, WRITE, FREQUENCY=1\n");
  fprintf(out,"*FILE FORMAT, ASCII\n");

  fprintf(out,"*STEP\n");
  fprintf(out,"*HEAT TRANSFER, STEADY STATE\n");
  fprintf(out,"*END STEP\n");

  fclose(out);

  if(info) printf("Wrote the mesh in ABAQUS restart format.\n");

  return(0);
}


int SaveFidapOutput(struct FemType *data,char *prefix,int info,
		    int vctrs,Real *vect1, ...)
/* This procedure saves the solution in a form that is understood by 
   the CFD program Fidap. The routine that reads this file seems
   to be stupidly place-dependent. The best manual for this 
   subroutine is provided in the appendix E of FIDAP users manual.
   */
{
  int noknots,noelements,dim,nonodes;
  int i,j,no,nogroup,material,cellelem;
  int elemcodes[MAT_MAXNUMBER+1],mat[MAT_MAXNUMBER+1];
  FILE *out;
  Real *dofs[MAXDOFS];
  char filename[MAXFILESIZE];
  va_list ap;

  if(!data->created) {
    printf("You tried to save points that were never created.\n");
    return(1);
  }

  printf("Saving results in FIDAP neutral format.\n");

  noknots = data->noknots;
  noelements = data->noelements;
  dim = data->dim;

  if(vctrs > 3) {
    printf("SaveSolutionFidap: Maximum of 3 d.o.f.\n");
    vctrs = 3;
  }

  /* Read the list of pointers to vectors to be saved. */
  if(vctrs > 0) {
    va_start(ap,vect1);
    dofs[1] = vect1;
    for(i=2;i<=vctrs;i++)
      dofs[i] = va_arg(ap,Real*);
    va_end(ap);
  }

  /* Create groups by placing the same materials to same groups. */
  nogroup = 0;
  for(i=1;i<=MAT_MAXNUMBER;i++) 
    elemcodes[i] = mat[i] = 0;
  for(j=1;j <= noelements;j++)  {
    material = data->material[j];
    if(material > 0 && material<=MAT_MAXNUMBER) mat[material] += 1;
    elemcodes[material] = data->elementtypes[j];
  } 
  for(i=1;i<=MAT_MAXNUMBER;i++) 
    if(mat[i] > 0) nogroup++;

  AddExtension(prefix,filename,"fidap");
  out = fopen(filename,"w");

  /* Control information */
  fprintf(out,"** FIDAP NEUTRAL FILE\n");
  fprintf(out,"Fidap input file creator by Peter.Raback@csc.fi\n");
  fprintf(out,"VERSION %7.2f\n",7.52); /* Fidap version */
  fprintf(out," 1 Dec 96    12:00:00\n");
  fprintf(out,"%15s%15s%15s%15s%15s\n","NO. OF NODES",
	  "NO. ELEMENTS","NO. ELT GROUPS","NDFCD","NDFVL");
  fprintf(out,"%15d%15d%15d%15d%15d\n",noknots,noelements,nogroup,dim,dim);
  fprintf(out,"%15s%15s%15s%15s\n",
	  "STEADY/TRANS","TURB. FLAG","FREE SURF FLAG","COMPR. FLAG");
  fprintf(out,"%15d%15d%15d%15d\n",0,0,0,0);
  fprintf(out,"%s\n","TEMPERATURE/SPECIES FLAGS");
  fprintf(out,"%s\n"," 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
  fprintf(out,"%s\n","PRESSURE FLAGS - IDCTS, IPENY MPDF");
  fprintf(out,"%10d%10d%10d\n",0,0,1);

  fprintf(out,"NODAL COORDINATES\n");
  for(i=1; i <= noknots; i++) 
    fprintf(out,"%10d%20.10le%20.10le\n",i,data->y[i],data->x[i]);

  /* Boundary and initial conditions */
  fprintf(out,"BOUNDARY CONDITIONS\n");
  fprintf(out,"%10d%10d%10d%20.10e\n",0,0,5,0.0);

  fprintf(out,"ELEMENT GROUPS\n");
  nogroup = 0;
  for(no=1;no<=MAT_MAXNUMBER;no++)
    if(mat[no] > 0) {
      nonodes = elemcodes[no]%100;
      nogroup++;
      cellelem = mat[no];
      fprintf(out,"GROUP:    %5d ELEMENTS:%10d NODES:   %10d GEOMETRY:%5d TYPE:%4d\n",
	      nogroup,cellelem,nonodes,1,1);
      fprintf(out,"ENTITY NAME:   %s%d\n","material",nogroup);

      for(j=1;j <= noelements;j++)  {
	if(data->material[j] == no) {    
	  fprintf(out,"%8d\n",j);
	  for(i=0;i < nonodes;i++)
	    fprintf(out,"%8d",data->topology[j][i]);
	  fprintf(out,"\n");
	}
      } 
    }    

  fprintf(out,"TIMESTEP: %5d TIME:     %15.7e INCRMNT: %15.7e\n",1,1.0,1.0);

  fprintf(out,"VELOCITY\n");            
  if(vctrs < 2) 
    for(i=1;i<=2*noknots;i++) {
      fprintf(out,"%16.9le",0.0);
      if(i%5==0) fprintf(out,"\n");
    }  
  else 
    for(i=1;i<=2*noknots;i++) {
      fprintf(out,"%16.9le",dofs[1][i]);
      if((2*i-1)%5 == 0) fprintf(out,"\n");
      fprintf(out,"%16.9le",dofs[2][i]);
      if((2*i)%5 == 0) fprintf(out,"\n");   
  }
  if((2*noknots)%5 != 0) fprintf(out,"\n");

  fprintf(out,"TEMPERATURE\n");

  if(vctrs == 2) 
    for(i=1;i<=noknots;i++) {
      fprintf(out,"%16.9le",0.0);
      if(i%5==0) fprintf(out,"\n");
    }
  else 
    for(i=1;i<=noknots;i++) {
      fprintf(out,"%16.9le",dofs[vctrs][i]);
      if(i%5==0) fprintf(out,"\n");
    }
  if(noknots%5 != 0) fprintf(out,"\n");
  fprintf(out,"ENDOFTIMESTEP\n");       

  fclose(out);
  if(info) printf("Results were saved in FIDAP neutral format to file %s.\n",filename);

  return(0);
}



int SaveFastcapInput(struct FemType *data,
		     struct BoundaryType *bound,char *prefix,int decimals,int info)
/* Saves the mesh in a form that may be used as input 
   in Fastcap calculations. 
   */
#define MAXELEMENTTYPE 827
{
  int noknots,noelements,quads,triangles,elemtype,maxelemtype,fail;
  int sideelemtype,nodesd1,nodesd2;
  int i,j,k,l;
  int ind[MAXNODESD1];
  FILE *out;
  char filename[MAXFILESIZE], outstyle[MAXFILESIZE];

  if(!data->created) {
    printf("You tried to save points that were never created.\n");
    return(1);
  }

  noelements = data->noelements;
  noknots = data->noknots;

  sprintf(filename,"%s%s",prefix,".fc");
  sprintf(outstyle,"%%.%dlg %%.%dlg %%.%dlg ",decimals,decimals,decimals);

  if(info) printf("Saving mesh in Fastcap format to filename %s.\n",filename);

  out = fopen(filename,"w");
  if(out == NULL) {
    printf("opening of file was not successful\n");
    return(2);
  }

  quads = 0;
  triangles = 0;
  maxelemtype = GetMaxElementType(data);
  
  if(maxelemtype < 500) {    
    if(info) printf("Saving Fastcap mesh using bulk elements.\n");

    fprintf(out,"0 Elmer mesh of %d elements, bulk elements\n",data->noelements);
 
    for(i=1;i<=noelements;i++) {
      elemtype = data->elementtypes[i];

      nodesd2 = elemtype%100;
      if(nodesd2 == 4) {
	quads++;
	fprintf(out,"Q %d ",data->material[i]);
	for(j=0;j < nodesd2;j++) {
	  k = data->topology[i][j];
	  fprintf(out,outstyle,data->x[k],data->y[k],data->z[k]);      
	}
	fprintf(out,"\n");
      }
      if(nodesd2 == 3) {
	triangles++;
	fprintf(out,"T %d ",data->material[i]);
	for(j=0;j < nodesd2;j++) {
	  k = data->topology[i][j];
	  fprintf(out,outstyle,data->x[k],data->y[k],data->z[k]);      
	}
	fprintf(out,"\n");
      }
    }
  }
  else {
    if(info) printf("Saving Fastcap mesh using boundary elements.\n");

    fprintf(out,"0 Elmer mesh of %d elements, boundary elements\n",data->noelements);
   
    for(j=0;j < MAXBOUNDARIES;j++) {
      
      if(bound[j].created == FALSE) continue;
      if(bound[j].nosides == 0) continue;
      
      for(i=1; i <= bound[j].nosides; i++) {
	
	GetElementSide(bound[j].parent[i],bound[j].side[i],bound[j].normal[i],data,ind,&sideelemtype); 

	nodesd2 = sideelemtype%100;
	if(nodesd2 == 4) {
	  quads++;
	  fprintf(out,"Q %d ",bound[j].types[i]);
	  for(k=0;k < nodesd2;k++) {
	    l = ind[k];
	    fprintf(out,outstyle,data->x[l],data->y[l],data->z[l]);      
	  }
	  fprintf(out,"\n");
	}
	if(nodesd2 == 3) {
	  triangles++;
	  fprintf(out,"T %d ",bound[j].types[i]);
	  for(k=0;k < nodesd2;k++) {
	    l = ind[k];
	    fprintf(out,outstyle,data->x[l],data->y[l],data->z[l]);      
	  }
	  fprintf(out,"\n");
	}
		
      }	
    }
  }

  if(info) {
    printf("Fastcap mesh saved successfully\n");
    printf("There are %d quadrilateral and %d triangular panels\n",quads,triangles);
  }

  fclose(out);

  return(0);
}
