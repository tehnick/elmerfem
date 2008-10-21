/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUI surface                                                         *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <QtGui>
#include <iostream>
#include "epmesh.h"
#include "matc.h"
#include "vtkpost.h"

#include <vtkFloatArray.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellDerivatives.h>
#include <vtkPointData.h>
#include <vtkCellDataToPointData.h>

using namespace std;

Matc::Matc(QWidget *parent)
  : QDialog(parent)
{
  ui.setupUi(this);

  connect(ui.mcOK, SIGNAL(clicked()), this, SLOT(okButtonClicked()));
  setWindowIcon(QIcon(":/icons/Mesh3D.png"));

  mtc_init( NULL, stdout, stderr );
  QString elmerGuiHome = getenv("ELMERGUI_HOME");
  QString mcIniLoad = "source(\"" + elmerGuiHome.replace("\\", "/") + "/edf/mc.ini\")";
  mtc_domath( mcIniLoad.toAscii().data() );
  com_init( (char *)"grad", FALSE, FALSE, com_grad, 1, 1,
            (char *)"r = grad(f): compute gradient of a scalar variable f.\n") ;
  com_init( (char *)"div", FALSE, FALSE, com_div, 1, 1,
            (char *)"r = div(f): compute divergence of a vector variable f.\n") ;
  com_init( (char *)"curl", FALSE, FALSE, com_curl, 1, 1,
            (char *)"r = curl(f): compute curl of a vector variable f.\n") ;

  in = NULL;
  out = NULL;
}

Matc::~Matc()
{
}


void Matc::okButtonClicked()
{
  close();
}

void Matc::setInput(double* ptr)
{
  in = ptr;
}

void Matc::setOutput(double* ptr)
{
  out = ptr;
}

void Matc::grad(VtkPost* vtkPost)
{
  vtkUnstructuredGrid* volumeGrid = vtkPost->GetVolumeGrid();
  vtkUnstructuredGrid* surfaceGrid = vtkPost->GetSurfaceGrid();
  
  vtkFloatArray *s = vtkFloatArray::New();
  s->SetNumberOfComponents(1);
  s->SetNumberOfTuples(vtkPost->NofNodes());
  for( int i=0;i<vtkPost->NofNodes(); i++ )
    s->SetValue(i,in[i] );
  
  vtkCellDerivatives *cd = vtkCellDerivatives::New();
  if ( volumeGrid->GetNumberOfCells()>0 ) {
    volumeGrid->GetPointData()->SetScalars(s);
    cd->SetInput(volumeGrid);
  } else {
    surfaceGrid->GetPointData()->SetScalars(s);
    cd->SetInput(surfaceGrid);
  }
  cd->SetVectorModeToComputeGradient();
  cd->Update();
  
  vtkCellDataToPointData *nd = vtkCellDataToPointData::New();
  nd->SetInput(cd->GetOutput());
  nd->Update();
  
  vtkDataArray *da = nd->GetOutput()->GetPointData()->GetVectors();
  int ncomp = da->GetNumberOfComponents();
  for( int i=0; i<vtkPost->NofNodes(); i++ )
    for( int j=0; j<ncomp; j++ )
      out[vtkPost->NofNodes()*j+i] = da->GetComponent(i,j);
  
  cd->Delete();
  nd->Delete();
  s->Delete(); 
}

void Matc::div(VtkPost* vtkPost)
{
  vtkUnstructuredGrid* volumeGrid = vtkPost->GetVolumeGrid();
  vtkUnstructuredGrid* surfaceGrid = vtkPost->GetSurfaceGrid();

  int n=volumeGrid->GetNumberOfCells();
  int ncomp = 3;
  
  vtkFloatArray *s = vtkFloatArray::New();
  s->SetNumberOfComponents(ncomp);
  s->SetNumberOfTuples(vtkPost->NofNodes());
  
  for( int j=0;j<ncomp; j++ )
    for( int i=0;i<vtkPost->NofNodes(); i++ )
      s->SetComponent(i,j,in[j*vtkPost->NofNodes()+i] );
  
  vtkCellDerivatives *cd = vtkCellDerivatives::New();
  if ( n>0 ) {
    volumeGrid->GetPointData()->SetVectors(s);
    cd->SetInput(volumeGrid);
  } else {
    surfaceGrid->GetPointData()->SetVectors(s);
    cd->SetInput(surfaceGrid);
  }
  cd->SetTensorModeToComputeGradient();
  cd->Update();
  
  vtkCellDataToPointData *nd = vtkCellDataToPointData::New();
  nd->SetInput(cd->GetOutput());
  nd->Update();
  
  vtkDataArray *da = nd->GetOutput()->GetPointData()->GetTensors();
  ncomp = da->GetNumberOfComponents();
  for( int i=0; i<vtkPost->NofNodes(); i++ )
    {
      out[i]  = da->GetComponent(i,0);
      out[i] += da->GetComponent(i,4);
      out[i] += da->GetComponent(i,8);
    }
  cd->Delete();
  nd->Delete();
  s->Delete(); 
}


void Matc::curl(VtkPost* vtkPost)
{
  vtkUnstructuredGrid* volumeGrid = vtkPost->GetVolumeGrid();
  vtkUnstructuredGrid* surfaceGrid = vtkPost->GetSurfaceGrid();

  int n=volumeGrid->GetNumberOfCells();
  int ncomp = 3;
  
  vtkFloatArray *s = vtkFloatArray::New();
  s->SetNumberOfComponents(ncomp);
  s->SetNumberOfTuples(vtkPost->NofNodes());
  
  for( int j=0;j<ncomp; j++ )
    for( int i=0;i<vtkPost->NofNodes(); i++ )
      s->SetComponent(i,j,in[j*vtkPost->NofNodes()+i] );
  
  vtkCellDerivatives *cd = vtkCellDerivatives::New();
  if ( n>0 ) {
    volumeGrid->GetPointData()->SetVectors(s);
    cd->SetInput(volumeGrid);
  } else {
    surfaceGrid->GetPointData()->SetVectors(s);
    cd->SetInput(surfaceGrid);
  }
  cd->SetTensorModeToComputeGradient();
  cd->Update();
  
  vtkCellDataToPointData *nd = vtkCellDataToPointData::New();
  nd->SetInput(cd->GetOutput());
  nd->Update();
  
  vtkDataArray *da = nd->GetOutput()->GetPointData()->GetTensors();
  for( int i=0; i<vtkPost->NofNodes(); i++ )
    {
      double gx_x = da->GetComponent(i,0);
      double gx_y = da->GetComponent(i,3);
      double gx_z = da->GetComponent(i,6);
      double gy_x = da->GetComponent(i,1);
      double gy_y = da->GetComponent(i,4);
      double gy_z = da->GetComponent(i,7);
      double gz_x = da->GetComponent(i,2);
      double gz_y = da->GetComponent(i,5);
      double gz_z = da->GetComponent(i,8);
      out[i] = gz_y-gy_z;
      out[vtkPost->NofNodes()+i] = gx_z-gz_x;
      out[2*vtkPost->NofNodes()+i] = gy_x-gx_y;
    }
  
  cd->Delete();
  nd->Delete();
  s->Delete(); 
}

void Matc::domatc(VtkPost* vtkPost)
{
  int scalarFields = vtkPost->GetScalarFields();
  ScalarField* scalarField = vtkPost->GetScalarField();
  EpMesh* epMesh = vtkPost->GetEpMesh();

  char *ptr;
  LIST *lst;
  int i;
  VARIABLE *var;
  
  QString cmd=ui.mcEdit->text().trimmed();
  
  ui.mcEdit->clear();
  
  ptr=mtc_domath(cmd.toAscii().data());
  ui.mcHistory->append(cmd);
  if ( ptr ) ui.mcOutput->append(ptr);
  
  QString vectorname;
  for( lst = listheaders[VARIABLES].next; lst; lst = NEXT(lst))
    {
      var = (VARIABLE *)lst;
      if ( !NAME(var) || (NCOL(var) % epMesh->epNodes != 0) ) continue;
      
      int found = false,n;
      for( int i=0; i < scalarFields; i++ )
	{
	  ScalarField *sf = &scalarField[i]; 
	  if ( NROW(var)==1 && sf->name == NAME(var) )
	    {
	      found = true;
	      if ( sf->value != MATR(var) )
		{
		  free(sf->value);
		  sf->value = MATR(var);
		}
	      sf->minVal =  1e99;
	      sf->maxVal = -1e99;
	      for(int j=0; j < sf->values; j++) {
		if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
		if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	      }
	      break;
	    } else if ( NROW(var)==3 ) {
	    vectorname = "";
	    n=sf->name.indexOf("_x");
	    if ( n<=0 ) n=sf->name.indexOf("_y");
	    if ( n<=0 ) n=sf->name.indexOf("_z");
	    if ( n>0 ) vectorname=sf->name.mid(0,n);
	    
	    if ( vectorname==NAME(var) ) {
	      found = true;
	      if ( sf->name.indexOf("_x")>0 ) {
		if ( sf->value != &M(var,0,0) )
		  {
                free(sf->value);
                sf->value = &M(var,0,0);
		  }
	      } else if ( sf->name.indexOf("_y")>0 ) {
		if ( sf->value != &M(var,1,0) )
		  {
		    free(sf->value);
		    sf->value = &M(var,1,0);
		  }
	      } else if ( sf->name.indexOf("_z")>0 ) {
		if ( sf->value != &M(var,2,0) )
		  {
		    free(sf->value);
		    sf->value = &M(var,2,0);
		  }
	      }
	      sf->minVal =  1e99;
	      sf->maxVal = -1e99;
	      for(int j=0; j<sf->values; j++) {
		if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
		if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	      }
	    }
	  }
	}
      
      if ( !found ) 
	{
	  if ( NROW(var) == 1 ) {
	    ScalarField *sf = vtkPost->addScalarField( NAME(var),NCOL(var),NULL );
	    for(int j = 0; j<NCOL(var); j++) {
	      if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
	      if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	    }
	  } else if ( NROW(var) == 3 ) {
	    QString qs = NAME(var);
	    ScalarField *sf;
	    sf = vtkPost->addScalarField( qs+"_x",NCOL(var), &M(var,0,0) );
	    for(int j = 0; j<NCOL(var); j++) {
	      if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
	      if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	    }
	    sf = vtkPost->addScalarField( qs+"_y",NCOL(var), &M(var,1,0) );
	    for(int j = 0; j<NCOL(var); j++) {
	      if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
	      if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	    }
	    sf = vtkPost->addScalarField( qs+"_z",NCOL(var), &M(var,2,0) );
	    for(int j = 0; j<NCOL(var); j++) {
	      if(sf->value[j] > sf->maxVal) sf->maxVal = sf->value[j];
	      if(sf->value[j] < sf->minVal) sf->minVal = sf->value[j];
	    }
	  }
	}
    }
  
  int count=0,n;
  
  for( int i=0; i<scalarFields; i++ )
    {
      ScalarField *sf = &scalarField[i]; 
      
      vectorname = "";
      n=sf->name.indexOf("_x");
      if ( n<=0 ) n=sf->name.indexOf("_y");
      if ( n<=0 ) n=sf->name.indexOf("_z");
      if ( n>0 ) vectorname=sf->name.mid(0,n);
      
      for( lst = listheaders[VARIABLES].next; lst; lst = NEXT(lst))
	{
	  var = (VARIABLE *)lst;
	  if ( !NAME(var) || (NCOL(var) % epMesh->epNodes != 0) ) continue;
	  
	  if ( NROW(var)==1 && sf->name == NAME(var) )
	    {
	      if ( count != i ) scalarField[count]=*sf;
	      count++;
	      break;
	    } else if ( NROW(var)==3 && vectorname==NAME(var) ) {
	    if ( count != i ) scalarField[count]=*sf;
	    count++;
	  }
	}
    }
  if ( count < scalarFields ) vtkPost->SetScalarFields(count);
}
