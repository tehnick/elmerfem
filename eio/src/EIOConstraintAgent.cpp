/***********************************************************************
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
*                Address: Center for Scientific Computing
*                         Tietotie 6, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Data base interface (EIO)
Author(s):  Harri Hakula 10.03.98
************************************************************************/

#include <string.h>
#include <ctype.h>

#include "EIOConstraintAgent.h"

extern void make_filename(char *buf, const char *model, const char *suffix);

static char *extension[] = {
  "constraints.header",
  "constraints.table"
};

enum { HEADER = 0, TABLE };

EIOConstraintAgent::EIOConstraintAgent(EIOModelManager *mm)
{
  manager = mm;
}

EIOConstraintAgent::~EIOConstraintAgent()
{
}

int EIOConstraintAgent::
openConstraints()
{
  int i;
  char filename[PATH_MAX];

  for(i = 0; i < constraintFiles; ++i)
    {
      //      make_filename(filename, manager->name(), extension[i]);
      manager->openStream(constraintFileStream[i], extension[i], std::ios::in);
    }

  fstream& str = constraintFileStream[HEADER];
  str >> constraintCount;
  return 0;
}

int EIOConstraintAgent::
closeConstraints()
{
  int i;
  char filename[PATH_MAX];

  for(i = 0; i < constraintFiles; ++i)
    {
      manager->closeStream(constraintFileStream[i]);
    } 
  return 0;
}

int EIOConstraintAgent::
descriptor(int& cCount)
{
  cCount = constraintCount;
  return 0;
}

static int cstep = 0;
int EIOConstraintAgent::
nextConstraint(int& tag, int& field, 
	       int *constraintType, double *constraintValue)
{
  int i;
  fstream& str = constraintFileStream[TABLE];
  if(cstep == constraintCount)
    {
      streampos pos = 0;
      filebuf *fbuf = str.rdbuf();
      fbuf->pubseekpos(pos, std::ios::in);
      cstep = 0;
      return -1;
    }
  str >> tag >> field;
  for(i = 0; i < field; ++i)
    {
      char name[10];
      str >> name;
      int j, len;
      len = strlen(name);
      for(j = 0; j < len; ++j) name [j] = toupper(name[j]);
      if(!strcmp(name, "U"))
	{
	  constraintType[i] = 0;
	}
      else if(!strcmp(name, "V"))
	{
	  constraintType[i] = 1;
	}
      else if(!strcmp(name, "T"))
	{
	  constraintType[i] = 100;
	}
      else
	{
	  constraintType[i] = 999;
	}
      str >> constraintValue[i];
    }
  ++cstep;
  return 0;
}

