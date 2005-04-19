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

#include "EIOPartWriter.h"
#include <stdio.h>
#include <string.h>

extern void make_filename(char *buf, const char *model, const char *suffix);

static char *extension[] = {
  "%s/part.%d.header",
  "%s/part.%d.nodes",
  "%s/part.%d.shared",
  "%s/part.%d.elements",
  "%s/part.%d.border"
};

enum { HEADER = 0, NODES, SHARED, ELEMENTS, BORDER};

EIOPartWriter::
EIOPartWriter(int& partCount, EIOModelManager *mm)
{
  parts = partCount;
  me = -1;
  manager = mm;
}

EIOPartWriter::~EIOPartWriter()
{
}

int EIOPartWriter::
createPartitioning(const char *dir)
{
  sprintf(newdir, "%s/partitioning.%d", dir, parts);
  return manager->makeDirectory(newdir);  
}

int EIOPartWriter::
activatePart(int part)
{
  me = part;
  openStreams();
  return 0;
}

int EIOPartWriter::
deactivatePart()
{
  closeStreams();
  me = -1;
  return 0;
}

int EIOPartWriter::
closePartitioning()
{
  if(me != -1)
    {
      closeStreams();
    }
  return 0;
}

int EIOPartWriter::
write_descriptor(int& nodeC,                 /* nodes */
		 int& sharedC,               /* shared nodes */
		 int& elementC,              /* elements (inner) */
		 int& borderC,               /* elements bordering the part */
		 int& boundaryC,
		 int& usedElementTypes, 
		 int* elementTypeTags,
		 int* elementCountByType)
{
  int i;
  fstream& str = meshFileStream[HEADER];
  str << nodeC << ' ' << elementC << ' ' << boundaryC << '\n';
  str << usedElementTypes << '\n';
  for(i = 0; i < usedElementTypes; ++i)
    str << elementTypeTags[i] << ' ' << elementCountByType[i] << '\n';

  str << sharedC << ' ' << borderC << '\n';
  return 0;
}

int EIOPartWriter::
write_node(int& tag, int& type, double *coord, int& partC, int *parts)
{
  fstream& str = meshFileStream[NODES];
  fstream& str2 = meshFileStream[SHARED];

  str << tag << ' ' << type << ' ';

  str.setf(std::ios::scientific);
  str.precision(16);

  str << coord[0] << ' ' << coord[1] << ' ' << coord[2] << std::endl;
  if(partC > 1)
    {
      int i;
      str2 << tag << ' ' << partC << ' ';
      for(i = 0; i < partC; ++i)
	{
	  str2 << parts[i] << ' ';
	}
      str2 << std::endl;
    }
  return 0;
}
int EIOPartWriter::
write_element(int& tag, int& body, int& type, int *nodes, int& border)
{
  int i;
  fstream& str = meshFileStream[ELEMENTS];
  fstream& str2 = meshFileStream[BORDER];

  str << tag << ' ' << body << ' ' << type << ' ';
  if(type == 303)
    {
      for(i = 0; i < 3; ++i)
	{
	  str << nodes[i] << ' ';
	}
    }
  str << std::endl;

  if(border)
    {
      str2 << tag << std::endl;
    }
  return 0;
}

void EIOPartWriter::
openStreams()
{
  int i;
  char filename[PATH_MAX];

  for(i = 0; i < partWriterFiles; ++i)
    {
      sprintf(filename, extension[i], newdir, me);
      manager->openStream(meshFileStream[i], filename, std::ios::out);
    }
}

void EIOPartWriter::
closeStreams()
{
  int i;
  for(i = 0; i < partWriterFiles; ++i)
    {
      manager->closeStream(meshFileStream[i]);
    }
}
