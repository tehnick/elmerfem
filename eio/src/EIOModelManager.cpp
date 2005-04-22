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
            Martti Verho 08.10.98  (Win32 related changes)
************************************************************************/

#include "EIOModelManager.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <iostream.h>
#include <fstream.h>

#if defined(WIN32)   
  # include <direct.h> 
  # include <io.h>     
#else                
  #include <unistd.h>
#endif

/*
  The ModelManager presides over the filesystem access:
  - the correctness of paths supplied by the user is verified
  - the location in the filesystem is maintained
  - streams are managed (opened/created/closed) here
 */

/*
  eio_mkdir
  eio_chdir
  eio_checkmodel

  are cover functions for the system level calls.
  Note, that in the current version the error value is handled locally.
  THIS WILL BE CHANGED IN THE FUTURE!

  Note, that there are other system calls in the default constructor.
  They will get their own cover functions later.
 */
int eio_mkdir(const char *dir)
{
  int rc;
  /* #ifndef LINUX_PC 
  extern int errno;
  #endif */

#if defined(WIN32)
  rc = _mkdir(dir);
#else
  rc = mkdir(dir, S_IRWXU|S_IRWXG);
#endif

  if(rc == -1)
  {
    switch(errno)
	  {
	  case EEXIST:
	    return 1;
	    break;
	  default:
	    std::cerr << "Unexpected error at mkdir" << std::endl;
	    break;
	  }

    return 0;
  }

  return 1;
}

int eio_chdir(const char *dir)
{
  int rc;
  /* #ifndef LINUX_PC
  extern int errno;
  #endif */

#if defined(WIN32)
  rc = _chdir(dir);
#else
  rc = chdir(dir);
#endif


  if(rc == -1)
    {
      switch(errno)
	{
	case EACCES:
	  std::cerr << "Check permissions: dir " << std::endl;
	  break;
	case EIO:
	  std::cerr << "I/O error: dir " << std::endl;
	  break;	  
	case ENOENT:
	  std::cerr << "No such dir" << std::endl;
	  break;
	case ENOTDIR:
	  std::cerr << "Check path: dir" << std::endl;
	  break;
	default:
	  std::cerr << "Unexpected error at chdir" << std::endl;
	  break;
	}
      return 0;
    }
  return 1;
}

/*
  eio_checkmodel

  has two parts:
   (i) verify the integrity of the path
  (ii) verify the permissions
 */
int eio_checkmodel(const char *model)
{
  int rc;
  /* #ifndef LINUX_PC
  extern int errno;
  #endif */

#if defined(WIN32)
  struct _stat buf;
  rc = _stat(model, &buf);
#else
  struct stat buf;
  rc = stat(model, &buf);
#endif

  if(rc == -1)
    {
      switch(errno)
	{
	case EACCES:
	  std::cerr << "Check permissions: model " << std::endl;
	  break;

	case EIO:
	  std::cerr << "I/O error: model " << std::endl;
	  break;	  

	case ENOENT:
	  std::cerr << "No such model" << std::endl;
	  break;

	case ENOTDIR:
	  std::cerr << "Check path: model" << std::endl;
	  break;
	  
	default:
	  std::cerr << "Unexpected error at stat" << std::endl;
	  break;
	}
      return 0;
    }

  /*
    Is model a directory?
    */
  int rc_access;

#if defined(WIN32)
  rc = buf.st_mode & _S_IFDIR;
  if (rc)
    rc_access = _access(model, 06);
#else
  rc = S_ISDIR(buf.st_mode);
    rc_access = access(model, R_OK | W_OK | X_OK);
#endif

  if(rc)
    {
      /*
	We need read/write/exec permissions, however, since we could stat,
	we can search.
	*/
      if(rc_access == -1)
	{
	  std::cerr << "No permission to operate: model" << std::endl;
	  return 0;
	}
    }
  else
    {
      std::cerr << model << " is not a directory" << std::endl;
      return 0;
    }
  return 1;
}



EIOModelManager::EIOModelManager()
{
  /*
    We must remember the current directory so that we can safely return
    to there after the database has been closed.

    We should also get the mask and use it in opening the streams.
    TO BE FIXED SOON.
   */
#if defined(WIN32)
  _getcwd(rundir, PATH_MAX);
/*  _umask(_S_IWRITE | _S_IREAD);*/
  _umask(0);
#else
  getcwd(rundir, PATH_MAX);
  umask(S_IRWXO);
#endif

}


EIOModelManager::~EIOModelManager()
{
  eio_chdir(rundir);
}

int EIOModelManager::createModel(const char *dir)
{
  strcpy(modeldir, dir);
  //  strcpy(modelname, model);

  if(!eio_chdir(modeldir))
    {
      return -1;
    }
  if(!eio_mkdir(modeldir))
    {
      return -1;
    }
  if(!eio_chdir(modeldir))
    {
      return -1;
    }
  return 0;
}

int EIOModelManager::openModel(const char *dir)
{
  strcpy(modeldir, dir);
  //  strcpy(modelname, dir);

  if(!eio_chdir(modeldir))
    {
      return -1;
    }
  if(!eio_checkmodel(modeldir))
    {
      return -1;
    }
  if(!eio_chdir(modeldir))
    {
      return -1;
    }
  return 0;
}

int EIOModelManager::
closeModel()
{
  return 0;
}

int EIOModelManager::openStream(fstream& fstr, const char *name, int mode)
{
  fstr.open(name, (std::ios::openmode) mode);
//  if(!fstr)
  if(fstr.fail())
    {
      std::cerr << "Could not open " << name << std::endl;
      return 0;
    }
  return 1;
}

int EIOModelManager::closeStream(fstream& fstr)
{
  fstr.close();
  return 1;
}

int EIOModelManager::makeDirectory(const char *dir)
{
  return eio_mkdir(dir);
}

