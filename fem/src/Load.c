/******************************************************************************
! *
! *       ELMER, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 - , Center for Scientific Computing,
! *                                    Finland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! *****************************************************************************/
 
/*******************************************************************************
! *
! *  Utilities for dynamic loading of user functions, and other operating
! *  system interfaces.
! *
! ******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2302
! *                              EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 02 Jun 1997
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* eg. FC_CHAR_PTR and FC_FUNC is defined here */
#include "../config.h"

#ifdef WIN32
#  include <direct.h>
#  include <windows.h>
#else
#include <strings.h>
#  include <dlfcn.h>
#endif

#define MAX_NAME_LEN 128

#ifdef SGI64
void corename_()
{
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/prctl.h>

 prctl( PR_COREPID,0,0 );
}
#endif

/* WIN32 needs some redundant attributes */
#ifdef WIN32
#define STDCALLBULL __stdcall
#else
#define STDCALLBULL 
#endif

/* pc needs more bits on 64bit arch  */
#ifdef ARCH_32_BITS
#define f_ptr int *
#else 
#define f_ptr long int *
#endif

/*--------------------------------------------------------------------------
  This routine will create a directory given name of the directory.
  -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(makedirectory,MAKEDIRECTORY) 
     ( FC_CHAR_PTR(Name,len) )
{
#ifdef WIN32
    if ( _mkdir( Name ) != 0 ) {
#else
    if ( mkdir( Name, 0x700 ) != 0 ) {
      chmod( Name, 0x700 );
#endif
    }
}

/*--------------------------------------------------------------------------
  This routine execute a operating system command.
  -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(systemc,SYSTEMC) ( FC_CHAR_PTR(str,l1) )
{
   system( str );
}

/*--------------------------------------------------------------------------
  This routine will return value of a environment variable to a
  given string variable.
  -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(envir,ENVIR) ( FC_CHAR_PTR(Name,l1), FC_CHAR_PTR(Value,l2), int *len )
{
    if ( getenv( Name ) ) {
      strncpy( Value,(char *)getenv(Name), MAX_NAME_LEN );
      *len = strlen( Value );
    } else {
      *len = 0;
      *Value = '\0';
    }
}

/*--------------------------------------------------------------------------
  Internal: convert function names into to fortran mangled form for dynamical
  loading
  ---------------------------------------------------------------------------*/
void fortranMangle(char *orig, char *mangled)
{
  int uscore, i;
  
  if(ELMER_LINKTYP == 1 || ELMER_LINKTYP == 3 || ELMER_LINKTYP == 4)
  {
    for( i=0 ; i<strlen(orig) ; i++ ) /* to lower case */
    {
      if ( orig[i] >= 'A'  && orig[i] <= 'Z' ) 
	orig[i] += 'a' - 'A';
    }
  }
  if(ELMER_LINKTYP == 2)
  {
    for( i=0; i<strlen(orig); i++ ) /* to upper case */
    {
      if ( orig[i] >= 'a'  && orig[i] <= 'z' ) 
	orig[i] += 'A' - 'a';
    }
  }
  
  strcpy( mangled, orig );
  if(ELMER_LINKTYP == 1) /* underscore */
  {
      strcat( mangled, "_" );
  }
  else if(ELMER_LINKTYP == 4) /* 1-2 underscores  */
  {
    uscore = 0;
    for( i=0; i<strlen(mangled); i++ )
      if(mangled[i] == '_')
	uscore++;
    
    if(uscore == 0)
    {
      strcat( mangled, "_" );
    } 
    else 
    {
      strcat( mangled, "__" );
    }
  }
}

/*--------------------------------------------------------------------------
  This routine will return address of a function given path to a dynamically
  loaded library and name of the routine.
  -------------------------------------------------------------------------*/
void *STDCALLBULL FC_FUNC(loadfunction,LOADFUNCTION) ( int Quiet, FC_CHAR_PTR(Library,l1),FC_CHAR_PTR(Name,l2) )
{
/*--------------------------------------------------------------------------*/
   void (*Function)(),*Handle;
   int i;
   char NewName[MAX_NAME_LEN];
   char NewLibName[MAX_NAME_LEN];
/*--------------------------------------------------------------------------*/
   
   fortranMangle( Name, NewName );
   strcpy( NewLibName, Library );

   if ( Quiet==0 ) 
     fprintf(stdout,"Loading user function library: [%s]...[%s]", NewLibName, NewName);
   
#ifdef HAVE_DLOPEN_API
   if ( ( Handle = dlopen( NewLibName , RTLD_NOW ) ) == NULL )
     { 
       fprintf( stderr, "Load: WARNING: Can't load shared image [%s]\n", NewLibName );
       fprintf( stderr, "Load: [%s]\n", dlerror() );

       /* Try again with shared library extension */
       strcat( NewLibName, SHL_EXTENSION );
       fprintf( stderr, "Trying %s\n", NewLibName );
       if ( ( Handle = dlopen( NewLibName , RTLD_NOW ) ) == NULL )
       { 
	   fprintf( stderr, "Load: FATAL: Can't load shared image [%s]\n", NewLibName );
	   fprintf( stderr, "Load: [%s]\n", dlerror() );
	   exit(0);
       }
   }

   if ( (Function = (void(*)())dlsym( Handle,NewName ) ) == NULL )
   {
      fprintf( stderr, "Load: FATAL: Can't find procedure [%s]\n", NewName );
      exit(0);
   }
#elif defined(HAVE_LOADLIBRARY_API)

   if ( ( Handle = (void *)LoadLibrary( Library ) ) == NULL )
   { 
     fprintf( stderr, "Load: FATAL: Can't load shared image [%s]\n", Library );
     exit(0);
   }

   for( i=0; i<strlen(Name); i++ )
   {
     if ( Name[i] >= 'a'  && Name[i] <= 'z' ) Name[i] += 'A' - 'a';
   }

   if ( (Function = (void *)GetProcAddress( Handle,Name ) ) == NULL )
   {
     fprintf( stderr,"Load: FATAL: Can't find procedure [%s]\n", Name );
     exit(0);
   }
#endif 
   
   if ( Quiet == 0 ) 
     fprintf( stdout, "...done.\n" );

   return (void *)Function;
}

/*--------------------------------------------------------------------------
  INTERNAL: Execute given function returning integer value
  -------------------------------------------------------------------------*/
static int IntExec( int (*Function)(),void *Model )
{
   return (*Function)( Model );
}

/*--------------------------------------------------------------------------
   Execute given function returning integer value
   -------------------------------------------------------------------------*/
int STDCALLBULL FC_FUNC(execintfunction,EXECINTFUNCTION) ( f_ptr Function,void *Model )
{
   return IntExec( (int (*)())*Function,Model );
}

/*--------------------------------------------------------------------------
   INTERNAL: Execute given function returning double value
  -------------------------------------------------------------------------*/
static void DoubleArrayExec( double *(*Function)(), void *Model,
               int *Node, double *Value, double *Array )
{
   (*Function)( Model,Node,Value,Array );
}

/*--------------------------------------------------------------------------
   Execute given function returning double value
   -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(execrealarrayfunction,EXECREALARRAYFUNCTION)
     ( f_ptr Function, void *Model,
       int *Node, double *Value, double *Array )
{
   DoubleArrayExec( (double*(*)())*Function,Model,Node,Value, Array );
}

/*--------------------------------------------------------------------------
   INTERNAL: Execute given function returning double value
  -------------------------------------------------------------------------*/
static double DoubleExec( double (*Function)(), void *Model,
               int *Node, double *Value )
{
   return (*Function)( Model,Node,Value );
}

/*--------------------------------------------------------------------------
   Execute given function returning double value
   -------------------------------------------------------------------------*/
double STDCALLBULL FC_FUNC(execrealfunction,EXECREALFUNCTION)
     ( f_ptr Function, void *Model,
       int *Node, double *Value )
{
   return DoubleExec( (double (*)())*Function,Model,Node,Value );
}

/*--------------------------------------------------------------------------
   INTERNAL: Execute given function returning double value
  -------------------------------------------------------------------------*/
static double ConstDoubleExec( double (*Function)(), void *Model,
			       double *x, double *y, double *z )
{
   return (*Function)( Model, x,y,z );
}

/*--------------------------------------------------------------------------
   Execute given function returning double value
   -------------------------------------------------------------------------*/
double STDCALLBULL FC_FUNC(execconstrealfunction,EXECCONSTREALFUNCTION)
     ( f_ptr Function, void *Model,
       double *x, double *y, double *z )
{
   return ConstDoubleExec( (double (*)())*Function,Model,x,y,z );
}


/*--------------------------------------------------------------------------
   Return argument (just to fool Fortran type checking)
   -------------------------------------------------------------------------*/
void *STDCALLBULL FC_FUNC(addrfunc,ADDRFUNC) ( void *Function )
{
   return (void *)Function;
}

/*--------------------------------------------------------------------------
   INTERNAL: Call solver routines at given address
  -------------------------------------------------------------------------*/
static void DoExecSolver(
  void (*SolverProc)(), void *Model, void *Solver, void *dt, void *Transient)
{
   (*SolverProc)( Model,Solver,dt,Transient );
}

/*--------------------------------------------------------------------------
   Call solver routines at given address
   -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(execsolver,EXECSOLVER)
     ( f_ptr *SolverProc, void *Model, void *Solver, void *dt, void *Transient )
{
  DoExecSolver( (void (*)())*SolverProc,Model,Solver,dt,Transient );
}

/*--------------------------------------------------------------------------
   INTERNAL: Call lin. solve routines at given address
  -------------------------------------------------------------------------*/
static int DoLinSolveProcs(
  int (*SolverProc)(), void *Model, void *Solver, void *Matrix, void *b, 
                void *x, void *n, void *DOFs, void *Norm )
{
   return (*SolverProc)( Model,Solver,Matrix,b,x,n, DOFs,Norm );
}


/*--------------------------------------------------------------------------
   Call lin. solver routines at given address
   -------------------------------------------------------------------------*/
int STDCALLBULL FC_FUNC(execlinsolveprocs,EXECLINSOLVEPROCS)
     ( f_ptr *SolverProc, void *Model, void *Solver, void *Matrix, void *b, void *x, void *n, void *DOFs, void *Norm )
{
   return DoLinSolveProcs( (int (*)())*SolverProc,Model,Solver,Matrix,b,x,n,DOFs,Norm );
}

char *mtc_domath(char *);
void mtc_init(FILE *,FILE *, FILE *);

/*--------------------------------------------------------------------------
  This routine will call matc and return matc result
  -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(matc,MATC) ( FC_CHAR_PTR(cmd,l1), FC_CHAR_PTR(Value,l2), int *len )
{
#define MAXLEN 8192

    static int been_here = 0;
    char *ptr, cc[32], *cmdcopy;

    if ( been_here==0 ) {
#ifdef WIN32
       mtc_init_std( NULL, stdout, stderr ); 
       mtc_domath_std( "format(12,\"rowform\")" );
#else
       mtc_init( NULL, stdout, stderr ); 
       strcpy( cc, "format( 12,\"rowform\")" );
       mtc_domath( cc );
#endif
       been_here = 1;
     }

    cmd[*len] = '\0';

#ifdef WIN32
    if ( ptr = (char *)mtc_domath_std( cmd ) ) {
#else
    if ( ptr = (char *)mtc_domath( cmd ) ) {
#endif
      strcpy( Value, (char *)ptr );
      *len = strlen(Value)-1;

      if ( strncmp( Value, "MATC ERROR:", 11 ) == 0 ) {
          fprintf( stderr, "Solver input file error: %s\n", Value );
          exit(0);
      }
    } else {
      *len = 0;
      *Value = ' ';
    }
}

/*--------------------------------------------------------------------------
  INTERNAL: execute user material function
  -------------------------------------------------------------------------*/
static double DoViscFunction(double (*SolverProc)(), void *Model, void *Element, void *Nodes, void *n,
     void *Basis, void *GradBasis, void *Viscosity, void *Velo, void *GradV )
{
   double s;
   s = (*SolverProc)( Model,Element,Nodes,n,Basis,GradBasis,
                   Viscosity, Velo, GradV );
   return s;
}

/*--------------------------------------------------------------------------
  This routine will call user defined material def. function
  -------------------------------------------------------------------------*/
double STDCALLBULL FC_FUNC(materialuserfunction,MATERIALUSERFUNCTION)
  ( f_ptr Function, void *Model, void *Element, void *Nodes, void *n, void *Basis,
    void *GradBasis, void *Viscosity, void *Velo, void *gradV )
{
   return DoViscFunction( (double (*)())*Function,Model,Element,Nodes,n,Basis,
                  GradBasis,Viscosity,Velo,gradV );
}

/*--------------------------------------------------------------------------
  INTERNAL: execute user material function
  -------------------------------------------------------------------------*/
static void DoSimulationProc( void (*SimulationProc)(), void *Model )
{ (*SimulationProc)( Model ); }

/*--------------------------------------------------------------------------
  This routine will call user defined material def. function
  -------------------------------------------------------------------------*/
void STDCALLBULL FC_FUNC(execsimulationproc,EXECSIMULATIONPROC)
     ( f_ptr Function, void *Model )
{
   DoSimulationProc( (void (*)())*Function,Model );
}
