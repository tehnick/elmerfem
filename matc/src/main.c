
/*
 * $Id: main.c,v 1.6 2005/06/07 10:55:15 jpr Exp $ 
 *
 * $Log: main.c,v $
 * Revision 1.6  2005/06/07 10:55:15  jpr
 * *** empty log message ***
 *
 * Revision 1.5  2005/05/26 12:34:54  vierinen
 * windows stuff
 *
 * Revision 1.4  2005/05/11 06:45:37  vierinen
 * stupid readline update
 *
 * Revision 1.3  2005/04/29 11:19:16  vierinen
 * stupid comment line fix
 *
 * Revision 1.2  2005/04/15 06:54:30  vierinen
 * added gnu readline support
 *
 * Revision 1.1.1.1  2005/04/14 13:29:14  vierinen
 * initial matc automake package
 *
 * Revision 1.2  1998/08/01 12:34:47  jpr
 *
 * Added Id, started Log.
 * 
 *
 */

#include <stdio.h>
#include <signal.h>

#include "../config.h"

#ifdef USE_READLINE
# ifdef HAVE_READLINE_READLINE_H
#  include <readline/readline.h>
#  include <readline/history.h>
# else
#  ifdef HAVE_READLINE_H
#   include <readline.h>
#   include <history.h>
#  endif
# endif
#endif 

#if !defined( __WINDOWS__ ) && ( defined( _Windows ) || defined( _WINDOWS ) )
  #define __WINDOWS__
#endif /* !__WINDOWS__ && ( _Windows || _WINDOWS ) */
#if !defined( __WIN32__ ) && ( defined( WIN32 ) || defined( _WIN32 ) )
  #ifndef __WINDOWS__
    #define __WINDOWS__
  #endif /* __WINDOWS__ */
  #define __WIN32__
#endif /* !__WIN32__ && ( WIN32 || _WIN32 ) */
#if defined( __WINDOWS__ ) && !defined( __WIN32__ )
  #define __WIN16__
#endif /* __WINDOWS__ && !__WIN32__ */
 
/* and now I map it to my version of the above define --tjh */
#ifdef __WINDOWS__
#ifndef WINDOWS
#define WINDOWS
#endif
#endif /* __WINDOWS__ */


void main( int argc, char **argv )
{
  char strt[2000];
  char *str;

    mtc_init( stdin,stdout, stderr );
    mtc_domath( "source(\"mc.ini\")" );

    signal( SIGINT, SIG_IGN );
    while( 1 )
    {
#ifdef USE_READLINE
      str = readline ("MATC> ");
      /* add to history */
      if (str && *str)
	add_history (str);

#else
      fprintf( stdout, "MATC> " );
      fgets( strt,  2000 , stdin);
      str = strt;      
#endif
      
/* kludge to enable exit. */
#ifdef WINDOWS
      if( stricmp(str,"exit") == 0  || stricmp(str,"quit") == 0 )
#else
      if( strcasecmp(str,"exit") == 0  || strcasecmp(str,"quit") == 0 )
#endif
      {
	return;
      }
      if ( *str ) fprintf( stdout, "%s\n", mtc_domath( str ) );
      
#ifdef USE_READLINE
      free(str);
#endif
    }  
    return;
}
