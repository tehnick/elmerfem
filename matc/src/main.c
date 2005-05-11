
/*
 * $Id: main.c,v 1.3 2005/04/29 11:19:16 vierinen Exp $ 
 *
 * $Log: main.c,v $
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
      if ( *str ) fprintf( stdout, "%s\n", mtc_domath( str ) );
      
      /* kludge to enable exit. */
      if( strcasecmp( str, "exit") == 0  || strcasecmp( str, "quit") == 0 )
      {
	return;
      }
      
#ifdef USE_READLINE
      free(str);
#endif
    }  
    return;
}
