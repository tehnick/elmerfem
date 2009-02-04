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

int main( int argc, char **argv )
{
  char strt[2000];
  char *str;

  (void)mtc_init( stdin, stdout, stderr );
  str = (char *)mtc_domath( "source(\"mc.ini\")" );

  signal( SIGINT, SIG_IGN );

  while( 1 )
  {
#ifdef USE_READLINE
      str = readline ("MATC> ");
      /* add to history */
      if (str && *str)
	add_history (str);

#else
      fgets( strt,  2000 , stdin);
      str = strt;      
#endif
      
/* kludge to enable exit. */
#if defined(WIN32) || defined(MINGW32)
      if( stricmp(str,"exit") == 0  || stricmp(str,"quit") == 0 )
#else
      if( strcasecmp(str,"exit") == 0  || strcasecmp(str,"quit") == 0 )
#endif
      {
	return 0;
      }
      if ( *str ) fprintf( stdout, "%s\n", (char *)mtc_domath( str ) );
      
#ifdef USE_READLINE
      free(str);
#endif
    }  
    return 0;
}
