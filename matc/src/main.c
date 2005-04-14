
/*
 * $Id: main.c,v 1.2 1998/08/01 12:34:47 jpr Exp $ 
 *
 * $Log: main.c,v $
 * Revision 1.2  1998/08/01 12:34:47  jpr
 *
 * Added Id, started Log.
 * 
 *
 */

#include <stdio.h>
#include <signal.h>

void main( int argc, char **argv )
{
    char str[2000];

    mtc_init( stdin,stdout, stderr );
    mtc_domath( "source(\"mc.ini\")" );

    signal( SIGINT, SIG_IGN );
    while( 1 )
    {
      fprintf( stdout, "MATC> " );
      fgets( str,  2000 , stdin);

      if ( *str ) fprintf( stdout, "%s\n", mtc_domath( str ) );
      
      // kludge to enable exit.
      if( strcasecmp( str, "exit") == 0  || strcasecmp( str, "quit") == 0 )
      {
	return(0);
      }
    }  
    return(0);
}
