//=============================================================================
// savempg.c
//
// Module for compressing and saving ElmerPost-pictures in MPEG1 format
//
// Compile e.g. as follows (you'll need ffmpeg installed in $FFMPEG):
//
//   MinGW: 
//
//     > gcc -shared -O -I$FFMPEG/include -L$FFMPEG/lib -o savempg.dll 
//               savempg.c -lopengl32 -ltcl84 -lavcodec -lavutil -lz
//
//   Linux:
//
//     > more or less the same (-lGL -ltcl -lavcodec -lavutil -lz)
//
// Note that the libraries required depend on the libavcodec build.
//
// Copy the shared library into $ELMER_POST_HOME/modules and run ElmerPost
//
// Usage in Elmer-Post:
//
//    savempg bitrate value       [ default value: 400000 (bps)          ]
//    savempg start file.mpg      [ initializes video compressor         ]
//    savempg append              [ adds current frame to video sequence ]
//    savempg stop                [ finalizes video compressor           ]
//    
// Parsed together from screensave.c && the ffmpeg samples
//
// Written by: ML, 30. Sept. 2007
//=============================================================================
#ifdef HAVE_AV_CONFIG_H
#undef HAVE_AV_CONFIG_H
#endif

#if defined(WIN32) || defined(win32)
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <GL/gl.h>
#include <tcl.h>
#include <ffmpeg/avcodec.h>

#define INBUF_SIZE 4096
#define STATE_READY   0
#define STATE_STARTED 1

#undef MPEG4

void SetMessage( Tcl_Interp *interp, char *message ) {
  sprintf( interp->result, "savempg: %s\n", message );
}

static int SaveMPG( ClientData cl,Tcl_Interp *interp,int argc,char **argv ) {
  static AVCodec *codec;
  static AVCodecContext *context = NULL;
  static AVFrame *YUVpicture;
  static AVFrame *RGBpicture;
  static int MPGbytes, PIXsize, RGBstride;
  static int y, nx, ny, ox, oy, viewp[4];
  static int i_state = STATE_READY;
  static int initialized = 0;
  static int count_frames = 0;
  static int bitrate = 400000;
  static int MPGbufsize = 200000;
  static uint8_t *MPGoutbuf, *YUVbuffer, *RGBbuffer, *RGBrowbuf;
  static FILE *MPGfile;
  static char *state, fname[256];

  if( argc<2 ) {
    SetMessage( interp, "too few arguments" );
    return TCL_ERROR;
  }

  state = argv[1];
  //===========================================================================
  //                              SET QUALITY
  //===========================================================================
  if( !strncmp( state, "bitrate", 7 ) ) {

    // Can't change while running:
    //----------------------------
    if( i_state != STATE_READY ) {
      SetMessage( interp, "can't change bitrate when running" );
      return TCL_ERROR;
    }

    if( argc >= 3 )
      bitrate = atoi( argv[2] );
    
    fprintf( stdout, "savempg: bitrate set to: %d bps\n", bitrate );
    fflush( stdout );

    return TCL_OK;
  }

  //===========================================================================
  //                           START COMPRESSION
  //===========================================================================
  else if( !strncmp( state, "start", 5 ) ) {
    
    // Can't start if already running:
    //----------------------------------
    if( i_state != STATE_READY ) {
      SetMessage( interp, "already started" );
      return TCL_ERROR;
    }

    // Detemine output file name:
    //---------------------------
    if( argc < 3 ) {
#if defined(MPEG4)
      strcpy( fname, "elmerpost.raw" );
#else
      strcpy( fname, "elmerpost.mpg" );
#endif
    } else {
      strncpy( fname, argv[2], 256 );
    }
    
    // Open output file:
    //------------------
    if( (MPGfile = fopen(fname, "wb")) == NULL ) {
      SetMessage( interp, "can't open file" );
      return TCL_ERROR;
    }

    fprintf( stdout, "savempg: saving to: %s\n", fname );
    fprintf( stdout, "savempg: libavcodec: %s\n", 
	     AV_STRINGIFY(LIBAVCODEC_VERSION) );
    fprintf( stdout, "savempg: libavutil: %s\n", 
	     AV_STRINGIFY(LIBAVUTIL_VERSION) );
    fflush( stdout );

    count_frames = 0;

    // Determine view port size etc.:
    //-------------------------------
    glGetIntegerv( GL_VIEWPORT, viewp );
    ox = viewp[0];
    oy = viewp[1];
    nx = viewp[2]+1;
    ny = viewp[3]+1;
    PIXsize = nx*ny;
    RGBstride = 3*nx;

    // Must be even:
    //--------------
    if( nx % 2 || ny % 2 ) {
      fprintf( stdout, "savempg: state: stopped\n" );
      fflush( stdout );
      fclose( MPGfile );
      SetMessage( interp, "view port must be even" );
      return TCL_ERROR;
    }

    // Allocate memory:
    //-----------------
    if ( !(RGBbuffer = malloc(RGBstride*ny)) ||
	 !(RGBrowbuf = malloc(RGBstride)) ||
	 !(YUVbuffer = malloc(3*(PIXsize/2))) ||
	 !(MPGoutbuf = malloc(MPGbufsize)) ) {
      fclose( MPGfile );
      SetMessage( interp, "can't allocate memory" );
      return TCL_ERROR;
    }

    // Initialize libavcodec:
    //-----------------------
    if( !initialized ) {
      avcodec_init();
      avcodec_register_all();
      initialized = 1;
    }

    // Choose MPEG1:
    //--------------
#if defined(MPEG4)
    codec = avcodec_find_encoder( CODEC_ID_MPEG4 );
#else
    codec = avcodec_find_encoder( CODEC_ID_MPEG1VIDEO );
#endif

    if( !codec ) {
      free( RGBbuffer );
      free( RGBrowbuf );
      free( YUVbuffer );
      free( MPGoutbuf );
      fclose( MPGfile );
      SetMessage( interp, "can't find codec" );
      return TCL_ERROR;
    }
    
    // Init codec context etc.:
    //--------------------------
    context = avcodec_alloc_context();

    context->bit_rate = bitrate;
    context->width = nx;
    context->height = ny;
    context->time_base = (AVRational){ 1, 25 };
    context->gop_size = 10;
    context->max_b_frames = 1;
    context->pix_fmt = PIX_FMT_YUV420P;
  
    if( avcodec_open( context, codec ) < 0 ) {
      free( RGBbuffer );
      free( RGBrowbuf );
      free( YUVbuffer );
      free( MPGoutbuf );
      fclose( MPGfile );
      SetMessage( interp, "can't open codec" );
      return TCL_ERROR;
    }

    YUVpicture = avcodec_alloc_frame();
    RGBpicture = avcodec_alloc_frame();

    YUVpicture->data[0] = YUVbuffer;
    YUVpicture->data[1] = YUVbuffer + PIXsize;
    YUVpicture->data[2] = YUVbuffer + PIXsize + PIXsize / 4;
    YUVpicture->linesize[0] = nx;
    YUVpicture->linesize[1] = nx / 2;
    YUVpicture->linesize[2] = nx / 2;

    RGBpicture->data[0] = RGBbuffer;
    RGBpicture->data[1] = RGBbuffer;
    RGBpicture->data[2] = RGBbuffer;
    RGBpicture->linesize[0] = RGBstride;
    RGBpicture->linesize[1] = RGBstride;
    RGBpicture->linesize[2] = RGBstride;
    
    // Set state "started":
    //----------------------
    i_state = STATE_STARTED;
    fprintf( stdout, "savempg: state: started\n" );
    fflush( stdout );
    
    return TCL_OK;
  } 

  //===========================================================================
  //                             COMPRESS FRAME
  //===========================================================================
  else if( !strncmp( state, "append", 6) ) {

    // Can't compress if status != started:
    //-------------------------------------
    if( i_state != STATE_STARTED ) {
      SetMessage( interp, "not started" );
      return TCL_ERROR;
    }

    // Read RGB data:
    //---------------
    glReadBuffer( GL_FRONT );
    glReadPixels( ox, oy, nx, ny, GL_RGB, GL_UNSIGNED_BYTE, RGBbuffer );

    // The picture is upside down - flip it:
    //---------------------------------------
    for( y=0; y<ny/2; y++ ) {
      uint8_t *RGBrow1 = RGBbuffer + RGBstride*y;
      uint8_t *RGBrow2 = RGBbuffer + RGBstride*(ny-1-y);
      memcpy( RGBrowbuf, RGBrow1, RGBstride );
      memcpy( RGBrow1, RGBrow2, RGBstride );
      memcpy( RGBrow2, RGBrowbuf, RGBstride );
    }

    // Convert to YUV:
    //----------------
    img_convert( (AVPicture*)YUVpicture, PIX_FMT_YUV420P, 
		 (AVPicture*)RGBpicture, PIX_FMT_RGB24, nx, ny );

    // Encode frame:
    //--------------
    MPGbytes = avcodec_encode_video( context, MPGoutbuf, MPGbufsize, YUVpicture );
    count_frames++;

    fprintf( stdout, "savempg: encoded frame %5d (size:%6d bytes)\n", 
	     count_frames, MPGbytes );
    fflush( stdout );

    fwrite( MPGoutbuf, 1, MPGbytes, MPGfile );
    
    return TCL_OK;
  } 

  //===========================================================================
  //                           STOP COMPRESSION
  //===========================================================================
  else if( !strncmp( state, "stop", 4) ) {

    // Can't stop if status != started:
    //---------------------------------
    if( i_state != STATE_STARTED ) {
      SetMessage( interp, "not started" );
      return TCL_ERROR;
    }

    // Get the delayed frames:
    //-------------------------
    for( ; MPGbytes; ) {
      fflush( stdout );
      MPGbytes = avcodec_encode_video( context, MPGoutbuf, MPGbufsize, NULL );
      count_frames++;
      fprintf( stdout, "savempg: wrote frame   %5d (size:%6d bytes)\n", 
	       count_frames, MPGbytes );
      fwrite( MPGoutbuf, 1, MPGbytes, MPGfile );
    }
    
    // Add sequence end code:
    //-----------------------
#if !defined(MPEG4)
    MPGoutbuf[0] = 0x00;
    MPGoutbuf[1] = 0x00;
    MPGoutbuf[2] = 0x01;
    MPGoutbuf[3] = 0xb7;
    fwrite( MPGoutbuf, 1, 4, MPGfile );
#endif
    fclose( MPGfile );

    free( YUVbuffer );
    free( MPGoutbuf );
    
    // Finalize:
    //-----------
    avcodec_close( context );
    av_free( context );
    av_free( YUVpicture );
    av_free( RGBpicture );
    
    i_state = STATE_READY;
    fprintf( stdout, "savempg: state: stopped\n" );
    fflush( stdout );

    free( RGBbuffer );
    free( RGBrowbuf );
    
    return TCL_OK;
  } 

  //===========================================================================
  //                              UNKNOWN STATE
  //===========================================================================
  else {
    SetMessage( interp, "unknown request" );
    return TCL_ERROR;
  }

  // We should never ever arrive at this point:
  //-------------------------------------------
  SetMessage( interp, "unknown error" );
  return TCL_ERROR;
}

#if defined(WIN32) || defined(win32)
__declspec(dllexport)
#endif
     
int Savempg_Init( Tcl_Interp *interp ) {
  Tcl_CreateCommand( interp, "savempg", (Tcl_CmdProc *)SaveMPG,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  return TCL_OK;
}
