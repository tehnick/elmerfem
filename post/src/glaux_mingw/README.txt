Building glaux for MinGW:
--------------------------

1) Obtain glaux.h and place it in ./include/GL

The file glaux.h (build 0004) is distributed by MS in
their Platform SDK. Get it, or simply Google around for
"glaux.h 0004"

2) Building libglaux.a:

          ./compile.sh

3) Copy the libs and headers in /mingw:

          cp -r ./lib/* /mingw/lib/.
          cp -r ./include/* /mingw/include/.
