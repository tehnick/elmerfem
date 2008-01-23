echo compiling sources...
cd src
gcc -c font.c -O -o font.o
gcc -c image.c -O -o image.o
gcc -c teapot.c -O -o teapot.o
gcc -c tkdib.c -O -o tkdib.o
gcc -c tkimage.c -O -o tkimage.o
gcc -c xform.c -O -o xform.o
gcc -c glaux.c -O -o glaux.o
gcc -c shapes.c -O -o shapes.o
gcc -c tk.c -O -o tk.o
gcc -c tkfont.c -O -o tkfont.o
gcc -c vect3d.c -O -o vect3d.o
echo creating library...
ar r libglaux.a `ls *.o`
echo cleaning up...
mv libglaux.a ../lib
rm *.o
cd ..
echo done