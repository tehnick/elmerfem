The Elmer documentation is now in a coherent way under the new cvs. This 
does not mean that we should immediately give the documentation away. 
Rather it means that if somebody wants to contribute we can sent him the 
necessary documentation package. Also it means that the documentation may 
be updated within minutes.

The different manuals are located in directories:

cscmodels
csctutorials
elmergrid
elmersolver
makedoc
models
tutorials

The manuals with a "csc" string are related to the non-GPL modules.

In practice you may do the following:

> cvs get doc
> cd doc
> ./makedoc

This will run the following script:
#!/bin/sh -f
modules="models tutorials elmersolver elmergrid cscmodels csctutorials"
for m in $modules; do
   cd $m && make manual && make install && make clean && cd ..
done

which creates the following manuals (size in kb:s):

2432 ElmerGridManual.pdf
1536 ElmerModelsManual.pdf
448 ElmerModelsManualCSC.pdf
1024 ElmerSolverManual.pdf
3744 ElmerTutorials.pdf
608 ElmerTutorialsCSC.pdf

The ones with a suffix CSC are not to be distributed openly. One may also 
of course make an own reduced set of the manuals for specific purposes.

Of these the ElmerSolverManual is run with latex, others use pdflatex 
directly. The main reason is economy in size and reusability of same 
pictures in www. Pdflatex eats pdf, png and jpg.

The style is almost plain report style except a few modifications that may 
be found at elmerdef.tex. If somebody has time and energy the style could 
be improved. The cscstyle had a complicated web of dependencies to csc 
environment and therefore chose to simplify the style.

There are basically two files that are common for all the subdirectories: 
elmerbib.bib and elmerdef.tex. Unfortunately I could not come up with a 
satisfactory way of having only one instance of these as cvs does not 
support symbolic links. Also the system should be such that even if only 
one subdirectory is given away it may be independently compiled. Therefore 
there are several instances of these files. If you update these files do 
it rather on the root level and copy the modifications also to the 
subdirectories.
