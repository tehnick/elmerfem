The Elmer documentation is now in a coherent way under version control. 
The different manuals are located in directories:

elmergrid
elmersolver
models
tutorials
matc 
overview
elmerparam

To compile all the tutorials do the following:

> cd doc
> ./makedoc

This will build the individual manuals and package the examples related to
them. Finally it will move them to directory Documentation from where 
they can be further copied to the desired destination. The content of 
directory Documentation should look something like:

ElmerGridFiles.tar.gz
ElmerGridManual.pdf
ElmerModelsManual.pdf
ElmerOverview.pdf
ElmerParamManual.pdf
ElmerSolverManual.pdf
ElmerTutorials.pdf
ElmerTutorialsFiles.tar.gz
MATCManual.pdf

Of the manual the ElmerSolverManual is run with latex, others use pdflatex 
directly. The main reason is economy in size and reusability of same 
pictures in www. Pdflatex accepts pdf, png and jpg.

The style is almost plain report style except a few modifications that may 
be found at elmerdef.tex. If somebody has time and energy the style could 
be improved. 

There are basically two files that are common for all the subdirectories: 
elmerbib.bib and elmerdef.tex. Unfortunately I could not come up with a 
satisfactory way of having only one instance of these as cvs does not 
support symbolic links. Also the system should be such that even if only 
one subdirectory is given away it may be independently compiled. Therefore 
there are several instances of these files. If you update these files do 
it rather on the root level and copy the modifications also to the 
subdirectories.
