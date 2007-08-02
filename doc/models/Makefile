SHELL = /bin/sh

manual: 
	pdflatex manual
	makeindex manual
	make biblio
	pdflatex manual
	make biblio
	pdflatex manual

update:
	pdflatex manual

bib:
	sh loopchapterbib;

biblio:
	bibtex arterybc/arterybc
	bibtex artifcomp/artifcomp
	bibtex blockpreconditioning/blockpreconditioning
	bibtex electricforce/electricforce
	bibtex electrokinetics/electrokinetics
	bibtex linearplate/linearplate
	bibtex linearrestriction/linearrestriction
	bibtex navier-stokes/navier-stokes
	bibtex PoissonBoltzmann/PoissonBoltzmann

chmod:
	chmod -R ug+rw,o= .
	chmod ug=rwx figs

view:
	acroread manual.pdf

install:
	mv manual.pdf ../ElmerModelsManual.pdf

clean:
	/bin/rm -f *~ .*~ *.log *.aux *.bbl *.blg *.toc *.ilg *.idx *.ind
	/bin/rm -f */*~ */.*~ */*.aux */*.bbl */*.blg */*.log 




