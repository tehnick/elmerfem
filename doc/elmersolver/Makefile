SHELL = /bin/sh

manual: 
	latex manual
	makeindex manual
	make biblio
	latex manual
	make biblio
	latex manual

pdf:
	dvips -o manual.ps manual
	ps2pdf manual.ps	

update:
	latex manual

bib:
	sh loopchapterbib;

biblio:	
	bibtex linearsystems/linearsystems
	bibtex fem/fem

chmod:
	chmod -R ug+rw,o= .
	chmod ug=rwx figs

view:
	xdvi manual

install:
	dvips -o manual.ps manual
	ps2pdf manual.ps 
	rm manual.ps
	mv manual.pdf ../ElmerSolverManual.pdf

clean:
	/bin/rm -f *~ .*~ *.log *.aux *.bbl *.blg *.toc *.ilg *.idx *.ind *.dvi
	/bin/rm -f */*~ */.*~ */*.aux */*.bbl */*.blg */*.log 
