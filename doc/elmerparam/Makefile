SHELL = /bin/sh

manual: 
	pdflatex ElmerParamGuide

update:
	pdflatex ElmerParamGuide

chmod:
	chmod -R ug+rw,o= .
	chmod ug=rwx figs

view:
	acroread manual.pdf

install:
	mv ElmerParamGuide.pdf ../ElmerParamManual.pdf

clean:
	/bin/rm -f *~ .*~ *.log *.aux *.bbl *.blg *.toc *.ilg *.idx *.ind
	/bin/rm -f */*~ */.*~ */*.aux */*.bbl */*.blg */*.log 




