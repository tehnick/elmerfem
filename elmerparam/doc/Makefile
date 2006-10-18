DOC=ElmerParamGuide
LATEX=latex

.SUFFIXES:
.SUFFIXES: .tex .dvi .ps .gle .eps .fig .png

.tex.dvi:
	$(LATEX) $<; 

.dvi.ps:
	dvips $< -o $@

.gle.eps:
	gle $< -deps

.fig.eps:
	fig2dev -L eps $< $@

.png.eps:
	convert $< $@


all: $(DOC).dvi

include .depend

$(DOC).ps: $(DOC).dvi

$(DOC).dvi $(DOC).aux: $(DOC).tex $(A_BILDER) $(S_BILDER)

bib $(DOC).bbl: $(DOC).aux
	bibtex $(DOC)

full: $(DOC).dvi bib
	$(LATEX) $(DOC).tex; $(LATEX) $(DOC).tex

clean:
	rm -f $(DOC).ps *.dvi *.toc *.aux *.log *.bbl *.blg $(A_BILDER)

depend .depend: $(DOC).tex
	latexdep $(DOC).tex > .depend



