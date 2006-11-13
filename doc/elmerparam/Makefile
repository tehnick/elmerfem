DOC=ElmerParamGuide
LATEX=latex

.SUFFIXES:
.SUFFIXES: .tex .dvi .ps .gle .eps .fig .png .ps .pdf

.tex.dvi:
	$(LATEX) $<; 
	$(LATEX) $<; 

.ps.pdf:
	ps2pdf $<

.dvi.ps:
	dvips $< -o $@

.gle.eps:
	gle $< -deps

.fig.eps:
	fig2dev -L eps $< $@

.png.eps:
	convert $< $@

all: manual

include .depend


manual: $(DOC).pdf

install: $(DOC).pdf
	cp $(DOC).pdf ../

$(DOC).pdf: $(DOC).ps
$(DOC).ps: $(DOC).dvi
$(DOC).dvi $(DOC).aux: $(DOC).tex $(A_BILDER) $(S_BILDER)

bib $(DOC).bbl: $(DOC).aux
	bibtex $(DOC)

full: $(DOC).dvi bib
	$(LATEX) $(DOC).tex; $(LATEX) $(DOC).tex

clean:
	rm -f $(DOC).ps $(DOC).pdf *.dvi *.toc *.aux *.log *.bbl *.blg

#depend .depend: $(DOC).tex
#	./latexdep $(DOC).tex > .depend



