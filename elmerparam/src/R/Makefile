all:

install:
	R CMD build elmerparam
	R CMD INSTALL -l /home/edelmann/elmer/lib/R elmerparam

distdir: clean
	cp -r ./* $(distdir)/

clean:
	rm -f elmerparam/src/*.{o,so}

check:
