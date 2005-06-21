all: html

html:
	rm -Rf dist
	mkdir dist
	for f in `ls *.html`; do \
	  xsltproc menu.xsl $${f} > dist/$${f}; \
	done
	cp *.css dist/
	cp -r images dist/images

install:
	cp -r dist/* /var/www/csc.mep.fi/htdocs/

