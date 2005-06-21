all: html print

html:
	rm -Rf dist
	mkdir dist
	for f in `ls *.html`; do \
	  xsltproc menu.xsl $${f} | sed -e "s/@print_version@/print_$${f}/g" > dist/$${f}; \
	done

	cp *.css dist/
	cp -r images dist/images

install:
	cp -r dist/* /var/www/csc.mep.fi/htdocs/

print:
	for f in `ls *.html`; do \
	  xsltproc print.xsl $${f} > "dist/print_$${f}"; \
	done
	cp *.css dist/
	cp -r images dist/images


