all: elmer_param.

elmer_param.: elmer_param.c
	mex elmer_param.c -I.. -L.. -lelmerparam -lm

install: elmer_param.
	cp elmer_param. /home/edelmann/elmer/lib

distdir: clean
	cp -r ./* $(distdir)/

clean:
	rm -f elmer_param.

check:
