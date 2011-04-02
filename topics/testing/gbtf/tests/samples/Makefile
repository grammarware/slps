build:

test:
	make $(patsubst %.bgf,%.test,$(wildcard *.bgf))

%.test:
	gbtf $*.bgf

clean:
	rm -f *~
	rm -f *.btf
	rm -f TMP-res
