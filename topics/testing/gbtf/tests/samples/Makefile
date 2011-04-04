build:

test:
	make $(patsubst %.bgf,%.test,$(wildcard *.bgf))

%.test:
	gbtf $*.bgf

show:
	make $(patsubst %.btf,%.show,$(wildcard *.btf))

%.show:
	showt $*.btf

clean:
	rm -f *~
	rm -f *.btf
	rm -f TMP-res
