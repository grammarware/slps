build:

test:
	make $(patsubst %.bgf,%.test,$(wildcard *.bgf))

%.test:
	gbtf $*.bgf

show:
	make $(patsubst %.btf,%.show,$(wildcard *.btf))

%.show:
	showt $*.btf

check:
	ls -1 *.btf | xargs -n1 validate btf

clean:
	rm -f *~
	rm -f *.btf
