all:
	make test

build:
	cp ../../mutation/anf/normal/*.normal.bgf base/

test:
	./converge.py master.bgf base/

clean:
	rm -f base/*.bgf
