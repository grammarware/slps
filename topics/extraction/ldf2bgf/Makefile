ldf2bgf = ../../../shared/tools/ldf2bgf
validator = ../../../download/msv/msv.jar
bgfxsd = ../../../shared/xsd/bgf.xsd

all:
	make clean
	make test

build:

test:
	find ../../.. -name "*.ldf" | xargs -n1 ${ldf2bgf}
	ls -1 *.bgf | xargs java -jar ${validator} ${bgfxsd}

clean:
	rm -f *.bgf
