input     = ../../../shared/xsd/xbgf.xsd
output    = tests/xbgf

build:

all: extract
	../../../shared/tools/ldf2html ${output}.ldf ${output}.html
	../../../shared/tools/ldf2pdf ${output}.ldf ${output}.pdf 2> /dev/null

extract:
	../../../shared/tools/xsd2bgf ${input} ${output}.bgf
	python ldfgen.py ${input} ${output}.bgf ${output}.1.ldf
	xsltproc ../../../shared/xsl/ldf2ldf_tail.xslt ${output}.1.ldf > ${output}.2.ldf
	xsltproc ../../../shared/xsl/ldf2ldf_lift.xslt ${output}.2.ldf > ${output}.ldf
    
test: extract
	../../../shared/tools/showbgf ${output}.bgf > ${output}.out
	../../../shared/tools/checkxml bgf ${output}.bgf
	../../../shared/tools/checkxml ldf ${output}.ldf

clean:
	rm -f tests/*
