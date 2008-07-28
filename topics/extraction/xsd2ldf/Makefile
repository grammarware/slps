input     = ../../../shared/xsd/xbgf.xsd
extracted = tests/xbgf.bgf
outputldf = tests/xbgf.ldf
outputpdf = tests/xbgf.pdf
outputhtm = tests/xbgf.html
templfd = tests/ldf.temp
validator = ../../../download/msv/msv.jar

all: extract
	sh ../../../shared/tools/ldf2html ${outputldf} ${outputhtm}
	sh ../../../shared/tools/ldf2pdf ${outputldf} ${outputpdf} 2> /dev/null

extract:
	sh ../../../shared/tools/xsd2bgf ${input} ${extracted}
	python ldfgen.py ${input} ${extracted} ${outputldf}
	xsltproc ../../../shared/xsl/ldf2ldf_tail.xslt ${outputldf} > ${templfd}
	xsltproc ../../../shared/xsl/ldf2ldf_lift.xslt ${templfd} > ${outputldf}
	@rm -f ${templfd}
    
test: extract
	sh ../../../shared/tools/showbgf ${extracted}
	java -jar ${validator} ../../../shared/xsd/bgf.xsd ${extracted}
	java -jar ${validator} ../../../shared/xsd/ldf.xsd ${outputldf}

clean:
	rm -f tests/*
