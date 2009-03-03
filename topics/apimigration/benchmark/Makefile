
# Makefile to obtain required dependencies


DOM4J = dom4j-1.6.1.tar.gz
JAXB = jaxb-2_1_9.zip
JDOM =jdom-1.1.tar.gz
XOM = xom-1.2.zip
JUNIT = junit-4.5.jar

DOM4J_URL = "http://garr.dl.sourceforge.net/sourceforge/dom4j/${DOM4J}"
JAXB_URL = "https://jaxb.dev.java.net/2.1.9/${JAXB}"
JDOM_URL = "http://www.jdom.org/dist/binary/${JDOM}"
XOM_URL = "http://www.cafeconleche.org/XOM/${XOM}"
JUNIT_URL = "http://surfnet.dl.sourceforge.net/sourceforge/junit/${JUNIT}"

DEPS = deps

deps: dom4j jaxb jdom xom junit unpack

unpack: tars zips

zips:
	cd ${DEPS}; for z in *.zip; do unzip $$z; done; cd -

tars:
	cd ${DEPS}; for z in *.tar.gz; do tar zxvf $$z; done ; cd -

junit:
	curl -o ${DEPS}/${JUNIT} ${JUNIT_URL}

dom4j:
	curl -o ${DEPS}/${DOM4J} ${DOM4J_URL}

jdom:
	curl -o ${DEPS}/${JDOM} ${JDOM_URL}

xom:
	curl -o ${DEPS}/${XOM} ${XOM_URL}

# My curl doesn't like the certificate of jaxb.dev.java.net...
jaxb:
	curl --insecure -o ${DEPS}/${JAXB} ${JAXB_URL}

