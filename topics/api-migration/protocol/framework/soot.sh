#! /bin/sh

SYS_CLASSES=$1
SOOT_CLASSES=$2
SRC=$3
DEP_DIR=$4
CLASS=$5
OUTPUT=$6

CP=${SOOT_CLASSES}/jasmin-2.3.0/lib/jasminclasses-2.3.0.jar:${SOOT_CLASSES}/jasmin-2.3.0/lib/jasminsrc-2.3.0.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/coffer.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/java_cup.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/JFlex.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/pao.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/polyglot.jar:${SOOT_CLASSES}/polyglot-1.3.5/lib/pth.jar:${SOOT_CLASSES}/soot-2.3.0/lib/sootclasses-2.3.0.jar:${SOOT_CLASSES}/soot-2.3.0/lib/sootsrc-2.3.0.jar

# The deps correspond to the deps of Eclipse
SOOT_CP=${SRC}:${DEP_DIR}/dom4j-1.6.1/dom4j-1.6.1.jar\
:${DEP_DIR}/dom4j-1.6.1/lib/jaxen-1.1-beta-6.jar\
:${DEP_DIR}/jaxb-ri/lib/jaxb-api.jar\
:${DEP_DIR}/jaxb-ri/lib/jaxb-impl.jar\
:${DEP_DIR}/jaxb-ri/lib/jaxb-xjc.jar\
:${DEP_DIR}/jaxb-ri/lib/jsr173_1.0_api.jar\
:${DEP_DIR}/jdom-1.1/build/jdom.jar\
:${DEP_DIR}/jdom-1.1/lib/xerces.jar\
:${DEP_DIR}/jdom-1.1/lib/xalan.jar\
:${DEP_DIR}/jdom-1.1/lib/xml-apis.jar\
:${DEP_DIR}/junit-4.5.jar\
:${DEP_DIR}/XOM/xom-1.1.jar\
:${SYS_CLASSES}/classes.jar\
:${SYS_CLASSES}/charsets.jar\
:${SYS_CLASSES}/dt.jar\
:${SYS_CLASSES}/jce.jar\
:${SYS_CLASSES}/jconsole.jar\
:${SYS_CLASSES}/jsse.jar\
:${SYS_CLASSES}/laf.jar\
:${SYS_CLASSES}/ui.jar

SOOT="java -Xmx400m -cp ${CP} soot.Main"



# 
#    -p jap.rdtagger enabled 
#    -p jap.dmt enabled 
#
#     -p cg.spark enabled 
#    -annot-side-effect 
#    -print-tags 
#     -app 
#    -w 
#    -annot-fieldrw 
#     -verbose

FLAGS=-"-xml-attributes \
    -src-prec java \
    -output-format jimple"

# Format X gives XML

 ${SOOT} ${FLAGS} -soot-classpath ${SOOT_CP} -output-dir ${OUTPUT} ${CLASS}