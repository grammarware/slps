#! /bin/sh

set -e

TABLE=tracing.trm.tbl
EQS=tracing.eqs

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo "Usage: annotate.sh <methodname> <jimplefile> <apipatterns>"
    exit 1
fi

METHODNAME=$1

if [ ! -f $2 ] ; then
    echo "Jimple file $2 does not exist"
    exit 1
fi
JIMPLE=$2

if [ ! -f $3 ] ; then
    echo "API patterns file $3 does not exist"
    exit 1
fi
PATTERNS=$3

echo "annotate(\"${METHODNAME}\", \"${JIMPLE}\", \"${PATTERNS}\")" \
    | sglr -p ${TABLE} \
    | asfe -e ${EQS} -p ${TABLE} \
    | unparsePT



