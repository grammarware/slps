#! /bin/sh

if [ -z $1 ] || [ -z $2 ]; then
    echo "Usage: migrate.sh <filewithannotatedstatements> <filewithtracemappingstatements>"
    exit 1
fi

if [ ! -f $1 ] ; then
    echo "Annotated statements file $1 does not exist."
    exit 1
fi
STATEMENTS=$1

if [ ! -f $2 ] ; then
    echo "Statement mapping $2 does not exist"
    exit 1
fi
MAPPING=$2

(
    echo "zip("
    cat ${STATEMENTS}
    echo ", "
    cat ${MAPPING}
    echo ")"
) | sglr -p ziptrace.trm.tbl | asfe -e ziptrace.eqs | unparsePT


