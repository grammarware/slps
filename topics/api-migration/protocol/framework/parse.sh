#! /bin/sh

if [ -z $1 ] || [ -z $2 ]; then
    echo "Usage: parse-trace.sh <grammar> <trace>"
    exit 1
fi

if [ ! -f $1 ] ; then
    echo "Grammar $1 does not exist"
    exit 1
fi
GRAMMAR=$1

if [ ! -f $2 ] ; then
    echo "Trace $2 does not exist"
    exit 1
fi
TRACE=$2

swipl --quiet -s tracing.pro -f ${GRAMMAR} -t "parse_trace_from_file('${TRACE}',Doc)"