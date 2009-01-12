#! /bin/sh

if [ -z $1 ]; then
    echo "Usage: xo-jdom2dom-mapped.sh <trace>"
    exit 1
fi

if [ ! -f $1 ] ; then
    echo "Trace $1 does not exist"
    exit 1
fi
TRACE=$1

swipl --quiet -s xo-jdom2dom.pro -t "mapped('${TRACE}')"