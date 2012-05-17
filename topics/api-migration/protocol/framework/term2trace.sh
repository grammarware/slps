#! /bin/sh

(
    echo "term2trace("
    cat
    echo ")"
) | sglr -p trace2term.trm.tbl | asfe -e trace2term.eqs | unparsePT 