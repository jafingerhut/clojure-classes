#! /bin/bash

for d in *.dot
do
    b=`basename $d .dot`
    set -x
    dot -Tpdf $d > $b.pdf
    dot -Tsvg $d > $b.svg
    set +x
done
