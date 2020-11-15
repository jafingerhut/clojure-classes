#! /bin/bash

if [ $# -ne 2 ]
then
    1>&2 echo "usage: `basename $0` <clojure-repository-local-root> <output-dir>"
    1>&2 echo ""
    1>&2 echo "For example:"
    1>&2 echo ""
    1>&2 echo "    `basename $0` /home/andy/clojure generate"
    exit 1
fi

CLOJURE_REPO_LOCAL_COPY=$1
OUTPUT_DIR=$2

#CLOJURE_REPO_LOCAL_COPY="/Users/andy/clj/clojure"
#OUTPUT_DIR="generate"

DIR=$PWD

for CLOJURE_VERSION in \
	1.3.0 \
	1.4.0 \
	1.5.1 \
	1.6.0 \
	1.7.0 \
	1.8.0 \
	1.9.0 \
	1.10.1
do
    # Check out the appropriate git tag in the Clojure source tree.
    cd $CLOJURE_REPO_LOCAL_COPY
    set -x
    git checkout clojure-${CLOJURE_VERSION}
    set +x

    cd $DIR
    set -x
    clojure -Sdeps "{:deps {org.clojure/clojure {:mvn/version \"${CLOJURE_VERSION}\"}}}" -m net.n01se.clojure-classes ${CLOJURE_REPO_LOCAL_COPY} ${OUTPUT_DIR}
    set +x
done

cd ${OUTPUT_DIR}
for d in *.dot
do
    b=`basename $d .dot`
    set -x
    dot -Tpdf $d > $b.pdf
    dot -Tsvg $d > $b.svg
    set +x
done

# clojure -Sdeps "{:deps {org.clojure/clojure {:mvn/version \"1.3.0\"}}}" -m net.n01se.clojure-classes /Users/andy/clj/clojure
