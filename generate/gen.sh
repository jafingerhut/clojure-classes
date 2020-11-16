#! /bin/bash

if [ $# -ne 2 ]
then
    1>&2 echo "usage: `basename $0` <output-dir> <clojure-repository-local-root>"
    1>&2 echo ""
    1>&2 echo "For example:"
    1>&2 echo ""
    1>&2 echo "    `basename $0` generate /home/andy/clojure"
    exit 1
fi

OUTPUT_DIR=$1
CLOJURE_REPO_LOCAL_COPY=$2

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
    clojure -Sdeps "{:deps {org.clojure/clojure {:mvn/version \"${CLOJURE_VERSION}\"}}}" -m net.n01se.clojure-classes ${OUTPUT_DIR} all-clojure-classes ${CLOJURE_REPO_LOCAL_COPY}
    set +x
done
