#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

LOG=log/concurrent-$TIMESTAMP.log
RESULTS=concurrent/$TIMESTAMP

function usage {
    echo "Usage example: $0 machines=\"1 2 3\" concurrent=\"1 10 20 50\" mode=\"{rate, 7}\""
}

function concurrent-load-worker-test {
    Count=$1
    CONCURRENT=$2
    MODE=$3
    echonormal "#### TEST: concurrent_load_worker_test $1 $2 #### ####"
    echonormal "\t    PCs:\t$Count"
    echonormal "\t    Backends:\t$Count"
    echonormal "\t    Riak nodes:\t$Count"
    echonormal "\t    Concurrent worker:\t$CONCURRENT"

    servers="${SERVERS[@]:0:$Count}"
    RES_DIR=$RESULTS/m_$Count\_c_$CONCURRENT/

    setup-and-benchmark $RES_DIR $servers
}

if [ $# -ne 3 ] ; then
    usage
    exit 0
fi

machines=`echo "$1" | sed "s:machines=\(.\+\):\1:g"`
concurrent_worker=`echo "$2" | sed "s:concurrent=\(.\+\):\1:g"`
mode=`echo "$3" | sed "s:mode=\(.\+\):\1:g"`

if [ "$machines" == "$1" ] || [ "$concurrent_worker" == "$2" ]  || [ "$mode" == "$3" ] ; then
    usage
    exit 0
fi

mkdir -p log/
for m in $machines; do
    for c in $concurrent_worker; do
        concurrent-load-worker-test $m $c "$mode"
    done
done | tee $LOG
cp $LOG $RESULTS/log

