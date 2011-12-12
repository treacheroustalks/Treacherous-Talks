#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

LOG=log/scaling-$TIMESTAMP.log
RESULTS=scaling/$TIMESTAMP

# X pcs
# X backend nodes (all machines)
# X riak nodes (all machines)
# on every machine 1 backend & 1 riak
function test_scaling {
    Count=$1
    echonormal "#### TEST: test_scaling $Count #### ####"
    echonormal "\t    PCs:\t$Count"
    echonormal "\t    Backends:\t$Count"
    echonormal "\t    Riak nodes:\t$Count"

    servers="${SERVERS[@]:0:$Count}"
    stop-all $servers
    setup-and-start-cluster $servers
    set-all-bucket-props $servers

    # run the test
    RES_DIR=$RESULTS/scaling_$Count/
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $servers
}

mkdir -p log/
for i in $@; do
    CONCURRENT=$i
    update-basho-config ${SERVERS[0]}
    test_scaling $i
done | tee $LOG
cp $LOG $RESULTS/log