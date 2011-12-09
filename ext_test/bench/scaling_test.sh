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
    stop-all $TEST_USER ${SERVERS[@]:0:$Count}
    setup-and-start-cluster $servers
    LST=$(echo "$Count-1" | bc)
    for i in $(seq 0 $LST); do
        set-bucket-n_vals ${SERVERS[$i]} $Count
    done

    # run the test
    RES_DIR=$RESULTS/scaling_$Count/
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER $servers
}

mkdir -p log/
for i in $@; do
    CONCURRENT=$i
    update-basho-config ${SERVERS[0]}
    test_scaling $i
done | tee $LOG
cp $LOG $RESULTS/log