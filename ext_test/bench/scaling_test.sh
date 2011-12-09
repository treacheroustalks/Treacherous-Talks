#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

TT_CONFIG=config/tt_general.config
#MODE="{rate, 7}"
MODE="max"
DURATION=60        # in minutes
REPORT_INTERVAL=60 # in seconds

TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
LOG=log/scaling-$TIMESTAMP.log
RESULTS=scaling/$TIMESTAMP
SLEEP=5

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

    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG
    # run the test
    RES_DIR=$RESULTS/scaling_$Count/
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER $servers
}

mkdir -p log/
for i in $@; do
    sed -i "s:{.*mode,.*}:{mode, $MODE}:g" $TT_CONFIG
    sed -i "s:{.*concurrent,.*}:{concurrent, $i}:g" $TT_CONFIG
    sed -i "s:{.*duration,.*}:{duration, $DURATION}:g" $TT_CONFIG
    sed -i "s:{.*report_interval,.*}:{report_interval, $REPORT_INTERVAL}:g" $TT_CONFIG
    test_scaling $i
done | tee $LOG
cp $LOG $RESULTS/log