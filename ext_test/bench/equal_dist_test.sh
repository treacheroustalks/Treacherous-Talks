#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

TT_CONFIG=config/tt_general.config
CONCURRENT=1
#MODE="{rate, 7}"
MODE="max"
DURATION=60        # in minutes
REPORT_INTERVAL=60 # in seconds

TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
LOG=log/equal_dist-$TIMESTAMP.log
RESULTS=equal_dist/$TIMESTAMP

# X pcs
# X backend nodes (all machines)
# X riak nodes (all machines)
# on every machine 1 backend & 1 riak
function test_equal_dist {
    Count=$1
    echonormal "TEST: test_equal_dist $Count"
    echonormal "\t    PCs:\t$Count"
    echonormal "\t    Backends:\t$Count"
    echonormal "\t    Riak nodes:\t$Count"

    servers="${SERVERS[@]:0:$Count}"
    stop-all $servers
    setup-and-start-cluster $servers

    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG
    # run the test
    RES_DIR=$RESULTS/equal_dist_$Count/
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $servers
}

if [ $# -ne 1 ] ; then
    echo "You need to specify the amount of machines to use"
    exit 0
fi

sed -i "s:{.*mode,.*}:{mode, $MODE}:g" $TT_CONFIG
sed -i "s:{.*duration,.*}:{duration, $DURATION}:g" $TT_CONFIG
sed -i "s:{.*report_interval,.*}:{report_interval, $REPORT_INTERVAL}:g" $TT_CONFIG
sed -i "s:{.*concurrent,.*}:{concurrent, $CONCURRENT}:g" $TT_CONFIG

mkdir -p log/
test_equal_dist $1 | tee $LOG

cp $LOG $RESULTS/log
