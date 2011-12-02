#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

TT_CONFIG=config/tt_general.config
DURATION=10 # in minutes
REPORT_INTERVAL=30 # in seconds

TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
LOG=log/burn-$TIMESTAMP.log
RESULTS=burn/$TIMESTAMP
SLEEP=5

function test_burn {
    Load=$1
    echonormal "TEST: test_burn $Load"
    echonormal "####: PCs:\t$1"
    echonormal "####: Backends:\t$1"
    echonormal "####: Riak nodes:\t$1"

    server=${SERVERS[0]}
    stop $TEST_USER@$server

    RES_DIR=$RESULTS/burn_$Load/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@$server'}:g" $TT_CONFIG
    sed -i "s:{.*concurrent,.*}:{concurrent, $Load}:g" $TT_CONFIG

    # setup riak
    IP=`host $server | sed "s:.* ::"`
    echonormal "Setting up riak nodes on machine $server"
    copy-mult-riak $TEST_USER $server \
        $LOCAL_RIAK_DIR $RIAK_DIR \
        $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
        1 $RIAK_DIR\1@$IP
    # setup all backends
    echonormal "Setting up backend node on $server"
    copy-backend $TEST_USER $server $LOCAL_B_DIR $B_DIR $IP 1 ""
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@$server

    stop $TEST_USER@$server
}

sed -i "s:{.*duration,.*}:{duration, $DURATION}:g" $TT_CONFIG
sed -i "s:{.*report_interval,.*}:{report_interval, $REPORT_INTERVAL}:g" $TT_CONFIG

mkdir -p log/

for i in $@; do
    test_burn $i
done | tee $LOG

cp $LOG $RESULTS/log
