#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

TT_CONFIG=config/tt_general.config
CONCURRENT=1
DURATION=60 # in minutes
REPORT_INTERVAL=60 # in seconds

TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
LOG=log/equal_dist-$TIMESTAMP.log
RESULTS=equal_dist/$TIMESTAMP
SLEEP=5

# X pcs
# X backend nodes (all machines)
# X riak nodes (all machines)
# on every machine 1 backend & 1 riak
function test_equal_dist {
    Count=$1
    echonormal "TEST: test_equal_dist $Count"
    echonormal "####: PCs:\t$Count"
    echonormal "####: Backends:\t$Count"
    echonormal "####: Riak nodes:\t$Count"

    servers="${SERVERS[@]:0:$Count}"
    stop-all $TEST_USER ${SERVERS[@]:0:$Count}

    RES_DIR=$RESULTS/equal_dist_$Count/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG
    CONF_SERVERS=""
    for server in $servers ; do
        CONF_SERVERS="$CONF_SERVERS '$B_NAME@$server'"
    done
    CONF_SERVERS="[`echo \"$CONF_SERVERS\" | sed \"s:^ ::g\" | tr ' ' ','`]"
    echo $CONF_SERVERS
    Cmd="sed -i \"s:{.*backend_nodes.*}:{backend_nodes, $CONF_SERVERS}:g\" $B_DIR/etc/app.config &&"

    # setup riak on all machines
    IP0=`host ${SERVERS[0]} | sed "s:.* ::"`
    for server in $servers ; do
        IP=`host $server | sed "s:.* ::"`
        echonormal "Setting up riak nodes on machine $server"
        copy-mult-riak $TEST_USER $server \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    done
    # setup all backends
    for server in $servers ; do
        IP=`host $server | sed "s:.* ::"`
        echonormal "Setting up backend node on $server"
        copy-backend $TEST_USER $server $LOCAL_B_DIR $B_DIR $IP 1 "$Cmd"
    done
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER $servers
}

if [ $# -ne 1 ] ; then
    echo "You need to specify the amount of machines to use"
    exit 0
fi

sed -i "s:{.*concurrent,.*}:{concurrent, $CONCURRENT}:g" $TT_CONFIG
sed -i "s:{.*duration,.*}:{duration, $DURATION}:g" $TT_CONFIG
sed -i "s:{.*report_interval,.*}:{report_interval, $REPORT_INTERVAL}:g" $TT_CONFIG

mkdir -p log/
test_equal_dist $1 | tee $LOG

cp $LOG $RESULTS/log
