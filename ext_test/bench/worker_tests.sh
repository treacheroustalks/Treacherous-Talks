#!/bin/bash

source ../common_functions.sh

source load_test_config
source test_functions.sh
declare WORKER=""
declare DRIVER="tt_user"
declare WORKER_COUNT="1 10 50"
VALID_WORKER="db_workers message_workers controller_app_workers game_workers"

declare -r TRUE=0
declare -r FALSE=1

###
# Print Usage 
###
function printUsage {
    echo -e "Wrong arguments: $@\n"
    echo -e "Usage: $0 Worker Driver Worker-Count\n" 
    echo -e "\tExample: $0 db_workers tt_user \"1 10 50\"\n"
    echo -e "\tWorker:\t\t `echo $VALID_WORKER | tr ' ' ','`,all "
    echo -e "\tDriver:\t the basho bench driver"
    echo -e "\tWorker-Count:\t List of values to test for that worker, like \"1 10 50\"\n"
}

###
# Check commandline arguments
###
function checkArguments {
    if [ $# -gt 2 ] ; then
        DRIVER=$2
        WORKER_COUNT="${@:3}"
        for w in $VALID_WORKER ; do
            if [ "$w" == "$1" ] || [ "all" == "$1" ] ; then
                WORKER=$1
                return $TRUE
            fi
        done
        printUsage $@
        echoerr "\nInvalid worker: $1"
        return $FALSE
    else
        printUsage $@
        return $FALSE
    fi
}

###
# Main part
###
function main {
    echonormal "Config:"
    echo "User: $TEST_USER"
    echo "Server: $TEST_SERVER"
    echo "Backend dir: $B_DIR"
    echo "Riak dir: $RIAK_DIR"
    echo "Schema dir: $SCHEMA_DIR"
    echo "Worker: $WORKER"
    echo "Basho driver: $DRIVER"
    echo ""

    TT_CONFIG=config/$DRIVER.config
    echonormal "Updating tt_node in $TT_CONFIG"
    # change local config
    sed -i "s:{.*tt_node,.*}:{tt_node, 'backend@$TEST_SERVER'}:g" $TT_CONFIG

    CONFIG="etc/app.config"
    TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
    W_DIR=worker_tests/$DRIVER-$TIMESTAMP/$WORKER
    IP=`host $TEST_SERVER | sed "s:.* ::"`

    echonormal "Starting worker load test"
    for c in $WORKER_COUNT; do
        echonormal "Worker count: $c"
        # change worker config on server
        UPDATE_CMD=""
        if [ "all" == "$WORKER" ] ; then
            for w in $VALID_WORKER ; do 
                UPDATE_CMD="$UPDATE_CMD
                             sed -i \"s:{.*$w,.*}:{$w, $c}:g\" $B_DIR/$CONFIG &&"
            done
        else
            UPDATE_CMD="sed -i \"s:{.*$WORKER,.*}:{$WORKER, $c}:g\" $B_DIR/$CONFIG &&"
        fi
        copy-riak $TEST_USER $TEST_SERVER \
            $LOCAL_RIAK_DIR $RIAK_DIR\1 \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1
        copy-backend $TEST_USER $TEST_SERVER $LOCAL_B_DIR $B_DIR $IP 1 "$UPDATE_CMD"
        sleep 2

        # run the test
        echonormal "Running basho bench"
        ./basho_bench $TT_CONFIG

        # move the results
        echonormal "Moving results to $W_DIR/$c/" &&
        mkdir -p $W_DIR/$c &&
        cp tests/current/* $W_DIR/$c/ &&
        priv/summary.r -i $W_DIR/$c/ &&
        xdg-open $W_DIR/$c/summary.png
    done
}

checkArguments $@  && main || exit 1
