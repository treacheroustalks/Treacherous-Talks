#!/bin/bash

source ../common_functions.sh

source worker_tests_config.sh
declare WORKER=""
declare TT_CONFIG="config/tt_user.config"
declare WORKER_COUNT="1 10 50"
VALID_WORKER="db_workers message_workers controller_app_workers game_workers"

declare -r TRUE=0
declare -r FALSE=1

###
# Print Usage 
###
function printUsage {
    echo -e "Usage: $0 Worker TTConfig Worker-Count\n" 
    echo -e "\tExample: $0 db_workers config/tt_user.config \"1 10 50\"\n"
    echo -e "\tWorker:\t\t `echo $VALID_WORKER | tr ' ' ','` "
    echo -e "\tTTConfig:\t the basho bench config"
    echo -e "\tWorker-Count:\t List of values to test for that worker, like \"1 10 50\""
}

###
# Check commandline arguments
###
function checkArguments {
    if [ $# -gt 2 ] ; then
        TT_CONFIG=$2
        WORKER_COUNT="${@:3}"
        for w in $VALID_WORKER ; do
            if [ "$w" == "$1" ] ; then
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
    echo "User: $SERVER_USER"
    echo "Server: $SERVER"
    echo "Backend dir: $B_DIR"
    echo "Riak dir: $RIAK_DIR"
    echo "Schema dir: $SCHEMA_DIR"
    echo "Worker: $WORKER"
    echo "Basho Config: $TT_CONFIG"
    echo ""

    echonormal "Updating tt_node in $TT_CONFIG"
    W_DIR=worker_tests/$WORKER
    # change local config
    sed -i "s:{.*tt_node,.*}:{tt_node, 'backend@$SERVER'}:g" $TT_CONFIG

    echonormal "Starting worker load test"
    for c in $WORKER_COUNT; do
        echonormal "Worker count: $c"
        # change worker config on server
        CONFIG="etc/app.config"
        CMD="
 echo \"Stopping backend\" ;
 $B_DIR/bin/backend stop;

 echo \"Stopping riak\" ;
 $RIAK_DIR/bin/riak stop;
 echo \"Emptying riak\" ;
 rm -rf $RIAK_DIR/data/leveldb &&
 echo \"Starting riak\" ;
 $RIAK_DIR/bin/riak start &&
 echo \"Reinstalling search schema\" ;
 $SCHEMA_DIR/install_schema.sh $RIAK_DIR &&

 echo \"Changing backend config\" ;
 sed -i \"s:{.*$WORKER,.*}:{$WORKER, $c}:g\" $B_DIR/$CONFIG &&
 echo \"Starting backend\" &&
 $B_DIR/bin/backend start"
        ssh -t $SERVER_USER@$SERVER $CMD

        # sleep - backend needs to start
        echonormal "Waiting for the backend"
        sleep 10

        # run the test
        echonormal "Running basho bench"
        ./basho_bench $TT_CONFIG

        # move the results
        echonormal "Moving results to $W_DIR/$c/"
        mkdir -p $W_DIR/$c
        cp tests/current/* $W_DIR/$c/
    done
}

checkArguments $@  && main || exit 1
