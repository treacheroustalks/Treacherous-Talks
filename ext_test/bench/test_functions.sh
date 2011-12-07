#!/bin/bash

source ../common_functions.sh
source load_test_config

function run-basho {
    # run the test
    echonormal "Running basho bench"
    ./basho_bench $1 

    # move the results
    echonormal "Moving results to $RES_DIR/"
    mkdir -p $2
    cp tests/current/* $2/
    priv/summary.r -i $2/
}


function stop {
    ssh $TEST_USER@$1 "killall -9 beam.smp" 2> /dev/null
}

function stop-all {
    for pc in $@; do
        stop $pc
    done
}

function start-local-sys-manager {
    SYS_DIR=$LOCAL_RELEASE/tt
    nodename=$SYS_DIR/etc/nodename.system_manager
    BIN=$SYS_DIR/bin/system_manager
    HOST=`hostname`

    $BIN stop > /dev/null
    sed -i "s:-name.*:-name system_manager@$HOST:g" $nodename &&
    $BIN start &&
    sleep $START_SLEEP &&
    $BIN ping
}

function copy-release {
    remote=$TEST_USER@$1
    SYS_DIR=$REMOTE_RELEASE/tt
    nodename=$SYS_DIR/etc/nodename.system_manager
    BIN=$SYS_DIR/bin/system_manager

    ssh -t $remote "
 echo \"Stopping system manager on $1\" ;
 $BIN stop > /dev/null;
 rm -rf $REMOTE_RELEASE"
    echo "Copying release to $1:$REMOTE_RELEASE"
    scp -r $LOCAL_RELEASE $remote:$REMOTE_RELEASE/ > /dev/null 
    ssh -t $remote "
 sed -i \"s:-name.*:-name $SYS_MGR_NAME@$1:g\" $nodename &&
 sed -i \"s:PIPE_DIR=.*:PIPE_DIR=/tmp/\\\$USER/\\\$RUNNER_BASE_DIR/:g\" $REMOTE_RELEASE/riak/bin/riak &&
 echo \"Starting system manager\" &&
 $BIN start &&
 sleep $START_SLEEP &&
 $BIN ping"
}


function start-remote-sys-managers {
    for server in $@; do
        run_script "Starting system manager on $server" "copy-release $server"
    done
}


function create-cluster-config {
    rm -f $CLUSTER_CONFIG > /dev/null
    echo "%% -*- erlang -*-" > $CLUSTER_CONFIG
    echo "[" >> $CLUSTER_CONFIG
    for server in $@; do
        ip=`echo $(host $server) | sed "s:.*address ::g"`
        echo " {host, \"$server\", \"$SYS_MGR_NAME\"," >> $CLUSTER_CONFIG
        echo "  [" >> $CLUSTER_CONFIG
        echo "   {release, riak, $RIAK_NAME," >> $CLUSTER_CONFIG
        echo "    [" >> $CLUSTER_CONFIG
        echo "     {riak_core," >> $CLUSTER_CONFIG
        echo "      [" >> $CLUSTER_CONFIG
        echo "       {http,[{\"$ip\", $RIAK_HTTP_PORT}]}," >> $CLUSTER_CONFIG
        echo "       {handoff_port, $RIAK_HANDSHAKE_PORT}" >> $CLUSTER_CONFIG
        echo "      ]" >> $CLUSTER_CONFIG
        echo "     }," >> $CLUSTER_CONFIG
        echo "     {riak_search,[{enabled,true}]}," >> $CLUSTER_CONFIG
        echo "     {riak_kv," >> $CLUSTER_CONFIG
        echo "      [" >> $CLUSTER_CONFIG
        echo "       {storage_backend, $RIAK_BACKEND}," >> $CLUSTER_CONFIG
        echo "       {pb_ip,\"$ip\"}," >> $CLUSTER_CONFIG
        echo "       {pb_port, $RIAK_PB_PORT}" >> $CLUSTER_CONFIG
        echo "      ]" >> $CLUSTER_CONFIG
        echo "     }" >> $CLUSTER_CONFIG
        echo "    ]" >> $CLUSTER_CONFIG
        echo "   }," >> $CLUSTER_CONFIG
        echo "   {release, backend, $B_NAME," >> $CLUSTER_CONFIG
        echo "    [" >> $CLUSTER_CONFIG
        echo "     {db," >> $CLUSTER_CONFIG
        echo "      [" >> $CLUSTER_CONFIG
        echo "       {riak_ip,\"$ip\"}," >> $CLUSTER_CONFIG
        echo "       {riak_database_port, $RIAK_HTTP_PORT}," >> $CLUSTER_CONFIG
        echo "       {riak_protobuf_port, $RIAK_PB_PORT}," >> $CLUSTER_CONFIG
        echo "       {db_workers, $DB_WORKER}" >> $CLUSTER_CONFIG
        echo "      ]" >> $CLUSTER_CONFIG
        echo "     }," >> $CLUSTER_CONFIG
        echo "     {controller_app," >> $CLUSTER_CONFIG
        echo "      [{controller_app_workers, $CONTROLLER_WORKER}]" >> $CLUSTER_CONFIG
        echo "     }," >> $CLUSTER_CONFIG
        echo "     {game," >> $CLUSTER_CONFIG
        echo "       [{game_workers, $GAME_WORKER}]" >> $CLUSTER_CONFIG
        echo "     }," >> $CLUSTER_CONFIG
        echo "     {message," >> $CLUSTER_CONFIG
        echo "       [{message_workers, $MESSAGE_WORKER}]" >> $CLUSTER_CONFIG
        echo "     }" >> $CLUSTER_CONFIG
        echo "    ]" >> $CLUSTER_CONFIG
        echo "   }" >> $CLUSTER_CONFIG
        echo "  ]" >> $CLUSTER_CONFIG
        if [ $server == ${@:$#} ] ; then
            echo " }" >> $CLUSTER_CONFIG
        else
            echo " }," >> $CLUSTER_CONFIG
        fi
    done
    echo "]." >> $CLUSTER_CONFIG
}

function start-cluster {
    BIN=$LOCAL_RELEASE/tt/bin/cluster_manager
    $BIN -c -s -j $CLUSTER_CONFIG &&
    sleep $START_SLEEP &&
    $BIN -p $CLUSTER_CONFIG
}


function setup-and-start-cluster {
    run_script "Starting local system manager" "start-local-sys-manager"

    echonormal "Starting remote system managers"
    start-remote-sys-managers "$@"

    create-cluster-config "$@"
    run_script "Starting up the cluster" "start-cluster"

    sleep $START_SLEEP
}