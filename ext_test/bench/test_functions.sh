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

function copy-release {
    tar_file=$1
    host=$2
    remote=$TEST_USER@$host
    SYS_DIR=system-release/tt
    nodename=$SYS_DIR/etc/nodename.system_manager
    BIN=$SYS_DIR/bin/system_manager

    ssh $remote "
 echo -e \"[$host]\tStopping system manager\" ;
 $BIN stop &> /dev/null;
 rm -rf system-release > /dev/null" &&
    echo -e "[$host]\tCopying release $tar_file"
    scp $tar_file $remote:. > /dev/null &&
    ssh $remote "
 tar xfz $tar_file &>/dev/null;
 rm $tar_file
 sed -i \"s:-name.*:-name $SYS_MGR_NAME@$host:g\" $nodename &&
 sed -i \"s:PIPE_DIR=.*:PIPE_DIR=/tmp/\\\$USER/\\\$RUNNER_BASE_DIR/:g\" system-release/riak/bin/riak &&
 echo -e \"[$host]\tStarting system manager\" &&
 $BIN start &&
 sleep $START_SLEEP &&
 echo -e \"[$host]\tPinging system manager: \`$BIN ping\`\""
}


function start-remote-sys-managers {
    tar_file=sys_mgr.tar.gz
    sys_dir=`dirname $LOCAL_RELEASE`
    tar_cmd="tar czf $tar_file `basename $LOCAL_RELEASE`"

    echo "$tar_cmd"
    rm -f $LOCAL_RELEASE/riak/data/ring/* &> /dev/null
    rm -rf $LOCAL_RELEASE/riak/log &> /dev/null
    rm -rf $LOCAL_RELEASE/tt/log &> /dev/null
    cd $sys_dir
    $tar_cmd &>/dev/null
    cd - &>/dev/null
    mv $sys_dir/$tar_file $tar_file

    for server in $@; do
        copy-release $tar_file $server &
    done
    ret_val=0
    for job in `jobs -p`; do
        wait $job || ret_val=1
    done
    rm -f $tar_file &> /dev/null
    return $ret_val
}


function create-cluster-config {
    echo 
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
    $BIN -f -c -s $CLUSTER_CONFIG &&
    $BIN -f -j $CLUSTER_CONFIG &&
    sleep $START_SLEEP &&
    $BIN -f -p $CLUSTER_CONFIG
}


function setup-and-start-cluster {
    run_script "Starting remote system managers" "start-remote-sys-managers $@" &&

    create-cluster-config "$@" &&
    run_script "Starting up the cluster" "start-cluster" &&

    sleep $START_SLEEP
}

function set-bucket-n_val {
    RIAK=$1
    BUCKET=$2
    N_VAL=$3
    
    echo -e "$RIAK:9101/riak/$BUCKET:\tn_val -> $N_VAL"
    curl -X PUT -H "Content-Type: application/json" -d "{\"props\":{\"n_val\":$N_VAL}}" $RIAK:9101/riak/$BUCKET
}

function set-bucket-n_vals {
    echo "setting bucket n_vals"
    RIAK=$1

    LOW=2
    NUM_OF_NODES=$LOW

    echo "LOW=$LOW, NUM_OF_NODES=$NUM_OF_NODES"

    if [ $LOW -gt $NUM_OF_NODES ]; then
        LOW=$NUM_OF_NODES
    fi

    set-bucket-n_val $RIAK "corpses" "$LOW"
    set-bucket-n_val $RIAK "game" "$NUM_OF_NODES"
    set-bucket-n_val $RIAK "game_current" "$NUM_OF_NODES"
    set-bucket-n_val $RIAK "game_order" "$LOW"
    set-bucket-n_val $RIAK "message" "$LOW"
    set-bucket-n_val $RIAK "game_message" "$LOW"
    set-bucket-n_val $RIAK "game_player" "$NUM_OF_NODES"
    set-bucket-n_val $RIAK "game_state" "$NUM_OF_NODES"
}


function set-all-bucket-props {
    for server in $@; do
        set-bucket-n_vals $server $#
    done
}

function update-basho-config {
    sed -i "s:{.*mode,.*}:{mode, $MODE}:g" $TT_CONFIG
    sed -i "s:{.*concurrent,.*}:{concurrent, $CONCURRENT}:g" $TT_CONFIG
    sed -i "s:{.*duration,.*}:{duration, $DURATION}:g" $TT_CONFIG
    sed -i "s:{.*report_interval,.*}:{report_interval, $REPORT_INTERVAL}:g" $TT_CONFIG
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@$1'}:g" $TT_CONFIG

}


function setup-and-benchmark {
    res_dir=$1
    server=$2
    servers=${@:2}

    update-basho-config $server
    stop-all $servers 

    ret_val=1
    setup-and-start-cluster $servers && ret_val=0

    # run the test
    if [ $ret_val -eq 0 ] ; then
        run-basho $TT_CONFIG $res_dir
        ./backend_stats.escript $B_NAME@$server
    fi

    stop-all $servers
    return $ret_val
}