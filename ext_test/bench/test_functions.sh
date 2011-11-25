#!/bin/bash

source ../common_functions.sh
source load_test_config

function copy-backend {
    remote=$1@$2
    from_dir=$3
    to_dir=$4
    riak_ip=$5
    riak_port=900$6
    riak_http=910$6
    cmd=$7
    ssh -t $remote "
 echo \"Stopping backend on $2\" ;
 $to_dir/bin/backend stop ;
 rm -rf $to_dir"
    echo "Copying backend to $2:$to_dir"
    scp -r $from_dir $remote:$to_dir/ > /dev/null 
    ssh -t $remote "
 sed -i \"s:-name.*:-name $B_NAME@$2:g\" $to_dir/etc/nodename &&
 sed -i \"s:{riak_ip.*}:{riak_ip, \\\"$IP\\\"}:g\" $to_dir/etc/app.config &&
 sed -i \"s:{riak_protobuf_port.*}:{riak_protobuf_port, $riak_port}:g\" $to_dir/etc/app.config &&
 sed -i \"s:{riak_database_port.*}:{riak_database_port, $riak_http}:g\" $to_dir/etc/app.config &&
 echo \"Changing backend config\" &&
 $cmd
 echo \"Starting backend\" &&
 $to_dir/bin/backend start &&
 sleep 5 &&
 $to_dir/bin/backend ping;
 echo \"\""
}

function copy-riak {
    remote=$1@$2
    from_dir=$3
    to_dir=$4
    from_schema=$5
    to_schema=$6
    pb_port=900$7
    http_port=910$7
    hand_port=920$7
    IP=`host $2 | sed "s:.* ::"`
    ssh -t $remote "
 echo \"Stopping riak\" ;
 $to_dir/bin/riak stop ;
 rm -rf $to_dir"
    echo "Copying riak to $2:$to_dir"
    scp -r $from_dir $remote:$to_dir/ > /dev/null 
    scp -r $from_schema $remote:$to_schema/ > /dev/null 
    ssh -t $remote "
 sed -i \"s:-name.*:-name $to_dir@$IP:g\" $to_dir/etc/vm.args &&
 sed -i \"s:{pb_ip.*}:{pb_ip, \\\"$IP\\\"}:g\" $to_dir/etc/app.config &&
 sed -i \"s:{pb_port,.*}:{pb_port, $pb_port}:g\" $to_dir/etc/app.config &&
 sed -i \"s:{http,.*}:{http, [{\\\"$IP\\\", $http_port}]}:g\" $to_dir/etc/app.config &&
 sed -i \"s:{handoff_port,.*}:{handoff_port, $hand_port}:g\" $to_dir/etc/app.config &&
 echo \"Starting riak\" &&
 $to_dir/bin/riak start &&
 $to_dir/bin/riak ping &&
 echo \"Waiting for riak\" &&
 sleep 5 &&
 $to_schema/install_schema.sh $to_dir"
}


function copy-mult-riak {
    IP=`host $2 | sed "s:.* ::"`
    for i in `seq 1 $7`; do
        node_dir=$4$i
        echonormal "Setting up riak node $i/$7"
        copy-riak $1 $2 $3 $node_dir $5 $6 $i
        if [ "$4$i@$IP" != "$8" ] ; then
            ssh $1@$2 "$node_dir/bin/riak-admin join $8"
        fi
    done
    sleep 10
    ssh $1@$2 "$4$7/bin/riak-admin member_status"
}

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
    ssh $1 "killall -9 beam.smp" 2> /dev/null
}

function stop-all {
    for pc in ${@:2}; do
        stop $1@$pc
    done
}