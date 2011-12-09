#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

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
    LST=$(echo "$Count-1" | bc)
    for i in $(seq 0 $LST); do
        set-bucket-n_vals ${SERVERS[$i]} $Count
    done

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

mkdir -p log/

update-basho-config ${SERVERS[0]}
test_equal_dist $1 | tee $LOG

cp $LOG $RESULTS/log
