#!/bin/bash

source ../common_functions.sh
source load_test_config
source test_functions.sh

TT_CONFIG=config/tt_general.config
TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
SLEEP=10

# 1 pc
# 1 backend node
# 1, 2 & 4 riak nodes
function test_1_pc {
    for riak_nodes in 1 2 4 ; do
        echonormal "TEST: test_1_pc"
        echonormal "####: PCs:\t1"
        echonormal "####: Backends:\t1"
        echonormal "####: Riak nodes:\t$riak_nodes"

        stop $TEST_USER@${SERVERS[0]}

        RES_DIR=mult_nodes/$TIMESTAMP/pcs_1-be_1-riak_$riak_nodes/
        #update basho driver
        sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

        IP=`host ${SERVERS[0]} | sed "s:.* ::"`
        # copy backend/riak
        echonormal "Setup on ${SERVERS[0]}"
        copy-mult-riak $TEST_USER ${SERVERS[0]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            $riak_nodes $RIAK_DIR\1@$IP
        copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP 1 ""
        sleep $SLEEP

        # run the test
        run-basho $TT_CONFIG $RES_DIR
        ./backend_stats.escript $B_NAME@${SERVERS[0]}

        stop $TEST_USER@${SERVERS[0]}
    done
}

# 2 pcs
# 1 backend node (1. machine)
# 1, 2 & 4 riak nodes (2. machine)
function test_2_pcs {
    for riak_nodes in 1 2 4; do
        echonormal "TEST: test_2_pcs"
        echonormal "####: PCs:\t2"
        echonormal "####: Backends:\t1"
        echonormal "####: Riak nodes:\t$riak_nodes"

        stop $TEST_USER@${SERVERS[0]}
        stop $TEST_USER@${SERVERS[1]}

        RES_DIR=mult_nodes/$TIMESTAMP/pcs_2-be_1-riak_$riak_nodes/
        #update basho driver
        sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

        IP=`host ${SERVERS[1]} | sed "s:.* ::"`
        # copy backend/riak
        echonormal "Setup on ${SERVERS[1]}"
        copy-mult-riak $TEST_USER ${SERVERS[1]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            $riak_nodes $RIAK_DIR\1@$IP
        echonormal "Setup on ${SERVERS[0]}"
        copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP 1 ""
        sleep $SLEEP

        # run the test
        run-basho $TT_CONFIG $RES_DIR
        ./backend_stats.escript $B_NAME@${SERVERS[0]}

        stop $TEST_USER@${SERVERS[0]}
        stop $TEST_USER@${SERVERS[1]}

    done
}

# 3 pcs
# 1 backend node (1. machine)
# 4 riak nodes (2.,3. machine)
function test_3_pcs_riak_dist {
    echonormal "TEST: test_3_pcs_riak_dist"
    echonormal "####: PCs:\t3"
    echonormal "####: Backends:\t1"
    echonormal "####: Riak nodes:\t4"

    stop-all $TEST_USER ${SERVERS[@]:0:3}

    RES_DIR=mult_nodes/$TIMESTAMP/pcs_3-be_1-riak_4/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    IP=`host ${SERVERS[1]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setup on ${SERVERS[1]}"
    copy-mult-riak $TEST_USER ${SERVERS[1]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            2 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[2]}"
    copy-mult-riak $TEST_USER ${SERVERS[2]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            2 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[0]}"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP 1 ""
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:3}
}

# 5 pcs
# 1 backend node (1. machine)
# 4 riak nodes (2.,3.,4.,5. machine)
function test_5_pcs_riak_dist {
    echonormal "TEST: test_5_pcs_riak_dist"
    echonormal "####: PCs:\t3"
    echonormal "####: Backends:\t1"
    echonormal "####: Riak nodes:\t4"

    stop-all $TEST_USER ${SERVERS[@]:0:5}
    RES_DIR=mult_nodes/$TIMESTAMP/pcs_5-be_1-riak_4/

    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    IP=`host ${SERVERS[1]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setup on ${SERVERS[1]}"
    copy-mult-riak $TEST_USER ${SERVERS[1]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[2]}"
    copy-mult-riak $TEST_USER ${SERVERS[2]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[3]}"
    copy-mult-riak $TEST_USER ${SERVERS[3]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[4]}"
    copy-mult-riak $TEST_USER ${SERVERS[4]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP
    echonormal "Setup on ${SERVERS[0]}"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP 1 ""
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:5}
}

# 3 pcs
# 3 backend nodes (all machines)
# 3 riak nodes (all machines)
function test_3_pcs_backend_dist {
    echonormal "TEST: test_3_pcs_backend_dist"
    echonormal "####: PCs:\t3"
    echonormal "####: Backends:\t3"
    echonormal "####: Riak nodes:\t3"

    stop-all $TEST_USER ${SERVERS[@]:0:3}

    RES_DIR=mult_nodes/$TIMESTAMP/pcs_3-be_3-riak_3/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    CONF_SERVERS="['$B_NAME@${SERVERS[0]}','$B_NAME@${SERVERS[1]}','$B_NAME@${SERVERS[2]}']"
    Cmd="sed -i \"s:{.*backend_nodes.*}:{backend_nodes, $CONF_SERVERS}:g\" $B_DIR/etc/app.config &&"
    IP0=`host ${SERVERS[0]} | sed "s:.* ::"`
    IP1=`host ${SERVERS[1]} | sed "s:.* ::"`
    IP2=`host ${SERVERS[2]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setting up riak nodes"
    copy-mult-riak $TEST_USER ${SERVERS[0]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[1]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[2]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    echonormal "Setting up backend nodes"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP0 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[1]} $LOCAL_B_DIR $B_DIR $IP1 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[2]} $LOCAL_B_DIR $B_DIR $IP2 1 "$Cmd"
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:3}
}

# 7 pcs
# 3 backend nodes (machines 1, 2, 3)
# 4 riak nodes (machines 1, 2, 3, 4)
function test_7_pcs_backend_riak_dist {
    echonormal "TEST: test_7_pcs_backend_riak_dist"
    echonormal "####: PCs:\t7"
    echonormal "####: Backends:\t3"
    echonormal "####: Riak nodes:\t4"

    stop-all $TEST_USER ${SERVERS[@]:0:7}

    RES_DIR=mult_nodes/$TIMESTAMP/pcs_7-be_3-riak_4/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    CONF_SERVERS="['$B_NAME@${SERVERS[0]}','$B_NAME@${SERVERS[1]}','$B_NAME@${SERVERS[2]}']"
    Cmd="sed -i \"s:{.*backend_nodes.*}:{backend_nodes, $CONF_SERVERS}:g\" $B_DIR/etc/app.config &&"
    IP3=`host ${SERVERS[3]} | sed "s:.* ::"`
    IP4=`host ${SERVERS[4]} | sed "s:.* ::"`
    IP5=`host ${SERVERS[5]} | sed "s:.* ::"`
    IP6=`host ${SERVERS[6]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setting up riak nodes"
    copy-mult-riak $TEST_USER ${SERVERS[3]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP3
    copy-mult-riak $TEST_USER ${SERVERS[4]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP3
    copy-mult-riak $TEST_USER ${SERVERS[5]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP3
    copy-mult-riak $TEST_USER ${SERVERS[6]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP3
    echonormal "Setting up backend nodes"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP3 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[1]} $LOCAL_B_DIR $B_DIR $IP4 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[2]} $LOCAL_B_DIR $B_DIR $IP5 1 "$Cmd"
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:7}
}

# 6 pcs
# 3 backend nodes (machines 1, 2, 3)
# 6 riak nodes (two on each machine: 4, 5, 6)
function test_6_pcs_backend_riak_dist {
    echonormal "TEST: test_6_pcs_backend_riak_dist"
    echonormal "####: PCs:\t6"
    echonormal "####: Backends:\t3"
    echonormal "####: Riak nodes:\t6"

    stop-all $TEST_USER ${SERVERS[@]:0:6}

    RES_DIR=mult_nodes/$TIMESTAMP/pcs_6-be_3-riak_6/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    CONF_SERVERS="['$B_NAME@${SERVERS[0]}','$B_NAME@${SERVERS[1]}','$B_NAME@${SERVERS[2]}']"
    Cmd="sed -i \"s:{.*backend_nodes.*}:{backend_nodes, $CONF_SERVERS}:g\" $B_DIR/etc/app.config &&"
    IP3=`host ${SERVERS[3]} | sed "s:.* ::"`
    IP4=`host ${SERVERS[4]} | sed "s:.* ::"`
    IP5=`host ${SERVERS[5]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setting up riak nodes"
    copy-mult-riak $TEST_USER ${SERVERS[3]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            2 $RIAK_DIR\1@$IP3
    copy-mult-riak $TEST_USER ${SERVERS[4]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            2 $RIAK_DIR\1@$IP3
    copy-mult-riak $TEST_USER ${SERVERS[5]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            2 $RIAK_DIR\1@$IP3
    echonormal "Setting up backend nodes"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP3 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[1]} $LOCAL_B_DIR $B_DIR $IP4 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[2]} $LOCAL_B_DIR $B_DIR $IP5 1 "$Cmd"
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:6}
}

# 7 pcs
# 3 backend nodes (machines 1, 2, 3)
# 7 riak nodes (all machines)
function test_7_pcs_riak_dist {
    echonormal "TEST: test_7_pcs_backend_riak_dist"
    echonormal "####: PCs:\t7"
    echonormal "####: Backends:\t3"
    echonormal "####: Riak nodes:\t7"

    stop-all $TEST_USER ${SERVERS[@]:0:7}

    RES_DIR=mult_nodes/$TIMESTAMP/pcs_7-be_3-riak_7/
    #update basho driver
    sed -i "s:{.*tt_node,.*}:{tt_node, '$B_NAME@${SERVERS[0]}'}:g" $TT_CONFIG

    CONF_SERVERS="['$B_NAME@${SERVERS[0]}','$B_NAME@${SERVERS[1]}','$B_NAME@${SERVERS[2]}']"
    Cmd="sed -i \"s:{.*backend_nodes.*}:{backend_nodes, $CONF_SERVERS}:g\" $B_DIR/etc/app.config &&"
    IP0=`host ${SERVERS[0]} | sed "s:.* ::"`
    IP1=`host ${SERVERS[1]} | sed "s:.* ::"`
    IP2=`host ${SERVERS[2]} | sed "s:.* ::"`
    # copy backend/riak
    echonormal "Setting up riak nodes"
    copy-mult-riak $TEST_USER ${SERVERS[0]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[1]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[2]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[3]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[4]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[5]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    copy-mult-riak $TEST_USER ${SERVERS[6]} \
            $LOCAL_RIAK_DIR $RIAK_DIR \
            $LOCAL_SCHEMA_DIR $SCHEMA_DIR \
            1 $RIAK_DIR\1@$IP0
    echonormal "Setting up backend nodes"
    copy-backend $TEST_USER ${SERVERS[0]} $LOCAL_B_DIR $B_DIR $IP0 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[1]} $LOCAL_B_DIR $B_DIR $IP1 1 "$Cmd"
    copy-backend $TEST_USER ${SERVERS[2]} $LOCAL_B_DIR $B_DIR $IP2 1 "$Cmd"
    sleep $SLEEP

        # run the test
    run-basho $TT_CONFIG $RES_DIR
    ./backend_stats.escript $B_NAME@${SERVERS[0]}

    stop-all $TEST_USER ${SERVERS[@]:0:7}
}

mkdir -p log/
test_1_pc  | tee log/mult_nodes_test-$TIMESTAMP.log
test_2_pcs | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_3_pcs_riak_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_5_pcs_riak_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_3_pcs_backend_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_7_pcs_backend_riak_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_6_pcs_backend_riak_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
test_7_pcs_riak_dist | tee -a log/mult_nodes_test-$TIMESTAMP.log
