#!/bin/bash

source ../common_functions.sh

ALL_WORKERS="tt_user 
             tt_message_online
             tt_message_offline
             tt_search
             tt_game_join
             tt_game_message
             tt_game_play"

ALL_VALUE=100

VARYING_WORKERS=(
    "all tt_general"
    "db_workers tt_general"
    "game_workers tt_general"
    "controller_app_workers tt_general"
    "message_workers tt_general"
)

VARYING_VALUES="10 50 200 500 100"

TIMESTAMP=`date +"%y.%m.%d_%H-%M"`
mkdir -p log

# Fixed workers tests
for driver in $ALL_WORKERS; do
    echonormal "$driver"
    ./worker_tests.sh all $driver $ALL_VALUE
done | tee log/worker_tests-$TIMESTAMP.log

# Varying workers tests
for i in `seq 0 $((${#VARYING_WORKERS[@]}-1))`; do
    echonormal "setup: ${VARYING_WORKERS[$i]}"
    ./worker_tests.sh ${VARYING_WORKERS[$i]} $VARYING_VALUES
done | tee -a log/worker_tests-$TIMESTAMP.log