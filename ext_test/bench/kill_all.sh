#!/bin/bash

source load_test_config
source test_functions.sh

stop-all $TEST_USER ${SERVERS[@]}

