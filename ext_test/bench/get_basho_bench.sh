#!/bin/bash

DIR=basho_git
VERSION="156f39052339043eee911af8446c283d45a931ba"
COOKIE="treacherous_talks"

source ../common_functions.sh

function get_bench {
    # get basho bench from github
    git clone https://github.com/basho/basho_bench.git $DIR &&
    cd $DIR &&
    git checkout $VERSION &&
    cd .. &&
    # move files to the correct place
    rm -rf ebin/
    mv src/* $DIR/src/ &&
    mv -f $DIR/* .
    rm -rf $DIR
}

function update_config {
    # update rebar config
    HOST=`hostname`
    echonormal "Using $HOST as hostname"
    ESCRIPT_ARGS="{escript_emu_args, \"%%! -name bb@$HOST -setcookie $COOKIE\\\\n\"}."
    sed -i "s:.*escript_emu_args.*:$ESCRIPT_ARGS:g" rebar.config
    sed -i "s:{deps, \[:{sub_dirs, \[\"../../apps/gen_moves\",\n            \"../../apps/load_test\"\]}.\n{deps,\[:g" rebar.config
}


if [ ! -e basho_bench ] ; then
    run_script "Can't find basho_bench, downloading it" "get_bench"
fi

run_script "Updating rebar.config" "update_config"