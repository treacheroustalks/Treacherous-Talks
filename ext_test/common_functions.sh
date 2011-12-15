#!/bin/bash
# This script contains common funtionality for test scripts

# Internal variables
cwd="$( cd -P "$( dirname "$0" )" && pwd )"
sysrel=$cwd/../system-release
cm=$sysrel/tt/bin/cluster_manager
sm=$sysrel/tt/bin/system_manager

# Set some color definitions, but only if we're running in a real terminal
if [ -t 1 ]; then
    c_red="\033[31m"
    c_brown="\033[33m"
    c_reset="\033[0m";
else
    c_red=""
    c_brown=""
    c_reset="";
fi

# Description: Echoes text in red color with ##### signs in front to STDERR
# First argument: text to echo
function echoerr() { echo -e $c_red##### "$@" 1>&2 $c_reset; }

# Description: Echoes text in brown color with ##### signs in front to STDOUT
# First argument: text to echo
function echonormal() { echo -e $c_brown##### "$@" $c_reset; }

# Description: Runs a command and prints message depending on exit code
# First argument: printed name
# Second argument: command to run
# Side effect: will set $errors to one if non-zero exit code
function run_script () {
    local_error=0
    # Run the script given in $1
    echonormal "$1"
    ${@:2} || local_error=1
    case "$local_error" in
        0)  echonormal "Exit status: success\n" ;;
        *)  echoerr "Exit status: ERROR\n"
            errors=1 ;;
    esac
    return $local_error
}

# Description: Starts a system using system_manager and cluster_manager
# Argument: the config file to use
function start_system () {
    local_error=0
    cd $cwd
    $sm start
    # Check if start failed. In that case see if stopping and starting again
    # will help. Also stop any old releases when the new system manager is up
    # and running, but first add the new config so that we can find any old
    # ones.
    if [ "$?" -ne 0 ]; then
        echo Stopping old system and starting it properly again...
        $sm stop
        $sm start
        $cm --setconfig --stop --parallel "$1"
    fi
    $cm --setconfig --start --parallel "$1" || local_error=1
    return $local_error
}

# Description: Stops a system using system_manager and cluster_manager
# Argument: the config file to use
function stop_system () {
    local_error=0
    cd $cwd
    $cm --setconfig --stop --parallel "$1"
    if [ "$?" -ne 0 ]; then
        echo Restarting system manager so that the system can be stopped...
        $sm stop
        $sm start
        $cm --setconfig --stop --parallel "$1" || local_error=1
    fi
    $sm stop || local_error=1
    return $local_error
}

# If the script is called interactivly, call the function passed as the first
# parameter to the script. The remaining script arguments can be passed to this
# function.
[[ "${BASH_SOURCE[0]}" = "${0}" ]] && $1 $2 $3 $4 $5
