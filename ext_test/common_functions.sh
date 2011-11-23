#!/bin/bash
# This script contains common funtionality for test scripts

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
    $2 || local_error=1
    case "$local_error" in
        0)  echonormal "Exit status: success\n" ;;
        *)  echoerr "Exit status: ERROR\n"
            errors=1 ;;
    esac
}

