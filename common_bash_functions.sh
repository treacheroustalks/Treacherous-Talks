#!/bin/bash
# Common definitions and functions for Treacherous Talks is stored here.

# Setup the shell to report errors even when piping commands. This allows sed
# to operate on output while preserving the exit code from the previous
# command. Warning! This is a bash specific feature.
set -o pipefail

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

# Description: Runs a command and prints message depending on exit code. All
#              output from the command is streamed through sed to replace
#              warnings. This is intended to use for building dependencies.
# First argument: printed name
# Second argument: command to run
# Side effect: will set $errors to one if non-zero exit code
function build_dep () {
    local_error=0
    # Run the script given in $1
    echonormal "$1"
    $2 | sed s/Warning/ignore-warning/ || local_error=1
    case "$local_error" in
        0) echonormal "Exit status: success\n" ;;
        *) echoerr "Exit status: ERROR\n"
           errors=1 ;;
    esac
}

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

# Description: Prints a helpful exit message
# First argument: printed name
# Second argument: exit/error code
function print_exit_status () {
    if (( $2 )); then
        echoerr "$1 was NOT successfull."
    else
        echonormal "$1 was successfull."
    fi
}

# Description: Starts a release and pings it to make sure that it is running
# First argument: path to system-release directory
# Second argument: name of release to start
# Side effect: will set $errors to one if non-zero exit code
function start_release () {
    command=$1/$2/bin/$2
    local_error=0
    # Start the release given in $1
    echonormal "Starting $2"
    $command start || local_error=1
    # Wait before pinging
    sleep 1
    # Do five ping attempts and fail if all is unsuccessfull
    running=0
    for i in {1..5}
    do
        echonormal "Ping attempt number $i for $2"
        # Run command and pipe output to find regexp
        $command ping 2>&1 | perl -pe 'END { print; exit $status }
                                       $status=100 if
                                       /.*not responding to pings/;'
        case "$?" in
            0) running=1
               break ;;
        esac
        # Sleep before next attempt
        sleep 1
    done
    # Check if ping was unsuccessfull and set errors accordingly
    if [ $running -eq 0 ]; then
        local_error=1
    fi
    case "$local_error" in
        0) echonormal "$2 is up and running\n" ;;
        *) echoerr "ERROR: Could not start $2\n"
           errors=1 ;;
    esac
}

# Description: Stops a release in a proper way
# First argument: path to system-release directory
# Second argument: name of release to stop
# Side effect: will set $errors to one if non-zero exit code
function stop_release () {
    command=$1/$2/bin/$2
    local_error=0
    # Stop the release given in $1
    echonormal "Stopping $2"
    $command stop || local_error=1
    case "$local_error" in
        0) echonormal "$2 is stopped\n" ;;
        *) echoerr "ERROR: Could not stop $2\n"
           errors=1 ;;
    esac
}
