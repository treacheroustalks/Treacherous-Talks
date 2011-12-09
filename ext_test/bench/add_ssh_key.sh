#!/bin/bash

source ../common_functions.sh
source load_test_config

if [ ! -f ~/.ssh/id_rsa.pub ] ; then
    echonormal "You have no public ssh key, generating one"
    echonormal "Just press enter for everything"
    ssh-keygen -t rsa
fi

echonormal "Adding your ssh key to the load_test users on all machines"
echonormal "Password: projcs"
for server in ${SERVERS[@]}; do
    cat ~/.ssh/id_rsa.pub | ssh load_test@$server 'cat >> .ssh/authorized_keys'
done