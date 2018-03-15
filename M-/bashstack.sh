#!/bin/bash
#  Author - Brett Giles
# Descritpion:  This script creates a temporary file containing the stack.csh
#                       aliases and the contents of file named on the command line. It then executes that file
#                       with the CLI csh.
# Dependancies: stack.csh must be in the same directory
# 
# Usage:   bashstack.sh filewithgeneratedcode


cat stack.csh $1 >/tmp/run$$
csh /tmp/run$$
rm /tmp/run$$
