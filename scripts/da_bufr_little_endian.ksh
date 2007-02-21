#!/bin/ksh

# Argument is input big endian BUFR file
# Output to little endian BUFR, for PC Linux etc

../build/da_bufr_little_endian.exe $1 ${1}.little_endian

