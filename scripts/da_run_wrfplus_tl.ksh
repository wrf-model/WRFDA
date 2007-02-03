#!/bin/ksh

# Called from da_minimisation inside WRFVAR. The script is only temporary 
# until we can couple the models through memory rather than files

# VAR produces tl00 used as input wrfinput_d${DOMAIN}

# WRF produces 3 timestep files for T+1 to T+3 hardwired to
# 2000-01-25 by the namelist file, renamed to tl01 to tl03 for
# use by VAR

#set -x

arg1=$1
arg2=$2

cd $WORK_DIR/tl

if test $NUM_PROCS=1; then
   ./wrfplus.exe > wrf_tl.out 2>wrf_tl.error
else
   $RUN_CMD ./wrfplus.exe > wrf_tl.out 2>wrf_tl.error
fi
