#!/bin/ksh

# Called from da_minimisation inside WRFVAR. The script is only temporary 
# until we can couple the models through memory rather than files

# VAR produces tl00 used as input wrfinput_d${DOMAIN}

# WRF produces 3 timestep files for T+1 to T+3 hardwired to
# 2000-01-25 by the namelist file, renamed to tl01 to tl03 for
# use by VAR

set -x


arg1=$1
arg2=$2

mkdir tl
cd tl

if test $arg1=pre && test $arg2=monitor; then
   cp ../namelist.var4dtl namelist.input
   cp tl00 wrfinput_d${DOMAIN}
fi

if test $NUM_PROCS=1; then
   ../wrfplus.exe > wrf_tl.out 2>wrf_tl.error
fi

if test $arg1=post && test $arg2=monitor; then
   for HOUR in 01 02 03 04 05 06; do
      export CCYY=`echo ${FG_DATE[$HOUR]} | cut -c1-4`
      export MM=`echo ${FG_DATE[$HOUR]} | cut -c5-6`
      export DD=`echo ${FG_DATE[$HOUR]} | cut -c7-8`
      export HH=`echo ${FG_DATE[$HOUR]} | cut -c9-10`
      cp tl${CCYY}_${MM}_${DD}_${HH}:00.00 tl$HOUR
   done

   if NL_JCDFI_USE; then
      export CCYY=`echo $END_DATE | cut -c1-4`
      export MM=`echo $END_DATE | cut -c5-6`
      export DD=`echo $END_DATE | cut -c7-8`
      export HH=`echo $END_DATE | cut -c9-10`
      cp auxhist3_d${DOMAIN}_${CCYY}_${MM}_${DD}_${HH}:00.00 tldf
   fi
fi 
