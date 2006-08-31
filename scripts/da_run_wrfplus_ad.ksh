#!/bin/ksh

# Called from da_transform_vtoy_adj inside WRFVAR. The script is only 
# temporary until we can couple the models through memory rather than files
#
# VAR provides files af00 to af06
#
# WRF produces gradient file wrfvar_input_d${DOMAIN}_2000-01-25_00:00:00 which
# is renamed gr00 for VAR

arg1=$1
arg2=$2

mkdir ad
cd ad

cp ../namelist.var4dad namelist.input

cp ../wrfvar_input wrfinput_d${DOMAIN}

# rm tl0* 

for HOUR in 01 02 03 04 05 06; do
   export CCYY=`echo ${FG_DATE[$HOUR]} | cut -c1-4`
   export MM=`echo ${FG_DATE[$HOUR]} | cut -c5-6`
   export DD=`echo ${FG_DATE[$HOUR]} | cut -c7-8`
   export HH=`echo ${FG_DATE[$HOUR]} | cut -c9-10`
   cp ../af$HOUR auxinput3_d${DOMAIN}_${CCYY}_${MM}_${DD}_${HH}:00.00
done

if NL_JCDFI_USE; then
   cp ../afdf auxinput3_d${DOMAIN}_dfi
fi

if test $NUM_PROCS=1; then
   ../wrfplus.exe > wrf_ad.out 2>wrf_ad.error
fi

if test $arg1=post && test $arg2=monitor; then
   export CCYY=`echo $DATE | cut -c1-4`
   export MM=`echo $DATE | cut -c5-6`
   export DD=`echo $DATE | cut -c7-8`
   export HH=`echo $DATE | cut -c9-10`
   mv wrfvar_input_d${DOMAIN}_${CCYY}_${MM}_${DD}_${HH}:00.00 gr00
fi

