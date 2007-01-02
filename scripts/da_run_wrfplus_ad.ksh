#!/bin/ksh

# Called from da_transform_vtoy_adj inside WRFVAR. The script is only 
# temporary until we can couple the models through memory rather than files
#
# VAR provides files af00 to af06
#
# WRF produces gradient file wrfvar_input_d${DOMAIN}_2000-01-25_00:00:00 which
# is renamed gr00 for VAR

if test $NUM_PROCS=1; then
   cd $WORK_DIR/ad
   ./wrfplus.exe > wrf_ad.out 2>wrf_ad.error
fi

