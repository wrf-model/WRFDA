#!/bin/ksh

# Called from da_solve inside WRFVAR. The script is only temporary until we
# can couple the models through memory rather than files
#
# When called the first time, wrfvar_output does not exist, so use the
# existng wrfvar_input as the initial file
#
# Not sure why the auxhistfiles are commented out
#
# The namelist hard wires the case as 3 hours from 200-01-25-00,
# so the T+0 input to VAR is wrfinput_d${DOMAIN} described above.
#
# The other input files for VAR are the T+1,T+2,T+3 dumps

arg1=$1
arg2=$2

cd $WORK_DIR/nl

export G95_UNIT_ENDIAN_98=BIG

if test $NUM_PROCS=1; then
   $RUN_CMD ./wrf.exe > wrf_nl.out 2>wrf_nl.error
fi

exit 0


