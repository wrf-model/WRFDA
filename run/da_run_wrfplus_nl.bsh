#!/bin/sh

# Called from da_solve_3d inside WRFVAR. The script is only temporary until we
# can couple the models through memory rather than files
#
# When called the first time, wrfvar_output does not exist, so use the
# existng wrfvar_input as the initial file
#
# Not sure why the auxhistfiles are commented out
#
# The namelist hard wires the case as 3 hours from 200-01-25-00,
# so the T+0 input to VAR is wrfinput_d01 described above.
#
# The other input files for VAR are the T+1,T+2,T+3 dumps

set -x 

mkdir nl
cd nl

cp ../namelist.var4dnl namelist.input
ln -s ../LANDUSE.TBL .
ln -s ../wrfbdy_d01 .

if test -e wrfvar_output; then
   mv ../wrfvar_output wrfinput_d01
else
   cp ../wrfvar_input wrfinput_d01
fi

export G95_UNIT_ENDIAN_98=BIG

mpirun -v -np 1 -machinefile ../hosts ../wrfplus.exe > wrf_nl.out 2>wrf_nl.error

# cp auxhist2_d01_$NL_ANALYSIS_DATE fgat_fg.01
# cp auxhist2_d01_$NL_ANALYSIS_DATE fgat_fg.02
# cp auxhist2_d01_$NL_ANALYSIS_DATE fgat_fg.03
# cp auxhist2_d01_$NL_ANALYSIS_DATE fgat_fg.04

cp wrfinput_d01                       fgat_fg.01
cp wrfvar_input_d01_$NL_ANALYSIS_DATE fgat_fg.02
cp wrfvar_input_d01_$NL_ANALYSIS_DATE fgat_fg.03
cp wrfvar_input_d01_$NL_ANALYSIS_DATE fgat_fg.04

