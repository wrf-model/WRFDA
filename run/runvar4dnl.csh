#!/bin/csh -f

# Called from solve_3d inside WRFVAR. The script is only temporary until we can
# couple the models through memory rather than files
#
# When called the first time, wrf_3dvar_output does not exist, so use the
# existng wrf_3dvar_input as the initial file
#
# Not sure why the auxhistfiles are commented out
#
# The namelist hard wires the case as 3 hours from 200-01-25-00,
# so the T+0 input to VAR is wrfinput_d01 described above.
#
# The other input files for VAR are the T+1,T+2,T+3 dumps

 set echo 

 cp namelist.var4dnl namelist.input

 if ( -e wrf_3dvar_output ) then
   mv wrf_3dvar_output wrfinput_d01
 else
   cp wrf_3dvar_input wrfinput_d01
 endif

 wrf.exe >>& stdout.nl

# cp auxhist2_d01_2000-01-25_00:00:00 fgat_fg.01
# cp auxhist2_d01_2000-01-25_01:00:00 fgat_fg.02
# cp auxhist2_d01_2000-01-25_02:00:00 fgat_fg.03
# cp auxhist2_d01_2000-01-25_03:00:00 fgat_fg.04

 cp wrfinput_d01                            fgat_fg.01
 mv wrf_3dvar_input_d01_2000-01-25_01:00:00 fgat_fg.02
 mv wrf_3dvar_input_d01_2000-01-25_02:00:00 fgat_fg.03
 mv wrf_3dvar_input_d01_2000-01-25_03:00:00 fgat_fg.04

