#!/bin/csh -f

# Called from DA_Transfer_WRFTLtoXa_Adj inside WRFVAR. The script is only temporary until we can
# couple the models through memory rather than files
#
# VAR provides files af00 to af03
#
# The namelist hard wires the case as 3 hours from 200-01-25-00, so
# the inputs to the adjoint, af03 to af00 correspond to T+3 t T+0
#
# WRF produces gradient file wrf_3dvar_input_d01_2000-01-25_00:00:00 which
# is renamed gr00 for VAR

 set echo 

 cp namelist.var4dad namelist.input

 cp wrf_3dvar_input wrfinput_d01

# rm tl0* 

 mv af03 auxinput3_d01_2000-01-25_03:00:00
 mv af02 auxinput3_d01_2000-01-25_02:00:00
 mv af01 auxinput3_d01_2000-01-25_01:00:00
 mv af00 auxinput3_d01_2000-01-25_00:00:00

 wrf.exe  >>& stdout.ad

 mv wrf_3dvar_input_d01_2000-01-25_00:00:00 gr00

 cp namelist.var4dnl namelist.input

