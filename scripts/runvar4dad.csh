#!/bin/csh -f

 set echo 

 cp namelist.var4dad namelist.input

 cp wrf_3dvar_input wrfinput_d01

 rm tl0* 

 mv af03 auxinput3_d01_2000-01-25_03:00:00
 mv af02 auxinput3_d01_2000-01-25_02:00:00
 mv af01 auxinput3_d01_2000-01-25_01:00:00
 mv af00 auxinput3_d01_2000-01-25_00:00:00

 wrf.exe  >>& stdout.ad

 mv wrf_3dvar_input_d01_2000-01-25_00:00:00 gr00

 cp namelist.var4dnl namelist.input

