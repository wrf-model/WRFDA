#!/bin/csh -f

# Called from DA_Minimisation inside WRFVAR. The script is only temporary until we can
# couple the models through memory rather than files

# VAR produces tl00 used as input wrfinput_d01

# WRF produces 3 timestep files for T+1 to T+3 hardwired to
# 2000-01-25 by the namelist file, renamed to tl01 to tl03 for
# use by VAR

 set echo 

 cp tl00 wrfinput_d01
 cp namelist.var4dtl namelist.input

 wrf.exe >>& stdout.tl

 mv tl2000-01-25_01:00:00 tl01
 mv tl2000-01-25_02:00:00 tl02
 mv tl2000-01-25_03:00:00 tl03

 cp namelist.var4dnl namelist.input

