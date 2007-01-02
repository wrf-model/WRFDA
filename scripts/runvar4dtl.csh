#!/bin/csh -f

 set echo 

 cp tl00 wrfinput_d01
 cp namelist.var4dtl namelist.input

 wrf.exe >>& stdout.tl

 mv tl2000-01-25_01:00:00 tl01
 mv tl2000-01-25_02:00:00 tl02
 mv tl2000-01-25_03:00:00 tl03

 cp namelist.var4dnl namelist.input

