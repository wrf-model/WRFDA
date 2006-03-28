#!/bin/csh -f

 set echo 

 cp namelist.var4dnl namelist.input

 if ( -e wrfvar_output ) then
   mv wrfvar_output wrfinput_d01
 else
   cp wrfvar_input wrfinput_d01
 endif

 wrf.exe >>& stdout.nl

 cp wrfinput_d01                            fgat_fg.01
 mv wrfvar_input_d01_2000-01-25_01:00:00 fgat_fg.02
 mv wrfvar_input_d01_2000-01-25_02:00:00 fgat_fg.03
 mv wrfvar_input_d01_2000-01-25_03:00:00 fgat_fg.04

