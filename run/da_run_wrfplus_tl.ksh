#!/bin/sh

# Called from da_minimisation inside WRFVAR. The script is only temporary 
# until we can couple the models through memory rather than files

# VAR produces tl00 used as input wrfinput_d01

# WRF produces 3 timestep files for T+1 to T+3 hardwired to
# 2000-01-25 by the namelist file, renamed to tl01 to tl03 for
# use by VAR

set -x

mkdir tl
cd tl

cp ../tl00 wrfinput_d01
cp ../namelist.var4dtl namelist.input

../wrfplus.exe > wrf_tl.out 2>wrf_tl.error

cp tl2000-01-25_01:00:00 ../tl01
cp tl2000-01-25_02:00:00 ../tl02
cp tl2000-01-25_03:00:00 ../tl03
