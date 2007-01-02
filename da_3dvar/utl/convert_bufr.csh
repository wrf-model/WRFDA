#!/bin/csh -f

 set echo

 set fl = $1

 bufr_little_endian_convert.exe ${fl} ${fl}.little_endian

