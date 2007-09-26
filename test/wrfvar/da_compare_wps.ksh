#!/bin/ksh

# da_compare_wps.ksh

# Purpose: Compare files between wps runs

# Text files
TEXT_FILES[1]=namelist.wps

da_compare_files.ksh $1 $2
exit $?
