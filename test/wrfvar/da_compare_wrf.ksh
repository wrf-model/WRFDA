#!/bin/ksh

# da_compare_wrf.ksh

# Purpose: Compare files between wrf runs

# Text files
TEXT_FILES[1]=rsl/rsl.out.0000.html
TEXT_FILES[2]=rsl/rsl.error.0000.html
TEXT_FILES[3]=namelist.input
TEXT_FILES[4]=namelist.output

da_compare_files.ksh $1 $2
exit $?
