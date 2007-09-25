#!/bin/ksh

# da_compare_real.ksh

# Purpose: Compare files between real runs

# Text files
TEXT_FILES[1]=namelist.input
TEXT_FILES[1]=namelist.output

da_compare_files.ksh $1 $2
exit $?
