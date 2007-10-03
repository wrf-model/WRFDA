#!/bin/ksh
# da_compare_wrfvar.ksh
# Purpose: Compare files between wrfvar runs

if [[ $# != 2 ]]; then
   echo "Arguments dir1 dir2"
   exit 1
fi

DIR1=$1
DIR2=$2

if [[ ! -d $DIR1 ]]; then
   echo "Directory $DIR1 does not exist"
   exit 1
fi

if [[ ! -d $DIR2 ]]; then
   echo "Directory $DIR2 does not exist"
   exit 1
fi

# Text files

TEXT_FILES[1]=cost_fn
TEXT_FILES[2]=grad_fn
#TEXT_FILES[3]=namelist.output
#TEXT_FILES[4]=statistics
#TEXT_FILES[5]=working/unpert_obs
#TEXT_FILES[6]=working/pert_obs
#TEXT_FILES[7]=working/check_max_iv
#TEXT_FILES[8]=working/gts_omb_oma
#TEXT_FILES[9]=working/filtered_obs
#TEXT_FILES[10]=working/rand_obs_error

NETCDF_FILES[1]=working/wrfvar_output

da_compare_files.ksh $1 $2
exit $?
