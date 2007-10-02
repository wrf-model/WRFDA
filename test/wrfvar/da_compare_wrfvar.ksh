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

FILES[1]=cost_fn
FILES[2]=grad_fn
#FILES[3]=namelist.output
#FILES[4]=statistics
#FILES[5]=working/unpert_obs
#FILES[6]=working/pert_obs
#FILES[7]=working/check_max_iv
#FILES[8]=working/gts_omb_oma
#FILES[9]=working/filtered_obs
#FILES[10]=working/rand_obs_error

COUNT=1

DIFFER=0

while [[ $COUNT -le ${#FILES[@]} ]]; do
   FILE=${FILES[$COUNT]}

   if [[ -f $DIR1/$FILE && -f $DIR2/$FILE ]]; then
      # Can't use -q option as missing from braindead Aix
      diff $DIR1/$FILE $DIR2/$FILE >/dev/null 2>&1
      if [[ $? != 0 ]] then
         echo "$DIR1/$FILE $DIR2/$FILE differ"
         DIFFER=1
         if $FULL; then
            diff $DIR1/$FILE $DIR2/$FILE
         fi
      fi
   fi 
   let COUNT=$COUNT+1
done

# binary files

NETCDF_FILES[1]=working/wrfvar_output

COUNT=1

while [[ $COUNT -le ${#NETCDF_FILES[@]} ]]; do
   FILE=${NETCDF_FILES[$COUNT]}

   if [[ -f $DIR1/$FILE && -f $DIR2/$FILE ]]; then
      da_compare_netcdf.ksh $DIR1/$FILE $DIR2/$FILE
      if [[ $? != 0 ]]; then
         DIFFER=1
      fi
   fi 
   let COUNT=$COUNT+1
done

exit $DIFFER
