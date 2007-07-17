#!/bin/ksh

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

if [[ -d $DIR1/wrf-var ]]; then
   TYPE1_OLD=true
else
   TYPE1_OLD=false
fi

if [[ -d $DIR2/wrf-var ]]; then
   TYPE2_OLD=true
else
   TYPE2_OLD=false
fi

export FULL=${FULL:-false}

# Text files

OLD_FILE[1]=cost_fn
OLD_FILE[2]=grad_fn
OLD_FILE[3]=statistics
OLD_FILE[4]=Missing
OLD_FILE[5]=Missing
OLD_FILE[6]=wrf-var/fort.60
OLD_FILE[7]=wrf-var/fort.50
OLD_FILE[8]=wrf-var/fort.192
OLD_FILE[9]=wrf-var/fort.45

NEW_FILE[1]=cost_fn
NEW_FILE[2]=grad_fn
NEW_FILE[3]=statistics
NEW_FILE[4]=working/unpert_obs
NEW_FILE[5]=working/pert_obs
NEW_FILE[6]=working/check_max_iv
NEW_FILE[7]=working/gts_omb_oma
NEW_FILE[8]=working/filtered_obs
NEW_FILE[9]=working/rand_obs_error

COUNT=1

while [[ $COUNT -le ${#NEW_FILE[@]} ]]; do
   if $TYPE1_OLD; then
      FILE1=${OLD_FILE[$COUNT]}
   else
      FILE1=${NEW_FILE[$COUNT]}
   fi
   if $TYPE2_OLD; then
      FILE2=${OLD_FILE[$COUNT]}
   else
      FILE2=${NEW_FILE[$COUNT]}
   fi

   if [[ -f $DIR1/$FILE1 && -f $DIR2/$FILE2 ]]; then
      diff -q $DIR1/$FILE1 $DIR2/$FILE2
      if [[ $? != 0 ]] && $FULL; then
         diff $DIR1/$FILE1 $DIR2/$FILE2
      fi
   fi 
   let COUNT=$COUNT+1
done

# binary files

OLD_BINARY_FILE[1]=wrf-var/wrf_3dvar_output

NEW_BINARY_FILE[1]=working/analysis

COUNT=1

while [[ $COUNT -le ${#NEW_BINARY_FILE[@]} ]]; do
   if $TYPE1_OLD; then
      FILE1=${OLD_BINARY_FILE[$COUNT]}
   else
      FILE1=${NEW_BINARY_FILE[$COUNT]}
   fi
   if $TYPE2_OLD; then
      FILE2=${OLD_BINARY_FILE[$COUNT]}
   else
      FILE2=${NEW_BINARY_FILE[$COUNT]}
   fi
   if [[ -f $FILE1 && -f $FILE2 ]]; then
      cmp $DIR1/$FILE1 $DIR2/$FILE2
      if [[ $? != 0 ]] && $FULL; then
        ncdump $DIR1/$FILE1 > tmp1
        ncdump $DIR2/$FILE2 > tmp2
        sdiff tmp1 tmp2
        rm -rf tmp1 tmp2
      fi
   fi 
   let COUNT=$COUNT+1
done
