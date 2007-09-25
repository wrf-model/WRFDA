#!/bin/ksh

# da_compare_data.ksh

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

set -x

cd $DIR1

for REG in *; do
   if [[ -d $DIR1/$RUN && -d $DIR2/$RUN ]]; then
      da_compare_region.ksh $DIR1/$REG $DIR2/$REG
   fi
done
