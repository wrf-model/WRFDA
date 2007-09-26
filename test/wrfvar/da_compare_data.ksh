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

cd $DIR1
DIR1=$PWD # expand partial directories

for REG in *; do
   if [[ -d $DIR1/$REG && -d $DIR2/$REG ]]; then
      da_compare_region.ksh $DIR1/$REG $DIR2/$REG
   fi
done
