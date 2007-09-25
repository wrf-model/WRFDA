#!/bin/ksh

# da_compare_region.ksh

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

for SUITE in *suite*; do
   if [[ -d $DIR1/$RUN && -d $DIR2/$RUN ]]; then
      da_compare_suite.ksh $DIR1/$SUITE $DIR2/$SUITE
   fi
done

if [[ -d $DIR1/vartest && -d $DIR2/vartest ]]; then
   da_compare_vartest.ksh $DIR1/vartest $DIR2/vartest
fi
