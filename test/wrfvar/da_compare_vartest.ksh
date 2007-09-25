#!/bin/ksh

# da_compare_vartest.ksh

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

for RUN in ${MACHINE}_*; do
   if [[ -d $DIR1/$RUN && -d $DIR2/$RUN ]]; then
      da_compare_wrfvar.ksh $DIR1/$RUN $DIR2/$RUN
   fi
done
