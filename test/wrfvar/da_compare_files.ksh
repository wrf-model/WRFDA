#!/bin/ksh

# da_compare_files.ksh

. da_test_defaults.ksh

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

COUNT=1

DIFFER=0

while [[ $COUNT -le ${#TEXT_FILES[@]} ]]; do
   FILE1=${TEXT_FILES[$COUNT]}
   FILE2=${TEXT_FILES[$COUNT]}

   if [[ -f $DIR1/$FILE1 && -f $DIR2/$FILE2 ]]; then
      # Can't use -q option as missing from braindead Aix
      diff $DIR1/$FILE1 $DIR2/$FILE2 >/dev/null 2>&1
      if [[ $? != 0 ]] then
         DIFFER=1
         echo "$DIR1/$FILE1 $DIR2/$FILE2 differ"
         if $FULL; then
            diff $DIR1/$FILE1 $DIR2/$FILE2
         fi
      fi
   fi 
   let COUNT=$COUNT+1
done

exit $DIFFER

