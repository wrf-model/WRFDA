#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_mss_files.ksh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo


#-----------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------

echo "---------------------------------------------------------------"
echo "Run Stage 0: Calculate ensemble perturbations from WRF forecasts"
echo "---------------------------------------------------------------"

BEGIN_CPU = `date`
echo "Beginning CPU time: ${BEGIN_CPU}"

# Define environment variables:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export REGION=${REGION:-katrina_12km}
export START_DATE=${START_DATE:-2005082012} # Starting time of period.
export FINAL_DATE=${END_DATE:-2005083100}   # Ending time of period.
export FCST_RANGE=${FCST_RANGE:-24}         # Forecast range of forecast (hours)
export INTERVAL=${INTERVAL:-12}             # Period between files (hours).
export DAT_DIR=${DAT_DIR:-$HOME/data}
export MSS_NOOBS_DIR=${MSS_NOOBS_DIR:-mss:/LIUZ/katrina_12km_noobs}
export DUMMY=${DUMMY:-false}

if test ! -d $DAT_DIR; then
   mkdir $DAT_DIR
fi

cd $DAT_DIR

# OK, let's go!

export DATE=$START_DATE

while test $DATE -lt $FINAL_DATE ; do

   #  Create file dates:

   export FCST_TIME=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $FCST_RANGE`
   echo "Restoring WRF forecasts valid at time " $FCST_TIME

   YYYY = `echo $FCST_TIME | cut -c1-4`
   MM = `echo $FCST_TIME | cut -c5-6`
   DD = `echo $FCST_TIME | cut -c7-8`
   HH = `echo $FCST_TIME | cut -c9-10`
   FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

   DATE2 = `${BIN_DIR}/advance_cymdh.exe $DATE $INTERVAL`

   # Create filenames for NMC-method input:
   FILE1 = ${DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   FILE2 = ${DATE2}/wrfout_d${DOMAIN}_${FILE_DATE}

   if test ! -d ${DATE}; then
      mkdir ${DATE}
   fi

   if test ! -d ${DATE2}; then
      mkdir ${DATE2}
   fi

   if $DUMMY; then
      echo Dummy msrcp ${MSS_NOOBS_DIR}/${FILE1} ${FILE1}
      echo Dummy msrcp ${MSS_NOOBS_DIR}/${FILE2} ${FILE2}
      touch ${FILE1}
      touch ${FILE2}
   else
      msrcp ${MSS_NOOBS_DIR}/${FILE1} ${FILE1}
      msrcp ${MSS_NOOBS_DIR}/${FILE2} ${FILE2}
   fi

   # if test ! -d $DAT_DIR/${DATE2}; then
   #    mkdir $DAT_DIR/${DATE2}
   # fi

   export DATE=$DATE2

done     # End loop over dates.

END_CPU = `date`
echo "Ending CPU time: ${END_CPU}"

exit 0

