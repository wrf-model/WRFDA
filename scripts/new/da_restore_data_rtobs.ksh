#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_rtobs.ksh
#
# Purpose: Restore real time observation files from Jim Bresch's archive
#
#-----------------------------------------------------------------------

#--------------------------------------------
# 0) Set up various environment variables:
#--------------------------------------------

export START_DATE=${START_DATE:-2003010100}
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}
export OBS_FREQ=${OBS_FREQ:-$CYCLE_PERIOD}
export OBS_WINDOW_START=${OBS_WINDOW_START:--3}        # Start ob window difference (hrs).
export OBS_WINDOW_END=${OBS_WINDOW_END:-3}             # End ob window difference (hrs).
export DUMMY=${DUMMY:-false}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export MSS_RTOBS_DIR=${MSS_RTOBS_DIR:-mss:/BRESCH/RT/DATA}
export RTOBS_DIR=${RTOBS_DIR:-$DAT_DIR/rtobs} 

if test ! -d $DAT_DIR; then mkdir $DAT_DIR; fi
if test ! -d $RTOBS_DIR; then mkdir $RTOBS_DIR; fi

echo "<HTML><HEAD><TITLE>$EXPT restore_data_rtobs</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_rtobs</H1><PRE>"

date

export END_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe ${START_DATE} $OBS_WINDOW_END 2>/dev/null`

echo "START_DATE $START_DATE"
echo "END_DATE   $END_DATE"
echo "OBS_FREQ   $OBS_FREQ"
echo "MSS_RTOBS_DIR $MSS_RTOBS_DIR"
echo 'RTOBS_DIR  <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'

LOCAL_DATE=$START_DATE

while test $LOCAL_DATE -le $END_DATE; do

   YEAR=`echo $LOCAL_DATE | cut -c1-4`
   MONTH=`echo $LOCAL_DATE | cut -c5-6`
   DAY=`echo $LOCAL_DATE | cut -c7-8`
   HOUR=`echo $LOCAL_DATE | cut -c9-10`

   DIR=${RTOBS_DIR}/${LOCAL_DATE}
   mkdir -p ${DIR}
   
   export FILE=obs.$LOCAL_DATE.gz

   if test ! -f ${DIR}/$FILE; then
      echo Retrieving ${MSS_RTOBS_DIR}/${YEAR}${MONTH}/$FILE to $DIR
      if $DUMMY; then
         echo Dummy restore_data > ${DIR}/${FILE}
      else
         msrcp ${MSS_RTOBS_DIR}/${YEAR}${MONTH}/$FILE $DIR
      fi
   else
      echo "File ${DIR}/$FILE exists, skipping"
   fi

   LOCAL_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe ${LOCAL_DATE} ${OBS_FREQ} 2>/dev/null`
done

date

echo "</BODY></HTML>"

exit 0
