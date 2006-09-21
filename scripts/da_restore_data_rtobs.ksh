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

export CYCLE_PERIOD=${CYCLE_PERIOD:-6}
export OBS_FREQ=${OBS_FREQ:-$CYCLE_PERIOD}
export DUMMY=${DUMMY:-false}

# Directories:

export REGION=${REGION:-con200}
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export MSS_RT_DIR=${MSS_RT_DIR:-mss:/BRESCH/RT/DATA}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export OB_DIR=${OB_DIR:-$REG_DIR/ob} 

echo "<HTML><HEAD><TITLE>$EXPT restore_data_rtobs</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_rtobs</H1><PRE>"

date

export START_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${DATE} $WINDOW_START 2>/dev/null`
export END_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${DATE} $WINDOW_END 2>/dev/null`

echo "DATE       $DATE"
echo "START_DATE $START_DATE"
echo "END_DATE   $END_DATE"
echo "OBS_FREQ   $OBS_FREQ"
echo "MSS_RT_DIR $MSS_RT_DIR"
echo 'OB_DIR     <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'

LOCAL_DATE=$START_DATE

while test $LOCAL_DATE -le $END_DATE; do

   YEAR=`echo $LOCAL_DATE | cut -c1-4`
   MONTH=`echo $LOCAL_DATE | cut -c5-6`
   DAY=`echo $LOCAL_DATE | cut -c7-8`
   HOUR=`echo $LOCAL_DATE | cut -c9-10`

   DIR=${OB_DIR}/${LOCAL_DATE}
   mkdir -p ${DIR}
   
   export FILE=obs.$LOCAL_DATE.gz

   if test ! -f ${DIR}/$FILE; then
      echo Retrieving ${MSS_RT_DIR}/${YEAR}${MONTH}/$FILE to $DIR
      if $DUMMY; then
         echo Dummy restore_data > ${DIR}/${FILE}
      else
         msrcp ${MSS_RT_DIR}/${YEAR}${MONTH}/$FILE $DIR
      fi
   else
      echo "File ${DIR}/$FILE exists, skipping"
   fi

   LOCAL_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${LOCAL_DATE} ${OBS_FREQ} 2>/dev/null`
done

date

echo "</BODY></HTML>"

exit 0
