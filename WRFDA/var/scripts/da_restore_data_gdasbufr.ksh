#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_rtobs.ksh
#
# Purpose: Restore GDAS BUFR observation files from Zhiquan Liu's archive
#
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/restore_data_gdasbufr}
export WORK_DIR=$RUN_DIR/working
export MSS_GDAS_DIR=mss:/LIUZ/GDAS

if [[ ! -d $DAT_DIR ]]; then mkdir $DAT_DIR; fi

echo "<HTML><HEAD><TITLE>$EXPT restore_data_gdasbufr</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_gdasbufr</H1><PRE>"

date

#export START_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} $WINDOW_START 2>/dev/null)
#export END_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} $WINDOW_END 2>/dev/null)

echo "DATE       $DATE"
#echo "START_DATE $START_DATE"
#echo "END_DATE   $END_DATE"
#echo "OBS_FREQ   $OBS_FREQ"
echo "MSS_GDAS_DIR $MSS_GDAS_DIR"
#echo 'RTOBS_DIR  <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'

LOCAL_DATE=$DATE

#while [[ $LOCAL_DATE -le $END_DATE ]]; do

   YEAR=$(echo $LOCAL_DATE | cut -c1-4)
   MONTH=$(echo $LOCAL_DATE | cut -c5-6)
   DAY=$(echo $LOCAL_DATE | cut -c7-8)
   HOUR=$(echo $LOCAL_DATE | cut -c9-10)

   DIR=${OB_DIR}/${LOCAL_DATE}
   mkdir ${DIR}
   
   if [[ ! -f ${DIR}/ob.bufr ]]; then
      echo Retrieving ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.prepbufr.nr to $DIR/ob.bufr
      msrcp ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.prepbufr.nr $DIR/ob.bufr
   else
      echo "File ${DIR}/ob.bufr exists, skipping"
   fi

   if [[ ! -f ${DIR}/amsua.bufr ]]; then
      echo Retrieving ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bamua.tm00.bufr_d to $DIR/amsua.bufr
      msrcp ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bamua.tm00.bufr_d $DIR/amsua.bufr
   else
      echo "File ${DIR}/amsua.bufr exists, skipping"
   fi

   if [[ ! -f ${DIR}/amsub.bufr ]]; then
      echo Retrieving ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bamub.tm00.bufr_d to $DIR/amsub.bufr
      msrcp ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bamub.tm00.bufr_d $DIR/amsub.bufr
   else
      echo "File ${DIR}/amsub.bufr exists, skipping"
   fi

   if [[ ! -f ${DIR}/mhs.bufr ]]; then
      echo Retrieving ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bmhs.tm00.bufr_d to $DIR/mhs.bufr
      msrcp ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.1bmhs.tm00.bufr_d $DIR/mhs.bufr
   else
      echo "File ${DIR}/mhs.bufr exists, skipping"
   fi

   if [[ ! -f ${DIR}/airs.bufr ]]; then
      echo Retrieving ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.airsev.tm00.bufr_d to $DIR/airs.bufr
      msrcp ${MSS_GDAS_DIR}/${YEAR}${MONTH}/${LOCAL_DATE}/gdas1.t${HOUR}z.airsev.tm00.bufr_d $DIR/airs.bufr
   else
      echo "File ${DIR}/airs.bufr exists, skipping"
   fi

#   LOCAL_DATE=$($BUILD_DIR/da_advance_time.exe ${LOCAL_DATE} ${OBS_FREQ} 2>/dev/null)
#done

date

echo "</BODY></HTML>"

exit 0
