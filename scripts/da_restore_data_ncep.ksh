#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_ncep.ksh
#
# Purpose: Restore data necessary ro run filter, forecasts from mass store:
#          1) Restore observation files.
#          2) Get global analyses (NCEP/FNL - 1deg res.).
#          3) Get sea ice file.
#
#-----------------------------------------------------------------------

#--------------------------------------------
# 0) Set up various environment variables:
#--------------------------------------------

export START_DATE=${START_DATE:-2004050100}
export END_DATE=${END_DATE:-2004050106}
export LBC_FREQ=${LBC_FREQ:-6}
export DUMMY=${DUMMY:-false}

# Directories:

export REGION=${REGION:-con200}
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export MSS_DIR=${MSS_DIR:-/BRAY/DATA}
export MSS_RT_DIR=${MSS_RT_DIR:-mss:/BRESCH/RT/DATA}
export MSS_AVN_DIR=${MSS_AVN_DIR:-mss:/DSS/DS083.2/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export CS_DIR=${CS_DIR:-$REG_DIR/cs}     
export OB_DIR=${OB_DIR:-$REG_DIR/ob} 

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT restore_data_ncep</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT restore_data_ncep</H1><PRE>"
fi

date

   echo "START_DATE $START_DATE"
   echo "END_DATE   $END_DATE"
   echo "LBC_FREQ   $LBC_FREQ"

LOCAL_DATE=$START_DATE

while test $LOCAL_DATE -le $END_DATE; do

   CCYY=`echo $LOCAL_DATE | cut -c1-4`
   YY=`echo $LOCAL_DATE | cut -c3-4`
   MM=`echo $LOCAL_DATE | cut -c5-6`
   DD=`echo $LOCAL_DATE | cut -c7-8`
   HH=`echo $LOCAL_DATE | cut -c9-10`

   DIR=${OB_DIR}/${LOCAL_DATE}
   mkdir -p ${DIR}
   
   export FILE=obs.$LOCAL_DATE.gz

   if test ! -f ${DIR}/$FILE; then
      echo Retrieving ${MSS_RT_DIR}/${CCYY}${MM}/$FILE to $DIR
      if $DUMMY; then
         echo Dummy restore_data > ${DIR}/${FILE}
      else
         msrcp ${MSS_RT_DIR}/${CCYY}${MM}/$FILE $DIR
      fi
   else
      echo "File ${DIR}/$FILE exists, skipping"
   fi

   AVN_FILE=fnl_${YY}${MM}${DD}_${HH}_00
   DIR=${CS_DIR}/$LOCAL_DATE
   mkdir -p ${DIR}

   if test ! -f $DIR/$AVN_FILE; then
      echo "Retrieving $MSS_AVN_DIR/$AVN_FILE to $DIR"
      if $DUMMY; then
         echo Dummy restore data > $DIR/$AVN_FILE
      else
         msrcp $MSS_AVN_DIR/$AVN_FILE $DIR
      fi
   else
      echo "File $DIR/$AVN_FILE exists, skipping"
   fi

   LOCAL_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 2>/dev/null`
done

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi

exit 0
