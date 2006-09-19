#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_ncep.ksh
#
# Purpose: Restore global analyses (NCEP/FNL - 1deg res.).
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
export MSS_NCEP_DIR=${MSS_NCEP_DIR:-mss:/DSS/DS083.2/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export NCEP_DIR=${NCEP_DIR:-$DAT_DIR/ncep}     

echo "<HTML><HEAD><TITLE>$EXPT restore_data_ncep</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_ncep</H1><PRE>"

date

echo "START_DATE   $START_DATE"
echo "END_DATE     $END_DATE"
echo "LBC_FREQ     $LBC_FREQ"
echo "MSS_NCEP_DIR $MSS_NCEP_DIR"
echo 'NCEP_DIR     <A HREF="file:'$NCEP_DIR'">'$NCEP_DIR'</a>'

LOCAL_DATE=$START_DATE

while test $LOCAL_DATE -le $END_DATE; do

   CCYY=`echo $LOCAL_DATE | cut -c1-4`
   YY=`echo $LOCAL_DATE | cut -c3-4`
   MM=`echo $LOCAL_DATE | cut -c5-6`
   DD=`echo $LOCAL_DATE | cut -c7-8`
   HH=`echo $LOCAL_DATE | cut -c9-10`
   
   FILE=fnl_${YY}${MM}${DD}_${HH}_00
   DIR=${NCEP_DIR}/$LOCAL_DATE
   mkdir -p ${DIR}

   if test ! -f $DIR/$FILE; then
      echo "Retrieving $MSS_NCEP_DIR/$FILE to $DIR"
      if $DUMMY; then
         echo Dummy restore data > $DIR/$FILE
      else
         msrcp $MSS_NCEP_DIR/$FILE $DIR
      fi
   else
      echo "File $DIR/$FILE exists, skipping"
   fi

   LOCAL_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 2>/dev/null`
done

date

echo "</BODY></HTML>"

exit 0
