#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_grib.ksh
#
# Purpose: Restore GRIB data for input into WPS (fnl 1 deg., gfs 1/2 deg. ).
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export FCST_RANGE=${FCST_RANGE:-$CYCLE_PERIOD}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

if test ! -d $GRIB_DIR; then mkdir $GRIB_DIR; fi

echo "<HTML><HEAD><TITLE>$EXPT restore_data_grib</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_grib</H1><PRE>"

date

export END_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $FCST_RANGE 2>/dev/null`

echo "DATE         $DATE"
echo "END_DATE     $END_DATE"
echo "LBC_FREQ     $LBC_FREQ"
echo "MSS_GRIB_DIR $MSS_GRIB_DIR"
echo 'GRIB_DIR     <A HREF="file:'$GRIB_DIR'">'$GRIB_DIR'</a>'

LOCAL_DATE=$DATE

while test $LOCAL_DATE -le $END_DATE; do

   YEAR=`echo $LOCAL_DATE | cut -c1-4`
   YY=`echo $LOCAL_DATE | cut -c3-4`
   MONTH=`echo $LOCAL_DATE | cut -c5-6`
   DAY=`echo $LOCAL_DATE | cut -c7-8`
   HOUR=`echo $LOCAL_DATE | cut -c9-10`
   
   FILE=fnl_${YY}${MONTH}${DAY}_${HOUR}_00
   DIR=${GRIB_DIR}/$LOCAL_DATE
   mkdir -p ${DIR}

   if test ! -f $DIR/$FILE; then
      echo "Retrieving $MSS_GRIB_DIR/$FILE to $DIR"
      if $DUMMY; then
         echo Dummy restore data > $DIR/$FILE
      else
         msrcp $MSS_GRIB_DIR/$FILE $DIR
      fi
   else
      echo "File $DIR/$FILE exists, skipping"
   fi

   LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 2>/dev/null`
done

date

echo "</BODY></HTML>"

exit 0
