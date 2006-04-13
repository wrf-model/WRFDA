#! /bin/csh -f
#-----------------------------------------------------------------------
# Script restore_data.csh
#
# Purpose: Restore data necessary ro run filter, forecasts from mass store:
#          1) Restore observation files.
#          2) Get global analyses (NCEP/FNL - 1deg res.).
#          3) Get sea ice file.
#
#-----------------------------------------------------------------------
#set echo
#--------------------------------------------
# 0) Set up various environment variables:
#--------------------------------------------

if ( ! $?START_DATE )       setenv START_DATE 2004050100
if ( ! $?FCST_RANGE )       setenv FCST_RANGE 12
if ( ! $?LBC_FREQ )         setenv LBC_FREQ 6

#Directories:
if ( ! $?BIN_DIR )          setenv BIN_DIR ${HOME}/bin
if ( ! $?DATA_DISK )        setenv DATA_DISK /data3
if ( ! $?DAT_DIR )          setenv DAT_DIR ${DATA_DISK}/${user}/data
if ( ! $?AVN_DIR )          setenv AVN_DIR ${DAT_DIR}/fnl
if ( ! $?MSS_DIR )          setenv MSS_DIR /BRESCH/RT/DATA 
if ( ! $?RTOBS_DIR )        setenv RTOBS_DIR ${DAT_DIR}/${START_YEAR}${START_MONTH}/mm5rt

source ${BIN_DIR}/get_date_range.csh $START_DATE $FCST_RANGE

#----------------------------------------------------------------
echo "   [1] Restore little_r observation files from ${MSS_DIR}."
#----------------------------------------------------------------

if ( ! -d ${DAT_DIR}/${START_YEAR}${START_MONTH} ) then
   mkdir ${DAT_DIR}/${START_YEAR}${START_MONTH} >&! /dev/null
endif

if ( ! -d ${RTOBS_DIR} ) mkdir $RTOBS_DIR >&! /dev/null

setenv OB_FILE obs.${START_YEAR}${START_MONTH}${START_DAY}${START_HOUR}.gz

if ( -s ${RTOBS_DIR}/$OB_FILE ) then
   echo "   File ${RTOBS_DIR}/$OB_FILE exists."
else
   cd ${RTOBS_DIR}
   msrcp mss:${MSS_DIR}/${START_YEAR}${START_MONTH}/${OB_FILE} .
endif
echo ""

#--------------------------------------------------
echo "   [2] Get global analyses (NCEP/FNL - 1deg res.)."
#---------------------------------------------------

set DATE = $START_DATE
while ( $DATE <= $END_DATE )
   set YY = `echo $DATE | cut -c3-4`
   set MM = `echo $DATE | cut -c5-6`
   set DD = `echo $DATE | cut -c7-8`
   set HH = `echo $DATE | cut -c9-10`

   set AVN_FILE = fnl_${YY}${MM}${DD}_${HH}_00

   if ( ! -s ${AVN_DIR}/$AVN_FILE ) then
      echo "   Retrieving ${AVN_DIR}/$AVN_FILE"
      msrcp mss:/DSS/DS083.2/data/$AVN_FILE $AVN_DIR/.
   else
      echo "   File ${AVN_DIR}/$AVN_FILE exists."
   endif

   set DATE=`${BIN_DIR}/advance_cymdh.exe ${DATE} ${LBC_FREQ}`
end
echo ""

exit (0)
