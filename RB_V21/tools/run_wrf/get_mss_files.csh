#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_stage0_wrf.csh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#
# History:
#     09/13/2005    Created. Dale Barker
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo

setenv DATA_DISK /mmmtmp
setenv DOMAIN katrina_12km
setenv START_DATE 2005082012
setenv FINAL_DATE 2005083100

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "---------------------------------------------------------------"
 echo "Run Stage 0: Calculate ensemble perturbations from WRF forecasts"
 echo "---------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

#Define environment variables:
 if ( ! $?START_DATE )    setenv START_DATE    2003010100 # Starting time of period.
 if ( ! $?FINAL_DATE )    setenv FINAL_DATE    2003012800 # Ending time of period.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    24         # Forecast range of forecast (hours).
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BIN_DIR )       setenv BIN_DIR ${HOME}/bin/wrf/run_wrf # Script directory.
 if ( ! $?DATA_DISK )     setenv DATA_DISK    /mmmtmp
 if ( ! $?DAT_DIR )       setenv DAT_DIR      ${DATA_DISK}/${USER}/data/${DOMAIN}
 if ( ! -d $DAT_DIR )        mkdir $DAT_DIR
 cd $DAT_DIR

#OK, let's go!

 setenv DATE $START_DATE

 while ( $DATE < $FINAL_DATE )

#  Create file dates:
   setenv FCST_TIME `${BIN_DIR}/advance_cymdh.exe $DATE $FCST_RANGE`
   echo "Restoring WRF forecasts valid at time " $FCST_TIME

   set YYYY = `echo $FCST_TIME | cut -c1-4`
   set MM = `echo $FCST_TIME | cut -c5-6`
   set DD = `echo $FCST_TIME | cut -c7-8`
   set HH = `echo $FCST_TIME | cut -c9-10`
   set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

   set DATE2 = `${BIN_DIR}/advance_cymdh.exe $DATE $INTERVAL`

#  Create filenames for NMC-method input:
   set FILE1 = ${DATE}/wrfout_d01_${FILE_DATE}
   set FILE2 = ${DATE2}/wrfout_d01_${FILE_DATE}

    if ( ! -d ${DATE} ) mkdir ${DATE}
    if ( ! -d ${DATE2} ) mkdir ${DATE2}
    msrcp mss:/LIUZ/katrina_12km_noobs/${FILE1} ${FILE1}
    msrcp mss:/LIUZ/katrina_12km_noobs/${FILE2} ${FILE2}

#    if ( ! -d $DAT_DIR/${DATE2} ) mkdir $DAT_DIR/${DATE2}

   setenv DATE $DATE2

 end     # End loop over dates.

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

 exit(0)

