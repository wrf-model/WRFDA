#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_stage0_wrf.csh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "---------------------------------------------------------------"
 echo "Run Stage 0: Calculate ensemble perturbations from WRF forecasts"
 echo "---------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

#Define environment variables:
 if ( ! $?START_DATE )    setenv START_DATE    2003010100 # Initial time of first forecast.
 if ( ! $?END_DATE )      setenv END_DATE      2003012800 # Initial time of last forecast. 
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    24         # Forecast range of forecast (hours).
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC-method or Ensemble-Method
 if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).

 if ( ! $?SRC_DIR )       setenv SRC_DIR      ${HOME}/code_development/WRF_V2.1.2
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR   ${SRC_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR    ${WRFVAR_DIR}/gen_be
 if ( ! $?BIN_DIR )       setenv BIN_DIR      ${WRFVAR_DIR}/tools
 if ( ! $?DATA_DISK )     setenv DATA_DISK    /ocotillo1
 if ( ! $?DOMAIN )        setenv DOMAIN       con200      # Application name.
 if ( ! $?DAT_DIR )       setenv DAT_DIR      ${DATA_DISK}/${USER}/data/${DOMAIN}/noobs
 if ( ! $?RUN_DIR )       setenv RUN_DIR      ${DAT_DIR}/gen_be
 if ( ! $?STAGE0_DIR )    setenv STAGE0_DIR   ${RUN_DIR}/stage0

 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}
 if ( ! -d ${STAGE0_DIR} )  mkdir ${STAGE0_DIR}

#OK, let's go!

 setenv DATE $START_DATE

 while ( $DATE < $END_DATE )

   setenv TMP_DIR ${RUN_DIR}/${DATE}
   rm -rf ${TMP_DIR} >&! /dev/null
   mkdir ${TMP_DIR} >&! /dev/null
   cd ${TMP_DIR}

#  Create file dates:
   setenv FCST_TIME `${BIN_DIR}/advance_cymdh $DATE $FCST_RANGE`
   echo "gen_be_stage0_wrf: Calculating standard perturbation fields valid at time " $FCST_TIME

   set YYYY = `echo $FCST_TIME | cut -c1-4`
   set MM = `echo $FCST_TIME | cut -c5-6`
   set DD = `echo $FCST_TIME | cut -c7-8`
   set HH = `echo $FCST_TIME | cut -c9-10`
   set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

   set DATE2 = `${BIN_DIR}/advance_cymdh $DATE $INTERVAL`

   if ( $BE_METHOD == NMC ) then
#     Create filenames for NMC-method input:
      set FILE1 = ${DAT_DIR}/${DATE}/wrfout_d01_${FILE_DATE}
      set FILE2 = ${DAT_DIR}/${DATE2}/wrfout_d01_${FILE_DATE}
   endif

   ln -fs ${BUILD_DIR}/gen_be_stage0_wrf.exe .
   ./gen_be_stage0_wrf.exe ${FCST_TIME} $FILE1 $FILE2 >&! gen_be_stage0_wrf.out
   mv diff ${STAGE0_DIR}/diff.${FILE_DATE}
   mv gen_be_stage0_wrf.out ${STAGE0_DIR}/gen_be_stage0_wrf.out.${FILE_DATE}
   rm -rf $TMP_DIR >&! /dev/null

   setenv DATE $DATE2

 end     # End loop over dates.

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

 exit(0)

