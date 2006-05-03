#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_stage0_wrf.csh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#
# Note: START_DATE and END_DATE are defined as the times of the first and 
# last perturbation. We derive START_DATE_STAGE0 and END_DATE_STAGE0
# from these using FCST_RANGE.  
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#Define environment variables:
 if ( ! $?START_DATE )    setenv START_DATE    2003010200 # Time of first perturbation.
 if ( ! $?END_DATE )      setenv END_DATE      2003012812 # Time of last perturbation.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    24         # Forecast range of forecast (hours).
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC-method or Ensemble-Method
 if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).

 if ( ! $?EXPT )          setenv EXPT         noobs
 if ( ! $?ID )            setenv ID           gen_be
 if ( ! $?SRC_DIR )       setenv SRC_DIR      ${HOME}/code_development/WRF_V2.1.2
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR   ${SRC_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR    ${WRFVAR_DIR}/gen_be
 if ( ! $?BIN_DIR )       setenv BIN_DIR      ${WRFVAR_DIR}/tools
 if ( ! $?DATA_DISK )     setenv DATA_DISK    /ocotillo1
 if ( ! $?DOMAIN )        setenv DOMAIN       con200      # Application name.
 if ( ! $?DAT_DIR )       setenv DAT_DIR      ${DATA_DISK}/${USER}/data/${DOMAIN}/${EXPT}
 if ( ! $?RUN_DIR )       setenv RUN_DIR      ${DAT_DIR}/$ID
 if ( ! $?STAGE0_DIR )    setenv STAGE0_DIR   ${RUN_DIR}/stage0

 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}
 if ( ! -d ${STAGE0_DIR} )  mkdir ${STAGE0_DIR}

#OK, let's go!

#Derive times of initial/final FCST_RANGE forecasts:
 setenv START_DATE_STAGE0 `${BIN_DIR}/advance_cymdh $START_DATE -$FCST_RANGE`
 setenv END_DATE_STAGE0   `${BIN_DIR}/advance_cymdh $END_DATE   -$FCST_RANGE`

 setenv DATE $START_DATE_STAGE0

 while ( $DATE <= $END_DATE_STAGE0 )

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
   set FILE = ${DAT_DIR}/${DATE}/wrfout_d01_${FILE_DATE}

   set NEXT_DATE = `${BIN_DIR}/advance_cymdh $DATE $INTERVAL`
   if ( $BE_METHOD == NMC ) then
      ln -sf $FILE ${FILE}.e001
      ln -sf ${DAT_DIR}/${NEXT_DATE}/wrfout_d01_${FILE_DATE} ${FILE}.e002
   endif

   ln -fs ${BUILD_DIR}/gen_be_stage0_wrf.exe .
   ./gen_be_stage0_wrf.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE >&! gen_be_stage0_wrf.${FCST_TIME}.out

#  Tidy:
#   mv pert.${FCST_TIME}* ${STAGE0_DIR}
#   mv gen_be_stage0_wrf.${FCST_TIME}.out ${STAGE0_DIR}
#   rm -rf $TMP_DIR >&! /dev/null

   setenv DATE $NEXT_DATE

 end     # End loop over dates.

 exit(0)

