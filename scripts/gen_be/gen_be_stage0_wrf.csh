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
 if ( ! $?DOMAIN )        setenv DOMAIN        01         # domain id
 if ( ! $?START_DATE )    setenv START_DATE    2003010200 # Time of first perturbation.
 if ( ! $?END_DATE )      setenv END_DATE      2003012812 # Time of last perturbation.
 if ( ! $?FCST_RANGE1 )   setenv FCST_RANGE1   24         # Forecast range 1 (hours).
 if ( ! $?FCST_RANGE2 )   setenv FCST_RANGE2   12         # Forecast range 2 (hours).
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC-method or ENS-Method.
 if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).

 if ( ! $?RELEASE )       setenv RELEASE       WRF_V2.1.2
 if ( ! $?REL_DIR )       setenv REL_DIR       ${HOME}/code/${RELEASE}
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR    ${REL_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR     ${WRFVAR_DIR}/build
 if ( ! $?DATA_DISK )     setenv DATA_DISK     /smoke
 if ( ! $?REGION )        setenv REGION        con200
 if ( ! $?EXPT )          setenv EXPT          noobs
 if ( ! $?DAT_DIR )       setenv DAT_DIR       ${DATA_DISK}/${USER}/data/${REGION}/${EXPT}
 if ( ! $?ID )            setenv ID            gen_be
 if ( ! $?RUN_DIR )       setenv RUN_DIR       ${DAT_DIR}/$ID
 if ( ! $?STAGE0_DIR )    setenv STAGE0_DIR    ${DAT_DIR}/stage0
 if ( ! $?LOCAL )         setenv LOCAL         false      # True if local machine.
 if ( ! $?NUM_JOBS )      setenv NUM_JOBS      1          # Number of jobs to run.
 if ( ! $?MACHINES )      set MACHINES = ( node1 node1 node2 node2 node3 node3 node4 node4 \
                                           node5 node5 node6 node6 node7 node7 node8 node8 )  # For parallel runs.

 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}
 if ( ! -d ${STAGE0_DIR} )  mkdir ${STAGE0_DIR}

#OK, let's go!

#Derive times of initial/final FCST_RANGE forecasts:
 setenv START_DATE_STAGE0 `${BUILD_DIR}/da_advance_cymdh.exe $START_DATE -$FCST_RANGE1`
 setenv END_DATE_STAGE0   `${BUILD_DIR}/da_advance_cymdh.exe $END_DATE   -$FCST_RANGE1`

 setenv DATE $START_DATE_STAGE0

 while ( $DATE <= $END_DATE_STAGE0 )

   setenv TMP_DIR ${RUN_DIR}/${DATE}
   rm -rf ${TMP_DIR} >&! /dev/null
   mkdir ${TMP_DIR} >&! /dev/null
   cd ${TMP_DIR}

   foreach SV ( psi chi t rh ps )
      if ( ! -d $SV ) mkdir $SV
   end

#  Create file dates:
   setenv FCST_TIME `${BUILD_DIR}/da_advance_cymdh.exe $DATE $FCST_RANGE1`
   echo "gen_be_stage0_wrf: Calculating standard perturbation fields valid at time " $FCST_TIME

   set YYYY = `echo $FCST_TIME | cut -c1-4`
   set MM = `echo $FCST_TIME | cut -c5-6`
   set DD = `echo $FCST_TIME | cut -c7-8`
   set HH = `echo $FCST_TIME | cut -c9-10`
   set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00
   set FILE = ${DAT_DIR}/${DATE}/wrfout_d01_${FILE_DATE}

   set NEXT_DATE = `${BUILD_DIR}/da_advance_cymdh.exe $DATE $FCST_RANGE2`
   if ( $BE_METHOD == NMC ) then
      ln -sf $FILE ${FILE}.e001
      ln -sf ${DAT_DIR}/${NEXT_DATE}/wrfout_d01_${FILE_DATE} ${FILE}.e002
   endif

   ln -fs ${BUILD_DIR}/gen_be_stage0_wrf.exe .
   ./gen_be_stage0_wrf.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE >&! gen_be_stage0_wrf.${FCST_TIME}.out

#  Tidy:
   mv pert.${FCST_TIME}* ${STAGE0_DIR}
#   mv mean.${FCST_TIME}* ${STAGE0_DIR}
   mv gen_be_stage0_wrf.${FCST_TIME}.out ${STAGE0_DIR}
   rm -rf $TMP_DIR >&! /dev/null

   echo $DATE $FILE ${DAT_DIR}/${NEXT_DATE}/wrfout_d01_${FILE_DATE}
   setenv DATE `${BUILD_DIR}/da_advance_cymdh.exe $DATE $INTERVAL`

 end     # End loop over dates.

 exit(0)

