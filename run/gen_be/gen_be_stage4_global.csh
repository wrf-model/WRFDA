#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_stage4_global.csh
#
# Purpose: To calculate power spectra for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#set echo

#Define job via environment variables:

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "---------------------------------------------------------------------"
 echo "Run Stage 4: Calculate horizontal covariances (global power spectra)."
 echo "---------------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 if ( ! $?START_DATE )    setenv START_DATE    2004050112 # Starting time of period.
 if ( ! $?END_DATE )      setenv END_DATE      2004052800 # Ending time of period.
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
 if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
 if ( ! $?NUM_LEVELS )    setenv NUM_LEVELS    27         # Hard-wired for now....
 if ( ! $?TESTING_SPECTRAL ) setenv TESTING_SPECTRAL .false. # True if performing spectral tests.

 if ( ! $?GAUSSIAN_LATS ) setenv GAUSSIAN_LATS .FALSE.   
 if ( ! $?ID )            setenv ID ${BE_METHOD}.bin_type${BIN_TYPE}
 if ( ! $?SRC_DIR )       setenv SRC_DIR ${HOME}/code_development/WRF_V2.1.2
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${SRC_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR ${WRFVAR_DIR}/gen_be
 if ( ! $?DATA_DISK )     setenv DATA_DISK /tara
 if ( ! $?DOMAIN )        setenv DOMAIN katrina.12km
 if ( ! $?DAT_DIR )       setenv DAT_DIR ${DATA_DISK}/${user}/data/${DOMAIN}/noobs/gen_be
 if ( ! $?RUN_DIR )       setenv RUN_DIR ${DAT_DIR}/${ID} 
 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}

 set CONTROL_VARIABLES = ( psi chi_u t_u rh ps_u ) 

 foreach CV ( $CONTROL_VARIABLES )
    if ( ! -d ${RUN_DIR}/$CV ) then
       echo "Input data directory ${RUN_DIR}/$CV is missing. Exiting"
       exit
    endif
 end
   
 ln -sf ${BUILD_DIR}/gen_be_stage4_global.exe .

 foreach CV ( $CONTROL_VARIABLES )
    setenv VARIABLE $CV

    if ( $CV == "ps"  ) then
       setenv MAX_VINDEX 1
    else if ( $CV == "ps_u" ) then
       setenv MAX_VINDEX 1
    else
       setenv MAX_VINDEX $NUM_LEVELS
    endif

    set VINDEX = 1
    while ( $VINDEX <= $MAX_VINDEX )

cat >! gen_be_stage4_global_nl.nl << EOF
  &gen_be_stage4_global_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    gaussian_lats = ${GAUSSIAN_LATS},
    testing_spectral = ${TESTING_SPECTRAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    k = ${VINDEX} /
EOF

         ./gen_be_stage4_global.exe >& gen_be_stage4_global.log

         set VINDEX = `expr $VINDEX + 1`
    end # Loop over levels
 end    # Loop over control variables

 exit(0)

