#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_ep1_ens.csh
#
# Purpose: To calculate ensemble perturbations for use in WRF-Var to
# represent flow-dependent forecast errors.
#
# This version (gen_be_ep1) calculates perturbations for use with WRF-Var and 
# NL_ALPHACV_METHOD=1: Perturbations in control variable (vp) space
# (psi, chi_u, t_u, rh, ps_u).
#
# Note: DATE is defined as the time of the perturbation. We derive 
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#-----------------------------------------------------------------------

#set echo
#Define job via environment variables:

 setenv WRFVAR_DIR /smoke/dmbarker/code/latest/wrfvar.gen_be
 setenv BE_DIR /smoke/dmbarker/data/con200/xwang/gen_be.2003010112

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 if ( ! $?DATE )          setenv DATE          2003010112 # Time of perturbation.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    12         # Forecast range of forecast (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     ENS        # ENS or NMC.
 if ( ! $?NE )            setenv NE            56         # Number of ensemble members (for ENS).
 if ( ! $?BIN_TYPE )      setenv BIN_TYPE      5          # 0=None, 1=1:ni, 2=latitude, ....
 if ( ! $?LAT_MIN )       setenv LAT_MIN       -90.0      # Used if BIN_TYPE = 2.
 if ( ! $?LAT_MAX )       setenv LAT_MAX       90.0       # Used if BIN_TYPE = 2.
 if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
 if ( ! $?HGT_MIN )       setenv HGT_MIN       0.0        # Used if BIN_TYPE = 2.
 if ( ! $?HGT_MAX )       setenv HGT_MAX       20000.0    # Used if BIN_TYPE = 2.
 if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
 if ( ! $?REMOVE_MEAN )   setenv REMOVE_MEAN   .true.     # Remove time/ensemble/area mean.

 if ( ! $?RELEASE )       setenv RELEASE    WRF_V2.1.2
 if ( ! $?REL_DIR )       setenv REL_DIR    ${HOME}/code/${RELEASE}
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${REL_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR  ${WRFVAR_DIR}/build
 if ( ! $?DATA_DISK )     setenv DATA_DISK  /smoke
 if ( ! $?REGION )        setenv REGION     con200
 if ( ! $?EXPT )          setenv EXPT       xwang  
 if ( ! $?DAT_DIR )       setenv DAT_DIR    ${DATA_DISK}/${USER}/data/${REGION}/${EXPT}
 if ( ! $?BE_DIR )        setenv BE_DIR     ${DAT_DIR}/gen_be
 if ( ! $?RUN_DIR )       setenv RUN_DIR    ${DAT_DIR}/${DATE}/ep1

 if ( ! -d ${DAT_DIR}/$DATE ) mkdir ${DAT_DIR}/$DATE
 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}

 setenv TMP_DIR $RUN_DIR/tmp
 rm -rf ${TMP_DIR}; mkdir $TMP_DIR >&! t
 cd $TMP_DIR

#variables:

 foreach SV ( fullflds psi chi t rh ps )
    if ( ! -d $SV ) mkdir $SV
 end

 set CONTROL_VARIABLES = ( psi chi_u t_u rh ps_u )

 foreach CV ( $CONTROL_VARIABLES )
    if ( ! -d $CV ) mkdir $CV
 end

 echo "---------------------------------------------------------------------"
 echo "Calculate model-space ensemble perturbations valid at time $DATE."
 echo "---------------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 set YYYY = `echo $DATE | cut -c1-4`
 set MM = `echo $DATE | cut -c5-6`
 set DD = `echo $DATE | cut -c7-8`
 set HH = `echo $DATE | cut -c9-10`
 set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

 setenv PREV_DATE `${BUILD_DIR}/advance_cymdh.exe $DATE -$FCST_RANGE`
 set FILE = ${DAT_DIR}/${PREV_DATE}/wrfout_d01_${FILE_DATE}

 ln -fs ${BUILD_DIR}/gen_be_stage0_wrf.exe .
 ./gen_be_stage0_wrf.exe ENS $DATE $NE $FILE >&! gen_be_stage0_wrf.out

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

 echo "-------------------------------------------------------------------------------"
 echo "Run gen_be stage 1: Read "standard fields", and remove time/ensemble/area mean."
 echo "-------------------------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 ln -sf ${BUILD_DIR}/gen_be_stage1.exe .

cat >! gen_be_stage1_nl.nl << EOF
  &gen_be_stage1_nl
    start_date = '${DATE}',
    end_date = '${DATE}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT},
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    remove_mean = ${REMOVE_MEAN},
    dat_dir = '${RUN_DIR}/tmp' /
EOF

 ./gen_be_stage1.exe >& gen_be_stage1.out

 set RC = $status
 if ( $RC != 0 ) then
    echo "Stage 1 failed with error" $RC
    exit 1
 endif

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

#First copy regression coefficients:
 ln -sf $BE_DIR/gen_be.${BE_METHOD}.dat .
 ${BUILD_DIR}/gen_be_read_regcoeffs.exe

 echo "---------------------------------------------------------------"
 echo "Run gen_be stage 2a: Calculate control variable fields."
 echo "---------------------------------------------------------------"
  
 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 ln -sf ${BUILD_DIR}/gen_be_stage2a.exe .

cat >! gen_be_stage2a_nl.nl << EOF
  &gen_be_stage2a_nl
    start_date = '${DATE}',
    end_date = '${DATE}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    expt = '${EXPT}',
    dat_dir = '${RUN_DIR}' /
EOF

 ./gen_be_stage2a.exe >&! gen_be_stage2a.out

 set RC = $status
 if ( $RC != 0 ) then
   echo "Stage 2a failed with error" $RC
   exit 1
 endif

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

#Tidy:
 foreach CV ( $CONTROL_VARIABLES )
    rm -rf $RUN_DIR/$CV; mkdir $RUN_DIR/$CV
    setenv MEMBER 1
    while ( $MEMBER <= $NE )
       setenv MEM $MEMBER
       if ( $MEMBER < 100 ) setenv MEM 0$MEMBER
       if ( $MEMBER < 10 )  setenv MEM 00$MEMBER
       if ( $CV == 'ps_u' ) then
          mv $CV/${DATE}.${CV}.${BE_METHOD}.e${MEM}.01 ${RUN_DIR}/${CV}/${CV}.e${MEM}
       else 
          mv $CV/${DATE}.${CV}.${BE_METHOD}.e${MEM} ${RUN_DIR}/${CV}/${CV}.e${MEM}
       endif
       setenv MEMBER `expr $MEMBER + 1`
    end
 end
 mv *.out ${RUN_DIR}
 rm -rf $TMP_DIR >&! /dev/null

exit(0)

