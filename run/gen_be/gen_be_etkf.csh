#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_etkf.csh
#
# Purpose: To perform an Ensemble Transform Kalman Filter (ETKF) 
# rescaling of ensemble forecast perturbations.
# The ensemble mean is the WRF-Var analysis.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo

setenv RELEASE latest
setenv REL_DIR /smoke/dmbarker/code/${RELEASE}
setenv HOSTS $HOME/hosts/smoke.hosts 
setenv EXPT ens_test
setenv NUM_MEMBERS 12
setenv OB_DIR /smoke/dmbarker/data/con200/obs_caya
setenv OB_DIR /smoke/dmbarker/data/con200/get_obs # Alain's cleaned obs.

#setenv DA_BACK_ERRORS /users/bray/data/con200/be/gen_be.NMC.dat
#setenv DA_OBSERVATIONS /users/bray/data/con200/ob/2003010112/ob.ascii
#setenv DA_FIRST_GUESS /users/bray/data/con200/rc/2003010112/wrfinput_d01
#setenv NV 3
#setenv CVAR "'T'"

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#Directory environment variables:
 if ( ! $?RUN_CMD )       setenv RUN_CMD    ' '
 if ( ! $?RELEASE )       setenv RELEASE    WRF_V2.1.2
 if ( ! $?REL_DIR )       setenv REL_DIR    ${HOME}/code/${RELEASE}
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${REL_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR  ${WRFVAR_DIR}/build
 if ( ! $?DATA_DISK )     setenv DATA_DISK  /smoke
 if ( ! $?DAT_DIR )       setenv DAT_DIR    ${DATA_DISK}/${USER}/data
 if ( ! $?REGION )        setenv REGION     con200
 if ( ! $?REG_DIR )       setenv REG_DIR    ${DAT_DIR}/${REGION}
 if ( ! $?EXPT )          setenv EXPT       xwang
 if ( ! $?EXP_DIR )       setenv EXP_DIR    ${REG_DIR}/${EXPT}
 if ( ! $?RUN_DIR )       setenv RUN_DIR    ${EXP_DIR}/etkf
 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}
 cd $RUN_DIR

#General parameters:
 if ( ! $?DATE )          setenv DATE          2003010112 # Time of perturbation.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    12         # Forecast range (hours).
 if ( ! $?NUM_MEMBERS )   setenv NUM_MEMBERS   56         # Number of ensemble members (for ENS).

#Ensemble mean parameters:
 if ( ! $?NV )            setenv NV            15         # Number of variables to average.
 if ( ! $?CVAR )          set CVAR = ( "'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'TSLB'", "'TSK'", \
                                       "'QCLOUD'", "'QRAIN'", "'QVAPOR'", "'U10'", "'V10'", "'T2'", "'Q2'" ) #Names

#WRF-Var parameters:
 if ( ! $?BE_DIR )        setenv BE_DIR ${REG_DIR}/be
 if ( ! $?DA_BACK_ERRORS ) setenv DA_BACK_ERRORS ${BE_DIR}/gen_be.dat
 if ( ! $?OB_DIR )        setenv OB_DIR ${REG_DIR}/ob
 if ( ! $?DA_OBSERVATIONS ) setenv DA_OBSERVATIONS ${OB_DIR}/obs_gts.3dvar.$DATE
 if ( ! $?WINDOW_START )  setenv WINDOW_START  -3         # Start of time window (hrs)
 if ( ! $?WINDOW_END )    setenv WINDOW_END    3          # End of time window (hrs)
 if ( ! $?NL_E_WE  )      setenv NL_E_WE 45               # i grid dimension.
 if ( ! $?NL_E_SN  )      setenv NL_E_SN 45               # j grid dimension.
 if ( ! $?NL_E_VERT )     setenv NL_E_VERT 28             # k grid dimension.
 if ( ! $?NL_DX )         setenv NL_DX 200000.0           # Resolution.
 if ( ! $?NL_DY )         setenv NL_DY 200000.0           # Resolution.
 if ( ! $?DA_NTMAX )      setenv DA_NTMAX 100             # Maximum number of WRF-Var iterations.

#ETKF parameters:
 if ( ! $?NACCUMT1 )      setenv NACCUMT1      1          # ?
 if ( ! $?NACCUMT2 )      setenv NACCUMT2      1          # ?
 if ( ! $?NSTARTACCUM1 )  setenv NSTARTACCUM1  1          # ?
 if ( ! $?NSTARTACCUM2 )  setenv NSTARTACCUM2  1          # ?
 if ( ! $?TAINFLATINPUT ) setenv TAINFLATINPUT 1.0        # ?
 if ( ! $?RHOINPUT )      setenv RHOINPUT      1.0        # ?
 if ( ! $?NOUT )          setenv NOUT          1          # ?

 if ( ! -d ${EXP_DIR}/${DATE} ) mkdir ${EXP_DIR}/${DATE}
 setenv PREV_DATE `${BUILD_DIR}/advance_cymdh.exe $DATE -$FCST_RANGE`

 set YYYY = `echo $DATE | cut -c1-4`
 set MM = `echo $DATE | cut -c5-6`
 set DD = `echo $DATE | cut -c7-8`
 set HH = `echo $DATE | cut -c9-10`
 set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 echo ""
 echo "----------------------------------------------------------------------"
 echo "[1] Compute ensemble mean first guess (run gen_be_ensmean)"
 echo "----------------------------------------------------------------------"

#Copy first member as template for mean:
 setenv DA_FILE ${EXP_DIR}/${PREV_DATE}/wrf_3dvar_input_d01_${FILE_DATE}
 cp ${DA_FILE}.e001 ${DA_FILE}.e000

cat >! gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    filestub = '${DA_FILE}.e000'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cvar = ${CVAR} /
EOF

#Run:
 cp ${BUILD_DIR}/gen_be_ensmean.exe .
 ./gen_be_ensmean.exe >&! gen_be_ensmean.out

 echo ""
 echo "----------------------------------------------------------------------"
 echo "[2] Run WRF-Var for mean analysis, and obs. input to ETKF"
 echo "----------------------------------------------------------------------"
 echo ""

 setenv MEMBER 0
 while ( $MEMBER <= $NUM_MEMBERS )
    if ( $MEMBER < 100 ) setenv MEM 0$MEMBER
    if ( $MEMBER < 10 )  setenv MEM 00$MEMBER
    setenv DA_FIRST_GUESS ${DA_FILE}.e${MEM}

    if ( $MEMBER == 0 ) then
       echo "    Compute ensemble mean analysis (run WRF-Var)" 
       setenv NL_NTMAX $DA_NTMAX # Only need to calculate Hxb, etc.
       setenv NL_ANALYSIS_TYPE "3D-VAR"
       setenv DA_ANALYSIS ${EXP_DIR}/${DATE}/wrfinput_d01
    else
       echo "    Compute ETKF obs. info (run zero iteration WRF-Var): Member $MEMBER" 
       setenv NL_NTMAX 0                # Only need to calculate Hxb, etc.
       setenv NL_ANALYSIS_TYPE "VERIFY" # Ensures ob files are same size for ETKF.
       setenv DA_ANALYSIS analysis      # Don't need to save these.
    endif

    rm -rf $DA_ANALYSIS >&! /dev/null

    ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh >&! da_run_wrfvar.e${MEM}.out
    setenv MEMBER `expr $MEMBER + 1`

#   Add line with number of observations, and move ETKF file:
    wc -l ${RUN_DIR}/working/fort.51 > ${EXP_DIR}/${DATE}/ob.etkf.e${MEM}
    cat ${RUN_DIR}/working/fort.51 >> ${EXP_DIR}/${DATE}/ob.etkf.e${MEM} 
 end

 echo "----------------------------------------------------------------------"
 echo "[3] Perform Ensemble Transform Kalman Filter (ETKF) assimilation"
 echo "----------------------------------------------------------------------"

 set FILE = ${DAT_DIR}/${PREV_DATE}/wrfout_d01_${FILE_DATE}

cat >! gen_be_etkf_nl.nl << EOF
  &gen_be_etkf_nl
    filestub = '${FILE}',
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cvar = ${CVAR},
    naccumt1 = ${NACCUMT1},
    naccumt2 = ${NACCUMT2},
    nstartaccum1 = ${NSTARTACCUM1},
    nstartaccum2 = ${NSTARTACCUM2},
    tainflatinput = ${TAINFLATINPUT},
    rhoinput = ${RHOINPUT},
    nout = ${NOUT} /
EOF

#Run:
 cp ${BUILD_DIR}/gen_be_etkf.exe .
 ./gen_be_etkf.exe >&! gen_be_etkf.out

#Tidy:
# rm -rf tmp* *.exe >&! /dev/null

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

exit(0)

