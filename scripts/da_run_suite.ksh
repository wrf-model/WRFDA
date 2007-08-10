#!/bin/ksh
#########################################################################
# Script: da_run_suite.ksh
#
# Purpose: End-to-end testing of the WRF system.
#
# Description:
# The da_run_suite.ksh script is designed for end-to-end real data 
# testing of the following components of the WRF system:
#
# WRFSI, WRF real, 3DVAR_OBSPROC, WRFVAR, UPDATE_BC, and WRF.
#
# Any stage can be switched on/off via environment variables as
# described below. The da_run_suite.ksh script can also cycle the
# above sequence for multiple times, and so can be used for
# extended period assessment of scientific impact. I have
# successfully run the script for month long periods with 6 
# hourly cycling all the above. 

# Before running da_run_suite.ksh, you must do the following:

# 1) Compile the executables for the WRF components you wish to 
# test.
# 3) Restore input datasets (e.g. AVN fields, observations, etc).
# A template da_restore_data_mss.ksh script is called from da_run_suite.ksh, 
# which I use when working on machines that have access to NCAR's 
# Mass Store).
# 4) Overwrite default directories, filenames, namelist parameters,
# etc in script da_run_wrf_wrapper.ksh. This is done via environment
# variables. (TO DO: Automate vertical levels ENV variable - currently hardwired
# in da_run_wrfsi.ksh).
#
# NOTE: The idea is that you overwrite defaults in da_run_suite_wrapper.ksh, 
# NOT da_run_suite.ksh itself. We want to maintain a clean script interface
# (da_run_wrf.ksh) to the WRF system. We cannot support changes made to 
# da_run_wrf.ksh. If you feel you need to modify da_run_wrf.ksh, please 
# email wrfhelp@ucar.edu and we will try to include your ideas. 
#
# Once you have set up your experiment as described above, then run the
# da_run_wrf_wrapper.ksh script.
#
# Thank you, and good luck.
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#Decide which stages to run (run if true):
export RUN_RESTORE_DATA_GRIB=${RUN_RESTORE_DATA_GRIB:-false}
export RUN_RESTORE_DATA_RTOBS=${RUN_RESTORE_DATA_RTOBS:-false}
export RUN_WPS=${RUN_WPS:-false}
export RUN_WRFSI=${RUN_WRFSI:-false}
export RUN_REAL=${RUN_REAL:-false}
export RUN_OBSPROC=${RUN_OBSPROC:-false}
export RUN_WRFVAR=${RUN_WRFVAR:-false}
export RUN_UPDATE_BC=${RUN_UPDATE_BC:-false}
export RUN_WRF=${RUN_WRF:-false}

#Experiment details:
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}                            # Domain name. 
export EXPT=${EXPT:-test}                              # Experiment name.
export SOLVER=${SOLVER:-em}
export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors for WRF-Var/WRF.
export HOSTS=${HOSTS:-${HOME}/hosts}
if test -f $HOSTS; then
   export RUN_CMD=${RUN_CMD:-mpirun -machinefile $HOSTS -np $NUM_PROCS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi
export CLEAN=${CLEAN:-false}
export CYCLING=${CYCLING:-false}                       # Cold start (false), cycle (true).
export FIRST=${FIRST:-true}                            # Cold start (false), cycle (true).

#Time info:
export INITIAL_DATE=${INITIAL_DATE:-2003010100}        # Start date of test period
export FINAL_DATE=${FINAL_DATE:-2003010200}            # Final date of test period.
export LBC_FREQ=${LBC_FREQ:-06}
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}                # Assimilation frequency.
export OBS_FREQ=${OBS_FREQ:-12}
export WINDOW_START=${WINDOW_START:-0}                 # Start ob window difference (hrs).
export WINDOW_END=${WINDOW_END:-0}                     # End ob window difference (hrs). 
export LONG_FCST_TIME_1=${LONG_FCST_TIME_1:-99}
export LONG_FCST_TIME_2=${LONG_FCST_TIME_2:-99}
export LONG_FCST_TIME_3=${LONG_FCST_TIME_3:-99}
export LONG_FCST_TIME_4=${LONG_FCST_TIME_4:-99}
export LONG_FCST_RANGE_1=${LONG_FCST_RANGE_1:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_2=${LONG_FCST_RANGE_2:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_3=${LONG_FCST_RANGE_3:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_4=${LONG_FCST_RANGE_4:-$CYCLE_PERIOD}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk} 
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}        
export DAT_DIR=${DAT_DIR:-$HOME/data} # Data directory.
export MSS_GRIB_DIR=${MSS_GRIB_DIR:-mss:/DSS/DS083.2/data}
export GRIB_DIR=${GRIB_DIR:-$DAT_DIR/fnl}     # GRIB data.
export MSS_RTOBS_DIR=${MSS_RTOBS_DIR:-mss:/BRESCH/RT/DATA}
export RTOBS_DIR=${RTOBS_DIR:-$DAT_DIR/rtobs}     # Real-time observation directory.
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} # Data directory for region.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT} #Run directory.
export RC_DIR=${RC_DIR:-$REG_DIR/rc}     # Reconfiguration directory
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}     # Forecast directory

export WRFSI_DIR=${WRFSI_DIR:-$REL_DIR/wrfsi}                
export WRF_BC_DIR=${WRF_BC_DIR:-$REL_DIR/wrfvar}                

export OK='<FONT COLOR="green">'
export ERR='<FONT COLOR="red">'
export END='</FONT>'

if test ! -d $DAT_DIR; then mkdir $DAT_DIR; fi
if test ! -d $REG_DIR; then mkdir $REG_DIR; fi
if test ! -d $EXP_DIR; then mkdir $EXP_DIR; fi
if test ! -d $EXP_DIR/run; then mkdir $EXP_DIR/run; fi

#From WPS (namelist.wps):
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}                
export RUN_GEOGRID=${RUN_GEOGRID:-true}
export RUN_UNGRIB_AFWA=${RUN_UNGRIB_AFWA:-false}
export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}
export WPS_GEOG_DIR=${WPS_GEOG_DIR:-~wrfhelp/WPS_GEOG}
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export MAP_PROJ=${MAP_PROJ:-lambert}
export REF_LAT=${REF_LAT:-40.0}
export REF_LON=${REF_LON:--98.0}
export TRUELAT1=${TRUELAT1:-30.0}
export TRUELAT2=${TRUELAT2:-60.0}
export STAND_LON=${STAND_LON:--98.0}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}
export GEOG_DATA_RES=${GEOG_DATA_RES:-30s}
export FG_TYPE=${FG_TYPE:-GFS}

#From WRF (namelist.input):
export WRF_DIR=${WRF_DIR:-$REL_DIR/WRFV2}
export WRFNL_DIR=${WRFNL_DIR:-$REL_DIR/wrfnl} 
#&time_control:
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_FRAMES_PER_OUTFILE=${NL_FRAMES_PER_OUTFILE:-1}
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-.true.}
#&domains:
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                        " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                        " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                        " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                        " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000"}
export NL_E_VERT=${NL_E_VERT:-28}                   #
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
#&physics:
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           #
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_RADT=${NL_RADT:-30}                #
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_NUM_SOIL_LAYERS=${NL_NUM_SOIL_LAYERS:-5}
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1}
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_CUDT=${NL_CUDT:-5}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}
#&dynamics:
export NL_W_DAMPING=${NL_W_DAMPING:-0}            #
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             #
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-6}    #
#&bdy_control:
export NL_SPECIFIED=${NL_SPECIFIED:-.true.}          #

#From OBSPROC:
export OBSPROC_DIR=${OBSPROC_DIR:-$REL_DIR/wrfvar/obsproc}   
export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export MAX_OB_RANGE=${MAX_OB_RANGE:-2}             # Maximum difference O, B (hours)

#From WRF-Var:
export NL_VAR4D=${NL_VAR4D:-false}
export BE_DIR=${BE_DIR:-$REG_DIR/be}     # Background error covariance directory.
export DA_DIR=${DA_DIR:-$EXP_DIR/da}     # Forecast directory
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # background errors.

export NL_OB_FORMAT=${NL_OB_FORMAT:-2} # Observation format: 1=BUFR, 2=ASCII "little_r"
export NL_VAR4D_MULTI_INC=${NL_VAR4D_MULTI_INC:-0} # Multi_incremental options: 0=off. 1=on

#From Update_BC:
export PHASE=${PHASE:-false}     # Indicate which phase update_bc is.
#------------------------------------------------------------------------------------------

echo "<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY><H1>$EXPT</H1><PRE>"

echo 'REL_DIR      <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR      <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'WRFVAR_DIR   <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
echo 'WRFPLUS_DIR  <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
echo 'WPS_DIR      <A HREF="file:'$WPS_DIR'">'$WPS_DIR'</a>' $WPS_VN
echo 'WRFSI_DIR    <A HREF="file:'$WRFSI_DIR'">'$WRFSI_DIR'</a>'
echo 'OBSPROC_DIR  <A HREF="file:'$OBSPROC_DIR'">'$OBSPROC_DIR'</a>'

echo "CYCLING      $CYCLING"
echo "DUMMY        $DUMMY"
echo "CLEAN        $CLEAN"
echo "NUM_PROCS    $NUM_PROCS"
echo "INITIAL_DATE $INITIAL_DATE"
echo "FINAL_DATE   $FINAL_DATE"
echo "CYCLE_PERIOD $CYCLE_PERIOD"
echo "LBC_FREQ     $LBC_FREQ"
echo "OBS_FREQ     $OBS_FREQ"
echo "WINDOW_START $WINDOW_START"
echo "WINDOW_END   $WINDOW_END"
echo 'BE_DIR       <A HREF="file:'$BE_DIR'">'$BE_DIR'</a>'
echo 'GRIB_DIR     <A HREF="file:'$GRIB_DIR'">'$GRIB_DIR'</a>'
echo 'RC_DIR       <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR       <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo 'OB_DIR       <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo 'RTOBS_DIR    <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'

export DATE=$INITIAL_DATE

while test $DATE -le $FINAL_DATE; do 
   export PREV_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE -$CYCLE_PERIOD 2>/dev/null`
   export HOUR=`echo $DATE | cut -c9-10`

   if test ! -d $FC_DIR/$DATE; then mkdir -p $FC_DIR/$DATE; fi

   echo "=========="
   echo $DATE
   echo "=========="

   # Decide on length of forecast to run
   export FCST_RANGE=$CYCLE_PERIOD
   if test $HOUR = $LONG_FCST_TIME_1; then export FCST_RANGE=$LONG_FCST_RANGE_1; fi
   if test $HOUR = $LONG_FCST_TIME_2; then export FCST_RANGE=$LONG_FCST_RANGE_2; fi
   if test $HOUR = $LONG_FCST_TIME_3; then export FCST_RANGE=$LONG_FCST_RANGE_3; fi
   if test $HOUR = $LONG_FCST_TIME_4; then export FCST_RANGE=$LONG_FCST_RANGE_4; fi

   if $RUN_RESTORE_DATA_GRIB; then
      export RUN_DIR=$EXP_DIR/run/$DATE/restore_data_grib
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_restore_data_grib $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_restore_data_grib.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error$RC$END"
         exit 1
      fi
   fi

   if $RUN_RESTORE_DATA_RTOBS; then
      export RUN_DIR=$EXP_DIR/run/$DATE/restore_data_rtobs
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_restore_data_rtobs $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_restore_data_rtobs.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error$RC$END"
         exit 1
      fi
   fi
  
   if $RUN_WRFSI; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrfsi
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrfsi $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_wrfsi.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_WPS; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wps
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wps $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_wps.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
      export RUN_GEOGRID=false # Only need to run it once.
   fi

   if $RUN_REAL; then
      export RUN_DIR=$EXP_DIR/run/$DATE/real
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_real $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_real.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_OBSPROC; then
      export RUN_DIR=$EXP_DIR/run/$DATE/obsproc
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_obsproc $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_obsproc.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00

   if $NL_VAR4D; then
     if $CYCLING; then
       if ! $FIRST; then
         if $RUN_UPDATE_BC; then
           export RUN_DIR=$EXP_DIR/run/$DATE/update_bc_4dvar
           export PHASE=true
           mkdir -p $RUN_DIR

           $WRFVAR_DIR/scripts/da_trace.ksh da_run_update_bc $RUN_DIR
           $WRFVAR_DIR/scripts/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
           RC=$?
           if test $? != 0; then
              echo `date` "${ERR}Failed with error $RC$END"
              exit 1
           fi
           export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d${DOMAIN}
         else
           export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
         fi 
       fi
      fi
   fi

   if $RUN_WRFVAR; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrfvar
      mkdir -p $RUN_DIR

      export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d${DOMAIN}
      if $CYCLING; then
         if ! $FIRST; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfout_d${DOMAIN}_${ANALYSIS_DATE}
         fi
      fi
      export DA_ANALYSIS=$FC_DIR/$DATE/analysis

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrfvar $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi

      if test $NL_VAR4D_MULTI_INC = 1 ; then

         export NL_VAR4D_MULTI_INC=2

         export OB_DIR_TMP=$OB_DIR
         export RC_DIR_TMP=$RC_DIR
         export BE_DIR_TMP=$BE_DIR
         export NL_E_WE_TMP=$NL_E_WE
         export NL_E_SN_TMP=$NL_E_SN
         export NL_DX_TMP=$NL_DX
         export NL_DY_TMP=$NL_DY
         export NL_TIME_STEP_TMP=$NL_TIME_STEP

         export OB_DIR=$OB_DIR_LOW
         export RC_DIR=$RC_DIR_LOW
         export BE_DIR=$BE_DIR_LOW
         export NL_E_WE=$NL_E_WE_LOW
         export NL_E_SN=$NL_E_SN_LOW
         export NL_DX=$NL_DX_LOW
         export NL_DY=$NL_DY_LOW
         export NL_TIME_STEP=$NL_TIME_STEP_LOW
         export DA_BACK_ERRORS=$BE_DIR/be.dat
         
         ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

         export OB_DIR=$OB_DIR_TMP
         export RC_DIR=$RC_DIR_TMP
         export BE_DIR=$BE_DIR_TMP
         export NL_E_WE=$NL_E_WE_TMP
         export NL_E_SN=$NL_E_SN_TMP
         export NL_DX=$NL_DX_TMP
         export NL_DY=$NL_DY_TMP
         export NL_TIME_STEP=$NL_TIME_STEP_TMP
         export DA_BACK_ERRORS=$BE_DIR/be.dat

         export NL_VAR4D_MULTI_INC=1

      fi

      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
      export WRF_INPUT=$DA_ANALYSIS
   else     
      if $CYCLING; then
         if ! $FIRST; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfout_d${DOMAIN}_${ANALYSIS_DATE}
         fi
      fi
   fi

   if $RUN_UPDATE_BC; then
      export RUN_DIR=$EXP_DIR/run/$DATE/update_bc
      export PHASE=false
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_update_bc $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $? != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
      export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d${DOMAIN}
#DALE remove
   else
      export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
   fi

   if $RUN_WRF; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrf
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrf $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   export NEXT_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $CYCLE_PERIOD 2>/dev/null`
   export DATE=$NEXT_DATE
   export FIRST=false

done

echo
echo `date` "Suite finished"

echo "</PRE></BODY></HTML>"

exit 0

