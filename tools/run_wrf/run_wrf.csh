#! /bin/csh -f
#########################################################################
# Script: run_wrf.csh
#
# Purpose: End-to-end testing of the WRF system.
#
# Author: Dale Barker, MMM Division, NCAR.
#
# History:
# 06/06/2003:  First Version.                                      Dale Barker
# 10/02/2004:  Given to Alain Caya for use in EnKF/WRF-VAr study.  Dale Barker
# 22/08/2005:  Given to Kwan-Young Chung for use in AMPS studies.  Dale Barker
# 10/25/2005:  Tidied up for use in Katrina tutorial case.         Dale Barker
# 02/15/2006:  Create beta release of scripts for commit.          Dale Barker
#
# Request: Please acknowledge author in work that uses this script.
#
# Description:
# The run_wrf.csh script is designed for end-to-end real data 
# testing of the following components of the WRF system:
#
# WRF SI, WRF real, 3DVAR_OBSPROC, WRF-Var, WRF_BC, and WRF.
#
# Any stage can be switched on/off via environment variables as
# described below. The run_wrf.csh script can also cycle the
# above sequence for multiple times, and so can be used for
# extended period assessment of scientific impact. I have
# successfully run the script for month long periods with 6 
# hourly cycling all the above. 

# Before running run_wrf.csh, you must do the following:

# 1) Compile the executables for the WRF components you wish to 
# test.
# 2) Manually compile advance_cymdh.f90 (TO DO: automate).
# 3) Restore input datasets (e.g. AVN fields, observations, etc).
# A template restore_data.csh script is called from run_wrf.csh, 
# which I use when working on machines that have access to NCAR's 
# Mass Store).
# 4) Overwrite default directories, filenames, namelist parameters,
# etc in script run_wrf_wrapper.csh. This is done via environment
# variables. (TO DO: Automate vertical levels ENV variable - currently hardwired
# in run_wrfsi.csh).
#
# NOTE: The idea is that you overwrite defaults in run_wrf_wrapper.csh, 
# NOT run_wrf.csh itself. We want to maintain a clean script interface
# (run_wrf.csh) to the WRF system. We cannot support changes made to 
# run_wrf.csh. If you feel you need to modify run_wrf.csh, please 
# email wrfhelp@ucar.edu and we will try to include your ideas. 
#
# Once you have set up your experiment as described above, then run the
# run_wrf_wrapper.csh script.
#
# Thank you, and good luck.
#########################################################################

#set echo

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

if ( ! $?START_DATE )       setenv START_DATE 2003010100     # Start date of test period
if ( ! $?FINAL_DATE )       setenv FINAL_DATE 2003012800     # Final date of test period.
if ( ! $?CYCLE_PERIOD )     setenv CYCLE_PERIOD 12           # Assimilation frequency.
if ( ! $?FCST_TIMES )       set FCST_TIMES = ( 00 12 99 99)  # Run FCST_RANGE_OUThr fcsts at these hours.
if ( ! $?FCST_RANGE_OUT )   setenv FCST_RANGE_OUT ${CYCLE_PERIOD} # Length of forecasts to run at FCST_TIMES.
if ( ! $?LBC_FREQ )         setenv LBC_FREQ 06               # Boundary condition frequency.
if ( ! $?DOMAIN )           setenv DOMAIN 1                  # Domain name. 
if ( ! $?REGION )           setenv REGION con200             # Region name. 
if ( ! $?NUM_PROCS )        setenv NUM_PROCS 1               # Number of processors for WRF-Var/WRF.

if ( ! $?RUN_RESTORE_DATA ) setenv RUN_RESTORE_DATA .FALSE.  # Run if true.
if ( ! $?RUN_SI )           setenv RUN_SI .FALSE.            # Run if true.
if ( ! $?RUN_REAL )         setenv RUN_REAL .FALSE.          # Run if true.
if ( ! $?RUN_OBSPROC )      setenv RUN_OBSPROC .FALSE.       # Run if true.
if ( ! $?RUN_VAR )          setenv RUN_VAR .FALSE.           # Run if true.
if ( ! $?RUN_WRF_BC )       setenv RUN_WRF_BC .FALSE.        # Run if true.
if ( ! $?RUN_WRF )          setenv RUN_WRF .FALSE.           # Run if true.
if ( ! $?CYCLING )          setenv CYCLING .FALSE.           # Cold start (false), cycle (true).
if ( ! $?EXPT )             setenv EXPT con200               # Experiment name.

#Directories:
if ( ! $?TMP_DIR )          setenv TMP_DIR /var/tmp/${user}  # Temporary directory.
if ( ! $?DATA_DISK )        setenv DATA_DISK /ocotillo1      # Data Disk name. 
if ( ! $?DAT_DIR )          setenv DAT_DIR ${DATA_DISK}/${user}/data # Data directory.
if ( ! -d $DAT_DIR )        mkdir $DAT_DIR
if ( ! $?REG_DIR )          setenv REG_DIR ${DAT_DIR}/${REGION}# Data directory for region.
if ( ! -d ${REG_DIR} )      mkdir ${REG_DIR}
if ( ! $?RUN_DISK )         setenv RUN_DISK ${DAT_DIR}/${REGION} #Run directory.
if ( ! -d ${RUN_DISK} )     mkdir ${RUN_DISK}
if ( ! $?OBS_DIR )          setenv OBS_DIR ${REG_DIR}/ob     # Observation directory.
if ( ! -d $OBS_DIR )        mkdir $OBS_DIR
if ( ! $?SRC_DIR )          setenv SRC_DIR ${HOME}/code/WRF_V2.1             # Code directory.
if ( ! $?WRFSI_DIR )        setenv WRFSI_DIR ${SRC_DIR}/wrfsi                # Code subdirectory. 
if ( ! $?WRF_DIR )          setenv WRF_DIR ${SRC_DIR}/WRFV2                  # Code subdirectory.
if ( ! $?OBSPROC_DIR )      setenv OBSPROC_DIR ${SRC_DIR}/3DVAR_OBSPROC      # Code subdirectory.
if ( ! $?WRFVAR_DIR )       setenv WRFVAR_DIR ${SRC_DIR}/wrfvar              # Code subdirectory.
if ( ! $?BIN_DIR )          setenv BIN_DIR ${WRFVAR_DIR}/tools/run_wrf       # These scripts.
if ( ! $?WRF_BC_DIR )       setenv WRF_BC_DIR ${SRC_DIR}/WRF_BC              # Code subdirectory.

#WRFSI directories:
if ( ! $?SOURCE_ROOT )      setenv SOURCE_ROOT ${WRFSI_DIR}
if ( ! $?INSTALLROOT )      setenv INSTALLROOT ${SOURCE_ROOT}             # Where SI binaries created.
if ( ! $?GEOG_DATAROOT )    setenv GEOG_DATAROOT /usr/local/wrfsi/SI_GEOG # Where case directories reside.
if ( ! $?EXT_DATAROOT )     setenv EXT_DATAROOT ${INSTALLROOT}/extdata    # GRIB files processed in grib_prep here.
if ( ! $?DATAROOT )         setenv DATAROOT ${DATA_DISK}/${user}/wrfsi/domains
if ( ! $?MOAD_DATAROOT )    setenv MOAD_DATAROOT ${DATAROOT}/${REGION}
if ( ! $?AVN_DIR )          setenv AVN_DIR ${DAT_DIR}/ncep    # Directory for AVN data (for SI).
if ( ! -d $AVN_DIR )        mkdir $AVN_DIR

#WRFSI namelist (wrfsi.nl) variables:
if ( ! $?RUN_GRID_GEN )     setenv RUN_GRID_GEN .TRUE.       # Run grid_gen if .TRUE.
if ( ! $?USE_OTHER_STATIC_DATA ) setenv USE_OTHER_STATIC_DATA .FALSE. # e.g. different topo files.
if ( ! $?OTHER_STATIC_DATA ) setenv OTHER_STATIC_DATA ${DAT_DIR}/amps60.static/static/static.wrfsi.d0${DOMAIN}
if ( ! $?XDIM )             setenv XDIM 45                   # 
if ( ! $?YDIM )             setenv YDIM 45                   # 
if ( ! $?MAP_PROJ_NAME )    setenv MAP_PROJ_NAME 'lambert'
if ( ! $?CENTRAL_LAT )      setenv CENTRAL_LAT 40.0          # 
if ( ! $?CENTRAL_LON )      setenv CENTRAL_LON -98.0         # 
if ( ! $?STAND_LATS1 )      setenv STAND_LATS1 30.0          # 
if ( ! $?STAND_LATS2 )      setenv STAND_LATS2 60.0          # 
if ( ! $?STAND_LONS )       setenv STAND_LONS -98.0          # 
if ( ! $?DELTA_X )          setenv DELTA_X 200000.0          # Resolution (m). 
if ( ! $?DELTA_Y )          setenv DELTA_Y 200000.0          # Resolution (m).
if ( ! $?SILAVWT_PARM_WRF ) setenv SILAVWT_PARM_WRF 0.0      # ?
if ( ! $?PTOP_PA )          setenv PTOP_PA 5000.0            # Model top pressure (Pa).
if ( ! $?HINTERP_METHOD )   setenv HINTERP_METHOD 1          #
if ( ! $?VERTICAL_LEVELS )  set VERTICAL_LEVELS = ( " 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                                        " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                                        " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                                        " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                                        " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000" )

#Additional Namelist variable for real (namelist.input):
if ( ! $?WRF_HIST_INT )     setenv WRF_HIST_INT 360          # (minutes)
if ( ! $?WRITE_INPUT_FREQ ) setenv WRITE_INPUT_FREQ 360      # Frequency to output input files (minutes)
if ( ! $?WRF_DT )           setenv WRF_DT 360                # Timestep (s) (dt=4-6*dx(km) recommended).
if ( ! $?ZDIM )             setenv ZDIM 28                   # 
if ( ! $?DEBUG_LEVEL )      setenv DEBUG_LEVEL 0             # 
if ( ! $?SMOOTH_OPTION )    setenv SMOOTH_OPTION 1           # ?
if ( ! $?DA_MP_PHYSICS )    setenv DA_MP_PHYSICS 3           # 
if ( ! $?MP_ZERO_OUT )      setenv MP_ZERO_OUT 2             # 
if ( ! $?DA_RADT )          setenv DA_RADT 30                # 
if ( ! $?DA_SF_SURFACE_PHYSICS ) setenv DA_SF_SURFACE_PHYSICS 1 #(1=Thermal diffusion, 2=Noah LSM).
if ( ! $?DA_W_DAMPING )     setenv DA_W_DAMPING 0            # 
if ( ! $?DA_DIFF_OPT )      setenv DA_DIFF_OPT 0             # 
if ( ! $?DA_KM_OPT )        setenv DA_KM_OPT 1               # 
if ( ! $?DA_DAMPCOEF )      setenv DA_DAMPCOEF 0.2           # 
if ( ! $?DA_TIME_STEP_SOUND ) setenv DA_TIME_STEP_SOUND 6    # 

#Additional namelist variables for 3DVAR_OBSPROC:
if ( ! $?MAX_OB_RANGE )     setenv MAX_OB_RANGE 2             # Maximum difference O, B (hours)
if ( ! $?PS0 )              setenv PS0 100000.0
if ( ! $?TS0 )              setenv TS0 300.0
if ( ! $?TLP )              setenv TLP 50.0

#Additional namelist variables for WRF-Var (a lot more specified in 3dvar script):
 if ( ! $?DA_FG_FORMAT )   setenv DA_FG_FORMAT 1 # First guess format: 1=WRF, 2=MM5, 3=KMA
 if ( ! $?DA_OB_FORMAT )   setenv DA_OB_FORMAT 2 # Observation format: 1=BUFR, 2=ASCII "little_r"
 if ( ! $?DA_CV_OPTIONS )  setenv DA_CV_OPTIONS 3 # Background error statistics: 2=NCAR, 3=NCEP.
 if ( ! $?DA_FIRST_GUESS ) setenv DA_FIRST_GUESS ${DAT_DIR}/wrfinput_d01 # wrf3dvar "first guess" input.
 if ( ! $?DA_BACK_ERRORS ) setenv DA_BACK_ERRORS ${DAT_DIR}/be/be.cv_${DA_CV_OPTIONS} # background errors.

 setenv VERTICAL_GRID_NUMBER $ZDIM # Number of vertical levels.
 setenv WEST_EAST_GRID_NUMBER $XDIM # Number of gridpoints in x(i) dim.
 setenv SOUTH_NORTH_GRID_NUMBER $YDIM # Number of gridpoints in y(j) dim.
 setenv GRID_DISTANCE $DELTA_X # Grid resolution (m).

 setenv PREV_DATE `${BIN_DIR}/advance_cymdh.exe $START_DATE -$CYCLE_PERIOD`

while ( $START_DATE <= $FINAL_DATE )  

   echo "Current date is " $START_DATE
   echo ""

   setenv CCYY `echo $START_DATE | cut -c1-4`
   setenv MM `echo $START_DATE | cut -c5-6`
   setenv DD `echo $START_DATE | cut -c7-8`
   setenv HH `echo $START_DATE | cut -c9-10`

#  Decide on length of forecast to run (longer forecast - FCST_TIMES/FCST_RANGE_OUT):
   setenv FCST_RANGE $CYCLE_PERIOD
   if ( $FCST_RANGE_OUT > $CYCLE_PERIOD ) then
      if ( $HH == $FCST_TIMES[1] || $HH == $FCST_TIMES[2] || \
           $HH == $FCST_TIMES[3] || $HH == $FCST_TIMES[4] ) setenv FCST_RANGE $FCST_RANGE_OUT
   endif

   source ${BIN_DIR}/get_date_range.csh $START_DATE $FCST_RANGE

   if ( $RUN_RESTORE_DATA == .TRUE. ) then
      echo "---------------------------------------------------------------"
      echo "1) Restore necessary files."
      echo "---------------------------------------------------------------"
      echo ""

      ${BIN_DIR}/restore_data.csh
   endif

   if ( $RUN_SI == .TRUE. ) then
      echo "---------------------------------------------------------------"
      echo "2) Run WRF Standard Initialization (SI)."
      echo "---------------------------------------------------------------"
      echo ""

#     Clean out SI output files:
      rm ${MOAD_DATAROOT}/siprd/wrf_real_input_em* >&! /dev/null

      ${BIN_DIR}/run_wrfsi.csh
      ln -sf ${MOAD_DATAROOT}/siprd/wrf_real_input_em* ${WRF_DIR}/test/em_real/.
   endif

#  Create WRF namelist.input:
   ${BIN_DIR}/create_namelist.input.csh >&! /dev/null

#  Link MPI node information:
   ln -sf ${BIN_DIR}/nodes ${WRF_DIR}/test/em_real/.

   if ( $RUN_REAL == .TRUE. ) then
      echo "---------------------------------------------------------------"
#DALE06      echo "3) Run WRF real.exe on ${NUM_PROCS} processors."
      echo "3) Run WRF real.exe on 1 processor."
      echo "---------------------------------------------------------------"
      echo ""

      cd ${WRF_DIR}/test/em_real

#     Run real:
      mpirun -nolocal -np 1 -machinefile nodes ./real.exe >&! /dev/null
      mv wrfinput_d01  ${REG_DIR}/wrfinput_d01.${START_DATE}
      mv wrfbdy_d01    ${REG_DIR}/wrfbdy_d01.${START_DATE}
      mv wrflowinp_d01 ${REG_DIR}/wrflowinp_d01.${START_DATE} >&! /dev/null

      rm ${MOAD_DATAROOT}/siprd/wrf_real_input_em* >&! /dev/null
      rm ${WRF_DIR}/test/em_real/wrf_real_input_em* >&! /dev/null
   endif

   setenv RUN_DIR ${RUN_DISK}/${EXPT}/${START_DATE}
   if ( ! -d ${RUN_DISK}/${EXPT} ) mkdir ${RUN_DISK}/${EXPT}
   if ( ! -d ${RUN_DIR} ) mkdir ${RUN_DIR}

#DALE06   setenv DA_OBSERVATIONS ${OBS_DIR}/obs_gts.3dvar.${START_DATE}
   setenv DA_OBSERVATIONS ${OBS_DIR}/${START_DATE}/ob.ascii

   if ( $RUN_OBSPROC == .TRUE. ) then

      echo "---------------------------------------------------------------"
      echo "4) Preprocess observations (runs 3DVAR_OBSPROC)."
      echo "---------------------------------------------------------------"
      echo ""

      ${BIN_DIR}/preprocess_ob.csh >&! /dev/null
   endif

   setenv DA_ANALYSIS ${RUN_DIR}/wrf_3dvar_output
   setenv START_YEAR `echo $START_DATE | cut -c1-4`
   setenv START_MONTH `echo $START_DATE | cut -c5-6`
   setenv START_DAY `echo $START_DATE | cut -c7-8`
   setenv START_HOUR `echo $START_DATE | cut -c9-10`

   setenv DA_FIRST_GUESS ${REG_DIR}/wrfinput_d01.${START_DATE}

   if ( $RUN_VAR == .TRUE. ) then
      echo "---------------------------------------------------------------"
      echo "5) Run WRF 3DVAR on ${NUM_PROCS} processors."
      echo "---------------------------------------------------------------"
      echo ""

      if ( $CYCLING == ".TRUE." ) then
         echo "   Running 3D-Var in full cycling mode."
         setenv CYCLING_FG wrf_3dvar_input_d01_${START_YEAR}-${START_MONTH}-${START_DAY}_${START_HOUR}:00:00
         setenv DA_FIRST_GUESS ${RUN_DISK}/$EXPT/${PREV_DATE}/${CYCLING_FG}
      else
         echo "   Running 3D-Var in cold-start mode."
      endif
      echo ""

      ${BIN_DIR}/DA_Run_WRF-Var.csh #>&! /dev/null

      mv ${RUN_DIR}/wrfvar/wrf_3dvar_output ${DA_ANALYSIS}
      mv ${RUN_DIR}/wrfvar/DAProg_WRF-Var.statistics ${RUN_DIR}/DAProg_WRF-Var.statistics
      mv ${RUN_DIR}/wrfvar/fort.50 ${RUN_DIR}/fort.50
      mv ${RUN_DIR}/wrfvar/rsl.out.0000 ${RUN_DIR}/rsl.out.0000.wrfvar
      mv ${RUN_DIR}/wrfvar/namelist.3dvar ${RUN_DIR}/namelist.3dvar
#      rm -rf ${RUN_DIR}/wrfvar >&! /dev/null
   else
      if ( $CYCLING == ".TRUE." ) then
         echo "   Running free-forecast."
         echo ""
         setenv CYCLING_FG wrf_3dvar_input_d01_${START_YEAR}-${START_MONTH}-${START_DAY}_${START_HOUR}:00:00
         ln -sf ${RUN_DISK}/$EXPT/${PREV_DATE}/${CYCLING_FG} $DA_ANALYSIS
      else
         echo "   Running noobs from interpolated fnl."
         echo ""
         ln -sf $REG_DIR/wrfinput_d01.${START_DATE} ${DA_ANALYSIS} #ICs
      endif
   endif

   if ( $RUN_WRF_BC == .TRUE. ) then
      echo "---------------------------------------------------------------"
      echo "6) Update WRF boundary conditions."
      echo "---------------------------------------------------------------"
      echo ""

      $BIN_DIR/update_wrf_bc.csh >&! /dev/null

   else
      ln -sf $REG_DIR/wrfbdy_d01.${START_DATE} ${RUN_DIR}/wrfbdy_d01         #LBCs
   endif

   if ( $RUN_WRF == .TRUE. ) then
      echo "---------------------------------------------------------------"
      echo "7) Run WRF ${FCST_RANGE}hour forecast on ${NUM_PROCS} processors."
      echo "---------------------------------------------------------------"
      echo ""

#     Create temporary directory:
      setenv TMP_DIR ${RUN_DIR}/tmpdir.wrf
      rm -rf ${TMP_DIR} >&! /dev/null
      mkdir ${TMP_DIR}; cd ${TMP_DIR}

#     Copy necessary info (better than link as not overwritten):
      cp ${BIN_DIR}/nodes .
      cp ${WRF_DIR}/main/wrf.exe .
      cp ${WRF_DIR}/test/em_real/namelist.input .
      cp ${WRF_DIR}/run/RRTM_DATA .
      cp ${WRF_DIR}/run/GENPARM.TBL .
      cp ${WRF_DIR}/run/LANDUSE.TBL .

      cp ${WRF_DIR}/run/SOILPARM.TBL .
      cp ${WRF_DIR}/run/VEGPARM.TBL .
      cp ${WRF_DIR}/run/gribmap.txt .
      cp ${DA_ANALYSIS} wrfinput_d01
      cp ${RUN_DIR}/wrfbdy_d01 wrfbdy_d01
#      cp ${REG_DIR}/wrflowinp_d01.${START_DATE} wrflowinp_d01

      mpirun -nolocal -np ${NUM_PROCS} -machinefile nodes ./wrf.exe >&! /dev/null

      mv wrf_3dvar_input* ../.
      mv wrfout* ../.
      rm ../wrfout_d01_${CCYY}-${MM}-${DD}_${HH}:00:00
      mv namelist.input ../.

      rm -rf ${TMP_DIR} >&! /dev/null
   endif

   setenv PREV_DATE ${START_DATE}
   setenv START_DATE `${BIN_DIR}/advance_cymdh.exe $START_DATE $CYCLE_PERIOD`

end

exit(0)

