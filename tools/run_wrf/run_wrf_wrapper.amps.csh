#! /bin/csh -f
#########################################################################
# Script: run_wrf_wrapper.csh
#
# Purpose: Provide user-modifiable interface to run_wrf.csh script.
#
# Author: Dale Barker, MMM Division, NCAR.
#
# History:
# 06/06/2003:  First Version.                                      Dale Barker
# 10/02/2004:  Given to Alain Caya for use in EnKF/WRF-VAr study.  Dale Barker
# 22/08/2005:  Given to Kwan-Young Chung for use in AMPS studies.  Dale Barker
# 10/25/2005:  Tidied up for use in WRF V2.1.1 regression tests.   Dale Barker
#
# Request: Please acknowledge author in work that uses this script.
#
# Description:
#
# Here are a few examples of environment variables you may want to 
# change in run_wrf_wrapper.csh:
#
# 1) "setenv RUN_SI .TRUE." runs the SI).
# 2) "setenv WRF_DIR /data1/dmbarker/WRF_V2.1" points to directory
# containing WRF code (I always include all components as subdirectories
# e.g. $WRF_DIR/wrfvar contains the WRF-Var code.
# 3) "setenv START_DATE 2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in run_wrf.csh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################

#########################################################################
#[1] Decide what stages to run:
#########################################################################

#setenv RUN_RESTORE_DATA .TRUE.
#setenv RUN_SI .TRUE.
setenv RUN_REAL .TRUE.
#setenv RUN_OBSPROC .TRUE.
#setenv RUN_VAR .TRUE.
#setenv RUN_WRF_BC .TRUE.
setenv RUN_WRF .TRUE.
#setenv CYCLING .TRUE.

#########################################################################
# [2] Override default environment variables here:
#########################################################################

#Disks, directories:
setenv START_DATE 2004050100
setenv FINAL_DATE 2004050100
setenv CYCLE_PERIOD 12
setenv FCST_RANGE_OUT 24
setenv REGION amps2.20km
setenv NUM_PROCS 8

#Job details:
setenv EXPT wrfvar
setenv DATA_DISK /data4

#WRF SI:
setenv GEOG_DATAROOT /data2/powers/wrfsi_2031/extdata/GEOG
#setenv RUN_GRID_GEN .FALSE.
setenv XDIM 331  # 10km: 662, 20km: 331, 60km: 110
setenv YDIM 313  # 10km: 626, 20km: 313, 60km: 104
setenv MAP_PROJ_NAME 'polar'
setenv CENTRAL_LAT -87.34148
setenv CENTRAL_LON 167.9589
setenv STAND_LATS1 -71.0
setenv STAND_LATS2 -90.0
setenv STAND_LONS 180.0
setenv DELTA_X 20000.0
setenv DELTA_Y 20000.0
setenv SILAVWT_PARM_WRF 2.0

#set VERTICAL_LEVELS - do this directly in run_wrfsi.csh.

#WRF:
setenv WRF_HIST_INT 720
setenv WRF_DT 80.0 # 20km: 80s, 60km: 240s
setenv ZDIM 31
setenv SMOOTH_OPTION 0
setenv DA_MP_PHYSICS 2
setenv DA_RADT 10
setenv DA_DIFF_OPT 1
setenv DA_KM_OPT 4
setenv DA_DAMPCOEF 0.01
setenv DA_TIME_STEP_SOUND 6

#WRF-Var:
#setenv DA_CV_OPTIONS 5
#setenv DA_BACK_ERRORS /ocotillo1/dmbarker/data/con200/noobs/gen_be/NMC.bin_type1/gen_be.NMC.dat

#########################################################################
# [3] Call run_wrf.csh script:
#########################################################################

./run_wrf.csh

exit(0)

