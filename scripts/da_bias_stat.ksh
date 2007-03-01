#!/usr/bin/ksh
###############################################
#  Bias correction off-line statistics script
#  Author: Zhiquan Liu NCAR/MMM 02/2007
###############################################
export USER=liuz
export WRFVAR_DIR=/ptmp/${USER}/wrfvar/trunk
#export DATA_DIR=/ptmp/hclin/exps/t44/test_gts_crtm/run
export DATA_DIR=/ptmp/${USER}/wrfvar/test/t44/bluevista_trunk2274_gts_crtm_mix_8/run
export BUILD_DIR=${WRFVAR_DIR}/build
export BIN_DIR=${WRFVAR_DIR}/da/da_biascorr_airmass
export WORKDIR=/ptmp/liuz/wrfvar/trunk/da/da_biascorr_airmass/work

export START_DATE=2006100106
export END_DATE=2006101118
export CYCLE_PERIOD=6
export PLATFORM=noaa
export PLATFORM_ID=1
export SATELLITE=16
export SENSOR=amsua
export SENSOR_ID=3
export NSCAN=30

 echo 'WORKING directory is $WORKDIR'

 mkdir $WORKDIR; cd $WORKDIR
 cp $BIN_DIR/mask_asc $WORKDIR

 CDATE=$START_DATE

 export sensor=${PLATFORM}-${SATELLITE}-${SENSOR}
 export sensor_date=${PLATFORM}_${SATELLITE}_${SENSOR}_${CDATE}
 \rm -f biasprep_${sensor}

# 1.0 cat the data together
#------------------------------------
 while [[ $CDATE -le $END_DATE ]]; do
   echo $CDATE
   cat ${DATA_DIR}/${CDATE}/wrfvar/working/biasprep_${PLATFORM}-${SATELLITE}-${SENSOR}.* >> biasprep_${sensor}
   CDATE=`${BUILD_DIR}/da_advance_cymdh.exe ${CDATE} ${CYCLE_PERIOD}`
 done

#--------------------------------------------------
# 2.0 data selection
#--------------------------------------------------
#  ISURF :  1=sea only, 2=land+sea, 3=land only
echo 'Start da_bias_sele'  
cat > nml_sele << EOF
 &INPUTS
  platform_id  = ${PLATFORM_ID},
  satellite_id = ${SATELLITE},
  sensor_id    = ${SENSOR_ID},
  nscan  = ${NSCAN},
  isurf = 2
 /
EOF
 
### &END Linux namelist conversion

         \rm -f fort.*
         ln -s biasprep_${sensor}  fort.10  # input: fort.10
         $BIN_DIR/da_bias_sele.exe < nml_sele > da_bias_sele_${sensor}.log
         mv fort.11 biassele_${sensor}      # output fort.11
echo '  End da_bias_sele'

#--------------------------------------------------------------------------------
# 3.0 Compute scan bias 
#--------------------------------------------------------------------------------
echo 'Start da_bias_scan'
cat > nml_scan << EOF
 &INPUTS
  kscan = ${NSCAN},
  fac=2,
  global=.false.,
  sband=10,
  eband=14,
  smoothing=.true.,
 /
EOF

       \rm -f fort.*
       ln -s biassele_${sensor}  fort.10  # input : fort.10
       $BIN_DIR/da_bias_scan.exe < nml_scan > da_bias_scan_${sensor}.log
       mv fort.11 scan_core_${sensor}     # output: fort.11, statistics not divided by nobs
       mv fort.12 scan_bias_${sensor}     # scan bias both band/noband 
echo '  End da_bias_scan'

#------------------------------------------------------------------------------
# 4.0 Compute air-mass bias coefs
#------------------------------------------------------------------------------

echo "Start da_bias_airmass"

cat > nml_bias << EOF
 &INPUTS
  global=.false.,
  lscan = .true.,
  kscan = ${NSCAN},
  check_limb = .false.,
  check_mask = .false.,
  FAC=2,
  cdate=${CDATE},
 /
EOF

       \rm -f fort.*
       ln -s scan_bias_${sensor} fort.12
       ln -s biassele_${sensor}  fort.10
       $BIN_DIR/da_bias_airmass.exe < nml_bias > da_bias_airmass_${sensor}.log
       mv bcor.asc ${sensor}.bcor

echo "  End da_bias_airmass"

#------------------------------------------------------------------------------
# 5.0 verification
#------------------------------------------------------------------------------

echo "Start da_bias_verif"

cat > nml_verif << EOF
 &INPUTS
  global=.false.,
  lscan = .true.,
  check_limb = .false.,
  check_mask = .false.,
  FAC=2,
 /
EOF

       \rm -f fort.*
       ln -s biassele_${sensor}    fort.10
       ln -s ${sensor}.bcor    bcor.asc
       $BIN_DIR/da_bias_verif.exe < nml_verif > da_bias_verif_${sensor}.log
       mv fort.11 bias_verif_${sensor}
       rm bcor.asc

echo "  End da_bias_verif"

  \rm -f fort*
exit
