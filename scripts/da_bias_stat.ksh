#!/usr/bin/ksh
###############################################
#  Bias correction off-line statistics script
#  Author: Zhiquan Liu NCAR/MMM 02/2007
###############################################
export USER=liuz
export WRFVAR_DIR=/ptmp/${USER}/wrfvar/trunk
#export DATA_DIR=/ptmp/hclin/exps/t44/test_gts_crtm/run
export DATA_DIR=/ptmp/${USER}/wrfvar/test/t44/bluevista_trunk2269_gts_crtm_8/run
export BUILD_DIR=${WRFVAR_DIR}/build
export BIN_DIR=${WRFVAR_DIR}/da/da_biascorr_airmass
export WORKDIR=/ptmp/liuz/wrfvar/trunk/da/da_biascorr_airmass/work

export START_DATE=2006100112
export END_DATE=2006100200
export CYCLE_PERIOD=12
export PLATFORM=noaa
export PLATFORM_ID=1
export SATELLITE=16
export SENSOR=amsua
export SENSOR_ID=3
export NSCAN=30
export bias_stat=1  # 1/0 do/no-do statistics/
export bias_veri=0  # 1/0 verif/no-verif

 echo 'WORKING directory is $WORKDIR'

 mkdir $WORKDIR; cd $WORKDIR
 cp $BIN_DIR/mask_asc $WORKDIR

 CDATE=$START_DATE

 export sensor=${PLATFORM}_${SATELLITE}_${SENSOR}
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
# 1.1 data selection and thinning of the data 
#      in the 5 latitude bands
#--------------------------------------------------
#  ISURF :  1=sea only, 2=land+sea, 3=land only
echo 'Start da_sele_bias'  
cat > nml_cycle_sele << EOF
 &INPUTS
  platform_id  = ${PLATFORM_ID},
  satellite_id = ${SATELLITE},
  sensor_id    = ${SENSOR_ID},
  isurf = 2,
  nscan  = ${NSCAN}
 /
EOF
 
### &END Linux namelist conversion

         \rm -f fort.*
         ln -s biasprep_${sensor}  fort.10  # input: fort.10
         $BUILD_DIR/da_sele_bias.exe < nml_cycle_sele > da_sele_bias_${sensor}.log
         mv fort.11 biassele_${sensor}      # output fort.11
echo '  End da_sele_bias'

#--------------------------------------------------------------------------------
# 1.2 Compute core arrays of scan data for this bias file.
#--------------------------------------------------------------------------------
echo 'Start da_scan_bias'
cat > nml_cycle_scan << EOF
 &INPUTS
  kscan = ${NSCAN},
  FAC=2,
  sband=10,
  eband=14,
  smoothing=.true.,
 /
EOF

       \rm -f fort.*
       ln -s biassele_${sensor}  fort.10  # input : fort.10
       $BUILD_DIR/da_scan_bias.exe < nml_cycle_scan > da_scan_bias_${sensor}.log
       mv fort.11 scan_core_${sensor}     # output: fort.11, statistics not divided by nobs
       mv fort.12 scan_bias_${sensor}     # scan bias both band/noband (no smoothing)
       mv fort.112  scan_bias_smooth_${sensor}   # scan bias only band (smoothing)
echo '  End da_scan_bias'

#------------------------------------------------------------------------------
# 2.0 Compute core array of air-mass bias data for this bias file.
#------------------------------------------------------------------------------

echo "Start da_airmass_bias"

cat > nml_cycle_bias << EOF
 &INPUTS
  platform_id  = ${PLATFORM_ID},
  satellite_id = $SATELLTE,
  sensor_id    = ${SENSOR_ID},
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
       ln -s scan_bias_smooth_${sensor} fort.112
       ln -s scan_bias_${sensor}        fort.12
       ln -s biassele_${sensor}         fort.10
       $BBUILD_DIR/da_airmass_bias.exe < nml_cycle_bias > da_airmass_bias_${sensor}.log
       mv fort.11 bias_core_${sensor}
       mv fort.111 bias_coef_${sensor}.bin

echo "  End da_airmass_bias"

#---------------------------------------------
#  3.0 convert binary bias coef file to ASCII
#---------------------------------------------
echo "Start da_conv_bias"

       \rm -f fort.*
       ln -s bias_coef_${sensor}.bin  fort.19
       $BUILD_DIR/da_conv_bias.exe > da_conv_bias_${sensor}.log
       mv fort.77 bias_coef_${sensor}.asc

echo "  End da_conv_bias"

#------------------------------------------------------------------------------
# 4.0 verification
#------------------------------------------------------------------------------

echo "Start da_veri_bias"

cat > nml_veri_bias << EOF
 &INPUTS
  lscan = .true.,
  check_limb = .false.,
  check_mask = .false.,
  FAC=2,
 /
EOF

       \rm -f fort.*
       ln -s biassele_${sensor}         fort.10
       ln -s bias_coef_${sensor}.bin    fort.111
       $BUILD_DIR/da_veri_bias.exe < nml_veri_bias > da_veri_bias_${sensor}.log
       mv fort.11 bias_veri_${sensor}

echo "  End da_veri_bias"

  \rm -f fort*
exit
