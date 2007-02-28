#!/usr/bin/ksh

export DATA_DIR=/ptmp/liuz/wrf21out/katrina_48km/trunk_biasprep
export WRFVAR_DIR=/ptmp/${USER}/wrfvar/trunk
export START_DATE=2005082500 #  2005080106
export END_DATE=2005083000   #  2005081518
export CYCLE_PERIOD=6
export PLATFORM=noaa
export SATELLITE=15
export SENSOR=amsua
export BIN_DIR=$HOME/bias-on-cluster/src_new
export BUILD_DIR=$WRFVAR_DIR/build
export WORKDIR=/ptmp/${USER}/bias_work
export bias_stat=0  # 1/0 do/no-do statistics/
export bias_veri=1  # 1/0 verif/no-verif

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
   cat ${DATA_DIR}/${CDATE}/biasprep_${PLATFORM}-${SATELLITE}-${SENSOR}.* >> biasprep_${sensor}
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
  platform_id  = 1,
  satellite_id = 15,
  sensor_id    = 3, 
  isurf = 2,
  nscan  = 30
 /
EOF
 
### &END Linux namelist conversion

         \rm -f fort.*
         ln -s biasprep_${sensor}  fort.10  # input: fort.10
         $BUILD_DIR/da_sele_bias.exe < nml_cycle_sele > da_sele_bias_${sensor}.log
         mv fort.11 biassele_${sensor}      # output fort.11
echo '  End da_sele_bias'

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
