#! /bin/csh -f
#-------------------------------------------------------------------
#  Script for running observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#             07/05/2006      Syed RH Rizvi
#-------------------------------------------------------------------

 echo ""
 echo "Running da_ominusb.exe"
 echo ""

#Set up directories:
 setenv REGION amps1

 setenv DA_DIR /Volumes/mahua2/rizvi/wrfvar_update
 setenv DAT_DIR /Volumes/mahua2/rizvi/data/${REGION}
 setenv RUN_DIR ${DAT_DIR}/da_diagnostics

 echo "DA_DIR      = " $DA_DIR
 echo "DAT_DIR     = " $DAT_DIR
 echo "RUN_DIR     = " $RUN_DIR

 if ( ! -d $RUN_DIR ) then
    mkdir $RUN_DIR
 endif
 cd $RUN_DIR

 echo ""
 echo "Running script da_ominusb.csh for region: " ${REGION}
 echo ""


 ln -fs ${DA_DIR}/build/da_ominusb.exe .

#
#---------------------------------------------------
 foreach TYPES ( synop buoy ships metar sondsfc )
 setenv TYPE $TYPES
#---------------------------------------------------
setenv VAR (u_omb v_omb t_omb q_omb p_omb ) 
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
 foreach TYPES ( sound bogus )                        
 setenv TYPE $TYPES
setenv VAR (u_omb v_omb t_omb q_omb ) 
#---------------------------------------------------
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
setenv TYPE airst 
setenv VAR ( t_omb q_omb ) 
#---------------------------------------------------
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
setenv TYPE airep 
setenv VAR ( u_omb v_omb t_omb ) 
#---------------------------------------------------
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
 foreach TYPES ( geoamv polaramv pilot profiler )
 setenv TYPE $TYPES
 setenv VAR (u_omb v_omb ) 
#---------------------------------------------------
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
 foreach TYPES ( gpsref )
 setenv TYPE $TYPES
 setenv VAR (ref_omb ) 
#---------------------------------------------------
 foreach OBS_VAR ( $VAR )
  setenv FILENAME $TYPE${OBS_VAR}
  ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe >&! da_ominusb_${FILENAME}.log

  rm fort.35
  mv fort.30     $FILENAME.sigma_o_b
  mv ominusb.out $FILENAME.out
 end
 end
#---------------------------------------------------
 echo ""
  echo " da_ominusb.csh completed! for " $TYPE
 echo ""
