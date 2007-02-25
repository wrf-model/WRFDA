#! /usr/local/bin/tcsh -f
###################################################
#
#  script for innovation/bias-correction coefs statistics
#
#  Method : need 3 runs (3 pass) of this script
#     pass1 : firstly get the statistics of rms and stdev
#     pass2 : using rms for Quality Control and get bias coefs
#     pass3 : apply bias correction to verify the correctness
#
#  Created by  Zhiquan Liu  2005/11/15
#
###################################################

## set up user environment variables

setenv CYCLE_PERIOD  06            #  cycling period (hour)
setenv PLATFORM      noaa          
setenv isatid        1     # satellite index of begin
setenv maxsatid      3     # number of satellites
set    SATID  =      ( 15 16 17 )     #  2 satllites
setenv SENSOR        amsub
setenv NCHAN         5            #  number of channels
setenv NSCAN         90            #  number of scan position
setenv SRC_DIR       /ptmp/liuz/wrf21out/katrina_12km   # source dir of innovation files
setenv KPASS         3                     #  pass number
setenv KPASS1        `expr ${KPASS} \- 1`  #  used bias file pass number 
#setenv DES_DIR       /ptmp/liuz/biascorr_out/${SENSOR}_pass${KPASS}  # statistics output dir
setenv DES_DIR       $HOME/statistics/${SENSOR}_pass${KPASS}
mkdir  $DES_DIR

if ( $KPASS == 1 ) then
  setenv WRITE_IV      .FALSE.   # if write up single innovation (IV) file
  setenv WRITE_PROF    .FALSE.   # if write RTM and model profiles when write single IV file
  setenv READBIAS      .FALSE.   # if read in bias file     (pass1:false; pass2,3:true)
  setenv BIASCORR      .FALSE.   # if apply bias correction (pass1,2:false; pass3:true)
endif
if ( $KPASS == 2 ) then
  setenv WRITE_IV      .FALSE.   # if write up single innovation (IV) file
  setenv WRITE_PROF    .FALSE.   # if write RTM and model profiles when write single IV file
  setenv READBIAS      .TRUE.    # if read in bias file     (pass1:false; pass2,3:true)
  setenv BIASCORR      .FALSE.   # if apply bias correction (pass1,2:false; pass3:true)
endif
if ( $KPASS == 3 ) then
  setenv WRITE_IV      .FALSE.   # if write up single innovation (IV) file
  setenv WRITE_PROF    .FALSE.   # if write RTM and model profiles when write single IV file
  setenv READBIAS      .TRUE.    # if read in bias file     (pass1:false; pass2,3:true)
  setenv BIASCORR      .TRUE.    # if apply bias correction (pass1,2:false; pass3:true)
endif

while ( $isatid <= $maxsatid )       ## loop over sensor

setenv START_DATE    2005090106
setenv CURRENT_DATE  $START_DATE
setenv END_DATE      2005090312
setenv NINX          1

while ( $CURRENT_DATE <= $END_DATE )   ## loop over date

##  perform statistics for current date
##
cat >! namelist.stat << EOF
&nam_stat
 cdate      = '${CURRENT_DATE}',
 platform   = '${PLATFORM}',
 satid      = '${SATID[${isatid}]}',
 instrument = '${SENSOR}',
 nchan      =  ${NCHAN},
 nscan      =  ${NSCAN},
 directory  = '${SRC_DIR}',
 lwrite_iv  = ${WRITE_IV},
 lwrite_prof= ${WRITE_PROF},
 lreadbias  = ${READBIAS},
 lbiascorr  = ${BIASCORR},
 biasfile   = './${PLATFORM}-${SATID[${isatid}]}-${SENSOR}.bias_pass${KPASS1}' /

EOF

   ./da_stats_rad.exe   # input : innovation file and GSI error file
                        # output: fort.77  ascii current statistics(mean,std,a,b)
                        #         fort.88  binary various sum quantity 
                        #         single innovation file

   mv fort.77 $DES_DIR/stat-${PLATFORM}-${SATID[${isatid}]}-${SENSOR}.${CURRENT_DATE} >&! /dev/null 
   mv ${PLATFORM}-${SATID[${isatid}]}-${SENSOR}.inv_${CURRENT_DATE} $DES_DIR >&! /dev/null

##  accumulate statistics from start to current date
##
cat >! namelist.add << EOF
&nam_stat
 sdate  = '${START_DATE}',
 cdate  = '${CURRENT_DATE}',
 cycle_hour =  ${CYCLE_PERIOD},
 platform   = '${PLATFORM}',
 satid      = '${SATID[${isatid}]}',
 instrument = '${SENSOR}',
 nchan      =  ${NCHAN},
 nscan      =  ${NSCAN} /

EOF
   
   if ( $NINX == 1 ) then
        ./da_stats_init.exe     ##  write init stat file: fort.99
   endif
   
   if ( -f fort.88 )  then      ## input : fort.99  binary total stat
        ./da_stats_add.exe      ##         fort.88  binary current stat
   endif                        ## output: fort.77  ascii total stat
                                ##         fort.98  fort.99+fort.88
   rm -f fort.88 >&! /dev/null
   mv fort.77 ${DES_DIR}/add-${PLATFORM}-${SATID[${isatid}]}-${SENSOR}.${CURRENT_DATE} >&! /dev/null
   mv fort.98 fort.99 >&! /dev/null
   setenv CURRENT_DATE `./advance_cymdh $CURRENT_DATE $CYCLE_PERIOD`
   setenv NINX `expr ${NINX} \+ 1`

end      # end loop over date

   rm -f fort.99 >&! /dev/null
   setenv isatid `expr ${isatid} \+ 1`

end      # end loop over sensor

exit(0)

