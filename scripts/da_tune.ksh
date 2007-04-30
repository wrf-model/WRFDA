#!/bin/ksh
#-------------------------------------------------------------------------------------------
# Script for Observation error tuning  (Desroziers method)
#   Ref: QJRMS (2001), 127, pp. 1433-1452,
#        Gerald Desroziers and Serguei Ivanov
#-------------------------------------------------------------------------------------------
# Input files :  
#  a) rand_obs_error WRF-Var output files with "omb_add_noise" & "put_rand_seed" as .TRUE.
#  b) pert_obs       WRF-Var output files with "omb_add_noise" & "put_rand_seed" as .TRUE.
#  c) unpert_obs     WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#  d) fort.48        WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#  e) rsl.out.0000   WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#-------------------------------------------------------------------------------------------
#  Note:  For radiance data tuning edit
#         "namelist.radiance" generated down this script
#
#-------------------------------------------------------------------------------------------

echo ""
echo "Running script da_tune.ksh"
echo ""

export REL_DIR=${REL_DIR:-$HOME/code/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export WORK_DIR=${WORK_DIR:-$REG_DIR/tuning}

export START_DATE=${START_DATE:-2006100106}
export END_DATE=${END_DATE:-2006100312}
export CYCLE_PERIOD=6

export YP_DIR=${YP_DIR:-$REG_DIR/with_noise}      # perturbed run
export Y_DIR=${Y_DIR:-$REG_DIR/no_noise}         # unperturbed run

export FILE_PERT=fort.45      # fort.45,  random perturbation
export FILE_YP=fort.46        # fort.46,  perturbed y=Hdx
export FILE_Y=fort.47         # fort.47,   y=Hdx
export FILE_JO=fort.48        # fort.48,   Jo
export FILE_RSLOUT=fort.49    # fort.49,  rsl.out.0000

export NL_RTMINIT_NSENSOR=${NL_RTMINIT_NSENSOR:-2}
export NL_RTMINIT_PLATFORM=${NL_RTMINIT_PLATFORM:-1,1}
export NL_RTMINIT_SATID=${NL_RTMINIT_SATID:-15,16}
export NL_RTMINIT_SENSOR=${NL_RTMINIT_SENSOR:-3,3}
export NL_RTMINIT_NCHAN=${NL_RTMINIT_NCHAN:-15,15} # Not a Registry name

rm -rf $WORK_DIR; mkdir $WORK_DIR; cd $WORK_DIR

ln -fs $WRFVAR_DIR/build/da_tune.exe .
ln -fs $WRFVAR_DIR/build/da_advance_cymdh.exe .
 
cat > namelist.radiance << EOF
&rtminit
   rtminit_nsensor     = $NL_RTMINIT_NSENSOR,
   rtminit_platform    = $NL_RTMINIT_PLATFORM,
   rtminit_satid       = $NL_RTMINIT_SATID,
   rtminit_sensor      = $NL_RTMINIT_SENSOR,
   rtminit_nchan       = $NL_RTMINIT_NCHAN /
EOF

touch fort.45 fort.46 fort.47 fort.48 fort.49 


export DATE=$START_DATE

while test $DATE -le $END_DATE; do

   echo Processing $DATE

   if test -f $YP_DIR/run/${DATE}/wrfvar/working/rand_obs_error; then
      cat $YP_DIR/run/${DATE}/wrfvar/working/rand_obs_error  >> $FILE_PERT        
   else
      echo " Please check file rand_obs_error in " $YP_DIR/run/${DATE}/wrfvar/working
      exit 1             
   fi

   if test -f $YP_DIR/run/${DATE}/wrfvar/working/pert_obs; then                  
      cat $YP_DIR/run/${DATE}/wrfvar/working/pert_obs       >> $FILE_YP        
   else
      echo " Please check file pert_obs in " $YP_DIR/run/${DATE}/wrfvar/working
      exit 2         
   fi

   if test -f $Y_DIR/run/${DATE}/wrfvar/working/unpert_obs; then
      cat $Y_DIR/run/${DATE}/wrfvar/working/unpert_obs      >> $FILE_Y        
   else
      echo " Please check file unpert_obs in " $Y_DIR/run/${DATE}/wrfvar/working
      exit 3          
   fi

   if test -f $Y_DIR/run/${DATE}/wrfvar/working/jo; then
      cat $Y_DIR/run/${DATE}/wrfvar/working/jo                >> $FILE_JO       
   else
      echo " Please check file jo in " $Y_DIR/run/${DATE}/wrfvar/working
      exit 4          
   fi

   if test -f $Y_DIR/run/${DATE}/wrfvar/rsl/rsl.out.0000.html; then
      cat  $Y_DIR/run/${DATE}/wrfvar/rsl/rsl.out.0000.html      >> ${FILE_RSLOUT}
   else
      echo " Please check file rsl.out.0000.html in " $Y_DIR/run/${DATE}/wrfvar
      exit 5       
   fi

   export DATE=`./da_advance_cymdh.exe $DATE $CYCLE_PERIOD`
done

# append ***** to file end
echo "*****" >> ${FILE_PERT}
echo "*****" >> ${FILE_YP}
echo "*****" >> ${FILE_Y}
echo "*****" >> ${FILE_JO}
echo "*****" >> ${FILE_RSLOUT}

./da_tune.exe > errfac.dat           

echo "da_tune.ksh completed"

exit 0

