#! /usr/local/bin/tcsh -f
#-------------------------------------------------------------------------------------------
# Script for Observation error tuning  (Desroziers method)
#   Ref: QJRMS (2001), 127, pp. 1433-1452,
#        Gerald Desroziers and Serguei Ivanov
#
#                10/16/2006          Syed RH Rizvi    
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
 echo "Running script tune.csh"
 echo ""

# setup env variables
#-------------------------------------------------------------------------------------------
 setenv USER rizvi
 setenv WRFVAR_DIR    $HOME/trunk/wrfvar
 setenv WORKDIR       /ptmp/$USER/work_tune
 setenv DIR_PREFIX    /ptmp/$USER/katrina_48km
 setenv START_DATE    2005080106
 setenv   END_DATE    2005081500
 setenv CYCLE_PERIOD  6
 setenv DIR_YP        with_noise  # perturbered run
 setenv DIR_Y         no_noise    # unperturbered run
 setenv FILE_PERT     fort.45     # fort.45,  random perturbation
 setenv FILE_YP       fort.46     # fort.46,  perturbed y=Hdx
 setenv FILE_Y        fort.47     # fort.47,   y=Hdx
 setenv FILE_JO       fort.48     # fort.48,   Jo
 setenv FILE_RSLOUT   fort.49     # fort.49,  rsl.out.0000

 rm -rf $WORKDIR; mkdir $WORKDIR; cd $WORKDIR
 cp $WRFVAR_DIR/da/da_util/da_tune.f90  .
 cp $HOME/advance_cymdh.exe .
 
#-------------------------------------------------------------------------------------------
cat > namelist.radiance << EOF
&rtminit
 rtminit_nsensor     = 2,
 rtminit_platform    =  1,1,
 rtminit_satid       =  15,16,
 rtminit_sensor      =  3,3,
 rtminit_nchan       = 15,15 /
EOF

 echo ""
 echo "Compiling da_tune.f90"
 echo ""

# f90 -r8 -o tune.exe tune.f90
 xlf90 -qrealsize=8 -o da_tune.exe da_tune.f90


 touch fort.45 fort.46 fort.47 fort.48 fort.49 
# loop for date
#-------------------------------------------------------------------------------------------
while ( $START_DATE <= $END_DATE )

echo $START_DATE

#-------------------------------------------------------------------------------------------
   if ( -s ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working/rand_obs_error) then
   cat ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working/rand_obs_error  >> $FILE_PERT        
   else
   echo " Please check file (rand_obs) in " ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working
   exit 1             
   endif

#-------------------------------------------------------------------------------------------
   if ( -s ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working/pert_obs) then                  
   cat ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working/pert_obs       >> $FILE_YP        
   else
   echo " Please check file(pert_obs) in " ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/wrfvar/working
   exit 2         
   endif

#-------------------------------------------------------------------------------------------
   if ( -s ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working/unpert_obs) then
   cat ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working/unpert_obs      >> $FILE_Y        
   else
   echo " Please check file(unpert_obs) in " ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working
   exit 3          
   endif

#-------------------------------------------------------------------------------------------
   if( -s  ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working/jo      ) then
   cat ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working/jo                >> $FILE_JO       
   else
   echo " Please check file (fort.48) in " ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/working
   exit 4          
   endif
#
#-------------------------------------------------------------------------------------------
#
   if ( -s ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/rsl/rsl.out.0000.html ) then
   cat  ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/rsl/rsl.out.0000.html      >> ${FILE_RSLOUT}
   else
   echo " Please check file (rsl.out.0000.html) in " ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar
   exit 5       
   endif

 setenv START_DATE `./advance_cymdh.exe $START_DATE $CYCLE_PERIOD`
end

#-------------------------------------------------------------------------------------------
# cat ***** to file end
echo "*****" >> ${FILE_PERT}
echo "*****" >> ${FILE_YP}
echo "*****" >> ${FILE_Y}
echo "*****" >> ${FILE_JO}
echo "*****" >> ${FILE_RSLOUT}

 ./da_tune.exe > test_tuning_gts.dat

 echo "da_tune.csh completed"
#-------------------------------------------------------------------------------------------

 exit (0)

