#! /usr/local/bin/tcsh -f

 echo ""
 echo "Running script tune.csh"
 echo ""

# setup env variables
#-------------------------
 setenv USER rizvi
 setenv WORKDIR       /ptmp/$USER/work_tune_gts
 setenv DIR_PREFIX    /ptmp/$USER/katrina_48km
 setenv START_DATE    2005080200
 setenv   END_DATE    2005080200
 setenv CYCLE_PERIOD  6
 setenv DIR_YP        with_noise  # perturbered run
 setenv DIR_Y         no_noise       # normal run
 setenv OUTPUT        wrfvar/working
 setenv FILE_PERT     45                    # fort.45,  random perturbation
 setenv FILE_YP       46                    # fort.46,  perturbed y=Hdx
 setenv FILE_Y        47                    # fort.47,   y=Hdx
 setenv FILE_JO       48                    # fort.48,   Jo
 setenv FILE_RSLOUT   49                    # fort.49,  rsl.out.0000
 setenv NUM_PROCS_START 0
 setenv NUM_PROCS       8

 rm -rf $WORKDIR; mkdir $WORKDIR; cd $WORKDIR
 cp $HOME/trunk-old/wrfvar/da/da_util/da_tune.f90  .
 cp $HOME/trunk-old/wrfvar/main/advance_cymdh.exe .
 
cat > namelist.radiance << EOF
&rtminit
 rtminit_nsensor     = 0,
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


# clean old files
#----------------------
 rm -f fort.*; touch fort.${FILE_PERT} fort.${FILE_YP} fort.${FILE_Y} 

# loop for date
#----------------------
while ( $START_DATE <= $END_DATE )

echo $START_DATE

# cat fort.45?? into fort.45 from different processors
#-----------------------------------------------------
set START_PROC = ${FILE_PERT}${NUM_PROCS_START}
set   END_PROC = ${FILE_PERT}${NUM_PROCS}
while ( $START_PROC <= $END_PROC )
 if ( -s ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/${OUTPUT}/fort.${START_PROC} ) then
   cat ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/${OUTPUT}/fort.${START_PROC} >> fort.${FILE_PERT}
 endif
 set START_PROC=`expr ${START_PROC} \+ 1`
end

# cat fort.46?? into fort.46 from different processors
#-----------------------------------------------------
set START_PROC = ${FILE_YP}${NUM_PROCS_START}
set   END_PROC = ${FILE_YP}${NUM_PROCS}
while ( $START_PROC <= $END_PROC )
 if ( -s ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/${OUTPUT}/fort.${START_PROC} ) then
   cat ${DIR_PREFIX}/${DIR_YP}/${START_DATE}/${OUTPUT}/fort.${START_PROC} >> fort.${FILE_YP}
 endif
 set START_PROC=`expr ${START_PROC} \+ 1`
end

# cat fort.47?? into fort.47 from different processors
#-----------------------------------------------------
set START_PROC = ${FILE_Y}${NUM_PROCS_START}
set   END_PROC = ${FILE_Y}${NUM_PROCS}
while ( $START_PROC <= $END_PROC )
 if ( -s ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/${OUTPUT}/fort.${START_PROC} ) then
   cat ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/${OUTPUT}/fort.${START_PROC} >> fort.${FILE_Y}
 endif
 set START_PROC=`expr ${START_PROC} \+ 1`
end

# cat fort.48 into one file
#
 cat  ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/${OUTPUT}/fort.${FILE_JO} >> fort.${FILE_JO}
 
# cat rsl.out.0000 into fort.49
#
 cat  ${DIR_PREFIX}/${DIR_Y}/${START_DATE}/wrfvar/rsl/rsl.out.0000.html  >> fort.${FILE_RSLOUT}

 setenv START_DATE `./advance_cymdh.exe $START_DATE $CYCLE_PERIOD`
end

# cat ***** to file end

echo "*****" >> fort.${FILE_PERT}
echo "*****" >> fort.${FILE_YP}
echo "*****" >> fort.${FILE_Y}
echo "*****" >> fort.${FILE_JO}
echo "*****" >> fort.${FILE_RSLOUT}

#  ./da_tune.exe > test_tuning_amsu.dat
 ./da_tune.exe > test_tuning_gts.dat

 echo "da_tune.csh completed"

 exit (0)

