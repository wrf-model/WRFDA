#!/bin/bash
#===========================================
RUN_DIR=/ptmp/rizvi/test_verif_plot
DIAG_DIR=/ptmp/rizvi/data/t44    
START_DATE=2006100100
END_DATE=2006102818
INTERVAL=06
Verify_Date_Range="01 - 28 October 2006 (${INTERVAL} hour Cycle)"
#---------------------------------------------------------------
NUM_EXPT=2
EXP_NAMES=( no_noise_t44 with_noise_t44 )
#EXP_NAMES=( verif_cold_start_12 verif_cycling_12 verif_cold_start_24 verif_cycling_24 )
#---------------------------------------------------------------
NUM_OBS_TYPE=4
OBS_TYPES=( "synop" "sound" "airep" "geoamv" )
#*************************************************************************************
#DESIRED_LEVELS='(/"1000","850","500","200","20","10"/)'
DESIRED_LEVELS='(/"850"/)'
DESIRED_SCORES='(/"RMSE","BIAS"/)'
EXP_LEGENDS='(/"No_noise","with_noise"/)'
#EXP_LEGENDS='(/"12 hr FCST (cold start)","12 hr FCST (cycling)", \
#               "24 hr FCST (cold start)","24 hr FCST (cycling)"/)' 
EXP_LINES_COLORS='(/"blue","green","orange"/)'
#EXP_LINES_COLORS='(/"blue","green","orange","red"/)'
#---------------------------------------------------------------
PLOT_WKS=x11
WRFVAR_DIR=/ptmp/rizvi/trunk
#=========================================================
#=========================================================
# BELOW THIS DO NOT CHANGE ANYTHING : CHANGE NOT REQUIRED
#=========================================================
#=========================================================
SRC_DIR=$WRFVAR_DIR/da/da_verif
DIAG_VAR="omb"
FILE_PATH_STRING=wrfvar/working/gts_omb_oma
cd ${SRC_DIR}
#make clean
#make
#=========================================================
mkdir -p ${RUN_DIR}
cd ${RUN_DIR}
rm -rf *
#=========================================================
iexp=0
exp_dirs=''
out_dirs=''
while [ "${iexp}" -lt "${NUM_EXPT}" ]
do
exp_dirs="$exp_dirs '${DIAG_DIR}/${EXP_NAMES[iexp]}/run',"
out_dirs="$out_dirs '$RUN_DIR/${EXP_NAMES[iexp]}',"
#-----------------------------------------
pdat_dirs[$iexp]="$RUN_DIR/${EXP_NAMES[iexp]}/"
#-----------------------------------------
mkdir -p $RUN_DIR/${EXP_NAMES[iexp]}
iexp=$((iexp + 1))
done
#-------------
# Making records 1 to 3 of namelist here
#-------------
cat > namelist.recs1to3 << EOF
&record1
 exp_num   = ${NUM_EXPT} ,
 exp_dirs  = ${exp_dirs}
 out_dirs  = ${out_dirs}
/
&record2
 start_date = '${START_DATE}',
 end_date   = '${END_DATE}',
 interval   = ${INTERVAL},
/
&record3
 if_plot_rmse = .TRUE.,
 if_plot_bias = .TRUE.,
 if_plot_abias = .TRUE.,
/
EOF
#--------------------------------------------------
# Making record 4 of namelist
#---------
iob=0
cat > record4 << EOR4_1
&record4
EOR4_1
#
while [ "$iob" -lt "$NUM_OBS_TYPE" ]
do
cat >> record4 << EOR4_2
 if_plot_${OBS_TYPES[iob]} = .TRUE.,
EOR4_2
iob=$((iob + 1))
done
cat >> record4 << EOR4_3
/
EOR4_3
#--------------------------------------------------
# Making record 5 of namelist
#---------
cat > record5 << EOR5
&record5
 file_path_string = '${FILE_PATH_STRING}',
/
EOR5
#------------
# Making full namelist file
#------------
cat namelist.recs1to3 record4 record5 > namelist.plot_diag
rm namelist.recs1to3 record4 record5
#--------------------------------------------------
#----------- Now run get_diag.exe ---------
ln -sf ${SRC_DIR}/src/get_diag.exe .
#
nfile=`ls *${DIAG_VAR}.diag | wc -l`
if [ $nfile -eq "0" ]; then
 ./get_diag.exe
  RC=$?
  if test $RC != 0; then
    echo " get_diag run is failed with error $RC"
    exit 1
  fi
else
  echo "Files already exists, skipping..."
  echo "If you want to compute diag again...."
  echo "Just delete files manually. then come again...."
#  exit 0
fi
#-----------------------------------------
#---- Create control file for NCL script
#-----------------------------------------
#cd $RUN_DIR/${EXP_NAMES[0]}
#echo $RUN_DIR/${EXP_NAMES[0]}
cd $RUN_DIR

echo "$Verify_Date_Range" > header_main
echo "$DIAG_VAR" >> header_main

#-----------------
#echo "${pdat_dirs[@]:0}" > ../dirnames
iexp=0
echo "${NUM_EXPT}" >> header_main
while [ $iexp -lt $NUM_EXPT ]
do
echo ${pdat_dirs[$iexp]} >> header_main
iexp=$((iexp + 1))
done
#-----------------
#declare -a ob_fnames
rm -f tmp_upr tmp_sfc
num_sfc=`ls ${pdat_dirs[0]}/surface*${DIAG_VAR}.diag |wc -l`
num_upr=`ls ${pdat_dirs[0]}/upr*${DIAG_VAR}.diag |wc -l`

cd ${pdat_dirs[0]}
for vn in u v t q p; do
  ls sur*${vn}*${DIAG_VAR}.diag >> ../tmp_sfc
  ls upr*${vn}*${DIAG_VAR}.diag >> ../tmp_upr
done
cd $RUN_DIR
 
if [ "$num_sfc" -lt 5 ]; then
   echo "All surface files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All surface files generated successfully.."
   echo "fnames_sfc" >> header_main

   anyfile=`head -1 "tmp_sfc"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > fnames_sfc
   echo $ncol >> fnames_sfc
   while read ob_fname
   do
     if [ "$ob_fname" = "surface_p_omb.diag" ]; then
        ob_unit='"hPa"'
     elif [ "$ob_fname" = "surface_t_omb.diag" ]; then
        ob_unit='T (~S~o~N~C)'
     elif [ "$ob_fname" = "surface_u_omb.diag" ]; then
        ob_unit='U (ms~S~-1~N~)' 
     elif [ "$ob_fname" = "surface_v_omb.diag" ]; then
        ob_unit='V (ms~S~-1~N~)' 
     elif [ "$ob_fname" = "surface_q_omb.diag" ]; then
        ob_unit='Q (gmKg~S~-1~N~)' 
     else
        echo "Unknown surface variable:-Don't know what to do??"
     fi
     echo "$ob_fname" >> fnames_sfc
     echo "$ob_unit" >> fnames_sfc
   done < tmp_sfc
fi
#----------------
if [ "$num_upr" -lt 4 ]; then
   echo "All upper-air files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All upper-air files generated successfully.."
   echo "fnames_upr" >> header_main    

   anyfile=`head -1 "tmp_upr"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > fnames_upr     
   echo $ncol >> fnames_upr     
   while read ob_fname
   do
     if [ "$ob_fname" = "upr_t_omb.diag" ]; then
        ob_unit='T (~S~o~N~C)'
     elif [ "$ob_fname" = "upr_u_omb.diag" ]; then
        ob_unit='U (ms~S~-1~N~)' 
     elif [ "$ob_fname" = "upr_v_omb.diag" ]; then
        ob_unit='V (ms~S~-1~N~)' 
     elif [ "$ob_fname" = "upr_q_omb.diag" ]; then
        ob_unit='Q (gmKg~S~-1~N~)' 
     else
        echo "Unknown upper-air variable:-Don't know what to do??"
     fi
     echo "${ob_fname}" >> fnames_upr
     echo "${ob_unit}" >> fnames_upr
   done < tmp_upr
fi
#----------------
#ob_fnames = ( `cat "$tmp_file"` )
#rm -f tmp_upr tmp_sfc
BAR_LABEL_ANGLE=45
#-----------------------------------------------------------------------------------------------------------------------
# Run NCL scripts now 
#-----------------------------------------------------------------------------------------------------------------------
NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${RUN_DIR}\"' 'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' 'select_levs=${DESIRED_LEVELS}' 'select_scores=${DESIRED_SCORES}' 'bar_label_angle=${BAR_LABEL_ANGLE}'"
echo "ncl ${NCL_COMMAND_LINE} ${SRC_DIR}/scripts/time_series.ncl" > run1
chmod +x run1
./run1
#rm -f run1
echo "ncl ${NCL_COMMAND_LINE} ${SRC_DIR}/scripts/time_average.ncl" > run2
chmod +x run2
./run2
#rm -f run2
echo "ncl ${NCL_COMMAND_LINE} ${SRC_DIR}/scripts/vert_profile.ncl" > run3
chmod +x run3
./run3
#rm -f run3
#----------------
#-----------------------------------------------------------------------------------------------------------------------
echo "successfully completed..."
#-----------------------------------------------------------------------------------------------------------------------
exit 0
