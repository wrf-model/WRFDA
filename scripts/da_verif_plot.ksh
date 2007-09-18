#!/bin/ksh
set -x
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/verification}
export WORK_DIR=$RUN_DIR/working

export NUM_EXPT=${NUM_EXPT:-1}
export EXP_NAMES=${EXP_NAMES:-control expt}
export EXP_LEGENDS=${EXP_LEGENDS:-(/"no_noise","with_noise"/)}

export INTERVAL=${INTERVAL:-06}
export Verify_Date_Range="01 - 28 October 2006 (${INTERVAL} hour Cycle)"

export NUM_OBS_TYPE=${NUM_OBS_TYPES:-4}
export OBS_TYPES=${OBS_TYPES:-synop sound airep geoamv}

export DESIRED_LEVELS='(/"850"/)'
export DESIRED_SCORES='(/"RMSE","BIAS","ABIAS"/)'
export EXP_LINES_COLORS='(/"blue","green","orange"/)'

export PLOT_WKS=${PLOT_WKS:-x11}

#=========================================================
#=========================================================
# BELOW THIS DO NOT CHANGE ANYTHING : CHANGE NOT REQUIRED
#=========================================================
#=========================================================
DIAG_VAR="omb"
FILE_PATH_STRING=wrfvar/working/gts_omb_oma


rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

iexp=0
exp_dirs=''
out_dirs=''
for EXP in $EXP_NAMES; do
   exp_dirs="$exp_dirs '$REG_DIR/$EXP/run',"
   out_dirs="$out_dirs '$EXP',"
   pdat_dirs[$iexp]="$EXP/"
   mkdir -p $EXP
   iexp=$((iexp + 1))
done

cat > namelist.plot_diag << EOF
&record1
   exp_num   = ${NUM_EXPT} ,
   exp_dirs  = ${exp_dirs}
   out_dirs  = ${out_dirs} /
&record2
   start_date = '${START_DATE}',
   end_date   = '${END_DATE}',
   interval   = ${INTERVAL} /
&record3
   if_plot_rmse = .TRUE.,
   if_plot_bias = .TRUE.,
   if_plot_abias = .TRUE. /
EOF

cat >> namelist.plot_diag << EOF
&record4
EOF


for OBS_TYPE in $OBS_TYPES; do
   cat >> namelist.plot_diag << EOF
   if_plot_${OBS_TYPE} = .TRUE.,
EOF
done
echo '/' >> namelist.plot_diag

cat >> namelist.plot_diag << EOF
&record5
   file_path_string = '${FILE_PATH_STRING}' /
EOF

ln -sf ${WRFVAR_DIR}/build/da_verif.exe .

nfile=$(ls *${DIAG_VAR}.diag 2>/dev/null | wc -l)
if [[ $nfile -eq 0 ]]; then
   ./da_verif.exe > da_verif.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "da_verif.exe failed with error $RC"
      exit 1
   fi
else
   echo "Files already exists, skipping..."
   echo "If you want to compute diag again...."
   echo "Just delete files manually and re-run"
fi

# Create control file for NCL script

echo "$Verify_Date_Range" > header_main
echo "$DIAG_VAR" >> header_main

iexp=0
echo "${NUM_EXPT}" >> header_main
while [[ $iexp -lt $NUM_EXPT ]]; do
   echo ${pdat_dirs[$iexp]} >> header_main
   iexp=$((iexp + 1))
done

rm -f tmp_upr tmp_sfc
num_sfc=$(ls ${pdat_dirs[0]}/surface*${DIAG_VAR}.diag |wc -l)
num_upr=$(ls ${pdat_dirs[0]}/upr*${DIAG_VAR}.diag |wc -l)

cd ${pdat_dirs[0]}
for vn in u v t q p; do
   ls sur*${vn}*${DIAG_VAR}.diag >> ../tmp_sfc 2>/dev/null
done

for vn in u v t q; do
   ls upr*${vn}*${DIAG_VAR}.diag >> ../tmp_upr 2>/dev/null
done

cd ..
 
if [ "$num_sfc" -lt 5 ]; then
   echo "All surface files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All surface files generated successfully.."
   echo "fnames_sfc" >> header_main

   anyfile=$(head -1 "tmp_sfc")
   ncol=$(head -1 ${pdat_dirs[0]}/$anyfile |wc -w)
   nrow=$(cat ${pdat_dirs[0]}/$anyfile |wc -l)
   echo $nrow > fnames_sfc
   echo $ncol >> fnames_sfc
   while read ob_fname
   do
      if [[ "$ob_fname" == "surface_p_omb.diag" ]]; then
         ob_unit='"hPa"'
      elif [[ "$ob_fname" == "surface_t_omb.diag" ]]; then
         ob_unit='T (~S~o~N~C)'
      elif [[ "$ob_fname" == "surface_u_omb.diag" ]]; then
         ob_unit='U (ms~S~-1~N~)' 
      elif [[ "$ob_fname" == "surface_v_omb.diag" ]]; then
         ob_unit='V (ms~S~-1~N~)' 
      elif [[ "$ob_fname" == "surface_q_omb.diag" ]]; then
         ob_unit='Q (gmKg~S~-1~N~)' 
      else
         echo "Unknown surface variable:-Don't know what to do??"
      fi
      echo "$ob_fname" >> fnames_sfc
      echo "$ob_unit" >> fnames_sfc
   done < tmp_sfc
fi

if [[ "$num_upr" -lt 4 ]]; then
   echo "All upper-air files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All upper-air files generated successfully.."
   echo "fnames_upr" >> header_main    

   anyfile=$(head -1 "tmp_upr")
   ncol=$(head -1 ${pdat_dirs[0]}/$anyfile |wc -w)
   nrow=$(cat ${pdat_dirs[0]}/$anyfile |wc -l)
   echo $nrow > fnames_upr     
   echo $ncol >> fnames_upr     
   while read ob_fname; do
      if [[ "$ob_fname" == "upr_t_omb.diag" ]]; then
         ob_unit='T (~S~o~N~C)'
      elif [[ "$ob_fname" == "upr_u_omb.diag" ]]; then
         ob_unit='U (ms~S~-1~N~)' 
      elif [[ "$ob_fname" == "upr_v_omb.diag" ]]; then
         ob_unit='V (ms~S~-1~N~)' 
      elif [[ "$ob_fname" == "upr_q_omb.diag" ]]; then
         ob_unit='Q (gmKg~S~-1~N~)' 
      else
         echo "Unknown upper-air variable:-Don't know what to do??"
      fi
      echo "${ob_fname}" >> fnames_upr
      echo "${ob_unit}" >> fnames_upr
   done < tmp_upr
fi

BAR_LABEL_ANGLE=45

# Run NCL scripts now 

NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${PWD}\"' \
   'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' \
   'select_levs=${DESIRED_LEVELS}' 'select_scores=${DESIRED_SCORES}' 'bar_label_angle=${BAR_LABEL_ANGLE}'"

echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/time_series.ncl" > run1
chmod +x run1
./run1 > run1.log 2>&1

echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/time_average.ncl" > run2
chmod +x run2
./run2 > run2.log 2>&1

echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/vert_profile.ncl" > run3
chmod +x run3
./run3 > run3.log 2>&1

echo "<HTML><HEAD><TITLE>Verification Plots for $EXP_NAMES<TITLE></HEAD><BODY>" > index.html

echo 'WORK_DIR <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>' >> index.html

echo "<P>Output logs<UL>" >> index.html
for FILE in *.log; do
   if [[ -f $FILE ]]; then
      echo '<LI><A HREF="'$FILE'">'$FILE'</a>' >> index.html
   fi
done

echo "</UL>Plots<UL>" >> index.html

for FILE in *.pdf; do
   if [[ -f $FILE ]]; then
      echo '<LI><A HREF="'$FILE'">'$FILE'</a>' >> index.html
   fi
done

echo "</UL></BODY></HTML>" >> index.html

# Move useful files
mv *pdf *html $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

echo "da_verif_plot.ksh successfully completed..."

exit 0
