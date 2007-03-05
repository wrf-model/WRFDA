#!/bin/ksh
#set -x
#=========================================================
RUN_DIR=/mmmtmp/rizvi/verify
DIAG_DIR=/mmmtmp/rizvi/input_gts_omb_oma
START_DATE=2005080200
END_DATE=2005083112
INTERVAL=12
Verify_Date_Range="02 - 31 August 2005 (${INTERVAL} hour Cycle)"
#---------------------------------------------------------------
NUM_EXPT=4
EXP_NAMES=( verify_an verify_00 verify_12 verify_24 )
NUM_OBS_TYPE=3
OBS_TYPES=( "synop" "sound" "airep" )
DESIRED_LEVELS='(/"850","500","200","10"/)'
DESIRED_SCORES='(/"RMSE","BIAS"/)'
EXP_LEGENDS='(/"WRF-Var","FNL (FG)","12 hour","24 hour"/)'
EXP_LINES_COLORS='(/"blue","green","orange","red"/)'
PLOT_WKS=pdf
WRFVAR_DIR=/mmmtmp/rizvi/trunk         
#=========================================================
# BELOW THIS DO NOT CHANGE ANYTHING : CHANGE NOT REQUIRED
#=========================================================
SRC_DIR=$WRFVAR_DIR/da/da_verif    
DIAG_VAR="omb"
cd ${SRC_DIR}
make clean
make
#=========================================================
mkdir -p ${RUN_DIR}
cd ${RUN_DIR}
rm -rf *
#=========================================================
iexp=0
while [ "${iexp}" -lt "${NUM_EXPT}" ]
do
expt=${EXP_NAMES[iexp]}
EXP_DIR=$RUN_DIR/$expt
mkdir -p $EXP_DIR 

cd $EXP_DIR
ln -fs ${SRC_DIR}/src/get_diag.exe .
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
#-------------
# Making records 1 to 3 of namelist here
#-------------
cat > namelist.recs1to3 << EOF
&record1
 diag_dir  = '${DIAG_DIR}',
 exp_names = '${expt}'
/
&record2
 start_date = '${START_DATE}'
 end_date   = '${END_DATE}'
 interval   = ${INTERVAL}
/
&record3
 if_plot_rmse = .TRUE.,
 if_plot_bias = .TRUE.,
 if_plot_abias = .TRUE.,
/
EOF
#------------
# Making full namelist file
#------------
cat namelist.recs1to3 record4 > namelist.plot_diag
rm namelist.recs1to3 record4
#--------------------------------------------------
#----------- Now run get_diag.exe ---------
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
pdat_dirs[$iexp]="$RUN_DIR/$expt/"
#-----------------------------------------
iexp=$((iexp + 1))
done
#-----------------------------------------
#---- Create control file for NCL script
#-----------------------------------------
cd $RUN_DIR/${EXP_NAMES[0]}
echo $RUN_DIR/${EXP_NAMES[0]}

echo "$Verify_Date_Range" > ../header_main
echo "$DIAG_VAR" >> ../header_main

#-----------------
#echo "${pdat_dirs[@]:0}" > ../dirnames
iexp=0
echo "${NUM_EXPT}" >> ../header_main
while [ $iexp -lt $NUM_EXPT ]
do
echo ${pdat_dirs[$iexp]} >> ../header_main
iexp=$((iexp + 1))
done
#-----------------
#declare -a ob_fnames
num_sfc=`ls surface*${DIAG_VAR}.diag |wc -l`
num_upr=`ls upr*${DIAG_VAR}.diag |wc -l`

ls surface*${DIAG_VAR}.diag |sort -r > ../tmp_sfc
ls upr*${DIAG_VAR}.diag |sort -r > ../tmp_upr
 
if [ "$num_sfc" -lt 5 ]; then
   echo "All surface files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All surface files generated successfully.."
   echo "fnames_sfc" >> ../header_main

   anyfile=`head -1 "../tmp_sfc"`
   ncol=`head -1 $anyfile |wc -w`
   nrow=`cat $anyfile |wc -l`
   echo $nrow > ../fnames_sfc
   echo $ncol >> ../fnames_sfc
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
     echo "$ob_fname" >> ../fnames_sfc
     echo "$ob_unit" >> ../fnames_sfc
   done < ../tmp_sfc
fi
#----------------
if [ "$num_upr" -lt 4 ]; then
   echo "All upper-air files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All upper-air files generated successfully.."
   echo "fnames_upr" >> ../header_main    

   anyfile=`head -1 "../tmp_upr"`
   ncol=`head -1 $anyfile |wc -w`
   nrow=`cat $anyfile |wc -l`
   echo $nrow > ../fnames_upr     
   echo $ncol >> ../fnames_upr     
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
     echo "${ob_fname}" >> ../fnames_upr
     echo "${ob_unit}" >> ../fnames_upr
   done < ../tmp_upr
fi
#----------------
#ob_fnames = ( `cat "$tmp_file"` )
rm -f ../tmp_upr ../tmp_sfc
#----------------
cd $RUN_DIR
NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${RUN_DIR}\"' 'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' 'select_levs=${DESIRED_LEVELS}' 'select_scores=${DESIRED_SCORES}'"
echo "ncl ${NCL_COMMAND_LINE} ${SRC_DIR}/scripts/time_series.ncl" > run
chmod +x run
./run
rm -f run
echo "ncl ${NCL_COMMAND_LINE} ${SRC_DIR}/scripts/vert_profile.ncl" > run
chmod +x run
./run
rm -f run
#----------------
echo "successfully completed..."
exit 0
