#!/bin/ksh
#=======================================================================================
#  Purpose : Main script for processing and display of results for
#            verification against analysis or Forecasts
# or Forecasts
#  Author  :  Syed RH Rizvi,  NCAR/MMM    10/12/2007
#    Udates:
#              Syed RH Rizvi     NCAR/MMM         06/05/2009
#              a) Added verification for wind vector & geopotentials 
#              b) For vertical profiles, added the choice to display the desired
#                 number of pressure levels 
#--------------------------------------------------------------------------------------
#=======================================================================================

echo "<PRE>"

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
export GRAPHICS_DIR=${GRAPHICS_DIR:-$WRFVAR_DIR/var/graphics/ncl}
. ${SCRIPTS_DIR}/da_set_defaults.ksh

export PLOT_WKS=${PLOT_WKS:-pdf}
export RUN_VERIF_GRID_STATS=${RUN_VERIF_GRID_STATS:-true}
export RUN_VERIF_GRID_PLOTS=${RUN_VERIF_GRID_PLOTS:-true}
export DATA_DIR=${DATA_DIR:-${REG_DIR}}    
export NUM_EXPT=${NUM_EXPT:-2}
export CONTROL_EXP_DIR=${CONTROL_EXP_DIR:-${REG_DIR}/noda}                 
export EXP_DIRS=${EXP_DIRS:-${REG_DIR}/noda ${REG_DIR}/cycling_rad}
export EXP_NAMES=${EXP_NAMES:-"NODA" "CY RAD"}
export VERIFICATION_FILE_STRING=${VERIFICATION_FILE_STRING:-'wrfout'}
export EXP_LEGENDS=${EXP_LEGENDS:-'(/"noda","cy rad"/)'}

export INTERVAL=${INTERVAL:-12}
export VERIFY_HOUR=${VERIFY_HOUR:-12}
export START_DATE=${START_DATE:-2003010100}
export END_DATE=${END_DATE:-2003010100}

export RUN_DIR=${RUN_DIR:-$PWD}
export VERIFY_ITS_OWN_ANALYSIS=${VERIFY_ITS_OWN_ANALYSIS:-true}
export Verify_Date_Range=${Verify_Date_Range:-"$START_DATE - $END_DATE (${INTERVAL} hour Cycle)"}

export DOMAIN=${DOMAIN:-1}
export PLOT_WKS=${PLOT_WKS:-pdf}

#=======================================================================================
export DESIRED_LEVELS=${DESIRED_LEVELS:-'(/"850","500","200"/)'}
export TOP_HPA_LEVEL_FOR_VERT_PROFILES=${TOP_HPA_LEVEL_FOR_VERT_PROFILES:-50.0}
export DESIRED_SCORES=${DESIRED_SCORES:-'(/"RMSE","BIAS","ABIAS"/)'}
export EXP_LINES_COLORS=${EXP_LINES_COLORS:-'(/"blue","green", "orange"/)'}

export NUM3D=${NUM3D:-6}
export VAR3D=${VAR3D:-"U  V  WV T  Z QVAPOR"}
export NUM2D=${NUM2D:-6}
export VAR2D=${VAR2D:-"MU PSFC U10M V10M T2M Q2M"}

export ISTART=${ISTART:-1}
export IEND=${IEND:-10000}
export JSTART=${JSTART:-1}
export JEND=${JEND:-10000}

#--------------------------------------------------------------------------------------
#=========================================================
# BELOW THIS LINE NO CHANGES ARE REQUIRED                 
#=========================================================
export WORK_DIR=${RUN_DIR}/working
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}
export VERT_TYPE=${VERT_TYPE:-'p'}
#=========================================================
iexp=0
exp_dirs=''
out_dirs=''
for EXP_DIR in $EXP_DIRS; do
   exp_dirs="$exp_dirs '$EXP_DIR/fc/',"
done
for EXP_NAME in $EXP_NAMES; do
   out_dirs="$out_dirs '$EXP_NAME',"
   pdat_dirs[$iexp]="$EXP_NAME/"
   mkdir -p $EXP_NAME
   iexp=$((iexp + 1))
done

export code='"'
VAR3D_LISTS=''
for VAR3D_LIST in $VAR3D; do
   VAR3D_LISTS="$VAR3D_LISTS ${code}$VAR3D_LIST${code},"
done
VAR2D_LISTS=''
for VAR2D_LIST in $VAR2D; do
   VAR2D_LISTS="$VAR2D_LISTS ${code}$VAR2D_LIST${code},"
done
#--------------------------------------------------
#--------------------------------------------------
 SYEAR=`echo $START_DATE | cut -c1-4`
 SMONTH=`echo $START_DATE | cut -c5-6`
 SDAY=`echo $START_DATE | cut -c7-8`
 SHOUR=`echo $START_DATE | cut -c9-10`
#-------------
 EYEAR=`echo $END_DATE | cut -c1-4`
 EMONTH=`echo $END_DATE | cut -c5-6`
 EDAY=`echo $END_DATE | cut -c7-8`
 EHOUR=`echo $END_DATE | cut -c9-10`
#--------------------------------------------------
# Making records for namelist 
#--------------------------------------------------
cat > namelist.in << EOF
&control_main
 verify_its_own_analysis     = ${VERIFY_ITS_OWN_ANALYSIS},
 control_exp_dir             = '${CONTROL_EXP_DIR}/fc', 
 num_verifying_experiments   = ${NUM_EXPT},
 verif_dirs                  = ${exp_dirs}
 out_dirs                    = ${out_dirs}
 verify_forecast_hour        = ${VERIFY_HOUR},
 verification_file_string    = '${VERIFICATION_FILE_STRING}',
 domain                      = ${DOMAIN},
 vertical_type    = '${VERT_TYPE}',
/
&control_times
 start_year    = ${SYEAR},
 start_month   = ${SMONTH},
 start_day     = ${SDAY},
 start_hour    = ${SHOUR},
 end_year      = ${EYEAR},
 end_month     = ${EMONTH},
 end_day       = ${EDAY},
 end_hour      = ${EHOUR},
 interval_hour = ${INTERVAL},
/
&control_vars
 num3dvar = ${NUM3D},
 var3d    = ${VAR3D_LISTS}
 num2dvar = ${NUM2D},
 var2d    = ${VAR2D_LISTS}
/
&sub_domain
 istart = ${ISTART}
 iend   = ${IEND}
 jstart = ${JSTART}
 jend   = ${JEND}
/
EOF
#------------
#--------------------------------------------------
if $RUN_VERIF_GRID_STATS ; then
ln -sf $BUILD_DIR/da_verif_grid.exe .
#
./da_verif_grid.exe  
 RC=$?
 if test $RC != 0; then
   echo " da_verif_grid run failed with error $RC"
   exit 1
 fi

#-----------------------------------------------------------------------------------------------------------------------
echo "Generation og statistics successfully completed..."
#-----------------------------------------------------------------------------------------------------------------------
fi  # RUN_VERIF_GRID_STATS 

#--------------------------------------------------
if $RUN_VERIF_GRID_PLOTS ; then
#--------------------------------------------------
export BAR_LABEL_ANGLE=${BAR_LABEL_ANGLE:-45}
#=========================================================
# Now plotting starts
#=========================================================
iexp=0
pdat_dirs=''
for EXP_NAME in $EXP_NAMES; do
pdat_dirs[$iexp]="${WORK_DIR}/$EXP_NAME/"
iexp=$((iexp + 1))
done
#--------------------------------------------------
#---- Create control file for NCL script
#-----------------------------------------
DIAG_VAR=${DIAG_VAR:-time_series_$VERIFY_HOUR}

echo "$Verify_Date_Range" > header_main
#-----------------
#echo "${pdat_dirs[@]:0}" > ../dirnames
iexp=0
echo "${NUM_EXPT}" >> header_main
while [[ $iexp -lt $NUM_EXPT ]]; do
echo ${pdat_dirs[$iexp]} >> header_main
iexp=$((iexp + 1))
done
#-----------------
rm -f $WORK_DIR/tmp_sfc 
rm -f $WORK_DIR/tmp_upr 
export num_upr=$NUM3D
export num_sfc=$NUM2D

OLDPWD=$PWD
cd ${pdat_dirs[0]}
for vn in $VAR3D   ; do
  ls ${vn}_${DIAG_VAR} >> $WORK_DIR/tmp_upr
done
for vn in $VAR2D ; do
  ls ${vn}_${DIAG_VAR} >> $WORK_DIR/tmp_sfc
done

cd $OLDPWD
 
 
#----------------
   echo "fnames_upr" >> header_main    
   anyfile=`head -1 "$WORK_DIR/tmp_upr"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > $WORK_DIR/fnames_upr     
   echo $ncol >> $WORK_DIR/fnames_upr     

for vn in $VAR3D   ; do
     echo ${vn}_${DIAG_VAR} >> $WORK_DIR/fnames_upr
     if   [[ "$vn" = "T" ]]; then
        ob_unit='T (K)'
     elif [[ "$vn" = "U" ]]; then
        ob_unit='U (m/s)' 
     elif [[ "$vn" = "V" ]]; then
        ob_unit='V (m/s)' 
     elif [[ "$vn" = "QVAPOR" ]]; then
        ob_unit='Q (g/Kg)' 
     elif [[ "$vn" = "Z" ]]; then
        ob_unit='Z (gpm)' 
     elif [[ "$vn" = "WV" ]]; then
        ob_unit='WV (m/s)' 
     else
        echo "Unknown upper-air variable " $vn ":-Don't know what to do??"
     exit
     fi
     echo "${ob_unit}" >> $WORK_DIR/fnames_upr
done

#----------------

   echo "fnames_sfc" >> header_main    
   anyfile=`head -1 "$WORK_DIR/tmp_sfc"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > $WORK_DIR/fnames_sfc     
   echo $ncol >> $WORK_DIR/fnames_sfc     


for vn in $VAR2D   ; do
     echo ${vn}_${DIAG_VAR} >> $WORK_DIR/fnames_sfc
     if   [[ "$vn" = "SLP" ]]; then
        ob_unit='SLP (Pascal)'
     elif [[ "$vn" = "PSFC" ]]; then
        ob_unit='PSFC (Pascal)'
     elif [[ "$vn" = "U10M" ]]; then
        ob_unit='U10M (m/s)' 
     elif [[ "$vn" = "V10M" ]]; then
        ob_unit='V10M (m/s)' 
     elif [[ "$vn" = "WV" ]]; then
        ob_unit='WV (m/s)' 
     elif [[ "$vn" = "T2M" ]]; then
        ob_unit='T2M (K)' 
     elif [[ "$vn" = "Q2M" ]]; then
        ob_unit='Q2M (g/Kg)' 
     elif [[ "$vn" = "MU" ]]; then
        ob_unit='MU (Pascal)' 
     else
        echo "Unknown Surface variable " $vn ":-Don't know what to do??"
     exit
     fi
     echo "${ob_unit}" >> $WORK_DIR/fnames_sfc
done


#-----------------------------------------------------------------------------------------------------------------------
# Run NCL scripts now 
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
for LEVEL in ${DESIRED_LEVELS} ; do

NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${RUN_DIR}\"' 'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' 'select_levs=${LEVEL}' 'select_scores=${DESIRED_SCORES}'   'bar_label_angle=${BAR_LABEL_ANGLE}' 'verify_hour=${VERIFY_HOUR}' 'p_top=${TOP_HPA_LEVEL_FOR_VERT_PROFILES}'"

echo "ncl ${NCL_COMMAND_LINE} ${GRAPHICS_DIR}/verif_grid_time_series.ncl" > run1
chmod +x run1
./run1
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
echo "ncl ${NCL_COMMAND_LINE} ${GRAPHICS_DIR}/verif_grid_time_average.ncl" > run2
chmod +x run2
./run2
#-----------------------------------------------------------------------------------------------------------------------
done
#-----------------------------------------------------------------------------------------------------------------------
echo "ncl ${NCL_COMMAND_LINE} ${GRAPHICS_DIR}/verif_grid_vert_profile.ncl" > run3
chmod +x run3
./run3
#-----------------------------------------------------------------------------------------------------------------------

mv $WORK_DIR/*.pdf $RUN_DIR
#-----------------------------------------------------------------------------------------------------------------------
echo "Ploting successfully completed..."
#-----------------------------------------------------------------------------------------------------------------------
fi #RUN_VERIF_GRID_PLOTS 
exit 0
