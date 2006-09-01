#!/bin/ksh

# Run real

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export SOLVER=${SOLVER:-EM}
export DUMMY=${DUMMY:-false}
export NL_USE_HTML=${NL_USE_HTML:-false}
export CYCLE_PERIOD=${CYCLE_PERIOD:-06}
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export LBC_FREQ=${LBC_FREQ:-06}

export RUN_DIR=${RUN_DIR:-$EXP_DIR/real}
export WORK_DIR=$RUN_DIR/working
export OUT_DIR=${OUT_DIR:-$RUN_DIR/$DATE}

# Do we remove the WORK_DIR at the end to save space
export CLEAN=${CLEAN:-false}

rm -rf $WORK_DIR
mkdir -p $OUT_DIR $WORK_DIR
cd $WORK_DIR

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT real</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT real</H1><PRE>"
fi

date

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh $DATE $CYCLE_PERIOD

echo "Release directory:           $REL_DIR"
echo "WRF directory:               $WRF_DIR $WRF_REV"
echo "Working directory:           $WORK_DIR"
echo "Output directory:            $OUT_DIR"
echo "Start date:                  $DATE"
echo "End date:                    $END_DATE"

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600

export NL_AUXINPUT1_INNAME="met_em.d<domain>.<date>"

if test -f $WRF_DIR/inc/namelist_script.inc; then
   #Superior solution
   . $WRF_DIR/inc/namelist_script.inc
else
   cat > namelist.input <<EOF
 &time_control
 run_days                            = 0,
 run_hours                           = 12,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $NL_START_YEAR,
 start_month                         = $NL_START_MONTH,
 start_day                           = $NL_START_DAY,
 start_hour                          = $NL_START_HOUR,
 start_minute                        = $NL_START_MINUTE,
 start_second                        = $NL_START_SECOND,
 end_year                            = $NL_END_YEAR,
 end_month                           = $NL_END_MONTH,
 end_day                             = $NL_END_DAY,
 end_hour                            = $NL_END_HOUR,
 end_minute                          = $NL_END_MINUTE,
 end_second                          = $NL_END_SECOND,
 interval_seconds                    = $NL_INTERVAL_SECONDS
 input_from_file                     = .true.
 history_interval                    = 180,
 frames_per_outfile                  = 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 auxinput1_inname                    = "met_em.d<domain>.<date>"
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,
 e_we                                = $NL_E_WE,
 s_sn                                = 1,
 e_sn                                = $NL_E_SN,
 s_vert                              = 1,
 e_vert                              = $NL_E_VERT,
 num_metgrid_levels                  = $NL_NUM_METGRID_LEVELS,
 dx                                  = $NL_DX,
 dy                                  = $NL_DY,
 grid_id                             = 1,
 parent_id                           = 0,
 i_parent_start                      = 0,
 j_parent_start                      = 0,
 parent_grid_ratio                   = 1,
 parent_time_step_ratio              = 1,
 feedback                            = 1,
 smooth_option                       = 0
 /
 interp_type                         = 1
 lagrange_order                      = 1
 zap_close_levels                    = 500
 force_sfc_in_vinterp                = 6
 p_top_requested                     = 5000
 eta_levels                          = 1.000, 0.990, 0.978, 0.964, 0.946, 
                                       0.922, 0.894, 0.860, 0.817, 0.766, 
                                       0.707, 0.644, 0.576, 0.507, 0.444, 
                                       0.380, 0.324, 0.273, 0.228, 0.188, 
                                       0.152, 0.121, 0.093, 0.069, 0.048, 
                                       0.029, 0.014, 0.000, 
 eta_levels                          = 1.000, 0.993, 0.983, 0.970, 0.954,
                                       0.934, 0.909, 0.880, 0.845, 0.807,
                                       0.765, 0.719, 0.672, 0.622, 0.571,
                                       0.520, 0.468, 0.420, 0.376, 0.335,
                                       0.298, 0.263, 0.231, 0.202, 0.175,
                                       0.150, 0.127, 0.106, 0.088, 0.070,
                                       0.055, 0.040, 0.026, 0.013, 0.000

 &physics
 mp_physics                          = 3,  
 ra_lw_physics                       = 1,  
 ra_sw_physics                       = 1,  
 radt                                = 30, 
 sf_sfclay_physics                   = 1,  
 sf_surface_physics                  = 1,  
 bl_pbl_physics                      = 1,  
 bldt                                = 0,  
 cu_physics                          = 1,  
 cudt                                = 5,  
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = 0,
 diff_opt                            = 0,
 km_opt                              = 1,
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000., 
 dampcoef                            = 0.01,  
 khdif                               = 0,     
 kvdif                               = 0,     
 smdiv                               = 0.1,   
 emdiv                               = 0.01,  
 epssm                               = 0.1,   
 non_hydrostatic                     = .true.,
 time_step_sound                     = 4,     
 h_mom_adv_order                     = 5,     
 v_mom_adv_order                     = 3,     
 h_sca_adv_order                     = 5,     
 v_sca_adv_order                     = 3,     
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., 
 periodic_x                          = .false.,
 symmetric_xs                        = .false.,
 symmetric_xe                        = .false.,
 open_xs                             = .false.,
 open_xe                             = .false.,
 periodic_y                          = .false.,
 symmetric_ys                        = .false.,
 symmetric_ye                        = .false.,
 open_ys                             = .false.,
 open_ye                             = .false.,
 nested                              = .false.,
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
EOF
fi

typeset -l LC_SOLVER
LC_SOLVER=$SOLVER

cp namelist.input $OUT_DIR

if test ! -f $CS_DIR/$DATE/wrfinput_d${DOMAIN}; then
   if $DUMMY; then
      echo "Dummy real"
      echo Dummy real > wrfinput_d${DOMAIN}
      echo Dummy real > wrfbdy_d${DOMAIN}
      echo Dummy real > wrflowinp_d${DOMAIN}
   else
      ln -fs $CS_DIR/$DATE/met_em.d* .
      $RUN_CMD ${WRF_DIR}/main/real.exe
      RC=$?

      if test -f fort.9; then
        cp fort.9 $OUT_DIR/namelist.output
      fi

      mkdir -p $OUT_DIR/rsl
      mv rsl* $OUT_DIR/rsl
      if $NL_USE_HTML; then
         cd $OUT_DIR/rsl
         for FILE in rsl*; do
            echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
            echo "<H1>$FILE</H1><PRE>" >> $FILE.html
            cat $FILE >> $FILE.html
            echo "</PRE></BODY></HTML>" >> $FILE.html
            rm $FILE
         done
         cd $OUT_DIR

         echo '<A HREF="namelist.input">Namelist input</a>'
         echo '<A HREF="namelist.output">Namelist output</a>'
         echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
         echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
         echo '<A HREF="rsl">Other RSL output</a>'
      fi

      if test $RC = 0; then
         echo `date` "${OK}Succeeded${END}"
      else
         echo `date` "${ERR}Failed${END} with error $RC"
         exit 1
      fi    
   fi

   mv $WORK_DIR/wrfinput_d${DOMAIN} $CS_DIR/$DATE
   mv $WORK_DIR/wrfbdy_d${DOMAIN} $CS_DIR/$DATE
   mv $WORK_DIR/wrflowinp_d${DOMAIN} $CS_DIR/$DATE
else
   echo $CS_DIR/$DATE/wrfinput_d${DOMAIN} exists, skipping
fi

#rm -f ${MOAD_DATAROOT}/siprd/wrf_real_input_${LC_SOLVER}*
#rm -f ${WRF_DIR}/test/${LC_SOLVER}_real/wrf_real_input_${LC_SOLVER}*

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi
