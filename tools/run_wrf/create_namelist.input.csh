#! /bin/csh -f
#-----------------------------------------------------------------------
# Script create_namelist.input.csh
#
# Purpose: Creates namelist info for real/WRF-Var/WRF runs.
# Note 1: Environment variables are assumed set in calling script.
# Note 2: namelist.input file is created in current directory.
#
#-----------------------------------------------------------------------

#set echo

cd ${WRF_DIR}/test/em_real

setenv LBC_FREQ_SS `expr $LBC_FREQ \* 3600`

if ( $DA_SF_SURFACE_PHYSICS == 1 ) setenv DA_NUM_SOIL_LAYERS 5 # (Thermal diffusion surface physics).
if ( $DA_SF_SURFACE_PHYSICS == 2 ) setenv DA_NUM_SOIL_LAYERS 4 # (Noah LSM surface physics).

cat >! namelist.input << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = ${FCST_RANGE},
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${START_YEAR}, ${START_YEAR}, ${START_YEAR},
 start_month                         = ${START_MONTH}, ${START_MONTH}, ${START_MONTH},
 start_day                           = ${START_DAY}, ${START_DAY}, ${START_DAY},
 start_hour                          = ${START_HOUR}, ${START_HOUR}, ${START_HOUR},
 start_minute                        = 00,   00,   00,
 start_second                        = 00,   00,   00,
 end_year                            = ${END_YEAR}, ${END_YEAR}, ${END_YEAR},
 end_month                           = ${END_MONTH}, ${END_MONTH}, ${END_MONTH},
 end_day                             = ${END_DAY}, ${END_DAY}, ${END_DAY},
 end_hour                            = ${END_HOUR}, ${END_HOUR}, ${END_HOUR},
 end_minute                          = 00,   00,   00,
 end_second                          = 00,   00,   00,
 interval_seconds                    = ${LBC_FREQ_SS}
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = ${WRF_HIST_INT},   60,   60,
 frames_per_outfile                  = 1,   12,   12,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = ${DEBUG_LEVEL}

 write_input                         = .true.,
 inputout_interval                   = ${WRITE_INPUT_FREQ},
 inputout_begin_y                    = 0, 0, 0,
 inputout_begin_mo                   = 0, 0, 0,
 inputout_begin_d                    = 0, 0, 0,
 inputout_begin_h                    = 0, 0, 0,
 inputout_begin_m                    = 0, 0, 0,
 inputout_begin_s                    = 0, 0, 0,
 inputout_end_y                      = 0, 0, 0,
 inputout_end_mo                     = 0, 0, 0,
 inputout_end_d                      = 0, 0, 0,
 inputout_end_h                      = ${FCST_RANGE}, 0, 0,
 inputout_end_m                      = 0, 0, 0,
 inputout_end_s                      = 0, 0, 0,
 input_outname                       = 'wrf_3dvar_input_d<domain>_<date>',

 /

 &domains
 time_step                           = ${WRF_DT},
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,   1,   1,
 e_we                                = ${XDIM}, 220, 79,
 s_sn                                = 1,   1,   1,
 e_sn                                = ${YDIM}, 208, 94,
 s_vert                              = 1,   1,   1,
 e_vert                              = ${ZDIM},  32,  32,
 dx                                  = ${DELTA_X}, 30000, 10000,
 dy                                  = ${DELTA_Y}, 30000, 10000,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    30,
 j_parent_start                      = 0,     20,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = ${SMOOTH_OPTION}
 /

 &physics
 mp_physics                          = ${DA_MP_PHYSICS},     2,     2,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = ${DA_RADT},    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = ${DA_SF_SURFACE_PHYSICS},     2,     2,     2,     2,     2,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 mp_zero_out                         = ${MP_ZERO_OUT},
 mp_zero_out_thresh                  = 1.e-8,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = ${DA_W_DAMPING},
 diff_opt                            = ${DA_DIFF_OPT},
 km_opt                              = ${DA_KM_OPT},
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = ${DA_DAMPCOEF},   0.01,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 smdiv                               = 0.1,    0.1,    0.1,
 emdiv                               = 0.01,   0.01,   0.01,
 epssm                               = 0.1,    0.1,    0.1
 non_hydrostatic                     = .true., .true., .true.,
 time_step_sound                     = ${DA_TIME_STEP_SOUND},      4,      4,
 h_mom_adv_order                     = 5,      5,      5,
 v_mom_adv_order                     = 3,      3,      3,
 h_sca_adv_order                     = 5,      5,      5,
 v_sca_adv_order                     = 3,      3,      3,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 periodic_x                          = .false.,.false.,.false.,
 symmetric_xs                        = .false.,.false.,.false.,
 symmetric_xe                        = .false.,.false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 symmetric_ys                        = .false.,.false.,.false.,
 symmetric_ye                        = .false.,.false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
EOF

exit (0)

