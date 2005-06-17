#! /bin/csh -f
#---------------------------------------------------------------
# Purpose : Calculate standard difference fields from WRF input forecasts.
#
# History : 10/01/2004 Creation.                       Mi-Seon Lee
#           05/21/2005 Modify for inclusion in wrfvar  Dale Barker
#           06/06/2005 Modified.                       Y.-R. Guo            
#---------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo
setenv START_BE_DATE           2002-01-01_00:00:00
setenv END_BE_DATE             2002-01-10_00:00:00
setenv WEST_EAST_GRID_NUMBER    90
setenv SOUTH_NORTH_GRID_NUMBER  90
setenv VERTICAL_GRID_NUMBER     28
setenv GRID_DISTANCE           100000
setenv FILE_HEAD               'wrfout_d01'
setenv EXPT                    ENS
setenv DAT_DIR                 /mmm/mmmtmp/guo/GEN_BE_data/${EXPT}
setenv WRFVAR_DIR              /home/bluesky/guo/wrfvar

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "-------------------------------------------------------------------------------"
 echo "Run WRF Stage 0: Calculate standard difference fields from WRF input forecasts."
 echo "-------------------------------------------------------------------------------"

 set BEGIN_CPU  = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 if ( ! $?START_BE_DATE )           setenv START_BE_DATE 2004-05-01_00:00:00
 if ( ! $?END_BE_DATE )             setenv END_BE_DATE   2004-05-28_00:00:00
 if ( ! $?BE_TYPE )                 setenv BE_TYPE  2  # 1: NMC(Default),  2: ENS
 if ( $BE_TYPE == 1 )               setenv MEMBER   1  # Only 1 member for NMC method.
 if ( ! $?MEMBER )                  setenv MEMBER  50  # Needed if BE_TYPE /= 1. 
 if ( ! $?WEST_EAST_GRID_NUMBER )   setenv WEST_EAST_GRID_NUMBER   165
 if ( ! $?SOUTH_NORTH_GRID_NUMBER ) setenv SOUTH_NORTH_GRID_NUMBER 217
 if ( ! $?VERTICAL_GRID_NUMBER )    setenv VERTICAL_GRID_NUMBER    31
 if ( ! $?GRID_DISTANCE )           setenv GRID_DISTANCE           60000
 if ( ! $?DA_NUM_SOIL_LAYERS )      setenv DA_NUM_SOIL_LAYERS      5
 if ( ! $?FILE_HEAD )               setenv FILE_HEAD 'wrfout_d01',
 if ( ! $?EXPT )                    setenv EXPT amps1.60km.may04.NMC
 if ( ! $?WRFVAR_DIR )              setenv WRFVAR_DIR /tara/dmbarker/code_development/wrfvar
 if ( ! $?SRC_DIR )                 setenv SRC_DIR ${WRFVAR_DIR}/gen_be
 if ( ! $?DAT_DIR )                 setenv DAT_DIR /tara/dmbarker/be/amps_stats/${EXPT}
 if ( ! -d $DAT_DIR ) then
    echo "Directory $DAT_DIR doesn't exist. Exiting."
    exit
 endif
 if ( ! $?RUN_DIR )                 setenv RUN_DIR /ptmp/guo/TEST_ENS
 if ( ! -d ${RUN_DIR} )             mkdir ${RUN_DIR}
 
 set START_CY = `echo $START_BE_DATE | cut -c1-4`
 set START_MM = `echo $START_BE_DATE | cut -c6-7`
 set START_DD = `echo $START_BE_DATE | cut -c9-10`
 set START_HH = `echo $START_BE_DATE | cut -c12-13`

 set END_CY = `echo $END_BE_DATE | cut -c1-4`
 set END_MM = `echo $END_BE_DATE | cut -c6-7`
 set END_DD = `echo $END_BE_DATE | cut -c9-10`
 set END_HH = `echo $END_BE_DATE | cut -c12-13`

 cd $RUN_DIR

cat >! namelist.stats << EOF
 &FILERECD
  DIRECTORY_NAME  = '${DAT_DIR}',
  FILE_HEAD       = '${FILE_HEAD}',
  BGN_DATE        = '${START_BE_DATE}',
  END_DATE        = '${END_BE_DATE}',
  TEST_TRANSFORMS = .FALSE.,
  PRINT_DETAIL    = 0, 
  from_mss        = .FALSE.,
  mss_directory   = '/MSLEE/MODL/DAOU/ENS/TAR-NEW' /

 &TIMERECD
  T_FORECAST1 = 12,
  T_FORECAST2 = 24,
  FILE_INTERVAL = 12 /

 &ANALTYPE
  BACKGROUND_OPTION  = ${BE_TYPE},
  MEMBERS = ${MEMBER} /
EOF

cat >! namelist.input << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = 12,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $START_CY, $START_CY, $START_CY,
 start_month                         = $START_MM, $START_MM, $START_MM,
 start_day                           = $START_DD, $START_DD, $START_DD,
 start_hour                          = $START_HH, $START_HH, $START_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $START_CY, $START_CY, $START_CY,
 end_month                           = $START_MM, $START_MM, $START_MM,
 end_day                             = $START_DD, $START_DD, $START_DD,
 end_hour                            = $START_HH, $START_HH, $START_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 21600,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 180,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 10
 /

 &domains
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1, 1, 1,
 e_we                                = $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER,
 s_sn                                = 1, 1, 1,
 e_sn                                = $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER,
 s_vert                              = 1, 1, 1,
 e_vert                              = $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER,
 dx                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 dy                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 grid_id                             = 1,     2,     3,
 level                               = 1,     1,     2,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    30,
 j_parent_start                      = 0,     20,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0,
 nproc_y                             = 1,
 /

 &physics
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 10,    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 surface_input_source                = 1,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 num_soil_layers                     = 5,

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = 0,
 diff_opt                            = 0,
 km_opt                              = 1,
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 smdiv                               = 0.1,    0.1,    0.1,
 emdiv                               = 0.01,   0.01,   0.01,
 epssm                               = 0.1,    0.1,    0.1
 non_hydrostatic                     = .true., .true., .true.,
 time_step_sound                     = 4,      4,      4,
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
 real_data_init_type                 = 1,
/

 &var_test_control
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
EOF

#cp namelist.input.00 namelist.input

 ln -sf $DAT_DIR/${FILE_HEAD}_${START_BE_DATE}.1  wrf_3dvar_input

 ln -sf $WRFVAR_DIR/run/LANDUSE.TBL LANDUSE.TBL
 ln -sf $WRFVAR_DIR/gen_be/gen_be_stage0.exe gen_be_stage0.exe

 gen_be_stage0.exe >& wrf_diff.log

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

exit(0)

