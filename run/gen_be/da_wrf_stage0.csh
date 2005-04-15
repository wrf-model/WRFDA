#! /bin/csh -xvf
#---------------------------------------------------------------
# Purpose : Generate WRF_DIFF 
#
# History : 10/01/2004                               Mi-Seon Lee
#---------------------------------------------------------------

 setenv WRF_DIR /mmmtmp1/guo/wrf3dvar
 setenv DAT_DIR /data1/guo/GEN_BE_data/ENS

 setenv RUN_DIR /mmmtmp1/guo/gen_be_ens
 mkdir -p $RUN_DIR
 
 set BE_TYPE =  2         # 1: NMC(Default),  2: ENS 
 set MEMBER  =  5         # (note) if BE_TYPE = 1, MEMBER should be 1

 setenv WEST_EAST_GRID_NUMBER   90
 setenv SOUTH_NORTH_GRID_NUMBER 90
 setenv VERTICAL_GRID_NUMBER    28
 setenv GRID_DISTANCE           100000

#set date
 set START_BE_DATE = 2002-01-01_00:00:00
 set END_BE_DATE   = 2002-01-04_00:00:00

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
  FILE_HEAD       = 'wrfout_d01',
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
 &namelist_01
 time_step_max                       = 1,
 max_dom                             = 1,
 dyn_opt                             = 2,
 rk_ord                              = 3,
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 1,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 num_soil_layers                     = 5,
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 tile_sz_x                           = 0,
 tile_sz_y                           = 0,
 numtiles                            = 1,
 debug_level                         = 0 /

 &namelist_02
 grid_id                             = 1,
 level                               = 1,
 s_we                                = 1,
 e_we                                = $WEST_EAST_GRID_NUMBER,
 s_sn                                = 1,
 e_sn                                = $SOUTH_NORTH_GRID_NUMBER,
 s_vert                              = 1,
 e_vert                              = $VERTICAL_GRID_NUMBER,
 time_step_count_output              = 36,
 frames_per_outfile                  = 10,
 time_step_count_restart             = 36,
 time_step_begin_restart             = 0,
 time_step_sound                     = 4 /

 &namelist_03
 dx                                  = $GRID_DISTANCE,
 dy                                  = $GRID_DISTANCE,
 dt                                  = 600.,
 ztop                                = 20000.0,
 zdamp                               = 5000.,
 dampcoef                            = 0.2,
 smdiv                               = 0.1,
 epssm                               = .1,
 khdif                               = 0,
 kvdif                               = 0,
 mix_cr_len                          = 200.,
 radt                                = 30,
 bldt                                = 0,
 cudt                                = 5,
 julyr                               = 0,
 julday                              = 1,
 gmt                                 = 0. /

 &namelist_04
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
 specified                           = .true.,
 top_radiation                       = .false.,
 chem_opt                            = 0,
 mp_physics                          = 3,
 ra_lw_physics                       = 1,
 ra_sw_physics                       = 1,
 bl_sfclay_physics                   = 1,
 bl_surface_physics                  = 1,
 bl_pbl_physics                      = 1,
 cu_physics                          = 1,
 h_mom_adv_order                     = 5,
 v_mom_adv_order                     = 3,
 h_sca_adv_order                     = 5,
 v_sca_adv_order                     = 3,
 io_form_history                     = 2,
 io_form_restart                     = 2,
 io_form_input                       = 2,
 io_form_boundary                    = 2 /

 &namelist_05
 start_year                          = $START_CY,
 start_month                         = $START_MM,
 start_day                           = $START_DD,
 start_hour                          = $START_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $END_CY,
 end_month                           = $END_MM,
 end_day                             = $END_DD,
 end_hour                            = $END_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 43200,
 real_data_init_type                 = 1 /

EOF

cd $RUN_DIR
ln -sf $DAT_DIR/wrfout_d01_2002-01-01_00:00:00.1 wrf_3dvar_input

ln -sf $WRF_DIR/run/LANDUSE.TBL LANDUSE.TBL
ln -sf $WRF_DIR/main/gen_be/gen_be_stage0.exe gen_be_stage0.exe

gen_be_stage0.exe >& wrf_diff.log

