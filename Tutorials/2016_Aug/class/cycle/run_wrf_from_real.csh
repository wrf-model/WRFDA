#!/bin/csh -f
set echo
set DATE        = 2013122300
set BIN_DIR     = /kumquat/users/${USER}/DA/WRFDA/var/build
set RC_DATDIR   = /kumquat/wrfhelp/DATA/WRFDA/cycling/rc/
set WRF_SRCDIR  = /kumquat/users/wrfhelp/WRFV3/
set RUN_BASEDIR = /kumquat/users/${USER}/DA/cycling/run
set WRF_RUNDIR  = ${RUN_BASEDIR}/${DATE}/wrf
set FCST_RANGE  = 6
set LBC_FREQ    = 6
set MAX_DOM     = 1
set NPROC       = 6

if ( ! -d ${WRF_RUNDIR}} ) mkdir -p ${WRF_RUNDIR}
cd ${WRF_RUNDIR}

rm -f ${WRF_RUNDIR}/rsl.*
#
# link constant files
#
ln -fs ${WRF_SRCDIR}/run/RRTM_DATA ./RRTM_DATA
ln -fs ${WRF_SRCDIR}/run/RRTMG_LW_DATA ./RRTMG_LW_DATA
ln -fs ${WRF_SRCDIR}/run/RRTMG_SW_DATA ./RRTMG_SW_DATA
#ln -fs ${WRF_SRCDIR}/run/RRTM_DATA_DBL ./RRTM_DATA
ln -fs ${WRF_SRCDIR}/run/*.TBL .
ln -fs ${WRF_SRCDIR}/run/ozone* .

set cc = `echo $DATE | cut -c1-2`
set yy = `echo $DATE | cut -c3-4`
set mm = `echo $DATE | cut -c5-6`
set dd = `echo $DATE | cut -c7-8`
set hh = `echo $DATE | cut -c9-10`

@ LBC_FREQ_SEC = $LBC_FREQ * 3600

set START_DATE = `${BIN_DIR}/da_advance_time.exe $DATE 0 -w`
set END_DATE = `${BIN_DIR}/da_advance_time.exe $DATE $FCST_RANGE -w`
set ccyy_s = `echo $START_DATE | cut -c1-4`
set mm_s   = `echo $START_DATE | cut -c6-7`
set dd_s   = `echo $START_DATE | cut -c9-10`
set hh_s   = `echo $START_DATE | cut -c12-13`
set ccyy_e = `echo $END_DATE | cut -c1-4`
set mm_e   = `echo $END_DATE | cut -c6-7`
set dd_e   = `echo $END_DATE | cut -c9-10`
set hh_e   = `echo $END_DATE | cut -c12-13`
#
# create namelist.input
#
cat >&! namelist.input << EOF
 &time_control
 start_year                          = ${ccyy_s}, ${ccyy_s},
 start_month                         = ${mm_s},   ${mm_s},
 start_day                           = ${dd_s},   ${dd_s},
 start_hour                          = ${hh_s},   ${hh_s},
 start_minute                        = 00,   00,
 start_second                        = 00,   00,
 end_year                            = ${ccyy_e}, ${ccyy_e},
 end_month                           = ${mm_e},   ${mm_e},
 end_day                             = ${dd_e},   ${dd_e},
 end_hour                            = ${hh_e},   ${hh_e},
 end_minute                          = 00,   00,
 end_second                          = 00,   00,
 interval_seconds                    = 21600
 input_from_file                     = .true.,.true.,
 fine_input_stream                   = 0, 0
 history_interval                    = 360,  360,
 frames_per_outfile                  = 1, 1,
 restart                             = .false.,
 restart_interval                    = 100000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 inputout_interval                   = 360, 360,
 inputout_begin_h                    =   6, 6,
 inputout_end_h                      =   6, 6,
 /
 &dfi_control
 /
 &domains
 time_step                           = 300,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = ${MAX_DOM},
 s_we                                = 1,     1,
 e_we                                = 151,  181,
 s_sn                                = 1,     1,
 e_sn                                = 151,  121,
 s_vert                              = 1,     1,
 e_vert                              = 41,   41,
 num_metgrid_levels                  = 27,
 dx                                  = 60000, 30000,
 dy                                  = 60000, 30000,
 grid_id                             = 1,     2,
 parent_id                           = 0,     1,
 i_parent_start                      = 1,     21,
 j_parent_start                      = 1,     11,
 parent_grid_ratio                   = 1,     2,
 parent_time_step_ratio              = 1,     2,
 feedback                            = 1,
 smooth_option                       = 0
 p_top_requested                     = 5000
 /

 &physics
 mp_physics                          = 4,     4,
 ra_lw_physics                       = 4,     4,
 ra_sw_physics                       = 24,    24,
 radt                                = 30,    30,
 sf_sfclay_physics                   = 1,     1,
 sf_surface_physics                  = 2,     2,
 bl_pbl_physics                      = 1,     1,
 bldt                                = 0,     0,
 cu_physics                          = 1,     1,
 cudt                                = 5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 num_land_cat                        = 24,
 /
 &fdda
 /
 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 damp_opt                            = 0,
 base_temp                           = 270.,
 iso_temp                            = 200.,
 zdamp                               = 5000.,  5000.,
 dampcoef                            = 0.01,   0.01,
 khdif                               = 0,      0,
 kvdif                               = 0,      0,
 /
 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /
 &grib2
 /
 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
EOF

ln -fs ${WRF_SRCDIR}/main/wrf.exe .

ln -fs $RC_DATDIR/${DATE}/wrfbdy_d01 .
ln -fs $RC_DATDIR/${DATE}/wrfinput_d01 .
if ( ${MAX_DOM} == 2 ) then
   ln -fs $RC_DATDIR/${DATE}/wrfinput_d02 .
endif
if ( $NPROC > 1 ) then
   mpirun -np ${NPROC} ./wrf.exe
else
   time ./wrf.exe >&! wrf_${DATE}.log
endif

echo "Done run_wrf_from_real.csh `date`"
