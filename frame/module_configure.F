!WRF:DRIVER_LAYER:CONFIGURATION
!
MODULE module_configure

   USE module_driver_constants
   USE module_state_description
   USE module_wrf_error

   TYPE model_config_rec_type
      SEQUENCE
! Statements that declare namelist variables are in this file
! Note that the namelist is SEQUENCE and generated such that the first item is an
! integer, first_item_in_struct and the last is an integer last_item_in_struct
! this provides a way of converting this to a buffer for passing to and from
! the driver.

!STARTOFREGISTRYGENERATEDINCLUDE inc/namelist_defines.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
logical , DIMENSION(max_domains) :: input_from_hires
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
integer , DIMENSION(max_domains) :: frames_per_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist5
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
character*256 :: history_outname
character*256 :: auxhist1_outname
character*256 :: auxhist2_outname
character*256 :: auxhist3_outname
character*256 :: auxhist4_outname
character*256 :: auxhist5_outname
character*256 :: history_inname
character*256 :: auxhist1_inname
character*256 :: auxhist2_inname
character*256 :: auxhist3_inname
character*256 :: auxhist4_inname
character*256 :: auxhist5_inname
character*256 :: auxinput1_outname
character*256 :: auxinput2_outname
character*256 :: auxinput3_outname
character*256 :: auxinput4_outname
character*256 :: auxinput5_outname
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
integer , DIMENSION(max_domains) :: history_interval_mo
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_mo
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer , DIMENSION(max_domains) :: auxhist1_interval_mo
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist2_interval_mo
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist3_interval_mo
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist4_interval_mo
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist5_interval_mo
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxinput1_interval_mo
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput2_interval_mo
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput3_interval_mo
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput4_interval_mo
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput5_interval_mo
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer :: restart_interval_mo
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_mo
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_mo
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_mo
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_mo
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_mo
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_mo
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_mo
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_mo
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_mo
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_mo
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_mo
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_mo
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer :: restart_begin_y
integer :: restart_begin_mo
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_mo
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_mo
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_mo
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_mo
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_mo
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_mo
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_mo
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_mo
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_mo
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_mo
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_mo
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_mo
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
logical :: write_input
logical :: write_restart_at_0h
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: level
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer , DIMENSION(max_domains) :: coral_dist
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: num_moves
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical :: test_rk_step_prep
logical :: test_init_zero_tendency
logical :: test_phy_prep
logical :: test_calculate_km_kh
logical :: test_rk_tendency
logical :: test_advect_scalar
logical :: test_relax_bdy_dry
logical :: test_rk_addtend_dry
logical :: test_spec_bdy_dry
logical :: test_small_step_prep
logical :: test_calc_p_rho
logical :: test_calc_coef_w
logical :: test_advance_uv
logical :: test_spec_bdyupdate
logical :: test_advance_mu_t
logical :: test_sumflux
logical :: test_advance_w
logical :: test_spec_bdyupdate_ph
logical :: test_calc_mu_uv_1
logical :: test_small_step_finish
logical :: test_rk_scalar_tend
logical :: test_spec_bdy_scalar
logical :: test_rk_update_scalar
logical :: test_calc_p_rho_phi
logical :: test_diagnose_w
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: surface_input_source
integer :: num_soil_layers
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: mix_cr_len
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: kh_tke_upper_bound
real , DIMENSION(max_domains) :: kv_tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical , DIMENSION(max_domains) :: mix_full_fields
real :: base_pres
real :: base_temp
real :: base_lapse
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
real , DIMENSION(max_domains) :: bdyfrq
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE
   END TYPE model_config_rec_type

   TYPE grid_config_rec_type

!STARTOFREGISTRYGENERATEDINCLUDE inc/namelist_defines2.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer :: start_year
integer :: start_month
integer :: start_day
integer :: start_hour
integer :: start_minute
integer :: start_second
integer :: end_year
integer :: end_month
integer :: end_day
integer :: end_hour
integer :: end_minute
integer :: end_second
integer :: interval_seconds
logical :: input_from_file
logical :: input_from_hires
integer :: history_interval
integer :: frames_per_outfile
integer :: frames_per_auxhist1
integer :: frames_per_auxhist2
integer :: frames_per_auxhist3
integer :: frames_per_auxhist4
integer :: frames_per_auxhist5
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
character*256 :: history_outname
character*256 :: auxhist1_outname
character*256 :: auxhist2_outname
character*256 :: auxhist3_outname
character*256 :: auxhist4_outname
character*256 :: auxhist5_outname
character*256 :: history_inname
character*256 :: auxhist1_inname
character*256 :: auxhist2_inname
character*256 :: auxhist3_inname
character*256 :: auxhist4_inname
character*256 :: auxhist5_inname
character*256 :: auxinput1_outname
character*256 :: auxinput2_outname
character*256 :: auxinput3_outname
character*256 :: auxinput4_outname
character*256 :: auxinput5_outname
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
integer :: history_interval_mo
integer :: history_interval_d
integer :: history_interval_h
integer :: history_interval_m
integer :: history_interval_s
integer :: inputout_interval_mo
integer :: inputout_interval_d
integer :: inputout_interval_h
integer :: inputout_interval_m
integer :: inputout_interval_s
integer :: inputout_interval
integer :: auxhist1_interval_mo
integer :: auxhist1_interval_d
integer :: auxhist1_interval_h
integer :: auxhist1_interval_m
integer :: auxhist1_interval_s
integer :: auxhist1_interval
integer :: auxhist2_interval_mo
integer :: auxhist2_interval_d
integer :: auxhist2_interval_h
integer :: auxhist2_interval_m
integer :: auxhist2_interval_s
integer :: auxhist2_interval
integer :: auxhist3_interval_mo
integer :: auxhist3_interval_d
integer :: auxhist3_interval_h
integer :: auxhist3_interval_m
integer :: auxhist3_interval_s
integer :: auxhist3_interval
integer :: auxhist4_interval_mo
integer :: auxhist4_interval_d
integer :: auxhist4_interval_h
integer :: auxhist4_interval_m
integer :: auxhist4_interval_s
integer :: auxhist4_interval
integer :: auxhist5_interval_mo
integer :: auxhist5_interval_d
integer :: auxhist5_interval_h
integer :: auxhist5_interval_m
integer :: auxhist5_interval_s
integer :: auxhist5_interval
integer :: auxinput1_interval_mo
integer :: auxinput1_interval_d
integer :: auxinput1_interval_h
integer :: auxinput1_interval_m
integer :: auxinput1_interval_s
integer :: auxinput1_interval
integer :: auxinput2_interval_mo
integer :: auxinput2_interval_d
integer :: auxinput2_interval_h
integer :: auxinput2_interval_m
integer :: auxinput2_interval_s
integer :: auxinput2_interval
integer :: auxinput3_interval_mo
integer :: auxinput3_interval_d
integer :: auxinput3_interval_h
integer :: auxinput3_interval_m
integer :: auxinput3_interval_s
integer :: auxinput3_interval
integer :: auxinput4_interval_mo
integer :: auxinput4_interval_d
integer :: auxinput4_interval_h
integer :: auxinput4_interval_m
integer :: auxinput4_interval_s
integer :: auxinput4_interval
integer :: auxinput5_interval_mo
integer :: auxinput5_interval_d
integer :: auxinput5_interval_h
integer :: auxinput5_interval_m
integer :: auxinput5_interval_s
integer :: auxinput5_interval
integer :: restart_interval_mo
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer :: history_begin_y
integer :: history_begin_mo
integer :: history_begin_d
integer :: history_begin_h
integer :: history_begin_m
integer :: history_begin_s
integer :: inputout_begin_y
integer :: inputout_begin_mo
integer :: inputout_begin_d
integer :: inputout_begin_h
integer :: inputout_begin_m
integer :: inputout_begin_s
integer :: auxhist1_begin_y
integer :: auxhist1_begin_mo
integer :: auxhist1_begin_d
integer :: auxhist1_begin_h
integer :: auxhist1_begin_m
integer :: auxhist1_begin_s
integer :: auxhist2_begin_y
integer :: auxhist2_begin_mo
integer :: auxhist2_begin_d
integer :: auxhist2_begin_h
integer :: auxhist2_begin_m
integer :: auxhist2_begin_s
integer :: auxhist3_begin_y
integer :: auxhist3_begin_mo
integer :: auxhist3_begin_d
integer :: auxhist3_begin_h
integer :: auxhist3_begin_m
integer :: auxhist3_begin_s
integer :: auxhist4_begin_y
integer :: auxhist4_begin_mo
integer :: auxhist4_begin_d
integer :: auxhist4_begin_h
integer :: auxhist4_begin_m
integer :: auxhist4_begin_s
integer :: auxhist5_begin_y
integer :: auxhist5_begin_mo
integer :: auxhist5_begin_d
integer :: auxhist5_begin_h
integer :: auxhist5_begin_m
integer :: auxhist5_begin_s
integer :: auxinput1_begin_y
integer :: auxinput1_begin_mo
integer :: auxinput1_begin_d
integer :: auxinput1_begin_h
integer :: auxinput1_begin_m
integer :: auxinput1_begin_s
integer :: auxinput2_begin_y
integer :: auxinput2_begin_mo
integer :: auxinput2_begin_d
integer :: auxinput2_begin_h
integer :: auxinput2_begin_m
integer :: auxinput2_begin_s
integer :: auxinput3_begin_y
integer :: auxinput3_begin_mo
integer :: auxinput3_begin_d
integer :: auxinput3_begin_h
integer :: auxinput3_begin_m
integer :: auxinput3_begin_s
integer :: auxinput4_begin_y
integer :: auxinput4_begin_mo
integer :: auxinput4_begin_d
integer :: auxinput4_begin_h
integer :: auxinput4_begin_m
integer :: auxinput4_begin_s
integer :: auxinput5_begin_y
integer :: auxinput5_begin_mo
integer :: auxinput5_begin_d
integer :: auxinput5_begin_h
integer :: auxinput5_begin_m
integer :: auxinput5_begin_s
integer :: restart_begin_y
integer :: restart_begin_mo
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: history_end_y
integer :: history_end_mo
integer :: history_end_d
integer :: history_end_h
integer :: history_end_m
integer :: history_end_s
integer :: inputout_end_y
integer :: inputout_end_mo
integer :: inputout_end_d
integer :: inputout_end_h
integer :: inputout_end_m
integer :: inputout_end_s
integer :: auxhist1_end_y
integer :: auxhist1_end_mo
integer :: auxhist1_end_d
integer :: auxhist1_end_h
integer :: auxhist1_end_m
integer :: auxhist1_end_s
integer :: auxhist2_end_y
integer :: auxhist2_end_mo
integer :: auxhist2_end_d
integer :: auxhist2_end_h
integer :: auxhist2_end_m
integer :: auxhist2_end_s
integer :: auxhist3_end_y
integer :: auxhist3_end_mo
integer :: auxhist3_end_d
integer :: auxhist3_end_h
integer :: auxhist3_end_m
integer :: auxhist3_end_s
integer :: auxhist4_end_y
integer :: auxhist4_end_mo
integer :: auxhist4_end_d
integer :: auxhist4_end_h
integer :: auxhist4_end_m
integer :: auxhist4_end_s
integer :: auxhist5_end_y
integer :: auxhist5_end_mo
integer :: auxhist5_end_d
integer :: auxhist5_end_h
integer :: auxhist5_end_m
integer :: auxhist5_end_s
integer :: auxinput1_end_y
integer :: auxinput1_end_mo
integer :: auxinput1_end_d
integer :: auxinput1_end_h
integer :: auxinput1_end_m
integer :: auxinput1_end_s
integer :: auxinput2_end_y
integer :: auxinput2_end_mo
integer :: auxinput2_end_d
integer :: auxinput2_end_h
integer :: auxinput2_end_m
integer :: auxinput2_end_s
integer :: auxinput3_end_y
integer :: auxinput3_end_mo
integer :: auxinput3_end_d
integer :: auxinput3_end_h
integer :: auxinput3_end_m
integer :: auxinput3_end_s
integer :: auxinput4_end_y
integer :: auxinput4_end_mo
integer :: auxinput4_end_d
integer :: auxinput4_end_h
integer :: auxinput4_end_m
integer :: auxinput4_end_s
integer :: auxinput5_end_y
integer :: auxinput5_end_mo
integer :: auxinput5_end_d
integer :: auxinput5_end_h
integer :: auxinput5_end_m
integer :: auxinput5_end_s
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer :: julyr
integer :: julday
real :: gmt
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
logical :: write_input
logical :: write_restart_at_0h
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: max_dom
integer :: s_we
integer :: e_we
integer :: s_sn
integer :: e_sn
integer :: s_vert
integer :: e_vert
real :: dx
real :: dy
integer :: grid_id
integer :: parent_id
integer :: level
integer :: i_parent_start
integer :: j_parent_start
integer :: parent_grid_ratio
integer :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real :: ztop
integer :: moad_grid_ratio
integer :: moad_time_step_ratio
integer :: shw
integer :: coral_dist
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
real :: dt
integer :: num_moves
integer :: move_id
integer :: move_interval
integer :: move_cd_x
integer :: move_cd_y
logical :: test_rk_step_prep
logical :: test_init_zero_tendency
logical :: test_phy_prep
logical :: test_calculate_km_kh
logical :: test_rk_tendency
logical :: test_advect_scalar
logical :: test_relax_bdy_dry
logical :: test_rk_addtend_dry
logical :: test_spec_bdy_dry
logical :: test_small_step_prep
logical :: test_calc_p_rho
logical :: test_calc_coef_w
logical :: test_advance_uv
logical :: test_spec_bdyupdate
logical :: test_advance_mu_t
logical :: test_sumflux
logical :: test_advance_w
logical :: test_spec_bdyupdate_ph
logical :: test_calc_mu_uv_1
logical :: test_small_step_finish
logical :: test_rk_scalar_tend
logical :: test_spec_bdy_scalar
logical :: test_rk_update_scalar
logical :: test_calc_p_rho_phi
logical :: test_diagnose_w
integer :: mp_physics
integer :: ra_lw_physics
integer :: ra_sw_physics
real :: radt
integer :: sf_sfclay_physics
integer :: sf_surface_physics
integer :: bl_pbl_physics
real :: bldt
integer :: cu_physics
real :: cudt
real :: gsmdt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: surface_input_source
integer :: num_soil_layers
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real :: zdamp
real :: dampcoef
real :: khdif
real :: kvdif
real :: smdiv
real :: emdiv
real :: epssm
logical :: non_hydrostatic
integer :: time_step_sound
integer :: h_mom_adv_order
integer :: v_mom_adv_order
integer :: h_sca_adv_order
integer :: v_sca_adv_order
logical :: top_radiation
real :: mix_cr_len
real :: tke_upper_bound
real :: kh_tke_upper_bound
real :: kv_tke_upper_bound
real :: tke_drag_coefficient
real :: tke_heat_flux
logical :: pert_coriolis
logical :: mix_full_fields
real :: base_pres
real :: base_temp
real :: base_lapse
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical :: specified
logical :: periodic_x
logical :: symmetric_xs
logical :: symmetric_xe
logical :: open_xs
logical :: open_xe
logical :: periodic_y
logical :: symmetric_ys
logical :: symmetric_ye
logical :: open_ys
logical :: open_ye
logical :: nested
integer :: real_data_init_type
real :: cen_lat
real :: cen_lon
real :: truelat1
real :: truelat2
real :: moad_cen_lat
real :: stand_lon
real :: bdyfrq
integer :: iswater
integer :: isice
integer :: isurban
integer :: isoilwater
integer :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE
   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec


!STARTOFREGISTRYGENERATEDINCLUDE inc/scalar_tables.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  INTEGER :: moist_index_table( param_num_moist, max_domains )
  INTEGER :: moist_num_table( max_domains )
  INTEGER :: chem_index_table( param_num_chem, max_domains )
  INTEGER :: chem_num_table( max_domains )
!ENDOFREGISTRYGENERATEDINCLUDE

! special entries (put here but not enshrined in Registry for one reason or other)

   CHARACTER (LEN=4) :: mminlu = '    '         ! character string for landuse table

   PRIVATE read_namelist_data

CONTAINS


! Model layer, even though it does I/O -- special case of namelist I/O.

   SUBROUTINE initial_config
!<DESCRIPTION>
! This routine reads in the namelist.input file and sets
! module_config_rec, a structure of TYPE(model_config_rec_type), which is is seen via USE association by any
! subprogram that uses module_configure.  The module_config_rec structure
! contains all namelist settings for all domains.  Variables that apply
! to the entire run and have only one value regardless of domain are
! scalars.  Variables that allow different settings for each domain are
! defined as arrays of dimension max_domains (defined in
! frame/module_driver_constants.F, from a setting passed in from
! configure.wrf). There is another type in WRF, TYPE(grid_config_rec_type), in which
! all fields pertain only to a single domain (and are all scalars). The subroutine
! model_to_grid_config_rec(), also in frame/module_configure.F, is used to retrieve
! the settings for a given domain from a TYPE(module_config_rec_type) and put them into
! a TYPE(grid_config_rec_type), variables of which type are often called <em>config_flags</em>
! in the WRF code.
! 
! Most of the code in this routine is generated from the Registry file
! rconfig entries and included from the following files (found in the inc directory):
! 
! <pre>
! namelist_defines.inc	declarations of namelist variables (local to this routine)
! namelist_statements.inc	NAMELIST statements for each variable
! namelist_defaults.inc	assignment to default values if specified in Registry
! config_reads.inc		read statements for each namelist record
! config_assigns.inc	assign each variable to field in module_config_rec
! </pre>
!
!NOTE: generated subroutines from Registry file rconfig entries are renamed nl_
! instead of rconfig_ due to length limits for subroutine names.
!
! Note for version WRF 2.0: there is code here to force all domains to
! have the same mp_physics setting. This is because different mp_physics
! packages have different numbers of tracers but the nest forcing and
! feedback code relies on the parent and nest having the same number and
! kind of tracers. This means that the microphysics option
! specified on the highest numbered domain is the microphysics
! option for <em>all</em> domains in the run. This will be revisited.
! 
!</DESCRIPTION>
      IMPLICIT NONE

      INTEGER              :: io_status, nml_unit
      INTEGER              :: i

! define as temporaries

!STARTOFREGISTRYGENERATEDINCLUDE inc/namelist_defines.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
logical , DIMENSION(max_domains) :: input_from_hires
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
integer , DIMENSION(max_domains) :: frames_per_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist5
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
character*256 :: history_outname
character*256 :: auxhist1_outname
character*256 :: auxhist2_outname
character*256 :: auxhist3_outname
character*256 :: auxhist4_outname
character*256 :: auxhist5_outname
character*256 :: history_inname
character*256 :: auxhist1_inname
character*256 :: auxhist2_inname
character*256 :: auxhist3_inname
character*256 :: auxhist4_inname
character*256 :: auxhist5_inname
character*256 :: auxinput1_outname
character*256 :: auxinput2_outname
character*256 :: auxinput3_outname
character*256 :: auxinput4_outname
character*256 :: auxinput5_outname
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
integer , DIMENSION(max_domains) :: history_interval_mo
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_mo
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer , DIMENSION(max_domains) :: auxhist1_interval_mo
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist2_interval_mo
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist3_interval_mo
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist4_interval_mo
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist5_interval_mo
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxinput1_interval_mo
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput2_interval_mo
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput3_interval_mo
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput4_interval_mo
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput5_interval_mo
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer :: restart_interval_mo
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_mo
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_mo
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_mo
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_mo
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_mo
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_mo
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_mo
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_mo
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_mo
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_mo
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_mo
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_mo
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer :: restart_begin_y
integer :: restart_begin_mo
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_mo
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_mo
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_mo
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_mo
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_mo
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_mo
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_mo
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_mo
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_mo
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_mo
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_mo
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_mo
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
logical :: write_input
logical :: write_restart_at_0h
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: level
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer , DIMENSION(max_domains) :: coral_dist
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: num_moves
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical :: test_rk_step_prep
logical :: test_init_zero_tendency
logical :: test_phy_prep
logical :: test_calculate_km_kh
logical :: test_rk_tendency
logical :: test_advect_scalar
logical :: test_relax_bdy_dry
logical :: test_rk_addtend_dry
logical :: test_spec_bdy_dry
logical :: test_small_step_prep
logical :: test_calc_p_rho
logical :: test_calc_coef_w
logical :: test_advance_uv
logical :: test_spec_bdyupdate
logical :: test_advance_mu_t
logical :: test_sumflux
logical :: test_advance_w
logical :: test_spec_bdyupdate_ph
logical :: test_calc_mu_uv_1
logical :: test_small_step_finish
logical :: test_rk_scalar_tend
logical :: test_spec_bdy_scalar
logical :: test_rk_update_scalar
logical :: test_calc_p_rho_phi
logical :: test_diagnose_w
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: surface_input_source
integer :: num_soil_layers
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: mix_cr_len
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: kh_tke_upper_bound
real , DIMENSION(max_domains) :: kv_tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical , DIMENSION(max_domains) :: mix_full_fields
real :: base_pres
real :: base_temp
real :: base_lapse
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
real , DIMENSION(max_domains) :: bdyfrq
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE

! Statements that specify the namelists

!STARTOFREGISTRYGENERATEDINCLUDE inc/namelist_statements.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ input_from_hires
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ parent_id
NAMELIST /domains/ level
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ coral_dist
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ test_rk_step_prep
NAMELIST /domains/ test_init_zero_tendency
NAMELIST /domains/ test_phy_prep
NAMELIST /domains/ test_calculate_km_kh
NAMELIST /domains/ test_rk_tendency
NAMELIST /domains/ test_advect_scalar
NAMELIST /domains/ test_relax_bdy_dry
NAMELIST /domains/ test_rk_addtend_dry
NAMELIST /domains/ test_spec_bdy_dry
NAMELIST /domains/ test_small_step_prep
NAMELIST /domains/ test_calc_p_rho
NAMELIST /domains/ test_calc_coef_w
NAMELIST /domains/ test_advance_uv
NAMELIST /domains/ test_spec_bdyupdate
NAMELIST /domains/ test_advance_mu_t
NAMELIST /domains/ test_sumflux
NAMELIST /domains/ test_advance_w
NAMELIST /domains/ test_spec_bdyupdate_ph
NAMELIST /domains/ test_calc_mu_uv_1
NAMELIST /domains/ test_small_step_finish
NAMELIST /domains/ test_rk_scalar_tend
NAMELIST /domains/ test_spec_bdy_scalar
NAMELIST /domains/ test_rk_update_scalar
NAMELIST /domains/ test_calc_p_rho_phi
NAMELIST /domains/ test_diagnose_w
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ mix_cr_len
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ kh_tke_upper_bound
NAMELIST /dynamics/ kv_tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ mix_full_fields
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
!ENDOFREGISTRYGENERATEDINCLUDE

      OPEN ( UNIT   = 10               ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3 ( "module_configure.b" , 96 ,  'ERROR OPENING namelist.input' )
      ENDIF

      nml_unit = 10

! Statements that set the namelist vars to default vals

!STARTOFREGISTRYGENERATEDINCLUDE inc/namelist_defaults.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
run_days = 0
run_hours = 0
run_minutes = 0
run_seconds = 0
start_year = 1993
start_month = 03
start_day = 13
start_hour = 12
start_minute = 00
start_second = 00
end_year = 1993
end_month = 03
end_day = 14
end_hour = 12
end_minute = 00
end_second = 00
interval_seconds = 43200
input_from_file = .false.
input_from_hires = .false.
history_interval = 0
frames_per_outfile = 10
frames_per_auxhist1 = 10
frames_per_auxhist2 = 1
frames_per_auxhist3 = 1
frames_per_auxhist4 = 10
frames_per_auxhist5 = 10
restart = .false.
restart_interval = 0
io_form_input = 2
io_form_history = 2
io_form_restart = 2
io_form_boundary = 2
debug_level = 0
history_outname = "wrfout_d<domain>_<date>"
auxhist1_outname = "auxhist1_d<domain>_<date>"
auxhist2_outname = "auxhist2_d<domain>_<date>"
auxhist3_outname = "auxhist3_d<domain>_<date>"
auxhist4_outname = "auxhist4_d<domain>_<date>"
auxhist5_outname = "auxhist5_d<domain>_<date>"
history_inname = "wrfhist_d<domain>_<date>"
auxhist1_inname = "auxhist1_d<domain>_<date>"
auxhist2_inname = "auxhist2_d<domain>_<date>"
auxhist3_inname = "auxhist3_d<domain>_<date>"
auxhist4_inname = "auxhist4_d<domain>_<date>"
auxhist5_inname = "auxhist5_d<domain>_<date>"
auxinput1_outname = "auxinput1_d<domain>_<date>"
auxinput2_outname = "auxinput2_d<domain>_<date>"
auxinput3_outname = "auxinput3_d<domain>_<date>"
auxinput4_outname = "auxinput4_d<domain>_<date>"
auxinput5_outname = "auxinput5_d<domain>_<date>"
auxinput1_inname = "auxinput1_d<domain>"
auxinput2_inname = "auxinput2_d<domain>"
auxinput3_inname = "auxinput3_d<domain>"
auxinput4_inname = "auxinput4_d<domain>"
auxinput5_inname = "auxinput5_d<domain>"
history_interval_mo = 0
history_interval_d = 0
history_interval_h = 0
history_interval_m = 0
history_interval_s = 0
inputout_interval_mo = 0
inputout_interval_d = 0
inputout_interval_h = 0
inputout_interval_m = 0
inputout_interval_s = 0
inputout_interval = 0
auxhist1_interval_mo = 0
auxhist1_interval_d = 0
auxhist1_interval_h = 0
auxhist1_interval_m = 0
auxhist1_interval_s = 0
auxhist1_interval = 0
auxhist2_interval_mo = 0
auxhist2_interval_d = 0
auxhist2_interval_h = 0
auxhist2_interval_m = 0
auxhist2_interval_s = 0
auxhist2_interval = 0
auxhist3_interval_mo = 0
auxhist3_interval_d = 0
auxhist3_interval_h = 0
auxhist3_interval_m = 0
auxhist3_interval_s = 0
auxhist3_interval = 0
auxhist4_interval_mo = 0
auxhist4_interval_d = 0
auxhist4_interval_h = 0
auxhist4_interval_m = 0
auxhist4_interval_s = 0
auxhist4_interval = 0
auxhist5_interval_mo = 0
auxhist5_interval_d = 0
auxhist5_interval_h = 0
auxhist5_interval_m = 0
auxhist5_interval_s = 0
auxhist5_interval = 0
auxinput1_interval_mo = 0
auxinput1_interval_d = 0
auxinput1_interval_h = 0
auxinput1_interval_m = 0
auxinput1_interval_s = 0
auxinput1_interval = 0
auxinput2_interval_mo = 0
auxinput2_interval_d = 0
auxinput2_interval_h = 0
auxinput2_interval_m = 0
auxinput2_interval_s = 0
auxinput2_interval = 0
auxinput3_interval_mo = 0
auxinput3_interval_d = 0
auxinput3_interval_h = 0
auxinput3_interval_m = 0
auxinput3_interval_s = 0
auxinput3_interval = 0
auxinput4_interval_mo = 0
auxinput4_interval_d = 0
auxinput4_interval_h = 0
auxinput4_interval_m = 0
auxinput4_interval_s = 0
auxinput4_interval = 0
auxinput5_interval_mo = 0
auxinput5_interval_d = 0
auxinput5_interval_h = 0
auxinput5_interval_m = 0
auxinput5_interval_s = 0
auxinput5_interval = 0
restart_interval_mo = 0
restart_interval_d = 0
restart_interval_h = 0
restart_interval_m = 0
restart_interval_s = 0
history_begin_y = 0
history_begin_mo = 0
history_begin_d = 0
history_begin_h = 0
history_begin_m = 0
history_begin_s = 0
inputout_begin_y = 0
inputout_begin_mo = 0
inputout_begin_d = 0
inputout_begin_h = 0
inputout_begin_m = 0
inputout_begin_s = 0
auxhist1_begin_y = 0
auxhist1_begin_mo = 0
auxhist1_begin_d = 0
auxhist1_begin_h = 0
auxhist1_begin_m = 0
auxhist1_begin_s = 0
auxhist2_begin_y = 0
auxhist2_begin_mo = 0
auxhist2_begin_d = 0
auxhist2_begin_h = 0
auxhist2_begin_m = 0
auxhist2_begin_s = 0
auxhist3_begin_y = 0
auxhist3_begin_mo = 0
auxhist3_begin_d = 0
auxhist3_begin_h = 0
auxhist3_begin_m = 0
auxhist3_begin_s = 0
auxhist4_begin_y = 0
auxhist4_begin_mo = 0
auxhist4_begin_d = 0
auxhist4_begin_h = 0
auxhist4_begin_m = 0
auxhist4_begin_s = 0
auxhist5_begin_y = 0
auxhist5_begin_mo = 0
auxhist5_begin_d = 0
auxhist5_begin_h = 0
auxhist5_begin_m = 0
auxhist5_begin_s = 0
auxinput1_begin_y = 0
auxinput1_begin_mo = 0
auxinput1_begin_d = 0
auxinput1_begin_h = 0
auxinput1_begin_m = 0
auxinput1_begin_s = 0
auxinput2_begin_y = 0
auxinput2_begin_mo = 0
auxinput2_begin_d = 0
auxinput2_begin_h = 0
auxinput2_begin_m = 0
auxinput2_begin_s = 0
auxinput3_begin_y = 0
auxinput3_begin_mo = 0
auxinput3_begin_d = 0
auxinput3_begin_h = 0
auxinput3_begin_m = 0
auxinput3_begin_s = 0
auxinput4_begin_y = 0
auxinput4_begin_mo = 0
auxinput4_begin_d = 0
auxinput4_begin_h = 0
auxinput4_begin_m = 0
auxinput4_begin_s = 0
auxinput5_begin_y = 0
auxinput5_begin_mo = 0
auxinput5_begin_d = 0
auxinput5_begin_h = 0
auxinput5_begin_m = 0
auxinput5_begin_s = 0
restart_begin_y = 0
restart_begin_mo = 0
restart_begin_d = 0
restart_begin_h = 0
restart_begin_m = 0
restart_begin_s = 0
history_end_y = 0
history_end_mo = 0
history_end_d = 0
history_end_h = 0
history_end_m = 0
history_end_s = 0
inputout_end_y = 0
inputout_end_mo = 0
inputout_end_d = 0
inputout_end_h = 0
inputout_end_m = 0
inputout_end_s = 0
auxhist1_end_y = 0
auxhist1_end_mo = 0
auxhist1_end_d = 0
auxhist1_end_h = 0
auxhist1_end_m = 0
auxhist1_end_s = 0
auxhist2_end_y = 0
auxhist2_end_mo = 0
auxhist2_end_d = 0
auxhist2_end_h = 0
auxhist2_end_m = 0
auxhist2_end_s = 0
auxhist3_end_y = 0
auxhist3_end_mo = 0
auxhist3_end_d = 0
auxhist3_end_h = 0
auxhist3_end_m = 0
auxhist3_end_s = 0
auxhist4_end_y = 0
auxhist4_end_mo = 0
auxhist4_end_d = 0
auxhist4_end_h = 0
auxhist4_end_m = 0
auxhist4_end_s = 0
auxhist5_end_y = 0
auxhist5_end_mo = 0
auxhist5_end_d = 0
auxhist5_end_h = 0
auxhist5_end_m = 0
auxhist5_end_s = 0
auxinput1_end_y = 0
auxinput1_end_mo = 0
auxinput1_end_d = 0
auxinput1_end_h = 0
auxinput1_end_m = 0
auxinput1_end_s = 0
auxinput2_end_y = 0
auxinput2_end_mo = 0
auxinput2_end_d = 0
auxinput2_end_h = 0
auxinput2_end_m = 0
auxinput2_end_s = 0
auxinput3_end_y = 0
auxinput3_end_mo = 0
auxinput3_end_d = 0
auxinput3_end_h = 0
auxinput3_end_m = 0
auxinput3_end_s = 0
auxinput4_end_y = 0
auxinput4_end_mo = 0
auxinput4_end_d = 0
auxinput4_end_h = 0
auxinput4_end_m = 0
auxinput4_end_s = 0
auxinput5_end_y = 0
auxinput5_end_mo = 0
auxinput5_end_d = 0
auxinput5_end_h = 0
auxinput5_end_m = 0
auxinput5_end_s = 0
io_form_auxinput1 = 2
io_form_auxinput2 = 2
io_form_auxinput3 = 2
io_form_auxinput4 = 2
io_form_auxinput5 = 2
io_form_auxhist1 = 2
io_form_auxhist2 = 2
io_form_auxhist3 = 2
io_form_auxhist4 = 2
io_form_auxhist5 = 2
julyr = 0
julday = 1
gmt = 0.
input_inname = "wrfinput_d<domain>"
input_outname = "wrfinput_d<domain>"
bdy_inname = "wrfbdy_d<domain>"
bdy_outname = "wrfbdy_d<domain>"
rst_inname = "wrfrst_d<domain>_<date>"
rst_outname = "wrfrst_d<domain>_<date>"
write_input = .false.
write_restart_at_0h = .false.
time_step_fract_num = 0
time_step_fract_den = 1
max_dom = 1
s_we = 1
e_we = 32
s_sn = 1
e_sn = 32
s_vert = 1
e_vert = 31
dx = 200
dy = 200
grid_id = 1
parent_id = 0
level = 1
i_parent_start = 1
j_parent_start = 1
parent_grid_ratio = 1
parent_time_step_ratio = 1
feedback = 1
smooth_option = 2
ztop = 15000.
moad_grid_ratio = 1
moad_time_step_ratio = 1
shw = 2
coral_dist = 8
tile_sz_x = 0
tile_sz_y = 0
numtiles = 1
nproc_x = -1
nproc_y = -1
irand = 0
dt = 2.
num_moves = 0
move_id = 0
move_interval = 999999999
move_cd_x = 0
move_cd_y = 0
test_rk_step_prep = .false.
test_init_zero_tendency = .false.
test_phy_prep = .false.
test_calculate_km_kh = .false.
test_rk_tendency = .false.
test_advect_scalar = .false.
test_relax_bdy_dry = .false.
test_rk_addtend_dry = .false.
test_spec_bdy_dry = .false.
test_small_step_prep = .false.
test_calc_p_rho = .false.
test_calc_coef_w = .false.
test_advance_uv = .false.
test_spec_bdyupdate = .false.
test_advance_mu_t = .false.
test_sumflux = .false.
test_advance_w = .false.
test_spec_bdyupdate_ph = .false.
test_calc_mu_uv_1 = .false.
test_small_step_finish = .false.
test_rk_scalar_tend = .false.
test_spec_bdy_scalar = .false.
test_rk_update_scalar = .false.
test_calc_p_rho_phi = .false.
test_diagnose_w = .false.
mp_physics = 0
ra_lw_physics = 0
ra_sw_physics = 0
radt = 0
sf_sfclay_physics = 0
sf_surface_physics = 0
bl_pbl_physics = 0
bldt = 0
cu_physics = 0
cudt = 0
gsmdt = 0
isfflx = 1
ifsnow = 0
icloud = 1
surface_input_source = 1
num_soil_layers = 5
maxiens = 1
maxens = 3
maxens2 = 3
maxens3 = 16
ensdim = 144
chem_opt = 0
num_land_cat = 24
num_soil_cat = 16
mp_zero_out = 0
mp_zero_out_thresh = 1.e-8
seaice_threshold = 271
dyn_opt = 1
rk_ord = 3
w_damping = 0
diff_opt = 1
km_opt = 1
damp_opt = 1
zdamp = 5000.
dampcoef = 0.2
khdif = 0
kvdif = 0
smdiv = 0.
emdiv = 0.
epssm = .1
non_hydrostatic = .true.
time_step_sound = 0
h_mom_adv_order = 3
v_mom_adv_order = 3
h_sca_adv_order = 3
v_sca_adv_order = 3
top_radiation = .false.
mix_cr_len = 200.
tke_upper_bound = 1000.
kh_tke_upper_bound = 1000.
kv_tke_upper_bound = 1000.
tke_drag_coefficient = 0.
tke_heat_flux = 0.
pert_coriolis = .false.
mix_full_fields = .false.
base_pres = 100000.
base_temp = 290.
base_lapse = 50.
spec_bdy_width = 5
spec_zone = 1
relax_zone = 4
specified = .false.
periodic_x = .false.
symmetric_xs = .false.
symmetric_xe = .false.
open_xs = .false.
open_xe = .false.
periodic_y = .false.
symmetric_ys = .false.
symmetric_ye = .false.
open_ys = .false.
open_ye = .false.
nested = .false.
real_data_init_type = 1
cen_lat = 0
cen_lon = 0
truelat1 = 0
truelat2 = 0
moad_cen_lat = 0
stand_lon = 0
bdyfrq = 0
iswater = 0
isice = 0
isurban = 0
isoilwater = 0
map_proj = 0
!ENDOFREGISTRYGENERATEDINCLUDE

! Statements that read the namelist are in this file


!STARTOFREGISTRYGENERATEDINCLUDE inc/config_reads.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains namelist statements for module_config.F.






!
 READ  ( UNIT = nml_unit , NML = time_control , ERR = 9200 )



 READ  ( UNIT = nml_unit , NML = domains , ERR = 9200 )



 READ  ( UNIT = nml_unit , NML = physics , ERR = 9200 )



 READ  ( UNIT = nml_unit , NML = dynamics , ERR = 9200 )



 READ  ( UNIT = nml_unit , NML = bdy_control , ERR = 9200 )



!ENDOFREGISTRYGENERATEDINCLUDE

! 2004/04/28  JM (with consensus by the group of developers)
! This is needed to ensure that nesting will work, since
! different mp_physics packages have different numbers of 
! tracers. Basically, this says that the microphysics option
! specified on the highest numbered domain *is* the microphysics
! option for the run. Not the best solution but okay for 2.0.
!

      DO i = 1, max_dom
         mp_physics(i) = mp_physics(max_dom)
      ENDDO

! Statements that assign the variables to the cfg record are in this file
! except the namelist_derived variables where are assigned below







!STARTOFREGISTRYGENERATEDINCLUDE inc/config_assigns.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 model_config_rec % run_days                   =  run_days 
 model_config_rec % run_hours                  =  run_hours 
 model_config_rec % run_minutes                =  run_minutes 
 model_config_rec % run_seconds                =  run_seconds 
 model_config_rec % start_year                 =  start_year 
 model_config_rec % start_month                =  start_month 
 model_config_rec % start_day                  =  start_day 
 model_config_rec % start_hour                 =  start_hour 
 model_config_rec % start_minute               =  start_minute 
 model_config_rec % start_second               =  start_second 
 model_config_rec % end_year                   =  end_year 
 model_config_rec % end_month                  =  end_month 
 model_config_rec % end_day                    =  end_day 
 model_config_rec % end_hour                   =  end_hour 
 model_config_rec % end_minute                 =  end_minute 
 model_config_rec % end_second                 =  end_second 
 model_config_rec % interval_seconds           =  interval_seconds 
 model_config_rec % input_from_file            =  input_from_file 
 model_config_rec % input_from_hires           =  input_from_hires 
 model_config_rec % history_interval           =  history_interval 
 model_config_rec % frames_per_outfile         =  frames_per_outfile 
 model_config_rec % frames_per_auxhist1        =  frames_per_auxhist1 
 model_config_rec % frames_per_auxhist2        =  frames_per_auxhist2 
 model_config_rec % frames_per_auxhist3        =  frames_per_auxhist3 
 model_config_rec % frames_per_auxhist4        =  frames_per_auxhist4 
 model_config_rec % frames_per_auxhist5        =  frames_per_auxhist5 
 model_config_rec % restart                    =  restart 
 model_config_rec % restart_interval           =  restart_interval 
 model_config_rec % io_form_input              =  io_form_input 
 model_config_rec % io_form_history            =  io_form_history 
 model_config_rec % io_form_restart            =  io_form_restart 
 model_config_rec % io_form_boundary           =  io_form_boundary 
 model_config_rec % debug_level                =  debug_level 
 model_config_rec % history_outname            =  history_outname 
 model_config_rec % auxhist1_outname           =  auxhist1_outname 
 model_config_rec % auxhist2_outname           =  auxhist2_outname 
 model_config_rec % auxhist3_outname           =  auxhist3_outname 
 model_config_rec % auxhist4_outname           =  auxhist4_outname 
 model_config_rec % auxhist5_outname           =  auxhist5_outname 
 model_config_rec % history_inname             =  history_inname 
 model_config_rec % auxhist1_inname            =  auxhist1_inname 
 model_config_rec % auxhist2_inname            =  auxhist2_inname 
 model_config_rec % auxhist3_inname            =  auxhist3_inname 
 model_config_rec % auxhist4_inname            =  auxhist4_inname 
 model_config_rec % auxhist5_inname            =  auxhist5_inname 
 model_config_rec % auxinput1_outname          =  auxinput1_outname 
 model_config_rec % auxinput2_outname          =  auxinput2_outname 
 model_config_rec % auxinput3_outname          =  auxinput3_outname 
 model_config_rec % auxinput4_outname          =  auxinput4_outname 
 model_config_rec % auxinput5_outname          =  auxinput5_outname 
 model_config_rec % auxinput1_inname           =  auxinput1_inname 
 model_config_rec % auxinput2_inname           =  auxinput2_inname 
 model_config_rec % auxinput3_inname           =  auxinput3_inname 
 model_config_rec % auxinput4_inname           =  auxinput4_inname 
 model_config_rec % auxinput5_inname           =  auxinput5_inname 
 model_config_rec % history_interval_mo        =  history_interval_mo 
 model_config_rec % history_interval_d         =  history_interval_d 
 model_config_rec % history_interval_h         =  history_interval_h 
 model_config_rec % history_interval_m         =  history_interval_m 
 model_config_rec % history_interval_s         =  history_interval_s 
 model_config_rec % inputout_interval_mo       =  inputout_interval_mo 
 model_config_rec % inputout_interval_d        =  inputout_interval_d 
 model_config_rec % inputout_interval_h        =  inputout_interval_h 
 model_config_rec % inputout_interval_m        =  inputout_interval_m 
 model_config_rec % inputout_interval_s        =  inputout_interval_s 
 model_config_rec % inputout_interval          =  inputout_interval 
 model_config_rec % auxhist1_interval_mo       =  auxhist1_interval_mo 
 model_config_rec % auxhist1_interval_d        =  auxhist1_interval_d 
 model_config_rec % auxhist1_interval_h        =  auxhist1_interval_h 
 model_config_rec % auxhist1_interval_m        =  auxhist1_interval_m 
 model_config_rec % auxhist1_interval_s        =  auxhist1_interval_s 
 model_config_rec % auxhist1_interval          =  auxhist1_interval 
 model_config_rec % auxhist2_interval_mo       =  auxhist2_interval_mo 
 model_config_rec % auxhist2_interval_d        =  auxhist2_interval_d 
 model_config_rec % auxhist2_interval_h        =  auxhist2_interval_h 
 model_config_rec % auxhist2_interval_m        =  auxhist2_interval_m 
 model_config_rec % auxhist2_interval_s        =  auxhist2_interval_s 
 model_config_rec % auxhist2_interval          =  auxhist2_interval 
 model_config_rec % auxhist3_interval_mo       =  auxhist3_interval_mo 
 model_config_rec % auxhist3_interval_d        =  auxhist3_interval_d 
 model_config_rec % auxhist3_interval_h        =  auxhist3_interval_h 
 model_config_rec % auxhist3_interval_m        =  auxhist3_interval_m 
 model_config_rec % auxhist3_interval_s        =  auxhist3_interval_s 
 model_config_rec % auxhist3_interval          =  auxhist3_interval 
 model_config_rec % auxhist4_interval_mo       =  auxhist4_interval_mo 
 model_config_rec % auxhist4_interval_d        =  auxhist4_interval_d 
 model_config_rec % auxhist4_interval_h        =  auxhist4_interval_h 
 model_config_rec % auxhist4_interval_m        =  auxhist4_interval_m 
 model_config_rec % auxhist4_interval_s        =  auxhist4_interval_s 
 model_config_rec % auxhist4_interval          =  auxhist4_interval 
 model_config_rec % auxhist5_interval_mo       =  auxhist5_interval_mo 
 model_config_rec % auxhist5_interval_d        =  auxhist5_interval_d 
 model_config_rec % auxhist5_interval_h        =  auxhist5_interval_h 
 model_config_rec % auxhist5_interval_m        =  auxhist5_interval_m 
 model_config_rec % auxhist5_interval_s        =  auxhist5_interval_s 
 model_config_rec % auxhist5_interval          =  auxhist5_interval 
 model_config_rec % auxinput1_interval_mo      =  auxinput1_interval_mo 
 model_config_rec % auxinput1_interval_d       =  auxinput1_interval_d 
 model_config_rec % auxinput1_interval_h       =  auxinput1_interval_h 
 model_config_rec % auxinput1_interval_m       =  auxinput1_interval_m 
 model_config_rec % auxinput1_interval_s       =  auxinput1_interval_s 
 model_config_rec % auxinput1_interval         =  auxinput1_interval 
 model_config_rec % auxinput2_interval_mo      =  auxinput2_interval_mo 
 model_config_rec % auxinput2_interval_d       =  auxinput2_interval_d 
 model_config_rec % auxinput2_interval_h       =  auxinput2_interval_h 
 model_config_rec % auxinput2_interval_m       =  auxinput2_interval_m 
 model_config_rec % auxinput2_interval_s       =  auxinput2_interval_s 
 model_config_rec % auxinput2_interval         =  auxinput2_interval 
 model_config_rec % auxinput3_interval_mo      =  auxinput3_interval_mo 
 model_config_rec % auxinput3_interval_d       =  auxinput3_interval_d 
 model_config_rec % auxinput3_interval_h       =  auxinput3_interval_h 
 model_config_rec % auxinput3_interval_m       =  auxinput3_interval_m 
 model_config_rec % auxinput3_interval_s       =  auxinput3_interval_s 
 model_config_rec % auxinput3_interval         =  auxinput3_interval 
 model_config_rec % auxinput4_interval_mo      =  auxinput4_interval_mo 
 model_config_rec % auxinput4_interval_d       =  auxinput4_interval_d 
 model_config_rec % auxinput4_interval_h       =  auxinput4_interval_h 
 model_config_rec % auxinput4_interval_m       =  auxinput4_interval_m 
 model_config_rec % auxinput4_interval_s       =  auxinput4_interval_s 
 model_config_rec % auxinput4_interval         =  auxinput4_interval 
 model_config_rec % auxinput5_interval_mo      =  auxinput5_interval_mo 
 model_config_rec % auxinput5_interval_d       =  auxinput5_interval_d 
 model_config_rec % auxinput5_interval_h       =  auxinput5_interval_h 
 model_config_rec % auxinput5_interval_m       =  auxinput5_interval_m 
 model_config_rec % auxinput5_interval_s       =  auxinput5_interval_s 
 model_config_rec % auxinput5_interval         =  auxinput5_interval 
 model_config_rec % restart_interval_mo        =  restart_interval_mo 
 model_config_rec % restart_interval_d         =  restart_interval_d 
 model_config_rec % restart_interval_h         =  restart_interval_h 
 model_config_rec % restart_interval_m         =  restart_interval_m 
 model_config_rec % restart_interval_s         =  restart_interval_s 
 model_config_rec % history_begin_y            =  history_begin_y 
 model_config_rec % history_begin_mo           =  history_begin_mo 
 model_config_rec % history_begin_d            =  history_begin_d 
 model_config_rec % history_begin_h            =  history_begin_h 
 model_config_rec % history_begin_m            =  history_begin_m 
 model_config_rec % history_begin_s            =  history_begin_s 
 model_config_rec % inputout_begin_y           =  inputout_begin_y 
 model_config_rec % inputout_begin_mo          =  inputout_begin_mo 
 model_config_rec % inputout_begin_d           =  inputout_begin_d 
 model_config_rec % inputout_begin_h           =  inputout_begin_h 
 model_config_rec % inputout_begin_m           =  inputout_begin_m 
 model_config_rec % inputout_begin_s           =  inputout_begin_s 
 model_config_rec % auxhist1_begin_y           =  auxhist1_begin_y 
 model_config_rec % auxhist1_begin_mo          =  auxhist1_begin_mo 
 model_config_rec % auxhist1_begin_d           =  auxhist1_begin_d 
 model_config_rec % auxhist1_begin_h           =  auxhist1_begin_h 
 model_config_rec % auxhist1_begin_m           =  auxhist1_begin_m 
 model_config_rec % auxhist1_begin_s           =  auxhist1_begin_s 
 model_config_rec % auxhist2_begin_y           =  auxhist2_begin_y 
 model_config_rec % auxhist2_begin_mo          =  auxhist2_begin_mo 
 model_config_rec % auxhist2_begin_d           =  auxhist2_begin_d 
 model_config_rec % auxhist2_begin_h           =  auxhist2_begin_h 
 model_config_rec % auxhist2_begin_m           =  auxhist2_begin_m 
 model_config_rec % auxhist2_begin_s           =  auxhist2_begin_s 
 model_config_rec % auxhist3_begin_y           =  auxhist3_begin_y 
 model_config_rec % auxhist3_begin_mo          =  auxhist3_begin_mo 
 model_config_rec % auxhist3_begin_d           =  auxhist3_begin_d 
 model_config_rec % auxhist3_begin_h           =  auxhist3_begin_h 
 model_config_rec % auxhist3_begin_m           =  auxhist3_begin_m 
 model_config_rec % auxhist3_begin_s           =  auxhist3_begin_s 
 model_config_rec % auxhist4_begin_y           =  auxhist4_begin_y 
 model_config_rec % auxhist4_begin_mo          =  auxhist4_begin_mo 
 model_config_rec % auxhist4_begin_d           =  auxhist4_begin_d 
 model_config_rec % auxhist4_begin_h           =  auxhist4_begin_h 
 model_config_rec % auxhist4_begin_m           =  auxhist4_begin_m 
 model_config_rec % auxhist4_begin_s           =  auxhist4_begin_s 
 model_config_rec % auxhist5_begin_y           =  auxhist5_begin_y 
 model_config_rec % auxhist5_begin_mo          =  auxhist5_begin_mo 
 model_config_rec % auxhist5_begin_d           =  auxhist5_begin_d 
 model_config_rec % auxhist5_begin_h           =  auxhist5_begin_h 
 model_config_rec % auxhist5_begin_m           =  auxhist5_begin_m 
 model_config_rec % auxhist5_begin_s           =  auxhist5_begin_s 
 model_config_rec % auxinput1_begin_y          =  auxinput1_begin_y 
 model_config_rec % auxinput1_begin_mo         =  auxinput1_begin_mo 
 model_config_rec % auxinput1_begin_d          =  auxinput1_begin_d 
 model_config_rec % auxinput1_begin_h          =  auxinput1_begin_h 
 model_config_rec % auxinput1_begin_m          =  auxinput1_begin_m 
 model_config_rec % auxinput1_begin_s          =  auxinput1_begin_s 
 model_config_rec % auxinput2_begin_y          =  auxinput2_begin_y 
 model_config_rec % auxinput2_begin_mo         =  auxinput2_begin_mo 
 model_config_rec % auxinput2_begin_d          =  auxinput2_begin_d 
 model_config_rec % auxinput2_begin_h          =  auxinput2_begin_h 
 model_config_rec % auxinput2_begin_m          =  auxinput2_begin_m 
 model_config_rec % auxinput2_begin_s          =  auxinput2_begin_s 
 model_config_rec % auxinput3_begin_y          =  auxinput3_begin_y 
 model_config_rec % auxinput3_begin_mo         =  auxinput3_begin_mo 
 model_config_rec % auxinput3_begin_d          =  auxinput3_begin_d 
 model_config_rec % auxinput3_begin_h          =  auxinput3_begin_h 
 model_config_rec % auxinput3_begin_m          =  auxinput3_begin_m 
 model_config_rec % auxinput3_begin_s          =  auxinput3_begin_s 
 model_config_rec % auxinput4_begin_y          =  auxinput4_begin_y 
 model_config_rec % auxinput4_begin_mo         =  auxinput4_begin_mo 
 model_config_rec % auxinput4_begin_d          =  auxinput4_begin_d 
 model_config_rec % auxinput4_begin_h          =  auxinput4_begin_h 
 model_config_rec % auxinput4_begin_m          =  auxinput4_begin_m 
 model_config_rec % auxinput4_begin_s          =  auxinput4_begin_s 
 model_config_rec % auxinput5_begin_y          =  auxinput5_begin_y 
 model_config_rec % auxinput5_begin_mo         =  auxinput5_begin_mo 
 model_config_rec % auxinput5_begin_d          =  auxinput5_begin_d 
 model_config_rec % auxinput5_begin_h          =  auxinput5_begin_h 
 model_config_rec % auxinput5_begin_m          =  auxinput5_begin_m 
 model_config_rec % auxinput5_begin_s          =  auxinput5_begin_s 
 model_config_rec % restart_begin_y            =  restart_begin_y 
 model_config_rec % restart_begin_mo           =  restart_begin_mo 
 model_config_rec % restart_begin_d            =  restart_begin_d 
 model_config_rec % restart_begin_h            =  restart_begin_h 
 model_config_rec % restart_begin_m            =  restart_begin_m 
 model_config_rec % restart_begin_s            =  restart_begin_s 
 model_config_rec % history_end_y              =  history_end_y 
 model_config_rec % history_end_mo             =  history_end_mo 
 model_config_rec % history_end_d              =  history_end_d 
 model_config_rec % history_end_h              =  history_end_h 
 model_config_rec % history_end_m              =  history_end_m 
 model_config_rec % history_end_s              =  history_end_s 
 model_config_rec % inputout_end_y             =  inputout_end_y 
 model_config_rec % inputout_end_mo            =  inputout_end_mo 
 model_config_rec % inputout_end_d             =  inputout_end_d 
 model_config_rec % inputout_end_h             =  inputout_end_h 
 model_config_rec % inputout_end_m             =  inputout_end_m 
 model_config_rec % inputout_end_s             =  inputout_end_s 
 model_config_rec % auxhist1_end_y             =  auxhist1_end_y 
 model_config_rec % auxhist1_end_mo            =  auxhist1_end_mo 
 model_config_rec % auxhist1_end_d             =  auxhist1_end_d 
 model_config_rec % auxhist1_end_h             =  auxhist1_end_h 
 model_config_rec % auxhist1_end_m             =  auxhist1_end_m 
 model_config_rec % auxhist1_end_s             =  auxhist1_end_s 
 model_config_rec % auxhist2_end_y             =  auxhist2_end_y 
 model_config_rec % auxhist2_end_mo            =  auxhist2_end_mo 
 model_config_rec % auxhist2_end_d             =  auxhist2_end_d 
 model_config_rec % auxhist2_end_h             =  auxhist2_end_h 
 model_config_rec % auxhist2_end_m             =  auxhist2_end_m 
 model_config_rec % auxhist2_end_s             =  auxhist2_end_s 
 model_config_rec % auxhist3_end_y             =  auxhist3_end_y 
 model_config_rec % auxhist3_end_mo            =  auxhist3_end_mo 
 model_config_rec % auxhist3_end_d             =  auxhist3_end_d 
 model_config_rec % auxhist3_end_h             =  auxhist3_end_h 
 model_config_rec % auxhist3_end_m             =  auxhist3_end_m 
 model_config_rec % auxhist3_end_s             =  auxhist3_end_s 
 model_config_rec % auxhist4_end_y             =  auxhist4_end_y 
 model_config_rec % auxhist4_end_mo            =  auxhist4_end_mo 
 model_config_rec % auxhist4_end_d             =  auxhist4_end_d 
 model_config_rec % auxhist4_end_h             =  auxhist4_end_h 
 model_config_rec % auxhist4_end_m             =  auxhist4_end_m 
 model_config_rec % auxhist4_end_s             =  auxhist4_end_s 
 model_config_rec % auxhist5_end_y             =  auxhist5_end_y 
 model_config_rec % auxhist5_end_mo            =  auxhist5_end_mo 
 model_config_rec % auxhist5_end_d             =  auxhist5_end_d 
 model_config_rec % auxhist5_end_h             =  auxhist5_end_h 
 model_config_rec % auxhist5_end_m             =  auxhist5_end_m 
 model_config_rec % auxhist5_end_s             =  auxhist5_end_s 
 model_config_rec % auxinput1_end_y            =  auxinput1_end_y 
 model_config_rec % auxinput1_end_mo           =  auxinput1_end_mo 
 model_config_rec % auxinput1_end_d            =  auxinput1_end_d 
 model_config_rec % auxinput1_end_h            =  auxinput1_end_h 
 model_config_rec % auxinput1_end_m            =  auxinput1_end_m 
 model_config_rec % auxinput1_end_s            =  auxinput1_end_s 
 model_config_rec % auxinput2_end_y            =  auxinput2_end_y 
 model_config_rec % auxinput2_end_mo           =  auxinput2_end_mo 
 model_config_rec % auxinput2_end_d            =  auxinput2_end_d 
 model_config_rec % auxinput2_end_h            =  auxinput2_end_h 
 model_config_rec % auxinput2_end_m            =  auxinput2_end_m 
 model_config_rec % auxinput2_end_s            =  auxinput2_end_s 
 model_config_rec % auxinput3_end_y            =  auxinput3_end_y 
 model_config_rec % auxinput3_end_mo           =  auxinput3_end_mo 
 model_config_rec % auxinput3_end_d            =  auxinput3_end_d 
 model_config_rec % auxinput3_end_h            =  auxinput3_end_h 
 model_config_rec % auxinput3_end_m            =  auxinput3_end_m 
 model_config_rec % auxinput3_end_s            =  auxinput3_end_s 
 model_config_rec % auxinput4_end_y            =  auxinput4_end_y 
 model_config_rec % auxinput4_end_mo           =  auxinput4_end_mo 
 model_config_rec % auxinput4_end_d            =  auxinput4_end_d 
 model_config_rec % auxinput4_end_h            =  auxinput4_end_h 
 model_config_rec % auxinput4_end_m            =  auxinput4_end_m 
 model_config_rec % auxinput4_end_s            =  auxinput4_end_s 
 model_config_rec % auxinput5_end_y            =  auxinput5_end_y 
 model_config_rec % auxinput5_end_mo           =  auxinput5_end_mo 
 model_config_rec % auxinput5_end_d            =  auxinput5_end_d 
 model_config_rec % auxinput5_end_h            =  auxinput5_end_h 
 model_config_rec % auxinput5_end_m            =  auxinput5_end_m 
 model_config_rec % auxinput5_end_s            =  auxinput5_end_s 
 model_config_rec % io_form_auxinput1          =  io_form_auxinput1 
 model_config_rec % io_form_auxinput2          =  io_form_auxinput2 
 model_config_rec % io_form_auxinput3          =  io_form_auxinput3 
 model_config_rec % io_form_auxinput4          =  io_form_auxinput4 
 model_config_rec % io_form_auxinput5          =  io_form_auxinput5 
 model_config_rec % io_form_auxhist1           =  io_form_auxhist1 
 model_config_rec % io_form_auxhist2           =  io_form_auxhist2 
 model_config_rec % io_form_auxhist3           =  io_form_auxhist3 
 model_config_rec % io_form_auxhist4           =  io_form_auxhist4 
 model_config_rec % io_form_auxhist5           =  io_form_auxhist5 
 model_config_rec % julyr                      =  julyr 
 model_config_rec % julday                     =  julday 
 model_config_rec % gmt                        =  gmt 
 model_config_rec % input_inname               =  input_inname 
 model_config_rec % input_outname              =  input_outname 
 model_config_rec % bdy_inname                 =  bdy_inname 
 model_config_rec % bdy_outname                =  bdy_outname 
 model_config_rec % rst_inname                 =  rst_inname 
 model_config_rec % rst_outname                =  rst_outname 
 model_config_rec % write_input                =  write_input 
 model_config_rec % write_restart_at_0h        =  write_restart_at_0h 
 model_config_rec % time_step                  =  time_step 
 model_config_rec % time_step_fract_num        =  time_step_fract_num 
 model_config_rec % time_step_fract_den        =  time_step_fract_den 
 model_config_rec % max_dom                    =  max_dom 
 model_config_rec % s_we                       =  s_we 
 model_config_rec % e_we                       =  e_we 
 model_config_rec % s_sn                       =  s_sn 
 model_config_rec % e_sn                       =  e_sn 
 model_config_rec % s_vert                     =  s_vert 
 model_config_rec % e_vert                     =  e_vert 
 model_config_rec % dx                         =  dx 
 model_config_rec % dy                         =  dy 
 model_config_rec % grid_id                    =  grid_id 
 model_config_rec % parent_id                  =  parent_id 
 model_config_rec % level                      =  level 
 model_config_rec % i_parent_start             =  i_parent_start 
 model_config_rec % j_parent_start             =  j_parent_start 
 model_config_rec % parent_grid_ratio          =  parent_grid_ratio 
 model_config_rec % parent_time_step_ratio     =  parent_time_step_ratio 
 model_config_rec % feedback                   =  feedback 
 model_config_rec % smooth_option              =  smooth_option 
 model_config_rec % ztop                       =  ztop 
 model_config_rec % moad_grid_ratio            =  moad_grid_ratio 
 model_config_rec % moad_time_step_ratio       =  moad_time_step_ratio 
 model_config_rec % shw                        =  shw 
 model_config_rec % coral_dist                 =  coral_dist 
 model_config_rec % tile_sz_x                  =  tile_sz_x 
 model_config_rec % tile_sz_y                  =  tile_sz_y 
 model_config_rec % numtiles                   =  numtiles 
 model_config_rec % nproc_x                    =  nproc_x 
 model_config_rec % nproc_y                    =  nproc_y 
 model_config_rec % irand                      =  irand 
 model_config_rec % dt                         =  dt 
 model_config_rec % num_moves                  =  num_moves 
 model_config_rec % move_id                    =  move_id 
 model_config_rec % move_interval              =  move_interval 
 model_config_rec % move_cd_x                  =  move_cd_x 
 model_config_rec % move_cd_y                  =  move_cd_y 
 model_config_rec % test_rk_step_prep          =  test_rk_step_prep 
 model_config_rec % test_init_zero_tendency    =  test_init_zero_tendency 
 model_config_rec % test_phy_prep              =  test_phy_prep 
 model_config_rec % test_calculate_km_kh       =  test_calculate_km_kh 
 model_config_rec % test_rk_tendency           =  test_rk_tendency 
 model_config_rec % test_advect_scalar         =  test_advect_scalar 
 model_config_rec % test_relax_bdy_dry         =  test_relax_bdy_dry 
 model_config_rec % test_rk_addtend_dry        =  test_rk_addtend_dry 
 model_config_rec % test_spec_bdy_dry          =  test_spec_bdy_dry 
 model_config_rec % test_small_step_prep       =  test_small_step_prep 
 model_config_rec % test_calc_p_rho            =  test_calc_p_rho 
 model_config_rec % test_calc_coef_w           =  test_calc_coef_w 
 model_config_rec % test_advance_uv            =  test_advance_uv 
 model_config_rec % test_spec_bdyupdate        =  test_spec_bdyupdate 
 model_config_rec % test_advance_mu_t          =  test_advance_mu_t 
 model_config_rec % test_sumflux               =  test_sumflux 
 model_config_rec % test_advance_w             =  test_advance_w 
 model_config_rec % test_spec_bdyupdate_ph     =  test_spec_bdyupdate_ph 
 model_config_rec % test_calc_mu_uv_1          =  test_calc_mu_uv_1 
 model_config_rec % test_small_step_finish     =  test_small_step_finish 
 model_config_rec % test_rk_scalar_tend        =  test_rk_scalar_tend 
 model_config_rec % test_spec_bdy_scalar       =  test_spec_bdy_scalar 
 model_config_rec % test_rk_update_scalar      =  test_rk_update_scalar 
 model_config_rec % test_calc_p_rho_phi        =  test_calc_p_rho_phi 
 model_config_rec % test_diagnose_w            =  test_diagnose_w 
 model_config_rec % mp_physics                 =  mp_physics 
 model_config_rec % ra_lw_physics              =  ra_lw_physics 
 model_config_rec % ra_sw_physics              =  ra_sw_physics 
 model_config_rec % radt                       =  radt 
 model_config_rec % sf_sfclay_physics          =  sf_sfclay_physics 
 model_config_rec % sf_surface_physics         =  sf_surface_physics 
 model_config_rec % bl_pbl_physics             =  bl_pbl_physics 
 model_config_rec % bldt                       =  bldt 
 model_config_rec % cu_physics                 =  cu_physics 
 model_config_rec % cudt                       =  cudt 
 model_config_rec % gsmdt                      =  gsmdt 
 model_config_rec % isfflx                     =  isfflx 
 model_config_rec % ifsnow                     =  ifsnow 
 model_config_rec % icloud                     =  icloud 
 model_config_rec % surface_input_source       =  surface_input_source 
 model_config_rec % num_soil_layers            =  num_soil_layers 
 model_config_rec % maxiens                    =  maxiens 
 model_config_rec % maxens                     =  maxens 
 model_config_rec % maxens2                    =  maxens2 
 model_config_rec % maxens3                    =  maxens3 
 model_config_rec % ensdim                     =  ensdim 
 model_config_rec % chem_opt                   =  chem_opt 
 model_config_rec % num_land_cat               =  num_land_cat 
 model_config_rec % num_soil_cat               =  num_soil_cat 
 model_config_rec % mp_zero_out                =  mp_zero_out 
 model_config_rec % mp_zero_out_thresh         =  mp_zero_out_thresh 
 model_config_rec % seaice_threshold           =  seaice_threshold 
 model_config_rec % dyn_opt                    =  dyn_opt 
 model_config_rec % rk_ord                     =  rk_ord 
 model_config_rec % w_damping                  =  w_damping 
 model_config_rec % diff_opt                   =  diff_opt 
 model_config_rec % km_opt                     =  km_opt 
 model_config_rec % damp_opt                   =  damp_opt 
 model_config_rec % zdamp                      =  zdamp 
 model_config_rec % dampcoef                   =  dampcoef 
 model_config_rec % khdif                      =  khdif 
 model_config_rec % kvdif                      =  kvdif 
 model_config_rec % smdiv                      =  smdiv 
 model_config_rec % emdiv                      =  emdiv 
 model_config_rec % epssm                      =  epssm 
 model_config_rec % non_hydrostatic            =  non_hydrostatic 
 model_config_rec % time_step_sound            =  time_step_sound 
 model_config_rec % h_mom_adv_order            =  h_mom_adv_order 
 model_config_rec % v_mom_adv_order            =  v_mom_adv_order 
 model_config_rec % h_sca_adv_order            =  h_sca_adv_order 
 model_config_rec % v_sca_adv_order            =  v_sca_adv_order 
 model_config_rec % top_radiation              =  top_radiation 
 model_config_rec % mix_cr_len                 =  mix_cr_len 
 model_config_rec % tke_upper_bound            =  tke_upper_bound 
 model_config_rec % kh_tke_upper_bound         =  kh_tke_upper_bound 
 model_config_rec % kv_tke_upper_bound         =  kv_tke_upper_bound 
 model_config_rec % tke_drag_coefficient       =  tke_drag_coefficient 
 model_config_rec % tke_heat_flux              =  tke_heat_flux 
 model_config_rec % pert_coriolis              =  pert_coriolis 
 model_config_rec % mix_full_fields            =  mix_full_fields 
 model_config_rec % base_pres                  =  base_pres 
 model_config_rec % base_temp                  =  base_temp 
 model_config_rec % base_lapse                 =  base_lapse 
 model_config_rec % spec_bdy_width             =  spec_bdy_width 
 model_config_rec % spec_zone                  =  spec_zone 
 model_config_rec % relax_zone                 =  relax_zone 
 model_config_rec % specified                  =  specified 
 model_config_rec % periodic_x                 =  periodic_x 
 model_config_rec % symmetric_xs               =  symmetric_xs 
 model_config_rec % symmetric_xe               =  symmetric_xe 
 model_config_rec % open_xs                    =  open_xs 
 model_config_rec % open_xe                    =  open_xe 
 model_config_rec % periodic_y                 =  periodic_y 
 model_config_rec % symmetric_ys               =  symmetric_ys 
 model_config_rec % symmetric_ye               =  symmetric_ye 
 model_config_rec % open_ys                    =  open_ys 
 model_config_rec % open_ye                    =  open_ye 
 model_config_rec % nested                     =  nested 
 model_config_rec % real_data_init_type        =  real_data_init_type 
 model_config_rec % cen_lat                    =  cen_lat 
 model_config_rec % cen_lon                    =  cen_lon 
 model_config_rec % truelat1                   =  truelat1 
 model_config_rec % truelat2                   =  truelat2 
 model_config_rec % moad_cen_lat               =  moad_cen_lat 
 model_config_rec % stand_lon                  =  stand_lon 
 model_config_rec % bdyfrq                     =  bdyfrq 
 model_config_rec % iswater                    =  iswater 
 model_config_rec % isice                      =  isice 
 model_config_rec % isurban                    =  isurban 
 model_config_rec % isoilwater                 =  isoilwater 
 model_config_rec % map_proj                   =  map_proj 
!ENDOFREGISTRYGENERATEDINCLUDE

      CLOSE ( UNIT = 10 , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3 ( "module_configure.b" , 133 ,  'ERROR CLOSING namelist.input' )
      ENDIF

      RETURN
9200  CONTINUE
      CALL wrf_error_fatal3 ( "module_configure.b" , 138 ,  'module_configure: initial_config: error reading namelist' )

   END SUBROUTINE initial_config


   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,   &
                                   model_config_rec%first_item_in_struct ,  &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3 ( "module_configure.b" , 156 ,  &
        "get_config_rec_as_buffer: buffer size to small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,  &
                                   model_config_rec%first_item_in_struct , &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3 ( "module_configure.b" , 176 ,  &
        "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer

   SUBROUTINE model_to_grid_config_rec ( id_id , model_config_rec , grid_config_rec )
      INTEGER , INTENT(IN)                         ::  id_id
      TYPE ( model_config_rec_type ) , INTENT(IN)  ::  model_config_rec
      TYPE ( grid_config_rec_type  ) , INTENT(OUT) ::  grid_config_rec
! <DESCRIPTION>
! This routine is called to populate a domain specific configuration
! record of TYPE(grid_config_rec_type) with the configuration information
! for that domain that is stored in TYPE(model_config_rec). Both types
! are defined in frame/module_configure.F.  The input argument is the
! record of type model_config_rec_type contains the model-wide
! configuration information (that is, settings that apply to the model in
! general) and configuration information for each individual domain.  The
! output argument is the record of type grid_config_rec_type which
! contains the model-wide configuration information and the
! domain-specific information for this domain only.  In the
! model_config_rec, the domain specific information is arrays, indexed by
! the grid ids.  In the grid_config_rec the domain-specific information
! is scalar and for the specific domain.  The first argument to this
! routine is the grid id (top-most domain is always 1) as specified in
! the domain-specific namelist variable grid_id.
! 
! The actual assignments form the model_config_rec_type to the
! grid_config_rec_type are generate from the rconfig entries in the
! Registry file and included by this routine from the file
! inc/config_assigns.inc.
!
!NOTE: generated subroutines from Registry file rconfig entries are renamed nl_
! instead of rconfig_ due to length limits for subroutine names.
!
! 
! </DESCRIPTION>







!STARTOFREGISTRYGENERATEDINCLUDE inc/config_assigns.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 grid_config_rec % run_days                   = model_config_rec % run_days 
 grid_config_rec % run_hours                  = model_config_rec % run_hours 
 grid_config_rec % run_minutes                = model_config_rec % run_minutes 
 grid_config_rec % run_seconds                = model_config_rec % run_seconds 
 grid_config_rec % start_year                 = model_config_rec % start_year (id_id)
 grid_config_rec % start_month                = model_config_rec % start_month (id_id)
 grid_config_rec % start_day                  = model_config_rec % start_day (id_id)
 grid_config_rec % start_hour                 = model_config_rec % start_hour (id_id)
 grid_config_rec % start_minute               = model_config_rec % start_minute (id_id)
 grid_config_rec % start_second               = model_config_rec % start_second (id_id)
 grid_config_rec % end_year                   = model_config_rec % end_year (id_id)
 grid_config_rec % end_month                  = model_config_rec % end_month (id_id)
 grid_config_rec % end_day                    = model_config_rec % end_day (id_id)
 grid_config_rec % end_hour                   = model_config_rec % end_hour (id_id)
 grid_config_rec % end_minute                 = model_config_rec % end_minute (id_id)
 grid_config_rec % end_second                 = model_config_rec % end_second (id_id)
 grid_config_rec % interval_seconds           = model_config_rec % interval_seconds 
 grid_config_rec % input_from_file            = model_config_rec % input_from_file (id_id)
 grid_config_rec % input_from_hires           = model_config_rec % input_from_hires (id_id)
 grid_config_rec % history_interval           = model_config_rec % history_interval (id_id)
 grid_config_rec % frames_per_outfile         = model_config_rec % frames_per_outfile (id_id)
 grid_config_rec % frames_per_auxhist1        = model_config_rec % frames_per_auxhist1 (id_id)
 grid_config_rec % frames_per_auxhist2        = model_config_rec % frames_per_auxhist2 (id_id)
 grid_config_rec % frames_per_auxhist3        = model_config_rec % frames_per_auxhist3 (id_id)
 grid_config_rec % frames_per_auxhist4        = model_config_rec % frames_per_auxhist4 (id_id)
 grid_config_rec % frames_per_auxhist5        = model_config_rec % frames_per_auxhist5 (id_id)
 grid_config_rec % restart                    = model_config_rec % restart 
 grid_config_rec % restart_interval           = model_config_rec % restart_interval 
 grid_config_rec % io_form_input              = model_config_rec % io_form_input 
 grid_config_rec % io_form_history            = model_config_rec % io_form_history 
 grid_config_rec % io_form_restart            = model_config_rec % io_form_restart 
 grid_config_rec % io_form_boundary           = model_config_rec % io_form_boundary 
 grid_config_rec % debug_level                = model_config_rec % debug_level 
 grid_config_rec % history_outname            = model_config_rec % history_outname 
 grid_config_rec % auxhist1_outname           = model_config_rec % auxhist1_outname 
 grid_config_rec % auxhist2_outname           = model_config_rec % auxhist2_outname 
 grid_config_rec % auxhist3_outname           = model_config_rec % auxhist3_outname 
 grid_config_rec % auxhist4_outname           = model_config_rec % auxhist4_outname 
 grid_config_rec % auxhist5_outname           = model_config_rec % auxhist5_outname 
 grid_config_rec % history_inname             = model_config_rec % history_inname 
 grid_config_rec % auxhist1_inname            = model_config_rec % auxhist1_inname 
 grid_config_rec % auxhist2_inname            = model_config_rec % auxhist2_inname 
 grid_config_rec % auxhist3_inname            = model_config_rec % auxhist3_inname 
 grid_config_rec % auxhist4_inname            = model_config_rec % auxhist4_inname 
 grid_config_rec % auxhist5_inname            = model_config_rec % auxhist5_inname 
 grid_config_rec % auxinput1_outname          = model_config_rec % auxinput1_outname 
 grid_config_rec % auxinput2_outname          = model_config_rec % auxinput2_outname 
 grid_config_rec % auxinput3_outname          = model_config_rec % auxinput3_outname 
 grid_config_rec % auxinput4_outname          = model_config_rec % auxinput4_outname 
 grid_config_rec % auxinput5_outname          = model_config_rec % auxinput5_outname 
 grid_config_rec % auxinput1_inname           = model_config_rec % auxinput1_inname 
 grid_config_rec % auxinput2_inname           = model_config_rec % auxinput2_inname 
 grid_config_rec % auxinput3_inname           = model_config_rec % auxinput3_inname 
 grid_config_rec % auxinput4_inname           = model_config_rec % auxinput4_inname 
 grid_config_rec % auxinput5_inname           = model_config_rec % auxinput5_inname 
 grid_config_rec % history_interval_mo        = model_config_rec % history_interval_mo (id_id)
 grid_config_rec % history_interval_d         = model_config_rec % history_interval_d (id_id)
 grid_config_rec % history_interval_h         = model_config_rec % history_interval_h (id_id)
 grid_config_rec % history_interval_m         = model_config_rec % history_interval_m (id_id)
 grid_config_rec % history_interval_s         = model_config_rec % history_interval_s (id_id)
 grid_config_rec % inputout_interval_mo       = model_config_rec % inputout_interval_mo (id_id)
 grid_config_rec % inputout_interval_d        = model_config_rec % inputout_interval_d (id_id)
 grid_config_rec % inputout_interval_h        = model_config_rec % inputout_interval_h (id_id)
 grid_config_rec % inputout_interval_m        = model_config_rec % inputout_interval_m (id_id)
 grid_config_rec % inputout_interval_s        = model_config_rec % inputout_interval_s (id_id)
 grid_config_rec % inputout_interval          = model_config_rec % inputout_interval (id_id)
 grid_config_rec % auxhist1_interval_mo       = model_config_rec % auxhist1_interval_mo (id_id)
 grid_config_rec % auxhist1_interval_d        = model_config_rec % auxhist1_interval_d (id_id)
 grid_config_rec % auxhist1_interval_h        = model_config_rec % auxhist1_interval_h (id_id)
 grid_config_rec % auxhist1_interval_m        = model_config_rec % auxhist1_interval_m (id_id)
 grid_config_rec % auxhist1_interval_s        = model_config_rec % auxhist1_interval_s (id_id)
 grid_config_rec % auxhist1_interval          = model_config_rec % auxhist1_interval (id_id)
 grid_config_rec % auxhist2_interval_mo       = model_config_rec % auxhist2_interval_mo (id_id)
 grid_config_rec % auxhist2_interval_d        = model_config_rec % auxhist2_interval_d (id_id)
 grid_config_rec % auxhist2_interval_h        = model_config_rec % auxhist2_interval_h (id_id)
 grid_config_rec % auxhist2_interval_m        = model_config_rec % auxhist2_interval_m (id_id)
 grid_config_rec % auxhist2_interval_s        = model_config_rec % auxhist2_interval_s (id_id)
 grid_config_rec % auxhist2_interval          = model_config_rec % auxhist2_interval (id_id)
 grid_config_rec % auxhist3_interval_mo       = model_config_rec % auxhist3_interval_mo (id_id)
 grid_config_rec % auxhist3_interval_d        = model_config_rec % auxhist3_interval_d (id_id)
 grid_config_rec % auxhist3_interval_h        = model_config_rec % auxhist3_interval_h (id_id)
 grid_config_rec % auxhist3_interval_m        = model_config_rec % auxhist3_interval_m (id_id)
 grid_config_rec % auxhist3_interval_s        = model_config_rec % auxhist3_interval_s (id_id)
 grid_config_rec % auxhist3_interval          = model_config_rec % auxhist3_interval (id_id)
 grid_config_rec % auxhist4_interval_mo       = model_config_rec % auxhist4_interval_mo (id_id)
 grid_config_rec % auxhist4_interval_d        = model_config_rec % auxhist4_interval_d (id_id)
 grid_config_rec % auxhist4_interval_h        = model_config_rec % auxhist4_interval_h (id_id)
 grid_config_rec % auxhist4_interval_m        = model_config_rec % auxhist4_interval_m (id_id)
 grid_config_rec % auxhist4_interval_s        = model_config_rec % auxhist4_interval_s (id_id)
 grid_config_rec % auxhist4_interval          = model_config_rec % auxhist4_interval (id_id)
 grid_config_rec % auxhist5_interval_mo       = model_config_rec % auxhist5_interval_mo (id_id)
 grid_config_rec % auxhist5_interval_d        = model_config_rec % auxhist5_interval_d (id_id)
 grid_config_rec % auxhist5_interval_h        = model_config_rec % auxhist5_interval_h (id_id)
 grid_config_rec % auxhist5_interval_m        = model_config_rec % auxhist5_interval_m (id_id)
 grid_config_rec % auxhist5_interval_s        = model_config_rec % auxhist5_interval_s (id_id)
 grid_config_rec % auxhist5_interval          = model_config_rec % auxhist5_interval (id_id)
 grid_config_rec % auxinput1_interval_mo      = model_config_rec % auxinput1_interval_mo (id_id)
 grid_config_rec % auxinput1_interval_d       = model_config_rec % auxinput1_interval_d (id_id)
 grid_config_rec % auxinput1_interval_h       = model_config_rec % auxinput1_interval_h (id_id)
 grid_config_rec % auxinput1_interval_m       = model_config_rec % auxinput1_interval_m (id_id)
 grid_config_rec % auxinput1_interval_s       = model_config_rec % auxinput1_interval_s (id_id)
 grid_config_rec % auxinput1_interval         = model_config_rec % auxinput1_interval (id_id)
 grid_config_rec % auxinput2_interval_mo      = model_config_rec % auxinput2_interval_mo (id_id)
 grid_config_rec % auxinput2_interval_d       = model_config_rec % auxinput2_interval_d (id_id)
 grid_config_rec % auxinput2_interval_h       = model_config_rec % auxinput2_interval_h (id_id)
 grid_config_rec % auxinput2_interval_m       = model_config_rec % auxinput2_interval_m (id_id)
 grid_config_rec % auxinput2_interval_s       = model_config_rec % auxinput2_interval_s (id_id)
 grid_config_rec % auxinput2_interval         = model_config_rec % auxinput2_interval (id_id)
 grid_config_rec % auxinput3_interval_mo      = model_config_rec % auxinput3_interval_mo (id_id)
 grid_config_rec % auxinput3_interval_d       = model_config_rec % auxinput3_interval_d (id_id)
 grid_config_rec % auxinput3_interval_h       = model_config_rec % auxinput3_interval_h (id_id)
 grid_config_rec % auxinput3_interval_m       = model_config_rec % auxinput3_interval_m (id_id)
 grid_config_rec % auxinput3_interval_s       = model_config_rec % auxinput3_interval_s (id_id)
 grid_config_rec % auxinput3_interval         = model_config_rec % auxinput3_interval (id_id)
 grid_config_rec % auxinput4_interval_mo      = model_config_rec % auxinput4_interval_mo (id_id)
 grid_config_rec % auxinput4_interval_d       = model_config_rec % auxinput4_interval_d (id_id)
 grid_config_rec % auxinput4_interval_h       = model_config_rec % auxinput4_interval_h (id_id)
 grid_config_rec % auxinput4_interval_m       = model_config_rec % auxinput4_interval_m (id_id)
 grid_config_rec % auxinput4_interval_s       = model_config_rec % auxinput4_interval_s (id_id)
 grid_config_rec % auxinput4_interval         = model_config_rec % auxinput4_interval (id_id)
 grid_config_rec % auxinput5_interval_mo      = model_config_rec % auxinput5_interval_mo (id_id)
 grid_config_rec % auxinput5_interval_d       = model_config_rec % auxinput5_interval_d (id_id)
 grid_config_rec % auxinput5_interval_h       = model_config_rec % auxinput5_interval_h (id_id)
 grid_config_rec % auxinput5_interval_m       = model_config_rec % auxinput5_interval_m (id_id)
 grid_config_rec % auxinput5_interval_s       = model_config_rec % auxinput5_interval_s (id_id)
 grid_config_rec % auxinput5_interval         = model_config_rec % auxinput5_interval (id_id)
 grid_config_rec % restart_interval_mo        = model_config_rec % restart_interval_mo 
 grid_config_rec % restart_interval_d         = model_config_rec % restart_interval_d 
 grid_config_rec % restart_interval_h         = model_config_rec % restart_interval_h 
 grid_config_rec % restart_interval_m         = model_config_rec % restart_interval_m 
 grid_config_rec % restart_interval_s         = model_config_rec % restart_interval_s 
 grid_config_rec % history_begin_y            = model_config_rec % history_begin_y (id_id)
 grid_config_rec % history_begin_mo           = model_config_rec % history_begin_mo (id_id)
 grid_config_rec % history_begin_d            = model_config_rec % history_begin_d (id_id)
 grid_config_rec % history_begin_h            = model_config_rec % history_begin_h (id_id)
 grid_config_rec % history_begin_m            = model_config_rec % history_begin_m (id_id)
 grid_config_rec % history_begin_s            = model_config_rec % history_begin_s (id_id)
 grid_config_rec % inputout_begin_y           = model_config_rec % inputout_begin_y (id_id)
 grid_config_rec % inputout_begin_mo          = model_config_rec % inputout_begin_mo (id_id)
 grid_config_rec % inputout_begin_d           = model_config_rec % inputout_begin_d (id_id)
 grid_config_rec % inputout_begin_h           = model_config_rec % inputout_begin_h (id_id)
 grid_config_rec % inputout_begin_m           = model_config_rec % inputout_begin_m (id_id)
 grid_config_rec % inputout_begin_s           = model_config_rec % inputout_begin_s (id_id)
 grid_config_rec % auxhist1_begin_y           = model_config_rec % auxhist1_begin_y (id_id)
 grid_config_rec % auxhist1_begin_mo          = model_config_rec % auxhist1_begin_mo (id_id)
 grid_config_rec % auxhist1_begin_d           = model_config_rec % auxhist1_begin_d (id_id)
 grid_config_rec % auxhist1_begin_h           = model_config_rec % auxhist1_begin_h (id_id)
 grid_config_rec % auxhist1_begin_m           = model_config_rec % auxhist1_begin_m (id_id)
 grid_config_rec % auxhist1_begin_s           = model_config_rec % auxhist1_begin_s (id_id)
 grid_config_rec % auxhist2_begin_y           = model_config_rec % auxhist2_begin_y (id_id)
 grid_config_rec % auxhist2_begin_mo          = model_config_rec % auxhist2_begin_mo (id_id)
 grid_config_rec % auxhist2_begin_d           = model_config_rec % auxhist2_begin_d (id_id)
 grid_config_rec % auxhist2_begin_h           = model_config_rec % auxhist2_begin_h (id_id)
 grid_config_rec % auxhist2_begin_m           = model_config_rec % auxhist2_begin_m (id_id)
 grid_config_rec % auxhist2_begin_s           = model_config_rec % auxhist2_begin_s (id_id)
 grid_config_rec % auxhist3_begin_y           = model_config_rec % auxhist3_begin_y (id_id)
 grid_config_rec % auxhist3_begin_mo          = model_config_rec % auxhist3_begin_mo (id_id)
 grid_config_rec % auxhist3_begin_d           = model_config_rec % auxhist3_begin_d (id_id)
 grid_config_rec % auxhist3_begin_h           = model_config_rec % auxhist3_begin_h (id_id)
 grid_config_rec % auxhist3_begin_m           = model_config_rec % auxhist3_begin_m (id_id)
 grid_config_rec % auxhist3_begin_s           = model_config_rec % auxhist3_begin_s (id_id)
 grid_config_rec % auxhist4_begin_y           = model_config_rec % auxhist4_begin_y (id_id)
 grid_config_rec % auxhist4_begin_mo          = model_config_rec % auxhist4_begin_mo (id_id)
 grid_config_rec % auxhist4_begin_d           = model_config_rec % auxhist4_begin_d (id_id)
 grid_config_rec % auxhist4_begin_h           = model_config_rec % auxhist4_begin_h (id_id)
 grid_config_rec % auxhist4_begin_m           = model_config_rec % auxhist4_begin_m (id_id)
 grid_config_rec % auxhist4_begin_s           = model_config_rec % auxhist4_begin_s (id_id)
 grid_config_rec % auxhist5_begin_y           = model_config_rec % auxhist5_begin_y (id_id)
 grid_config_rec % auxhist5_begin_mo          = model_config_rec % auxhist5_begin_mo (id_id)
 grid_config_rec % auxhist5_begin_d           = model_config_rec % auxhist5_begin_d (id_id)
 grid_config_rec % auxhist5_begin_h           = model_config_rec % auxhist5_begin_h (id_id)
 grid_config_rec % auxhist5_begin_m           = model_config_rec % auxhist5_begin_m (id_id)
 grid_config_rec % auxhist5_begin_s           = model_config_rec % auxhist5_begin_s (id_id)
 grid_config_rec % auxinput1_begin_y          = model_config_rec % auxinput1_begin_y (id_id)
 grid_config_rec % auxinput1_begin_mo         = model_config_rec % auxinput1_begin_mo (id_id)
 grid_config_rec % auxinput1_begin_d          = model_config_rec % auxinput1_begin_d (id_id)
 grid_config_rec % auxinput1_begin_h          = model_config_rec % auxinput1_begin_h (id_id)
 grid_config_rec % auxinput1_begin_m          = model_config_rec % auxinput1_begin_m (id_id)
 grid_config_rec % auxinput1_begin_s          = model_config_rec % auxinput1_begin_s (id_id)
 grid_config_rec % auxinput2_begin_y          = model_config_rec % auxinput2_begin_y (id_id)
 grid_config_rec % auxinput2_begin_mo         = model_config_rec % auxinput2_begin_mo (id_id)
 grid_config_rec % auxinput2_begin_d          = model_config_rec % auxinput2_begin_d (id_id)
 grid_config_rec % auxinput2_begin_h          = model_config_rec % auxinput2_begin_h (id_id)
 grid_config_rec % auxinput2_begin_m          = model_config_rec % auxinput2_begin_m (id_id)
 grid_config_rec % auxinput2_begin_s          = model_config_rec % auxinput2_begin_s (id_id)
 grid_config_rec % auxinput3_begin_y          = model_config_rec % auxinput3_begin_y (id_id)
 grid_config_rec % auxinput3_begin_mo         = model_config_rec % auxinput3_begin_mo (id_id)
 grid_config_rec % auxinput3_begin_d          = model_config_rec % auxinput3_begin_d (id_id)
 grid_config_rec % auxinput3_begin_h          = model_config_rec % auxinput3_begin_h (id_id)
 grid_config_rec % auxinput3_begin_m          = model_config_rec % auxinput3_begin_m (id_id)
 grid_config_rec % auxinput3_begin_s          = model_config_rec % auxinput3_begin_s (id_id)
 grid_config_rec % auxinput4_begin_y          = model_config_rec % auxinput4_begin_y (id_id)
 grid_config_rec % auxinput4_begin_mo         = model_config_rec % auxinput4_begin_mo (id_id)
 grid_config_rec % auxinput4_begin_d          = model_config_rec % auxinput4_begin_d (id_id)
 grid_config_rec % auxinput4_begin_h          = model_config_rec % auxinput4_begin_h (id_id)
 grid_config_rec % auxinput4_begin_m          = model_config_rec % auxinput4_begin_m (id_id)
 grid_config_rec % auxinput4_begin_s          = model_config_rec % auxinput4_begin_s (id_id)
 grid_config_rec % auxinput5_begin_y          = model_config_rec % auxinput5_begin_y (id_id)
 grid_config_rec % auxinput5_begin_mo         = model_config_rec % auxinput5_begin_mo (id_id)
 grid_config_rec % auxinput5_begin_d          = model_config_rec % auxinput5_begin_d (id_id)
 grid_config_rec % auxinput5_begin_h          = model_config_rec % auxinput5_begin_h (id_id)
 grid_config_rec % auxinput5_begin_m          = model_config_rec % auxinput5_begin_m (id_id)
 grid_config_rec % auxinput5_begin_s          = model_config_rec % auxinput5_begin_s (id_id)
 grid_config_rec % restart_begin_y            = model_config_rec % restart_begin_y 
 grid_config_rec % restart_begin_mo           = model_config_rec % restart_begin_mo 
 grid_config_rec % restart_begin_d            = model_config_rec % restart_begin_d 
 grid_config_rec % restart_begin_h            = model_config_rec % restart_begin_h 
 grid_config_rec % restart_begin_m            = model_config_rec % restart_begin_m 
 grid_config_rec % restart_begin_s            = model_config_rec % restart_begin_s 
 grid_config_rec % history_end_y              = model_config_rec % history_end_y (id_id)
 grid_config_rec % history_end_mo             = model_config_rec % history_end_mo (id_id)
 grid_config_rec % history_end_d              = model_config_rec % history_end_d (id_id)
 grid_config_rec % history_end_h              = model_config_rec % history_end_h (id_id)
 grid_config_rec % history_end_m              = model_config_rec % history_end_m (id_id)
 grid_config_rec % history_end_s              = model_config_rec % history_end_s (id_id)
 grid_config_rec % inputout_end_y             = model_config_rec % inputout_end_y (id_id)
 grid_config_rec % inputout_end_mo            = model_config_rec % inputout_end_mo (id_id)
 grid_config_rec % inputout_end_d             = model_config_rec % inputout_end_d (id_id)
 grid_config_rec % inputout_end_h             = model_config_rec % inputout_end_h (id_id)
 grid_config_rec % inputout_end_m             = model_config_rec % inputout_end_m (id_id)
 grid_config_rec % inputout_end_s             = model_config_rec % inputout_end_s (id_id)
 grid_config_rec % auxhist1_end_y             = model_config_rec % auxhist1_end_y (id_id)
 grid_config_rec % auxhist1_end_mo            = model_config_rec % auxhist1_end_mo (id_id)
 grid_config_rec % auxhist1_end_d             = model_config_rec % auxhist1_end_d (id_id)
 grid_config_rec % auxhist1_end_h             = model_config_rec % auxhist1_end_h (id_id)
 grid_config_rec % auxhist1_end_m             = model_config_rec % auxhist1_end_m (id_id)
 grid_config_rec % auxhist1_end_s             = model_config_rec % auxhist1_end_s (id_id)
 grid_config_rec % auxhist2_end_y             = model_config_rec % auxhist2_end_y (id_id)
 grid_config_rec % auxhist2_end_mo            = model_config_rec % auxhist2_end_mo (id_id)
 grid_config_rec % auxhist2_end_d             = model_config_rec % auxhist2_end_d (id_id)
 grid_config_rec % auxhist2_end_h             = model_config_rec % auxhist2_end_h (id_id)
 grid_config_rec % auxhist2_end_m             = model_config_rec % auxhist2_end_m (id_id)
 grid_config_rec % auxhist2_end_s             = model_config_rec % auxhist2_end_s (id_id)
 grid_config_rec % auxhist3_end_y             = model_config_rec % auxhist3_end_y (id_id)
 grid_config_rec % auxhist3_end_mo            = model_config_rec % auxhist3_end_mo (id_id)
 grid_config_rec % auxhist3_end_d             = model_config_rec % auxhist3_end_d (id_id)
 grid_config_rec % auxhist3_end_h             = model_config_rec % auxhist3_end_h (id_id)
 grid_config_rec % auxhist3_end_m             = model_config_rec % auxhist3_end_m (id_id)
 grid_config_rec % auxhist3_end_s             = model_config_rec % auxhist3_end_s (id_id)
 grid_config_rec % auxhist4_end_y             = model_config_rec % auxhist4_end_y (id_id)
 grid_config_rec % auxhist4_end_mo            = model_config_rec % auxhist4_end_mo (id_id)
 grid_config_rec % auxhist4_end_d             = model_config_rec % auxhist4_end_d (id_id)
 grid_config_rec % auxhist4_end_h             = model_config_rec % auxhist4_end_h (id_id)
 grid_config_rec % auxhist4_end_m             = model_config_rec % auxhist4_end_m (id_id)
 grid_config_rec % auxhist4_end_s             = model_config_rec % auxhist4_end_s (id_id)
 grid_config_rec % auxhist5_end_y             = model_config_rec % auxhist5_end_y (id_id)
 grid_config_rec % auxhist5_end_mo            = model_config_rec % auxhist5_end_mo (id_id)
 grid_config_rec % auxhist5_end_d             = model_config_rec % auxhist5_end_d (id_id)
 grid_config_rec % auxhist5_end_h             = model_config_rec % auxhist5_end_h (id_id)
 grid_config_rec % auxhist5_end_m             = model_config_rec % auxhist5_end_m (id_id)
 grid_config_rec % auxhist5_end_s             = model_config_rec % auxhist5_end_s (id_id)
 grid_config_rec % auxinput1_end_y            = model_config_rec % auxinput1_end_y (id_id)
 grid_config_rec % auxinput1_end_mo           = model_config_rec % auxinput1_end_mo (id_id)
 grid_config_rec % auxinput1_end_d            = model_config_rec % auxinput1_end_d (id_id)
 grid_config_rec % auxinput1_end_h            = model_config_rec % auxinput1_end_h (id_id)
 grid_config_rec % auxinput1_end_m            = model_config_rec % auxinput1_end_m (id_id)
 grid_config_rec % auxinput1_end_s            = model_config_rec % auxinput1_end_s (id_id)
 grid_config_rec % auxinput2_end_y            = model_config_rec % auxinput2_end_y (id_id)
 grid_config_rec % auxinput2_end_mo           = model_config_rec % auxinput2_end_mo (id_id)
 grid_config_rec % auxinput2_end_d            = model_config_rec % auxinput2_end_d (id_id)
 grid_config_rec % auxinput2_end_h            = model_config_rec % auxinput2_end_h (id_id)
 grid_config_rec % auxinput2_end_m            = model_config_rec % auxinput2_end_m (id_id)
 grid_config_rec % auxinput2_end_s            = model_config_rec % auxinput2_end_s (id_id)
 grid_config_rec % auxinput3_end_y            = model_config_rec % auxinput3_end_y (id_id)
 grid_config_rec % auxinput3_end_mo           = model_config_rec % auxinput3_end_mo (id_id)
 grid_config_rec % auxinput3_end_d            = model_config_rec % auxinput3_end_d (id_id)
 grid_config_rec % auxinput3_end_h            = model_config_rec % auxinput3_end_h (id_id)
 grid_config_rec % auxinput3_end_m            = model_config_rec % auxinput3_end_m (id_id)
 grid_config_rec % auxinput3_end_s            = model_config_rec % auxinput3_end_s (id_id)
 grid_config_rec % auxinput4_end_y            = model_config_rec % auxinput4_end_y (id_id)
 grid_config_rec % auxinput4_end_mo           = model_config_rec % auxinput4_end_mo (id_id)
 grid_config_rec % auxinput4_end_d            = model_config_rec % auxinput4_end_d (id_id)
 grid_config_rec % auxinput4_end_h            = model_config_rec % auxinput4_end_h (id_id)
 grid_config_rec % auxinput4_end_m            = model_config_rec % auxinput4_end_m (id_id)
 grid_config_rec % auxinput4_end_s            = model_config_rec % auxinput4_end_s (id_id)
 grid_config_rec % auxinput5_end_y            = model_config_rec % auxinput5_end_y (id_id)
 grid_config_rec % auxinput5_end_mo           = model_config_rec % auxinput5_end_mo (id_id)
 grid_config_rec % auxinput5_end_d            = model_config_rec % auxinput5_end_d (id_id)
 grid_config_rec % auxinput5_end_h            = model_config_rec % auxinput5_end_h (id_id)
 grid_config_rec % auxinput5_end_m            = model_config_rec % auxinput5_end_m (id_id)
 grid_config_rec % auxinput5_end_s            = model_config_rec % auxinput5_end_s (id_id)
 grid_config_rec % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid_config_rec % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid_config_rec % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid_config_rec % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid_config_rec % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid_config_rec % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid_config_rec % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid_config_rec % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid_config_rec % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid_config_rec % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid_config_rec % julyr                      = model_config_rec % julyr (id_id)
 grid_config_rec % julday                     = model_config_rec % julday (id_id)
 grid_config_rec % gmt                        = model_config_rec % gmt (id_id)
 grid_config_rec % input_inname               = model_config_rec % input_inname 
 grid_config_rec % input_outname              = model_config_rec % input_outname 
 grid_config_rec % bdy_inname                 = model_config_rec % bdy_inname 
 grid_config_rec % bdy_outname                = model_config_rec % bdy_outname 
 grid_config_rec % rst_inname                 = model_config_rec % rst_inname 
 grid_config_rec % rst_outname                = model_config_rec % rst_outname 
 grid_config_rec % write_input                = model_config_rec % write_input 
 grid_config_rec % write_restart_at_0h        = model_config_rec % write_restart_at_0h 
 grid_config_rec % time_step                  = model_config_rec % time_step 
 grid_config_rec % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid_config_rec % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid_config_rec % max_dom                    = model_config_rec % max_dom 
 grid_config_rec % s_we                       = model_config_rec % s_we (id_id)
 grid_config_rec % e_we                       = model_config_rec % e_we (id_id)
 grid_config_rec % s_sn                       = model_config_rec % s_sn (id_id)
 grid_config_rec % e_sn                       = model_config_rec % e_sn (id_id)
 grid_config_rec % s_vert                     = model_config_rec % s_vert (id_id)
 grid_config_rec % e_vert                     = model_config_rec % e_vert (id_id)
 grid_config_rec % dx                         = model_config_rec % dx (id_id)
 grid_config_rec % dy                         = model_config_rec % dy (id_id)
 grid_config_rec % grid_id                    = model_config_rec % grid_id (id_id)
 grid_config_rec % parent_id                  = model_config_rec % parent_id (id_id)
 grid_config_rec % level                      = model_config_rec % level (id_id)
 grid_config_rec % i_parent_start             = model_config_rec % i_parent_start (id_id)
 grid_config_rec % j_parent_start             = model_config_rec % j_parent_start (id_id)
 grid_config_rec % parent_grid_ratio          = model_config_rec % parent_grid_ratio (id_id)
 grid_config_rec % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (id_id)
 grid_config_rec % feedback                   = model_config_rec % feedback 
 grid_config_rec % smooth_option              = model_config_rec % smooth_option 
 grid_config_rec % ztop                       = model_config_rec % ztop (id_id)
 grid_config_rec % moad_grid_ratio            = model_config_rec % moad_grid_ratio (id_id)
 grid_config_rec % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (id_id)
 grid_config_rec % shw                        = model_config_rec % shw (id_id)
 grid_config_rec % coral_dist                 = model_config_rec % coral_dist (id_id)
 grid_config_rec % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid_config_rec % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid_config_rec % numtiles                   = model_config_rec % numtiles 
 grid_config_rec % nproc_x                    = model_config_rec % nproc_x 
 grid_config_rec % nproc_y                    = model_config_rec % nproc_y 
 grid_config_rec % irand                      = model_config_rec % irand 
 grid_config_rec % dt                         = model_config_rec % dt (id_id)
 grid_config_rec % num_moves                  = model_config_rec % num_moves 
 grid_config_rec % move_id                    = model_config_rec % move_id (id_id)
 grid_config_rec % move_interval              = model_config_rec % move_interval (id_id)
 grid_config_rec % move_cd_x                  = model_config_rec % move_cd_x (id_id)
 grid_config_rec % move_cd_y                  = model_config_rec % move_cd_y (id_id)
 grid_config_rec % test_rk_step_prep          = model_config_rec % test_rk_step_prep 
 grid_config_rec % test_init_zero_tendency    = model_config_rec % test_init_zero_tendency 
 grid_config_rec % test_phy_prep              = model_config_rec % test_phy_prep 
 grid_config_rec % test_calculate_km_kh       = model_config_rec % test_calculate_km_kh 
 grid_config_rec % test_rk_tendency           = model_config_rec % test_rk_tendency 
 grid_config_rec % test_advect_scalar         = model_config_rec % test_advect_scalar 
 grid_config_rec % test_relax_bdy_dry         = model_config_rec % test_relax_bdy_dry 
 grid_config_rec % test_rk_addtend_dry        = model_config_rec % test_rk_addtend_dry 
 grid_config_rec % test_spec_bdy_dry          = model_config_rec % test_spec_bdy_dry 
 grid_config_rec % test_small_step_prep       = model_config_rec % test_small_step_prep 
 grid_config_rec % test_calc_p_rho            = model_config_rec % test_calc_p_rho 
 grid_config_rec % test_calc_coef_w           = model_config_rec % test_calc_coef_w 
 grid_config_rec % test_advance_uv            = model_config_rec % test_advance_uv 
 grid_config_rec % test_spec_bdyupdate        = model_config_rec % test_spec_bdyupdate 
 grid_config_rec % test_advance_mu_t          = model_config_rec % test_advance_mu_t 
 grid_config_rec % test_sumflux               = model_config_rec % test_sumflux 
 grid_config_rec % test_advance_w             = model_config_rec % test_advance_w 
 grid_config_rec % test_spec_bdyupdate_ph     = model_config_rec % test_spec_bdyupdate_ph 
 grid_config_rec % test_calc_mu_uv_1          = model_config_rec % test_calc_mu_uv_1 
 grid_config_rec % test_small_step_finish     = model_config_rec % test_small_step_finish 
 grid_config_rec % test_rk_scalar_tend        = model_config_rec % test_rk_scalar_tend 
 grid_config_rec % test_spec_bdy_scalar       = model_config_rec % test_spec_bdy_scalar 
 grid_config_rec % test_rk_update_scalar      = model_config_rec % test_rk_update_scalar 
 grid_config_rec % test_calc_p_rho_phi        = model_config_rec % test_calc_p_rho_phi 
 grid_config_rec % test_diagnose_w            = model_config_rec % test_diagnose_w 
 grid_config_rec % mp_physics                 = model_config_rec % mp_physics (id_id)
 grid_config_rec % ra_lw_physics              = model_config_rec % ra_lw_physics (id_id)
 grid_config_rec % ra_sw_physics              = model_config_rec % ra_sw_physics (id_id)
 grid_config_rec % radt                       = model_config_rec % radt (id_id)
 grid_config_rec % sf_sfclay_physics          = model_config_rec % sf_sfclay_physics (id_id)
 grid_config_rec % sf_surface_physics         = model_config_rec % sf_surface_physics (id_id)
 grid_config_rec % bl_pbl_physics             = model_config_rec % bl_pbl_physics (id_id)
 grid_config_rec % bldt                       = model_config_rec % bldt (id_id)
 grid_config_rec % cu_physics                 = model_config_rec % cu_physics (id_id)
 grid_config_rec % cudt                       = model_config_rec % cudt (id_id)
 grid_config_rec % gsmdt                      = model_config_rec % gsmdt (id_id)
 grid_config_rec % isfflx                     = model_config_rec % isfflx 
 grid_config_rec % ifsnow                     = model_config_rec % ifsnow 
 grid_config_rec % icloud                     = model_config_rec % icloud 
 grid_config_rec % surface_input_source       = model_config_rec % surface_input_source 
 grid_config_rec % num_soil_layers            = model_config_rec % num_soil_layers 
 grid_config_rec % maxiens                    = model_config_rec % maxiens 
 grid_config_rec % maxens                     = model_config_rec % maxens 
 grid_config_rec % maxens2                    = model_config_rec % maxens2 
 grid_config_rec % maxens3                    = model_config_rec % maxens3 
 grid_config_rec % ensdim                     = model_config_rec % ensdim 
 grid_config_rec % chem_opt                   = model_config_rec % chem_opt (id_id)
 grid_config_rec % num_land_cat               = model_config_rec % num_land_cat 
 grid_config_rec % num_soil_cat               = model_config_rec % num_soil_cat 
 grid_config_rec % mp_zero_out                = model_config_rec % mp_zero_out 
 grid_config_rec % mp_zero_out_thresh         = model_config_rec % mp_zero_out_thresh 
 grid_config_rec % seaice_threshold           = model_config_rec % seaice_threshold 
 grid_config_rec % dyn_opt                    = model_config_rec % dyn_opt 
 grid_config_rec % rk_ord                     = model_config_rec % rk_ord 
 grid_config_rec % w_damping                  = model_config_rec % w_damping 
 grid_config_rec % diff_opt                   = model_config_rec % diff_opt 
 grid_config_rec % km_opt                     = model_config_rec % km_opt 
 grid_config_rec % damp_opt                   = model_config_rec % damp_opt 
 grid_config_rec % zdamp                      = model_config_rec % zdamp (id_id)
 grid_config_rec % dampcoef                   = model_config_rec % dampcoef (id_id)
 grid_config_rec % khdif                      = model_config_rec % khdif (id_id)
 grid_config_rec % kvdif                      = model_config_rec % kvdif (id_id)
 grid_config_rec % smdiv                      = model_config_rec % smdiv (id_id)
 grid_config_rec % emdiv                      = model_config_rec % emdiv (id_id)
 grid_config_rec % epssm                      = model_config_rec % epssm (id_id)
 grid_config_rec % non_hydrostatic            = model_config_rec % non_hydrostatic (id_id)
 grid_config_rec % time_step_sound            = model_config_rec % time_step_sound (id_id)
 grid_config_rec % h_mom_adv_order            = model_config_rec % h_mom_adv_order (id_id)
 grid_config_rec % v_mom_adv_order            = model_config_rec % v_mom_adv_order (id_id)
 grid_config_rec % h_sca_adv_order            = model_config_rec % h_sca_adv_order (id_id)
 grid_config_rec % v_sca_adv_order            = model_config_rec % v_sca_adv_order (id_id)
 grid_config_rec % top_radiation              = model_config_rec % top_radiation (id_id)
 grid_config_rec % mix_cr_len                 = model_config_rec % mix_cr_len (id_id)
 grid_config_rec % tke_upper_bound            = model_config_rec % tke_upper_bound (id_id)
 grid_config_rec % kh_tke_upper_bound         = model_config_rec % kh_tke_upper_bound (id_id)
 grid_config_rec % kv_tke_upper_bound         = model_config_rec % kv_tke_upper_bound (id_id)
 grid_config_rec % tke_drag_coefficient       = model_config_rec % tke_drag_coefficient (id_id)
 grid_config_rec % tke_heat_flux              = model_config_rec % tke_heat_flux (id_id)
 grid_config_rec % pert_coriolis              = model_config_rec % pert_coriolis (id_id)
 grid_config_rec % mix_full_fields            = model_config_rec % mix_full_fields (id_id)
 grid_config_rec % base_pres                  = model_config_rec % base_pres 
 grid_config_rec % base_temp                  = model_config_rec % base_temp 
 grid_config_rec % base_lapse                 = model_config_rec % base_lapse 
 grid_config_rec % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid_config_rec % spec_zone                  = model_config_rec % spec_zone 
 grid_config_rec % relax_zone                 = model_config_rec % relax_zone 
 grid_config_rec % specified                  = model_config_rec % specified (id_id)
 grid_config_rec % periodic_x                 = model_config_rec % periodic_x (id_id)
 grid_config_rec % symmetric_xs               = model_config_rec % symmetric_xs (id_id)
 grid_config_rec % symmetric_xe               = model_config_rec % symmetric_xe (id_id)
 grid_config_rec % open_xs                    = model_config_rec % open_xs (id_id)
 grid_config_rec % open_xe                    = model_config_rec % open_xe (id_id)
 grid_config_rec % periodic_y                 = model_config_rec % periodic_y (id_id)
 grid_config_rec % symmetric_ys               = model_config_rec % symmetric_ys (id_id)
 grid_config_rec % symmetric_ye               = model_config_rec % symmetric_ye (id_id)
 grid_config_rec % open_ys                    = model_config_rec % open_ys (id_id)
 grid_config_rec % open_ye                    = model_config_rec % open_ye (id_id)
 grid_config_rec % nested                     = model_config_rec % nested (id_id)
 grid_config_rec % real_data_init_type        = model_config_rec % real_data_init_type 
 grid_config_rec % cen_lat                    = model_config_rec % cen_lat (id_id)
 grid_config_rec % cen_lon                    = model_config_rec % cen_lon (id_id)
 grid_config_rec % truelat1                   = model_config_rec % truelat1 (id_id)
 grid_config_rec % truelat2                   = model_config_rec % truelat2 (id_id)
 grid_config_rec % moad_cen_lat               = model_config_rec % moad_cen_lat (id_id)
 grid_config_rec % stand_lon                  = model_config_rec % stand_lon (id_id)
 grid_config_rec % bdyfrq                     = model_config_rec % bdyfrq (id_id)
 grid_config_rec % iswater                    = model_config_rec % iswater (id_id)
 grid_config_rec % isice                      = model_config_rec % isice (id_id)
 grid_config_rec % isurban                    = model_config_rec % isurban (id_id)
 grid_config_rec % isoilwater                 = model_config_rec % isoilwater (id_id)
 grid_config_rec % map_proj                   = model_config_rec % map_proj (id_id)
!ENDOFREGISTRYGENERATEDINCLUDE
   END SUBROUTINE model_to_grid_config_rec

! Include the definitions of all the routines that return a namelist values
! back to the driver. These are generated by the registry

   SUBROUTINE init_module_configure
     IMPLICIT NONE
     ! Local vars

     INTEGER i , j

     DO j = 1, max_domains

!STARTOFREGISTRYGENERATEDINCLUDE inc/scalar_tables_init.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  moist_num_table( j ) = 1
  chem_num_table( j ) = 1
!ENDOFREGISTRYGENERATEDINCLUDE
     END DO
   END SUBROUTINE init_module_configure

! When the compiler has Intel Inside (TM) (that is, ifort), the large
! number of nl_get and nl_set routines inside the module causes the
! compiler to never finish with this routine. For ifort, move the
! routines outside the module. Note, the registry generates a 
! USE module_configure for all the nl_get and nl_set routines
! if IFORT_KLUDGE is in effect.









!STARTOFREGISTRYGENERATEDINCLUDE inc/get_nl_config.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_get_run_days ( id_id , run_days )
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_run_days: run_days applies to all domains. First arg ignored.')
  ENDIF
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_run_hours: run_hours applies to all domains. First arg ignored.')
  ENDIF
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_run_minutes: run_minutes applies to all domains. First arg ignored.')
  ENDIF
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_run_seconds: run_seconds applies to all domains. First arg ignored.')
  ENDIF
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3559 , emess)
  ENDIF
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3570 , emess)
  ENDIF
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3581 , emess)
  ENDIF
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3592 , emess)
  ENDIF
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3603 , emess)
  ENDIF
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3614 , emess)
  ENDIF
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3625 , emess)
  ENDIF
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3636 , emess)
  ENDIF
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3647 , emess)
  ENDIF
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3658 , emess)
  ENDIF
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3669 , emess)
  ENDIF
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3680 , emess)
  ENDIF
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_interval_seconds: interval_seconds applies to all domains. First arg ignored.')
  ENDIF
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_input_from_file: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3701 , emess)
  ENDIF
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_input_from_hires ( id_id , input_from_hires )
  logical , INTENT(OUT) :: input_from_hires
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_input_from_hires: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3712 , emess)
  ENDIF
  input_from_hires = model_config_rec%input_from_hires(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_hires
SUBROUTINE nl_get_history_interval ( id_id , history_interval )
  integer , INTENT(OUT) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3723 , emess)
  ENDIF
  history_interval = model_config_rec%history_interval(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval
SUBROUTINE nl_get_frames_per_outfile ( id_id , frames_per_outfile )
  integer , INTENT(OUT) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_outfile: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3734 , emess)
  ENDIF
  frames_per_outfile = model_config_rec%frames_per_outfile(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_outfile
SUBROUTINE nl_get_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  integer , INTENT(OUT) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist1: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3745 , emess)
  ENDIF
  frames_per_auxhist1 = model_config_rec%frames_per_auxhist1(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist1
SUBROUTINE nl_get_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  integer , INTENT(OUT) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist2: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3756 , emess)
  ENDIF
  frames_per_auxhist2 = model_config_rec%frames_per_auxhist2(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist2
SUBROUTINE nl_get_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  integer , INTENT(OUT) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist3: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3767 , emess)
  ENDIF
  frames_per_auxhist3 = model_config_rec%frames_per_auxhist3(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist3
SUBROUTINE nl_get_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  integer , INTENT(OUT) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist4: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3778 , emess)
  ENDIF
  frames_per_auxhist4 = model_config_rec%frames_per_auxhist4(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist4
SUBROUTINE nl_get_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  integer , INTENT(OUT) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist5: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 3789 , emess)
  ENDIF
  frames_per_auxhist5 = model_config_rec%frames_per_auxhist5(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist5
SUBROUTINE nl_get_restart ( id_id , restart )
  logical , INTENT(OUT) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart: restart applies to all domains. First arg ignored.')
  ENDIF
  restart = model_config_rec%restart
  RETURN
END SUBROUTINE nl_get_restart
SUBROUTINE nl_get_restart_interval ( id_id , restart_interval )
  integer , INTENT(OUT) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval: restart_interval applies to all domains. First arg ignored.')
  ENDIF
  restart_interval = model_config_rec%restart_interval
  RETURN
END SUBROUTINE nl_get_restart_interval
SUBROUTINE nl_get_io_form_input ( id_id , io_form_input )
  integer , INTENT(OUT) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_input: io_form_input applies to all domains. First arg ignored.')
  ENDIF
  io_form_input = model_config_rec%io_form_input
  RETURN
END SUBROUTINE nl_get_io_form_input
SUBROUTINE nl_get_io_form_history ( id_id , io_form_history )
  integer , INTENT(OUT) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_history: io_form_history applies to all domains. First arg ignored.')
  ENDIF
  io_form_history = model_config_rec%io_form_history
  RETURN
END SUBROUTINE nl_get_io_form_history
SUBROUTINE nl_get_io_form_restart ( id_id , io_form_restart )
  integer , INTENT(OUT) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_restart: io_form_restart applies to all domains. First arg ignored.')
  ENDIF
  io_form_restart = model_config_rec%io_form_restart
  RETURN
END SUBROUTINE nl_get_io_form_restart
SUBROUTINE nl_get_io_form_boundary ( id_id , io_form_boundary )
  integer , INTENT(OUT) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_boundary: io_form_boundary applies to all domains. First arg ignored.')
  ENDIF
  io_form_boundary = model_config_rec%io_form_boundary
  RETURN
END SUBROUTINE nl_get_io_form_boundary
SUBROUTINE nl_get_debug_level ( id_id , debug_level )
  integer , INTENT(OUT) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_debug_level: debug_level applies to all domains. First arg ignored.')
  ENDIF
  debug_level = model_config_rec%debug_level
  RETURN
END SUBROUTINE nl_get_debug_level
SUBROUTINE nl_get_history_outname ( id_id , history_outname )
  character*256 , INTENT(OUT) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_history_outname: history_outname applies to all domains. First arg ignored.')
  ENDIF
  history_outname = model_config_rec%history_outname
  RETURN
END SUBROUTINE nl_get_history_outname
SUBROUTINE nl_get_auxhist1_outname ( id_id , auxhist1_outname )
  character*256 , INTENT(OUT) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist1_outname: auxhist1_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist1_outname = model_config_rec%auxhist1_outname
  RETURN
END SUBROUTINE nl_get_auxhist1_outname
SUBROUTINE nl_get_auxhist2_outname ( id_id , auxhist2_outname )
  character*256 , INTENT(OUT) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist2_outname: auxhist2_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist2_outname = model_config_rec%auxhist2_outname
  RETURN
END SUBROUTINE nl_get_auxhist2_outname
SUBROUTINE nl_get_auxhist3_outname ( id_id , auxhist3_outname )
  character*256 , INTENT(OUT) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist3_outname: auxhist3_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist3_outname = model_config_rec%auxhist3_outname
  RETURN
END SUBROUTINE nl_get_auxhist3_outname
SUBROUTINE nl_get_auxhist4_outname ( id_id , auxhist4_outname )
  character*256 , INTENT(OUT) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist4_outname: auxhist4_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist4_outname = model_config_rec%auxhist4_outname
  RETURN
END SUBROUTINE nl_get_auxhist4_outname
SUBROUTINE nl_get_auxhist5_outname ( id_id , auxhist5_outname )
  character*256 , INTENT(OUT) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist5_outname: auxhist5_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist5_outname = model_config_rec%auxhist5_outname
  RETURN
END SUBROUTINE nl_get_auxhist5_outname
SUBROUTINE nl_get_history_inname ( id_id , history_inname )
  character*256 , INTENT(OUT) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_history_inname: history_inname applies to all domains. First arg ignored.')
  ENDIF
  history_inname = model_config_rec%history_inname
  RETURN
END SUBROUTINE nl_get_history_inname
SUBROUTINE nl_get_auxhist1_inname ( id_id , auxhist1_inname )
  character*256 , INTENT(OUT) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist1_inname: auxhist1_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist1_inname = model_config_rec%auxhist1_inname
  RETURN
END SUBROUTINE nl_get_auxhist1_inname
SUBROUTINE nl_get_auxhist2_inname ( id_id , auxhist2_inname )
  character*256 , INTENT(OUT) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist2_inname: auxhist2_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist2_inname = model_config_rec%auxhist2_inname
  RETURN
END SUBROUTINE nl_get_auxhist2_inname
SUBROUTINE nl_get_auxhist3_inname ( id_id , auxhist3_inname )
  character*256 , INTENT(OUT) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist3_inname: auxhist3_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist3_inname = model_config_rec%auxhist3_inname
  RETURN
END SUBROUTINE nl_get_auxhist3_inname
SUBROUTINE nl_get_auxhist4_inname ( id_id , auxhist4_inname )
  character*256 , INTENT(OUT) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist4_inname: auxhist4_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist4_inname = model_config_rec%auxhist4_inname
  RETURN
END SUBROUTINE nl_get_auxhist4_inname
SUBROUTINE nl_get_auxhist5_inname ( id_id , auxhist5_inname )
  character*256 , INTENT(OUT) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxhist5_inname: auxhist5_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist5_inname = model_config_rec%auxhist5_inname
  RETURN
END SUBROUTINE nl_get_auxhist5_inname
SUBROUTINE nl_get_auxinput1_outname ( id_id , auxinput1_outname )
  character*256 , INTENT(OUT) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput1_outname: auxinput1_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput1_outname = model_config_rec%auxinput1_outname
  RETURN
END SUBROUTINE nl_get_auxinput1_outname
SUBROUTINE nl_get_auxinput2_outname ( id_id , auxinput2_outname )
  character*256 , INTENT(OUT) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput2_outname: auxinput2_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput2_outname = model_config_rec%auxinput2_outname
  RETURN
END SUBROUTINE nl_get_auxinput2_outname
SUBROUTINE nl_get_auxinput3_outname ( id_id , auxinput3_outname )
  character*256 , INTENT(OUT) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput3_outname: auxinput3_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput3_outname = model_config_rec%auxinput3_outname
  RETURN
END SUBROUTINE nl_get_auxinput3_outname
SUBROUTINE nl_get_auxinput4_outname ( id_id , auxinput4_outname )
  character*256 , INTENT(OUT) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput4_outname: auxinput4_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput4_outname = model_config_rec%auxinput4_outname
  RETURN
END SUBROUTINE nl_get_auxinput4_outname
SUBROUTINE nl_get_auxinput5_outname ( id_id , auxinput5_outname )
  character*256 , INTENT(OUT) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput5_outname: auxinput5_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput5_outname = model_config_rec%auxinput5_outname
  RETURN
END SUBROUTINE nl_get_auxinput5_outname
SUBROUTINE nl_get_auxinput1_inname ( id_id , auxinput1_inname )
  character*256 , INTENT(OUT) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput1_inname: auxinput1_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput1_inname = model_config_rec%auxinput1_inname
  RETURN
END SUBROUTINE nl_get_auxinput1_inname
SUBROUTINE nl_get_auxinput2_inname ( id_id , auxinput2_inname )
  character*256 , INTENT(OUT) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput2_inname: auxinput2_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput2_inname = model_config_rec%auxinput2_inname
  RETURN
END SUBROUTINE nl_get_auxinput2_inname
SUBROUTINE nl_get_auxinput3_inname ( id_id , auxinput3_inname )
  character*256 , INTENT(OUT) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput3_inname: auxinput3_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput3_inname = model_config_rec%auxinput3_inname
  RETURN
END SUBROUTINE nl_get_auxinput3_inname
SUBROUTINE nl_get_auxinput4_inname ( id_id , auxinput4_inname )
  character*256 , INTENT(OUT) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput4_inname: auxinput4_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput4_inname = model_config_rec%auxinput4_inname
  RETURN
END SUBROUTINE nl_get_auxinput4_inname
SUBROUTINE nl_get_auxinput5_inname ( id_id , auxinput5_inname )
  character*256 , INTENT(OUT) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_auxinput5_inname: auxinput5_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput5_inname = model_config_rec%auxinput5_inname
  RETURN
END SUBROUTINE nl_get_auxinput5_inname
SUBROUTINE nl_get_history_interval_mo ( id_id , history_interval_mo )
  integer , INTENT(OUT) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4090 , emess)
  ENDIF
  history_interval_mo = model_config_rec%history_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_mo
SUBROUTINE nl_get_history_interval_d ( id_id , history_interval_d )
  integer , INTENT(OUT) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4101 , emess)
  ENDIF
  history_interval_d = model_config_rec%history_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_d
SUBROUTINE nl_get_history_interval_h ( id_id , history_interval_h )
  integer , INTENT(OUT) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4112 , emess)
  ENDIF
  history_interval_h = model_config_rec%history_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_h
SUBROUTINE nl_get_history_interval_m ( id_id , history_interval_m )
  integer , INTENT(OUT) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4123 , emess)
  ENDIF
  history_interval_m = model_config_rec%history_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_m
SUBROUTINE nl_get_history_interval_s ( id_id , history_interval_s )
  integer , INTENT(OUT) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4134 , emess)
  ENDIF
  history_interval_s = model_config_rec%history_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_s
SUBROUTINE nl_get_inputout_interval_mo ( id_id , inputout_interval_mo )
  integer , INTENT(OUT) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4145 , emess)
  ENDIF
  inputout_interval_mo = model_config_rec%inputout_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_mo
SUBROUTINE nl_get_inputout_interval_d ( id_id , inputout_interval_d )
  integer , INTENT(OUT) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4156 , emess)
  ENDIF
  inputout_interval_d = model_config_rec%inputout_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_d
SUBROUTINE nl_get_inputout_interval_h ( id_id , inputout_interval_h )
  integer , INTENT(OUT) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4167 , emess)
  ENDIF
  inputout_interval_h = model_config_rec%inputout_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_h
SUBROUTINE nl_get_inputout_interval_m ( id_id , inputout_interval_m )
  integer , INTENT(OUT) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4178 , emess)
  ENDIF
  inputout_interval_m = model_config_rec%inputout_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_m
SUBROUTINE nl_get_inputout_interval_s ( id_id , inputout_interval_s )
  integer , INTENT(OUT) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4189 , emess)
  ENDIF
  inputout_interval_s = model_config_rec%inputout_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_s
SUBROUTINE nl_get_inputout_interval ( id_id , inputout_interval )
  integer , INTENT(OUT) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4200 , emess)
  ENDIF
  inputout_interval = model_config_rec%inputout_interval(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval
SUBROUTINE nl_get_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  integer , INTENT(OUT) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4211 , emess)
  ENDIF
  auxhist1_interval_mo = model_config_rec%auxhist1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_mo
SUBROUTINE nl_get_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  integer , INTENT(OUT) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4222 , emess)
  ENDIF
  auxhist1_interval_d = model_config_rec%auxhist1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_d
SUBROUTINE nl_get_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  integer , INTENT(OUT) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4233 , emess)
  ENDIF
  auxhist1_interval_h = model_config_rec%auxhist1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_h
SUBROUTINE nl_get_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  integer , INTENT(OUT) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4244 , emess)
  ENDIF
  auxhist1_interval_m = model_config_rec%auxhist1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_m
SUBROUTINE nl_get_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  integer , INTENT(OUT) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4255 , emess)
  ENDIF
  auxhist1_interval_s = model_config_rec%auxhist1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_s
SUBROUTINE nl_get_auxhist1_interval ( id_id , auxhist1_interval )
  integer , INTENT(OUT) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4266 , emess)
  ENDIF
  auxhist1_interval = model_config_rec%auxhist1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval
SUBROUTINE nl_get_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  integer , INTENT(OUT) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4277 , emess)
  ENDIF
  auxhist2_interval_mo = model_config_rec%auxhist2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_mo
SUBROUTINE nl_get_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  integer , INTENT(OUT) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4288 , emess)
  ENDIF
  auxhist2_interval_d = model_config_rec%auxhist2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_d
SUBROUTINE nl_get_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  integer , INTENT(OUT) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4299 , emess)
  ENDIF
  auxhist2_interval_h = model_config_rec%auxhist2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_h
SUBROUTINE nl_get_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  integer , INTENT(OUT) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4310 , emess)
  ENDIF
  auxhist2_interval_m = model_config_rec%auxhist2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_m
SUBROUTINE nl_get_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  integer , INTENT(OUT) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4321 , emess)
  ENDIF
  auxhist2_interval_s = model_config_rec%auxhist2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_s
SUBROUTINE nl_get_auxhist2_interval ( id_id , auxhist2_interval )
  integer , INTENT(OUT) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4332 , emess)
  ENDIF
  auxhist2_interval = model_config_rec%auxhist2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval
SUBROUTINE nl_get_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  integer , INTENT(OUT) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4343 , emess)
  ENDIF
  auxhist3_interval_mo = model_config_rec%auxhist3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_mo
SUBROUTINE nl_get_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  integer , INTENT(OUT) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4354 , emess)
  ENDIF
  auxhist3_interval_d = model_config_rec%auxhist3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_d
SUBROUTINE nl_get_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  integer , INTENT(OUT) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4365 , emess)
  ENDIF
  auxhist3_interval_h = model_config_rec%auxhist3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_h
SUBROUTINE nl_get_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  integer , INTENT(OUT) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4376 , emess)
  ENDIF
  auxhist3_interval_m = model_config_rec%auxhist3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_m
SUBROUTINE nl_get_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  integer , INTENT(OUT) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4387 , emess)
  ENDIF
  auxhist3_interval_s = model_config_rec%auxhist3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_s
SUBROUTINE nl_get_auxhist3_interval ( id_id , auxhist3_interval )
  integer , INTENT(OUT) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4398 , emess)
  ENDIF
  auxhist3_interval = model_config_rec%auxhist3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval
SUBROUTINE nl_get_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  integer , INTENT(OUT) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4409 , emess)
  ENDIF
  auxhist4_interval_mo = model_config_rec%auxhist4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_mo
SUBROUTINE nl_get_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  integer , INTENT(OUT) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4420 , emess)
  ENDIF
  auxhist4_interval_d = model_config_rec%auxhist4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_d
SUBROUTINE nl_get_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  integer , INTENT(OUT) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4431 , emess)
  ENDIF
  auxhist4_interval_h = model_config_rec%auxhist4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_h
SUBROUTINE nl_get_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  integer , INTENT(OUT) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4442 , emess)
  ENDIF
  auxhist4_interval_m = model_config_rec%auxhist4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_m
SUBROUTINE nl_get_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  integer , INTENT(OUT) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4453 , emess)
  ENDIF
  auxhist4_interval_s = model_config_rec%auxhist4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_s
SUBROUTINE nl_get_auxhist4_interval ( id_id , auxhist4_interval )
  integer , INTENT(OUT) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4464 , emess)
  ENDIF
  auxhist4_interval = model_config_rec%auxhist4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval
SUBROUTINE nl_get_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  integer , INTENT(OUT) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4475 , emess)
  ENDIF
  auxhist5_interval_mo = model_config_rec%auxhist5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_mo
SUBROUTINE nl_get_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  integer , INTENT(OUT) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4486 , emess)
  ENDIF
  auxhist5_interval_d = model_config_rec%auxhist5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_d
SUBROUTINE nl_get_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  integer , INTENT(OUT) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4497 , emess)
  ENDIF
  auxhist5_interval_h = model_config_rec%auxhist5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_h
SUBROUTINE nl_get_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  integer , INTENT(OUT) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4508 , emess)
  ENDIF
  auxhist5_interval_m = model_config_rec%auxhist5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_m
SUBROUTINE nl_get_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  integer , INTENT(OUT) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4519 , emess)
  ENDIF
  auxhist5_interval_s = model_config_rec%auxhist5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_s
SUBROUTINE nl_get_auxhist5_interval ( id_id , auxhist5_interval )
  integer , INTENT(OUT) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4530 , emess)
  ENDIF
  auxhist5_interval = model_config_rec%auxhist5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval
SUBROUTINE nl_get_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  integer , INTENT(OUT) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4541 , emess)
  ENDIF
  auxinput1_interval_mo = model_config_rec%auxinput1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_mo
SUBROUTINE nl_get_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  integer , INTENT(OUT) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4552 , emess)
  ENDIF
  auxinput1_interval_d = model_config_rec%auxinput1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_d
SUBROUTINE nl_get_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  integer , INTENT(OUT) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4563 , emess)
  ENDIF
  auxinput1_interval_h = model_config_rec%auxinput1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_h
SUBROUTINE nl_get_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  integer , INTENT(OUT) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4574 , emess)
  ENDIF
  auxinput1_interval_m = model_config_rec%auxinput1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_m
SUBROUTINE nl_get_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  integer , INTENT(OUT) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4585 , emess)
  ENDIF
  auxinput1_interval_s = model_config_rec%auxinput1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_s
SUBROUTINE nl_get_auxinput1_interval ( id_id , auxinput1_interval )
  integer , INTENT(OUT) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4596 , emess)
  ENDIF
  auxinput1_interval = model_config_rec%auxinput1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval
SUBROUTINE nl_get_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  integer , INTENT(OUT) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4607 , emess)
  ENDIF
  auxinput2_interval_mo = model_config_rec%auxinput2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_mo
SUBROUTINE nl_get_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  integer , INTENT(OUT) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4618 , emess)
  ENDIF
  auxinput2_interval_d = model_config_rec%auxinput2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_d
SUBROUTINE nl_get_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  integer , INTENT(OUT) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4629 , emess)
  ENDIF
  auxinput2_interval_h = model_config_rec%auxinput2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_h
SUBROUTINE nl_get_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  integer , INTENT(OUT) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4640 , emess)
  ENDIF
  auxinput2_interval_m = model_config_rec%auxinput2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_m
SUBROUTINE nl_get_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  integer , INTENT(OUT) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4651 , emess)
  ENDIF
  auxinput2_interval_s = model_config_rec%auxinput2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_s
SUBROUTINE nl_get_auxinput2_interval ( id_id , auxinput2_interval )
  integer , INTENT(OUT) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4662 , emess)
  ENDIF
  auxinput2_interval = model_config_rec%auxinput2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval
SUBROUTINE nl_get_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  integer , INTENT(OUT) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4673 , emess)
  ENDIF
  auxinput3_interval_mo = model_config_rec%auxinput3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_mo
SUBROUTINE nl_get_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  integer , INTENT(OUT) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4684 , emess)
  ENDIF
  auxinput3_interval_d = model_config_rec%auxinput3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_d
SUBROUTINE nl_get_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  integer , INTENT(OUT) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4695 , emess)
  ENDIF
  auxinput3_interval_h = model_config_rec%auxinput3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_h
SUBROUTINE nl_get_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  integer , INTENT(OUT) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4706 , emess)
  ENDIF
  auxinput3_interval_m = model_config_rec%auxinput3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_m
SUBROUTINE nl_get_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  integer , INTENT(OUT) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4717 , emess)
  ENDIF
  auxinput3_interval_s = model_config_rec%auxinput3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_s
SUBROUTINE nl_get_auxinput3_interval ( id_id , auxinput3_interval )
  integer , INTENT(OUT) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4728 , emess)
  ENDIF
  auxinput3_interval = model_config_rec%auxinput3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval
SUBROUTINE nl_get_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  integer , INTENT(OUT) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4739 , emess)
  ENDIF
  auxinput4_interval_mo = model_config_rec%auxinput4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_mo
SUBROUTINE nl_get_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  integer , INTENT(OUT) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4750 , emess)
  ENDIF
  auxinput4_interval_d = model_config_rec%auxinput4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_d
SUBROUTINE nl_get_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  integer , INTENT(OUT) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4761 , emess)
  ENDIF
  auxinput4_interval_h = model_config_rec%auxinput4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_h
SUBROUTINE nl_get_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  integer , INTENT(OUT) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4772 , emess)
  ENDIF
  auxinput4_interval_m = model_config_rec%auxinput4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_m
SUBROUTINE nl_get_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  integer , INTENT(OUT) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4783 , emess)
  ENDIF
  auxinput4_interval_s = model_config_rec%auxinput4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_s
SUBROUTINE nl_get_auxinput4_interval ( id_id , auxinput4_interval )
  integer , INTENT(OUT) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4794 , emess)
  ENDIF
  auxinput4_interval = model_config_rec%auxinput4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval
SUBROUTINE nl_get_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  integer , INTENT(OUT) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4805 , emess)
  ENDIF
  auxinput5_interval_mo = model_config_rec%auxinput5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_mo
SUBROUTINE nl_get_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  integer , INTENT(OUT) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4816 , emess)
  ENDIF
  auxinput5_interval_d = model_config_rec%auxinput5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_d
SUBROUTINE nl_get_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  integer , INTENT(OUT) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4827 , emess)
  ENDIF
  auxinput5_interval_h = model_config_rec%auxinput5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_h
SUBROUTINE nl_get_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  integer , INTENT(OUT) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4838 , emess)
  ENDIF
  auxinput5_interval_m = model_config_rec%auxinput5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_m
SUBROUTINE nl_get_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  integer , INTENT(OUT) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4849 , emess)
  ENDIF
  auxinput5_interval_s = model_config_rec%auxinput5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_s
SUBROUTINE nl_get_auxinput5_interval ( id_id , auxinput5_interval )
  integer , INTENT(OUT) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4860 , emess)
  ENDIF
  auxinput5_interval = model_config_rec%auxinput5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval
SUBROUTINE nl_get_restart_interval_mo ( id_id , restart_interval_mo )
  integer , INTENT(OUT) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval_mo: restart_interval_mo applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_mo = model_config_rec%restart_interval_mo
  RETURN
END SUBROUTINE nl_get_restart_interval_mo
SUBROUTINE nl_get_restart_interval_d ( id_id , restart_interval_d )
  integer , INTENT(OUT) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval_d: restart_interval_d applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_d = model_config_rec%restart_interval_d
  RETURN
END SUBROUTINE nl_get_restart_interval_d
SUBROUTINE nl_get_restart_interval_h ( id_id , restart_interval_h )
  integer , INTENT(OUT) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval_h: restart_interval_h applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_h = model_config_rec%restart_interval_h
  RETURN
END SUBROUTINE nl_get_restart_interval_h
SUBROUTINE nl_get_restart_interval_m ( id_id , restart_interval_m )
  integer , INTENT(OUT) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval_m: restart_interval_m applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_m = model_config_rec%restart_interval_m
  RETURN
END SUBROUTINE nl_get_restart_interval_m
SUBROUTINE nl_get_restart_interval_s ( id_id , restart_interval_s )
  integer , INTENT(OUT) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_interval_s: restart_interval_s applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_s = model_config_rec%restart_interval_s
  RETURN
END SUBROUTINE nl_get_restart_interval_s
SUBROUTINE nl_get_history_begin_y ( id_id , history_begin_y )
  integer , INTENT(OUT) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4921 , emess)
  ENDIF
  history_begin_y = model_config_rec%history_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_y
SUBROUTINE nl_get_history_begin_mo ( id_id , history_begin_mo )
  integer , INTENT(OUT) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4932 , emess)
  ENDIF
  history_begin_mo = model_config_rec%history_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_mo
SUBROUTINE nl_get_history_begin_d ( id_id , history_begin_d )
  integer , INTENT(OUT) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4943 , emess)
  ENDIF
  history_begin_d = model_config_rec%history_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_d
SUBROUTINE nl_get_history_begin_h ( id_id , history_begin_h )
  integer , INTENT(OUT) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4954 , emess)
  ENDIF
  history_begin_h = model_config_rec%history_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_h
SUBROUTINE nl_get_history_begin_m ( id_id , history_begin_m )
  integer , INTENT(OUT) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4965 , emess)
  ENDIF
  history_begin_m = model_config_rec%history_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_m
SUBROUTINE nl_get_history_begin_s ( id_id , history_begin_s )
  integer , INTENT(OUT) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4976 , emess)
  ENDIF
  history_begin_s = model_config_rec%history_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_s
SUBROUTINE nl_get_inputout_begin_y ( id_id , inputout_begin_y )
  integer , INTENT(OUT) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4987 , emess)
  ENDIF
  inputout_begin_y = model_config_rec%inputout_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_y
SUBROUTINE nl_get_inputout_begin_mo ( id_id , inputout_begin_mo )
  integer , INTENT(OUT) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 4998 , emess)
  ENDIF
  inputout_begin_mo = model_config_rec%inputout_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_mo
SUBROUTINE nl_get_inputout_begin_d ( id_id , inputout_begin_d )
  integer , INTENT(OUT) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5009 , emess)
  ENDIF
  inputout_begin_d = model_config_rec%inputout_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_d
SUBROUTINE nl_get_inputout_begin_h ( id_id , inputout_begin_h )
  integer , INTENT(OUT) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5020 , emess)
  ENDIF
  inputout_begin_h = model_config_rec%inputout_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_h
SUBROUTINE nl_get_inputout_begin_m ( id_id , inputout_begin_m )
  integer , INTENT(OUT) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5031 , emess)
  ENDIF
  inputout_begin_m = model_config_rec%inputout_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_m
SUBROUTINE nl_get_inputout_begin_s ( id_id , inputout_begin_s )
  integer , INTENT(OUT) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5042 , emess)
  ENDIF
  inputout_begin_s = model_config_rec%inputout_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_s
SUBROUTINE nl_get_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  integer , INTENT(OUT) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5053 , emess)
  ENDIF
  auxhist1_begin_y = model_config_rec%auxhist1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_y
SUBROUTINE nl_get_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  integer , INTENT(OUT) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5064 , emess)
  ENDIF
  auxhist1_begin_mo = model_config_rec%auxhist1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_mo
SUBROUTINE nl_get_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  integer , INTENT(OUT) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5075 , emess)
  ENDIF
  auxhist1_begin_d = model_config_rec%auxhist1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_d
SUBROUTINE nl_get_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  integer , INTENT(OUT) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5086 , emess)
  ENDIF
  auxhist1_begin_h = model_config_rec%auxhist1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_h
SUBROUTINE nl_get_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  integer , INTENT(OUT) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5097 , emess)
  ENDIF
  auxhist1_begin_m = model_config_rec%auxhist1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_m
SUBROUTINE nl_get_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  integer , INTENT(OUT) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5108 , emess)
  ENDIF
  auxhist1_begin_s = model_config_rec%auxhist1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_s
SUBROUTINE nl_get_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  integer , INTENT(OUT) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5119 , emess)
  ENDIF
  auxhist2_begin_y = model_config_rec%auxhist2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_y
SUBROUTINE nl_get_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  integer , INTENT(OUT) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5130 , emess)
  ENDIF
  auxhist2_begin_mo = model_config_rec%auxhist2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_mo
SUBROUTINE nl_get_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  integer , INTENT(OUT) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5141 , emess)
  ENDIF
  auxhist2_begin_d = model_config_rec%auxhist2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_d
SUBROUTINE nl_get_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  integer , INTENT(OUT) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5152 , emess)
  ENDIF
  auxhist2_begin_h = model_config_rec%auxhist2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_h
SUBROUTINE nl_get_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  integer , INTENT(OUT) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5163 , emess)
  ENDIF
  auxhist2_begin_m = model_config_rec%auxhist2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_m
SUBROUTINE nl_get_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  integer , INTENT(OUT) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5174 , emess)
  ENDIF
  auxhist2_begin_s = model_config_rec%auxhist2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_s
SUBROUTINE nl_get_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  integer , INTENT(OUT) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5185 , emess)
  ENDIF
  auxhist3_begin_y = model_config_rec%auxhist3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_y
SUBROUTINE nl_get_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  integer , INTENT(OUT) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5196 , emess)
  ENDIF
  auxhist3_begin_mo = model_config_rec%auxhist3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_mo
SUBROUTINE nl_get_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  integer , INTENT(OUT) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5207 , emess)
  ENDIF
  auxhist3_begin_d = model_config_rec%auxhist3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_d
SUBROUTINE nl_get_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  integer , INTENT(OUT) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5218 , emess)
  ENDIF
  auxhist3_begin_h = model_config_rec%auxhist3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_h
SUBROUTINE nl_get_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  integer , INTENT(OUT) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5229 , emess)
  ENDIF
  auxhist3_begin_m = model_config_rec%auxhist3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_m
SUBROUTINE nl_get_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  integer , INTENT(OUT) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5240 , emess)
  ENDIF
  auxhist3_begin_s = model_config_rec%auxhist3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_s
SUBROUTINE nl_get_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  integer , INTENT(OUT) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5251 , emess)
  ENDIF
  auxhist4_begin_y = model_config_rec%auxhist4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_y
SUBROUTINE nl_get_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  integer , INTENT(OUT) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5262 , emess)
  ENDIF
  auxhist4_begin_mo = model_config_rec%auxhist4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_mo
SUBROUTINE nl_get_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  integer , INTENT(OUT) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5273 , emess)
  ENDIF
  auxhist4_begin_d = model_config_rec%auxhist4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_d
SUBROUTINE nl_get_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  integer , INTENT(OUT) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5284 , emess)
  ENDIF
  auxhist4_begin_h = model_config_rec%auxhist4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_h
SUBROUTINE nl_get_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  integer , INTENT(OUT) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5295 , emess)
  ENDIF
  auxhist4_begin_m = model_config_rec%auxhist4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_m
SUBROUTINE nl_get_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  integer , INTENT(OUT) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5306 , emess)
  ENDIF
  auxhist4_begin_s = model_config_rec%auxhist4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_s
SUBROUTINE nl_get_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  integer , INTENT(OUT) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5317 , emess)
  ENDIF
  auxhist5_begin_y = model_config_rec%auxhist5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_y
SUBROUTINE nl_get_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  integer , INTENT(OUT) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5328 , emess)
  ENDIF
  auxhist5_begin_mo = model_config_rec%auxhist5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_mo
SUBROUTINE nl_get_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  integer , INTENT(OUT) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5339 , emess)
  ENDIF
  auxhist5_begin_d = model_config_rec%auxhist5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_d
SUBROUTINE nl_get_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  integer , INTENT(OUT) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5350 , emess)
  ENDIF
  auxhist5_begin_h = model_config_rec%auxhist5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_h
SUBROUTINE nl_get_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  integer , INTENT(OUT) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5361 , emess)
  ENDIF
  auxhist5_begin_m = model_config_rec%auxhist5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_m
SUBROUTINE nl_get_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  integer , INTENT(OUT) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5372 , emess)
  ENDIF
  auxhist5_begin_s = model_config_rec%auxhist5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_s
SUBROUTINE nl_get_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  integer , INTENT(OUT) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5383 , emess)
  ENDIF
  auxinput1_begin_y = model_config_rec%auxinput1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_y
SUBROUTINE nl_get_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  integer , INTENT(OUT) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5394 , emess)
  ENDIF
  auxinput1_begin_mo = model_config_rec%auxinput1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_mo
SUBROUTINE nl_get_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  integer , INTENT(OUT) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5405 , emess)
  ENDIF
  auxinput1_begin_d = model_config_rec%auxinput1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_d
SUBROUTINE nl_get_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  integer , INTENT(OUT) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5416 , emess)
  ENDIF
  auxinput1_begin_h = model_config_rec%auxinput1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_h
SUBROUTINE nl_get_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  integer , INTENT(OUT) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5427 , emess)
  ENDIF
  auxinput1_begin_m = model_config_rec%auxinput1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_m
SUBROUTINE nl_get_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  integer , INTENT(OUT) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5438 , emess)
  ENDIF
  auxinput1_begin_s = model_config_rec%auxinput1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_s
SUBROUTINE nl_get_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  integer , INTENT(OUT) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5449 , emess)
  ENDIF
  auxinput2_begin_y = model_config_rec%auxinput2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_y
SUBROUTINE nl_get_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  integer , INTENT(OUT) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5460 , emess)
  ENDIF
  auxinput2_begin_mo = model_config_rec%auxinput2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_mo
SUBROUTINE nl_get_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  integer , INTENT(OUT) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5471 , emess)
  ENDIF
  auxinput2_begin_d = model_config_rec%auxinput2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_d
SUBROUTINE nl_get_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  integer , INTENT(OUT) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5482 , emess)
  ENDIF
  auxinput2_begin_h = model_config_rec%auxinput2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_h
SUBROUTINE nl_get_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  integer , INTENT(OUT) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5493 , emess)
  ENDIF
  auxinput2_begin_m = model_config_rec%auxinput2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_m
SUBROUTINE nl_get_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  integer , INTENT(OUT) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5504 , emess)
  ENDIF
  auxinput2_begin_s = model_config_rec%auxinput2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_s
SUBROUTINE nl_get_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  integer , INTENT(OUT) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5515 , emess)
  ENDIF
  auxinput3_begin_y = model_config_rec%auxinput3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_y
SUBROUTINE nl_get_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  integer , INTENT(OUT) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5526 , emess)
  ENDIF
  auxinput3_begin_mo = model_config_rec%auxinput3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_mo
SUBROUTINE nl_get_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  integer , INTENT(OUT) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5537 , emess)
  ENDIF
  auxinput3_begin_d = model_config_rec%auxinput3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_d
SUBROUTINE nl_get_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  integer , INTENT(OUT) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5548 , emess)
  ENDIF
  auxinput3_begin_h = model_config_rec%auxinput3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_h
SUBROUTINE nl_get_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  integer , INTENT(OUT) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5559 , emess)
  ENDIF
  auxinput3_begin_m = model_config_rec%auxinput3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_m
SUBROUTINE nl_get_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  integer , INTENT(OUT) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5570 , emess)
  ENDIF
  auxinput3_begin_s = model_config_rec%auxinput3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_s
SUBROUTINE nl_get_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  integer , INTENT(OUT) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5581 , emess)
  ENDIF
  auxinput4_begin_y = model_config_rec%auxinput4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_y
SUBROUTINE nl_get_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  integer , INTENT(OUT) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5592 , emess)
  ENDIF
  auxinput4_begin_mo = model_config_rec%auxinput4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_mo
SUBROUTINE nl_get_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  integer , INTENT(OUT) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5603 , emess)
  ENDIF
  auxinput4_begin_d = model_config_rec%auxinput4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_d
SUBROUTINE nl_get_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  integer , INTENT(OUT) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5614 , emess)
  ENDIF
  auxinput4_begin_h = model_config_rec%auxinput4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_h
SUBROUTINE nl_get_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  integer , INTENT(OUT) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5625 , emess)
  ENDIF
  auxinput4_begin_m = model_config_rec%auxinput4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_m
SUBROUTINE nl_get_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  integer , INTENT(OUT) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5636 , emess)
  ENDIF
  auxinput4_begin_s = model_config_rec%auxinput4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_s
SUBROUTINE nl_get_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  integer , INTENT(OUT) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5647 , emess)
  ENDIF
  auxinput5_begin_y = model_config_rec%auxinput5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_y
SUBROUTINE nl_get_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  integer , INTENT(OUT) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5658 , emess)
  ENDIF
  auxinput5_begin_mo = model_config_rec%auxinput5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_mo
SUBROUTINE nl_get_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  integer , INTENT(OUT) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5669 , emess)
  ENDIF
  auxinput5_begin_d = model_config_rec%auxinput5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_d
SUBROUTINE nl_get_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  integer , INTENT(OUT) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5680 , emess)
  ENDIF
  auxinput5_begin_h = model_config_rec%auxinput5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_h
SUBROUTINE nl_get_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  integer , INTENT(OUT) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5691 , emess)
  ENDIF
  auxinput5_begin_m = model_config_rec%auxinput5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_m
SUBROUTINE nl_get_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  integer , INTENT(OUT) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5702 , emess)
  ENDIF
  auxinput5_begin_s = model_config_rec%auxinput5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_s
SUBROUTINE nl_get_restart_begin_y ( id_id , restart_begin_y )
  integer , INTENT(OUT) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_y: restart_begin_y applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_y = model_config_rec%restart_begin_y
  RETURN
END SUBROUTINE nl_get_restart_begin_y
SUBROUTINE nl_get_restart_begin_mo ( id_id , restart_begin_mo )
  integer , INTENT(OUT) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_mo: restart_begin_mo applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_mo = model_config_rec%restart_begin_mo
  RETURN
END SUBROUTINE nl_get_restart_begin_mo
SUBROUTINE nl_get_restart_begin_d ( id_id , restart_begin_d )
  integer , INTENT(OUT) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_d: restart_begin_d applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_d = model_config_rec%restart_begin_d
  RETURN
END SUBROUTINE nl_get_restart_begin_d
SUBROUTINE nl_get_restart_begin_h ( id_id , restart_begin_h )
  integer , INTENT(OUT) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_h: restart_begin_h applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_h = model_config_rec%restart_begin_h
  RETURN
END SUBROUTINE nl_get_restart_begin_h
SUBROUTINE nl_get_restart_begin_m ( id_id , restart_begin_m )
  integer , INTENT(OUT) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_m: restart_begin_m applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_m = model_config_rec%restart_begin_m
  RETURN
END SUBROUTINE nl_get_restart_begin_m
SUBROUTINE nl_get_restart_begin_s ( id_id , restart_begin_s )
  integer , INTENT(OUT) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_restart_begin_s: restart_begin_s applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_s = model_config_rec%restart_begin_s
  RETURN
END SUBROUTINE nl_get_restart_begin_s
SUBROUTINE nl_get_history_end_y ( id_id , history_end_y )
  integer , INTENT(OUT) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5773 , emess)
  ENDIF
  history_end_y = model_config_rec%history_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_y
SUBROUTINE nl_get_history_end_mo ( id_id , history_end_mo )
  integer , INTENT(OUT) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5784 , emess)
  ENDIF
  history_end_mo = model_config_rec%history_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_mo
SUBROUTINE nl_get_history_end_d ( id_id , history_end_d )
  integer , INTENT(OUT) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5795 , emess)
  ENDIF
  history_end_d = model_config_rec%history_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_d
SUBROUTINE nl_get_history_end_h ( id_id , history_end_h )
  integer , INTENT(OUT) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5806 , emess)
  ENDIF
  history_end_h = model_config_rec%history_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_h
SUBROUTINE nl_get_history_end_m ( id_id , history_end_m )
  integer , INTENT(OUT) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5817 , emess)
  ENDIF
  history_end_m = model_config_rec%history_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_m
SUBROUTINE nl_get_history_end_s ( id_id , history_end_s )
  integer , INTENT(OUT) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5828 , emess)
  ENDIF
  history_end_s = model_config_rec%history_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_s
SUBROUTINE nl_get_inputout_end_y ( id_id , inputout_end_y )
  integer , INTENT(OUT) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5839 , emess)
  ENDIF
  inputout_end_y = model_config_rec%inputout_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_y
SUBROUTINE nl_get_inputout_end_mo ( id_id , inputout_end_mo )
  integer , INTENT(OUT) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5850 , emess)
  ENDIF
  inputout_end_mo = model_config_rec%inputout_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_mo
SUBROUTINE nl_get_inputout_end_d ( id_id , inputout_end_d )
  integer , INTENT(OUT) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5861 , emess)
  ENDIF
  inputout_end_d = model_config_rec%inputout_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_d
SUBROUTINE nl_get_inputout_end_h ( id_id , inputout_end_h )
  integer , INTENT(OUT) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5872 , emess)
  ENDIF
  inputout_end_h = model_config_rec%inputout_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_h
SUBROUTINE nl_get_inputout_end_m ( id_id , inputout_end_m )
  integer , INTENT(OUT) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5883 , emess)
  ENDIF
  inputout_end_m = model_config_rec%inputout_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_m
SUBROUTINE nl_get_inputout_end_s ( id_id , inputout_end_s )
  integer , INTENT(OUT) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5894 , emess)
  ENDIF
  inputout_end_s = model_config_rec%inputout_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_s
SUBROUTINE nl_get_auxhist1_end_y ( id_id , auxhist1_end_y )
  integer , INTENT(OUT) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5905 , emess)
  ENDIF
  auxhist1_end_y = model_config_rec%auxhist1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_y
SUBROUTINE nl_get_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  integer , INTENT(OUT) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5916 , emess)
  ENDIF
  auxhist1_end_mo = model_config_rec%auxhist1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_mo
SUBROUTINE nl_get_auxhist1_end_d ( id_id , auxhist1_end_d )
  integer , INTENT(OUT) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5927 , emess)
  ENDIF
  auxhist1_end_d = model_config_rec%auxhist1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_d
SUBROUTINE nl_get_auxhist1_end_h ( id_id , auxhist1_end_h )
  integer , INTENT(OUT) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5938 , emess)
  ENDIF
  auxhist1_end_h = model_config_rec%auxhist1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_h
SUBROUTINE nl_get_auxhist1_end_m ( id_id , auxhist1_end_m )
  integer , INTENT(OUT) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5949 , emess)
  ENDIF
  auxhist1_end_m = model_config_rec%auxhist1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_m
SUBROUTINE nl_get_auxhist1_end_s ( id_id , auxhist1_end_s )
  integer , INTENT(OUT) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5960 , emess)
  ENDIF
  auxhist1_end_s = model_config_rec%auxhist1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_s
SUBROUTINE nl_get_auxhist2_end_y ( id_id , auxhist2_end_y )
  integer , INTENT(OUT) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5971 , emess)
  ENDIF
  auxhist2_end_y = model_config_rec%auxhist2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_y
SUBROUTINE nl_get_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  integer , INTENT(OUT) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5982 , emess)
  ENDIF
  auxhist2_end_mo = model_config_rec%auxhist2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_mo
SUBROUTINE nl_get_auxhist2_end_d ( id_id , auxhist2_end_d )
  integer , INTENT(OUT) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 5993 , emess)
  ENDIF
  auxhist2_end_d = model_config_rec%auxhist2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_d
SUBROUTINE nl_get_auxhist2_end_h ( id_id , auxhist2_end_h )
  integer , INTENT(OUT) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6004 , emess)
  ENDIF
  auxhist2_end_h = model_config_rec%auxhist2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_h
SUBROUTINE nl_get_auxhist2_end_m ( id_id , auxhist2_end_m )
  integer , INTENT(OUT) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6015 , emess)
  ENDIF
  auxhist2_end_m = model_config_rec%auxhist2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_m
SUBROUTINE nl_get_auxhist2_end_s ( id_id , auxhist2_end_s )
  integer , INTENT(OUT) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6026 , emess)
  ENDIF
  auxhist2_end_s = model_config_rec%auxhist2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_s
SUBROUTINE nl_get_auxhist3_end_y ( id_id , auxhist3_end_y )
  integer , INTENT(OUT) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6037 , emess)
  ENDIF
  auxhist3_end_y = model_config_rec%auxhist3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_y
SUBROUTINE nl_get_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  integer , INTENT(OUT) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6048 , emess)
  ENDIF
  auxhist3_end_mo = model_config_rec%auxhist3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_mo
SUBROUTINE nl_get_auxhist3_end_d ( id_id , auxhist3_end_d )
  integer , INTENT(OUT) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6059 , emess)
  ENDIF
  auxhist3_end_d = model_config_rec%auxhist3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_d
SUBROUTINE nl_get_auxhist3_end_h ( id_id , auxhist3_end_h )
  integer , INTENT(OUT) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6070 , emess)
  ENDIF
  auxhist3_end_h = model_config_rec%auxhist3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_h
SUBROUTINE nl_get_auxhist3_end_m ( id_id , auxhist3_end_m )
  integer , INTENT(OUT) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6081 , emess)
  ENDIF
  auxhist3_end_m = model_config_rec%auxhist3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_m
SUBROUTINE nl_get_auxhist3_end_s ( id_id , auxhist3_end_s )
  integer , INTENT(OUT) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6092 , emess)
  ENDIF
  auxhist3_end_s = model_config_rec%auxhist3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_s
SUBROUTINE nl_get_auxhist4_end_y ( id_id , auxhist4_end_y )
  integer , INTENT(OUT) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6103 , emess)
  ENDIF
  auxhist4_end_y = model_config_rec%auxhist4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_y
SUBROUTINE nl_get_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  integer , INTENT(OUT) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6114 , emess)
  ENDIF
  auxhist4_end_mo = model_config_rec%auxhist4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_mo
SUBROUTINE nl_get_auxhist4_end_d ( id_id , auxhist4_end_d )
  integer , INTENT(OUT) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6125 , emess)
  ENDIF
  auxhist4_end_d = model_config_rec%auxhist4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_d
SUBROUTINE nl_get_auxhist4_end_h ( id_id , auxhist4_end_h )
  integer , INTENT(OUT) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6136 , emess)
  ENDIF
  auxhist4_end_h = model_config_rec%auxhist4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_h
SUBROUTINE nl_get_auxhist4_end_m ( id_id , auxhist4_end_m )
  integer , INTENT(OUT) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6147 , emess)
  ENDIF
  auxhist4_end_m = model_config_rec%auxhist4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_m
SUBROUTINE nl_get_auxhist4_end_s ( id_id , auxhist4_end_s )
  integer , INTENT(OUT) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6158 , emess)
  ENDIF
  auxhist4_end_s = model_config_rec%auxhist4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_s
SUBROUTINE nl_get_auxhist5_end_y ( id_id , auxhist5_end_y )
  integer , INTENT(OUT) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6169 , emess)
  ENDIF
  auxhist5_end_y = model_config_rec%auxhist5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_y
SUBROUTINE nl_get_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  integer , INTENT(OUT) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6180 , emess)
  ENDIF
  auxhist5_end_mo = model_config_rec%auxhist5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_mo
SUBROUTINE nl_get_auxhist5_end_d ( id_id , auxhist5_end_d )
  integer , INTENT(OUT) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6191 , emess)
  ENDIF
  auxhist5_end_d = model_config_rec%auxhist5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_d
SUBROUTINE nl_get_auxhist5_end_h ( id_id , auxhist5_end_h )
  integer , INTENT(OUT) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6202 , emess)
  ENDIF
  auxhist5_end_h = model_config_rec%auxhist5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_h
SUBROUTINE nl_get_auxhist5_end_m ( id_id , auxhist5_end_m )
  integer , INTENT(OUT) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6213 , emess)
  ENDIF
  auxhist5_end_m = model_config_rec%auxhist5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_m
SUBROUTINE nl_get_auxhist5_end_s ( id_id , auxhist5_end_s )
  integer , INTENT(OUT) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6224 , emess)
  ENDIF
  auxhist5_end_s = model_config_rec%auxhist5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_s
SUBROUTINE nl_get_auxinput1_end_y ( id_id , auxinput1_end_y )
  integer , INTENT(OUT) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6235 , emess)
  ENDIF
  auxinput1_end_y = model_config_rec%auxinput1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_y
SUBROUTINE nl_get_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  integer , INTENT(OUT) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6246 , emess)
  ENDIF
  auxinput1_end_mo = model_config_rec%auxinput1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_mo
SUBROUTINE nl_get_auxinput1_end_d ( id_id , auxinput1_end_d )
  integer , INTENT(OUT) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6257 , emess)
  ENDIF
  auxinput1_end_d = model_config_rec%auxinput1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_d
SUBROUTINE nl_get_auxinput1_end_h ( id_id , auxinput1_end_h )
  integer , INTENT(OUT) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6268 , emess)
  ENDIF
  auxinput1_end_h = model_config_rec%auxinput1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_h
SUBROUTINE nl_get_auxinput1_end_m ( id_id , auxinput1_end_m )
  integer , INTENT(OUT) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6279 , emess)
  ENDIF
  auxinput1_end_m = model_config_rec%auxinput1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_m
SUBROUTINE nl_get_auxinput1_end_s ( id_id , auxinput1_end_s )
  integer , INTENT(OUT) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6290 , emess)
  ENDIF
  auxinput1_end_s = model_config_rec%auxinput1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_s
SUBROUTINE nl_get_auxinput2_end_y ( id_id , auxinput2_end_y )
  integer , INTENT(OUT) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6301 , emess)
  ENDIF
  auxinput2_end_y = model_config_rec%auxinput2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_y
SUBROUTINE nl_get_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  integer , INTENT(OUT) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6312 , emess)
  ENDIF
  auxinput2_end_mo = model_config_rec%auxinput2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_mo
SUBROUTINE nl_get_auxinput2_end_d ( id_id , auxinput2_end_d )
  integer , INTENT(OUT) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6323 , emess)
  ENDIF
  auxinput2_end_d = model_config_rec%auxinput2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_d
SUBROUTINE nl_get_auxinput2_end_h ( id_id , auxinput2_end_h )
  integer , INTENT(OUT) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6334 , emess)
  ENDIF
  auxinput2_end_h = model_config_rec%auxinput2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_h
SUBROUTINE nl_get_auxinput2_end_m ( id_id , auxinput2_end_m )
  integer , INTENT(OUT) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6345 , emess)
  ENDIF
  auxinput2_end_m = model_config_rec%auxinput2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_m
SUBROUTINE nl_get_auxinput2_end_s ( id_id , auxinput2_end_s )
  integer , INTENT(OUT) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6356 , emess)
  ENDIF
  auxinput2_end_s = model_config_rec%auxinput2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_s
SUBROUTINE nl_get_auxinput3_end_y ( id_id , auxinput3_end_y )
  integer , INTENT(OUT) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6367 , emess)
  ENDIF
  auxinput3_end_y = model_config_rec%auxinput3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_y
SUBROUTINE nl_get_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  integer , INTENT(OUT) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6378 , emess)
  ENDIF
  auxinput3_end_mo = model_config_rec%auxinput3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_mo
SUBROUTINE nl_get_auxinput3_end_d ( id_id , auxinput3_end_d )
  integer , INTENT(OUT) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6389 , emess)
  ENDIF
  auxinput3_end_d = model_config_rec%auxinput3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_d
SUBROUTINE nl_get_auxinput3_end_h ( id_id , auxinput3_end_h )
  integer , INTENT(OUT) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6400 , emess)
  ENDIF
  auxinput3_end_h = model_config_rec%auxinput3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_h
SUBROUTINE nl_get_auxinput3_end_m ( id_id , auxinput3_end_m )
  integer , INTENT(OUT) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6411 , emess)
  ENDIF
  auxinput3_end_m = model_config_rec%auxinput3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_m
SUBROUTINE nl_get_auxinput3_end_s ( id_id , auxinput3_end_s )
  integer , INTENT(OUT) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6422 , emess)
  ENDIF
  auxinput3_end_s = model_config_rec%auxinput3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_s
SUBROUTINE nl_get_auxinput4_end_y ( id_id , auxinput4_end_y )
  integer , INTENT(OUT) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6433 , emess)
  ENDIF
  auxinput4_end_y = model_config_rec%auxinput4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_y
SUBROUTINE nl_get_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  integer , INTENT(OUT) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6444 , emess)
  ENDIF
  auxinput4_end_mo = model_config_rec%auxinput4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_mo
SUBROUTINE nl_get_auxinput4_end_d ( id_id , auxinput4_end_d )
  integer , INTENT(OUT) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6455 , emess)
  ENDIF
  auxinput4_end_d = model_config_rec%auxinput4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_d
SUBROUTINE nl_get_auxinput4_end_h ( id_id , auxinput4_end_h )
  integer , INTENT(OUT) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6466 , emess)
  ENDIF
  auxinput4_end_h = model_config_rec%auxinput4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_h
SUBROUTINE nl_get_auxinput4_end_m ( id_id , auxinput4_end_m )
  integer , INTENT(OUT) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6477 , emess)
  ENDIF
  auxinput4_end_m = model_config_rec%auxinput4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_m
SUBROUTINE nl_get_auxinput4_end_s ( id_id , auxinput4_end_s )
  integer , INTENT(OUT) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6488 , emess)
  ENDIF
  auxinput4_end_s = model_config_rec%auxinput4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_s
SUBROUTINE nl_get_auxinput5_end_y ( id_id , auxinput5_end_y )
  integer , INTENT(OUT) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6499 , emess)
  ENDIF
  auxinput5_end_y = model_config_rec%auxinput5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_y
SUBROUTINE nl_get_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  integer , INTENT(OUT) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6510 , emess)
  ENDIF
  auxinput5_end_mo = model_config_rec%auxinput5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_mo
SUBROUTINE nl_get_auxinput5_end_d ( id_id , auxinput5_end_d )
  integer , INTENT(OUT) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6521 , emess)
  ENDIF
  auxinput5_end_d = model_config_rec%auxinput5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_d
SUBROUTINE nl_get_auxinput5_end_h ( id_id , auxinput5_end_h )
  integer , INTENT(OUT) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6532 , emess)
  ENDIF
  auxinput5_end_h = model_config_rec%auxinput5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_h
SUBROUTINE nl_get_auxinput5_end_m ( id_id , auxinput5_end_m )
  integer , INTENT(OUT) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6543 , emess)
  ENDIF
  auxinput5_end_m = model_config_rec%auxinput5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_m
SUBROUTINE nl_get_auxinput5_end_s ( id_id , auxinput5_end_s )
  integer , INTENT(OUT) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6554 , emess)
  ENDIF
  auxinput5_end_s = model_config_rec%auxinput5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_s
SUBROUTINE nl_get_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  integer , INTENT(OUT) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxinput1: io_form_auxinput1 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput1 = model_config_rec%io_form_auxinput1
  RETURN
END SUBROUTINE nl_get_io_form_auxinput1
SUBROUTINE nl_get_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  integer , INTENT(OUT) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxinput2: io_form_auxinput2 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput2 = model_config_rec%io_form_auxinput2
  RETURN
END SUBROUTINE nl_get_io_form_auxinput2
SUBROUTINE nl_get_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  integer , INTENT(OUT) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxinput3: io_form_auxinput3 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput3 = model_config_rec%io_form_auxinput3
  RETURN
END SUBROUTINE nl_get_io_form_auxinput3
SUBROUTINE nl_get_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  integer , INTENT(OUT) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxinput4: io_form_auxinput4 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput4 = model_config_rec%io_form_auxinput4
  RETURN
END SUBROUTINE nl_get_io_form_auxinput4
SUBROUTINE nl_get_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  integer , INTENT(OUT) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxinput5: io_form_auxinput5 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput5 = model_config_rec%io_form_auxinput5
  RETURN
END SUBROUTINE nl_get_io_form_auxinput5
SUBROUTINE nl_get_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  integer , INTENT(OUT) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxhist1: io_form_auxhist1 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist1 = model_config_rec%io_form_auxhist1
  RETURN
END SUBROUTINE nl_get_io_form_auxhist1
SUBROUTINE nl_get_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  integer , INTENT(OUT) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxhist2: io_form_auxhist2 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist2 = model_config_rec%io_form_auxhist2
  RETURN
END SUBROUTINE nl_get_io_form_auxhist2
SUBROUTINE nl_get_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  integer , INTENT(OUT) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxhist3: io_form_auxhist3 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist3 = model_config_rec%io_form_auxhist3
  RETURN
END SUBROUTINE nl_get_io_form_auxhist3
SUBROUTINE nl_get_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  integer , INTENT(OUT) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxhist4: io_form_auxhist4 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist4 = model_config_rec%io_form_auxhist4
  RETURN
END SUBROUTINE nl_get_io_form_auxhist4
SUBROUTINE nl_get_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  integer , INTENT(OUT) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_io_form_auxhist5: io_form_auxhist5 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist5 = model_config_rec%io_form_auxhist5
  RETURN
END SUBROUTINE nl_get_io_form_auxhist5
SUBROUTINE nl_get_julyr ( id_id , julyr )
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_julyr: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6665 , emess)
  ENDIF
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_julday: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6676 , emess)
  ENDIF
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gmt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6687 , emess)
  ENDIF
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_input_inname: input_inname applies to all domains. First arg ignored.')
  ENDIF
  input_inname = model_config_rec%input_inname
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_input_outname: input_outname applies to all domains. First arg ignored.')
  ENDIF
  input_outname = model_config_rec%input_outname
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_bdy_inname: bdy_inname applies to all domains. First arg ignored.')
  ENDIF
  bdy_inname = model_config_rec%bdy_inname
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_bdy_outname: bdy_outname applies to all domains. First arg ignored.')
  ENDIF
  bdy_outname = model_config_rec%bdy_outname
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_rst_inname: rst_inname applies to all domains. First arg ignored.')
  ENDIF
  rst_inname = model_config_rec%rst_inname
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_rst_outname: rst_outname applies to all domains. First arg ignored.')
  ENDIF
  rst_outname = model_config_rec%rst_outname
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_write_input: write_input applies to all domains. First arg ignored.')
  ENDIF
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_write_restart_at_0h: write_restart_at_0h applies to all domains. First arg ignored.')
  ENDIF
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_time_step ( id_id , time_step )
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_time_step: time_step applies to all domains. First arg ignored.')
  ENDIF
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_time_step_fract_num: time_step_fract_num applies to all domains. First arg ignored.')
  ENDIF
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_time_step_fract_den: time_step_fract_den applies to all domains. First arg ignored.')
  ENDIF
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_max_dom: max_dom applies to all domains. First arg ignored.')
  ENDIF
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_s_we ( id_id , s_we )
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6818 , emess)
  ENDIF
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6829 , emess)
  ENDIF
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6840 , emess)
  ENDIF
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6851 , emess)
  ENDIF
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6862 , emess)
  ENDIF
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6873 , emess)
  ENDIF
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_dx ( id_id , dx )
  real , INTENT(OUT) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dx: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6884 , emess)
  ENDIF
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  real , INTENT(OUT) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dy: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6895 , emess)
  ENDIF
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_grid_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6906 , emess)
  ENDIF
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6917 , emess)
  ENDIF
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_level ( id_id , level )
  integer , INTENT(OUT) :: level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_level: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6928 , emess)
  ENDIF
  level = model_config_rec%level(id_id)
  RETURN
END SUBROUTINE nl_get_level
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_i_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6939 , emess)
  ENDIF
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_j_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6950 , emess)
  ENDIF
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6961 , emess)
  ENDIF
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 6972 , emess)
  ENDIF
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_feedback: feedback applies to all domains. First arg ignored.')
  ENDIF
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_smooth_option: smooth_option applies to all domains. First arg ignored.')
  ENDIF
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_ztop ( id_id , ztop )
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ztop: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7003 , emess)
  ENDIF
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7014 , emess)
  ENDIF
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7025 , emess)
  ENDIF
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_shw: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7036 , emess)
  ENDIF
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_coral_dist ( id_id , coral_dist )
  integer , INTENT(OUT) :: coral_dist
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_coral_dist: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7047 , emess)
  ENDIF
  coral_dist = model_config_rec%coral_dist(id_id)
  RETURN
END SUBROUTINE nl_get_coral_dist
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_tile_sz_x: tile_sz_x applies to all domains. First arg ignored.')
  ENDIF
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_tile_sz_y: tile_sz_y applies to all domains. First arg ignored.')
  ENDIF
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_numtiles: numtiles applies to all domains. First arg ignored.')
  ENDIF
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_nproc_x: nproc_x applies to all domains. First arg ignored.')
  ENDIF
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_nproc_y: nproc_y applies to all domains. First arg ignored.')
  ENDIF
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_irand: irand applies to all domains. First arg ignored.')
  ENDIF
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  real , INTENT(OUT) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7118 , emess)
  ENDIF
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_num_moves: num_moves applies to all domains. First arg ignored.')
  ENDIF
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_move_id ( id_id , move_id )
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_id: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7139 , emess)
  ENDIF
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_interval: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7150 , emess)
  ENDIF
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_cd_x: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7161 , emess)
  ENDIF
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_cd_y: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7172 , emess)
  ENDIF
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_test_rk_step_prep ( id_id , test_rk_step_prep )
  logical , INTENT(OUT) :: test_rk_step_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_rk_step_prep: test_rk_step_prep applies to all domains. First arg ignored.')
  ENDIF
  test_rk_step_prep = model_config_rec%test_rk_step_prep
  RETURN
END SUBROUTINE nl_get_test_rk_step_prep
SUBROUTINE nl_get_test_init_zero_tendency ( id_id , test_init_zero_tendency )
  logical , INTENT(OUT) :: test_init_zero_tendency
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_init_zero_tendency: test_init_zero_tendency applies to all domains. First arg ignored.')
  ENDIF
  test_init_zero_tendency = model_config_rec%test_init_zero_tendency
  RETURN
END SUBROUTINE nl_get_test_init_zero_tendency
SUBROUTINE nl_get_test_phy_prep ( id_id , test_phy_prep )
  logical , INTENT(OUT) :: test_phy_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_phy_prep: test_phy_prep applies to all domains. First arg ignored.')
  ENDIF
  test_phy_prep = model_config_rec%test_phy_prep
  RETURN
END SUBROUTINE nl_get_test_phy_prep
SUBROUTINE nl_get_test_calculate_km_kh ( id_id , test_calculate_km_kh )
  logical , INTENT(OUT) :: test_calculate_km_kh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_calculate_km_kh: test_calculate_km_kh applies to all domains. First arg ignored.')
  ENDIF
  test_calculate_km_kh = model_config_rec%test_calculate_km_kh
  RETURN
END SUBROUTINE nl_get_test_calculate_km_kh
SUBROUTINE nl_get_test_rk_tendency ( id_id , test_rk_tendency )
  logical , INTENT(OUT) :: test_rk_tendency
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_rk_tendency: test_rk_tendency applies to all domains. First arg ignored.')
  ENDIF
  test_rk_tendency = model_config_rec%test_rk_tendency
  RETURN
END SUBROUTINE nl_get_test_rk_tendency
SUBROUTINE nl_get_test_advect_scalar ( id_id , test_advect_scalar )
  logical , INTENT(OUT) :: test_advect_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_advect_scalar: test_advect_scalar applies to all domains. First arg ignored.')
  ENDIF
  test_advect_scalar = model_config_rec%test_advect_scalar
  RETURN
END SUBROUTINE nl_get_test_advect_scalar
SUBROUTINE nl_get_test_relax_bdy_dry ( id_id , test_relax_bdy_dry )
  logical , INTENT(OUT) :: test_relax_bdy_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_relax_bdy_dry: test_relax_bdy_dry applies to all domains. First arg ignored.')
  ENDIF
  test_relax_bdy_dry = model_config_rec%test_relax_bdy_dry
  RETURN
END SUBROUTINE nl_get_test_relax_bdy_dry
SUBROUTINE nl_get_test_rk_addtend_dry ( id_id , test_rk_addtend_dry )
  logical , INTENT(OUT) :: test_rk_addtend_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_rk_addtend_dry: test_rk_addtend_dry applies to all domains. First arg ignored.')
  ENDIF
  test_rk_addtend_dry = model_config_rec%test_rk_addtend_dry
  RETURN
END SUBROUTINE nl_get_test_rk_addtend_dry
SUBROUTINE nl_get_test_spec_bdy_dry ( id_id , test_spec_bdy_dry )
  logical , INTENT(OUT) :: test_spec_bdy_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_spec_bdy_dry: test_spec_bdy_dry applies to all domains. First arg ignored.')
  ENDIF
  test_spec_bdy_dry = model_config_rec%test_spec_bdy_dry
  RETURN
END SUBROUTINE nl_get_test_spec_bdy_dry
SUBROUTINE nl_get_test_small_step_prep ( id_id , test_small_step_prep )
  logical , INTENT(OUT) :: test_small_step_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_small_step_prep: test_small_step_prep applies to all domains. First arg ignored.')
  ENDIF
  test_small_step_prep = model_config_rec%test_small_step_prep
  RETURN
END SUBROUTINE nl_get_test_small_step_prep
SUBROUTINE nl_get_test_calc_p_rho ( id_id , test_calc_p_rho )
  logical , INTENT(OUT) :: test_calc_p_rho
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_calc_p_rho: test_calc_p_rho applies to all domains. First arg ignored.')
  ENDIF
  test_calc_p_rho = model_config_rec%test_calc_p_rho
  RETURN
END SUBROUTINE nl_get_test_calc_p_rho
SUBROUTINE nl_get_test_calc_coef_w ( id_id , test_calc_coef_w )
  logical , INTENT(OUT) :: test_calc_coef_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_calc_coef_w: test_calc_coef_w applies to all domains. First arg ignored.')
  ENDIF
  test_calc_coef_w = model_config_rec%test_calc_coef_w
  RETURN
END SUBROUTINE nl_get_test_calc_coef_w
SUBROUTINE nl_get_test_advance_uv ( id_id , test_advance_uv )
  logical , INTENT(OUT) :: test_advance_uv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_advance_uv: test_advance_uv applies to all domains. First arg ignored.')
  ENDIF
  test_advance_uv = model_config_rec%test_advance_uv
  RETURN
END SUBROUTINE nl_get_test_advance_uv
SUBROUTINE nl_get_test_spec_bdyupdate ( id_id , test_spec_bdyupdate )
  logical , INTENT(OUT) :: test_spec_bdyupdate
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_spec_bdyupdate: test_spec_bdyupdate applies to all domains. First arg ignored.')
  ENDIF
  test_spec_bdyupdate = model_config_rec%test_spec_bdyupdate
  RETURN
END SUBROUTINE nl_get_test_spec_bdyupdate
SUBROUTINE nl_get_test_advance_mu_t ( id_id , test_advance_mu_t )
  logical , INTENT(OUT) :: test_advance_mu_t
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_advance_mu_t: test_advance_mu_t applies to all domains. First arg ignored.')
  ENDIF
  test_advance_mu_t = model_config_rec%test_advance_mu_t
  RETURN
END SUBROUTINE nl_get_test_advance_mu_t
SUBROUTINE nl_get_test_sumflux ( id_id , test_sumflux )
  logical , INTENT(OUT) :: test_sumflux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_sumflux: test_sumflux applies to all domains. First arg ignored.')
  ENDIF
  test_sumflux = model_config_rec%test_sumflux
  RETURN
END SUBROUTINE nl_get_test_sumflux
SUBROUTINE nl_get_test_advance_w ( id_id , test_advance_w )
  logical , INTENT(OUT) :: test_advance_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_advance_w: test_advance_w applies to all domains. First arg ignored.')
  ENDIF
  test_advance_w = model_config_rec%test_advance_w
  RETURN
END SUBROUTINE nl_get_test_advance_w
SUBROUTINE nl_get_test_spec_bdyupdate_ph ( id_id , test_spec_bdyupdate_ph )
  logical , INTENT(OUT) :: test_spec_bdyupdate_ph
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_spec_bdyupdate_ph: test_spec_bdyupdate_ph applies to all domains. First arg ignored.')
  ENDIF
  test_spec_bdyupdate_ph = model_config_rec%test_spec_bdyupdate_ph
  RETURN
END SUBROUTINE nl_get_test_spec_bdyupdate_ph
SUBROUTINE nl_get_test_calc_mu_uv_1 ( id_id , test_calc_mu_uv_1 )
  logical , INTENT(OUT) :: test_calc_mu_uv_1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_calc_mu_uv_1: test_calc_mu_uv_1 applies to all domains. First arg ignored.')
  ENDIF
  test_calc_mu_uv_1 = model_config_rec%test_calc_mu_uv_1
  RETURN
END SUBROUTINE nl_get_test_calc_mu_uv_1
SUBROUTINE nl_get_test_small_step_finish ( id_id , test_small_step_finish )
  logical , INTENT(OUT) :: test_small_step_finish
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_small_step_finish: test_small_step_finish applies to all domains. First arg ignored.')
  ENDIF
  test_small_step_finish = model_config_rec%test_small_step_finish
  RETURN
END SUBROUTINE nl_get_test_small_step_finish
SUBROUTINE nl_get_test_rk_scalar_tend ( id_id , test_rk_scalar_tend )
  logical , INTENT(OUT) :: test_rk_scalar_tend
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_rk_scalar_tend: test_rk_scalar_tend applies to all domains. First arg ignored.')
  ENDIF
  test_rk_scalar_tend = model_config_rec%test_rk_scalar_tend
  RETURN
END SUBROUTINE nl_get_test_rk_scalar_tend
SUBROUTINE nl_get_test_spec_bdy_scalar ( id_id , test_spec_bdy_scalar )
  logical , INTENT(OUT) :: test_spec_bdy_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_spec_bdy_scalar: test_spec_bdy_scalar applies to all domains. First arg ignored.')
  ENDIF
  test_spec_bdy_scalar = model_config_rec%test_spec_bdy_scalar
  RETURN
END SUBROUTINE nl_get_test_spec_bdy_scalar
SUBROUTINE nl_get_test_rk_update_scalar ( id_id , test_rk_update_scalar )
  logical , INTENT(OUT) :: test_rk_update_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_rk_update_scalar: test_rk_update_scalar applies to all domains. First arg ignored.')
  ENDIF
  test_rk_update_scalar = model_config_rec%test_rk_update_scalar
  RETURN
END SUBROUTINE nl_get_test_rk_update_scalar
SUBROUTINE nl_get_test_calc_p_rho_phi ( id_id , test_calc_p_rho_phi )
  logical , INTENT(OUT) :: test_calc_p_rho_phi
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_calc_p_rho_phi: test_calc_p_rho_phi applies to all domains. First arg ignored.')
  ENDIF
  test_calc_p_rho_phi = model_config_rec%test_calc_p_rho_phi
  RETURN
END SUBROUTINE nl_get_test_calc_p_rho_phi
SUBROUTINE nl_get_test_diagnose_w ( id_id , test_diagnose_w )
  logical , INTENT(OUT) :: test_diagnose_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_test_diagnose_w: test_diagnose_w applies to all domains. First arg ignored.')
  ENDIF
  test_diagnose_w = model_config_rec%test_diagnose_w
  RETURN
END SUBROUTINE nl_get_test_diagnose_w
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_mp_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7433 , emess)
  ENDIF
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ra_lw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7444 , emess)
  ENDIF
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ra_sw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7455 , emess)
  ENDIF
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_radt ( id_id , radt )
  real , INTENT(OUT) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_radt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7466 , emess)
  ENDIF
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_sf_sfclay_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7477 , emess)
  ENDIF
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_sf_surface_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7488 , emess)
  ENDIF
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bl_pbl_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7499 , emess)
  ENDIF
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_bldt ( id_id , bldt )
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bldt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7510 , emess)
  ENDIF
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cu_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7521 , emess)
  ENDIF
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
SUBROUTINE nl_get_cudt ( id_id , cudt )
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cudt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7532 , emess)
  ENDIF
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE nl_get_cudt
SUBROUTINE nl_get_gsmdt ( id_id , gsmdt )
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gsmdt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7543 , emess)
  ENDIF
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE nl_get_gsmdt
SUBROUTINE nl_get_isfflx ( id_id , isfflx )
  integer , INTENT(OUT) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_isfflx: isfflx applies to all domains. First arg ignored.')
  ENDIF
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE nl_get_isfflx
SUBROUTINE nl_get_ifsnow ( id_id , ifsnow )
  integer , INTENT(OUT) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_ifsnow: ifsnow applies to all domains. First arg ignored.')
  ENDIF
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE nl_get_ifsnow
SUBROUTINE nl_get_icloud ( id_id , icloud )
  integer , INTENT(OUT) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_icloud: icloud applies to all domains. First arg ignored.')
  ENDIF
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE nl_get_icloud
SUBROUTINE nl_get_surface_input_source ( id_id , surface_input_source )
  integer , INTENT(OUT) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_surface_input_source: surface_input_source applies to all domains. First arg ignored.')
  ENDIF
  surface_input_source = model_config_rec%surface_input_source
  RETURN
END SUBROUTINE nl_get_surface_input_source
SUBROUTINE nl_get_num_soil_layers ( id_id , num_soil_layers )
  integer , INTENT(OUT) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_num_soil_layers: num_soil_layers applies to all domains. First arg ignored.')
  ENDIF
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE nl_get_num_soil_layers
SUBROUTINE nl_get_maxiens ( id_id , maxiens )
  integer , INTENT(OUT) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_maxiens: maxiens applies to all domains. First arg ignored.')
  ENDIF
  maxiens = model_config_rec%maxiens
  RETURN
END SUBROUTINE nl_get_maxiens
SUBROUTINE nl_get_maxens ( id_id , maxens )
  integer , INTENT(OUT) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_maxens: maxens applies to all domains. First arg ignored.')
  ENDIF
  maxens = model_config_rec%maxens
  RETURN
END SUBROUTINE nl_get_maxens
SUBROUTINE nl_get_maxens2 ( id_id , maxens2 )
  integer , INTENT(OUT) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_maxens2: maxens2 applies to all domains. First arg ignored.')
  ENDIF
  maxens2 = model_config_rec%maxens2
  RETURN
END SUBROUTINE nl_get_maxens2
SUBROUTINE nl_get_maxens3 ( id_id , maxens3 )
  integer , INTENT(OUT) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_maxens3: maxens3 applies to all domains. First arg ignored.')
  ENDIF
  maxens3 = model_config_rec%maxens3
  RETURN
END SUBROUTINE nl_get_maxens3
SUBROUTINE nl_get_ensdim ( id_id , ensdim )
  integer , INTENT(OUT) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_ensdim: ensdim applies to all domains. First arg ignored.')
  ENDIF
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE nl_get_ensdim
SUBROUTINE nl_get_chem_opt ( id_id , chem_opt )
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_chem_opt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7654 , emess)
  ENDIF
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_opt
SUBROUTINE nl_get_num_land_cat ( id_id , num_land_cat )
  integer , INTENT(OUT) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_num_land_cat: num_land_cat applies to all domains. First arg ignored.')
  ENDIF
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE nl_get_num_land_cat
SUBROUTINE nl_get_num_soil_cat ( id_id , num_soil_cat )
  integer , INTENT(OUT) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_num_soil_cat: num_soil_cat applies to all domains. First arg ignored.')
  ENDIF
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE nl_get_num_soil_cat
SUBROUTINE nl_get_mp_zero_out ( id_id , mp_zero_out )
  integer , INTENT(OUT) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_mp_zero_out: mp_zero_out applies to all domains. First arg ignored.')
  ENDIF
  mp_zero_out = model_config_rec%mp_zero_out
  RETURN
END SUBROUTINE nl_get_mp_zero_out
SUBROUTINE nl_get_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  real , INTENT(OUT) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_mp_zero_out_thresh: mp_zero_out_thresh applies to all domains. First arg ignored.')
  ENDIF
  mp_zero_out_thresh = model_config_rec%mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_get_mp_zero_out_thresh
SUBROUTINE nl_get_seaice_threshold ( id_id , seaice_threshold )
  real , INTENT(OUT) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_seaice_threshold: seaice_threshold applies to all domains. First arg ignored.')
  ENDIF
  seaice_threshold = model_config_rec%seaice_threshold
  RETURN
END SUBROUTINE nl_get_seaice_threshold
SUBROUTINE nl_get_dyn_opt ( id_id , dyn_opt )
  integer , INTENT(OUT) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_dyn_opt: dyn_opt applies to all domains. First arg ignored.')
  ENDIF
  dyn_opt = model_config_rec%dyn_opt
  RETURN
END SUBROUTINE nl_get_dyn_opt
SUBROUTINE nl_get_rk_ord ( id_id , rk_ord )
  integer , INTENT(OUT) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_rk_ord: rk_ord applies to all domains. First arg ignored.')
  ENDIF
  rk_ord = model_config_rec%rk_ord
  RETURN
END SUBROUTINE nl_get_rk_ord
SUBROUTINE nl_get_w_damping ( id_id , w_damping )
  integer , INTENT(OUT) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_w_damping: w_damping applies to all domains. First arg ignored.')
  ENDIF
  w_damping = model_config_rec%w_damping
  RETURN
END SUBROUTINE nl_get_w_damping
SUBROUTINE nl_get_diff_opt ( id_id , diff_opt )
  integer , INTENT(OUT) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_diff_opt: diff_opt applies to all domains. First arg ignored.')
  ENDIF
  diff_opt = model_config_rec%diff_opt
  RETURN
END SUBROUTINE nl_get_diff_opt
SUBROUTINE nl_get_km_opt ( id_id , km_opt )
  integer , INTENT(OUT) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_km_opt: km_opt applies to all domains. First arg ignored.')
  ENDIF
  km_opt = model_config_rec%km_opt
  RETURN
END SUBROUTINE nl_get_km_opt
SUBROUTINE nl_get_damp_opt ( id_id , damp_opt )
  integer , INTENT(OUT) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_damp_opt: damp_opt applies to all domains. First arg ignored.')
  ENDIF
  damp_opt = model_config_rec%damp_opt
  RETURN
END SUBROUTINE nl_get_damp_opt
SUBROUTINE nl_get_zdamp ( id_id , zdamp )
  real , INTENT(OUT) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_zdamp: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7775 , emess)
  ENDIF
  zdamp = model_config_rec%zdamp(id_id)
  RETURN
END SUBROUTINE nl_get_zdamp
SUBROUTINE nl_get_dampcoef ( id_id , dampcoef )
  real , INTENT(OUT) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dampcoef: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7786 , emess)
  ENDIF
  dampcoef = model_config_rec%dampcoef(id_id)
  RETURN
END SUBROUTINE nl_get_dampcoef
SUBROUTINE nl_get_khdif ( id_id , khdif )
  real , INTENT(OUT) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_khdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7797 , emess)
  ENDIF
  khdif = model_config_rec%khdif(id_id)
  RETURN
END SUBROUTINE nl_get_khdif
SUBROUTINE nl_get_kvdif ( id_id , kvdif )
  real , INTENT(OUT) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kvdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7808 , emess)
  ENDIF
  kvdif = model_config_rec%kvdif(id_id)
  RETURN
END SUBROUTINE nl_get_kvdif
SUBROUTINE nl_get_smdiv ( id_id , smdiv )
  real , INTENT(OUT) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_smdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7819 , emess)
  ENDIF
  smdiv = model_config_rec%smdiv(id_id)
  RETURN
END SUBROUTINE nl_get_smdiv
SUBROUTINE nl_get_emdiv ( id_id , emdiv )
  real , INTENT(OUT) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_emdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7830 , emess)
  ENDIF
  emdiv = model_config_rec%emdiv(id_id)
  RETURN
END SUBROUTINE nl_get_emdiv
SUBROUTINE nl_get_epssm ( id_id , epssm )
  real , INTENT(OUT) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_epssm: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7841 , emess)
  ENDIF
  epssm = model_config_rec%epssm(id_id)
  RETURN
END SUBROUTINE nl_get_epssm
SUBROUTINE nl_get_non_hydrostatic ( id_id , non_hydrostatic )
  logical , INTENT(OUT) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_non_hydrostatic: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7852 , emess)
  ENDIF
  non_hydrostatic = model_config_rec%non_hydrostatic(id_id)
  RETURN
END SUBROUTINE nl_get_non_hydrostatic
SUBROUTINE nl_get_time_step_sound ( id_id , time_step_sound )
  integer , INTENT(OUT) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_time_step_sound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7863 , emess)
  ENDIF
  time_step_sound = model_config_rec%time_step_sound(id_id)
  RETURN
END SUBROUTINE nl_get_time_step_sound
SUBROUTINE nl_get_h_mom_adv_order ( id_id , h_mom_adv_order )
  integer , INTENT(OUT) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_h_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7874 , emess)
  ENDIF
  h_mom_adv_order = model_config_rec%h_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_mom_adv_order
SUBROUTINE nl_get_v_mom_adv_order ( id_id , v_mom_adv_order )
  integer , INTENT(OUT) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_v_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7885 , emess)
  ENDIF
  v_mom_adv_order = model_config_rec%v_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_mom_adv_order
SUBROUTINE nl_get_h_sca_adv_order ( id_id , h_sca_adv_order )
  integer , INTENT(OUT) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_h_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7896 , emess)
  ENDIF
  h_sca_adv_order = model_config_rec%h_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_sca_adv_order
SUBROUTINE nl_get_v_sca_adv_order ( id_id , v_sca_adv_order )
  integer , INTENT(OUT) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_v_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7907 , emess)
  ENDIF
  v_sca_adv_order = model_config_rec%v_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_sca_adv_order
SUBROUTINE nl_get_top_radiation ( id_id , top_radiation )
  logical , INTENT(OUT) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_top_radiation: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7918 , emess)
  ENDIF
  top_radiation = model_config_rec%top_radiation(id_id)
  RETURN
END SUBROUTINE nl_get_top_radiation
SUBROUTINE nl_get_mix_cr_len ( id_id , mix_cr_len )
  real , INTENT(OUT) :: mix_cr_len
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_mix_cr_len: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7929 , emess)
  ENDIF
  mix_cr_len = model_config_rec%mix_cr_len(id_id)
  RETURN
END SUBROUTINE nl_get_mix_cr_len
SUBROUTINE nl_get_tke_upper_bound ( id_id , tke_upper_bound )
  real , INTENT(OUT) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7940 , emess)
  ENDIF
  tke_upper_bound = model_config_rec%tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_tke_upper_bound
SUBROUTINE nl_get_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  real , INTENT(OUT) :: kh_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kh_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7951 , emess)
  ENDIF
  kh_tke_upper_bound = model_config_rec%kh_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_kh_tke_upper_bound
SUBROUTINE nl_get_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  real , INTENT(OUT) :: kv_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kv_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7962 , emess)
  ENDIF
  kv_tke_upper_bound = model_config_rec%kv_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_kv_tke_upper_bound
SUBROUTINE nl_get_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  real , INTENT(OUT) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_drag_coefficient: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7973 , emess)
  ENDIF
  tke_drag_coefficient = model_config_rec%tke_drag_coefficient(id_id)
  RETURN
END SUBROUTINE nl_get_tke_drag_coefficient
SUBROUTINE nl_get_tke_heat_flux ( id_id , tke_heat_flux )
  real , INTENT(OUT) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_heat_flux: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7984 , emess)
  ENDIF
  tke_heat_flux = model_config_rec%tke_heat_flux(id_id)
  RETURN
END SUBROUTINE nl_get_tke_heat_flux
SUBROUTINE nl_get_pert_coriolis ( id_id , pert_coriolis )
  logical , INTENT(OUT) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_pert_coriolis: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 7995 , emess)
  ENDIF
  pert_coriolis = model_config_rec%pert_coriolis(id_id)
  RETURN
END SUBROUTINE nl_get_pert_coriolis
SUBROUTINE nl_get_mix_full_fields ( id_id , mix_full_fields )
  logical , INTENT(OUT) :: mix_full_fields
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_mix_full_fields: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8006 , emess)
  ENDIF
  mix_full_fields = model_config_rec%mix_full_fields(id_id)
  RETURN
END SUBROUTINE nl_get_mix_full_fields
SUBROUTINE nl_get_base_pres ( id_id , base_pres )
  real , INTENT(OUT) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_base_pres: base_pres applies to all domains. First arg ignored.')
  ENDIF
  base_pres = model_config_rec%base_pres
  RETURN
END SUBROUTINE nl_get_base_pres
SUBROUTINE nl_get_base_temp ( id_id , base_temp )
  real , INTENT(OUT) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_base_temp: base_temp applies to all domains. First arg ignored.')
  ENDIF
  base_temp = model_config_rec%base_temp
  RETURN
END SUBROUTINE nl_get_base_temp
SUBROUTINE nl_get_base_lapse ( id_id , base_lapse )
  real , INTENT(OUT) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_base_lapse: base_lapse applies to all domains. First arg ignored.')
  ENDIF
  base_lapse = model_config_rec%base_lapse
  RETURN
END SUBROUTINE nl_get_base_lapse
SUBROUTINE nl_get_spec_bdy_width ( id_id , spec_bdy_width )
  integer , INTENT(OUT) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_spec_bdy_width: spec_bdy_width applies to all domains. First arg ignored.')
  ENDIF
  spec_bdy_width = model_config_rec%spec_bdy_width
  RETURN
END SUBROUTINE nl_get_spec_bdy_width
SUBROUTINE nl_get_spec_zone ( id_id , spec_zone )
  integer , INTENT(OUT) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_spec_zone: spec_zone applies to all domains. First arg ignored.')
  ENDIF
  spec_zone = model_config_rec%spec_zone
  RETURN
END SUBROUTINE nl_get_spec_zone
SUBROUTINE nl_get_relax_zone ( id_id , relax_zone )
  integer , INTENT(OUT) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_relax_zone: relax_zone applies to all domains. First arg ignored.')
  ENDIF
  relax_zone = model_config_rec%relax_zone
  RETURN
END SUBROUTINE nl_get_relax_zone
SUBROUTINE nl_get_specified ( id_id , specified )
  logical , INTENT(OUT) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_specified: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8077 , emess)
  ENDIF
  specified = model_config_rec%specified(id_id)
  RETURN
END SUBROUTINE nl_get_specified
SUBROUTINE nl_get_periodic_x ( id_id , periodic_x )
  logical , INTENT(OUT) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_periodic_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8088 , emess)
  ENDIF
  periodic_x = model_config_rec%periodic_x(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_x
SUBROUTINE nl_get_symmetric_xs ( id_id , symmetric_xs )
  logical , INTENT(OUT) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8099 , emess)
  ENDIF
  symmetric_xs = model_config_rec%symmetric_xs(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xs
SUBROUTINE nl_get_symmetric_xe ( id_id , symmetric_xe )
  logical , INTENT(OUT) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8110 , emess)
  ENDIF
  symmetric_xe = model_config_rec%symmetric_xe(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xe
SUBROUTINE nl_get_open_xs ( id_id , open_xs )
  logical , INTENT(OUT) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8121 , emess)
  ENDIF
  open_xs = model_config_rec%open_xs(id_id)
  RETURN
END SUBROUTINE nl_get_open_xs
SUBROUTINE nl_get_open_xe ( id_id , open_xe )
  logical , INTENT(OUT) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8132 , emess)
  ENDIF
  open_xe = model_config_rec%open_xe(id_id)
  RETURN
END SUBROUTINE nl_get_open_xe
SUBROUTINE nl_get_periodic_y ( id_id , periodic_y )
  logical , INTENT(OUT) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_periodic_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8143 , emess)
  ENDIF
  periodic_y = model_config_rec%periodic_y(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_y
SUBROUTINE nl_get_symmetric_ys ( id_id , symmetric_ys )
  logical , INTENT(OUT) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8154 , emess)
  ENDIF
  symmetric_ys = model_config_rec%symmetric_ys(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ys
SUBROUTINE nl_get_symmetric_ye ( id_id , symmetric_ye )
  logical , INTENT(OUT) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8165 , emess)
  ENDIF
  symmetric_ye = model_config_rec%symmetric_ye(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ye
SUBROUTINE nl_get_open_ys ( id_id , open_ys )
  logical , INTENT(OUT) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8176 , emess)
  ENDIF
  open_ys = model_config_rec%open_ys(id_id)
  RETURN
END SUBROUTINE nl_get_open_ys
SUBROUTINE nl_get_open_ye ( id_id , open_ye )
  logical , INTENT(OUT) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8187 , emess)
  ENDIF
  open_ye = model_config_rec%open_ye(id_id)
  RETURN
END SUBROUTINE nl_get_open_ye
SUBROUTINE nl_get_nested ( id_id , nested )
  logical , INTENT(OUT) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nested: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8198 , emess)
  ENDIF
  nested = model_config_rec%nested(id_id)
  RETURN
END SUBROUTINE nl_get_nested
SUBROUTINE nl_get_real_data_init_type ( id_id , real_data_init_type )
  integer , INTENT(OUT) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_get_real_data_init_type: real_data_init_type applies to all domains. First arg ignored.')
  ENDIF
  real_data_init_type = model_config_rec%real_data_init_type
  RETURN
END SUBROUTINE nl_get_real_data_init_type
SUBROUTINE nl_get_cen_lat ( id_id , cen_lat )
  real , INTENT(OUT) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8219 , emess)
  ENDIF
  cen_lat = model_config_rec%cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lat
SUBROUTINE nl_get_cen_lon ( id_id , cen_lon )
  real , INTENT(OUT) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cen_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8230 , emess)
  ENDIF
  cen_lon = model_config_rec%cen_lon(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lon
SUBROUTINE nl_get_truelat1 ( id_id , truelat1 )
  real , INTENT(OUT) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_truelat1: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8241 , emess)
  ENDIF
  truelat1 = model_config_rec%truelat1(id_id)
  RETURN
END SUBROUTINE nl_get_truelat1
SUBROUTINE nl_get_truelat2 ( id_id , truelat2 )
  real , INTENT(OUT) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_truelat2: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8252 , emess)
  ENDIF
  truelat2 = model_config_rec%truelat2(id_id)
  RETURN
END SUBROUTINE nl_get_truelat2
SUBROUTINE nl_get_moad_cen_lat ( id_id , moad_cen_lat )
  real , INTENT(OUT) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8263 , emess)
  ENDIF
  moad_cen_lat = model_config_rec%moad_cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_moad_cen_lat
SUBROUTINE nl_get_stand_lon ( id_id , stand_lon )
  real , INTENT(OUT) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_stand_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8274 , emess)
  ENDIF
  stand_lon = model_config_rec%stand_lon(id_id)
  RETURN
END SUBROUTINE nl_get_stand_lon
SUBROUTINE nl_get_bdyfrq ( id_id , bdyfrq )
  real , INTENT(OUT) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bdyfrq: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8285 , emess)
  ENDIF
  bdyfrq = model_config_rec%bdyfrq(id_id)
  RETURN
END SUBROUTINE nl_get_bdyfrq
SUBROUTINE nl_get_iswater ( id_id , iswater )
  integer , INTENT(OUT) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_iswater: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8296 , emess)
  ENDIF
  iswater = model_config_rec%iswater(id_id)
  RETURN
END SUBROUTINE nl_get_iswater
SUBROUTINE nl_get_isice ( id_id , isice )
  integer , INTENT(OUT) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isice: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8307 , emess)
  ENDIF
  isice = model_config_rec%isice(id_id)
  RETURN
END SUBROUTINE nl_get_isice
SUBROUTINE nl_get_isurban ( id_id , isurban )
  integer , INTENT(OUT) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isurban: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8318 , emess)
  ENDIF
  isurban = model_config_rec%isurban(id_id)
  RETURN
END SUBROUTINE nl_get_isurban
SUBROUTINE nl_get_isoilwater ( id_id , isoilwater )
  integer , INTENT(OUT) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isoilwater: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8329 , emess)
  ENDIF
  isoilwater = model_config_rec%isoilwater(id_id)
  RETURN
END SUBROUTINE nl_get_isoilwater
SUBROUTINE nl_get_map_proj ( id_id , map_proj )
  integer , INTENT(OUT) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_map_proj: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8340 , emess)
  ENDIF
  map_proj = model_config_rec%map_proj(id_id)
  RETURN
END SUBROUTINE nl_get_map_proj
SUBROUTINE nl_set_run_days ( id_id , run_days )
  integer , INTENT(IN) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_run_days: run_days applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_days = run_days 
  RETURN
END SUBROUTINE nl_set_run_days
SUBROUTINE nl_set_run_hours ( id_id , run_hours )
  integer , INTENT(IN) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_run_hours: run_hours applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_hours = run_hours 
  RETURN
END SUBROUTINE nl_set_run_hours
SUBROUTINE nl_set_run_minutes ( id_id , run_minutes )
  integer , INTENT(IN) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_run_minutes: run_minutes applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_minutes = run_minutes 
  RETURN
END SUBROUTINE nl_set_run_minutes
SUBROUTINE nl_set_run_seconds ( id_id , run_seconds )
  integer , INTENT(IN) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_run_seconds: run_seconds applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_seconds = run_seconds 
  RETURN
END SUBROUTINE nl_set_run_seconds
SUBROUTINE nl_set_start_year ( id_id , start_year )
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8391 , emess)
  ENDIF
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE nl_set_start_year
SUBROUTINE nl_set_start_month ( id_id , start_month )
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8402 , emess)
  ENDIF
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE nl_set_start_month
SUBROUTINE nl_set_start_day ( id_id , start_day )
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8413 , emess)
  ENDIF
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE nl_set_start_day
SUBROUTINE nl_set_start_hour ( id_id , start_hour )
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8424 , emess)
  ENDIF
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE nl_set_start_hour
SUBROUTINE nl_set_start_minute ( id_id , start_minute )
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8435 , emess)
  ENDIF
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE nl_set_start_minute
SUBROUTINE nl_set_start_second ( id_id , start_second )
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8446 , emess)
  ENDIF
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE nl_set_start_second
SUBROUTINE nl_set_end_year ( id_id , end_year )
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8457 , emess)
  ENDIF
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE nl_set_end_year
SUBROUTINE nl_set_end_month ( id_id , end_month )
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8468 , emess)
  ENDIF
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE nl_set_end_month
SUBROUTINE nl_set_end_day ( id_id , end_day )
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8479 , emess)
  ENDIF
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE nl_set_end_day
SUBROUTINE nl_set_end_hour ( id_id , end_hour )
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8490 , emess)
  ENDIF
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE nl_set_end_hour
SUBROUTINE nl_set_end_minute ( id_id , end_minute )
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8501 , emess)
  ENDIF
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE nl_set_end_minute
SUBROUTINE nl_set_end_second ( id_id , end_second )
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8512 , emess)
  ENDIF
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE nl_set_end_second
SUBROUTINE nl_set_interval_seconds ( id_id , interval_seconds )
  integer , INTENT(IN) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_interval_seconds: interval_seconds applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%interval_seconds = interval_seconds 
  RETURN
END SUBROUTINE nl_set_interval_seconds
SUBROUTINE nl_set_input_from_file ( id_id , input_from_file )
  logical , INTENT(IN) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_input_from_file: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8533 , emess)
  ENDIF
  model_config_rec%input_from_file(id_id) = input_from_file
  RETURN
END SUBROUTINE nl_set_input_from_file
SUBROUTINE nl_set_input_from_hires ( id_id , input_from_hires )
  logical , INTENT(IN) :: input_from_hires
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_input_from_hires: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8544 , emess)
  ENDIF
  model_config_rec%input_from_hires(id_id) = input_from_hires
  RETURN
END SUBROUTINE nl_set_input_from_hires
SUBROUTINE nl_set_history_interval ( id_id , history_interval )
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8555 , emess)
  ENDIF
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE nl_set_history_interval
SUBROUTINE nl_set_frames_per_outfile ( id_id , frames_per_outfile )
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_outfile: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8566 , emess)
  ENDIF
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE nl_set_frames_per_outfile
SUBROUTINE nl_set_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  integer , INTENT(IN) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist1: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8577 , emess)
  ENDIF
  model_config_rec%frames_per_auxhist1(id_id) = frames_per_auxhist1
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist1
SUBROUTINE nl_set_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  integer , INTENT(IN) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist2: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8588 , emess)
  ENDIF
  model_config_rec%frames_per_auxhist2(id_id) = frames_per_auxhist2
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist2
SUBROUTINE nl_set_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  integer , INTENT(IN) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist3: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8599 , emess)
  ENDIF
  model_config_rec%frames_per_auxhist3(id_id) = frames_per_auxhist3
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist3
SUBROUTINE nl_set_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  integer , INTENT(IN) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist4: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8610 , emess)
  ENDIF
  model_config_rec%frames_per_auxhist4(id_id) = frames_per_auxhist4
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist4
SUBROUTINE nl_set_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  integer , INTENT(IN) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist5: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8621 , emess)
  ENDIF
  model_config_rec%frames_per_auxhist5(id_id) = frames_per_auxhist5
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist5
SUBROUTINE nl_set_restart ( id_id , restart )
  logical , INTENT(IN) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart: restart applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE nl_set_restart
SUBROUTINE nl_set_restart_interval ( id_id , restart_interval )
  integer , INTENT(IN) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval: restart_interval applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE nl_set_restart_interval
SUBROUTINE nl_set_io_form_input ( id_id , io_form_input )
  integer , INTENT(IN) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_input: io_form_input applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE nl_set_io_form_input
SUBROUTINE nl_set_io_form_history ( id_id , io_form_history )
  integer , INTENT(IN) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_history: io_form_history applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE nl_set_io_form_history
SUBROUTINE nl_set_io_form_restart ( id_id , io_form_restart )
  integer , INTENT(IN) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_restart: io_form_restart applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE nl_set_io_form_restart
SUBROUTINE nl_set_io_form_boundary ( id_id , io_form_boundary )
  integer , INTENT(IN) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_boundary: io_form_boundary applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE nl_set_io_form_boundary
SUBROUTINE nl_set_debug_level ( id_id , debug_level )
  integer , INTENT(IN) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_debug_level: debug_level applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE nl_set_debug_level
SUBROUTINE nl_set_history_outname ( id_id , history_outname )
  character*256 , INTENT(IN) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_history_outname: history_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%history_outname = history_outname 
  RETURN
END SUBROUTINE nl_set_history_outname
SUBROUTINE nl_set_auxhist1_outname ( id_id , auxhist1_outname )
  character*256 , INTENT(IN) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist1_outname: auxhist1_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist1_outname = auxhist1_outname 
  RETURN
END SUBROUTINE nl_set_auxhist1_outname
SUBROUTINE nl_set_auxhist2_outname ( id_id , auxhist2_outname )
  character*256 , INTENT(IN) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist2_outname: auxhist2_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist2_outname = auxhist2_outname 
  RETURN
END SUBROUTINE nl_set_auxhist2_outname
SUBROUTINE nl_set_auxhist3_outname ( id_id , auxhist3_outname )
  character*256 , INTENT(IN) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist3_outname: auxhist3_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist3_outname = auxhist3_outname 
  RETURN
END SUBROUTINE nl_set_auxhist3_outname
SUBROUTINE nl_set_auxhist4_outname ( id_id , auxhist4_outname )
  character*256 , INTENT(IN) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist4_outname: auxhist4_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist4_outname = auxhist4_outname 
  RETURN
END SUBROUTINE nl_set_auxhist4_outname
SUBROUTINE nl_set_auxhist5_outname ( id_id , auxhist5_outname )
  character*256 , INTENT(IN) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist5_outname: auxhist5_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist5_outname = auxhist5_outname 
  RETURN
END SUBROUTINE nl_set_auxhist5_outname
SUBROUTINE nl_set_history_inname ( id_id , history_inname )
  character*256 , INTENT(IN) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_history_inname: history_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%history_inname = history_inname 
  RETURN
END SUBROUTINE nl_set_history_inname
SUBROUTINE nl_set_auxhist1_inname ( id_id , auxhist1_inname )
  character*256 , INTENT(IN) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist1_inname: auxhist1_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist1_inname = auxhist1_inname 
  RETURN
END SUBROUTINE nl_set_auxhist1_inname
SUBROUTINE nl_set_auxhist2_inname ( id_id , auxhist2_inname )
  character*256 , INTENT(IN) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist2_inname: auxhist2_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist2_inname = auxhist2_inname 
  RETURN
END SUBROUTINE nl_set_auxhist2_inname
SUBROUTINE nl_set_auxhist3_inname ( id_id , auxhist3_inname )
  character*256 , INTENT(IN) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist3_inname: auxhist3_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist3_inname = auxhist3_inname 
  RETURN
END SUBROUTINE nl_set_auxhist3_inname
SUBROUTINE nl_set_auxhist4_inname ( id_id , auxhist4_inname )
  character*256 , INTENT(IN) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist4_inname: auxhist4_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist4_inname = auxhist4_inname 
  RETURN
END SUBROUTINE nl_set_auxhist4_inname
SUBROUTINE nl_set_auxhist5_inname ( id_id , auxhist5_inname )
  character*256 , INTENT(IN) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxhist5_inname: auxhist5_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist5_inname = auxhist5_inname 
  RETURN
END SUBROUTINE nl_set_auxhist5_inname
SUBROUTINE nl_set_auxinput1_outname ( id_id , auxinput1_outname )
  character*256 , INTENT(IN) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput1_outname: auxinput1_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput1_outname = auxinput1_outname 
  RETURN
END SUBROUTINE nl_set_auxinput1_outname
SUBROUTINE nl_set_auxinput2_outname ( id_id , auxinput2_outname )
  character*256 , INTENT(IN) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput2_outname: auxinput2_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput2_outname = auxinput2_outname 
  RETURN
END SUBROUTINE nl_set_auxinput2_outname
SUBROUTINE nl_set_auxinput3_outname ( id_id , auxinput3_outname )
  character*256 , INTENT(IN) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput3_outname: auxinput3_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput3_outname = auxinput3_outname 
  RETURN
END SUBROUTINE nl_set_auxinput3_outname
SUBROUTINE nl_set_auxinput4_outname ( id_id , auxinput4_outname )
  character*256 , INTENT(IN) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput4_outname: auxinput4_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput4_outname = auxinput4_outname 
  RETURN
END SUBROUTINE nl_set_auxinput4_outname
SUBROUTINE nl_set_auxinput5_outname ( id_id , auxinput5_outname )
  character*256 , INTENT(IN) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput5_outname: auxinput5_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput5_outname = auxinput5_outname 
  RETURN
END SUBROUTINE nl_set_auxinput5_outname
SUBROUTINE nl_set_auxinput1_inname ( id_id , auxinput1_inname )
  character*256 , INTENT(IN) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput1_inname: auxinput1_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput1_inname = auxinput1_inname 
  RETURN
END SUBROUTINE nl_set_auxinput1_inname
SUBROUTINE nl_set_auxinput2_inname ( id_id , auxinput2_inname )
  character*256 , INTENT(IN) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput2_inname: auxinput2_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput2_inname = auxinput2_inname 
  RETURN
END SUBROUTINE nl_set_auxinput2_inname
SUBROUTINE nl_set_auxinput3_inname ( id_id , auxinput3_inname )
  character*256 , INTENT(IN) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput3_inname: auxinput3_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput3_inname = auxinput3_inname 
  RETURN
END SUBROUTINE nl_set_auxinput3_inname
SUBROUTINE nl_set_auxinput4_inname ( id_id , auxinput4_inname )
  character*256 , INTENT(IN) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput4_inname: auxinput4_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput4_inname = auxinput4_inname 
  RETURN
END SUBROUTINE nl_set_auxinput4_inname
SUBROUTINE nl_set_auxinput5_inname ( id_id , auxinput5_inname )
  character*256 , INTENT(IN) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_auxinput5_inname: auxinput5_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput5_inname = auxinput5_inname 
  RETURN
END SUBROUTINE nl_set_auxinput5_inname
SUBROUTINE nl_set_history_interval_mo ( id_id , history_interval_mo )
  integer , INTENT(IN) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8922 , emess)
  ENDIF
  model_config_rec%history_interval_mo(id_id) = history_interval_mo
  RETURN
END SUBROUTINE nl_set_history_interval_mo
SUBROUTINE nl_set_history_interval_d ( id_id , history_interval_d )
  integer , INTENT(IN) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8933 , emess)
  ENDIF
  model_config_rec%history_interval_d(id_id) = history_interval_d
  RETURN
END SUBROUTINE nl_set_history_interval_d
SUBROUTINE nl_set_history_interval_h ( id_id , history_interval_h )
  integer , INTENT(IN) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8944 , emess)
  ENDIF
  model_config_rec%history_interval_h(id_id) = history_interval_h
  RETURN
END SUBROUTINE nl_set_history_interval_h
SUBROUTINE nl_set_history_interval_m ( id_id , history_interval_m )
  integer , INTENT(IN) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8955 , emess)
  ENDIF
  model_config_rec%history_interval_m(id_id) = history_interval_m
  RETURN
END SUBROUTINE nl_set_history_interval_m
SUBROUTINE nl_set_history_interval_s ( id_id , history_interval_s )
  integer , INTENT(IN) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8966 , emess)
  ENDIF
  model_config_rec%history_interval_s(id_id) = history_interval_s
  RETURN
END SUBROUTINE nl_set_history_interval_s
SUBROUTINE nl_set_inputout_interval_mo ( id_id , inputout_interval_mo )
  integer , INTENT(IN) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8977 , emess)
  ENDIF
  model_config_rec%inputout_interval_mo(id_id) = inputout_interval_mo
  RETURN
END SUBROUTINE nl_set_inputout_interval_mo
SUBROUTINE nl_set_inputout_interval_d ( id_id , inputout_interval_d )
  integer , INTENT(IN) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8988 , emess)
  ENDIF
  model_config_rec%inputout_interval_d(id_id) = inputout_interval_d
  RETURN
END SUBROUTINE nl_set_inputout_interval_d
SUBROUTINE nl_set_inputout_interval_h ( id_id , inputout_interval_h )
  integer , INTENT(IN) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 8999 , emess)
  ENDIF
  model_config_rec%inputout_interval_h(id_id) = inputout_interval_h
  RETURN
END SUBROUTINE nl_set_inputout_interval_h
SUBROUTINE nl_set_inputout_interval_m ( id_id , inputout_interval_m )
  integer , INTENT(IN) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9010 , emess)
  ENDIF
  model_config_rec%inputout_interval_m(id_id) = inputout_interval_m
  RETURN
END SUBROUTINE nl_set_inputout_interval_m
SUBROUTINE nl_set_inputout_interval_s ( id_id , inputout_interval_s )
  integer , INTENT(IN) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9021 , emess)
  ENDIF
  model_config_rec%inputout_interval_s(id_id) = inputout_interval_s
  RETURN
END SUBROUTINE nl_set_inputout_interval_s
SUBROUTINE nl_set_inputout_interval ( id_id , inputout_interval )
  integer , INTENT(IN) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9032 , emess)
  ENDIF
  model_config_rec%inputout_interval(id_id) = inputout_interval
  RETURN
END SUBROUTINE nl_set_inputout_interval
SUBROUTINE nl_set_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  integer , INTENT(IN) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9043 , emess)
  ENDIF
  model_config_rec%auxhist1_interval_mo(id_id) = auxhist1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_mo
SUBROUTINE nl_set_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  integer , INTENT(IN) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9054 , emess)
  ENDIF
  model_config_rec%auxhist1_interval_d(id_id) = auxhist1_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_d
SUBROUTINE nl_set_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  integer , INTENT(IN) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9065 , emess)
  ENDIF
  model_config_rec%auxhist1_interval_h(id_id) = auxhist1_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_h
SUBROUTINE nl_set_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  integer , INTENT(IN) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9076 , emess)
  ENDIF
  model_config_rec%auxhist1_interval_m(id_id) = auxhist1_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_m
SUBROUTINE nl_set_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  integer , INTENT(IN) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9087 , emess)
  ENDIF
  model_config_rec%auxhist1_interval_s(id_id) = auxhist1_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_s
SUBROUTINE nl_set_auxhist1_interval ( id_id , auxhist1_interval )
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9098 , emess)
  ENDIF
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE nl_set_auxhist1_interval
SUBROUTINE nl_set_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  integer , INTENT(IN) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9109 , emess)
  ENDIF
  model_config_rec%auxhist2_interval_mo(id_id) = auxhist2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_mo
SUBROUTINE nl_set_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  integer , INTENT(IN) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9120 , emess)
  ENDIF
  model_config_rec%auxhist2_interval_d(id_id) = auxhist2_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_d
SUBROUTINE nl_set_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  integer , INTENT(IN) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9131 , emess)
  ENDIF
  model_config_rec%auxhist2_interval_h(id_id) = auxhist2_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_h
SUBROUTINE nl_set_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  integer , INTENT(IN) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9142 , emess)
  ENDIF
  model_config_rec%auxhist2_interval_m(id_id) = auxhist2_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_m
SUBROUTINE nl_set_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  integer , INTENT(IN) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9153 , emess)
  ENDIF
  model_config_rec%auxhist2_interval_s(id_id) = auxhist2_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_s
SUBROUTINE nl_set_auxhist2_interval ( id_id , auxhist2_interval )
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9164 , emess)
  ENDIF
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE nl_set_auxhist2_interval
SUBROUTINE nl_set_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  integer , INTENT(IN) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9175 , emess)
  ENDIF
  model_config_rec%auxhist3_interval_mo(id_id) = auxhist3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_mo
SUBROUTINE nl_set_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  integer , INTENT(IN) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9186 , emess)
  ENDIF
  model_config_rec%auxhist3_interval_d(id_id) = auxhist3_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_d
SUBROUTINE nl_set_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  integer , INTENT(IN) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9197 , emess)
  ENDIF
  model_config_rec%auxhist3_interval_h(id_id) = auxhist3_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_h
SUBROUTINE nl_set_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  integer , INTENT(IN) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9208 , emess)
  ENDIF
  model_config_rec%auxhist3_interval_m(id_id) = auxhist3_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_m
SUBROUTINE nl_set_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  integer , INTENT(IN) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9219 , emess)
  ENDIF
  model_config_rec%auxhist3_interval_s(id_id) = auxhist3_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_s
SUBROUTINE nl_set_auxhist3_interval ( id_id , auxhist3_interval )
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9230 , emess)
  ENDIF
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE nl_set_auxhist3_interval
SUBROUTINE nl_set_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  integer , INTENT(IN) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9241 , emess)
  ENDIF
  model_config_rec%auxhist4_interval_mo(id_id) = auxhist4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_mo
SUBROUTINE nl_set_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  integer , INTENT(IN) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9252 , emess)
  ENDIF
  model_config_rec%auxhist4_interval_d(id_id) = auxhist4_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_d
SUBROUTINE nl_set_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  integer , INTENT(IN) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9263 , emess)
  ENDIF
  model_config_rec%auxhist4_interval_h(id_id) = auxhist4_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_h
SUBROUTINE nl_set_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  integer , INTENT(IN) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9274 , emess)
  ENDIF
  model_config_rec%auxhist4_interval_m(id_id) = auxhist4_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_m
SUBROUTINE nl_set_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  integer , INTENT(IN) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9285 , emess)
  ENDIF
  model_config_rec%auxhist4_interval_s(id_id) = auxhist4_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_s
SUBROUTINE nl_set_auxhist4_interval ( id_id , auxhist4_interval )
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9296 , emess)
  ENDIF
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE nl_set_auxhist4_interval
SUBROUTINE nl_set_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  integer , INTENT(IN) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9307 , emess)
  ENDIF
  model_config_rec%auxhist5_interval_mo(id_id) = auxhist5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_mo
SUBROUTINE nl_set_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  integer , INTENT(IN) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9318 , emess)
  ENDIF
  model_config_rec%auxhist5_interval_d(id_id) = auxhist5_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_d
SUBROUTINE nl_set_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  integer , INTENT(IN) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9329 , emess)
  ENDIF
  model_config_rec%auxhist5_interval_h(id_id) = auxhist5_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_h
SUBROUTINE nl_set_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  integer , INTENT(IN) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9340 , emess)
  ENDIF
  model_config_rec%auxhist5_interval_m(id_id) = auxhist5_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_m
SUBROUTINE nl_set_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  integer , INTENT(IN) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9351 , emess)
  ENDIF
  model_config_rec%auxhist5_interval_s(id_id) = auxhist5_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_s
SUBROUTINE nl_set_auxhist5_interval ( id_id , auxhist5_interval )
  integer , INTENT(IN) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9362 , emess)
  ENDIF
  model_config_rec%auxhist5_interval(id_id) = auxhist5_interval
  RETURN
END SUBROUTINE nl_set_auxhist5_interval
SUBROUTINE nl_set_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  integer , INTENT(IN) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9373 , emess)
  ENDIF
  model_config_rec%auxinput1_interval_mo(id_id) = auxinput1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_mo
SUBROUTINE nl_set_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  integer , INTENT(IN) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9384 , emess)
  ENDIF
  model_config_rec%auxinput1_interval_d(id_id) = auxinput1_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_d
SUBROUTINE nl_set_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  integer , INTENT(IN) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9395 , emess)
  ENDIF
  model_config_rec%auxinput1_interval_h(id_id) = auxinput1_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_h
SUBROUTINE nl_set_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  integer , INTENT(IN) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9406 , emess)
  ENDIF
  model_config_rec%auxinput1_interval_m(id_id) = auxinput1_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_m
SUBROUTINE nl_set_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  integer , INTENT(IN) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9417 , emess)
  ENDIF
  model_config_rec%auxinput1_interval_s(id_id) = auxinput1_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_s
SUBROUTINE nl_set_auxinput1_interval ( id_id , auxinput1_interval )
  integer , INTENT(IN) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9428 , emess)
  ENDIF
  model_config_rec%auxinput1_interval(id_id) = auxinput1_interval
  RETURN
END SUBROUTINE nl_set_auxinput1_interval
SUBROUTINE nl_set_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  integer , INTENT(IN) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9439 , emess)
  ENDIF
  model_config_rec%auxinput2_interval_mo(id_id) = auxinput2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_mo
SUBROUTINE nl_set_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  integer , INTENT(IN) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9450 , emess)
  ENDIF
  model_config_rec%auxinput2_interval_d(id_id) = auxinput2_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_d
SUBROUTINE nl_set_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  integer , INTENT(IN) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9461 , emess)
  ENDIF
  model_config_rec%auxinput2_interval_h(id_id) = auxinput2_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_h
SUBROUTINE nl_set_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  integer , INTENT(IN) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9472 , emess)
  ENDIF
  model_config_rec%auxinput2_interval_m(id_id) = auxinput2_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_m
SUBROUTINE nl_set_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  integer , INTENT(IN) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9483 , emess)
  ENDIF
  model_config_rec%auxinput2_interval_s(id_id) = auxinput2_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_s
SUBROUTINE nl_set_auxinput2_interval ( id_id , auxinput2_interval )
  integer , INTENT(IN) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9494 , emess)
  ENDIF
  model_config_rec%auxinput2_interval(id_id) = auxinput2_interval
  RETURN
END SUBROUTINE nl_set_auxinput2_interval
SUBROUTINE nl_set_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  integer , INTENT(IN) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9505 , emess)
  ENDIF
  model_config_rec%auxinput3_interval_mo(id_id) = auxinput3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_mo
SUBROUTINE nl_set_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  integer , INTENT(IN) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9516 , emess)
  ENDIF
  model_config_rec%auxinput3_interval_d(id_id) = auxinput3_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_d
SUBROUTINE nl_set_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  integer , INTENT(IN) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9527 , emess)
  ENDIF
  model_config_rec%auxinput3_interval_h(id_id) = auxinput3_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_h
SUBROUTINE nl_set_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  integer , INTENT(IN) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9538 , emess)
  ENDIF
  model_config_rec%auxinput3_interval_m(id_id) = auxinput3_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_m
SUBROUTINE nl_set_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  integer , INTENT(IN) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9549 , emess)
  ENDIF
  model_config_rec%auxinput3_interval_s(id_id) = auxinput3_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_s
SUBROUTINE nl_set_auxinput3_interval ( id_id , auxinput3_interval )
  integer , INTENT(IN) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9560 , emess)
  ENDIF
  model_config_rec%auxinput3_interval(id_id) = auxinput3_interval
  RETURN
END SUBROUTINE nl_set_auxinput3_interval
SUBROUTINE nl_set_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  integer , INTENT(IN) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9571 , emess)
  ENDIF
  model_config_rec%auxinput4_interval_mo(id_id) = auxinput4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_mo
SUBROUTINE nl_set_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  integer , INTENT(IN) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9582 , emess)
  ENDIF
  model_config_rec%auxinput4_interval_d(id_id) = auxinput4_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_d
SUBROUTINE nl_set_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  integer , INTENT(IN) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9593 , emess)
  ENDIF
  model_config_rec%auxinput4_interval_h(id_id) = auxinput4_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_h
SUBROUTINE nl_set_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  integer , INTENT(IN) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9604 , emess)
  ENDIF
  model_config_rec%auxinput4_interval_m(id_id) = auxinput4_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_m
SUBROUTINE nl_set_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  integer , INTENT(IN) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9615 , emess)
  ENDIF
  model_config_rec%auxinput4_interval_s(id_id) = auxinput4_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_s
SUBROUTINE nl_set_auxinput4_interval ( id_id , auxinput4_interval )
  integer , INTENT(IN) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9626 , emess)
  ENDIF
  model_config_rec%auxinput4_interval(id_id) = auxinput4_interval
  RETURN
END SUBROUTINE nl_set_auxinput4_interval
SUBROUTINE nl_set_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  integer , INTENT(IN) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9637 , emess)
  ENDIF
  model_config_rec%auxinput5_interval_mo(id_id) = auxinput5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_mo
SUBROUTINE nl_set_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  integer , INTENT(IN) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9648 , emess)
  ENDIF
  model_config_rec%auxinput5_interval_d(id_id) = auxinput5_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_d
SUBROUTINE nl_set_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  integer , INTENT(IN) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9659 , emess)
  ENDIF
  model_config_rec%auxinput5_interval_h(id_id) = auxinput5_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_h
SUBROUTINE nl_set_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  integer , INTENT(IN) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9670 , emess)
  ENDIF
  model_config_rec%auxinput5_interval_m(id_id) = auxinput5_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_m
SUBROUTINE nl_set_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  integer , INTENT(IN) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9681 , emess)
  ENDIF
  model_config_rec%auxinput5_interval_s(id_id) = auxinput5_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_s
SUBROUTINE nl_set_auxinput5_interval ( id_id , auxinput5_interval )
  integer , INTENT(IN) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9692 , emess)
  ENDIF
  model_config_rec%auxinput5_interval(id_id) = auxinput5_interval
  RETURN
END SUBROUTINE nl_set_auxinput5_interval
SUBROUTINE nl_set_restart_interval_mo ( id_id , restart_interval_mo )
  integer , INTENT(IN) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval_mo: restart_interval_mo applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_mo = restart_interval_mo 
  RETURN
END SUBROUTINE nl_set_restart_interval_mo
SUBROUTINE nl_set_restart_interval_d ( id_id , restart_interval_d )
  integer , INTENT(IN) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval_d: restart_interval_d applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_d = restart_interval_d 
  RETURN
END SUBROUTINE nl_set_restart_interval_d
SUBROUTINE nl_set_restart_interval_h ( id_id , restart_interval_h )
  integer , INTENT(IN) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval_h: restart_interval_h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_h = restart_interval_h 
  RETURN
END SUBROUTINE nl_set_restart_interval_h
SUBROUTINE nl_set_restart_interval_m ( id_id , restart_interval_m )
  integer , INTENT(IN) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval_m: restart_interval_m applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_m = restart_interval_m 
  RETURN
END SUBROUTINE nl_set_restart_interval_m
SUBROUTINE nl_set_restart_interval_s ( id_id , restart_interval_s )
  integer , INTENT(IN) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_interval_s: restart_interval_s applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_s = restart_interval_s 
  RETURN
END SUBROUTINE nl_set_restart_interval_s
SUBROUTINE nl_set_history_begin_y ( id_id , history_begin_y )
  integer , INTENT(IN) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9753 , emess)
  ENDIF
  model_config_rec%history_begin_y(id_id) = history_begin_y
  RETURN
END SUBROUTINE nl_set_history_begin_y
SUBROUTINE nl_set_history_begin_mo ( id_id , history_begin_mo )
  integer , INTENT(IN) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9764 , emess)
  ENDIF
  model_config_rec%history_begin_mo(id_id) = history_begin_mo
  RETURN
END SUBROUTINE nl_set_history_begin_mo
SUBROUTINE nl_set_history_begin_d ( id_id , history_begin_d )
  integer , INTENT(IN) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9775 , emess)
  ENDIF
  model_config_rec%history_begin_d(id_id) = history_begin_d
  RETURN
END SUBROUTINE nl_set_history_begin_d
SUBROUTINE nl_set_history_begin_h ( id_id , history_begin_h )
  integer , INTENT(IN) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9786 , emess)
  ENDIF
  model_config_rec%history_begin_h(id_id) = history_begin_h
  RETURN
END SUBROUTINE nl_set_history_begin_h
SUBROUTINE nl_set_history_begin_m ( id_id , history_begin_m )
  integer , INTENT(IN) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9797 , emess)
  ENDIF
  model_config_rec%history_begin_m(id_id) = history_begin_m
  RETURN
END SUBROUTINE nl_set_history_begin_m
SUBROUTINE nl_set_history_begin_s ( id_id , history_begin_s )
  integer , INTENT(IN) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9808 , emess)
  ENDIF
  model_config_rec%history_begin_s(id_id) = history_begin_s
  RETURN
END SUBROUTINE nl_set_history_begin_s
SUBROUTINE nl_set_inputout_begin_y ( id_id , inputout_begin_y )
  integer , INTENT(IN) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9819 , emess)
  ENDIF
  model_config_rec%inputout_begin_y(id_id) = inputout_begin_y
  RETURN
END SUBROUTINE nl_set_inputout_begin_y
SUBROUTINE nl_set_inputout_begin_mo ( id_id , inputout_begin_mo )
  integer , INTENT(IN) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9830 , emess)
  ENDIF
  model_config_rec%inputout_begin_mo(id_id) = inputout_begin_mo
  RETURN
END SUBROUTINE nl_set_inputout_begin_mo
SUBROUTINE nl_set_inputout_begin_d ( id_id , inputout_begin_d )
  integer , INTENT(IN) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9841 , emess)
  ENDIF
  model_config_rec%inputout_begin_d(id_id) = inputout_begin_d
  RETURN
END SUBROUTINE nl_set_inputout_begin_d
SUBROUTINE nl_set_inputout_begin_h ( id_id , inputout_begin_h )
  integer , INTENT(IN) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9852 , emess)
  ENDIF
  model_config_rec%inputout_begin_h(id_id) = inputout_begin_h
  RETURN
END SUBROUTINE nl_set_inputout_begin_h
SUBROUTINE nl_set_inputout_begin_m ( id_id , inputout_begin_m )
  integer , INTENT(IN) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9863 , emess)
  ENDIF
  model_config_rec%inputout_begin_m(id_id) = inputout_begin_m
  RETURN
END SUBROUTINE nl_set_inputout_begin_m
SUBROUTINE nl_set_inputout_begin_s ( id_id , inputout_begin_s )
  integer , INTENT(IN) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9874 , emess)
  ENDIF
  model_config_rec%inputout_begin_s(id_id) = inputout_begin_s
  RETURN
END SUBROUTINE nl_set_inputout_begin_s
SUBROUTINE nl_set_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  integer , INTENT(IN) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9885 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_y(id_id) = auxhist1_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_y
SUBROUTINE nl_set_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  integer , INTENT(IN) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9896 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_mo(id_id) = auxhist1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_mo
SUBROUTINE nl_set_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  integer , INTENT(IN) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9907 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_d(id_id) = auxhist1_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_d
SUBROUTINE nl_set_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  integer , INTENT(IN) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9918 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_h(id_id) = auxhist1_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_h
SUBROUTINE nl_set_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  integer , INTENT(IN) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9929 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_m(id_id) = auxhist1_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_m
SUBROUTINE nl_set_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  integer , INTENT(IN) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9940 , emess)
  ENDIF
  model_config_rec%auxhist1_begin_s(id_id) = auxhist1_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_s
SUBROUTINE nl_set_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  integer , INTENT(IN) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9951 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_y(id_id) = auxhist2_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_y
SUBROUTINE nl_set_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  integer , INTENT(IN) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9962 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_mo(id_id) = auxhist2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_mo
SUBROUTINE nl_set_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  integer , INTENT(IN) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9973 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_d(id_id) = auxhist2_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_d
SUBROUTINE nl_set_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  integer , INTENT(IN) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9984 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_h(id_id) = auxhist2_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_h
SUBROUTINE nl_set_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  integer , INTENT(IN) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 9995 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_m(id_id) = auxhist2_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_m
SUBROUTINE nl_set_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  integer , INTENT(IN) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10006 , emess)
  ENDIF
  model_config_rec%auxhist2_begin_s(id_id) = auxhist2_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_s
SUBROUTINE nl_set_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  integer , INTENT(IN) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10017 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_y(id_id) = auxhist3_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_y
SUBROUTINE nl_set_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  integer , INTENT(IN) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10028 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_mo(id_id) = auxhist3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_mo
SUBROUTINE nl_set_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  integer , INTENT(IN) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10039 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_d(id_id) = auxhist3_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_d
SUBROUTINE nl_set_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  integer , INTENT(IN) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10050 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_h(id_id) = auxhist3_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_h
SUBROUTINE nl_set_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  integer , INTENT(IN) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10061 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_m(id_id) = auxhist3_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_m
SUBROUTINE nl_set_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  integer , INTENT(IN) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10072 , emess)
  ENDIF
  model_config_rec%auxhist3_begin_s(id_id) = auxhist3_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_s
SUBROUTINE nl_set_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  integer , INTENT(IN) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10083 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_y(id_id) = auxhist4_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_y
SUBROUTINE nl_set_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  integer , INTENT(IN) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10094 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_mo(id_id) = auxhist4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_mo
SUBROUTINE nl_set_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  integer , INTENT(IN) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10105 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_d(id_id) = auxhist4_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_d
SUBROUTINE nl_set_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  integer , INTENT(IN) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10116 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_h(id_id) = auxhist4_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_h
SUBROUTINE nl_set_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  integer , INTENT(IN) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10127 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_m(id_id) = auxhist4_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_m
SUBROUTINE nl_set_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  integer , INTENT(IN) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10138 , emess)
  ENDIF
  model_config_rec%auxhist4_begin_s(id_id) = auxhist4_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_s
SUBROUTINE nl_set_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  integer , INTENT(IN) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10149 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_y(id_id) = auxhist5_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_y
SUBROUTINE nl_set_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  integer , INTENT(IN) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10160 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_mo(id_id) = auxhist5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_mo
SUBROUTINE nl_set_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  integer , INTENT(IN) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10171 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_d(id_id) = auxhist5_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_d
SUBROUTINE nl_set_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  integer , INTENT(IN) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10182 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_h(id_id) = auxhist5_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_h
SUBROUTINE nl_set_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  integer , INTENT(IN) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10193 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_m(id_id) = auxhist5_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_m
SUBROUTINE nl_set_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  integer , INTENT(IN) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10204 , emess)
  ENDIF
  model_config_rec%auxhist5_begin_s(id_id) = auxhist5_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_s
SUBROUTINE nl_set_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  integer , INTENT(IN) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10215 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_y(id_id) = auxinput1_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_y
SUBROUTINE nl_set_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  integer , INTENT(IN) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10226 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_mo(id_id) = auxinput1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_mo
SUBROUTINE nl_set_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  integer , INTENT(IN) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10237 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_d(id_id) = auxinput1_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_d
SUBROUTINE nl_set_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  integer , INTENT(IN) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10248 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_h(id_id) = auxinput1_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_h
SUBROUTINE nl_set_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  integer , INTENT(IN) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10259 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_m(id_id) = auxinput1_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_m
SUBROUTINE nl_set_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  integer , INTENT(IN) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10270 , emess)
  ENDIF
  model_config_rec%auxinput1_begin_s(id_id) = auxinput1_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_s
SUBROUTINE nl_set_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  integer , INTENT(IN) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10281 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_y(id_id) = auxinput2_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_y
SUBROUTINE nl_set_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  integer , INTENT(IN) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10292 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_mo(id_id) = auxinput2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_mo
SUBROUTINE nl_set_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  integer , INTENT(IN) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10303 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_d(id_id) = auxinput2_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_d
SUBROUTINE nl_set_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  integer , INTENT(IN) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10314 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_h(id_id) = auxinput2_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_h
SUBROUTINE nl_set_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  integer , INTENT(IN) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10325 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_m(id_id) = auxinput2_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_m
SUBROUTINE nl_set_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  integer , INTENT(IN) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10336 , emess)
  ENDIF
  model_config_rec%auxinput2_begin_s(id_id) = auxinput2_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_s
SUBROUTINE nl_set_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  integer , INTENT(IN) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10347 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_y(id_id) = auxinput3_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_y
SUBROUTINE nl_set_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  integer , INTENT(IN) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10358 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_mo(id_id) = auxinput3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_mo
SUBROUTINE nl_set_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  integer , INTENT(IN) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10369 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_d(id_id) = auxinput3_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_d
SUBROUTINE nl_set_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  integer , INTENT(IN) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10380 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_h(id_id) = auxinput3_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_h
SUBROUTINE nl_set_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  integer , INTENT(IN) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10391 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_m(id_id) = auxinput3_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_m
SUBROUTINE nl_set_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  integer , INTENT(IN) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10402 , emess)
  ENDIF
  model_config_rec%auxinput3_begin_s(id_id) = auxinput3_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_s
SUBROUTINE nl_set_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  integer , INTENT(IN) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10413 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_y(id_id) = auxinput4_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_y
SUBROUTINE nl_set_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  integer , INTENT(IN) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10424 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_mo(id_id) = auxinput4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_mo
SUBROUTINE nl_set_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  integer , INTENT(IN) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10435 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_d(id_id) = auxinput4_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_d
SUBROUTINE nl_set_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  integer , INTENT(IN) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10446 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_h(id_id) = auxinput4_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_h
SUBROUTINE nl_set_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  integer , INTENT(IN) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10457 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_m(id_id) = auxinput4_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_m
SUBROUTINE nl_set_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  integer , INTENT(IN) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10468 , emess)
  ENDIF
  model_config_rec%auxinput4_begin_s(id_id) = auxinput4_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_s
SUBROUTINE nl_set_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  integer , INTENT(IN) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10479 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_y(id_id) = auxinput5_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_y
SUBROUTINE nl_set_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  integer , INTENT(IN) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10490 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_mo(id_id) = auxinput5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_mo
SUBROUTINE nl_set_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  integer , INTENT(IN) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10501 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_d(id_id) = auxinput5_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_d
SUBROUTINE nl_set_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  integer , INTENT(IN) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10512 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_h(id_id) = auxinput5_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_h
SUBROUTINE nl_set_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  integer , INTENT(IN) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10523 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_m(id_id) = auxinput5_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_m
SUBROUTINE nl_set_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  integer , INTENT(IN) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10534 , emess)
  ENDIF
  model_config_rec%auxinput5_begin_s(id_id) = auxinput5_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_s
SUBROUTINE nl_set_restart_begin_y ( id_id , restart_begin_y )
  integer , INTENT(IN) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_y: restart_begin_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_y = restart_begin_y 
  RETURN
END SUBROUTINE nl_set_restart_begin_y
SUBROUTINE nl_set_restart_begin_mo ( id_id , restart_begin_mo )
  integer , INTENT(IN) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_mo: restart_begin_mo applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_mo = restart_begin_mo 
  RETURN
END SUBROUTINE nl_set_restart_begin_mo
SUBROUTINE nl_set_restart_begin_d ( id_id , restart_begin_d )
  integer , INTENT(IN) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_d: restart_begin_d applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_d = restart_begin_d 
  RETURN
END SUBROUTINE nl_set_restart_begin_d
SUBROUTINE nl_set_restart_begin_h ( id_id , restart_begin_h )
  integer , INTENT(IN) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_h: restart_begin_h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_h = restart_begin_h 
  RETURN
END SUBROUTINE nl_set_restart_begin_h
SUBROUTINE nl_set_restart_begin_m ( id_id , restart_begin_m )
  integer , INTENT(IN) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_m: restart_begin_m applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_m = restart_begin_m 
  RETURN
END SUBROUTINE nl_set_restart_begin_m
SUBROUTINE nl_set_restart_begin_s ( id_id , restart_begin_s )
  integer , INTENT(IN) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_restart_begin_s: restart_begin_s applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_s = restart_begin_s 
  RETURN
END SUBROUTINE nl_set_restart_begin_s
SUBROUTINE nl_set_history_end_y ( id_id , history_end_y )
  integer , INTENT(IN) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10605 , emess)
  ENDIF
  model_config_rec%history_end_y(id_id) = history_end_y
  RETURN
END SUBROUTINE nl_set_history_end_y
SUBROUTINE nl_set_history_end_mo ( id_id , history_end_mo )
  integer , INTENT(IN) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10616 , emess)
  ENDIF
  model_config_rec%history_end_mo(id_id) = history_end_mo
  RETURN
END SUBROUTINE nl_set_history_end_mo
SUBROUTINE nl_set_history_end_d ( id_id , history_end_d )
  integer , INTENT(IN) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10627 , emess)
  ENDIF
  model_config_rec%history_end_d(id_id) = history_end_d
  RETURN
END SUBROUTINE nl_set_history_end_d
SUBROUTINE nl_set_history_end_h ( id_id , history_end_h )
  integer , INTENT(IN) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10638 , emess)
  ENDIF
  model_config_rec%history_end_h(id_id) = history_end_h
  RETURN
END SUBROUTINE nl_set_history_end_h
SUBROUTINE nl_set_history_end_m ( id_id , history_end_m )
  integer , INTENT(IN) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10649 , emess)
  ENDIF
  model_config_rec%history_end_m(id_id) = history_end_m
  RETURN
END SUBROUTINE nl_set_history_end_m
SUBROUTINE nl_set_history_end_s ( id_id , history_end_s )
  integer , INTENT(IN) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10660 , emess)
  ENDIF
  model_config_rec%history_end_s(id_id) = history_end_s
  RETURN
END SUBROUTINE nl_set_history_end_s
SUBROUTINE nl_set_inputout_end_y ( id_id , inputout_end_y )
  integer , INTENT(IN) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10671 , emess)
  ENDIF
  model_config_rec%inputout_end_y(id_id) = inputout_end_y
  RETURN
END SUBROUTINE nl_set_inputout_end_y
SUBROUTINE nl_set_inputout_end_mo ( id_id , inputout_end_mo )
  integer , INTENT(IN) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10682 , emess)
  ENDIF
  model_config_rec%inputout_end_mo(id_id) = inputout_end_mo
  RETURN
END SUBROUTINE nl_set_inputout_end_mo
SUBROUTINE nl_set_inputout_end_d ( id_id , inputout_end_d )
  integer , INTENT(IN) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10693 , emess)
  ENDIF
  model_config_rec%inputout_end_d(id_id) = inputout_end_d
  RETURN
END SUBROUTINE nl_set_inputout_end_d
SUBROUTINE nl_set_inputout_end_h ( id_id , inputout_end_h )
  integer , INTENT(IN) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10704 , emess)
  ENDIF
  model_config_rec%inputout_end_h(id_id) = inputout_end_h
  RETURN
END SUBROUTINE nl_set_inputout_end_h
SUBROUTINE nl_set_inputout_end_m ( id_id , inputout_end_m )
  integer , INTENT(IN) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10715 , emess)
  ENDIF
  model_config_rec%inputout_end_m(id_id) = inputout_end_m
  RETURN
END SUBROUTINE nl_set_inputout_end_m
SUBROUTINE nl_set_inputout_end_s ( id_id , inputout_end_s )
  integer , INTENT(IN) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10726 , emess)
  ENDIF
  model_config_rec%inputout_end_s(id_id) = inputout_end_s
  RETURN
END SUBROUTINE nl_set_inputout_end_s
SUBROUTINE nl_set_auxhist1_end_y ( id_id , auxhist1_end_y )
  integer , INTENT(IN) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10737 , emess)
  ENDIF
  model_config_rec%auxhist1_end_y(id_id) = auxhist1_end_y
  RETURN
END SUBROUTINE nl_set_auxhist1_end_y
SUBROUTINE nl_set_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  integer , INTENT(IN) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10748 , emess)
  ENDIF
  model_config_rec%auxhist1_end_mo(id_id) = auxhist1_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_end_mo
SUBROUTINE nl_set_auxhist1_end_d ( id_id , auxhist1_end_d )
  integer , INTENT(IN) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10759 , emess)
  ENDIF
  model_config_rec%auxhist1_end_d(id_id) = auxhist1_end_d
  RETURN
END SUBROUTINE nl_set_auxhist1_end_d
SUBROUTINE nl_set_auxhist1_end_h ( id_id , auxhist1_end_h )
  integer , INTENT(IN) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10770 , emess)
  ENDIF
  model_config_rec%auxhist1_end_h(id_id) = auxhist1_end_h
  RETURN
END SUBROUTINE nl_set_auxhist1_end_h
SUBROUTINE nl_set_auxhist1_end_m ( id_id , auxhist1_end_m )
  integer , INTENT(IN) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10781 , emess)
  ENDIF
  model_config_rec%auxhist1_end_m(id_id) = auxhist1_end_m
  RETURN
END SUBROUTINE nl_set_auxhist1_end_m
SUBROUTINE nl_set_auxhist1_end_s ( id_id , auxhist1_end_s )
  integer , INTENT(IN) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10792 , emess)
  ENDIF
  model_config_rec%auxhist1_end_s(id_id) = auxhist1_end_s
  RETURN
END SUBROUTINE nl_set_auxhist1_end_s
SUBROUTINE nl_set_auxhist2_end_y ( id_id , auxhist2_end_y )
  integer , INTENT(IN) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10803 , emess)
  ENDIF
  model_config_rec%auxhist2_end_y(id_id) = auxhist2_end_y
  RETURN
END SUBROUTINE nl_set_auxhist2_end_y
SUBROUTINE nl_set_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  integer , INTENT(IN) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10814 , emess)
  ENDIF
  model_config_rec%auxhist2_end_mo(id_id) = auxhist2_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_end_mo
SUBROUTINE nl_set_auxhist2_end_d ( id_id , auxhist2_end_d )
  integer , INTENT(IN) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10825 , emess)
  ENDIF
  model_config_rec%auxhist2_end_d(id_id) = auxhist2_end_d
  RETURN
END SUBROUTINE nl_set_auxhist2_end_d
SUBROUTINE nl_set_auxhist2_end_h ( id_id , auxhist2_end_h )
  integer , INTENT(IN) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10836 , emess)
  ENDIF
  model_config_rec%auxhist2_end_h(id_id) = auxhist2_end_h
  RETURN
END SUBROUTINE nl_set_auxhist2_end_h
SUBROUTINE nl_set_auxhist2_end_m ( id_id , auxhist2_end_m )
  integer , INTENT(IN) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10847 , emess)
  ENDIF
  model_config_rec%auxhist2_end_m(id_id) = auxhist2_end_m
  RETURN
END SUBROUTINE nl_set_auxhist2_end_m
SUBROUTINE nl_set_auxhist2_end_s ( id_id , auxhist2_end_s )
  integer , INTENT(IN) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10858 , emess)
  ENDIF
  model_config_rec%auxhist2_end_s(id_id) = auxhist2_end_s
  RETURN
END SUBROUTINE nl_set_auxhist2_end_s
SUBROUTINE nl_set_auxhist3_end_y ( id_id , auxhist3_end_y )
  integer , INTENT(IN) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10869 , emess)
  ENDIF
  model_config_rec%auxhist3_end_y(id_id) = auxhist3_end_y
  RETURN
END SUBROUTINE nl_set_auxhist3_end_y
SUBROUTINE nl_set_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  integer , INTENT(IN) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10880 , emess)
  ENDIF
  model_config_rec%auxhist3_end_mo(id_id) = auxhist3_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_end_mo
SUBROUTINE nl_set_auxhist3_end_d ( id_id , auxhist3_end_d )
  integer , INTENT(IN) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10891 , emess)
  ENDIF
  model_config_rec%auxhist3_end_d(id_id) = auxhist3_end_d
  RETURN
END SUBROUTINE nl_set_auxhist3_end_d
SUBROUTINE nl_set_auxhist3_end_h ( id_id , auxhist3_end_h )
  integer , INTENT(IN) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10902 , emess)
  ENDIF
  model_config_rec%auxhist3_end_h(id_id) = auxhist3_end_h
  RETURN
END SUBROUTINE nl_set_auxhist3_end_h
SUBROUTINE nl_set_auxhist3_end_m ( id_id , auxhist3_end_m )
  integer , INTENT(IN) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10913 , emess)
  ENDIF
  model_config_rec%auxhist3_end_m(id_id) = auxhist3_end_m
  RETURN
END SUBROUTINE nl_set_auxhist3_end_m
SUBROUTINE nl_set_auxhist3_end_s ( id_id , auxhist3_end_s )
  integer , INTENT(IN) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10924 , emess)
  ENDIF
  model_config_rec%auxhist3_end_s(id_id) = auxhist3_end_s
  RETURN
END SUBROUTINE nl_set_auxhist3_end_s
SUBROUTINE nl_set_auxhist4_end_y ( id_id , auxhist4_end_y )
  integer , INTENT(IN) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10935 , emess)
  ENDIF
  model_config_rec%auxhist4_end_y(id_id) = auxhist4_end_y
  RETURN
END SUBROUTINE nl_set_auxhist4_end_y
SUBROUTINE nl_set_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  integer , INTENT(IN) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10946 , emess)
  ENDIF
  model_config_rec%auxhist4_end_mo(id_id) = auxhist4_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_end_mo
SUBROUTINE nl_set_auxhist4_end_d ( id_id , auxhist4_end_d )
  integer , INTENT(IN) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10957 , emess)
  ENDIF
  model_config_rec%auxhist4_end_d(id_id) = auxhist4_end_d
  RETURN
END SUBROUTINE nl_set_auxhist4_end_d
SUBROUTINE nl_set_auxhist4_end_h ( id_id , auxhist4_end_h )
  integer , INTENT(IN) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10968 , emess)
  ENDIF
  model_config_rec%auxhist4_end_h(id_id) = auxhist4_end_h
  RETURN
END SUBROUTINE nl_set_auxhist4_end_h
SUBROUTINE nl_set_auxhist4_end_m ( id_id , auxhist4_end_m )
  integer , INTENT(IN) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10979 , emess)
  ENDIF
  model_config_rec%auxhist4_end_m(id_id) = auxhist4_end_m
  RETURN
END SUBROUTINE nl_set_auxhist4_end_m
SUBROUTINE nl_set_auxhist4_end_s ( id_id , auxhist4_end_s )
  integer , INTENT(IN) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 10990 , emess)
  ENDIF
  model_config_rec%auxhist4_end_s(id_id) = auxhist4_end_s
  RETURN
END SUBROUTINE nl_set_auxhist4_end_s
SUBROUTINE nl_set_auxhist5_end_y ( id_id , auxhist5_end_y )
  integer , INTENT(IN) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11001 , emess)
  ENDIF
  model_config_rec%auxhist5_end_y(id_id) = auxhist5_end_y
  RETURN
END SUBROUTINE nl_set_auxhist5_end_y
SUBROUTINE nl_set_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  integer , INTENT(IN) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11012 , emess)
  ENDIF
  model_config_rec%auxhist5_end_mo(id_id) = auxhist5_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_end_mo
SUBROUTINE nl_set_auxhist5_end_d ( id_id , auxhist5_end_d )
  integer , INTENT(IN) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11023 , emess)
  ENDIF
  model_config_rec%auxhist5_end_d(id_id) = auxhist5_end_d
  RETURN
END SUBROUTINE nl_set_auxhist5_end_d
SUBROUTINE nl_set_auxhist5_end_h ( id_id , auxhist5_end_h )
  integer , INTENT(IN) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11034 , emess)
  ENDIF
  model_config_rec%auxhist5_end_h(id_id) = auxhist5_end_h
  RETURN
END SUBROUTINE nl_set_auxhist5_end_h
SUBROUTINE nl_set_auxhist5_end_m ( id_id , auxhist5_end_m )
  integer , INTENT(IN) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11045 , emess)
  ENDIF
  model_config_rec%auxhist5_end_m(id_id) = auxhist5_end_m
  RETURN
END SUBROUTINE nl_set_auxhist5_end_m
SUBROUTINE nl_set_auxhist5_end_s ( id_id , auxhist5_end_s )
  integer , INTENT(IN) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11056 , emess)
  ENDIF
  model_config_rec%auxhist5_end_s(id_id) = auxhist5_end_s
  RETURN
END SUBROUTINE nl_set_auxhist5_end_s
SUBROUTINE nl_set_auxinput1_end_y ( id_id , auxinput1_end_y )
  integer , INTENT(IN) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11067 , emess)
  ENDIF
  model_config_rec%auxinput1_end_y(id_id) = auxinput1_end_y
  RETURN
END SUBROUTINE nl_set_auxinput1_end_y
SUBROUTINE nl_set_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  integer , INTENT(IN) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11078 , emess)
  ENDIF
  model_config_rec%auxinput1_end_mo(id_id) = auxinput1_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_end_mo
SUBROUTINE nl_set_auxinput1_end_d ( id_id , auxinput1_end_d )
  integer , INTENT(IN) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11089 , emess)
  ENDIF
  model_config_rec%auxinput1_end_d(id_id) = auxinput1_end_d
  RETURN
END SUBROUTINE nl_set_auxinput1_end_d
SUBROUTINE nl_set_auxinput1_end_h ( id_id , auxinput1_end_h )
  integer , INTENT(IN) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11100 , emess)
  ENDIF
  model_config_rec%auxinput1_end_h(id_id) = auxinput1_end_h
  RETURN
END SUBROUTINE nl_set_auxinput1_end_h
SUBROUTINE nl_set_auxinput1_end_m ( id_id , auxinput1_end_m )
  integer , INTENT(IN) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11111 , emess)
  ENDIF
  model_config_rec%auxinput1_end_m(id_id) = auxinput1_end_m
  RETURN
END SUBROUTINE nl_set_auxinput1_end_m
SUBROUTINE nl_set_auxinput1_end_s ( id_id , auxinput1_end_s )
  integer , INTENT(IN) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11122 , emess)
  ENDIF
  model_config_rec%auxinput1_end_s(id_id) = auxinput1_end_s
  RETURN
END SUBROUTINE nl_set_auxinput1_end_s
SUBROUTINE nl_set_auxinput2_end_y ( id_id , auxinput2_end_y )
  integer , INTENT(IN) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11133 , emess)
  ENDIF
  model_config_rec%auxinput2_end_y(id_id) = auxinput2_end_y
  RETURN
END SUBROUTINE nl_set_auxinput2_end_y
SUBROUTINE nl_set_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  integer , INTENT(IN) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11144 , emess)
  ENDIF
  model_config_rec%auxinput2_end_mo(id_id) = auxinput2_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_end_mo
SUBROUTINE nl_set_auxinput2_end_d ( id_id , auxinput2_end_d )
  integer , INTENT(IN) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11155 , emess)
  ENDIF
  model_config_rec%auxinput2_end_d(id_id) = auxinput2_end_d
  RETURN
END SUBROUTINE nl_set_auxinput2_end_d
SUBROUTINE nl_set_auxinput2_end_h ( id_id , auxinput2_end_h )
  integer , INTENT(IN) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11166 , emess)
  ENDIF
  model_config_rec%auxinput2_end_h(id_id) = auxinput2_end_h
  RETURN
END SUBROUTINE nl_set_auxinput2_end_h
SUBROUTINE nl_set_auxinput2_end_m ( id_id , auxinput2_end_m )
  integer , INTENT(IN) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11177 , emess)
  ENDIF
  model_config_rec%auxinput2_end_m(id_id) = auxinput2_end_m
  RETURN
END SUBROUTINE nl_set_auxinput2_end_m
SUBROUTINE nl_set_auxinput2_end_s ( id_id , auxinput2_end_s )
  integer , INTENT(IN) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11188 , emess)
  ENDIF
  model_config_rec%auxinput2_end_s(id_id) = auxinput2_end_s
  RETURN
END SUBROUTINE nl_set_auxinput2_end_s
SUBROUTINE nl_set_auxinput3_end_y ( id_id , auxinput3_end_y )
  integer , INTENT(IN) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11199 , emess)
  ENDIF
  model_config_rec%auxinput3_end_y(id_id) = auxinput3_end_y
  RETURN
END SUBROUTINE nl_set_auxinput3_end_y
SUBROUTINE nl_set_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  integer , INTENT(IN) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11210 , emess)
  ENDIF
  model_config_rec%auxinput3_end_mo(id_id) = auxinput3_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_end_mo
SUBROUTINE nl_set_auxinput3_end_d ( id_id , auxinput3_end_d )
  integer , INTENT(IN) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11221 , emess)
  ENDIF
  model_config_rec%auxinput3_end_d(id_id) = auxinput3_end_d
  RETURN
END SUBROUTINE nl_set_auxinput3_end_d
SUBROUTINE nl_set_auxinput3_end_h ( id_id , auxinput3_end_h )
  integer , INTENT(IN) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11232 , emess)
  ENDIF
  model_config_rec%auxinput3_end_h(id_id) = auxinput3_end_h
  RETURN
END SUBROUTINE nl_set_auxinput3_end_h
SUBROUTINE nl_set_auxinput3_end_m ( id_id , auxinput3_end_m )
  integer , INTENT(IN) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11243 , emess)
  ENDIF
  model_config_rec%auxinput3_end_m(id_id) = auxinput3_end_m
  RETURN
END SUBROUTINE nl_set_auxinput3_end_m
SUBROUTINE nl_set_auxinput3_end_s ( id_id , auxinput3_end_s )
  integer , INTENT(IN) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11254 , emess)
  ENDIF
  model_config_rec%auxinput3_end_s(id_id) = auxinput3_end_s
  RETURN
END SUBROUTINE nl_set_auxinput3_end_s
SUBROUTINE nl_set_auxinput4_end_y ( id_id , auxinput4_end_y )
  integer , INTENT(IN) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11265 , emess)
  ENDIF
  model_config_rec%auxinput4_end_y(id_id) = auxinput4_end_y
  RETURN
END SUBROUTINE nl_set_auxinput4_end_y
SUBROUTINE nl_set_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  integer , INTENT(IN) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11276 , emess)
  ENDIF
  model_config_rec%auxinput4_end_mo(id_id) = auxinput4_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_end_mo
SUBROUTINE nl_set_auxinput4_end_d ( id_id , auxinput4_end_d )
  integer , INTENT(IN) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11287 , emess)
  ENDIF
  model_config_rec%auxinput4_end_d(id_id) = auxinput4_end_d
  RETURN
END SUBROUTINE nl_set_auxinput4_end_d
SUBROUTINE nl_set_auxinput4_end_h ( id_id , auxinput4_end_h )
  integer , INTENT(IN) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11298 , emess)
  ENDIF
  model_config_rec%auxinput4_end_h(id_id) = auxinput4_end_h
  RETURN
END SUBROUTINE nl_set_auxinput4_end_h
SUBROUTINE nl_set_auxinput4_end_m ( id_id , auxinput4_end_m )
  integer , INTENT(IN) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11309 , emess)
  ENDIF
  model_config_rec%auxinput4_end_m(id_id) = auxinput4_end_m
  RETURN
END SUBROUTINE nl_set_auxinput4_end_m
SUBROUTINE nl_set_auxinput4_end_s ( id_id , auxinput4_end_s )
  integer , INTENT(IN) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11320 , emess)
  ENDIF
  model_config_rec%auxinput4_end_s(id_id) = auxinput4_end_s
  RETURN
END SUBROUTINE nl_set_auxinput4_end_s
SUBROUTINE nl_set_auxinput5_end_y ( id_id , auxinput5_end_y )
  integer , INTENT(IN) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11331 , emess)
  ENDIF
  model_config_rec%auxinput5_end_y(id_id) = auxinput5_end_y
  RETURN
END SUBROUTINE nl_set_auxinput5_end_y
SUBROUTINE nl_set_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  integer , INTENT(IN) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11342 , emess)
  ENDIF
  model_config_rec%auxinput5_end_mo(id_id) = auxinput5_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_end_mo
SUBROUTINE nl_set_auxinput5_end_d ( id_id , auxinput5_end_d )
  integer , INTENT(IN) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11353 , emess)
  ENDIF
  model_config_rec%auxinput5_end_d(id_id) = auxinput5_end_d
  RETURN
END SUBROUTINE nl_set_auxinput5_end_d
SUBROUTINE nl_set_auxinput5_end_h ( id_id , auxinput5_end_h )
  integer , INTENT(IN) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11364 , emess)
  ENDIF
  model_config_rec%auxinput5_end_h(id_id) = auxinput5_end_h
  RETURN
END SUBROUTINE nl_set_auxinput5_end_h
SUBROUTINE nl_set_auxinput5_end_m ( id_id , auxinput5_end_m )
  integer , INTENT(IN) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11375 , emess)
  ENDIF
  model_config_rec%auxinput5_end_m(id_id) = auxinput5_end_m
  RETURN
END SUBROUTINE nl_set_auxinput5_end_m
SUBROUTINE nl_set_auxinput5_end_s ( id_id , auxinput5_end_s )
  integer , INTENT(IN) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11386 , emess)
  ENDIF
  model_config_rec%auxinput5_end_s(id_id) = auxinput5_end_s
  RETURN
END SUBROUTINE nl_set_auxinput5_end_s
SUBROUTINE nl_set_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  integer , INTENT(IN) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxinput1: io_form_auxinput1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput1
SUBROUTINE nl_set_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  integer , INTENT(IN) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxinput2: io_form_auxinput2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput2 = io_form_auxinput2 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput2
SUBROUTINE nl_set_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  integer , INTENT(IN) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxinput3: io_form_auxinput3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput3 = io_form_auxinput3 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput3
SUBROUTINE nl_set_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  integer , INTENT(IN) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxinput4: io_form_auxinput4 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput4 = io_form_auxinput4 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput4
SUBROUTINE nl_set_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  integer , INTENT(IN) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxinput5: io_form_auxinput5 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput5 = io_form_auxinput5 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput5
SUBROUTINE nl_set_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  integer , INTENT(IN) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxhist1: io_form_auxhist1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist1
SUBROUTINE nl_set_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  integer , INTENT(IN) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxhist2: io_form_auxhist2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist2
SUBROUTINE nl_set_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  integer , INTENT(IN) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxhist3: io_form_auxhist3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist3
SUBROUTINE nl_set_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  integer , INTENT(IN) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxhist4: io_form_auxhist4 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist4
SUBROUTINE nl_set_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  integer , INTENT(IN) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_io_form_auxhist5: io_form_auxhist5 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist5 = io_form_auxhist5 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist5
SUBROUTINE nl_set_julyr ( id_id , julyr )
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_julyr: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11497 , emess)
  ENDIF
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE nl_set_julyr
SUBROUTINE nl_set_julday ( id_id , julday )
  integer , INTENT(IN) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_julday: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11508 , emess)
  ENDIF
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE nl_set_julday
SUBROUTINE nl_set_gmt ( id_id , gmt )
  real , INTENT(IN) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gmt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11519 , emess)
  ENDIF
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE nl_set_gmt
SUBROUTINE nl_set_input_inname ( id_id , input_inname )
  character*256 , INTENT(IN) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_input_inname: input_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%input_inname = input_inname 
  RETURN
END SUBROUTINE nl_set_input_inname
SUBROUTINE nl_set_input_outname ( id_id , input_outname )
  character*256 , INTENT(IN) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_input_outname: input_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%input_outname = input_outname 
  RETURN
END SUBROUTINE nl_set_input_outname
SUBROUTINE nl_set_bdy_inname ( id_id , bdy_inname )
  character*256 , INTENT(IN) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_bdy_inname: bdy_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%bdy_inname = bdy_inname 
  RETURN
END SUBROUTINE nl_set_bdy_inname
SUBROUTINE nl_set_bdy_outname ( id_id , bdy_outname )
  character*256 , INTENT(IN) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_bdy_outname: bdy_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%bdy_outname = bdy_outname 
  RETURN
END SUBROUTINE nl_set_bdy_outname
SUBROUTINE nl_set_rst_inname ( id_id , rst_inname )
  character*256 , INTENT(IN) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_rst_inname: rst_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rst_inname = rst_inname 
  RETURN
END SUBROUTINE nl_set_rst_inname
SUBROUTINE nl_set_rst_outname ( id_id , rst_outname )
  character*256 , INTENT(IN) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_rst_outname: rst_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rst_outname = rst_outname 
  RETURN
END SUBROUTINE nl_set_rst_outname
SUBROUTINE nl_set_write_input ( id_id , write_input )
  logical , INTENT(IN) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_write_input: write_input applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%write_input = write_input 
  RETURN
END SUBROUTINE nl_set_write_input
SUBROUTINE nl_set_write_restart_at_0h ( id_id , write_restart_at_0h )
  logical , INTENT(IN) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_write_restart_at_0h: write_restart_at_0h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%write_restart_at_0h = write_restart_at_0h 
  RETURN
END SUBROUTINE nl_set_write_restart_at_0h
SUBROUTINE nl_set_time_step ( id_id , time_step )
  integer , INTENT(IN) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_time_step: time_step applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step = time_step 
  RETURN
END SUBROUTINE nl_set_time_step
SUBROUTINE nl_set_time_step_fract_num ( id_id , time_step_fract_num )
  integer , INTENT(IN) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_time_step_fract_num: time_step_fract_num applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step_fract_num = time_step_fract_num 
  RETURN
END SUBROUTINE nl_set_time_step_fract_num
SUBROUTINE nl_set_time_step_fract_den ( id_id , time_step_fract_den )
  integer , INTENT(IN) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_time_step_fract_den: time_step_fract_den applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step_fract_den = time_step_fract_den 
  RETURN
END SUBROUTINE nl_set_time_step_fract_den
SUBROUTINE nl_set_max_dom ( id_id , max_dom )
  integer , INTENT(IN) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_max_dom: max_dom applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%max_dom = max_dom 
  RETURN
END SUBROUTINE nl_set_max_dom
SUBROUTINE nl_set_s_we ( id_id , s_we )
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11650 , emess)
  ENDIF
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE nl_set_s_we
SUBROUTINE nl_set_e_we ( id_id , e_we )
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11661 , emess)
  ENDIF
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE nl_set_e_we
SUBROUTINE nl_set_s_sn ( id_id , s_sn )
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11672 , emess)
  ENDIF
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE nl_set_s_sn
SUBROUTINE nl_set_e_sn ( id_id , e_sn )
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11683 , emess)
  ENDIF
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE nl_set_e_sn
SUBROUTINE nl_set_s_vert ( id_id , s_vert )
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11694 , emess)
  ENDIF
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE nl_set_s_vert
SUBROUTINE nl_set_e_vert ( id_id , e_vert )
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11705 , emess)
  ENDIF
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE nl_set_e_vert
SUBROUTINE nl_set_dx ( id_id , dx )
  real , INTENT(IN) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dx: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11716 , emess)
  ENDIF
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE nl_set_dx
SUBROUTINE nl_set_dy ( id_id , dy )
  real , INTENT(IN) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dy: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11727 , emess)
  ENDIF
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE nl_set_dy
SUBROUTINE nl_set_grid_id ( id_id , grid_id )
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_grid_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11738 , emess)
  ENDIF
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE nl_set_grid_id
SUBROUTINE nl_set_parent_id ( id_id , parent_id )
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11749 , emess)
  ENDIF
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE nl_set_parent_id
SUBROUTINE nl_set_level ( id_id , level )
  integer , INTENT(IN) :: level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_level: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11760 , emess)
  ENDIF
  model_config_rec%level(id_id) = level
  RETURN
END SUBROUTINE nl_set_level
SUBROUTINE nl_set_i_parent_start ( id_id , i_parent_start )
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_i_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11771 , emess)
  ENDIF
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE nl_set_i_parent_start
SUBROUTINE nl_set_j_parent_start ( id_id , j_parent_start )
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_j_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11782 , emess)
  ENDIF
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE nl_set_j_parent_start
SUBROUTINE nl_set_parent_grid_ratio ( id_id , parent_grid_ratio )
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11793 , emess)
  ENDIF
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE nl_set_parent_grid_ratio
SUBROUTINE nl_set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11804 , emess)
  ENDIF
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE nl_set_parent_time_step_ratio
SUBROUTINE nl_set_feedback ( id_id , feedback )
  integer , INTENT(IN) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_feedback: feedback applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%feedback = feedback 
  RETURN
END SUBROUTINE nl_set_feedback
SUBROUTINE nl_set_smooth_option ( id_id , smooth_option )
  integer , INTENT(IN) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_smooth_option: smooth_option applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%smooth_option = smooth_option 
  RETURN
END SUBROUTINE nl_set_smooth_option
SUBROUTINE nl_set_ztop ( id_id , ztop )
  real , INTENT(IN) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ztop: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11835 , emess)
  ENDIF
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE nl_set_ztop
SUBROUTINE nl_set_moad_grid_ratio ( id_id , moad_grid_ratio )
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11846 , emess)
  ENDIF
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE nl_set_moad_grid_ratio
SUBROUTINE nl_set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11857 , emess)
  ENDIF
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE nl_set_moad_time_step_ratio
SUBROUTINE nl_set_shw ( id_id , shw )
  integer , INTENT(IN) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_shw: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11868 , emess)
  ENDIF
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE nl_set_shw
SUBROUTINE nl_set_coral_dist ( id_id , coral_dist )
  integer , INTENT(IN) :: coral_dist
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_coral_dist: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11879 , emess)
  ENDIF
  model_config_rec%coral_dist(id_id) = coral_dist
  RETURN
END SUBROUTINE nl_set_coral_dist
SUBROUTINE nl_set_tile_sz_x ( id_id , tile_sz_x )
  integer , INTENT(IN) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_tile_sz_x: tile_sz_x applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%tile_sz_x = tile_sz_x 
  RETURN
END SUBROUTINE nl_set_tile_sz_x
SUBROUTINE nl_set_tile_sz_y ( id_id , tile_sz_y )
  integer , INTENT(IN) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_tile_sz_y: tile_sz_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%tile_sz_y = tile_sz_y 
  RETURN
END SUBROUTINE nl_set_tile_sz_y
SUBROUTINE nl_set_numtiles ( id_id , numtiles )
  integer , INTENT(IN) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_numtiles: numtiles applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%numtiles = numtiles 
  RETURN
END SUBROUTINE nl_set_numtiles
SUBROUTINE nl_set_nproc_x ( id_id , nproc_x )
  integer , INTENT(IN) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_nproc_x: nproc_x applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%nproc_x = nproc_x 
  RETURN
END SUBROUTINE nl_set_nproc_x
SUBROUTINE nl_set_nproc_y ( id_id , nproc_y )
  integer , INTENT(IN) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_nproc_y: nproc_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%nproc_y = nproc_y 
  RETURN
END SUBROUTINE nl_set_nproc_y
SUBROUTINE nl_set_irand ( id_id , irand )
  integer , INTENT(IN) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_irand: irand applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%irand = irand 
  RETURN
END SUBROUTINE nl_set_irand
SUBROUTINE nl_set_dt ( id_id , dt )
  real , INTENT(IN) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11950 , emess)
  ENDIF
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE nl_set_dt
SUBROUTINE nl_set_num_moves ( id_id , num_moves )
  integer , INTENT(IN) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_num_moves: num_moves applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_moves = num_moves 
  RETURN
END SUBROUTINE nl_set_num_moves
SUBROUTINE nl_set_move_id ( id_id , move_id )
  integer , INTENT(IN) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_id: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11971 , emess)
  ENDIF
  model_config_rec%move_id(id_id) = move_id
  RETURN
END SUBROUTINE nl_set_move_id
SUBROUTINE nl_set_move_interval ( id_id , move_interval )
  integer , INTENT(IN) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_interval: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11982 , emess)
  ENDIF
  model_config_rec%move_interval(id_id) = move_interval
  RETURN
END SUBROUTINE nl_set_move_interval
SUBROUTINE nl_set_move_cd_x ( id_id , move_cd_x )
  integer , INTENT(IN) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_cd_x: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 11993 , emess)
  ENDIF
  model_config_rec%move_cd_x(id_id) = move_cd_x
  RETURN
END SUBROUTINE nl_set_move_cd_x
SUBROUTINE nl_set_move_cd_y ( id_id , move_cd_y )
  integer , INTENT(IN) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_cd_y: Out of range move number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12004 , emess)
  ENDIF
  model_config_rec%move_cd_y(id_id) = move_cd_y
  RETURN
END SUBROUTINE nl_set_move_cd_y
SUBROUTINE nl_set_test_rk_step_prep ( id_id , test_rk_step_prep )
  logical , INTENT(IN) :: test_rk_step_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_rk_step_prep: test_rk_step_prep applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_rk_step_prep = test_rk_step_prep 
  RETURN
END SUBROUTINE nl_set_test_rk_step_prep
SUBROUTINE nl_set_test_init_zero_tendency ( id_id , test_init_zero_tendency )
  logical , INTENT(IN) :: test_init_zero_tendency
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_init_zero_tendency: test_init_zero_tendency applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_init_zero_tendency = test_init_zero_tendency 
  RETURN
END SUBROUTINE nl_set_test_init_zero_tendency
SUBROUTINE nl_set_test_phy_prep ( id_id , test_phy_prep )
  logical , INTENT(IN) :: test_phy_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_phy_prep: test_phy_prep applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_phy_prep = test_phy_prep 
  RETURN
END SUBROUTINE nl_set_test_phy_prep
SUBROUTINE nl_set_test_calculate_km_kh ( id_id , test_calculate_km_kh )
  logical , INTENT(IN) :: test_calculate_km_kh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_calculate_km_kh: test_calculate_km_kh applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_calculate_km_kh = test_calculate_km_kh 
  RETURN
END SUBROUTINE nl_set_test_calculate_km_kh
SUBROUTINE nl_set_test_rk_tendency ( id_id , test_rk_tendency )
  logical , INTENT(IN) :: test_rk_tendency
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_rk_tendency: test_rk_tendency applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_rk_tendency = test_rk_tendency 
  RETURN
END SUBROUTINE nl_set_test_rk_tendency
SUBROUTINE nl_set_test_advect_scalar ( id_id , test_advect_scalar )
  logical , INTENT(IN) :: test_advect_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_advect_scalar: test_advect_scalar applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_advect_scalar = test_advect_scalar 
  RETURN
END SUBROUTINE nl_set_test_advect_scalar
SUBROUTINE nl_set_test_relax_bdy_dry ( id_id , test_relax_bdy_dry )
  logical , INTENT(IN) :: test_relax_bdy_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_relax_bdy_dry: test_relax_bdy_dry applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_relax_bdy_dry = test_relax_bdy_dry 
  RETURN
END SUBROUTINE nl_set_test_relax_bdy_dry
SUBROUTINE nl_set_test_rk_addtend_dry ( id_id , test_rk_addtend_dry )
  logical , INTENT(IN) :: test_rk_addtend_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_rk_addtend_dry: test_rk_addtend_dry applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_rk_addtend_dry = test_rk_addtend_dry 
  RETURN
END SUBROUTINE nl_set_test_rk_addtend_dry
SUBROUTINE nl_set_test_spec_bdy_dry ( id_id , test_spec_bdy_dry )
  logical , INTENT(IN) :: test_spec_bdy_dry
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_spec_bdy_dry: test_spec_bdy_dry applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_spec_bdy_dry = test_spec_bdy_dry 
  RETURN
END SUBROUTINE nl_set_test_spec_bdy_dry
SUBROUTINE nl_set_test_small_step_prep ( id_id , test_small_step_prep )
  logical , INTENT(IN) :: test_small_step_prep
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_small_step_prep: test_small_step_prep applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_small_step_prep = test_small_step_prep 
  RETURN
END SUBROUTINE nl_set_test_small_step_prep
SUBROUTINE nl_set_test_calc_p_rho ( id_id , test_calc_p_rho )
  logical , INTENT(IN) :: test_calc_p_rho
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_calc_p_rho: test_calc_p_rho applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_calc_p_rho = test_calc_p_rho 
  RETURN
END SUBROUTINE nl_set_test_calc_p_rho
SUBROUTINE nl_set_test_calc_coef_w ( id_id , test_calc_coef_w )
  logical , INTENT(IN) :: test_calc_coef_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_calc_coef_w: test_calc_coef_w applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_calc_coef_w = test_calc_coef_w 
  RETURN
END SUBROUTINE nl_set_test_calc_coef_w
SUBROUTINE nl_set_test_advance_uv ( id_id , test_advance_uv )
  logical , INTENT(IN) :: test_advance_uv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_advance_uv: test_advance_uv applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_advance_uv = test_advance_uv 
  RETURN
END SUBROUTINE nl_set_test_advance_uv
SUBROUTINE nl_set_test_spec_bdyupdate ( id_id , test_spec_bdyupdate )
  logical , INTENT(IN) :: test_spec_bdyupdate
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_spec_bdyupdate: test_spec_bdyupdate applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_spec_bdyupdate = test_spec_bdyupdate 
  RETURN
END SUBROUTINE nl_set_test_spec_bdyupdate
SUBROUTINE nl_set_test_advance_mu_t ( id_id , test_advance_mu_t )
  logical , INTENT(IN) :: test_advance_mu_t
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_advance_mu_t: test_advance_mu_t applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_advance_mu_t = test_advance_mu_t 
  RETURN
END SUBROUTINE nl_set_test_advance_mu_t
SUBROUTINE nl_set_test_sumflux ( id_id , test_sumflux )
  logical , INTENT(IN) :: test_sumflux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_sumflux: test_sumflux applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_sumflux = test_sumflux 
  RETURN
END SUBROUTINE nl_set_test_sumflux
SUBROUTINE nl_set_test_advance_w ( id_id , test_advance_w )
  logical , INTENT(IN) :: test_advance_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_advance_w: test_advance_w applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_advance_w = test_advance_w 
  RETURN
END SUBROUTINE nl_set_test_advance_w
SUBROUTINE nl_set_test_spec_bdyupdate_ph ( id_id , test_spec_bdyupdate_ph )
  logical , INTENT(IN) :: test_spec_bdyupdate_ph
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_spec_bdyupdate_ph: test_spec_bdyupdate_ph applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_spec_bdyupdate_ph = test_spec_bdyupdate_ph 
  RETURN
END SUBROUTINE nl_set_test_spec_bdyupdate_ph
SUBROUTINE nl_set_test_calc_mu_uv_1 ( id_id , test_calc_mu_uv_1 )
  logical , INTENT(IN) :: test_calc_mu_uv_1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_calc_mu_uv_1: test_calc_mu_uv_1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_calc_mu_uv_1 = test_calc_mu_uv_1 
  RETURN
END SUBROUTINE nl_set_test_calc_mu_uv_1
SUBROUTINE nl_set_test_small_step_finish ( id_id , test_small_step_finish )
  logical , INTENT(IN) :: test_small_step_finish
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_small_step_finish: test_small_step_finish applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_small_step_finish = test_small_step_finish 
  RETURN
END SUBROUTINE nl_set_test_small_step_finish
SUBROUTINE nl_set_test_rk_scalar_tend ( id_id , test_rk_scalar_tend )
  logical , INTENT(IN) :: test_rk_scalar_tend
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_rk_scalar_tend: test_rk_scalar_tend applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_rk_scalar_tend = test_rk_scalar_tend 
  RETURN
END SUBROUTINE nl_set_test_rk_scalar_tend
SUBROUTINE nl_set_test_spec_bdy_scalar ( id_id , test_spec_bdy_scalar )
  logical , INTENT(IN) :: test_spec_bdy_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_spec_bdy_scalar: test_spec_bdy_scalar applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_spec_bdy_scalar = test_spec_bdy_scalar 
  RETURN
END SUBROUTINE nl_set_test_spec_bdy_scalar
SUBROUTINE nl_set_test_rk_update_scalar ( id_id , test_rk_update_scalar )
  logical , INTENT(IN) :: test_rk_update_scalar
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_rk_update_scalar: test_rk_update_scalar applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_rk_update_scalar = test_rk_update_scalar 
  RETURN
END SUBROUTINE nl_set_test_rk_update_scalar
SUBROUTINE nl_set_test_calc_p_rho_phi ( id_id , test_calc_p_rho_phi )
  logical , INTENT(IN) :: test_calc_p_rho_phi
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_calc_p_rho_phi: test_calc_p_rho_phi applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_calc_p_rho_phi = test_calc_p_rho_phi 
  RETURN
END SUBROUTINE nl_set_test_calc_p_rho_phi
SUBROUTINE nl_set_test_diagnose_w ( id_id , test_diagnose_w )
  logical , INTENT(IN) :: test_diagnose_w
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_test_diagnose_w: test_diagnose_w applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%test_diagnose_w = test_diagnose_w 
  RETURN
END SUBROUTINE nl_set_test_diagnose_w
SUBROUTINE nl_set_mp_physics ( id_id , mp_physics )
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_mp_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12265 , emess)
  ENDIF
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE nl_set_mp_physics
SUBROUTINE nl_set_ra_lw_physics ( id_id , ra_lw_physics )
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ra_lw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12276 , emess)
  ENDIF
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE nl_set_ra_lw_physics
SUBROUTINE nl_set_ra_sw_physics ( id_id , ra_sw_physics )
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ra_sw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12287 , emess)
  ENDIF
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE nl_set_ra_sw_physics
SUBROUTINE nl_set_radt ( id_id , radt )
  real , INTENT(IN) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_radt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12298 , emess)
  ENDIF
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE nl_set_radt
SUBROUTINE nl_set_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  integer , INTENT(IN) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_sf_sfclay_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12309 , emess)
  ENDIF
  model_config_rec%sf_sfclay_physics(id_id) = sf_sfclay_physics
  RETURN
END SUBROUTINE nl_set_sf_sfclay_physics
SUBROUTINE nl_set_sf_surface_physics ( id_id , sf_surface_physics )
  integer , INTENT(IN) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_sf_surface_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12320 , emess)
  ENDIF
  model_config_rec%sf_surface_physics(id_id) = sf_surface_physics
  RETURN
END SUBROUTINE nl_set_sf_surface_physics
SUBROUTINE nl_set_bl_pbl_physics ( id_id , bl_pbl_physics )
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bl_pbl_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12331 , emess)
  ENDIF
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics
SUBROUTINE nl_set_bldt ( id_id , bldt )
  real , INTENT(IN) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bldt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12342 , emess)
  ENDIF
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE nl_set_bldt
SUBROUTINE nl_set_cu_physics ( id_id , cu_physics )
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cu_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12353 , emess)
  ENDIF
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE nl_set_cu_physics
SUBROUTINE nl_set_cudt ( id_id , cudt )
  real , INTENT(IN) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cudt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12364 , emess)
  ENDIF
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gsmdt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12375 , emess)
  ENDIF
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_isfflx: isfflx applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_ifsnow: ifsnow applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_icloud: icloud applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_surface_input_source: surface_input_source applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%surface_input_source = surface_input_source 
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_num_soil_layers: num_soil_layers applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_maxiens: maxiens applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxiens = maxiens 
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_maxens: maxens applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens = maxens 
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_maxens2: maxens2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens2 = maxens2 
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_maxens3: maxens3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens3 = maxens3 
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_ensdim: ensdim applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_chem_opt ( id_id , chem_opt )
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_chem_opt: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12486 , emess)
  ENDIF
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE nl_set_chem_opt
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_num_land_cat: num_land_cat applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_num_soil_cat: num_soil_cat applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_mp_zero_out: mp_zero_out applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%mp_zero_out = mp_zero_out 
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_mp_zero_out_thresh: mp_zero_out_thresh applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh 
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_seaice_threshold: seaice_threshold applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%seaice_threshold = seaice_threshold 
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_dyn_opt ( id_id , dyn_opt )
  integer , INTENT(IN) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_dyn_opt: dyn_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE nl_set_dyn_opt
SUBROUTINE nl_set_rk_ord ( id_id , rk_ord )
  integer , INTENT(IN) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_rk_ord: rk_ord applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE nl_set_rk_ord
SUBROUTINE nl_set_w_damping ( id_id , w_damping )
  integer , INTENT(IN) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_w_damping: w_damping applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%w_damping = w_damping 
  RETURN
END SUBROUTINE nl_set_w_damping
SUBROUTINE nl_set_diff_opt ( id_id , diff_opt )
  integer , INTENT(IN) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_diff_opt: diff_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%diff_opt = diff_opt 
  RETURN
END SUBROUTINE nl_set_diff_opt
SUBROUTINE nl_set_km_opt ( id_id , km_opt )
  integer , INTENT(IN) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_km_opt: km_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%km_opt = km_opt 
  RETURN
END SUBROUTINE nl_set_km_opt
SUBROUTINE nl_set_damp_opt ( id_id , damp_opt )
  integer , INTENT(IN) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_damp_opt: damp_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE nl_set_damp_opt
SUBROUTINE nl_set_zdamp ( id_id , zdamp )
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_zdamp: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12607 , emess)
  ENDIF
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE nl_set_zdamp
SUBROUTINE nl_set_dampcoef ( id_id , dampcoef )
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dampcoef: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12618 , emess)
  ENDIF
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE nl_set_dampcoef
SUBROUTINE nl_set_khdif ( id_id , khdif )
  real , INTENT(IN) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_khdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12629 , emess)
  ENDIF
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE nl_set_khdif
SUBROUTINE nl_set_kvdif ( id_id , kvdif )
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kvdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12640 , emess)
  ENDIF
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE nl_set_kvdif
SUBROUTINE nl_set_smdiv ( id_id , smdiv )
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_smdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12651 , emess)
  ENDIF
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE nl_set_smdiv
SUBROUTINE nl_set_emdiv ( id_id , emdiv )
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_emdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12662 , emess)
  ENDIF
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE nl_set_emdiv
SUBROUTINE nl_set_epssm ( id_id , epssm )
  real , INTENT(IN) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_epssm: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12673 , emess)
  ENDIF
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE nl_set_epssm
SUBROUTINE nl_set_non_hydrostatic ( id_id , non_hydrostatic )
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_non_hydrostatic: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12684 , emess)
  ENDIF
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE nl_set_non_hydrostatic
SUBROUTINE nl_set_time_step_sound ( id_id , time_step_sound )
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_time_step_sound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12695 , emess)
  ENDIF
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE nl_set_time_step_sound
SUBROUTINE nl_set_h_mom_adv_order ( id_id , h_mom_adv_order )
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_h_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12706 , emess)
  ENDIF
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE nl_set_h_mom_adv_order
SUBROUTINE nl_set_v_mom_adv_order ( id_id , v_mom_adv_order )
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_v_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12717 , emess)
  ENDIF
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE nl_set_v_mom_adv_order
SUBROUTINE nl_set_h_sca_adv_order ( id_id , h_sca_adv_order )
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_h_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12728 , emess)
  ENDIF
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE nl_set_h_sca_adv_order
SUBROUTINE nl_set_v_sca_adv_order ( id_id , v_sca_adv_order )
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_v_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12739 , emess)
  ENDIF
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE nl_set_v_sca_adv_order
SUBROUTINE nl_set_top_radiation ( id_id , top_radiation )
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_top_radiation: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12750 , emess)
  ENDIF
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE nl_set_top_radiation
SUBROUTINE nl_set_mix_cr_len ( id_id , mix_cr_len )
  real , INTENT(IN) :: mix_cr_len
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_mix_cr_len: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12761 , emess)
  ENDIF
  model_config_rec%mix_cr_len(id_id) = mix_cr_len
  RETURN
END SUBROUTINE nl_set_mix_cr_len
SUBROUTINE nl_set_tke_upper_bound ( id_id , tke_upper_bound )
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12772 , emess)
  ENDIF
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE nl_set_tke_upper_bound
SUBROUTINE nl_set_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  real , INTENT(IN) :: kh_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kh_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12783 , emess)
  ENDIF
  model_config_rec%kh_tke_upper_bound(id_id) = kh_tke_upper_bound
  RETURN
END SUBROUTINE nl_set_kh_tke_upper_bound
SUBROUTINE nl_set_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  real , INTENT(IN) :: kv_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kv_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12794 , emess)
  ENDIF
  model_config_rec%kv_tke_upper_bound(id_id) = kv_tke_upper_bound
  RETURN
END SUBROUTINE nl_set_kv_tke_upper_bound
SUBROUTINE nl_set_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  real , INTENT(IN) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_drag_coefficient: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12805 , emess)
  ENDIF
  model_config_rec%tke_drag_coefficient(id_id) = tke_drag_coefficient
  RETURN
END SUBROUTINE nl_set_tke_drag_coefficient
SUBROUTINE nl_set_tke_heat_flux ( id_id , tke_heat_flux )
  real , INTENT(IN) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_heat_flux: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12816 , emess)
  ENDIF
  model_config_rec%tke_heat_flux(id_id) = tke_heat_flux
  RETURN
END SUBROUTINE nl_set_tke_heat_flux
SUBROUTINE nl_set_pert_coriolis ( id_id , pert_coriolis )
  logical , INTENT(IN) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_pert_coriolis: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12827 , emess)
  ENDIF
  model_config_rec%pert_coriolis(id_id) = pert_coriolis
  RETURN
END SUBROUTINE nl_set_pert_coriolis
SUBROUTINE nl_set_mix_full_fields ( id_id , mix_full_fields )
  logical , INTENT(IN) :: mix_full_fields
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_mix_full_fields: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12838 , emess)
  ENDIF
  model_config_rec%mix_full_fields(id_id) = mix_full_fields
  RETURN
END SUBROUTINE nl_set_mix_full_fields
SUBROUTINE nl_set_base_pres ( id_id , base_pres )
  real , INTENT(IN) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_base_pres: base_pres applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_pres = base_pres 
  RETURN
END SUBROUTINE nl_set_base_pres
SUBROUTINE nl_set_base_temp ( id_id , base_temp )
  real , INTENT(IN) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_base_temp: base_temp applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_temp = base_temp 
  RETURN
END SUBROUTINE nl_set_base_temp
SUBROUTINE nl_set_base_lapse ( id_id , base_lapse )
  real , INTENT(IN) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_base_lapse: base_lapse applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_lapse = base_lapse 
  RETURN
END SUBROUTINE nl_set_base_lapse
SUBROUTINE nl_set_spec_bdy_width ( id_id , spec_bdy_width )
  integer , INTENT(IN) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_spec_bdy_width: spec_bdy_width applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE nl_set_spec_bdy_width
SUBROUTINE nl_set_spec_zone ( id_id , spec_zone )
  integer , INTENT(IN) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_spec_zone: spec_zone applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE nl_set_spec_zone
SUBROUTINE nl_set_relax_zone ( id_id , relax_zone )
  integer , INTENT(IN) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_relax_zone: relax_zone applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE nl_set_relax_zone
SUBROUTINE nl_set_specified ( id_id , specified )
  logical , INTENT(IN) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_specified: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12909 , emess)
  ENDIF
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE nl_set_specified
SUBROUTINE nl_set_periodic_x ( id_id , periodic_x )
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_periodic_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12920 , emess)
  ENDIF
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE nl_set_periodic_x
SUBROUTINE nl_set_symmetric_xs ( id_id , symmetric_xs )
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12931 , emess)
  ENDIF
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE nl_set_symmetric_xs
SUBROUTINE nl_set_symmetric_xe ( id_id , symmetric_xe )
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12942 , emess)
  ENDIF
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE nl_set_symmetric_xe
SUBROUTINE nl_set_open_xs ( id_id , open_xs )
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12953 , emess)
  ENDIF
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE nl_set_open_xs
SUBROUTINE nl_set_open_xe ( id_id , open_xe )
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12964 , emess)
  ENDIF
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE nl_set_open_xe
SUBROUTINE nl_set_periodic_y ( id_id , periodic_y )
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_periodic_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12975 , emess)
  ENDIF
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE nl_set_periodic_y
SUBROUTINE nl_set_symmetric_ys ( id_id , symmetric_ys )
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12986 , emess)
  ENDIF
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE nl_set_symmetric_ys
SUBROUTINE nl_set_symmetric_ye ( id_id , symmetric_ye )
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 12997 , emess)
  ENDIF
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE nl_set_symmetric_ye
SUBROUTINE nl_set_open_ys ( id_id , open_ys )
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13008 , emess)
  ENDIF
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE nl_set_open_ys
SUBROUTINE nl_set_open_ye ( id_id , open_ye )
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13019 , emess)
  ENDIF
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE nl_set_open_ye
SUBROUTINE nl_set_nested ( id_id , nested )
  logical , INTENT(IN) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nested: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13030 , emess)
  ENDIF
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE nl_set_nested
SUBROUTINE nl_set_real_data_init_type ( id_id , real_data_init_type )
  integer , INTENT(IN) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,'WARNING in nl_set_real_data_init_type: real_data_init_type applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE nl_set_real_data_init_type
SUBROUTINE nl_set_cen_lat ( id_id , cen_lat )
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13051 , emess)
  ENDIF
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE nl_set_cen_lat
SUBROUTINE nl_set_cen_lon ( id_id , cen_lon )
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cen_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13062 , emess)
  ENDIF
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE nl_set_cen_lon
SUBROUTINE nl_set_truelat1 ( id_id , truelat1 )
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_truelat1: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13073 , emess)
  ENDIF
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE nl_set_truelat1
SUBROUTINE nl_set_truelat2 ( id_id , truelat2 )
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_truelat2: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13084 , emess)
  ENDIF
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE nl_set_truelat2
SUBROUTINE nl_set_moad_cen_lat ( id_id , moad_cen_lat )
  real , INTENT(IN) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13095 , emess)
  ENDIF
  model_config_rec%moad_cen_lat(id_id) = moad_cen_lat
  RETURN
END SUBROUTINE nl_set_moad_cen_lat
SUBROUTINE nl_set_stand_lon ( id_id , stand_lon )
  real , INTENT(IN) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_stand_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13106 , emess)
  ENDIF
  model_config_rec%stand_lon(id_id) = stand_lon
  RETURN
END SUBROUTINE nl_set_stand_lon
SUBROUTINE nl_set_bdyfrq ( id_id , bdyfrq )
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bdyfrq: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13117 , emess)
  ENDIF
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE nl_set_bdyfrq
SUBROUTINE nl_set_iswater ( id_id , iswater )
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_iswater: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13128 , emess)
  ENDIF
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE nl_set_iswater
SUBROUTINE nl_set_isice ( id_id , isice )
  integer , INTENT(IN) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isice: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13139 , emess)
  ENDIF
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE nl_set_isice
SUBROUTINE nl_set_isurban ( id_id , isurban )
  integer , INTENT(IN) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isurban: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13150 , emess)
  ENDIF
  model_config_rec%isurban(id_id) = isurban
  RETURN
END SUBROUTINE nl_set_isurban
SUBROUTINE nl_set_isoilwater ( id_id , isoilwater )
  integer , INTENT(IN) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isoilwater: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13161 , emess)
  ENDIF
  model_config_rec%isoilwater(id_id) = isoilwater
  RETURN
END SUBROUTINE nl_set_isoilwater
SUBROUTINE nl_set_map_proj ( id_id , map_proj )
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_map_proj: Out of range domain number: ',id_id
    CALL wrf_error_fatal3 ( "module_configure.b" , 13172 , emess)
  ENDIF
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE nl_set_map_proj
!ENDOFREGISTRYGENERATEDINCLUDE

END MODULE module_configure



! Special (outside registry)
SUBROUTINE nl_get_mminlu ( idum , retval )
  USE module_configure
  CHARACTER(LEN=4)  :: retval
  INTEGER idum
  retval(1:4) = mminlu(1:4)   ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE nl_get_mminlu
SUBROUTINE nl_set_mminlu ( idum, inval )
  USE module_configure
  CHARACTER(LEN=4) :: inval
  INTEGER idum
  mminlu(1:4) = inval(1:4)    ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE nl_set_mminlu


SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: idomain
  INTEGER               :: dummy1
  INTEGER               :: dummy2

!<DESCRIPTION>
!This routine is called to adjust the integer variables that are defined
!in frame/module_state_description.F (Registry-generated) and that serve
!as indices into 4D tracer arrays for moisture, chemistry, etc.
!Different domains (different grid data structures) are allowed to have
!different sets of tracers so these indices can vary from domain to
!domain. However, since the indices are defined globally in
!module_state_description (a shortcoming in the current software), it is
!necessary that these indices be reset each time a different grid is to
!be computed on.
!
!The scalar idices are set according to the particular physics
!packages -- more specifically in the case of the moisture tracers, microphysics
!packages -- that are stored for each domain in model_config_rec and
!indexed by the grid id, passed in as an argument to this routine.  (The
!initial_config() routine in module_configure is what reads the
!namelist.input file and sets model_config_rec.)
!
!The actual code for calculating the scalar indices on a particular
!domain is generated from the Registry state array definitions for the
!4d tracers and from the package definitions that indicate which physics
!packages use which tracers.
!
!</DESCRIPTION>


!STARTOFREGISTRYGENERATEDINCLUDE inc/scalar_indices.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  P_qv = 1 ; F_qv = .FALSE. 
  P_qc = 1 ; F_qc = .FALSE. 
  P_qr = 1 ; F_qr = .FALSE. 
  P_qi = 1 ; F_qi = .FALSE. 
  P_qs = 1 ; F_qs = .FALSE. 
  P_qg = 1 ; F_qg = .FALSE. 
  IF (model_config_rec%dyn_opt==0)THEN
  END IF
  IF (model_config_rec%dyn_opt==2)THEN
  END IF
  IF (model_config_rec%dyn_opt==102)THEN
  END IF
  IF (model_config_rec%dyn_opt==202)THEN
  END IF
  IF (model_config_rec%dyn_opt==302)THEN
  END IF
  IF (model_config_rec%dyn_opt==402)THEN
  END IF
  IF (model_config_rec%dyn_opt==502)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   F_qi = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==98)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==99)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%io_form_restart==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==2)THEN
  END IF
  IF (model_config_rec%io_form_restart==3)THEN
  END IF
  IF (model_config_rec%io_form_restart==4)THEN
  END IF
  IF (model_config_rec%io_form_restart==5)THEN
  END IF
  IF (model_config_rec%io_form_restart==6)THEN
  END IF
  IF (model_config_rec%io_form_restart==7)THEN
  END IF
!ENDOFREGISTRYGENERATEDINCLUDE

!STARTOFREGISTRYGENERATEDINCLUDE inc/scalar_indices_init.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  num_moist = moist_num_table( idomain )
  num_chem = chem_num_table( idomain )
!ENDOFREGISTRYGENERATEDINCLUDE
  RETURN
END SUBROUTINE set_scalar_indices_from_config
