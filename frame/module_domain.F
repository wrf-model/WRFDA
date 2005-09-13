!WRF:DRIVER_LAYER:DOMAIN_OBJECT
!
!  Following are the routines contained within this MODULE:

!  alloc_and_configure_domain        1. Allocate the space for a single domain (constants
!                                       and null terminate pointers).
!                                    2. Connect the domains as a linked list.
!                                    3. Store all of the domain constants.
!                                    4. CALL alloc_space_field.

!  alloc_space_field                 1. Allocate space for the gridded data required for
!                                       each domain.

!  dealloc_space_domain              1. Reconnect linked list nodes since the current
!                                       node is removed.
!                                    2. CALL dealloc_space_field.
!                                    3. Deallocate single domain.

!  dealloc_space_field               1. Deallocate each of the fields for a particular
!                                       domain.

!  first_loc_integer                 1. Find the first incidence of a particular
!                                       domain identifier from an array of domain
!                                       identifiers.

MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_state_description
   USE module_configure
   USE module_wrf_error
   USE WRF_ESMF_MOD

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the "next"
   !  domain that is to be stored, the other is the next domain which happens to
   !  also be on the "same_level".

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   INTEGER, PARAMETER :: HISTORY_ALARM=1, AUXHIST1_ALARM=2, AUXHIST2_ALARM=3,     &
                         AUXHIST3_ALARM=4, AUXHIST4_ALARM=5, AUXHIST5_ALARM=6,    &
                         AUXINPUT1_ALARM=7, AUXINPUT2_ALARM=8, AUXINPUT3_ALARM=9, &
                         AUXINPUT4_ALARM=10, AUXINPUT5_ALARM=11,                  &
                         RESTART_ALARM=12, BOUNDARY_ALARM=13, INPUTOUT_ALARM=14,  &  ! for outputing input (e.g. for 3dvar)
                         ALARM_SUBTIME=15,                                        &
                         MAX_WRF_ALARMS=20  ! ESMF 2.0.1+ requires user to dimension alarm 
                                            ! lists.  MAX_WRF_ALARMS must be big enough 
                                            ! to include all of the above alarms.  


!STARTOFREGISTRYGENERATEDINCLUDE inc/state_subtypes.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE

!STARTOFREGISTRYGENERATEDINCLUDE inc/state_struct.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real                                     :: cfn
real                                     :: cfn1
real                                     :: epsts
integer                                  :: step_number
real                                     :: rdx
real                                     :: rdy
real                                     :: dts
real                                     :: dtseps
real                                     :: resm
real                                     :: zetatop
real                                     :: cf1
real                                     :: cf2
real                                     :: cf3
integer                                  :: number_at_same_level
integer                                  :: itimestep
real                                     :: avg_abs_dpsdt
real                                     :: avg_abs_dmudt
integer                                  :: oid
integer                                  :: auxhist1_oid
integer                                  :: auxhist2_oid
integer                                  :: auxhist3_oid
integer                                  :: auxhist4_oid
integer                                  :: auxhist5_oid
integer                                  :: auxinput1_oid
integer                                  :: auxinput2_oid
integer                                  :: auxinput3_oid
integer                                  :: auxinput4_oid
integer                                  :: auxinput5_oid
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
real                                     :: dtbc
real                                     :: g_dtbc
real                                     :: a_dtbc
integer                                  :: ifndsnowh
integer                                  :: ifndsoilw
real                                     :: u_frame
real                                     :: v_frame
real                                     :: p_top
integer                                  :: imicrogram
real                                     :: em_lat_ll_t
real                                     :: em_lat_ul_t
real                                     :: em_lat_ur_t
real                                     :: em_lat_lr_t
real                                     :: em_lat_ll_u
real                                     :: em_lat_ul_u
real                                     :: em_lat_ur_u
real                                     :: em_lat_lr_u
real                                     :: em_lat_ll_v
real                                     :: em_lat_ul_v
real                                     :: em_lat_ur_v
real                                     :: em_lat_lr_v
real                                     :: em_lat_ll_d
real                                     :: em_lat_ul_d
real                                     :: em_lat_ur_d
real                                     :: em_lat_lr_d
real                                     :: em_lon_ll_t
real                                     :: em_lon_ul_t
real                                     :: em_lon_ur_t
real                                     :: em_lon_lr_t
real                                     :: em_lon_ll_u
real                                     :: em_lon_ul_u
real                                     :: em_lon_ur_u
real                                     :: em_lon_lr_u
real                                     :: em_lon_ll_v
real                                     :: em_lon_ul_v
real                                     :: em_lon_ur_v
real                                     :: em_lon_lr_v
real                                     :: em_lon_ll_d
real                                     :: em_lon_ul_d
real                                     :: em_lon_ur_d
real                                     :: em_lon_lr_d
integer                                  :: stepcu
integer                                  :: stepra
integer                                  :: stepbl
logical                                  :: warm_rain
logical                                  :: moved
integer                                  :: run_days
integer                                  :: run_hours
integer                                  :: run_minutes
integer                                  :: run_seconds
integer                                  :: start_year
integer                                  :: start_month
integer                                  :: start_day
integer                                  :: start_hour
integer                                  :: start_minute
integer                                  :: start_second
integer                                  :: end_year
integer                                  :: end_month
integer                                  :: end_day
integer                                  :: end_hour
integer                                  :: end_minute
integer                                  :: end_second
integer                                  :: interval_seconds
logical                                  :: input_from_file
logical                                  :: input_from_hires
integer                                  :: history_interval
integer                                  :: frames_per_outfile
integer                                  :: frames_per_auxhist1
integer                                  :: frames_per_auxhist2
integer                                  :: frames_per_auxhist3
integer                                  :: frames_per_auxhist4
integer                                  :: frames_per_auxhist5
logical                                  :: restart
integer                                  :: restart_interval
integer                                  :: io_form_input
integer                                  :: io_form_history
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: debug_level
character*256                               :: history_outname
character*256                               :: auxhist1_outname
character*256                               :: auxhist2_outname
character*256                               :: auxhist3_outname
character*256                               :: auxhist4_outname
character*256                               :: auxhist5_outname
character*256                               :: history_inname
character*256                               :: auxhist1_inname
character*256                               :: auxhist2_inname
character*256                               :: auxhist3_inname
character*256                               :: auxhist4_inname
character*256                               :: auxhist5_inname
character*256                               :: auxinput1_outname
character*256                               :: auxinput2_outname
character*256                               :: auxinput3_outname
character*256                               :: auxinput4_outname
character*256                               :: auxinput5_outname
character*256                               :: auxinput1_inname
character*256                               :: auxinput2_inname
character*256                               :: auxinput3_inname
character*256                               :: auxinput4_inname
character*256                               :: auxinput5_inname
integer                                  :: history_interval_mo
integer                                  :: history_interval_d
integer                                  :: history_interval_h
integer                                  :: history_interval_m
integer                                  :: history_interval_s
integer                                  :: inputout_interval_mo
integer                                  :: inputout_interval_d
integer                                  :: inputout_interval_h
integer                                  :: inputout_interval_m
integer                                  :: inputout_interval_s
integer                                  :: inputout_interval
integer                                  :: auxhist1_interval_mo
integer                                  :: auxhist1_interval_d
integer                                  :: auxhist1_interval_h
integer                                  :: auxhist1_interval_m
integer                                  :: auxhist1_interval_s
integer                                  :: auxhist1_interval
integer                                  :: auxhist2_interval_mo
integer                                  :: auxhist2_interval_d
integer                                  :: auxhist2_interval_h
integer                                  :: auxhist2_interval_m
integer                                  :: auxhist2_interval_s
integer                                  :: auxhist2_interval
integer                                  :: auxhist3_interval_mo
integer                                  :: auxhist3_interval_d
integer                                  :: auxhist3_interval_h
integer                                  :: auxhist3_interval_m
integer                                  :: auxhist3_interval_s
integer                                  :: auxhist3_interval
integer                                  :: auxhist4_interval_mo
integer                                  :: auxhist4_interval_d
integer                                  :: auxhist4_interval_h
integer                                  :: auxhist4_interval_m
integer                                  :: auxhist4_interval_s
integer                                  :: auxhist4_interval
integer                                  :: auxhist5_interval_mo
integer                                  :: auxhist5_interval_d
integer                                  :: auxhist5_interval_h
integer                                  :: auxhist5_interval_m
integer                                  :: auxhist5_interval_s
integer                                  :: auxhist5_interval
integer                                  :: auxinput1_interval_mo
integer                                  :: auxinput1_interval_d
integer                                  :: auxinput1_interval_h
integer                                  :: auxinput1_interval_m
integer                                  :: auxinput1_interval_s
integer                                  :: auxinput1_interval
integer                                  :: auxinput2_interval_mo
integer                                  :: auxinput2_interval_d
integer                                  :: auxinput2_interval_h
integer                                  :: auxinput2_interval_m
integer                                  :: auxinput2_interval_s
integer                                  :: auxinput2_interval
integer                                  :: auxinput3_interval_mo
integer                                  :: auxinput3_interval_d
integer                                  :: auxinput3_interval_h
integer                                  :: auxinput3_interval_m
integer                                  :: auxinput3_interval_s
integer                                  :: auxinput3_interval
integer                                  :: auxinput4_interval_mo
integer                                  :: auxinput4_interval_d
integer                                  :: auxinput4_interval_h
integer                                  :: auxinput4_interval_m
integer                                  :: auxinput4_interval_s
integer                                  :: auxinput4_interval
integer                                  :: auxinput5_interval_mo
integer                                  :: auxinput5_interval_d
integer                                  :: auxinput5_interval_h
integer                                  :: auxinput5_interval_m
integer                                  :: auxinput5_interval_s
integer                                  :: auxinput5_interval
integer                                  :: restart_interval_mo
integer                                  :: restart_interval_d
integer                                  :: restart_interval_h
integer                                  :: restart_interval_m
integer                                  :: restart_interval_s
integer                                  :: history_begin_y
integer                                  :: history_begin_mo
integer                                  :: history_begin_d
integer                                  :: history_begin_h
integer                                  :: history_begin_m
integer                                  :: history_begin_s
integer                                  :: inputout_begin_y
integer                                  :: inputout_begin_mo
integer                                  :: inputout_begin_d
integer                                  :: inputout_begin_h
integer                                  :: inputout_begin_m
integer                                  :: inputout_begin_s
integer                                  :: auxhist1_begin_y
integer                                  :: auxhist1_begin_mo
integer                                  :: auxhist1_begin_d
integer                                  :: auxhist1_begin_h
integer                                  :: auxhist1_begin_m
integer                                  :: auxhist1_begin_s
integer                                  :: auxhist2_begin_y
integer                                  :: auxhist2_begin_mo
integer                                  :: auxhist2_begin_d
integer                                  :: auxhist2_begin_h
integer                                  :: auxhist2_begin_m
integer                                  :: auxhist2_begin_s
integer                                  :: auxhist3_begin_y
integer                                  :: auxhist3_begin_mo
integer                                  :: auxhist3_begin_d
integer                                  :: auxhist3_begin_h
integer                                  :: auxhist3_begin_m
integer                                  :: auxhist3_begin_s
integer                                  :: auxhist4_begin_y
integer                                  :: auxhist4_begin_mo
integer                                  :: auxhist4_begin_d
integer                                  :: auxhist4_begin_h
integer                                  :: auxhist4_begin_m
integer                                  :: auxhist4_begin_s
integer                                  :: auxhist5_begin_y
integer                                  :: auxhist5_begin_mo
integer                                  :: auxhist5_begin_d
integer                                  :: auxhist5_begin_h
integer                                  :: auxhist5_begin_m
integer                                  :: auxhist5_begin_s
integer                                  :: auxinput1_begin_y
integer                                  :: auxinput1_begin_mo
integer                                  :: auxinput1_begin_d
integer                                  :: auxinput1_begin_h
integer                                  :: auxinput1_begin_m
integer                                  :: auxinput1_begin_s
integer                                  :: auxinput2_begin_y
integer                                  :: auxinput2_begin_mo
integer                                  :: auxinput2_begin_d
integer                                  :: auxinput2_begin_h
integer                                  :: auxinput2_begin_m
integer                                  :: auxinput2_begin_s
integer                                  :: auxinput3_begin_y
integer                                  :: auxinput3_begin_mo
integer                                  :: auxinput3_begin_d
integer                                  :: auxinput3_begin_h
integer                                  :: auxinput3_begin_m
integer                                  :: auxinput3_begin_s
integer                                  :: auxinput4_begin_y
integer                                  :: auxinput4_begin_mo
integer                                  :: auxinput4_begin_d
integer                                  :: auxinput4_begin_h
integer                                  :: auxinput4_begin_m
integer                                  :: auxinput4_begin_s
integer                                  :: auxinput5_begin_y
integer                                  :: auxinput5_begin_mo
integer                                  :: auxinput5_begin_d
integer                                  :: auxinput5_begin_h
integer                                  :: auxinput5_begin_m
integer                                  :: auxinput5_begin_s
integer                                  :: restart_begin_y
integer                                  :: restart_begin_mo
integer                                  :: restart_begin_d
integer                                  :: restart_begin_h
integer                                  :: restart_begin_m
integer                                  :: restart_begin_s
integer                                  :: history_end_y
integer                                  :: history_end_mo
integer                                  :: history_end_d
integer                                  :: history_end_h
integer                                  :: history_end_m
integer                                  :: history_end_s
integer                                  :: inputout_end_y
integer                                  :: inputout_end_mo
integer                                  :: inputout_end_d
integer                                  :: inputout_end_h
integer                                  :: inputout_end_m
integer                                  :: inputout_end_s
integer                                  :: auxhist1_end_y
integer                                  :: auxhist1_end_mo
integer                                  :: auxhist1_end_d
integer                                  :: auxhist1_end_h
integer                                  :: auxhist1_end_m
integer                                  :: auxhist1_end_s
integer                                  :: auxhist2_end_y
integer                                  :: auxhist2_end_mo
integer                                  :: auxhist2_end_d
integer                                  :: auxhist2_end_h
integer                                  :: auxhist2_end_m
integer                                  :: auxhist2_end_s
integer                                  :: auxhist3_end_y
integer                                  :: auxhist3_end_mo
integer                                  :: auxhist3_end_d
integer                                  :: auxhist3_end_h
integer                                  :: auxhist3_end_m
integer                                  :: auxhist3_end_s
integer                                  :: auxhist4_end_y
integer                                  :: auxhist4_end_mo
integer                                  :: auxhist4_end_d
integer                                  :: auxhist4_end_h
integer                                  :: auxhist4_end_m
integer                                  :: auxhist4_end_s
integer                                  :: auxhist5_end_y
integer                                  :: auxhist5_end_mo
integer                                  :: auxhist5_end_d
integer                                  :: auxhist5_end_h
integer                                  :: auxhist5_end_m
integer                                  :: auxhist5_end_s
integer                                  :: auxinput1_end_y
integer                                  :: auxinput1_end_mo
integer                                  :: auxinput1_end_d
integer                                  :: auxinput1_end_h
integer                                  :: auxinput1_end_m
integer                                  :: auxinput1_end_s
integer                                  :: auxinput2_end_y
integer                                  :: auxinput2_end_mo
integer                                  :: auxinput2_end_d
integer                                  :: auxinput2_end_h
integer                                  :: auxinput2_end_m
integer                                  :: auxinput2_end_s
integer                                  :: auxinput3_end_y
integer                                  :: auxinput3_end_mo
integer                                  :: auxinput3_end_d
integer                                  :: auxinput3_end_h
integer                                  :: auxinput3_end_m
integer                                  :: auxinput3_end_s
integer                                  :: auxinput4_end_y
integer                                  :: auxinput4_end_mo
integer                                  :: auxinput4_end_d
integer                                  :: auxinput4_end_h
integer                                  :: auxinput4_end_m
integer                                  :: auxinput4_end_s
integer                                  :: auxinput5_end_y
integer                                  :: auxinput5_end_mo
integer                                  :: auxinput5_end_d
integer                                  :: auxinput5_end_h
integer                                  :: auxinput5_end_m
integer                                  :: auxinput5_end_s
integer                                  :: io_form_auxinput1
integer                                  :: io_form_auxinput2
integer                                  :: io_form_auxinput3
integer                                  :: io_form_auxinput4
integer                                  :: io_form_auxinput5
integer                                  :: io_form_auxhist1
integer                                  :: io_form_auxhist2
integer                                  :: io_form_auxhist3
integer                                  :: io_form_auxhist4
integer                                  :: io_form_auxhist5
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
character*256                               :: input_inname
character*256                               :: input_outname
character*256                               :: bdy_inname
character*256                               :: bdy_outname
character*256                               :: rst_inname
character*256                               :: rst_outname
logical                                  :: write_input
logical                                  :: write_restart_at_0h
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
integer                                  :: max_dom
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
real                                     :: dx
real                                     :: dy
integer                                  :: grid_id
integer                                  :: parent_id
integer                                  :: level
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: feedback
integer                                  :: smooth_option
real                                     :: ztop
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
integer                                  :: shw
integer                                  :: coral_dist
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: nproc_x
integer                                  :: nproc_y
integer                                  :: irand
real                                     :: dt
integer                                  :: num_moves
integer                                  :: move_id
integer                                  :: move_interval
integer                                  :: move_cd_x
integer                                  :: move_cd_y
logical                                  :: test_rk_step_prep
logical                                  :: test_init_zero_tendency
logical                                  :: test_phy_prep
logical                                  :: test_calculate_km_kh
logical                                  :: test_rk_tendency
logical                                  :: test_advect_scalar
logical                                  :: test_relax_bdy_dry
logical                                  :: test_rk_addtend_dry
logical                                  :: test_spec_bdy_dry
logical                                  :: test_small_step_prep
logical                                  :: test_calc_p_rho
logical                                  :: test_calc_coef_w
logical                                  :: test_advance_uv
logical                                  :: test_spec_bdyupdate
logical                                  :: test_advance_mu_t
logical                                  :: test_sumflux
logical                                  :: test_advance_w
logical                                  :: test_spec_bdyupdate_ph
logical                                  :: test_calc_mu_uv_1
logical                                  :: test_small_step_finish
logical                                  :: test_rk_scalar_tend
logical                                  :: test_spec_bdy_scalar
logical                                  :: test_rk_update_scalar
logical                                  :: test_calc_p_rho_phi
logical                                  :: test_diagnose_w
integer                                  :: mp_physics
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
real                                     :: radt
integer                                  :: sf_sfclay_physics
integer                                  :: sf_surface_physics
integer                                  :: bl_pbl_physics
real                                     :: bldt
integer                                  :: cu_physics
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: isfflx
integer                                  :: ifsnow
integer                                  :: icloud
integer                                  :: surface_input_source
integer                                  :: num_soil_layers
integer                                  :: maxiens
integer                                  :: maxens
integer                                  :: maxens2
integer                                  :: maxens3
integer                                  :: ensdim
integer                                  :: chem_opt
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: mp_zero_out
real                                     :: mp_zero_out_thresh
real                                     :: seaice_threshold
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
real                                     :: zdamp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
logical                                  :: top_radiation
real                                     :: mix_cr_len
real                                     :: tke_upper_bound
real                                     :: kh_tke_upper_bound
real                                     :: kv_tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
logical                                  :: mix_full_fields
real                                     :: base_pres
real                                     :: base_temp
real                                     :: base_lapse
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
logical                                  :: specified
logical                                  :: periodic_x
logical                                  :: symmetric_xs
logical                                  :: symmetric_xe
logical                                  :: open_xs
logical                                  :: open_xe
logical                                  :: periodic_y
logical                                  :: symmetric_ys
logical                                  :: symmetric_ye
logical                                  :: open_ys
logical                                  :: open_ye
logical                                  :: nested
integer                                  :: real_data_init_type
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: moad_cen_lat
real                                     :: stand_lon
real                                     :: bdyfrq
integer                                  :: iswater
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
integer                                  :: nodyn_nodyn_dummy
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ru_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ru_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_u_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_u_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_rv_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_rv_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_v_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_v_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_rw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_rw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_ph_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_ph_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phb_fine
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph0
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_init
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_t_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_t_save
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_2
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_mu_2
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_mu_2
real      ,DIMENSION(:,:)     ,POINTER   :: em_mub
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_mub
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_mub
real      ,DIMENSION(:,:)     ,POINTER   :: em_mub_fine
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu0
real      ,DIMENSION(:,:)     ,POINTER   :: em_mudf
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_mudf
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_mudf
real      ,DIMENSION(:,:)     ,POINTER   :: em_muu
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_muu
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_muu
real      ,DIMENSION(:,:)     ,POINTER   :: em_muv
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_muv
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_muv
real      ,DIMENSION(:,:)     ,POINTER   :: em_mut
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_mut
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_mut
real      ,DIMENSION(:,:)     ,POINTER   :: em_muts
real      ,DIMENSION(:,:)     ,POINTER   :: em_g_muts
real      ,DIMENSION(:,:)     ,POINTER   :: em_a_muts
real      ,DIMENSION(:,:)     ,POINTER   :: nest_pos
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tke_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tke_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_al
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_al
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_al
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_alb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_zx
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_zy
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rdz
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rdzw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_pb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_alpha
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_alpha
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_alpha
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_gamma
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_gamma
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_gamma
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_c2a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_c2a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_c2a
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rho
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_rho
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_rho
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phm
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_phm
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_phm
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_cqu
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_cqu
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_cqu
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_cqv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_cqv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_cqv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_cqw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_cqw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_cqw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_pm1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_pm1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_pm1
real      ,DIMENSION(:)       ,POINTER   :: em_fnm
real      ,DIMENSION(:)       ,POINTER   :: em_fnp
real      ,DIMENSION(:)       ,POINTER   :: em_rdnw
real      ,DIMENSION(:)       ,POINTER   :: em_rdn
real      ,DIMENSION(:)       ,POINTER   :: em_dnw
real      ,DIMENSION(:)       ,POINTER   :: em_dn
real      ,DIMENSION(:)       ,POINTER   :: em_znu
real      ,DIMENSION(:)       ,POINTER   :: em_znw
real      ,DIMENSION(:)       ,POINTER   :: em_t_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_z
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_g_z
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_a_z
real      ,DIMENSION(:,:)     ,POINTER   :: q2
real      ,DIMENSION(:,:)     ,POINTER   :: t2
real      ,DIMENSION(:,:)     ,POINTER   :: th2
real      ,DIMENSION(:,:)     ,POINTER   :: psfc
real      ,DIMENSION(:,:)     ,POINTER   :: u10
real      ,DIMENSION(:,:)     ,POINTER   :: v10
real      ,DIMENSION(:,:)     ,POINTER   :: dpsdt
real      ,DIMENSION(:,:)     ,POINTER   :: dmudt
real      ,DIMENSION(:,:)     ,POINTER   :: psfcm
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_conve_3d
real      ,DIMENSION(:,:)     ,POINTER   :: q_conve_2d
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_nostag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xstag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_ystag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xystag
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_moist_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_moist_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_moist_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_moist_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_u_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_u_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_v_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_v_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_w_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_w_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_w_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_w_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_w_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_w_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_ph_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_ph_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_ph_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_ph_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_t_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_t_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_mu_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_mu_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_mu_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_mu_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_rqv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_rqv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_rqv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_rqv_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_g_rqv_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_a_rqv_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqc_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqc_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqc_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqc_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqc_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqc_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqr_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqr_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqr_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqr_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqr_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqr_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqi_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqi_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqi_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqi_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqi_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqi_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqs_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqs_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqs_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqs_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqs_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqs_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqg_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqg_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqg_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: rqg_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: g_rqg_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: a_rqg_bt
real      ,DIMENSION(:)       ,POINTER   :: fcx
real      ,DIMENSION(:)       ,POINTER   :: g_fcx
real      ,DIMENSION(:)       ,POINTER   :: a_fcx
real      ,DIMENSION(:)       ,POINTER   :: gcx
real      ,DIMENSION(:)       ,POINTER   :: g_gcx
real      ,DIMENSION(:)       ,POINTER   :: a_gcx
real      ,DIMENSION(:,:)     ,POINTER   :: sm000010
real      ,DIMENSION(:,:)     ,POINTER   :: sm010040
real      ,DIMENSION(:,:)     ,POINTER   :: sm040100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100200
real      ,DIMENSION(:,:)     ,POINTER   :: sm010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilm000
real      ,DIMENSION(:,:)     ,POINTER   :: soilm005
real      ,DIMENSION(:,:)     ,POINTER   :: soilm020
real      ,DIMENSION(:,:)     ,POINTER   :: soilm040
real      ,DIMENSION(:,:)     ,POINTER   :: soilm160
real      ,DIMENSION(:,:)     ,POINTER   :: soilm300
real      ,DIMENSION(:,:)     ,POINTER   :: sw000010
real      ,DIMENSION(:,:)     ,POINTER   :: sw010040
real      ,DIMENSION(:,:)     ,POINTER   :: sw040100
real      ,DIMENSION(:,:)     ,POINTER   :: sw100200
real      ,DIMENSION(:,:)     ,POINTER   :: sw010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilw000
real      ,DIMENSION(:,:)     ,POINTER   :: soilw005
real      ,DIMENSION(:,:)     ,POINTER   :: soilw020
real      ,DIMENSION(:,:)     ,POINTER   :: soilw040
real      ,DIMENSION(:,:)     ,POINTER   :: soilw160
real      ,DIMENSION(:,:)     ,POINTER   :: soilw300
real      ,DIMENSION(:,:)     ,POINTER   :: st000010
real      ,DIMENSION(:,:)     ,POINTER   :: st010040
real      ,DIMENSION(:,:)     ,POINTER   :: st040100
real      ,DIMENSION(:,:)     ,POINTER   :: st100200
real      ,DIMENSION(:,:)     ,POINTER   :: st010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilt000
real      ,DIMENSION(:,:)     ,POINTER   :: soilt005
real      ,DIMENSION(:,:)     ,POINTER   :: soilt020
real      ,DIMENSION(:,:)     ,POINTER   :: soilt040
real      ,DIMENSION(:,:)     ,POINTER   :: soilt160
real      ,DIMENSION(:,:)     ,POINTER   :: soilt300
real      ,DIMENSION(:,:)     ,POINTER   :: landmask
real      ,DIMENSION(:,:)     ,POINTER   :: topostdv
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpx
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpy
real      ,DIMENSION(:,:)     ,POINTER   :: shdmax
real      ,DIMENSION(:,:)     ,POINTER   :: shdmin
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:)     ,POINTER   :: slopecat
real      ,DIMENSION(:,:)     ,POINTER   :: toposoil
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb
real      ,DIMENSION(:)       ,POINTER   :: zs
real      ,DIMENSION(:)       ,POINTER   :: dzs
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o
real      ,DIMENSION(:,:)     ,POINTER   :: xice
real      ,DIMENSION(:,:)     ,POINTER   :: smstav
real      ,DIMENSION(:,:)     ,POINTER   :: smstot
real      ,DIMENSION(:,:)     ,POINTER   :: sfcrunoff
real      ,DIMENSION(:,:)     ,POINTER   :: udrunoff
integer   ,DIMENSION(:,:)     ,POINTER   :: ivgtyp
integer   ,DIMENSION(:,:)     ,POINTER   :: isltyp
real      ,DIMENSION(:,:)     ,POINTER   :: vegfra
real      ,DIMENSION(:,:)     ,POINTER   :: sfcevp
real      ,DIMENSION(:,:)     ,POINTER   :: grdflx
real      ,DIMENSION(:,:)     ,POINTER   :: sfcexc
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: snowh
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: keepfr3dflag
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:)     ,POINTER   :: ct
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: z0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
integer   ,DIMENSION(:,:)     ,POINTER   :: kpbl
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:)     ,POINTER   :: totswdn
real      ,DIMENSION(:,:)     ,POINTER   :: totlwdn
real      ,DIMENSION(:,:)     ,POINTER   :: rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: czmean
real      ,DIMENSION(:,:)     ,POINTER   :: cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: cfracm
real      ,DIMENSION(:,:)     ,POINTER   :: cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: acfrst
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrst
real      ,DIMENSION(:,:)     ,POINTER   :: acfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrcv
real      ,DIMENSION(:,:,:)   ,POINTER   :: aer_dry
real      ,DIMENSION(:,:,:)   ,POINTER   :: aer_water
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: h_diabatic
real      ,DIMENSION(:,:)     ,POINTER   :: msft
real      ,DIMENSION(:,:)     ,POINTER   :: msfu
real      ,DIMENSION(:,:)     ,POINTER   :: msfv
real      ,DIMENSION(:,:)     ,POINTER   :: f
real      ,DIMENSION(:,:)     ,POINTER   :: e
real      ,DIMENSION(:,:)     ,POINTER   :: sina
real      ,DIMENSION(:,:)     ,POINTER   :: cosa
real      ,DIMENSION(:,:)     ,POINTER   :: ht
real      ,DIMENSION(:,:)     ,POINTER   :: ht_fine
real      ,DIMENSION(:,:)     ,POINTER   :: ht_int
real      ,DIMENSION(:,:)     ,POINTER   :: ht_input
real      ,DIMENSION(:,:)     ,POINTER   :: tsk
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_save
real      ,DIMENSION(:)       ,POINTER   :: u_base
real      ,DIMENSION(:)       ,POINTER   :: v_base
real      ,DIMENSION(:)       ,POINTER   :: qv_base
real      ,DIMENSION(:)       ,POINTER   :: z_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqrcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqccuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqscuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqicuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: rainc
real      ,DIMENSION(:,:)     ,POINTER   :: rainnc
real      ,DIMENSION(:,:)     ,POINTER   :: raincv
real      ,DIMENSION(:,:)     ,POINTER   :: rainncv
real      ,DIMENSION(:,:)     ,POINTER   :: rainbl
real      ,DIMENSION(:,:)     ,POINTER   :: nca
integer   ,DIMENSION(:,:)     ,POINTER   :: lowlyr
real      ,DIMENSION(:,:)     ,POINTER   :: mass_flux
real      ,DIMENSION(:,:)     ,POINTER   :: apr_gr
real      ,DIMENSION(:,:)     ,POINTER   :: apr_w
real      ,DIMENSION(:,:)     ,POINTER   :: apr_mc
real      ,DIMENSION(:,:)     ,POINTER   :: apr_st
real      ,DIMENSION(:,:)     ,POINTER   :: apr_as
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capma
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capme
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capmi
real      ,DIMENSION(:,:,:)   ,POINTER   :: xf_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: pr_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthraten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratenlw
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratensw
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: swdown
real      ,DIMENSION(:,:)     ,POINTER   :: gsw
real      ,DIMENSION(:,:)     ,POINTER   :: glw
real      ,DIMENSION(:,:)     ,POINTER   :: xlat
real      ,DIMENSION(:,:)     ,POINTER   :: xlong
real      ,DIMENSION(:,:)     ,POINTER   :: albedo
real      ,DIMENSION(:,:)     ,POINTER   :: albbck
real      ,DIMENSION(:,:)     ,POINTER   :: emiss
real      ,DIMENSION(:,:)     ,POINTER   :: cldefi
real      ,DIMENSION(:,:,:)   ,POINTER   :: rublten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqiblten
real      ,DIMENSION(:,:)     ,POINTER   :: tmn
real      ,DIMENSION(:,:)     ,POINTER   :: xland
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: ust
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: pblh
real      ,DIMENSION(:,:)     ,POINTER   :: capg
real      ,DIMENSION(:,:)     ,POINTER   :: thc
real      ,DIMENSION(:,:)     ,POINTER   :: hfx
real      ,DIMENSION(:,:)     ,POINTER   :: qfx
real      ,DIMENSION(:,:)     ,POINTER   :: lh
real      ,DIMENSION(:,:)     ,POINTER   :: flhc
real      ,DIMENSION(:,:)     ,POINTER   :: flqc
real      ,DIMENSION(:,:)     ,POINTER   :: qsg
real      ,DIMENSION(:,:)     ,POINTER   :: qvg
real      ,DIMENSION(:,:)     ,POINTER   :: qcg
real      ,DIMENSION(:,:)     ,POINTER   :: soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: snowc
real      ,DIMENSION(:,:)     ,POINTER   :: mavail
real      ,DIMENSION(:,:)     ,POINTER   :: tkesfcf
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldi
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldc
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor11
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor22
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor12
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor33
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor13
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor23
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmh
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: g_xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhh
real      ,DIMENSION(:,:,:)   ,POINTER   :: div
real      ,DIMENSION(:,:,:)   ,POINTER   :: bn2
!ENDOFREGISTRYGENERATEDINCLUDE

      INTEGER                                             :: comms( max_comms ), shift_x, shift_y

      INTEGER                                             :: id
      INTEGER                                             :: domdesc
      INTEGER                                             :: communicator
      INTEGER                                             :: iocommunicator
      INTEGER,POINTER                                     :: mapping(:,:)
      INTEGER,POINTER                                     :: i_start(:),i_end(:)
      INTEGER,POINTER                                     :: j_start(:),j_end(:)
      INTEGER                                             :: max_tiles
      INTEGER                                             :: num_tiles        ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_x      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_y      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_spec   ! place to store number of tiles computed from 
                                                                              ! externally specified params

      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents                            
      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests                            
      TYPE(domain) , POINTER                              :: sibling ! overlapped domains at same lev
      TYPE(domain) , POINTER                              :: intermediate_grid
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active

      INTEGER      , DIMENSION(0:5)                       :: nframes          ! frames per outfile for history 
                                                                              ! streams (0 is main history)                  

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor

      LOGICAL                                             :: first_force


      ! domain dimensions

      INTEGER    :: sd31,   ed31,   sd32,   ed32,   sd33,   ed33,         &
                    sd21,   ed21,   sd22,   ed22,                         &
                    sd11,   ed11

      INTEGER    :: sp31,   ep31,   sp32,   ep32,   sp33,   ep33,         &
                    sp21,   ep21,   sp22,   ep22,                         &
                    sp11,   ep11,                                         &
                    sm31,   em31,   sm32,   em32,   sm33,   em33,         &
                    sm21,   em21,   sm22,   em22,                         &
                    sm11,   em11,                                         &
                    sp31x,  ep31x,  sp32x,  ep32x,  sp33x,  ep33x,        &
                    sp21x,  ep21x,  sp22x,  ep22x,                        &
                    sm31x,  em31x,  sm32x,  em32x,  sm33x,  em33x,        &
                    sm21x,  em21x,  sm22x,  em22x,                        &
                    sp31y,  ep31y,  sp32y,  ep32y,  sp33y,  ep33y,        &
                    sp21y,  ep21y,  sp22y,  ep22y,                        &
                    sm31y,  em31y,  sm32y,  em32y,  sm33y,  em33y,        &
                    sm21y,  em21y,  sm22y,  em22y
      Type(ESMF_Clock)                                    :: domain_clock
      Type(ESMF_Time)                                     :: start_time, stop_time, current_time
      Type(ESMF_Time)                                     :: start_subtime, stop_subtime
      Type(ESMF_Time)                                     :: this_bdy_time, next_bdy_time
      Type(ESMF_Time)                                     :: this_emi_time, next_emi_time
      Type(ESMF_TimeInterval) :: step_time
      Type(ESMF_Alarm), pointer :: alarms(:)

   END TYPE domain

   !  Now that a "domain" TYPE exists, we can use it to store a few pointers
   !  to this type.  These are primarily for use in traversing the linked list.
   !  The "head_grid" is always the pointer to the first domain that is
   !  allocated.  This is available and is not to be changed.  The others are
   !  just temporary pointers.

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   !  To facilitate an easy integration of each of the domains that are on the
   !  same level, we have an array for the head pointer for each level.  This
   !  removed the need to search through the linked list at each time step to
   !  find which domains are to be active.

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level
   
CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx

    END SELECT data_ordering

    CALL dealloc_space_field ( grid )

    CALL alloc_space_field ( grid, grid%id , 1 ,          &
                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                             grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                             grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                             grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )

    RETURN

   END SUBROUTINE adjust_domain_dims_for_move


   SUBROUTINE get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe    )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_YXZ )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_ZXY )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_ZYX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_XZY )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp32 
           kpe             = grid%ep32 

       CASE  ( DATA_ORDER_YZX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp32 
           kpe             = grid%ep32 

    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid

! Default version ; Otherwise module containing interface to DM library will provide

   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
!<DESCRIPTION>
! Wrf_patch_domain is called as part of the process of initiating a new
! domain.  Based on the global domain dimension information that is
! passed in it computes the patch and memory dimensions on this
! distributed-memory process for parallel compilation when DM_PARALLEL is
! defined in configure.wrf.  In this case, it relies on an external
! communications package-contributed routine, wrf_dm_patch_domain. For
! non-parallel compiles, it returns the patch and memory dimensions based
! on the entire domain. In either case, the memory dimensions will be
! larger than the patch dimensions, since they allow for distributed
! memory halo regions (DM_PARALLEL only) and for boundary regions around
! the domain (used for idealized cases only).  The width of the boundary
! regions to be accommodated is passed in as bdx and bdy.
! 
! The bdy_mask argument is a four-dimensional logical array, each element
! of which is set to true for any boundaries that this processs patch
! contains (all four are true in the non-DM_PARALLEL case) and false
! otherwise. The indices into the bdy_mask are defined in
! frame/module_state_description.F. P_XSB corresponds boundary that
! exists at the beginning of the X-dimension; ie. the western boundary;
! P_XEB to the boundary that corresponds to the end of the X-dimension
! (east). Likewise for Y (south and north respectively).
! 
! The correspondence of the first, second, and third dimension of each
! set (domain, memory, and patch) with the coordinate axes of the model
! domain is based on the setting of the variable model_data_order, which
! comes into this routine through USE association of
! module_driver_constants in the enclosing module of this routine,
! module_domain.  Model_data_order is defined by the Registry, based on
! the dimspec entries which associate dimension specifiers (e.g. k) in
! the Registry with a coordinate axis and specify which dimension of the
! arrays they represent. For WRF, the sd1 , ed1 , sp1 , ep1 , sm1 , and
! em1 correspond to the starts and ends of the global, patch, and memory
! dimensions in X; those with 2 specify Z (vertical); and those with 3
! specify Y.  Note that the WRF convention is to overdimension to allow
! for staggered fields so that sd<em>n</em>:ed<em>n</em> are the starts
! and ends of the staggered domains in X.  The non-staggered grid runs
! sd<em>n</em>:ed<em>n</em>-1. The extra row or column on the north or
! east boundaries is not used for non-staggered fields.
! 
! The domdesc and parent_domdesc arguments are for external communication
! packages (e.g. RSL) that establish and return to WRF integer handles
! for referring to operations on domains.  These descriptors are not set
! or used otherwise and they are opaque, which means they are never
! accessed or modified in WRF; they are only only passed between calls to
! the external package.
!</DESCRIPTION>

   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  ! z-xpose (std)
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  ! x-xpose
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  ! y-xpose
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent

!local data

   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )



   bdy_mask = .true.     ! only one processor so all 4 boundaries are there

! this is a trivial version -- 1 patch per processor; 
! use version in module_dm to compute for DM
   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    ! just copy
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    ! just copy
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3
! assigns mostly just to suppress warning messages that INTENT OUT vars not assigned
   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3


   RETURN
   END SUBROUTINE wrf_patch_domain
!
   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )

!<DESCRIPTION>
! This subroutine is used to allocate a domain data structure of
! TYPE(DOMAIN) pointed to by the argument <em>grid</em>, link it into the
! nested domain hierarchy, and set its configuration information from
! the appropriate settings in the WRF namelist file. Specifically, if the
! domain being allocated and configured is nest, the <em>parent</em>
! argument will point to the already existing domain data structure for
! the parent domain and the <em>kid</em> argument will be set to an
! integer indicating which child of the parent this grid will be (child
! indices start at 1).  If this is the top-level domain, the parent and
! kid arguments are ignored.  <b>WRF domains may have multiple children
! but only ever have one parent.</b>
!
! The <em>domain_id</em> argument is the
! integer handle by which this new domain will be referred; it comes from
! the grid_id setting in the namelist, and these grid ids correspond to
! the ordering of settings in the namelist, starting with 1 for the
! top-level domain. The id of 1 always corresponds to the top-level
! domain.  and these grid ids correspond to the ordering of settings in
! the namelist, starting with 1 for the top-level domain.
! 
! Model_data_order is provide by USE association of
! module_driver_constants and is set from dimspec entries in the
! Registry.
! 
! The allocation of the TYPE(DOMAIN) itself occurs in this routine.
! However, the numerous multi-dimensional arrays that make up the members
! of the domain are allocated in the call to alloc_space_field, after
! wrf_patch_domain has been called to determine the dimensions in memory
! that should be allocated.  It bears noting here that arrays and code
! that indexes these arrays are always global, regardless of how the
! model is decomposed over patches. Thus, when arrays are allocated on a
! given process, the start and end of an array dimension are the global
! indices of the start and end of that processs subdomain.
! 
! Configuration information for the domain (that is, information from the
! namelist) is added by the call to <a href=med_add_config_info_to_grid.html>med_add_config_info_to_grid</a>, defined
! in share/mediation_wrfmain.F. 
!</DESCRIPTION>

      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)                           :: domain_id
      TYPE( domain ) , POINTER                       :: grid
      TYPE( domain ) , POINTER                       :: parent
      INTEGER , INTENT(IN)                           :: kid    ! which kid of parent am I?

      !  Local data.
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny


! This next step uses information that is listed in the registry as namelist_derived
! to properly size the domain and the patches; this in turn is stored in the new_grid
! data structure


      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering


      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: Incorrect value for num_time_levels ', &
                                       num_time_levels
        CALL wrf_error_fatal3 ( "module_domain.b" , 718 ,  TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF

! provided by application, WRF defines in share/module_bc.F
      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE ( new_grid%parents( max_parents ) )
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    ! initialize in setup_timekeeping

      ! set up the pointers that represent the nest hierarchy
      ! set this up *prior* to calling the patching or allocation
      ! routines so that implementations of these routines can
      ! traverse the nest hierarchy (through the root head_grid)
      ! if they need to 

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    ! note assumption that nest can have only 1 parent
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 ! this needs to be assigned prior to calling wrf_patch_domain

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     ! z-xpose dims
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     ! (standard)
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     ! x-xpose dims
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     ! y-xpose dims
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 

      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         ! initialize the number of frames per file (array assignment)

      CALL alloc_space_field ( new_grid, domain_id , 3 ,           &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   ! x-xpose
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   ! y-xpose
      )

      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           ! this is a mediation layer routine

! Some miscellaneous state that is in the Registry but not namelist data

      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)

! This next set of includes causes all but the namelist_derived variables to be
! properly assigned to the new_grid record

      grid => new_grid






   END SUBROUTINE alloc_and_configure_domain

!

!  This routine ALLOCATEs the required space for the meteorological fields
!  for a specific domain.  The fields are simply ALLOCATEd as an -1.  They
!  are referenced as wind, temperature, moisture, etc. in routines that are
!  below this top-level of data allocation and management (in the solve routine
!  and below).

   SUBROUTINE alloc_space_field ( grid,   id, setinitval ,            &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      
      USE module_configure
      IMPLICIT NONE
 

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval   ! 3 = everything, 1 = arrays only, 0 = none
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      !  Local data.
      INTEGER dyn_opt, idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop




      CALL get_initial_data_value ( initial_data_value )


      CALL nl_get_dyn_opt( 1, dyn_opt )
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 


      IF ( dyn_opt == DYN_NODYN ) THEN

        CALL wrf_message ( 'DYNAMICS OPTION: dynamics disabled ' )

!STARTOFREGISTRYGENERATEDINCLUDE inc/nodyn_allocs.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1951 ,  &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
ALLOCATE(grid%lu_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1957 ,  &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ALLOCATE(grid%nest_pos(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1963 ,  &
'frame/module_domain.f: Failed to allocate grid%nest_pos(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_pos=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epsts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%step_number=0
ALLOCATE(grid%q2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1973 ,  &
'frame/module_domain.f: Failed to allocate grid%q2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2=initial_data_value
ALLOCATE(grid%t2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1979 ,  &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1985 ,  &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
ALLOCATE(grid%psfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1991 ,  &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 1997 ,  &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2003 ,  &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtseps=initial_data_value
IF ( setinitval .EQ. 3 ) grid%resm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%zetatop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf3=initial_data_value
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
ALLOCATE(grid%dpsdt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2020 ,  &
'frame/module_domain.f: Failed to allocate grid%dpsdt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dpsdt=initial_data_value
ALLOCATE(grid%dmudt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2026 ,  &
'frame/module_domain.f: Failed to allocate grid%dmudt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dmudt=initial_data_value
ALLOCATE(grid%psfcm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2032 ,  &
'frame/module_domain.f: Failed to allocate grid%psfcm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfcm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%avg_abs_dpsdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%avg_abs_dmudt=initial_data_value
ALLOCATE(grid%q_conve_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2040 ,  &
'frame/module_domain.f: Failed to allocate grid%q_conve_3d(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_conve_3d=initial_data_value
ALLOCATE(grid%q_conve_2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2046 ,  &
'frame/module_domain.f: Failed to allocate grid%q_conve_2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_conve_2d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
ALLOCATE(grid%imask_nostag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2066 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_nostag=0
ALLOCATE(grid%imask_xstag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2072 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xstag=0
ALLOCATE(grid%imask_ystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2078 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_ystag=0
ALLOCATE(grid%imask_xystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2084 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ALLOCATE(grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2090 ,  &
'frame/module_domain.f: Failed to allocate grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_1=initial_data_value
ALLOCATE(grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2096 ,  &
'frame/module_domain.f: Failed to allocate grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_2=initial_data_value
ALLOCATE(grid%g_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2102 ,  &
'frame/module_domain.f: Failed to allocate grid%g_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_moist_1=initial_data_value
ALLOCATE(grid%g_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2108 ,  &
'frame/module_domain.f: Failed to allocate grid%g_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_moist_2=initial_data_value
ALLOCATE(grid%a_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2114 ,  &
'frame/module_domain.f: Failed to allocate grid%a_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_moist_1=initial_data_value
ALLOCATE(grid%a_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2120 ,  &
'frame/module_domain.f: Failed to allocate grid%a_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_moist_2=initial_data_value
ALLOCATE(grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2126 ,  &
'frame/module_domain.f: Failed to allocate grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem_1=initial_data_value
ALLOCATE(grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2132 ,  &
'frame/module_domain.f: Failed to allocate grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem_2=initial_data_value
ALLOCATE(grid%rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2138 ,  &
'frame/module_domain.f: Failed to allocate grid%rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqc_b=initial_data_value
ALLOCATE(grid%g_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2144 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqc_b=initial_data_value
ALLOCATE(grid%a_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2150 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqc_b=initial_data_value
ALLOCATE(grid%rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2156 ,  &
'frame/module_domain.f: Failed to allocate grid%rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqc_bt=initial_data_value
ALLOCATE(grid%g_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2162 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqc_bt=initial_data_value
ALLOCATE(grid%a_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2168 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqc_bt=initial_data_value
ALLOCATE(grid%rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2174 ,  &
'frame/module_domain.f: Failed to allocate grid%rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqr_b=initial_data_value
ALLOCATE(grid%g_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2180 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqr_b=initial_data_value
ALLOCATE(grid%a_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2186 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqr_b=initial_data_value
ALLOCATE(grid%rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2192 ,  &
'frame/module_domain.f: Failed to allocate grid%rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqr_bt=initial_data_value
ALLOCATE(grid%g_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2198 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqr_bt=initial_data_value
ALLOCATE(grid%a_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2204 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqr_bt=initial_data_value
ALLOCATE(grid%rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2210 ,  &
'frame/module_domain.f: Failed to allocate grid%rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqi_b=initial_data_value
ALLOCATE(grid%g_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2216 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqi_b=initial_data_value
ALLOCATE(grid%a_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2222 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqi_b=initial_data_value
ALLOCATE(grid%rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2228 ,  &
'frame/module_domain.f: Failed to allocate grid%rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqi_bt=initial_data_value
ALLOCATE(grid%g_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2234 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqi_bt=initial_data_value
ALLOCATE(grid%a_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2240 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqi_bt=initial_data_value
ALLOCATE(grid%rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2246 ,  &
'frame/module_domain.f: Failed to allocate grid%rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqs_b=initial_data_value
ALLOCATE(grid%g_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2252 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqs_b=initial_data_value
ALLOCATE(grid%a_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2258 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqs_b=initial_data_value
ALLOCATE(grid%rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2264 ,  &
'frame/module_domain.f: Failed to allocate grid%rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqs_bt=initial_data_value
ALLOCATE(grid%g_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2270 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqs_bt=initial_data_value
ALLOCATE(grid%a_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2276 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqs_bt=initial_data_value
ALLOCATE(grid%rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2282 ,  &
'frame/module_domain.f: Failed to allocate grid%rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqg_b=initial_data_value
ALLOCATE(grid%g_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2288 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqg_b=initial_data_value
ALLOCATE(grid%a_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2294 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqg_b=initial_data_value
ALLOCATE(grid%rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2300 ,  &
'frame/module_domain.f: Failed to allocate grid%rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqg_bt=initial_data_value
ALLOCATE(grid%g_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2306 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqg_bt=initial_data_value
ALLOCATE(grid%a_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2312 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqg_bt=initial_data_value
ALLOCATE(grid%fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2318 ,  &
'frame/module_domain.f: Failed to allocate grid%fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fcx=initial_data_value
ALLOCATE(grid%g_fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2324 ,  &
'frame/module_domain.f: Failed to allocate grid%g_fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_fcx=initial_data_value
ALLOCATE(grid%a_fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2330 ,  &
'frame/module_domain.f: Failed to allocate grid%a_fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_fcx=initial_data_value
ALLOCATE(grid%gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2336 ,  &
'frame/module_domain.f: Failed to allocate grid%gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gcx=initial_data_value
ALLOCATE(grid%g_gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2342 ,  &
'frame/module_domain.f: Failed to allocate grid%g_gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_gcx=initial_data_value
ALLOCATE(grid%a_gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2348 ,  &
'frame/module_domain.f: Failed to allocate grid%a_gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_gcx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2355 ,  &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000010=initial_data_value
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2361 ,  &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010040=initial_data_value
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2367 ,  &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm040100=initial_data_value
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2373 ,  &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100200=initial_data_value
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2379 ,  &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010200=initial_data_value
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2385 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm000=initial_data_value
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2391 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm005=initial_data_value
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2397 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm020=initial_data_value
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2403 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm040=initial_data_value
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2409 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm160=initial_data_value
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2415 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm300=initial_data_value
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2421 ,  &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw000010=initial_data_value
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2427 ,  &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010040=initial_data_value
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2433 ,  &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw040100=initial_data_value
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2439 ,  &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw100200=initial_data_value
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2445 ,  &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010200=initial_data_value
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2451 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw000=initial_data_value
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2457 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw005=initial_data_value
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2463 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw020=initial_data_value
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2469 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw040=initial_data_value
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2475 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw160=initial_data_value
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2481 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw300=initial_data_value
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2487 ,  &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000010=initial_data_value
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2493 ,  &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010040=initial_data_value
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2499 ,  &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st040100=initial_data_value
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2505 ,  &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100200=initial_data_value
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2511 ,  &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010200=initial_data_value
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2517 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt000=initial_data_value
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2523 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt005=initial_data_value
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2529 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt020=initial_data_value
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2535 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt040=initial_data_value
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2541 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt160=initial_data_value
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2547 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2553 ,  &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2559 ,  &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%topostdv=initial_data_value
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2565 ,  &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpx=initial_data_value
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2571 ,  &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpy=initial_data_value
ALLOCATE(grid%shdmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2577 ,  &
'frame/module_domain.f: Failed to allocate grid%shdmax(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmax=initial_data_value
ALLOCATE(grid%shdmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2583 ,  &
'frame/module_domain.f: Failed to allocate grid%shdmin(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmin=initial_data_value
ALLOCATE(grid%snoalb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2589 ,  &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snoalb=initial_data_value
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2595 ,  &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%slopecat=initial_data_value
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2601 ,  &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposoil=initial_data_value
ALLOCATE(grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2607 ,  &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef=initial_data_value
ALLOCATE(grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2613 ,  &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop=initial_data_value
ALLOCATE(grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2619 ,  &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ALLOCATE(grid%soilcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2625 ,  &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcat=initial_data_value
ALLOCATE(grid%vegcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2631 ,  &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ALLOCATE(grid%tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2637 ,  &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
ALLOCATE(grid%zs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2643 ,  &
'frame/module_domain.f: Failed to allocate grid%zs(model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zs=initial_data_value
ALLOCATE(grid%dzs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2649 ,  &
'frame/module_domain.f: Failed to allocate grid%dzs(model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzs=initial_data_value
ALLOCATE(grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2655 ,  &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ALLOCATE(grid%sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2661 ,  &
'frame/module_domain.f: Failed to allocate grid%sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh2o=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2667 ,  &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2673 ,  &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2679 ,  &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2685 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2691 ,  &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2697 ,  &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2703 ,  &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2709 ,  &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2715 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcevp=initial_data_value
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2721 ,  &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grdflx=initial_data_value
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2727 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2733 ,  &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2739 ,  &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2745 ,  &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2751 ,  &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2757 ,  &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2763 ,  &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ifndsnowh=0
IF ( setinitval .EQ. 3 ) grid%ifndsoilw=0
ALLOCATE(grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2771 ,  &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smfr3d=initial_data_value
ALLOCATE(grid%keepfr3dflag(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2777 ,  &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%keepfr3dflag=initial_data_value
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2783 ,  &
'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tke_myj=initial_data_value
ALLOCATE(grid%exch_h(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2789 ,  &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_h=initial_data_value
ALLOCATE(grid%ct(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2795 ,  &
'frame/module_domain.f: Failed to allocate grid%ct(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ct=initial_data_value
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2801 ,  &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thz0=initial_data_value
ALLOCATE(grid%z0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2807 ,  &
'frame/module_domain.f: Failed to allocate grid%z0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0=initial_data_value
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2813 ,  &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qz0=initial_data_value
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2819 ,  &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0=initial_data_value
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2825 ,  &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0=initial_data_value
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2831 ,  &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsfc=initial_data_value
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2837 ,  &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs=initial_data_value
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2843 ,  &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms=initial_data_value
ALLOCATE(grid%kpbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2849 ,  &
'frame/module_domain.f: Failed to allocate grid%kpbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%kpbl=0
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2855 ,  &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htop=initial_data_value
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2861 ,  &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbot=initial_data_value
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2867 ,  &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuppt=initial_data_value
ALLOCATE(grid%totswdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2873 ,  &
'frame/module_domain.f: Failed to allocate grid%totswdn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%totswdn=initial_data_value
ALLOCATE(grid%totlwdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2879 ,  &
'frame/module_domain.f: Failed to allocate grid%totlwdn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%totlwdn=initial_data_value
ALLOCATE(grid%rswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2885 ,  &
'frame/module_domain.f: Failed to allocate grid%rswtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtoa=initial_data_value
ALLOCATE(grid%rlwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2891 ,  &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtoa=initial_data_value
ALLOCATE(grid%czmean(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2897 ,  &
'frame/module_domain.f: Failed to allocate grid%czmean(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czmean=initial_data_value
ALLOCATE(grid%cfracl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2903 ,  &
'frame/module_domain.f: Failed to allocate grid%cfracl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracl=initial_data_value
ALLOCATE(grid%cfracm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2909 ,  &
'frame/module_domain.f: Failed to allocate grid%cfracm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracm=initial_data_value
ALLOCATE(grid%cfrach(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2915 ,  &
'frame/module_domain.f: Failed to allocate grid%cfrach(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfrach=initial_data_value
ALLOCATE(grid%acfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2921 ,  &
'frame/module_domain.f: Failed to allocate grid%acfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrst=initial_data_value
ALLOCATE(grid%ncfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2927 ,  &
'frame/module_domain.f: Failed to allocate grid%ncfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrst=0
ALLOCATE(grid%acfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2933 ,  &
'frame/module_domain.f: Failed to allocate grid%acfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrcv=initial_data_value
ALLOCATE(grid%ncfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2939 ,  &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrcv=0
ALLOCATE(grid%aer_dry(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2945 ,  &
'frame/module_domain.f: Failed to allocate grid%aer_dry(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aer_dry=initial_data_value
ALLOCATE(grid%aer_water(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2951 ,  &
'frame/module_domain.f: Failed to allocate grid%aer_water(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aer_water=initial_data_value
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2957 ,  &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2963 ,  &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2969 ,  &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
ALLOCATE(grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2975 ,  &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%h_diabatic=initial_data_value
ALLOCATE(grid%msft(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2981 ,  &
'frame/module_domain.f: Failed to allocate grid%msft(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msft=initial_data_value
ALLOCATE(grid%msfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2987 ,  &
'frame/module_domain.f: Failed to allocate grid%msfu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfu=initial_data_value
ALLOCATE(grid%msfv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2993 ,  &
'frame/module_domain.f: Failed to allocate grid%msfv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfv=initial_data_value
ALLOCATE(grid%f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 2999 ,  &
'frame/module_domain.f: Failed to allocate grid%f(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f=initial_data_value
ALLOCATE(grid%e(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3005 ,  &
'frame/module_domain.f: Failed to allocate grid%e(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%e=initial_data_value
ALLOCATE(grid%sina(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3011 ,  &
'frame/module_domain.f: Failed to allocate grid%sina(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sina=initial_data_value
ALLOCATE(grid%cosa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3017 ,  &
'frame/module_domain.f: Failed to allocate grid%cosa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosa=initial_data_value
ALLOCATE(grid%ht(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3023 ,  &
'frame/module_domain.f: Failed to allocate grid%ht(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht=initial_data_value
ALLOCATE(grid%ht_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3029 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_fine=initial_data_value
ALLOCATE(grid%ht_int(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3035 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_int(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_int=initial_data_value
ALLOCATE(grid%ht_input(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3041 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_input(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_input=initial_data_value
ALLOCATE(grid%tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3047 ,  &
'frame/module_domain.f: Failed to allocate grid%tsk(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk=initial_data_value
ALLOCATE(grid%tsk_save(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3053 ,  &
'frame/module_domain.f: Failed to allocate grid%tsk_save(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk_save=initial_data_value
ALLOCATE(grid%u_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3059 ,  &
'frame/module_domain.f: Failed to allocate grid%u_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_base=initial_data_value
ALLOCATE(grid%v_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3065 ,  &
'frame/module_domain.f: Failed to allocate grid%v_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_base=initial_data_value
ALLOCATE(grid%qv_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3071 ,  &
'frame/module_domain.f: Failed to allocate grid%qv_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qv_base=initial_data_value
ALLOCATE(grid%z_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3077 ,  &
'frame/module_domain.f: Failed to allocate grid%z_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z_base=initial_data_value
IF ( setinitval .EQ. 3 ) grid%u_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%v_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%p_top=initial_data_value
IF ( setinitval .EQ. 3 ) grid%imicrogram=0
ALLOCATE(grid%rthcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3087 ,  &
'frame/module_domain.f: Failed to allocate grid%rthcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthcuten=initial_data_value
ALLOCATE(grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3093 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvcuten=initial_data_value
ALLOCATE(grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3099 ,  &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqrcuten=initial_data_value
ALLOCATE(grid%rqccuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3105 ,  &
'frame/module_domain.f: Failed to allocate grid%rqccuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqccuten=initial_data_value
ALLOCATE(grid%rqscuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3111 ,  &
'frame/module_domain.f: Failed to allocate grid%rqscuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqscuten=initial_data_value
ALLOCATE(grid%rqicuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3117 ,  &
'frame/module_domain.f: Failed to allocate grid%rqicuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqicuten=initial_data_value
ALLOCATE(grid%w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3123 ,  &
'frame/module_domain.f: Failed to allocate grid%w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w0avg=initial_data_value
ALLOCATE(grid%rainc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3129 ,  &
'frame/module_domain.f: Failed to allocate grid%rainc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainc=initial_data_value
ALLOCATE(grid%rainnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3135 ,  &
'frame/module_domain.f: Failed to allocate grid%rainnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainnc=initial_data_value
ALLOCATE(grid%raincv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3141 ,  &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%raincv=initial_data_value
ALLOCATE(grid%rainncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3147 ,  &
'frame/module_domain.f: Failed to allocate grid%rainncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainncv=initial_data_value
ALLOCATE(grid%rainbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3153 ,  &
'frame/module_domain.f: Failed to allocate grid%rainbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainbl=initial_data_value
ALLOCATE(grid%nca(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3159 ,  &
'frame/module_domain.f: Failed to allocate grid%nca(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nca=initial_data_value
ALLOCATE(grid%lowlyr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3165 ,  &
'frame/module_domain.f: Failed to allocate grid%lowlyr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lowlyr=0
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3171 ,  &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mass_flux=initial_data_value
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3177 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_gr=initial_data_value
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3183 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_w=initial_data_value
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3189 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_mc=initial_data_value
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3195 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_st=initial_data_value
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3201 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_as=initial_data_value
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3207 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capma=initial_data_value
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3213 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capme=initial_data_value
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3219 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capmi=initial_data_value
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3225 ,  &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xf_ens=initial_data_value
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3231 ,  &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pr_ens=initial_data_value
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3237 ,  &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthften=initial_data_value
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3243 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepcu=0
ALLOCATE(grid%rthraten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3250 ,  &
'frame/module_domain.f: Failed to allocate grid%rthraten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthraten=initial_data_value
ALLOCATE(grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3256 ,  &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratenlw=initial_data_value
ALLOCATE(grid%rthratensw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3262 ,  &
'frame/module_domain.f: Failed to allocate grid%rthratensw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratensw=initial_data_value
ALLOCATE(grid%cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3268 ,  &
'frame/module_domain.f: Failed to allocate grid%cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldfra=initial_data_value
ALLOCATE(grid%swdown(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3274 ,  &
'frame/module_domain.f: Failed to allocate grid%swdown(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdown=initial_data_value
ALLOCATE(grid%gsw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3280 ,  &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ALLOCATE(grid%glw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3286 ,  &
'frame/module_domain.f: Failed to allocate grid%glw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glw=initial_data_value
ALLOCATE(grid%xlat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3292 ,  &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ALLOCATE(grid%xlong(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3298 ,  &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ALLOCATE(grid%albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3304 ,  &
'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3310 ,  &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
ALLOCATE(grid%emiss(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3316 ,  &
'frame/module_domain.f: Failed to allocate grid%emiss(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emiss=initial_data_value
ALLOCATE(grid%cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3322 ,  &
'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepra=0
ALLOCATE(grid%rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3329 ,  &
'frame/module_domain.f: Failed to allocate grid%rublten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rublten=initial_data_value
ALLOCATE(grid%rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3335 ,  &
'frame/module_domain.f: Failed to allocate grid%rvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvblten=initial_data_value
ALLOCATE(grid%rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3341 ,  &
'frame/module_domain.f: Failed to allocate grid%rthblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthblten=initial_data_value
ALLOCATE(grid%rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3347 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvblten=initial_data_value
ALLOCATE(grid%rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3353 ,  &
'frame/module_domain.f: Failed to allocate grid%rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqcblten=initial_data_value
ALLOCATE(grid%rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3359 ,  &
'frame/module_domain.f: Failed to allocate grid%rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqiblten=initial_data_value
ALLOCATE(grid%tmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3365 ,  &
'frame/module_domain.f: Failed to allocate grid%tmn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn=initial_data_value
ALLOCATE(grid%xland(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3371 ,  &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3377 ,  &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
ALLOCATE(grid%ust(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3383 ,  &
'frame/module_domain.f: Failed to allocate grid%ust(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ust=initial_data_value
ALLOCATE(grid%rmol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3389 ,  &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3395 ,  &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
ALLOCATE(grid%pblh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3401 ,  &
'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
ALLOCATE(grid%capg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3407 ,  &
'frame/module_domain.f: Failed to allocate grid%capg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%capg=initial_data_value
ALLOCATE(grid%thc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3413 ,  &
'frame/module_domain.f: Failed to allocate grid%thc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thc=initial_data_value
ALLOCATE(grid%hfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3419 ,  &
'frame/module_domain.f: Failed to allocate grid%hfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hfx=initial_data_value
ALLOCATE(grid%qfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3425 ,  &
'frame/module_domain.f: Failed to allocate grid%qfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qfx=initial_data_value
ALLOCATE(grid%lh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3431 ,  &
'frame/module_domain.f: Failed to allocate grid%lh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh=initial_data_value
ALLOCATE(grid%flhc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3437 ,  &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
ALLOCATE(grid%flqc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3443 ,  &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
ALLOCATE(grid%qsg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3449 ,  &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
ALLOCATE(grid%qvg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3455 ,  &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
ALLOCATE(grid%qcg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3461 ,  &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
ALLOCATE(grid%soilt1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3467 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
ALLOCATE(grid%tsnav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3473 ,  &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ALLOCATE(grid%snowc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3479 ,  &
'frame/module_domain.f: Failed to allocate grid%snowc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowc=initial_data_value
ALLOCATE(grid%mavail(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3485 ,  &
'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
ALLOCATE(grid%tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3491 ,  &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tkesfcf=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepbl=0
ALLOCATE(grid%taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3498 ,  &
'frame/module_domain.f: Failed to allocate grid%taucldi(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldi=initial_data_value
ALLOCATE(grid%taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3504 ,  &
'frame/module_domain.f: Failed to allocate grid%taucldc(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldc=initial_data_value
ALLOCATE(grid%defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3510 ,  &
'frame/module_domain.f: Failed to allocate grid%defor11(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor11=initial_data_value
ALLOCATE(grid%defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3516 ,  &
'frame/module_domain.f: Failed to allocate grid%defor22(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor22=initial_data_value
ALLOCATE(grid%defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3522 ,  &
'frame/module_domain.f: Failed to allocate grid%defor12(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor12=initial_data_value
ALLOCATE(grid%defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3528 ,  &
'frame/module_domain.f: Failed to allocate grid%defor33(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor33=initial_data_value
ALLOCATE(grid%defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3534 ,  &
'frame/module_domain.f: Failed to allocate grid%defor13(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor13=initial_data_value
ALLOCATE(grid%defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3540 ,  &
'frame/module_domain.f: Failed to allocate grid%defor23(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor23=initial_data_value
ALLOCATE(grid%xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3546 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmv=initial_data_value
ALLOCATE(grid%xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3552 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmh=initial_data_value
ALLOCATE(grid%xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3558 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmhd=initial_data_value
ALLOCATE(grid%g_xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3564 ,  &
'frame/module_domain.f: Failed to allocate grid%g_xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_xkmhd=initial_data_value
ALLOCATE(grid%a_xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3570 ,  &
'frame/module_domain.f: Failed to allocate grid%a_xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_xkmhd=initial_data_value
ALLOCATE(grid%xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3576 ,  &
'frame/module_domain.f: Failed to allocate grid%xkhv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhv=initial_data_value
ALLOCATE(grid%xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3582 ,  &
'frame/module_domain.f: Failed to allocate grid%xkhh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhh=initial_data_value
ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3588 ,  &
'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
ALLOCATE(grid%bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 3594 ,  &
'frame/module_domain.f: Failed to allocate grid%bn2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bn2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%warm_rain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%input_from_hires=.FALSE.
IF ( setinitval .EQ. 3 ) grid%history_interval=0
IF ( setinitval .EQ. 3 ) grid%frames_per_outfile=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%restart=.FALSE.
IF ( setinitval .EQ. 3 ) grid%restart_interval=0
IF ( setinitval .EQ. 3 ) grid%io_form_input=0
IF ( setinitval .EQ. 3 ) grid%io_form_history=0
IF ( setinitval .EQ. 3 ) grid%io_form_restart=0
IF ( setinitval .EQ. 3 ) grid%io_form_boundary=0
IF ( setinitval .EQ. 3 ) grid%debug_level=0
IF ( setinitval .EQ. 3 ) grid%history_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%history_interval_d=0
IF ( setinitval .EQ. 3 ) grid%history_interval_h=0
IF ( setinitval .EQ. 3 ) grid%history_interval_m=0
IF ( setinitval .EQ. 3 ) grid%history_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_d=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_h=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_m=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_s=0
IF ( setinitval .EQ. 3 ) grid%history_begin_y=0
IF ( setinitval .EQ. 3 ) grid%history_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%history_begin_d=0
IF ( setinitval .EQ. 3 ) grid%history_begin_h=0
IF ( setinitval .EQ. 3 ) grid%history_begin_m=0
IF ( setinitval .EQ. 3 ) grid%history_begin_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_y=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_d=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_h=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_m=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_s=0
IF ( setinitval .EQ. 3 ) grid%history_end_y=0
IF ( setinitval .EQ. 3 ) grid%history_end_mo=0
IF ( setinitval .EQ. 3 ) grid%history_end_d=0
IF ( setinitval .EQ. 3 ) grid%history_end_h=0
IF ( setinitval .EQ. 3 ) grid%history_end_m=0
IF ( setinitval .EQ. 3 ) grid%history_end_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_s=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%level=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%coral_dist=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%test_rk_step_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_init_zero_tendency=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_phy_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calculate_km_kh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_tendency=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advect_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_relax_bdy_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_addtend_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdy_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_small_step_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_p_rho=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_coef_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_uv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdyupdate=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_mu_t=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_sumflux=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdyupdate_ph=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_mu_uv_1=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_small_step_finish=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_scalar_tend=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdy_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_update_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_p_rho_phi=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_diagnose_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%chem_opt=0
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_full_fields=.FALSE.
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
IF ( setinitval .EQ. 3 ) grid%nodyn_nodyn_dummy=0
!ENDOFREGISTRYGENERATEDINCLUDE


      ELSE IF ( mod(dyn_opt, 100) == DYN_EM ) THEN
        CALL wrf_message ( 'DYNAMICS OPTION: Eulerian Mass Coordinate ')

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_allocs.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4038 ,  &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
ALLOCATE(grid%lu_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4044 ,  &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ALLOCATE(grid%em_u_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4050 ,  &
'frame/module_domain.f: Failed to allocate grid%em_u_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_1=initial_data_value
ALLOCATE(grid%em_u_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4056 ,  &
'frame/module_domain.f: Failed to allocate grid%em_u_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_2=initial_data_value
ALLOCATE(grid%em_g_u_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4062 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_u_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_u_1=initial_data_value
ALLOCATE(grid%em_g_u_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4068 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_u_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_u_2=initial_data_value
ALLOCATE(grid%em_a_u_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4074 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_u_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_u_1=initial_data_value
ALLOCATE(grid%em_a_u_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4080 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_u_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_u_2=initial_data_value
ALLOCATE(grid%em_ru(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4086 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ru(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru=initial_data_value
ALLOCATE(grid%em_g_ru(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4092 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ru(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ru=initial_data_value
ALLOCATE(grid%em_a_ru(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4098 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ru(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ru=initial_data_value
ALLOCATE(grid%em_ru_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4104 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ru_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru_m=initial_data_value
ALLOCATE(grid%em_g_ru_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4110 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ru_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ru_m=initial_data_value
ALLOCATE(grid%em_a_ru_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4116 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ru_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ru_m=initial_data_value
ALLOCATE(grid%em_ru_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4122 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ru_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru_tend=initial_data_value
ALLOCATE(grid%em_g_ru_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4128 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ru_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ru_tend=initial_data_value
ALLOCATE(grid%em_a_ru_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4134 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ru_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ru_tend=initial_data_value
ALLOCATE(grid%em_u_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4140 ,  &
'frame/module_domain.f: Failed to allocate grid%em_u_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_save=initial_data_value
ALLOCATE(grid%em_g_u_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4146 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_u_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_u_save=initial_data_value
ALLOCATE(grid%em_a_u_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4152 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_u_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_u_save=initial_data_value
ALLOCATE(grid%em_v_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4158 ,  &
'frame/module_domain.f: Failed to allocate grid%em_v_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_1=initial_data_value
ALLOCATE(grid%em_v_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4164 ,  &
'frame/module_domain.f: Failed to allocate grid%em_v_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_2=initial_data_value
ALLOCATE(grid%em_g_v_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4170 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_v_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_v_1=initial_data_value
ALLOCATE(grid%em_g_v_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4176 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_v_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_v_2=initial_data_value
ALLOCATE(grid%em_a_v_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4182 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_v_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_v_1=initial_data_value
ALLOCATE(grid%em_a_v_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4188 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_v_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_v_2=initial_data_value
ALLOCATE(grid%em_rv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4194 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv=initial_data_value
ALLOCATE(grid%em_g_rv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4200 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rv=initial_data_value
ALLOCATE(grid%em_a_rv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4206 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rv=initial_data_value
ALLOCATE(grid%em_rv_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4212 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rv_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv_m=initial_data_value
ALLOCATE(grid%em_g_rv_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4218 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rv_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rv_m=initial_data_value
ALLOCATE(grid%em_a_rv_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4224 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rv_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rv_m=initial_data_value
ALLOCATE(grid%em_rv_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4230 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rv_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv_tend=initial_data_value
ALLOCATE(grid%em_g_rv_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4236 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rv_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rv_tend=initial_data_value
ALLOCATE(grid%em_a_rv_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4242 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rv_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rv_tend=initial_data_value
ALLOCATE(grid%em_v_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4248 ,  &
'frame/module_domain.f: Failed to allocate grid%em_v_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_save=initial_data_value
ALLOCATE(grid%em_g_v_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4254 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_v_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_v_save=initial_data_value
ALLOCATE(grid%em_a_v_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4260 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_v_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_v_save=initial_data_value
ALLOCATE(grid%em_w_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4266 ,  &
'frame/module_domain.f: Failed to allocate grid%em_w_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_1=initial_data_value
ALLOCATE(grid%em_w_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4272 ,  &
'frame/module_domain.f: Failed to allocate grid%em_w_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_2=initial_data_value
ALLOCATE(grid%em_g_w_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4278 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_w_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_w_1=initial_data_value
ALLOCATE(grid%em_g_w_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4284 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_w_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_w_2=initial_data_value
ALLOCATE(grid%em_a_w_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4290 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_w_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_w_1=initial_data_value
ALLOCATE(grid%em_a_w_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4296 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_w_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_w_2=initial_data_value
ALLOCATE(grid%em_ww(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4302 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ww(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ww=initial_data_value
ALLOCATE(grid%em_g_ww(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4308 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ww(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ww=initial_data_value
ALLOCATE(grid%em_a_ww(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4314 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ww(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ww=initial_data_value
ALLOCATE(grid%em_rw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4320 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rw=initial_data_value
ALLOCATE(grid%em_g_rw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4326 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rw=initial_data_value
ALLOCATE(grid%em_a_rw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4332 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rw=initial_data_value
ALLOCATE(grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4338 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_1=initial_data_value
ALLOCATE(grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4344 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_2=initial_data_value
ALLOCATE(grid%em_g_ph_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4350 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ph_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ph_1=initial_data_value
ALLOCATE(grid%em_g_ph_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4356 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ph_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ph_2=initial_data_value
ALLOCATE(grid%em_a_ph_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4362 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ph_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ph_1=initial_data_value
ALLOCATE(grid%em_a_ph_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4368 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ph_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ph_2=initial_data_value
ALLOCATE(grid%em_phb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4374 ,  &
'frame/module_domain.f: Failed to allocate grid%em_phb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_phb=initial_data_value
ALLOCATE(grid%em_g_phb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4380 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_phb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_phb=initial_data_value
ALLOCATE(grid%em_a_phb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4386 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_phb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_phb=initial_data_value
ALLOCATE(grid%em_phb_fine(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4392 ,  &
'frame/module_domain.f: Failed to allocate grid%em_phb_fine(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_phb_fine=initial_data_value
ALLOCATE(grid%em_ph0(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4398 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ph0(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph0=initial_data_value
ALLOCATE(grid%em_php(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4404 ,  &
'frame/module_domain.f: Failed to allocate grid%em_php(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_php=initial_data_value
ALLOCATE(grid%em_g_php(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4410 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_php(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_php=initial_data_value
ALLOCATE(grid%em_a_php(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4416 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_php(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_php=initial_data_value
ALLOCATE(grid%em_t_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4422 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_1=initial_data_value
ALLOCATE(grid%em_t_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4428 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_2=initial_data_value
ALLOCATE(grid%em_g_t_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4434 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_t_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_t_1=initial_data_value
ALLOCATE(grid%em_g_t_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4440 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_t_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_t_2=initial_data_value
ALLOCATE(grid%em_a_t_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4446 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_t_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_t_1=initial_data_value
ALLOCATE(grid%em_a_t_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4452 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_t_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_t_2=initial_data_value
ALLOCATE(grid%em_t_init(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4458 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_init(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_init=initial_data_value
ALLOCATE(grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4464 ,  &
'frame/module_domain.f: Failed to allocate grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tp_1=initial_data_value
ALLOCATE(grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4470 ,  &
'frame/module_domain.f: Failed to allocate grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tp_2=initial_data_value
ALLOCATE(grid%em_t_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4476 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_save=initial_data_value
ALLOCATE(grid%em_g_t_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4482 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_t_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_t_save=initial_data_value
ALLOCATE(grid%em_a_t_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4488 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_t_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_t_save=initial_data_value
ALLOCATE(grid%em_mu_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4494 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mu_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_1=initial_data_value
ALLOCATE(grid%em_mu_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4500 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mu_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_2=initial_data_value
ALLOCATE(grid%em_g_mu_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4506 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mu_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mu_1=initial_data_value
ALLOCATE(grid%em_g_mu_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4512 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mu_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mu_2=initial_data_value
ALLOCATE(grid%em_a_mu_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4518 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mu_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mu_1=initial_data_value
ALLOCATE(grid%em_a_mu_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4524 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mu_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mu_2=initial_data_value
ALLOCATE(grid%em_mub(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4530 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mub(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mub=initial_data_value
ALLOCATE(grid%em_g_mub(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4536 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mub(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mub=initial_data_value
ALLOCATE(grid%em_a_mub(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4542 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mub(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mub=initial_data_value
ALLOCATE(grid%em_mub_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4548 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mub_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mub_fine=initial_data_value
ALLOCATE(grid%em_mu0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4554 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mu0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu0=initial_data_value
ALLOCATE(grid%em_mudf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4560 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mudf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mudf=initial_data_value
ALLOCATE(grid%em_g_mudf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4566 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mudf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mudf=initial_data_value
ALLOCATE(grid%em_a_mudf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4572 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mudf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mudf=initial_data_value
ALLOCATE(grid%em_muu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4578 ,  &
'frame/module_domain.f: Failed to allocate grid%em_muu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muu=initial_data_value
ALLOCATE(grid%em_g_muu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4584 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_muu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_muu=initial_data_value
ALLOCATE(grid%em_a_muu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4590 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_muu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_muu=initial_data_value
ALLOCATE(grid%em_muv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4596 ,  &
'frame/module_domain.f: Failed to allocate grid%em_muv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muv=initial_data_value
ALLOCATE(grid%em_g_muv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4602 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_muv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_muv=initial_data_value
ALLOCATE(grid%em_a_muv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4608 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_muv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_muv=initial_data_value
ALLOCATE(grid%em_mut(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4614 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mut(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mut=initial_data_value
ALLOCATE(grid%em_g_mut(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4620 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mut(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mut=initial_data_value
ALLOCATE(grid%em_a_mut(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4626 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mut(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mut=initial_data_value
ALLOCATE(grid%em_muts(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4632 ,  &
'frame/module_domain.f: Failed to allocate grid%em_muts(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muts=initial_data_value
ALLOCATE(grid%em_g_muts(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4638 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_muts(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_muts=initial_data_value
ALLOCATE(grid%em_a_muts(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4644 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_muts(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_muts=initial_data_value
ALLOCATE(grid%nest_pos(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4650 ,  &
'frame/module_domain.f: Failed to allocate grid%nest_pos(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_pos=initial_data_value
ALLOCATE(grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4656 ,  &
'frame/module_domain.f: Failed to allocate grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tke_1=initial_data_value
ALLOCATE(grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4662 ,  &
'frame/module_domain.f: Failed to allocate grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tke_2=initial_data_value
ALLOCATE(grid%em_p(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4668 ,  &
'frame/module_domain.f: Failed to allocate grid%em_p(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_p=initial_data_value
ALLOCATE(grid%em_g_p(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4674 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_p(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_p=initial_data_value
ALLOCATE(grid%em_a_p(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4680 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_p(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_p=initial_data_value
ALLOCATE(grid%em_al(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4686 ,  &
'frame/module_domain.f: Failed to allocate grid%em_al(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_al=initial_data_value
ALLOCATE(grid%em_g_al(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4692 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_al(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_al=initial_data_value
ALLOCATE(grid%em_a_al(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4698 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_al(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_al=initial_data_value
ALLOCATE(grid%em_alt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4704 ,  &
'frame/module_domain.f: Failed to allocate grid%em_alt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_alt=initial_data_value
ALLOCATE(grid%em_g_alt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4710 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_alt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_alt=initial_data_value
ALLOCATE(grid%em_a_alt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4716 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_alt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_alt=initial_data_value
ALLOCATE(grid%em_alb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4722 ,  &
'frame/module_domain.f: Failed to allocate grid%em_alb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_alb=initial_data_value
ALLOCATE(grid%em_zx(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4728 ,  &
'frame/module_domain.f: Failed to allocate grid%em_zx(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_zx=initial_data_value
ALLOCATE(grid%em_zy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4734 ,  &
'frame/module_domain.f: Failed to allocate grid%em_zy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_zy=initial_data_value
ALLOCATE(grid%em_rdz(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4740 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rdz(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdz=initial_data_value
ALLOCATE(grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4746 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdzw=initial_data_value
ALLOCATE(grid%em_pb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4752 ,  &
'frame/module_domain.f: Failed to allocate grid%em_pb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pb=initial_data_value
ALLOCATE(grid%em_alpha(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4758 ,  &
'frame/module_domain.f: Failed to allocate grid%em_alpha(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_alpha=initial_data_value
ALLOCATE(grid%em_g_alpha(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4764 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_alpha(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_alpha=initial_data_value
ALLOCATE(grid%em_a_alpha(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4770 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_alpha(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_alpha=initial_data_value
ALLOCATE(grid%em_a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4776 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a=initial_data_value
ALLOCATE(grid%em_g_a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4782 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_a=initial_data_value
ALLOCATE(grid%em_a_a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4788 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_a=initial_data_value
ALLOCATE(grid%em_gamma(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4794 ,  &
'frame/module_domain.f: Failed to allocate grid%em_gamma(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_gamma=initial_data_value
ALLOCATE(grid%em_g_gamma(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4800 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_gamma(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_gamma=initial_data_value
ALLOCATE(grid%em_a_gamma(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4806 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_gamma(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_gamma=initial_data_value
ALLOCATE(grid%em_c2a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4812 ,  &
'frame/module_domain.f: Failed to allocate grid%em_c2a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_c2a=initial_data_value
ALLOCATE(grid%em_g_c2a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4818 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_c2a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_c2a=initial_data_value
ALLOCATE(grid%em_a_c2a(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4824 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_c2a(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_c2a=initial_data_value
ALLOCATE(grid%em_rho(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4830 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rho(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rho=initial_data_value
ALLOCATE(grid%em_g_rho(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4836 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rho(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rho=initial_data_value
ALLOCATE(grid%em_a_rho(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4842 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rho(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rho=initial_data_value
ALLOCATE(grid%em_phm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4848 ,  &
'frame/module_domain.f: Failed to allocate grid%em_phm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_phm=initial_data_value
ALLOCATE(grid%em_g_phm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4854 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_phm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_phm=initial_data_value
ALLOCATE(grid%em_a_phm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4860 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_phm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_phm=initial_data_value
ALLOCATE(grid%em_cqu(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4866 ,  &
'frame/module_domain.f: Failed to allocate grid%em_cqu(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_cqu=initial_data_value
ALLOCATE(grid%em_g_cqu(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4872 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_cqu(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_cqu=initial_data_value
ALLOCATE(grid%em_a_cqu(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4878 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_cqu(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_cqu=initial_data_value
ALLOCATE(grid%em_cqv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4884 ,  &
'frame/module_domain.f: Failed to allocate grid%em_cqv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_cqv=initial_data_value
ALLOCATE(grid%em_g_cqv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4890 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_cqv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_cqv=initial_data_value
ALLOCATE(grid%em_a_cqv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4896 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_cqv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_cqv=initial_data_value
ALLOCATE(grid%em_cqw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4902 ,  &
'frame/module_domain.f: Failed to allocate grid%em_cqw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_cqw=initial_data_value
ALLOCATE(grid%em_g_cqw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4908 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_cqw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_cqw=initial_data_value
ALLOCATE(grid%em_a_cqw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4914 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_cqw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_cqw=initial_data_value
ALLOCATE(grid%em_pm1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4920 ,  &
'frame/module_domain.f: Failed to allocate grid%em_pm1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pm1=initial_data_value
ALLOCATE(grid%em_g_pm1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4926 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_pm1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_pm1=initial_data_value
ALLOCATE(grid%em_a_pm1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4932 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_pm1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_pm1=initial_data_value
ALLOCATE(grid%em_fnm(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4938 ,  &
'frame/module_domain.f: Failed to allocate grid%em_fnm(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_fnm=initial_data_value
ALLOCATE(grid%em_fnp(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4944 ,  &
'frame/module_domain.f: Failed to allocate grid%em_fnp(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_fnp=initial_data_value
ALLOCATE(grid%em_rdnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4950 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rdnw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdnw=initial_data_value
ALLOCATE(grid%em_rdn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4956 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rdn(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdn=initial_data_value
ALLOCATE(grid%em_dnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4962 ,  &
'frame/module_domain.f: Failed to allocate grid%em_dnw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_dnw=initial_data_value
ALLOCATE(grid%em_dn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4968 ,  &
'frame/module_domain.f: Failed to allocate grid%em_dn(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_dn=initial_data_value
ALLOCATE(grid%em_znu(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4974 ,  &
'frame/module_domain.f: Failed to allocate grid%em_znu(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_znu=initial_data_value
ALLOCATE(grid%em_znw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4980 ,  &
'frame/module_domain.f: Failed to allocate grid%em_znw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_znw=initial_data_value
ALLOCATE(grid%em_t_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4986 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_base=initial_data_value
ALLOCATE(grid%em_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4992 ,  &
'frame/module_domain.f: Failed to allocate grid%em_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_z=initial_data_value
ALLOCATE(grid%em_g_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 4998 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_z=initial_data_value
ALLOCATE(grid%em_a_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5004 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_z=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epsts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%step_number=0
ALLOCATE(grid%q2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5014 ,  &
'frame/module_domain.f: Failed to allocate grid%q2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2=initial_data_value
ALLOCATE(grid%t2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5020 ,  &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5026 ,  &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
ALLOCATE(grid%psfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5032 ,  &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5038 ,  &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5044 ,  &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtseps=initial_data_value
IF ( setinitval .EQ. 3 ) grid%resm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%zetatop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf3=initial_data_value
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
ALLOCATE(grid%dpsdt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5061 ,  &
'frame/module_domain.f: Failed to allocate grid%dpsdt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dpsdt=initial_data_value
ALLOCATE(grid%dmudt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5067 ,  &
'frame/module_domain.f: Failed to allocate grid%dmudt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dmudt=initial_data_value
ALLOCATE(grid%psfcm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5073 ,  &
'frame/module_domain.f: Failed to allocate grid%psfcm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfcm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%avg_abs_dpsdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%avg_abs_dmudt=initial_data_value
ALLOCATE(grid%q_conve_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5081 ,  &
'frame/module_domain.f: Failed to allocate grid%q_conve_3d(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_conve_3d=initial_data_value
ALLOCATE(grid%q_conve_2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5087 ,  &
'frame/module_domain.f: Failed to allocate grid%q_conve_2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_conve_2d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
ALLOCATE(grid%imask_nostag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5107 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_nostag=0
ALLOCATE(grid%imask_xstag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5113 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xstag=0
ALLOCATE(grid%imask_ystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5119 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_ystag=0
ALLOCATE(grid%imask_xystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5125 ,  &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ALLOCATE(grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5131 ,  &
'frame/module_domain.f: Failed to allocate grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_1=initial_data_value
ALLOCATE(grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5137 ,  &
'frame/module_domain.f: Failed to allocate grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_2=initial_data_value
ALLOCATE(grid%g_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5143 ,  &
'frame/module_domain.f: Failed to allocate grid%g_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_moist_1=initial_data_value
ALLOCATE(grid%g_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5149 ,  &
'frame/module_domain.f: Failed to allocate grid%g_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_moist_2=initial_data_value
ALLOCATE(grid%a_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5155 ,  &
'frame/module_domain.f: Failed to allocate grid%a_moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_moist_1=initial_data_value
ALLOCATE(grid%a_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5161 ,  &
'frame/module_domain.f: Failed to allocate grid%a_moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_moist_2=initial_data_value
ALLOCATE(grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5167 ,  &
'frame/module_domain.f: Failed to allocate grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem_1=initial_data_value
ALLOCATE(grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5173 ,  &
'frame/module_domain.f: Failed to allocate grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem_2=initial_data_value
ALLOCATE(grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5179 ,  &
'frame/module_domain.f: Failed to allocate grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_b=initial_data_value
ALLOCATE(grid%em_g_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5185 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_u_b=initial_data_value
ALLOCATE(grid%em_a_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5191 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_u_b=initial_data_value
ALLOCATE(grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5197 ,  &
'frame/module_domain.f: Failed to allocate grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_bt=initial_data_value
ALLOCATE(grid%em_g_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5203 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_u_bt=initial_data_value
ALLOCATE(grid%em_a_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5209 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_u_bt=initial_data_value
ALLOCATE(grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5215 ,  &
'frame/module_domain.f: Failed to allocate grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_b=initial_data_value
ALLOCATE(grid%em_g_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5221 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_v_b=initial_data_value
ALLOCATE(grid%em_a_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5227 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_v_b=initial_data_value
ALLOCATE(grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5233 ,  &
'frame/module_domain.f: Failed to allocate grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_bt=initial_data_value
ALLOCATE(grid%em_g_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5239 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_v_bt=initial_data_value
ALLOCATE(grid%em_a_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5245 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_v_bt=initial_data_value
ALLOCATE(grid%em_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5251 ,  &
'frame/module_domain.f: Failed to allocate grid%em_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_b=initial_data_value
ALLOCATE(grid%em_g_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5257 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_w_b=initial_data_value
ALLOCATE(grid%em_a_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5263 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_w_b=initial_data_value
ALLOCATE(grid%em_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5269 ,  &
'frame/module_domain.f: Failed to allocate grid%em_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_bt=initial_data_value
ALLOCATE(grid%em_g_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5275 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_w_bt=initial_data_value
ALLOCATE(grid%em_a_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5281 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_w_bt=initial_data_value
ALLOCATE(grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5287 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_b=initial_data_value
ALLOCATE(grid%em_g_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5293 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ph_b=initial_data_value
ALLOCATE(grid%em_a_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5299 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ph_b=initial_data_value
ALLOCATE(grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5305 ,  &
'frame/module_domain.f: Failed to allocate grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_bt=initial_data_value
ALLOCATE(grid%em_g_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5311 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_ph_bt=initial_data_value
ALLOCATE(grid%em_a_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5317 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_ph_bt=initial_data_value
ALLOCATE(grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5323 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_b=initial_data_value
ALLOCATE(grid%em_g_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5329 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_t_b=initial_data_value
ALLOCATE(grid%em_a_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5335 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_t_b=initial_data_value
ALLOCATE(grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5341 ,  &
'frame/module_domain.f: Failed to allocate grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_bt=initial_data_value
ALLOCATE(grid%em_g_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5347 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_t_bt=initial_data_value
ALLOCATE(grid%em_a_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5353 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_t_bt=initial_data_value
ALLOCATE(grid%em_mu_b(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5359 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mu_b(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_b=initial_data_value
ALLOCATE(grid%em_g_mu_b(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5365 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mu_b(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mu_b=initial_data_value
ALLOCATE(grid%em_a_mu_b(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5371 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mu_b(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mu_b=initial_data_value
ALLOCATE(grid%em_mu_bt(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5377 ,  &
'frame/module_domain.f: Failed to allocate grid%em_mu_bt(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_bt=initial_data_value
ALLOCATE(grid%em_g_mu_bt(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5383 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_mu_bt(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_mu_bt=initial_data_value
ALLOCATE(grid%em_a_mu_bt(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5389 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_mu_bt(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_mu_bt=initial_data_value
ALLOCATE(grid%em_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5395 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rqv_b=initial_data_value
ALLOCATE(grid%em_g_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5401 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rqv_b=initial_data_value
ALLOCATE(grid%em_a_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5407 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rqv_b=initial_data_value
ALLOCATE(grid%em_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5413 ,  &
'frame/module_domain.f: Failed to allocate grid%em_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rqv_bt=initial_data_value
ALLOCATE(grid%em_g_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5419 ,  &
'frame/module_domain.f: Failed to allocate grid%em_g_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_g_rqv_bt=initial_data_value
ALLOCATE(grid%em_a_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5425 ,  &
'frame/module_domain.f: Failed to allocate grid%em_a_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_a_rqv_bt=initial_data_value
ALLOCATE(grid%rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5431 ,  &
'frame/module_domain.f: Failed to allocate grid%rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqc_b=initial_data_value
ALLOCATE(grid%g_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5437 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqc_b=initial_data_value
ALLOCATE(grid%a_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5443 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqc_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqc_b=initial_data_value
ALLOCATE(grid%rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5449 ,  &
'frame/module_domain.f: Failed to allocate grid%rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqc_bt=initial_data_value
ALLOCATE(grid%g_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5455 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqc_bt=initial_data_value
ALLOCATE(grid%a_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5461 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqc_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqc_bt=initial_data_value
ALLOCATE(grid%rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5467 ,  &
'frame/module_domain.f: Failed to allocate grid%rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqr_b=initial_data_value
ALLOCATE(grid%g_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5473 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqr_b=initial_data_value
ALLOCATE(grid%a_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5479 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqr_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqr_b=initial_data_value
ALLOCATE(grid%rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5485 ,  &
'frame/module_domain.f: Failed to allocate grid%rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqr_bt=initial_data_value
ALLOCATE(grid%g_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5491 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqr_bt=initial_data_value
ALLOCATE(grid%a_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5497 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqr_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqr_bt=initial_data_value
ALLOCATE(grid%rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5503 ,  &
'frame/module_domain.f: Failed to allocate grid%rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqi_b=initial_data_value
ALLOCATE(grid%g_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5509 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqi_b=initial_data_value
ALLOCATE(grid%a_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5515 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqi_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqi_b=initial_data_value
ALLOCATE(grid%rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5521 ,  &
'frame/module_domain.f: Failed to allocate grid%rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqi_bt=initial_data_value
ALLOCATE(grid%g_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5527 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqi_bt=initial_data_value
ALLOCATE(grid%a_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5533 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqi_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqi_bt=initial_data_value
ALLOCATE(grid%rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5539 ,  &
'frame/module_domain.f: Failed to allocate grid%rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqs_b=initial_data_value
ALLOCATE(grid%g_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5545 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqs_b=initial_data_value
ALLOCATE(grid%a_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5551 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqs_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqs_b=initial_data_value
ALLOCATE(grid%rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5557 ,  &
'frame/module_domain.f: Failed to allocate grid%rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqs_bt=initial_data_value
ALLOCATE(grid%g_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5563 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqs_bt=initial_data_value
ALLOCATE(grid%a_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5569 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqs_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqs_bt=initial_data_value
ALLOCATE(grid%rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5575 ,  &
'frame/module_domain.f: Failed to allocate grid%rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqg_b=initial_data_value
ALLOCATE(grid%g_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5581 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqg_b=initial_data_value
ALLOCATE(grid%a_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5587 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqg_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqg_b=initial_data_value
ALLOCATE(grid%rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5593 ,  &
'frame/module_domain.f: Failed to allocate grid%rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqg_bt=initial_data_value
ALLOCATE(grid%g_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5599 ,  &
'frame/module_domain.f: Failed to allocate grid%g_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_rqg_bt=initial_data_value
ALLOCATE(grid%a_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5605 ,  &
'frame/module_domain.f: Failed to allocate grid%a_rqg_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_rqg_bt=initial_data_value
ALLOCATE(grid%fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5611 ,  &
'frame/module_domain.f: Failed to allocate grid%fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fcx=initial_data_value
ALLOCATE(grid%g_fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5617 ,  &
'frame/module_domain.f: Failed to allocate grid%g_fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_fcx=initial_data_value
ALLOCATE(grid%a_fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5623 ,  &
'frame/module_domain.f: Failed to allocate grid%a_fcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_fcx=initial_data_value
ALLOCATE(grid%gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5629 ,  &
'frame/module_domain.f: Failed to allocate grid%gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gcx=initial_data_value
ALLOCATE(grid%g_gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5635 ,  &
'frame/module_domain.f: Failed to allocate grid%g_gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_gcx=initial_data_value
ALLOCATE(grid%a_gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5641 ,  &
'frame/module_domain.f: Failed to allocate grid%a_gcx(model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_gcx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5648 ,  &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000010=initial_data_value
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5654 ,  &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010040=initial_data_value
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5660 ,  &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm040100=initial_data_value
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5666 ,  &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100200=initial_data_value
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5672 ,  &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010200=initial_data_value
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5678 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm000=initial_data_value
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5684 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm005=initial_data_value
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5690 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm020=initial_data_value
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5696 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm040=initial_data_value
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5702 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm160=initial_data_value
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5708 ,  &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm300=initial_data_value
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5714 ,  &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw000010=initial_data_value
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5720 ,  &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010040=initial_data_value
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5726 ,  &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw040100=initial_data_value
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5732 ,  &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw100200=initial_data_value
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5738 ,  &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010200=initial_data_value
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5744 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw000=initial_data_value
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5750 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw005=initial_data_value
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5756 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw020=initial_data_value
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5762 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw040=initial_data_value
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5768 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw160=initial_data_value
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5774 ,  &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw300=initial_data_value
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5780 ,  &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000010=initial_data_value
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5786 ,  &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010040=initial_data_value
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5792 ,  &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st040100=initial_data_value
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5798 ,  &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100200=initial_data_value
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5804 ,  &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010200=initial_data_value
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5810 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt000=initial_data_value
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5816 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt005=initial_data_value
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5822 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt020=initial_data_value
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5828 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt040=initial_data_value
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5834 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt160=initial_data_value
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5840 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5846 ,  &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5852 ,  &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%topostdv=initial_data_value
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5858 ,  &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpx=initial_data_value
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5864 ,  &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpy=initial_data_value
ALLOCATE(grid%shdmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5870 ,  &
'frame/module_domain.f: Failed to allocate grid%shdmax(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmax=initial_data_value
ALLOCATE(grid%shdmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5876 ,  &
'frame/module_domain.f: Failed to allocate grid%shdmin(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmin=initial_data_value
ALLOCATE(grid%snoalb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5882 ,  &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snoalb=initial_data_value
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5888 ,  &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%slopecat=initial_data_value
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5894 ,  &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposoil=initial_data_value
ALLOCATE(grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5900 ,  &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef=initial_data_value
ALLOCATE(grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5906 ,  &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop=initial_data_value
ALLOCATE(grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5912 ,  &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ALLOCATE(grid%soilcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5918 ,  &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcat=initial_data_value
ALLOCATE(grid%vegcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5924 ,  &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ALLOCATE(grid%tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5930 ,  &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
ALLOCATE(grid%zs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5936 ,  &
'frame/module_domain.f: Failed to allocate grid%zs(model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zs=initial_data_value
ALLOCATE(grid%dzs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5942 ,  &
'frame/module_domain.f: Failed to allocate grid%dzs(model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzs=initial_data_value
ALLOCATE(grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5948 ,  &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ALLOCATE(grid%sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5954 ,  &
'frame/module_domain.f: Failed to allocate grid%sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh2o=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5960 ,  &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5966 ,  &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5972 ,  &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5978 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5984 ,  &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5990 ,  &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 5996 ,  &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6002 ,  &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6008 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcevp=initial_data_value
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6014 ,  &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grdflx=initial_data_value
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6020 ,  &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6026 ,  &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6032 ,  &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6038 ,  &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6044 ,  &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6050 ,  &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6056 ,  &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ifndsnowh=0
IF ( setinitval .EQ. 3 ) grid%ifndsoilw=0
ALLOCATE(grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6064 ,  &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smfr3d=initial_data_value
ALLOCATE(grid%keepfr3dflag(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6070 ,  &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%keepfr3dflag=initial_data_value
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6076 ,  &
'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tke_myj=initial_data_value
ALLOCATE(grid%exch_h(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6082 ,  &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_h=initial_data_value
ALLOCATE(grid%ct(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6088 ,  &
'frame/module_domain.f: Failed to allocate grid%ct(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ct=initial_data_value
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6094 ,  &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thz0=initial_data_value
ALLOCATE(grid%z0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6100 ,  &
'frame/module_domain.f: Failed to allocate grid%z0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0=initial_data_value
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6106 ,  &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qz0=initial_data_value
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6112 ,  &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0=initial_data_value
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6118 ,  &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0=initial_data_value
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6124 ,  &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsfc=initial_data_value
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6130 ,  &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs=initial_data_value
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6136 ,  &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms=initial_data_value
ALLOCATE(grid%kpbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6142 ,  &
'frame/module_domain.f: Failed to allocate grid%kpbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%kpbl=0
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6148 ,  &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htop=initial_data_value
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6154 ,  &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbot=initial_data_value
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6160 ,  &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuppt=initial_data_value
ALLOCATE(grid%totswdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6166 ,  &
'frame/module_domain.f: Failed to allocate grid%totswdn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%totswdn=initial_data_value
ALLOCATE(grid%totlwdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6172 ,  &
'frame/module_domain.f: Failed to allocate grid%totlwdn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%totlwdn=initial_data_value
ALLOCATE(grid%rswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6178 ,  &
'frame/module_domain.f: Failed to allocate grid%rswtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtoa=initial_data_value
ALLOCATE(grid%rlwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6184 ,  &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtoa=initial_data_value
ALLOCATE(grid%czmean(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6190 ,  &
'frame/module_domain.f: Failed to allocate grid%czmean(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czmean=initial_data_value
ALLOCATE(grid%cfracl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6196 ,  &
'frame/module_domain.f: Failed to allocate grid%cfracl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracl=initial_data_value
ALLOCATE(grid%cfracm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6202 ,  &
'frame/module_domain.f: Failed to allocate grid%cfracm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracm=initial_data_value
ALLOCATE(grid%cfrach(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6208 ,  &
'frame/module_domain.f: Failed to allocate grid%cfrach(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfrach=initial_data_value
ALLOCATE(grid%acfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6214 ,  &
'frame/module_domain.f: Failed to allocate grid%acfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrst=initial_data_value
ALLOCATE(grid%ncfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6220 ,  &
'frame/module_domain.f: Failed to allocate grid%ncfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrst=0
ALLOCATE(grid%acfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6226 ,  &
'frame/module_domain.f: Failed to allocate grid%acfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrcv=initial_data_value
ALLOCATE(grid%ncfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6232 ,  &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrcv=0
ALLOCATE(grid%aer_dry(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6238 ,  &
'frame/module_domain.f: Failed to allocate grid%aer_dry(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aer_dry=initial_data_value
ALLOCATE(grid%aer_water(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6244 ,  &
'frame/module_domain.f: Failed to allocate grid%aer_water(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aer_water=initial_data_value
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6250 ,  &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6256 ,  &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6262 ,  &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
ALLOCATE(grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6268 ,  &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%h_diabatic=initial_data_value
ALLOCATE(grid%msft(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6274 ,  &
'frame/module_domain.f: Failed to allocate grid%msft(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msft=initial_data_value
ALLOCATE(grid%msfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6280 ,  &
'frame/module_domain.f: Failed to allocate grid%msfu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfu=initial_data_value
ALLOCATE(grid%msfv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6286 ,  &
'frame/module_domain.f: Failed to allocate grid%msfv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfv=initial_data_value
ALLOCATE(grid%f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6292 ,  &
'frame/module_domain.f: Failed to allocate grid%f(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f=initial_data_value
ALLOCATE(grid%e(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6298 ,  &
'frame/module_domain.f: Failed to allocate grid%e(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%e=initial_data_value
ALLOCATE(grid%sina(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6304 ,  &
'frame/module_domain.f: Failed to allocate grid%sina(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sina=initial_data_value
ALLOCATE(grid%cosa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6310 ,  &
'frame/module_domain.f: Failed to allocate grid%cosa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosa=initial_data_value
ALLOCATE(grid%ht(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6316 ,  &
'frame/module_domain.f: Failed to allocate grid%ht(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht=initial_data_value
ALLOCATE(grid%ht_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6322 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_fine=initial_data_value
ALLOCATE(grid%ht_int(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6328 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_int(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_int=initial_data_value
ALLOCATE(grid%ht_input(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6334 ,  &
'frame/module_domain.f: Failed to allocate grid%ht_input(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_input=initial_data_value
ALLOCATE(grid%tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6340 ,  &
'frame/module_domain.f: Failed to allocate grid%tsk(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk=initial_data_value
ALLOCATE(grid%tsk_save(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6346 ,  &
'frame/module_domain.f: Failed to allocate grid%tsk_save(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk_save=initial_data_value
ALLOCATE(grid%u_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6352 ,  &
'frame/module_domain.f: Failed to allocate grid%u_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_base=initial_data_value
ALLOCATE(grid%v_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6358 ,  &
'frame/module_domain.f: Failed to allocate grid%v_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_base=initial_data_value
ALLOCATE(grid%qv_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6364 ,  &
'frame/module_domain.f: Failed to allocate grid%qv_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qv_base=initial_data_value
ALLOCATE(grid%z_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6370 ,  &
'frame/module_domain.f: Failed to allocate grid%z_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z_base=initial_data_value
IF ( setinitval .EQ. 3 ) grid%u_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%v_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%p_top=initial_data_value
IF ( setinitval .EQ. 3 ) grid%imicrogram=0
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_d=initial_data_value
ALLOCATE(grid%rthcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6412 ,  &
'frame/module_domain.f: Failed to allocate grid%rthcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthcuten=initial_data_value
ALLOCATE(grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6418 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvcuten=initial_data_value
ALLOCATE(grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6424 ,  &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqrcuten=initial_data_value
ALLOCATE(grid%rqccuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6430 ,  &
'frame/module_domain.f: Failed to allocate grid%rqccuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqccuten=initial_data_value
ALLOCATE(grid%rqscuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6436 ,  &
'frame/module_domain.f: Failed to allocate grid%rqscuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqscuten=initial_data_value
ALLOCATE(grid%rqicuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6442 ,  &
'frame/module_domain.f: Failed to allocate grid%rqicuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqicuten=initial_data_value
ALLOCATE(grid%w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6448 ,  &
'frame/module_domain.f: Failed to allocate grid%w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w0avg=initial_data_value
ALLOCATE(grid%rainc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6454 ,  &
'frame/module_domain.f: Failed to allocate grid%rainc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainc=initial_data_value
ALLOCATE(grid%rainnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6460 ,  &
'frame/module_domain.f: Failed to allocate grid%rainnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainnc=initial_data_value
ALLOCATE(grid%raincv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6466 ,  &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%raincv=initial_data_value
ALLOCATE(grid%rainncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6472 ,  &
'frame/module_domain.f: Failed to allocate grid%rainncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainncv=initial_data_value
ALLOCATE(grid%rainbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6478 ,  &
'frame/module_domain.f: Failed to allocate grid%rainbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainbl=initial_data_value
ALLOCATE(grid%nca(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6484 ,  &
'frame/module_domain.f: Failed to allocate grid%nca(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nca=initial_data_value
ALLOCATE(grid%lowlyr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6490 ,  &
'frame/module_domain.f: Failed to allocate grid%lowlyr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lowlyr=0
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6496 ,  &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mass_flux=initial_data_value
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6502 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_gr=initial_data_value
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6508 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_w=initial_data_value
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6514 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_mc=initial_data_value
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6520 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_st=initial_data_value
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6526 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_as=initial_data_value
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6532 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capma=initial_data_value
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6538 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capme=initial_data_value
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6544 ,  &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capmi=initial_data_value
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6550 ,  &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xf_ens=initial_data_value
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6556 ,  &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pr_ens=initial_data_value
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6562 ,  &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthften=initial_data_value
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6568 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepcu=0
ALLOCATE(grid%rthraten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6575 ,  &
'frame/module_domain.f: Failed to allocate grid%rthraten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthraten=initial_data_value
ALLOCATE(grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6581 ,  &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratenlw=initial_data_value
ALLOCATE(grid%rthratensw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6587 ,  &
'frame/module_domain.f: Failed to allocate grid%rthratensw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratensw=initial_data_value
ALLOCATE(grid%cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6593 ,  &
'frame/module_domain.f: Failed to allocate grid%cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldfra=initial_data_value
ALLOCATE(grid%swdown(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6599 ,  &
'frame/module_domain.f: Failed to allocate grid%swdown(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdown=initial_data_value
ALLOCATE(grid%gsw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6605 ,  &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ALLOCATE(grid%glw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6611 ,  &
'frame/module_domain.f: Failed to allocate grid%glw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glw=initial_data_value
ALLOCATE(grid%xlat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6617 ,  &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ALLOCATE(grid%xlong(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6623 ,  &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ALLOCATE(grid%albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6629 ,  &
'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6635 ,  &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
ALLOCATE(grid%emiss(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6641 ,  &
'frame/module_domain.f: Failed to allocate grid%emiss(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emiss=initial_data_value
ALLOCATE(grid%cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6647 ,  &
'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepra=0
ALLOCATE(grid%rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6654 ,  &
'frame/module_domain.f: Failed to allocate grid%rublten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rublten=initial_data_value
ALLOCATE(grid%rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6660 ,  &
'frame/module_domain.f: Failed to allocate grid%rvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvblten=initial_data_value
ALLOCATE(grid%rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6666 ,  &
'frame/module_domain.f: Failed to allocate grid%rthblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthblten=initial_data_value
ALLOCATE(grid%rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6672 ,  &
'frame/module_domain.f: Failed to allocate grid%rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvblten=initial_data_value
ALLOCATE(grid%rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6678 ,  &
'frame/module_domain.f: Failed to allocate grid%rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqcblten=initial_data_value
ALLOCATE(grid%rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6684 ,  &
'frame/module_domain.f: Failed to allocate grid%rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqiblten=initial_data_value
ALLOCATE(grid%tmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6690 ,  &
'frame/module_domain.f: Failed to allocate grid%tmn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn=initial_data_value
ALLOCATE(grid%xland(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6696 ,  &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6702 ,  &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
ALLOCATE(grid%ust(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6708 ,  &
'frame/module_domain.f: Failed to allocate grid%ust(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ust=initial_data_value
ALLOCATE(grid%rmol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6714 ,  &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6720 ,  &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
ALLOCATE(grid%pblh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6726 ,  &
'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
ALLOCATE(grid%capg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6732 ,  &
'frame/module_domain.f: Failed to allocate grid%capg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%capg=initial_data_value
ALLOCATE(grid%thc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6738 ,  &
'frame/module_domain.f: Failed to allocate grid%thc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thc=initial_data_value
ALLOCATE(grid%hfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6744 ,  &
'frame/module_domain.f: Failed to allocate grid%hfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hfx=initial_data_value
ALLOCATE(grid%qfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6750 ,  &
'frame/module_domain.f: Failed to allocate grid%qfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qfx=initial_data_value
ALLOCATE(grid%lh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6756 ,  &
'frame/module_domain.f: Failed to allocate grid%lh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh=initial_data_value
ALLOCATE(grid%flhc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6762 ,  &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
ALLOCATE(grid%flqc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6768 ,  &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
ALLOCATE(grid%qsg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6774 ,  &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
ALLOCATE(grid%qvg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6780 ,  &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
ALLOCATE(grid%qcg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6786 ,  &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
ALLOCATE(grid%soilt1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6792 ,  &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
ALLOCATE(grid%tsnav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6798 ,  &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ALLOCATE(grid%snowc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6804 ,  &
'frame/module_domain.f: Failed to allocate grid%snowc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowc=initial_data_value
ALLOCATE(grid%mavail(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6810 ,  &
'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
ALLOCATE(grid%tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6816 ,  &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tkesfcf=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stepbl=0
ALLOCATE(grid%taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6823 ,  &
'frame/module_domain.f: Failed to allocate grid%taucldi(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldi=initial_data_value
ALLOCATE(grid%taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6829 ,  &
'frame/module_domain.f: Failed to allocate grid%taucldc(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldc=initial_data_value
ALLOCATE(grid%defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6835 ,  &
'frame/module_domain.f: Failed to allocate grid%defor11(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor11=initial_data_value
ALLOCATE(grid%defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6841 ,  &
'frame/module_domain.f: Failed to allocate grid%defor22(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor22=initial_data_value
ALLOCATE(grid%defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6847 ,  &
'frame/module_domain.f: Failed to allocate grid%defor12(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor12=initial_data_value
ALLOCATE(grid%defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6853 ,  &
'frame/module_domain.f: Failed to allocate grid%defor33(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor33=initial_data_value
ALLOCATE(grid%defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6859 ,  &
'frame/module_domain.f: Failed to allocate grid%defor13(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor13=initial_data_value
ALLOCATE(grid%defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6865 ,  &
'frame/module_domain.f: Failed to allocate grid%defor23(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor23=initial_data_value
ALLOCATE(grid%xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6871 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmv=initial_data_value
ALLOCATE(grid%xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6877 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmh=initial_data_value
ALLOCATE(grid%xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6883 ,  &
'frame/module_domain.f: Failed to allocate grid%xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmhd=initial_data_value
ALLOCATE(grid%g_xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6889 ,  &
'frame/module_domain.f: Failed to allocate grid%g_xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_xkmhd=initial_data_value
ALLOCATE(grid%a_xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6895 ,  &
'frame/module_domain.f: Failed to allocate grid%a_xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%a_xkmhd=initial_data_value
ALLOCATE(grid%xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6901 ,  &
'frame/module_domain.f: Failed to allocate grid%xkhv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhv=initial_data_value
ALLOCATE(grid%xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6907 ,  &
'frame/module_domain.f: Failed to allocate grid%xkhh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhh=initial_data_value
ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6913 ,  &
'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
ALLOCATE(grid%bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 6919 ,  &
'frame/module_domain.f: Failed to allocate grid%bn2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bn2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%warm_rain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%input_from_hires=.FALSE.
IF ( setinitval .EQ. 3 ) grid%history_interval=0
IF ( setinitval .EQ. 3 ) grid%frames_per_outfile=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%restart=.FALSE.
IF ( setinitval .EQ. 3 ) grid%restart_interval=0
IF ( setinitval .EQ. 3 ) grid%io_form_input=0
IF ( setinitval .EQ. 3 ) grid%io_form_history=0
IF ( setinitval .EQ. 3 ) grid%io_form_restart=0
IF ( setinitval .EQ. 3 ) grid%io_form_boundary=0
IF ( setinitval .EQ. 3 ) grid%debug_level=0
IF ( setinitval .EQ. 3 ) grid%history_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%history_interval_d=0
IF ( setinitval .EQ. 3 ) grid%history_interval_h=0
IF ( setinitval .EQ. 3 ) grid%history_interval_m=0
IF ( setinitval .EQ. 3 ) grid%history_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_d=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_h=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_m=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_s=0
IF ( setinitval .EQ. 3 ) grid%history_begin_y=0
IF ( setinitval .EQ. 3 ) grid%history_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%history_begin_d=0
IF ( setinitval .EQ. 3 ) grid%history_begin_h=0
IF ( setinitval .EQ. 3 ) grid%history_begin_m=0
IF ( setinitval .EQ. 3 ) grid%history_begin_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_y=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_d=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_h=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_m=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_s=0
IF ( setinitval .EQ. 3 ) grid%history_end_y=0
IF ( setinitval .EQ. 3 ) grid%history_end_mo=0
IF ( setinitval .EQ. 3 ) grid%history_end_d=0
IF ( setinitval .EQ. 3 ) grid%history_end_h=0
IF ( setinitval .EQ. 3 ) grid%history_end_m=0
IF ( setinitval .EQ. 3 ) grid%history_end_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_s=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%level=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%coral_dist=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%test_rk_step_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_init_zero_tendency=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_phy_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calculate_km_kh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_tendency=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advect_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_relax_bdy_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_addtend_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdy_dry=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_small_step_prep=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_p_rho=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_coef_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_uv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdyupdate=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_mu_t=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_sumflux=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_advance_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdyupdate_ph=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_mu_uv_1=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_small_step_finish=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_scalar_tend=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_spec_bdy_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_rk_update_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_calc_p_rho_phi=.FALSE.
IF ( setinitval .EQ. 3 ) grid%test_diagnose_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%chem_opt=0
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_full_fields=.FALSE.
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
!ENDOFREGISTRYGENERATEDINCLUDE

      ELSE

        WRITE( wrf_err_message , * )'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1025 ,  TRIM ( wrf_err_message ) )
      ENDIF

      WRITE(message,*)'alloc_space_field: domain ',id,' ',num_bytes_allocated
      CALL  wrf_debug( 1, message )

   END SUBROUTINE alloc_space_field

!

!  This routine is used to DEALLOCATE space for a single domain.  First
!  the pointers in the linked list are fixed (so the one in the middle can
!  be removed).  Second, the field data are all removed through a CALL to 
!  the dealloc_space_domain routine.  Finally, the pointer to the domain
!  itself is DEALLOCATEd.

   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)            :: id

      !  Local data.

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      !  Initializations required to start the routine.

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      !  The identity of the domain to delete is based upon the "id".
      !  We search all of the possible grids.  It is required to find a domain
      !  otherwise it is a fatal error.  

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL dealloc_space_field ( grid )
            DEALLOCATE(grid)
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3 ( "module_domain.b" , 1078 ,  TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain

!

!  This routine DEALLOCATEs each gridded field for this domain.  For each type of
!  different array (1d, 2d, 3d, etc.), the space for each pointer is DEALLOCATEd
!  for every -1 (i.e., each different meteorological field).

   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain)              , POINTER :: grid

      !  Local data.

      INTEGER                             :: dyn_opt, ierr

      CALL nl_get_dyn_opt( 1, dyn_opt )

      IF      ( .FALSE. )           THEN


      ELSE IF ( mod(dyn_opt, 100) == DYN_EM ) THEN

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_deallocs.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7444 ,  &
'frame/module_domain.f: Failed to dallocate grid%lu_index. ')
 endif
DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7449 ,  &
'frame/module_domain.f: Failed to dallocate grid%lu_mask. ')
 endif
DEALLOCATE(grid%em_u_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7454 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_u_1. ')
 endif
DEALLOCATE(grid%em_u_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7459 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_u_2. ')
 endif
DEALLOCATE(grid%em_ru,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7464 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ru. ')
 endif
DEALLOCATE(grid%em_ru_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7469 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ru_m. ')
 endif
DEALLOCATE(grid%em_ru_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7474 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ru_tend. ')
 endif
DEALLOCATE(grid%em_u_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7479 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_u_save. ')
 endif
DEALLOCATE(grid%em_v_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7484 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_v_1. ')
 endif
DEALLOCATE(grid%em_v_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7489 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_v_2. ')
 endif
DEALLOCATE(grid%em_rv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7494 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rv. ')
 endif
DEALLOCATE(grid%em_rv_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7499 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rv_m. ')
 endif
DEALLOCATE(grid%em_rv_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7504 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rv_tend. ')
 endif
DEALLOCATE(grid%em_v_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7509 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_v_save. ')
 endif
DEALLOCATE(grid%em_w_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7514 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_w_1. ')
 endif
DEALLOCATE(grid%em_w_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7519 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_w_2. ')
 endif
DEALLOCATE(grid%em_ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7524 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ww. ')
 endif
DEALLOCATE(grid%em_rw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7529 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rw. ')
 endif
DEALLOCATE(grid%em_ph_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7534 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ph_1. ')
 endif
DEALLOCATE(grid%em_ph_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7539 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ph_2. ')
 endif
DEALLOCATE(grid%em_phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7544 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_phb. ')
 endif
DEALLOCATE(grid%em_phb_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7549 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_phb_fine. ')
 endif
DEALLOCATE(grid%em_ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7554 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ph0. ')
 endif
DEALLOCATE(grid%em_php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7559 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_php. ')
 endif
DEALLOCATE(grid%em_t_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7564 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_1. ')
 endif
DEALLOCATE(grid%em_t_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7569 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_2. ')
 endif
DEALLOCATE(grid%em_t_init,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7574 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_init. ')
 endif
DEALLOCATE(grid%em_tp_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7579 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_tp_1. ')
 endif
DEALLOCATE(grid%em_tp_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7584 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_tp_2. ')
 endif
DEALLOCATE(grid%em_t_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7589 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_save. ')
 endif
DEALLOCATE(grid%em_mu_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7594 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mu_1. ')
 endif
DEALLOCATE(grid%em_mu_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7599 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mu_2. ')
 endif
DEALLOCATE(grid%em_mub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7604 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mub. ')
 endif
DEALLOCATE(grid%em_mub_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7609 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mub_fine. ')
 endif
DEALLOCATE(grid%em_mu0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7614 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mu0. ')
 endif
DEALLOCATE(grid%em_mudf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7619 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mudf. ')
 endif
DEALLOCATE(grid%em_muu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7624 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_muu. ')
 endif
DEALLOCATE(grid%em_muv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7629 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_muv. ')
 endif
DEALLOCATE(grid%em_mut,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7634 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mut. ')
 endif
DEALLOCATE(grid%em_muts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7639 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_muts. ')
 endif
DEALLOCATE(grid%nest_pos,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7644 ,  &
'frame/module_domain.f: Failed to dallocate grid%nest_pos. ')
 endif
DEALLOCATE(grid%em_tke_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7649 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_tke_1. ')
 endif
DEALLOCATE(grid%em_tke_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7654 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_tke_2. ')
 endif
DEALLOCATE(grid%em_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7659 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_p. ')
 endif
DEALLOCATE(grid%em_al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7664 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_al. ')
 endif
DEALLOCATE(grid%em_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7669 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_alt. ')
 endif
DEALLOCATE(grid%em_alb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7674 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_alb. ')
 endif
DEALLOCATE(grid%em_zx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7679 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_zx. ')
 endif
DEALLOCATE(grid%em_zy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7684 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_zy. ')
 endif
DEALLOCATE(grid%em_rdz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7689 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rdz. ')
 endif
DEALLOCATE(grid%em_rdzw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7694 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rdzw. ')
 endif
DEALLOCATE(grid%em_pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7699 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_pb. ')
 endif
DEALLOCATE(grid%em_alpha,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7704 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_alpha. ')
 endif
DEALLOCATE(grid%em_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7709 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_a. ')
 endif
DEALLOCATE(grid%em_gamma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7714 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_gamma. ')
 endif
DEALLOCATE(grid%em_c2a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7719 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_c2a. ')
 endif
DEALLOCATE(grid%em_rho,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7724 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rho. ')
 endif
DEALLOCATE(grid%em_phm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7729 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_phm. ')
 endif
DEALLOCATE(grid%em_cqu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7734 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_cqu. ')
 endif
DEALLOCATE(grid%em_cqv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7739 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_cqv. ')
 endif
DEALLOCATE(grid%em_cqw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7744 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_cqw. ')
 endif
DEALLOCATE(grid%em_pm1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7749 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_pm1. ')
 endif
DEALLOCATE(grid%em_fnm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7754 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_fnm. ')
 endif
DEALLOCATE(grid%em_fnp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7759 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_fnp. ')
 endif
DEALLOCATE(grid%em_rdnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7764 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rdnw. ')
 endif
DEALLOCATE(grid%em_rdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7769 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rdn. ')
 endif
DEALLOCATE(grid%em_dnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7774 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_dnw. ')
 endif
DEALLOCATE(grid%em_dn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7779 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_dn. ')
 endif
DEALLOCATE(grid%em_znu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7784 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_znu. ')
 endif
DEALLOCATE(grid%em_znw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7789 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_znw. ')
 endif
DEALLOCATE(grid%em_t_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7794 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_base. ')
 endif
DEALLOCATE(grid%em_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7799 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_z. ')
 endif
DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7804 ,  &
'frame/module_domain.f: Failed to dallocate grid%q2. ')
 endif
DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7809 ,  &
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7814 ,  &
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7819 ,  &
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7824 ,  &
'frame/module_domain.f: Failed to dallocate grid%u10. ')
 endif
DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7829 ,  &
'frame/module_domain.f: Failed to dallocate grid%v10. ')
 endif
DEALLOCATE(grid%dpsdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7834 ,  &
'frame/module_domain.f: Failed to dallocate grid%dpsdt. ')
 endif
DEALLOCATE(grid%dmudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7839 ,  &
'frame/module_domain.f: Failed to dallocate grid%dmudt. ')
 endif
DEALLOCATE(grid%psfcm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7844 ,  &
'frame/module_domain.f: Failed to dallocate grid%psfcm. ')
 endif
DEALLOCATE(grid%q_conve_3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7849 ,  &
'frame/module_domain.f: Failed to dallocate grid%q_conve_3d. ')
 endif
DEALLOCATE(grid%q_conve_2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7854 ,  &
'frame/module_domain.f: Failed to dallocate grid%q_conve_2d. ')
 endif
DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7859 ,  &
'frame/module_domain.f: Failed to dallocate grid%imask_nostag. ')
 endif
DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7864 ,  &
'frame/module_domain.f: Failed to dallocate grid%imask_xstag. ')
 endif
DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7869 ,  &
'frame/module_domain.f: Failed to dallocate grid%imask_ystag. ')
 endif
DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7874 ,  &
'frame/module_domain.f: Failed to dallocate grid%imask_xystag. ')
 endif
DEALLOCATE(grid%moist_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7879 ,  &
'frame/module_domain.f: Failed to dallocate grid%moist_1. ')
 endif
DEALLOCATE(grid%moist_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7884 ,  &
'frame/module_domain.f: Failed to dallocate grid%moist_2. ')
 endif
DEALLOCATE(grid%chem_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7889 ,  &
'frame/module_domain.f: Failed to dallocate grid%chem_1. ')
 endif
DEALLOCATE(grid%chem_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7894 ,  &
'frame/module_domain.f: Failed to dallocate grid%chem_2. ')
 endif
DEALLOCATE(grid%em_u_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7899 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_u_b. ')
 endif
DEALLOCATE(grid%em_u_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7904 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_u_bt. ')
 endif
DEALLOCATE(grid%em_v_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7909 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_v_b. ')
 endif
DEALLOCATE(grid%em_v_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7914 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_v_bt. ')
 endif
DEALLOCATE(grid%em_w_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7919 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_w_b. ')
 endif
DEALLOCATE(grid%em_w_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7924 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_w_bt. ')
 endif
DEALLOCATE(grid%em_ph_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7929 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ph_b. ')
 endif
DEALLOCATE(grid%em_ph_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7934 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_ph_bt. ')
 endif
DEALLOCATE(grid%em_t_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7939 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_b. ')
 endif
DEALLOCATE(grid%em_t_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7944 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_t_bt. ')
 endif
DEALLOCATE(grid%em_mu_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7949 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mu_b. ')
 endif
DEALLOCATE(grid%em_mu_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7954 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_mu_bt. ')
 endif
DEALLOCATE(grid%em_rqv_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7959 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rqv_b. ')
 endif
DEALLOCATE(grid%em_rqv_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7964 ,  &
'frame/module_domain.f: Failed to dallocate grid%em_rqv_bt. ')
 endif
DEALLOCATE(grid%rqc_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7969 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqc_b. ')
 endif
DEALLOCATE(grid%rqc_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7974 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqc_bt. ')
 endif
DEALLOCATE(grid%rqr_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7979 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqr_b. ')
 endif
DEALLOCATE(grid%rqr_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7984 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqr_bt. ')
 endif
DEALLOCATE(grid%rqi_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7989 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqi_b. ')
 endif
DEALLOCATE(grid%rqi_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7994 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqi_bt. ')
 endif
DEALLOCATE(grid%rqs_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 7999 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqs_b. ')
 endif
DEALLOCATE(grid%rqs_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8004 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqs_bt. ')
 endif
DEALLOCATE(grid%rqg_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8009 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqg_b. ')
 endif
DEALLOCATE(grid%rqg_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8014 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqg_bt. ')
 endif
DEALLOCATE(grid%fcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8019 ,  &
'frame/module_domain.f: Failed to dallocate grid%fcx. ')
 endif
DEALLOCATE(grid%gcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8024 ,  &
'frame/module_domain.f: Failed to dallocate grid%gcx. ')
 endif
DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8029 ,  &
'frame/module_domain.f: Failed to dallocate grid%sm000010. ')
 endif
DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8034 ,  &
'frame/module_domain.f: Failed to dallocate grid%sm010040. ')
 endif
DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8039 ,  &
'frame/module_domain.f: Failed to dallocate grid%sm040100. ')
 endif
DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8044 ,  &
'frame/module_domain.f: Failed to dallocate grid%sm100200. ')
 endif
DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8049 ,  &
'frame/module_domain.f: Failed to dallocate grid%sm010200. ')
 endif
DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8054 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm000. ')
 endif
DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8059 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm005. ')
 endif
DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8064 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm020. ')
 endif
DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8069 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm040. ')
 endif
DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8074 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm160. ')
 endif
DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8079 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilm300. ')
 endif
DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8084 ,  &
'frame/module_domain.f: Failed to dallocate grid%sw000010. ')
 endif
DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8089 ,  &
'frame/module_domain.f: Failed to dallocate grid%sw010040. ')
 endif
DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8094 ,  &
'frame/module_domain.f: Failed to dallocate grid%sw040100. ')
 endif
DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8099 ,  &
'frame/module_domain.f: Failed to dallocate grid%sw100200. ')
 endif
DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8104 ,  &
'frame/module_domain.f: Failed to dallocate grid%sw010200. ')
 endif
DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8109 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw000. ')
 endif
DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8114 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw005. ')
 endif
DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8119 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw020. ')
 endif
DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8124 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw040. ')
 endif
DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8129 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw160. ')
 endif
DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8134 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilw300. ')
 endif
DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8139 ,  &
'frame/module_domain.f: Failed to dallocate grid%st000010. ')
 endif
DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8144 ,  &
'frame/module_domain.f: Failed to dallocate grid%st010040. ')
 endif
DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8149 ,  &
'frame/module_domain.f: Failed to dallocate grid%st040100. ')
 endif
DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8154 ,  &
'frame/module_domain.f: Failed to dallocate grid%st100200. ')
 endif
DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8159 ,  &
'frame/module_domain.f: Failed to dallocate grid%st010200. ')
 endif
DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8164 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt000. ')
 endif
DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8169 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt005. ')
 endif
DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8174 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt020. ')
 endif
DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8179 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt040. ')
 endif
DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8184 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt160. ')
 endif
DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8189 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt300. ')
 endif
DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8194 ,  &
'frame/module_domain.f: Failed to dallocate grid%landmask. ')
 endif
DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8199 ,  &
'frame/module_domain.f: Failed to dallocate grid%topostdv. ')
 endif
DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8204 ,  &
'frame/module_domain.f: Failed to dallocate grid%toposlpx. ')
 endif
DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8209 ,  &
'frame/module_domain.f: Failed to dallocate grid%toposlpy. ')
 endif
DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8214 ,  &
'frame/module_domain.f: Failed to dallocate grid%shdmax. ')
 endif
DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8219 ,  &
'frame/module_domain.f: Failed to dallocate grid%shdmin. ')
 endif
DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8224 ,  &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8229 ,  &
'frame/module_domain.f: Failed to dallocate grid%slopecat. ')
 endif
DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8234 ,  &
'frame/module_domain.f: Failed to dallocate grid%toposoil. ')
 endif
DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8239 ,  &
'frame/module_domain.f: Failed to dallocate grid%landusef. ')
 endif
DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8244 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilctop. ')
 endif
DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8249 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilcbot. ')
 endif
DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8254 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8259 ,  &
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8264 ,  &
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
DEALLOCATE(grid%zs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8269 ,  &
'frame/module_domain.f: Failed to dallocate grid%zs. ')
 endif
DEALLOCATE(grid%dzs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8274 ,  &
'frame/module_domain.f: Failed to dallocate grid%dzs. ')
 endif
DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8279 ,  &
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8284 ,  &
'frame/module_domain.f: Failed to dallocate grid%sh2o. ')
 endif
DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8289 ,  &
'frame/module_domain.f: Failed to dallocate grid%xice. ')
 endif
DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8294 ,  &
'frame/module_domain.f: Failed to dallocate grid%smstav. ')
 endif
DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8299 ,  &
'frame/module_domain.f: Failed to dallocate grid%smstot. ')
 endif
DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8304 ,  &
'frame/module_domain.f: Failed to dallocate grid%sfcrunoff. ')
 endif
DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8309 ,  &
'frame/module_domain.f: Failed to dallocate grid%udrunoff. ')
 endif
DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8314 ,  &
'frame/module_domain.f: Failed to dallocate grid%ivgtyp. ')
 endif
DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8319 ,  &
'frame/module_domain.f: Failed to dallocate grid%isltyp. ')
 endif
DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8324 ,  &
'frame/module_domain.f: Failed to dallocate grid%vegfra. ')
 endif
DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8329 ,  &
'frame/module_domain.f: Failed to dallocate grid%sfcevp. ')
 endif
DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8334 ,  &
'frame/module_domain.f: Failed to dallocate grid%grdflx. ')
 endif
DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8339 ,  &
'frame/module_domain.f: Failed to dallocate grid%sfcexc. ')
 endif
DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8344 ,  &
'frame/module_domain.f: Failed to dallocate grid%acsnow. ')
 endif
DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8349 ,  &
'frame/module_domain.f: Failed to dallocate grid%acsnom. ')
 endif
DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8354 ,  &
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8359 ,  &
'frame/module_domain.f: Failed to dallocate grid%snowh. ')
 endif
DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8364 ,  &
'frame/module_domain.f: Failed to dallocate grid%canwat. ')
 endif
DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8369 ,  &
'frame/module_domain.f: Failed to dallocate grid%sst. ')
 endif
DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8374 ,  &
'frame/module_domain.f: Failed to dallocate grid%smfr3d. ')
 endif
DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8379 ,  &
'frame/module_domain.f: Failed to dallocate grid%keepfr3dflag. ')
 endif
DEALLOCATE(grid%tke_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8384 ,  &
'frame/module_domain.f: Failed to dallocate grid%tke_myj. ')
 endif
DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8389 ,  &
'frame/module_domain.f: Failed to dallocate grid%exch_h. ')
 endif
DEALLOCATE(grid%ct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8394 ,  &
'frame/module_domain.f: Failed to dallocate grid%ct. ')
 endif
DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8399 ,  &
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8404 ,  &
'frame/module_domain.f: Failed to dallocate grid%z0. ')
 endif
DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8409 ,  &
'frame/module_domain.f: Failed to dallocate grid%qz0. ')
 endif
DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8414 ,  &
'frame/module_domain.f: Failed to dallocate grid%uz0. ')
 endif
DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8419 ,  &
'frame/module_domain.f: Failed to dallocate grid%vz0. ')
 endif
DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8424 ,  &
'frame/module_domain.f: Failed to dallocate grid%qsfc. ')
 endif
DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8429 ,  &
'frame/module_domain.f: Failed to dallocate grid%akhs. ')
 endif
DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8434 ,  &
'frame/module_domain.f: Failed to dallocate grid%akms. ')
 endif
DEALLOCATE(grid%kpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8439 ,  &
'frame/module_domain.f: Failed to dallocate grid%kpbl. ')
 endif
DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8444 ,  &
'frame/module_domain.f: Failed to dallocate grid%htop. ')
 endif
DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8449 ,  &
'frame/module_domain.f: Failed to dallocate grid%hbot. ')
 endif
DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8454 ,  &
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
DEALLOCATE(grid%totswdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8459 ,  &
'frame/module_domain.f: Failed to dallocate grid%totswdn. ')
 endif
DEALLOCATE(grid%totlwdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8464 ,  &
'frame/module_domain.f: Failed to dallocate grid%totlwdn. ')
 endif
DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8469 ,  &
'frame/module_domain.f: Failed to dallocate grid%rswtoa. ')
 endif
DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8474 ,  &
'frame/module_domain.f: Failed to dallocate grid%rlwtoa. ')
 endif
DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8479 ,  &
'frame/module_domain.f: Failed to dallocate grid%czmean. ')
 endif
DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8484 ,  &
'frame/module_domain.f: Failed to dallocate grid%cfracl. ')
 endif
DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8489 ,  &
'frame/module_domain.f: Failed to dallocate grid%cfracm. ')
 endif
DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8494 ,  &
'frame/module_domain.f: Failed to dallocate grid%cfrach. ')
 endif
DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8499 ,  &
'frame/module_domain.f: Failed to dallocate grid%acfrst. ')
 endif
DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8504 ,  &
'frame/module_domain.f: Failed to dallocate grid%ncfrst. ')
 endif
DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8509 ,  &
'frame/module_domain.f: Failed to dallocate grid%acfrcv. ')
 endif
DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8514 ,  &
'frame/module_domain.f: Failed to dallocate grid%ncfrcv. ')
 endif
DEALLOCATE(grid%aer_dry,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8519 ,  &
'frame/module_domain.f: Failed to dallocate grid%aer_dry. ')
 endif
DEALLOCATE(grid%aer_water,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8524 ,  &
'frame/module_domain.f: Failed to dallocate grid%aer_water. ')
 endif
DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8529 ,  &
'frame/module_domain.f: Failed to dallocate grid%f_ice_phy. ')
 endif
DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8534 ,  &
'frame/module_domain.f: Failed to dallocate grid%f_rain_phy. ')
 endif
DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8539 ,  &
'frame/module_domain.f: Failed to dallocate grid%f_rimef_phy. ')
 endif
DEALLOCATE(grid%h_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8544 ,  &
'frame/module_domain.f: Failed to dallocate grid%h_diabatic. ')
 endif
DEALLOCATE(grid%msft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8549 ,  &
'frame/module_domain.f: Failed to dallocate grid%msft. ')
 endif
DEALLOCATE(grid%msfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8554 ,  &
'frame/module_domain.f: Failed to dallocate grid%msfu. ')
 endif
DEALLOCATE(grid%msfv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8559 ,  &
'frame/module_domain.f: Failed to dallocate grid%msfv. ')
 endif
DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8564 ,  &
'frame/module_domain.f: Failed to dallocate grid%f. ')
 endif
DEALLOCATE(grid%e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8569 ,  &
'frame/module_domain.f: Failed to dallocate grid%e. ')
 endif
DEALLOCATE(grid%sina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8574 ,  &
'frame/module_domain.f: Failed to dallocate grid%sina. ')
 endif
DEALLOCATE(grid%cosa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8579 ,  &
'frame/module_domain.f: Failed to dallocate grid%cosa. ')
 endif
DEALLOCATE(grid%ht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8584 ,  &
'frame/module_domain.f: Failed to dallocate grid%ht. ')
 endif
DEALLOCATE(grid%ht_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8589 ,  &
'frame/module_domain.f: Failed to dallocate grid%ht_fine. ')
 endif
DEALLOCATE(grid%ht_int,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8594 ,  &
'frame/module_domain.f: Failed to dallocate grid%ht_int. ')
 endif
DEALLOCATE(grid%ht_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8599 ,  &
'frame/module_domain.f: Failed to dallocate grid%ht_input. ')
 endif
DEALLOCATE(grid%tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8604 ,  &
'frame/module_domain.f: Failed to dallocate grid%tsk. ')
 endif
DEALLOCATE(grid%tsk_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8609 ,  &
'frame/module_domain.f: Failed to dallocate grid%tsk_save. ')
 endif
DEALLOCATE(grid%u_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8614 ,  &
'frame/module_domain.f: Failed to dallocate grid%u_base. ')
 endif
DEALLOCATE(grid%v_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8619 ,  &
'frame/module_domain.f: Failed to dallocate grid%v_base. ')
 endif
DEALLOCATE(grid%qv_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8624 ,  &
'frame/module_domain.f: Failed to dallocate grid%qv_base. ')
 endif
DEALLOCATE(grid%z_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8629 ,  &
'frame/module_domain.f: Failed to dallocate grid%z_base. ')
 endif
DEALLOCATE(grid%rthcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8634 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthcuten. ')
 endif
DEALLOCATE(grid%rqvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8639 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqvcuten. ')
 endif
DEALLOCATE(grid%rqrcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8644 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqrcuten. ')
 endif
DEALLOCATE(grid%rqccuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8649 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqccuten. ')
 endif
DEALLOCATE(grid%rqscuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8654 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqscuten. ')
 endif
DEALLOCATE(grid%rqicuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8659 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqicuten. ')
 endif
DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8664 ,  &
'frame/module_domain.f: Failed to dallocate grid%w0avg. ')
 endif
DEALLOCATE(grid%rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8669 ,  &
'frame/module_domain.f: Failed to dallocate grid%rainc. ')
 endif
DEALLOCATE(grid%rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8674 ,  &
'frame/module_domain.f: Failed to dallocate grid%rainnc. ')
 endif
DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8679 ,  &
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
DEALLOCATE(grid%rainncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8684 ,  &
'frame/module_domain.f: Failed to dallocate grid%rainncv. ')
 endif
DEALLOCATE(grid%rainbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8689 ,  &
'frame/module_domain.f: Failed to dallocate grid%rainbl. ')
 endif
DEALLOCATE(grid%nca,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8694 ,  &
'frame/module_domain.f: Failed to dallocate grid%nca. ')
 endif
DEALLOCATE(grid%lowlyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8699 ,  &
'frame/module_domain.f: Failed to dallocate grid%lowlyr. ')
 endif
DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8704 ,  &
'frame/module_domain.f: Failed to dallocate grid%mass_flux. ')
 endif
DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8709 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_gr. ')
 endif
DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8714 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_w. ')
 endif
DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8719 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_mc. ')
 endif
DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8724 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_st. ')
 endif
DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8729 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_as. ')
 endif
DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8734 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_capma. ')
 endif
DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8739 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_capme. ')
 endif
DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8744 ,  &
'frame/module_domain.f: Failed to dallocate grid%apr_capmi. ')
 endif
DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8749 ,  &
'frame/module_domain.f: Failed to dallocate grid%xf_ens. ')
 endif
DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8754 ,  &
'frame/module_domain.f: Failed to dallocate grid%pr_ens. ')
 endif
DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8759 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthften. ')
 endif
DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8764 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqvften. ')
 endif
DEALLOCATE(grid%rthraten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8769 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthraten. ')
 endif
DEALLOCATE(grid%rthratenlw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8774 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthratenlw. ')
 endif
DEALLOCATE(grid%rthratensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8779 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthratensw. ')
 endif
DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8784 ,  &
'frame/module_domain.f: Failed to dallocate grid%cldfra. ')
 endif
DEALLOCATE(grid%swdown,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8789 ,  &
'frame/module_domain.f: Failed to dallocate grid%swdown. ')
 endif
DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8794 ,  &
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
DEALLOCATE(grid%glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8799 ,  &
'frame/module_domain.f: Failed to dallocate grid%glw. ')
 endif
DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8804 ,  &
'frame/module_domain.f: Failed to dallocate grid%xlat. ')
 endif
DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8809 ,  &
'frame/module_domain.f: Failed to dallocate grid%xlong. ')
 endif
DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8814 ,  &
'frame/module_domain.f: Failed to dallocate grid%albedo. ')
 endif
DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8819 ,  &
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
DEALLOCATE(grid%emiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8824 ,  &
'frame/module_domain.f: Failed to dallocate grid%emiss. ')
 endif
DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8829 ,  &
'frame/module_domain.f: Failed to dallocate grid%cldefi. ')
 endif
DEALLOCATE(grid%rublten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8834 ,  &
'frame/module_domain.f: Failed to dallocate grid%rublten. ')
 endif
DEALLOCATE(grid%rvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8839 ,  &
'frame/module_domain.f: Failed to dallocate grid%rvblten. ')
 endif
DEALLOCATE(grid%rthblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8844 ,  &
'frame/module_domain.f: Failed to dallocate grid%rthblten. ')
 endif
DEALLOCATE(grid%rqvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8849 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqvblten. ')
 endif
DEALLOCATE(grid%rqcblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8854 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqcblten. ')
 endif
DEALLOCATE(grid%rqiblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8859 ,  &
'frame/module_domain.f: Failed to dallocate grid%rqiblten. ')
 endif
DEALLOCATE(grid%tmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8864 ,  &
'frame/module_domain.f: Failed to dallocate grid%tmn. ')
 endif
DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8869 ,  &
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8874 ,  &
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
DEALLOCATE(grid%ust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8879 ,  &
'frame/module_domain.f: Failed to dallocate grid%ust. ')
 endif
DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8884 ,  &
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8889 ,  &
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8894 ,  &
'frame/module_domain.f: Failed to dallocate grid%pblh. ')
 endif
DEALLOCATE(grid%capg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8899 ,  &
'frame/module_domain.f: Failed to dallocate grid%capg. ')
 endif
DEALLOCATE(grid%thc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8904 ,  &
'frame/module_domain.f: Failed to dallocate grid%thc. ')
 endif
DEALLOCATE(grid%hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8909 ,  &
'frame/module_domain.f: Failed to dallocate grid%hfx. ')
 endif
DEALLOCATE(grid%qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8914 ,  &
'frame/module_domain.f: Failed to dallocate grid%qfx. ')
 endif
DEALLOCATE(grid%lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8919 ,  &
'frame/module_domain.f: Failed to dallocate grid%lh. ')
 endif
DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8924 ,  &
'frame/module_domain.f: Failed to dallocate grid%flhc. ')
 endif
DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8929 ,  &
'frame/module_domain.f: Failed to dallocate grid%flqc. ')
 endif
DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8934 ,  &
'frame/module_domain.f: Failed to dallocate grid%qsg. ')
 endif
DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8939 ,  &
'frame/module_domain.f: Failed to dallocate grid%qvg. ')
 endif
DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8944 ,  &
'frame/module_domain.f: Failed to dallocate grid%qcg. ')
 endif
DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8949 ,  &
'frame/module_domain.f: Failed to dallocate grid%soilt1. ')
 endif
DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8954 ,  &
'frame/module_domain.f: Failed to dallocate grid%tsnav. ')
 endif
DEALLOCATE(grid%snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8959 ,  &
'frame/module_domain.f: Failed to dallocate grid%snowc. ')
 endif
DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8964 ,  &
'frame/module_domain.f: Failed to dallocate grid%mavail. ')
 endif
DEALLOCATE(grid%tkesfcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8969 ,  &
'frame/module_domain.f: Failed to dallocate grid%tkesfcf. ')
 endif
DEALLOCATE(grid%taucldi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8974 ,  &
'frame/module_domain.f: Failed to dallocate grid%taucldi. ')
 endif
DEALLOCATE(grid%taucldc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8979 ,  &
'frame/module_domain.f: Failed to dallocate grid%taucldc. ')
 endif
DEALLOCATE(grid%defor11,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8984 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor11. ')
 endif
DEALLOCATE(grid%defor22,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8989 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor22. ')
 endif
DEALLOCATE(grid%defor12,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8994 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor12. ')
 endif
DEALLOCATE(grid%defor33,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 8999 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor33. ')
 endif
DEALLOCATE(grid%defor13,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9004 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor13. ')
 endif
DEALLOCATE(grid%defor23,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9009 ,  &
'frame/module_domain.f: Failed to dallocate grid%defor23. ')
 endif
DEALLOCATE(grid%xkmv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9014 ,  &
'frame/module_domain.f: Failed to dallocate grid%xkmv. ')
 endif
DEALLOCATE(grid%xkmh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9019 ,  &
'frame/module_domain.f: Failed to dallocate grid%xkmh. ')
 endif
DEALLOCATE(grid%xkmhd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9024 ,  &
'frame/module_domain.f: Failed to dallocate grid%xkmhd. ')
 endif
DEALLOCATE(grid%xkhv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9029 ,  &
'frame/module_domain.f: Failed to dallocate grid%xkhv. ')
 endif
DEALLOCATE(grid%xkhh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9034 ,  &
'frame/module_domain.f: Failed to dallocate grid%xkhh. ')
 endif
DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9039 ,  &
'frame/module_domain.f: Failed to dallocate grid%div. ')
 endif
DEALLOCATE(grid%bn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3 ( "module_domain.b" , 9044 ,  &
'frame/module_domain.f: Failed to dallocate grid%bn2. ')
 endif
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE
        WRITE( wrf_err_message , * )'dealloc_space_field: Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1123 ,  TRIM ( wrf_err_message ) )
      ENDIF

   END SUBROUTINE dealloc_space_field

!
!
   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid
! <DESCRIPTION>
! This is a recursive subroutine that traverses the domain hierarchy rooted
! at the input argument <em>in_grid</em>, a pointer to TYPE(domain), and returns
! a pointer to the domain matching the integer argument <em>id</em> if it exists.
!
! </DESCRIPTION>
      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      IF ( ASSOCIATED( in_grid ) ) THEN
      IF ( in_grid%id .EQ. id ) THEN
         result_grid => in_grid
      ELSE
         grid_ptr => in_grid
         DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
            DO kid = 1, max_nests
               IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                  CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                  IF ( ASSOCIATED( result_grid ) ) THEN
                    IF ( result_grid%id .EQ. id ) found = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
         ENDDO
      ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      !  Output data.

      INTEGER                             :: loc

!<DESCRIPTION>
!  This routine is used to find a specific domain identifier in an array
!  of domain identifiers.
!
!</DESCRIPTION>
      
      !  Local data.

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer
!
   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain

END MODULE module_domain
