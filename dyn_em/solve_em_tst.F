!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em_tst ( grid , config_flags , &
! Actual arguments generated from Registry

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_dummy_args.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
lu_index,lu_mask,u_1,u_2,g_u_1,g_u_2,a_u_1,a_u_2,ru,g_ru,a_ru,ru_m,g_ru_m,a_ru_m,ru_tend,g_ru_tend,a_ru_tend,u_save,g_u_save, &
a_u_save,v_1,v_2,g_v_1,g_v_2,a_v_1,a_v_2,rv,g_rv,a_rv,rv_m,g_rv_m,a_rv_m,rv_tend,g_rv_tend,a_rv_tend,v_save,g_v_save,a_v_save, &
w_1,w_2,g_w_1,g_w_2,a_w_1,a_w_2,ww,g_ww,a_ww,rw,g_rw,a_rw,ph_1,ph_2,g_ph_1,g_ph_2,a_ph_1,a_ph_2,phb,g_phb,a_phb,phb_fine,ph0, &
php,g_php,a_php,t_1,t_2,g_t_1,g_t_2,a_t_1,a_t_2,t_init,tp_1,tp_2,t_save,g_t_save,a_t_save,mu_1,mu_2,g_mu_1,g_mu_2,a_mu_1,a_mu_2, &
mub,g_mub,a_mub,mub_fine,mu0,mudf,g_mudf,a_mudf,muu,g_muu,a_muu,muv,g_muv,a_muv,mut,g_mut,a_mut,muts,g_muts,a_muts,nest_pos, &
tke_1,tke_2,p,g_p,a_p,al,g_al,a_al,alt,g_alt,a_alt,alb,zx,zy,rdz,rdzw,pb,alpha,g_alpha,a_alpha,a,g_a,a_a,gamma,g_gamma,a_gamma, &
c2a,g_c2a,a_c2a,rho,g_rho,a_rho,phm,g_phm,a_phm,cqu,g_cqu,a_cqu,cqv,g_cqv,a_cqv,cqw,g_cqw,a_cqw,pm1,g_pm1,a_pm1,fnm,fnp,rdnw, &
rdn,dnw,dn,znu,znw,t_base,z,g_z,a_z,q2,t2,th2,psfc,u10,v10,dpsdt,dmudt,psfcm,q_conve_3d,q_conve_2d,imask_nostag,imask_xstag, &
imask_ystag,imask_xystag,moist_1,moist_2,g_moist_1,g_moist_2,a_moist_1,a_moist_2,chem_1,chem_2,u_b,g_u_b,a_u_b,u_bt,g_u_bt, &
a_u_bt,v_b,g_v_b,a_v_b,v_bt,g_v_bt,a_v_bt,w_b,g_w_b,a_w_b,w_bt,g_w_bt,a_w_bt,ph_b,g_ph_b,a_ph_b,ph_bt,g_ph_bt,a_ph_bt,t_b,g_t_b, &
a_t_b,t_bt,g_t_bt,a_t_bt,mu_b,g_mu_b,a_mu_b,mu_bt,g_mu_bt,a_mu_bt,rqv_b,g_rqv_b,a_rqv_b,rqv_bt,g_rqv_bt,a_rqv_bt,rqc_b,g_rqc_b, &
a_rqc_b,rqc_bt,g_rqc_bt,a_rqc_bt,rqr_b,g_rqr_b,a_rqr_b,rqr_bt,g_rqr_bt,a_rqr_bt,rqi_b,g_rqi_b,a_rqi_b,rqi_bt,g_rqi_bt,a_rqi_bt, &
rqs_b,g_rqs_b,a_rqs_b,rqs_bt,g_rqs_bt,a_rqs_bt,rqg_b,g_rqg_b,a_rqg_b,rqg_bt,g_rqg_bt,a_rqg_bt,fcx,g_fcx,a_fcx,gcx,g_gcx,a_gcx, &
sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300,sw000010,sw010040,sw040100, &
sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100,st100200,st010200,soilt000, &
soilt005,soilt020,soilt040,soilt160,soilt300,landmask,topostdv,toposlpx,toposlpy,shdmax,shdmin,snoalb,slopecat,toposoil, &
landusef,soilctop,soilcbot,soilcat,vegcat,tslb,zs,dzs,smois,sh2o,xice,smstav,smstot,sfcrunoff,udrunoff,ivgtyp,isltyp,vegfra, &
sfcevp,grdflx,sfcexc,acsnow,acsnom,snow,snowh,canwat,sst,smfr3d,keepfr3dflag,tke_myj,exch_h,ct,thz0,z0,qz0,uz0,vz0,qsfc,akhs, &
akms,kpbl,htop,hbot,cuppt,totswdn,totlwdn,rswtoa,rlwtoa,czmean,cfracl,cfracm,cfrach,acfrst,ncfrst,acfrcv,ncfrcv,aer_dry, &
aer_water,f_ice_phy,f_rain_phy,f_rimef_phy,h_diabatic,msft,msfu,msfv,f,e,sina,cosa,ht,ht_fine,ht_int,ht_input,tsk,tsk_save, &
u_base,v_base,qv_base,z_base,rthcuten,rqvcuten,rqrcuten,rqccuten,rqscuten,rqicuten,w0avg,rainc,rainnc,raincv,rainncv,rainbl,nca, &
lowlyr,mass_flux,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,apr_capme,apr_capmi,xf_ens,pr_ens,rthften,rqvften,rthraten, &
rthratenlw,rthratensw,cldfra,swdown,gsw,glw,xlat,xlong,albedo,albbck,emiss,cldefi,rublten,rvblten,rthblten,rqvblten,rqcblten, &
rqiblten,tmn,xland,znt,ust,rmol,mol,pblh,capg,thc,hfx,qfx,lh,flhc,flqc,qsg,qvg,qcg,soilt1,tsnav,snowc,mavail,tkesfcf,taucldi, &
taucldc,defor11,defor22,defor12,defor33,defor13,defor23,xkmv,xkmh,xkmhd,g_xkmhd,a_xkmhd,xkhv,xkhh,div,bn2 &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                    )

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants
   USE module_small_step_em
   USE module_em
   USE module_big_step_utilities_em
   USE module_bc
   USE module_bc_em
   USE module_solvedebug_em
   USE module_physics_addtendc
   USE module_diffusion_em
! Registry generated module
   USE module_state_description
   USE module_radiation_driver
   USE module_surface_driver
   USE module_cumulus_driver
   USE module_microphysics_driver
   USE module_pbl_driver




   USE module_check

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_dummy_decl.inc
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
real                                     :: lat_ll_t
real                                     :: lat_ul_t
real                                     :: lat_ur_t
real                                     :: lat_lr_t
real                                     :: lat_ll_u
real                                     :: lat_ul_u
real                                     :: lat_ur_u
real                                     :: lat_lr_u
real                                     :: lat_ll_v
real                                     :: lat_ul_v
real                                     :: lat_ur_v
real                                     :: lat_lr_v
real                                     :: lat_ll_d
real                                     :: lat_ul_d
real                                     :: lat_ur_d
real                                     :: lat_lr_d
real                                     :: lon_ll_t
real                                     :: lon_ul_t
real                                     :: lon_ur_t
real                                     :: lon_lr_t
real                                     :: lon_ll_u
real                                     :: lon_ul_u
real                                     :: lon_ur_u
real                                     :: lon_lr_u
real                                     :: lon_ll_v
real                                     :: lon_ul_v
real                                     :: lon_ur_v
real                                     :: lon_lr_v
real                                     :: lon_ll_d
real                                     :: lon_ul_d
real                                     :: lon_ur_d
real                                     :: lon_lr_d
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lu_index
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lu_mask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phb_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_init
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tp_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tp_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mub_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: nest_pos
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rdz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rdzw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rho
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rho
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pm1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_pm1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_pm1
real      ,DIMENSION(grid%sm32:grid%em32)           :: fnm
real      ,DIMENSION(grid%sm32:grid%em32)           :: fnp
real      ,DIMENSION(grid%sm32:grid%em32)           :: rdnw
real      ,DIMENSION(grid%sm32:grid%em32)           :: rdn
real      ,DIMENSION(grid%sm32:grid%em32)           :: dnw
real      ,DIMENSION(grid%sm32:grid%em32)           :: dn
real      ,DIMENSION(grid%sm32:grid%em32)           :: znu
real      ,DIMENSION(grid%sm32:grid%em32)           :: znw
real      ,DIMENSION(grid%sm32:grid%em32)           :: t_base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: t2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: u10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: v10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: dpsdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: dmudt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psfcm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q_conve_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q_conve_2d
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: imask_nostag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: imask_xstag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: imask_ystag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: imask_xystag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: g_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: g_moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: a_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: a_moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: g_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: a_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: g_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: a_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: rqg_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: g_rqg_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: a_rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: g_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: a_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: gcx
real      ,DIMENSION(grid%spec_bdy_width)           :: g_gcx
real      ,DIMENSION(grid%spec_bdy_width)           :: a_gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: landmask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: topostdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snoalb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: slopecat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposoil
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_land_cat,grid%sm33:grid%em33)           :: landusef
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilctop
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilcbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: tslb
real      ,DIMENSION(grid%num_soil_layers)           :: zs
real      ,DIMENSION(grid%num_soil_layers)           :: dzs
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smois
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: sh2o
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcrunoff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: udrunoff
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ivgtyp
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: isltyp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: grdflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcexc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smfr3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: keepfr3dflag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: exch_h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: thz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: uz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akhs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akms
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: kpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: htop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cuppt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totswdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totlwdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rlwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: czmean
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfrach
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrst
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrcv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: aer_dry
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: aer_water
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_ice_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rain_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rimef_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: h_diabatic
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msfu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: msfv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: f
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: e
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sina
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cosa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_fine
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_int
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_input
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tsk_save
real      ,DIMENSION(grid%sm32:grid%em32)           :: u_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: v_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: qv_base
real      ,DIMENSION(grid%sm32:grid%em32)           :: z_base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqrcuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqccuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqscuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqicuten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w0avg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainnc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainncv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rainbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: nca
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lowlyr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mass_flux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_gr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_mc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_st
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_as
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capme
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capmi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: xf_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: pr_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthraten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthratenlw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthratensw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cldfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: swdown
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gsw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: glw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xlat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xlong
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albedo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albbck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: emiss
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cldefi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rublten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rvblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqcblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqiblten
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tmn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xland
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ust
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rmol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pblh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: capg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: thc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hfx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qfx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: flhc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: flqc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qsg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qvg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qcg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tsnav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snowc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mavail
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tkesfcf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: taucldi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: taucldc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor11
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor22
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor12
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor33
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor13
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: defor23
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkmhd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_xkmhd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_xkmhd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkhv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: xkhh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: div
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: bn2


!ENDOFREGISTRYGENERATEDINCLUDE

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: itmpstep
   INTEGER                         :: sz

! storage for tendencies and decoupled state (generated from Registry)

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_i1_decl.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ww_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ww_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: wwp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: g_mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: a_mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: mu_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z_at_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_z_at_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_z_at_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psim
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psih
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wspd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gz1oz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: br
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_u_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_u_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_v_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_v_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: g_rho_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rho_phy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cu_act_flag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: regime
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_tend
!ENDOFREGISTRYGENERATEDINCLUDE

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk

! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

! Define benchmarking timers if -DBENCH is compiled



!----------------------
! Executable statements
!----------------------

! Trick problematic compilers into not performing copy-in/copy-out by adding
! indices to array arguments in the CALL statements in this routine.
! It has the effect of passing only the first element of the array, rather 
! than the entire array.  See:  
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm



! Limit the number of arguments if compiled with -DLIMIT_ARGS by copying 
! scalar (non-array) arguments out of the grid data structure into locally
! defined copies (defined in em_dummy_decl.inc, above, as they are if they
! are arguments). An equivalent include of em_scalar_derefs.inc appears
! at the end of the routine to copy back any chnaged non-array values.
! The definition of COPY_IN or COPY_OUT before the include defines the
! direction of the copy.  Em_scalar_derefs.inc is generated from Registry


!STARTOFREGISTRYGENERATEDINCLUDE inc/em_scalar_derefs.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS


 cfn = grid%cfn
 cfn1 = grid%cfn1
 epsts = grid%epsts
 step_number = grid%step_number
 rdx = grid%rdx
 rdy = grid%rdy
 dts = grid%dts
 dtseps = grid%dtseps
 resm = grid%resm
 zetatop = grid%zetatop
 cf1 = grid%cf1
 cf2 = grid%cf2
 cf3 = grid%cf3
 number_at_same_level = grid%number_at_same_level
 itimestep = grid%itimestep
 avg_abs_dpsdt = grid%avg_abs_dpsdt
 avg_abs_dmudt = grid%avg_abs_dmudt
 oid = grid%oid
 auxhist1_oid = grid%auxhist1_oid
 auxhist2_oid = grid%auxhist2_oid
 auxhist3_oid = grid%auxhist3_oid
 auxhist4_oid = grid%auxhist4_oid
 auxhist5_oid = grid%auxhist5_oid
 auxinput1_oid = grid%auxinput1_oid
 auxinput2_oid = grid%auxinput2_oid
 auxinput3_oid = grid%auxinput3_oid
 auxinput4_oid = grid%auxinput4_oid
 auxinput5_oid = grid%auxinput5_oid
 lbc_fid = grid%lbc_fid
 tiled = grid%tiled
 patched = grid%patched
 dtbc = grid%dtbc
 ifndsnowh = grid%ifndsnowh
 ifndsoilw = grid%ifndsoilw
 u_frame = grid%u_frame
 v_frame = grid%v_frame
 p_top = grid%p_top
 imicrogram = grid%imicrogram
 lat_ll_t = grid%em_lat_ll_t
 lat_ul_t = grid%em_lat_ul_t
 lat_ur_t = grid%em_lat_ur_t
 lat_lr_t = grid%em_lat_lr_t
 lat_ll_u = grid%em_lat_ll_u
 lat_ul_u = grid%em_lat_ul_u
 lat_ur_u = grid%em_lat_ur_u
 lat_lr_u = grid%em_lat_lr_u
 lat_ll_v = grid%em_lat_ll_v
 lat_ul_v = grid%em_lat_ul_v
 lat_ur_v = grid%em_lat_ur_v
 lat_lr_v = grid%em_lat_lr_v
 lat_ll_d = grid%em_lat_ll_d
 lat_ul_d = grid%em_lat_ul_d
 lat_ur_d = grid%em_lat_ur_d
 lat_lr_d = grid%em_lat_lr_d
 lon_ll_t = grid%em_lon_ll_t
 lon_ul_t = grid%em_lon_ul_t
 lon_ur_t = grid%em_lon_ur_t
 lon_lr_t = grid%em_lon_lr_t
 lon_ll_u = grid%em_lon_ll_u
 lon_ul_u = grid%em_lon_ul_u
 lon_ur_u = grid%em_lon_ur_u
 lon_lr_u = grid%em_lon_lr_u
 lon_ll_v = grid%em_lon_ll_v
 lon_ul_v = grid%em_lon_ul_v
 lon_ur_v = grid%em_lon_ur_v
 lon_lr_v = grid%em_lon_lr_v
 lon_ll_d = grid%em_lon_ll_d
 lon_ul_d = grid%em_lon_ul_d
 lon_ur_d = grid%em_lon_ur_d
 lon_lr_d = grid%em_lon_lr_d
 stepcu = grid%stepcu
 stepra = grid%stepra
 stepbl = grid%stepbl
 warm_rain = grid%warm_rain
 moved = grid%moved
 run_days = grid%run_days
 run_hours = grid%run_hours
 run_minutes = grid%run_minutes
 run_seconds = grid%run_seconds
 start_year = grid%start_year
 start_month = grid%start_month
 start_day = grid%start_day
 start_hour = grid%start_hour
 start_minute = grid%start_minute
 start_second = grid%start_second
 end_year = grid%end_year
 end_month = grid%end_month
 end_day = grid%end_day
 end_hour = grid%end_hour
 end_minute = grid%end_minute
 end_second = grid%end_second
 interval_seconds = grid%interval_seconds
 input_from_file = grid%input_from_file
 input_from_hires = grid%input_from_hires
 history_interval = grid%history_interval
 frames_per_outfile = grid%frames_per_outfile
 frames_per_auxhist1 = grid%frames_per_auxhist1
 frames_per_auxhist2 = grid%frames_per_auxhist2
 frames_per_auxhist3 = grid%frames_per_auxhist3
 frames_per_auxhist4 = grid%frames_per_auxhist4
 frames_per_auxhist5 = grid%frames_per_auxhist5
 restart = grid%restart
 restart_interval = grid%restart_interval
 io_form_input = grid%io_form_input
 io_form_history = grid%io_form_history
 io_form_restart = grid%io_form_restart
 io_form_boundary = grid%io_form_boundary
 debug_level = grid%debug_level
 history_outname = grid%history_outname
 auxhist1_outname = grid%auxhist1_outname
 auxhist2_outname = grid%auxhist2_outname
 auxhist3_outname = grid%auxhist3_outname
 auxhist4_outname = grid%auxhist4_outname
 auxhist5_outname = grid%auxhist5_outname
 history_inname = grid%history_inname
 auxhist1_inname = grid%auxhist1_inname
 auxhist2_inname = grid%auxhist2_inname
 auxhist3_inname = grid%auxhist3_inname
 auxhist4_inname = grid%auxhist4_inname
 auxhist5_inname = grid%auxhist5_inname
 auxinput1_outname = grid%auxinput1_outname
 auxinput2_outname = grid%auxinput2_outname
 auxinput3_outname = grid%auxinput3_outname
 auxinput4_outname = grid%auxinput4_outname
 auxinput5_outname = grid%auxinput5_outname
 auxinput1_inname = grid%auxinput1_inname
 auxinput2_inname = grid%auxinput2_inname
 auxinput3_inname = grid%auxinput3_inname
 auxinput4_inname = grid%auxinput4_inname
 auxinput5_inname = grid%auxinput5_inname
 history_interval_mo = grid%history_interval_mo
 history_interval_d = grid%history_interval_d
 history_interval_h = grid%history_interval_h
 history_interval_m = grid%history_interval_m
 history_interval_s = grid%history_interval_s
 inputout_interval_mo = grid%inputout_interval_mo
 inputout_interval_d = grid%inputout_interval_d
 inputout_interval_h = grid%inputout_interval_h
 inputout_interval_m = grid%inputout_interval_m
 inputout_interval_s = grid%inputout_interval_s
 inputout_interval = grid%inputout_interval
 auxhist1_interval_mo = grid%auxhist1_interval_mo
 auxhist1_interval_d = grid%auxhist1_interval_d
 auxhist1_interval_h = grid%auxhist1_interval_h
 auxhist1_interval_m = grid%auxhist1_interval_m
 auxhist1_interval_s = grid%auxhist1_interval_s
 auxhist1_interval = grid%auxhist1_interval
 auxhist2_interval_mo = grid%auxhist2_interval_mo
 auxhist2_interval_d = grid%auxhist2_interval_d
 auxhist2_interval_h = grid%auxhist2_interval_h
 auxhist2_interval_m = grid%auxhist2_interval_m
 auxhist2_interval_s = grid%auxhist2_interval_s
 auxhist2_interval = grid%auxhist2_interval
 auxhist3_interval_mo = grid%auxhist3_interval_mo
 auxhist3_interval_d = grid%auxhist3_interval_d
 auxhist3_interval_h = grid%auxhist3_interval_h
 auxhist3_interval_m = grid%auxhist3_interval_m
 auxhist3_interval_s = grid%auxhist3_interval_s
 auxhist3_interval = grid%auxhist3_interval
 auxhist4_interval_mo = grid%auxhist4_interval_mo
 auxhist4_interval_d = grid%auxhist4_interval_d
 auxhist4_interval_h = grid%auxhist4_interval_h
 auxhist4_interval_m = grid%auxhist4_interval_m
 auxhist4_interval_s = grid%auxhist4_interval_s
 auxhist4_interval = grid%auxhist4_interval
 auxhist5_interval_mo = grid%auxhist5_interval_mo
 auxhist5_interval_d = grid%auxhist5_interval_d
 auxhist5_interval_h = grid%auxhist5_interval_h
 auxhist5_interval_m = grid%auxhist5_interval_m
 auxhist5_interval_s = grid%auxhist5_interval_s
 auxhist5_interval = grid%auxhist5_interval
 auxinput1_interval_mo = grid%auxinput1_interval_mo
 auxinput1_interval_d = grid%auxinput1_interval_d
 auxinput1_interval_h = grid%auxinput1_interval_h
 auxinput1_interval_m = grid%auxinput1_interval_m
 auxinput1_interval_s = grid%auxinput1_interval_s
 auxinput1_interval = grid%auxinput1_interval
 auxinput2_interval_mo = grid%auxinput2_interval_mo
 auxinput2_interval_d = grid%auxinput2_interval_d
 auxinput2_interval_h = grid%auxinput2_interval_h
 auxinput2_interval_m = grid%auxinput2_interval_m
 auxinput2_interval_s = grid%auxinput2_interval_s
 auxinput2_interval = grid%auxinput2_interval
 auxinput3_interval_mo = grid%auxinput3_interval_mo
 auxinput3_interval_d = grid%auxinput3_interval_d
 auxinput3_interval_h = grid%auxinput3_interval_h
 auxinput3_interval_m = grid%auxinput3_interval_m
 auxinput3_interval_s = grid%auxinput3_interval_s
 auxinput3_interval = grid%auxinput3_interval
 auxinput4_interval_mo = grid%auxinput4_interval_mo
 auxinput4_interval_d = grid%auxinput4_interval_d
 auxinput4_interval_h = grid%auxinput4_interval_h
 auxinput4_interval_m = grid%auxinput4_interval_m
 auxinput4_interval_s = grid%auxinput4_interval_s
 auxinput4_interval = grid%auxinput4_interval
 auxinput5_interval_mo = grid%auxinput5_interval_mo
 auxinput5_interval_d = grid%auxinput5_interval_d
 auxinput5_interval_h = grid%auxinput5_interval_h
 auxinput5_interval_m = grid%auxinput5_interval_m
 auxinput5_interval_s = grid%auxinput5_interval_s
 auxinput5_interval = grid%auxinput5_interval
 restart_interval_mo = grid%restart_interval_mo
 restart_interval_d = grid%restart_interval_d
 restart_interval_h = grid%restart_interval_h
 restart_interval_m = grid%restart_interval_m
 restart_interval_s = grid%restart_interval_s
 history_begin_y = grid%history_begin_y
 history_begin_mo = grid%history_begin_mo
 history_begin_d = grid%history_begin_d
 history_begin_h = grid%history_begin_h
 history_begin_m = grid%history_begin_m
 history_begin_s = grid%history_begin_s
 inputout_begin_y = grid%inputout_begin_y
 inputout_begin_mo = grid%inputout_begin_mo
 inputout_begin_d = grid%inputout_begin_d
 inputout_begin_h = grid%inputout_begin_h
 inputout_begin_m = grid%inputout_begin_m
 inputout_begin_s = grid%inputout_begin_s
 auxhist1_begin_y = grid%auxhist1_begin_y
 auxhist1_begin_mo = grid%auxhist1_begin_mo
 auxhist1_begin_d = grid%auxhist1_begin_d
 auxhist1_begin_h = grid%auxhist1_begin_h
 auxhist1_begin_m = grid%auxhist1_begin_m
 auxhist1_begin_s = grid%auxhist1_begin_s
 auxhist2_begin_y = grid%auxhist2_begin_y
 auxhist2_begin_mo = grid%auxhist2_begin_mo
 auxhist2_begin_d = grid%auxhist2_begin_d
 auxhist2_begin_h = grid%auxhist2_begin_h
 auxhist2_begin_m = grid%auxhist2_begin_m
 auxhist2_begin_s = grid%auxhist2_begin_s
 auxhist3_begin_y = grid%auxhist3_begin_y
 auxhist3_begin_mo = grid%auxhist3_begin_mo
 auxhist3_begin_d = grid%auxhist3_begin_d
 auxhist3_begin_h = grid%auxhist3_begin_h
 auxhist3_begin_m = grid%auxhist3_begin_m
 auxhist3_begin_s = grid%auxhist3_begin_s
 auxhist4_begin_y = grid%auxhist4_begin_y
 auxhist4_begin_mo = grid%auxhist4_begin_mo
 auxhist4_begin_d = grid%auxhist4_begin_d
 auxhist4_begin_h = grid%auxhist4_begin_h
 auxhist4_begin_m = grid%auxhist4_begin_m
 auxhist4_begin_s = grid%auxhist4_begin_s
 auxhist5_begin_y = grid%auxhist5_begin_y
 auxhist5_begin_mo = grid%auxhist5_begin_mo
 auxhist5_begin_d = grid%auxhist5_begin_d
 auxhist5_begin_h = grid%auxhist5_begin_h
 auxhist5_begin_m = grid%auxhist5_begin_m
 auxhist5_begin_s = grid%auxhist5_begin_s
 auxinput1_begin_y = grid%auxinput1_begin_y
 auxinput1_begin_mo = grid%auxinput1_begin_mo
 auxinput1_begin_d = grid%auxinput1_begin_d
 auxinput1_begin_h = grid%auxinput1_begin_h
 auxinput1_begin_m = grid%auxinput1_begin_m
 auxinput1_begin_s = grid%auxinput1_begin_s
 auxinput2_begin_y = grid%auxinput2_begin_y
 auxinput2_begin_mo = grid%auxinput2_begin_mo
 auxinput2_begin_d = grid%auxinput2_begin_d
 auxinput2_begin_h = grid%auxinput2_begin_h
 auxinput2_begin_m = grid%auxinput2_begin_m
 auxinput2_begin_s = grid%auxinput2_begin_s
 auxinput3_begin_y = grid%auxinput3_begin_y
 auxinput3_begin_mo = grid%auxinput3_begin_mo
 auxinput3_begin_d = grid%auxinput3_begin_d
 auxinput3_begin_h = grid%auxinput3_begin_h
 auxinput3_begin_m = grid%auxinput3_begin_m
 auxinput3_begin_s = grid%auxinput3_begin_s
 auxinput4_begin_y = grid%auxinput4_begin_y
 auxinput4_begin_mo = grid%auxinput4_begin_mo
 auxinput4_begin_d = grid%auxinput4_begin_d
 auxinput4_begin_h = grid%auxinput4_begin_h
 auxinput4_begin_m = grid%auxinput4_begin_m
 auxinput4_begin_s = grid%auxinput4_begin_s
 auxinput5_begin_y = grid%auxinput5_begin_y
 auxinput5_begin_mo = grid%auxinput5_begin_mo
 auxinput5_begin_d = grid%auxinput5_begin_d
 auxinput5_begin_h = grid%auxinput5_begin_h
 auxinput5_begin_m = grid%auxinput5_begin_m
 auxinput5_begin_s = grid%auxinput5_begin_s
 restart_begin_y = grid%restart_begin_y
 restart_begin_mo = grid%restart_begin_mo
 restart_begin_d = grid%restart_begin_d
 restart_begin_h = grid%restart_begin_h
 restart_begin_m = grid%restart_begin_m
 restart_begin_s = grid%restart_begin_s
 history_end_y = grid%history_end_y
 history_end_mo = grid%history_end_mo
 history_end_d = grid%history_end_d
 history_end_h = grid%history_end_h
 history_end_m = grid%history_end_m
 history_end_s = grid%history_end_s
 inputout_end_y = grid%inputout_end_y
 inputout_end_mo = grid%inputout_end_mo
 inputout_end_d = grid%inputout_end_d
 inputout_end_h = grid%inputout_end_h
 inputout_end_m = grid%inputout_end_m
 inputout_end_s = grid%inputout_end_s
 auxhist1_end_y = grid%auxhist1_end_y
 auxhist1_end_mo = grid%auxhist1_end_mo
 auxhist1_end_d = grid%auxhist1_end_d
 auxhist1_end_h = grid%auxhist1_end_h
 auxhist1_end_m = grid%auxhist1_end_m
 auxhist1_end_s = grid%auxhist1_end_s
 auxhist2_end_y = grid%auxhist2_end_y
 auxhist2_end_mo = grid%auxhist2_end_mo
 auxhist2_end_d = grid%auxhist2_end_d
 auxhist2_end_h = grid%auxhist2_end_h
 auxhist2_end_m = grid%auxhist2_end_m
 auxhist2_end_s = grid%auxhist2_end_s
 auxhist3_end_y = grid%auxhist3_end_y
 auxhist3_end_mo = grid%auxhist3_end_mo
 auxhist3_end_d = grid%auxhist3_end_d
 auxhist3_end_h = grid%auxhist3_end_h
 auxhist3_end_m = grid%auxhist3_end_m
 auxhist3_end_s = grid%auxhist3_end_s
 auxhist4_end_y = grid%auxhist4_end_y
 auxhist4_end_mo = grid%auxhist4_end_mo
 auxhist4_end_d = grid%auxhist4_end_d
 auxhist4_end_h = grid%auxhist4_end_h
 auxhist4_end_m = grid%auxhist4_end_m
 auxhist4_end_s = grid%auxhist4_end_s
 auxhist5_end_y = grid%auxhist5_end_y
 auxhist5_end_mo = grid%auxhist5_end_mo
 auxhist5_end_d = grid%auxhist5_end_d
 auxhist5_end_h = grid%auxhist5_end_h
 auxhist5_end_m = grid%auxhist5_end_m
 auxhist5_end_s = grid%auxhist5_end_s
 auxinput1_end_y = grid%auxinput1_end_y
 auxinput1_end_mo = grid%auxinput1_end_mo
 auxinput1_end_d = grid%auxinput1_end_d
 auxinput1_end_h = grid%auxinput1_end_h
 auxinput1_end_m = grid%auxinput1_end_m
 auxinput1_end_s = grid%auxinput1_end_s
 auxinput2_end_y = grid%auxinput2_end_y
 auxinput2_end_mo = grid%auxinput2_end_mo
 auxinput2_end_d = grid%auxinput2_end_d
 auxinput2_end_h = grid%auxinput2_end_h
 auxinput2_end_m = grid%auxinput2_end_m
 auxinput2_end_s = grid%auxinput2_end_s
 auxinput3_end_y = grid%auxinput3_end_y
 auxinput3_end_mo = grid%auxinput3_end_mo
 auxinput3_end_d = grid%auxinput3_end_d
 auxinput3_end_h = grid%auxinput3_end_h
 auxinput3_end_m = grid%auxinput3_end_m
 auxinput3_end_s = grid%auxinput3_end_s
 auxinput4_end_y = grid%auxinput4_end_y
 auxinput4_end_mo = grid%auxinput4_end_mo
 auxinput4_end_d = grid%auxinput4_end_d
 auxinput4_end_h = grid%auxinput4_end_h
 auxinput4_end_m = grid%auxinput4_end_m
 auxinput4_end_s = grid%auxinput4_end_s
 auxinput5_end_y = grid%auxinput5_end_y
 auxinput5_end_mo = grid%auxinput5_end_mo
 auxinput5_end_d = grid%auxinput5_end_d
 auxinput5_end_h = grid%auxinput5_end_h
 auxinput5_end_m = grid%auxinput5_end_m
 auxinput5_end_s = grid%auxinput5_end_s
 io_form_auxinput1 = grid%io_form_auxinput1
 io_form_auxinput2 = grid%io_form_auxinput2
 io_form_auxinput3 = grid%io_form_auxinput3
 io_form_auxinput4 = grid%io_form_auxinput4
 io_form_auxinput5 = grid%io_form_auxinput5
 io_form_auxhist1 = grid%io_form_auxhist1
 io_form_auxhist2 = grid%io_form_auxhist2
 io_form_auxhist3 = grid%io_form_auxhist3
 io_form_auxhist4 = grid%io_form_auxhist4
 io_form_auxhist5 = grid%io_form_auxhist5
 julyr = grid%julyr
 julday = grid%julday
 gmt = grid%gmt
 input_inname = grid%input_inname
 input_outname = grid%input_outname
 bdy_inname = grid%bdy_inname
 bdy_outname = grid%bdy_outname
 rst_inname = grid%rst_inname
 rst_outname = grid%rst_outname
 write_input = grid%write_input
 write_restart_at_0h = grid%write_restart_at_0h
 time_step = grid%time_step
 time_step_fract_num = grid%time_step_fract_num
 time_step_fract_den = grid%time_step_fract_den
 max_dom = grid%max_dom
 s_we = grid%s_we
 e_we = grid%e_we
 s_sn = grid%s_sn
 e_sn = grid%e_sn
 s_vert = grid%s_vert
 e_vert = grid%e_vert
 dx = grid%dx
 dy = grid%dy
 grid_id = grid%grid_id
 parent_id = grid%parent_id
 level = grid%level
 i_parent_start = grid%i_parent_start
 j_parent_start = grid%j_parent_start
 parent_grid_ratio = grid%parent_grid_ratio
 parent_time_step_ratio = grid%parent_time_step_ratio
 feedback = grid%feedback
 smooth_option = grid%smooth_option
 ztop = grid%ztop
 moad_grid_ratio = grid%moad_grid_ratio
 moad_time_step_ratio = grid%moad_time_step_ratio
 shw = grid%shw
 coral_dist = grid%coral_dist
 tile_sz_x = grid%tile_sz_x
 tile_sz_y = grid%tile_sz_y
 numtiles = grid%numtiles
 nproc_x = grid%nproc_x
 nproc_y = grid%nproc_y
 irand = grid%irand
 dt = grid%dt
 num_moves = grid%num_moves
 move_id = grid%move_id
 move_interval = grid%move_interval
 move_cd_x = grid%move_cd_x
 move_cd_y = grid%move_cd_y
 test_rk_step_prep = grid%test_rk_step_prep
 test_init_zero_tendency = grid%test_init_zero_tendency
 test_phy_prep = grid%test_phy_prep
 test_calculate_km_kh = grid%test_calculate_km_kh
 test_rk_tendency = grid%test_rk_tendency
 test_advect_scalar = grid%test_advect_scalar
 test_relax_bdy_dry = grid%test_relax_bdy_dry
 test_rk_addtend_dry = grid%test_rk_addtend_dry
 test_spec_bdy_dry = grid%test_spec_bdy_dry
 test_small_step_prep = grid%test_small_step_prep
 test_calc_p_rho = grid%test_calc_p_rho
 test_calc_coef_w = grid%test_calc_coef_w
 test_advance_uv = grid%test_advance_uv
 test_spec_bdyupdate = grid%test_spec_bdyupdate
 test_advance_mu_t = grid%test_advance_mu_t
 test_sumflux = grid%test_sumflux
 test_advance_w = grid%test_advance_w
 test_spec_bdyupdate_ph = grid%test_spec_bdyupdate_ph
 test_calc_mu_uv_1 = grid%test_calc_mu_uv_1
 test_small_step_finish = grid%test_small_step_finish
 test_rk_scalar_tend = grid%test_rk_scalar_tend
 test_spec_bdy_scalar = grid%test_spec_bdy_scalar
 test_rk_update_scalar = grid%test_rk_update_scalar
 test_calc_p_rho_phi = grid%test_calc_p_rho_phi
 test_diagnose_w = grid%test_diagnose_w
 mp_physics = grid%mp_physics
 ra_lw_physics = grid%ra_lw_physics
 ra_sw_physics = grid%ra_sw_physics
 radt = grid%radt
 sf_sfclay_physics = grid%sf_sfclay_physics
 sf_surface_physics = grid%sf_surface_physics
 bl_pbl_physics = grid%bl_pbl_physics
 bldt = grid%bldt
 cu_physics = grid%cu_physics
 cudt = grid%cudt
 gsmdt = grid%gsmdt
 isfflx = grid%isfflx
 ifsnow = grid%ifsnow
 icloud = grid%icloud
 surface_input_source = grid%surface_input_source
 num_soil_layers = grid%num_soil_layers
 maxiens = grid%maxiens
 maxens = grid%maxens
 maxens2 = grid%maxens2
 maxens3 = grid%maxens3
 ensdim = grid%ensdim
 chem_opt = grid%chem_opt
 num_land_cat = grid%num_land_cat
 num_soil_cat = grid%num_soil_cat
 mp_zero_out = grid%mp_zero_out
 mp_zero_out_thresh = grid%mp_zero_out_thresh
 seaice_threshold = grid%seaice_threshold
 dyn_opt = grid%dyn_opt
 rk_ord = grid%rk_ord
 w_damping = grid%w_damping
 diff_opt = grid%diff_opt
 km_opt = grid%km_opt
 damp_opt = grid%damp_opt
 zdamp = grid%zdamp
 dampcoef = grid%dampcoef
 khdif = grid%khdif
 kvdif = grid%kvdif
 smdiv = grid%smdiv
 emdiv = grid%emdiv
 epssm = grid%epssm
 non_hydrostatic = grid%non_hydrostatic
 time_step_sound = grid%time_step_sound
 h_mom_adv_order = grid%h_mom_adv_order
 v_mom_adv_order = grid%v_mom_adv_order
 h_sca_adv_order = grid%h_sca_adv_order
 v_sca_adv_order = grid%v_sca_adv_order
 top_radiation = grid%top_radiation
 mix_cr_len = grid%mix_cr_len
 tke_upper_bound = grid%tke_upper_bound
 kh_tke_upper_bound = grid%kh_tke_upper_bound
 kv_tke_upper_bound = grid%kv_tke_upper_bound
 tke_drag_coefficient = grid%tke_drag_coefficient
 tke_heat_flux = grid%tke_heat_flux
 pert_coriolis = grid%pert_coriolis
 mix_full_fields = grid%mix_full_fields
 base_pres = grid%base_pres
 base_temp = grid%base_temp
 base_lapse = grid%base_lapse
 spec_bdy_width = grid%spec_bdy_width
 spec_zone = grid%spec_zone
 relax_zone = grid%relax_zone
 specified = grid%specified
 periodic_x = grid%periodic_x
 symmetric_xs = grid%symmetric_xs
 symmetric_xe = grid%symmetric_xe
 open_xs = grid%open_xs
 open_xe = grid%open_xe
 periodic_y = grid%periodic_y
 symmetric_ys = grid%symmetric_ys
 symmetric_ye = grid%symmetric_ye
 open_ys = grid%open_ys
 open_ye = grid%open_ye
 nested = grid%nested
 real_data_init_type = grid%real_data_init_type
 cen_lat = grid%cen_lat
 cen_lon = grid%cen_lon
 truelat1 = grid%truelat1
 truelat2 = grid%truelat2
 moad_cen_lat = grid%moad_cen_lat
 stand_lon = grid%stand_lon
 bdyfrq = grid%bdyfrq
 iswater = grid%iswater
 isice = grid%isice
 isurban = grid%isurban
 isoilwater = grid%isoilwater
 map_proj = grid%map_proj

! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE

! Needed by some comm layers, e.g. RSL. If needed, nmm_data_calls.inc is
! generated from the registry.  The definition of REGISTER_I1 allows
! I1 data to be communicated in this routine if necessary.





!<DESCRIPTION>
!<pre>
! solve_em is the main driver for advancing a grid a single timestep.
! It is a mediation-layer routine -> DM and SM calls are made where 
! needed for parallel processing.  
!
! solve_em can integrate the equations using 3 time-integration methods
!      
!    - 3rd order Runge-Kutta time integration (recommended)
!      
!    - 2nd order Runge-Kutta time integration
!      
!    - Leapfrog time integration
!      (note: the leapfrog scheme is not correctly implemented
!      for most of the physics)
!
! The main sections of solve_em are
!     
! (1) Runge-Kutta (RK) loop
!     
! (2) Non-timesplit physics (i.e., tendencies computed for updating
!     model state variables during the first RK sub-step (loop)
!     
! (3) Small (acoustic, sound) timestep loop - within the RK sub-steps
!     
! (4) Scalar advance for moist and chem scalar variables (and TKE)
!     within the RK sub-steps.
!     
! (5) time-split physics (after the RK step), currently this includes
!     only microphyics
!
! A more detailed description of these sections follows.
!</pre>
!</DESCRIPTION>

! Initialize timers if compiled with -DBENCH




















































!  set leapfrog or runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  Obtain dimension information stored in the grid data structure.
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  k_start         = kps
  k_end           = kpe

  ijds = min(ids, jds)
  ijde = max(ide, jde)

  num_3d_m        = num_moist
  num_3d_c        = num_chem

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  itimestep = itimestep + 1

!**********************************************************************
!
!  LET US BEGIN.......
!
!<DESCRIPTION>
!<pre>
! (1) RK integration loop is named the "Runge_Kutta_loop:"
!
!   Predictor-corrector type time integration.
!   Advection terms are evaluated at time t for the predictor step,
!   and advection is re-evaluated with the latest predicted value for
!   each succeeding time corrector step
!
!   2nd order Runge Kutta (rk_order = 2):
!   Step 1 is taken to the midpoint predictor, step 2 is the full step.
!
!   3rd order Runge Kutta (rk_order = 3):
!   Step 1 is taken to from t to dt/3, step 2 is from t to dt/2,
!   and step 3 is from t to dt.
!
!   non-timesplit physics are evaluated during first RK step and
!   these physics tendencies are stored for use in each RK pass.
!</pre>
!</DESCRIPTION>
!**********************************************************************


 rk_order = config_flags%rk_ord
 leapfrog = .false.

 IF (time_step_sound == 0) THEN
! auto-set option
! This function will give 4 for 6*dx and 6 for 10*dx and returns even numbers only
   time_step_sound = max ( 2 * ( INT (300.*dt/dx-0.01) + 1 ), 4 )
   WRITE(wrf_err_message,*)'dx, dt, time_step_sound=',dx,dt,time_step_sound
   CALL wrf_debug ( 50 , wrf_err_message )
 ENDIF

 dts = dt/float(time_step_sound)

 IF(rk_ord == 1) leapfrog = .true.

 Runge_Kutta_loop:  DO rk_step = 1, rk_order

   !  Set the step size and number of small timesteps for
   !  each part of the timestep

   dtm = dt
   IF ( rk_order == 1 ) THEN   ! Leapfrog

       IF (step_number /= 1) THEN
         number_of_small_timesteps = 2*time_step_sound
         dt_rk = dt
         dtm = 2*dt
       ELSE
         number_of_small_timesteps = time_step_sound
         dt_rk = dt/2.
         dtm = dt
       END IF

       dts_rk = dts

   ELSE IF ( rk_order == 2 ) THEN   ! 2nd order Runge-Kutta timestep

       IF ( rk_step == 1) THEN
             dt_rk  = 0.5*dt
             dts_rk = dts
             number_of_small_timesteps = time_step_sound/2
       ELSE
             dt_rk = dt
             dts_rk = dts
             number_of_small_timesteps = time_step_sound
       ENDIF

   ELSE IF ( rk_order == 3 ) THEN ! third order Runge-Kutta

       IF ( rk_step == 1) THEN
            dt_rk = dt/3.
            dts_rk = dt_rk
            number_of_small_timesteps = 1
       ELSE IF (rk_step == 2) THEN
            dt_rk  = 0.5*dt
            dts_rk = dts
            number_of_small_timesteps = time_step_sound/2
       ELSE
            dt_rk = dt
            dts_rk = dts
            number_of_small_timesteps = time_step_sound
       ENDIF

   ELSE

      write(wrf_err_message,*)' unknown solver, error exit for dynamics_option = ',dynamics_option
      CALL wrf_error_fatal3 ( "solve_em_tst.b" , 327 ,  wrf_err_message )

   END IF

!
!  Time level t is in the *_2 variable in the first part 
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
   CALL wrf_debug ( 200 , ' call rk_step_prep ' )


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

!zzma begining

      print*,'TEST_rk_step_prep = ',TEST_rk_step_prep
      if (TEST_rk_step_prep) &
      CALL t_rk_step_prep  ( config_flags, rk_step,            &
                           u_2, v_2, w_2, t_2, ph_2, mu_2,   &
                           moist_2,                          &
                           ru, rv, rw, ww, php, alt, muu, muv,   &
                           mub, mut, phb, pb, p, al, alb,    &
                           cqu, cqv, cqw,                    &
                           msfu, msfv, msft,                 &
                           fnm, fnp, dnw, rdx, rdy,          &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                   )

   print*,'rk_step_prep zzma01'
!   stop
!zzma end

      CALL rk_step_prep  ( config_flags, rk_step,            &
                           u_2, v_2, w_2, t_2, ph_2, mu_2,   &
                           moist_2,                          &
                           ru, rv, rw, ww, php, alt, muu, muv,   &
                           mub, mut, phb, pb, p, al, alb,    &
                           cqu, cqv, cqw,                    &
                           msfu, msfv, msft,                 &
                           fnm, fnp, dnw, rdx, rdy,          &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                   )

   END DO
   !$OMP END PARALLEL DO



! set boundary conditions on variables 
! from big_step_prep for use in big_step_proc





!   CALL set_tiles ( grid , ids , ide , jds , jde , ips-1 , ipe+1 , jps-1 , jpe+1 )


   if(dyn_opt == DYN_EM) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

        CALL rk_phys_bc_dry_1( config_flags, ru, rv, rw, ww,      & 
                               muu, muv, mut, php, alt, p,        &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               ips, ipe, jps, jpe, kps, kpe,      &
                               grid%i_start(ij), grid%i_end(ij),  &
                               grid%j_start(ij), grid%j_end(ij),  &
                               k_start, k_end                )

       CALL set_physical_bc3d( ph_2, 'w', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                               grid%i_start(ij), grid%i_end(ij),        &
                               grid%j_start(ij), grid%j_end(ij),        &
                               k_start, k_end                )

   END DO
   !$OMP END PARALLEL DO
   endif


    rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).
 

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call init_zero_tendency' )

!zzma added
        print*,'TEST_init_zero_tendency = ',TEST_init_zero_tendency
        if(TEST_init_zero_tendency) &
        CALL t_init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                  ph_tendf, t_tendf, tke_tend,      &
                                  moist_tend,chem_tend,             &
                                  num_3d_m,num_3d_c,rk_step,        &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                   )

    print*, 'init_zero_tendency zzma02'
!    stop
!zzma added end


        CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                  ph_tendf, t_tendf, tke_tend,      &
                                  moist_tend,chem_tend,             &
                                  num_3d_m,num_3d_c,rk_step,        &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                   )

     END DO
   !$OMP END PARALLEL DO






!<DESCRIPTION>
!<pre>
!(2) The non-timesplit physics begins with a call to "phy_prep"
!    (which computes some diagnostic variables such as temperature,
!    pressure, u and v at p points, etc).  This is followed by
!    calls to the physics drivers:
!
!              radiation,
!              surface,
!              pbl,
!              cumulus,
!              3D TKE and mixing.
!<pre>
!</DESCRIPTION>



      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call phy_prep' )

! zzma 
          print*,' TEST_phy_prep = ',TEST_phy_prep
          if(TEST_phy_prep) &
          CALL t_phy_prep ( config_flags,                           &
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist_2, num_3d_m, &
                         mu_3d, rho,                             &
                         th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                         p8w, t_phy, t8w, z, z_at_w,             &
                         dz8w, fnm, fnp,                         &
                         RTHRATEN,                               &
                         RTHBLTEN, RUBLTEN, RVBLTEN,             &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,           &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,           &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,           &
                         RTHFTEN,  RQVFTEN,                      &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )

    print*, 'phy_prep zzma03'
!    stop
!zzma added end


         CALL phy_prep ( config_flags,                           &
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist_2, num_3d_m, &
                         mu_3d, rho,                             &
                         th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                         p8w, t_phy, t8w, z, z_at_w,             &
                         dz8w, fnm, fnp,                         &    
                         RTHRATEN,                               &
                         RTHBLTEN, RUBLTEN, RVBLTEN,             &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,           &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,           &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,           &
                         RTHFTEN,  RQVFTEN,                      &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO



!  physics to implement

!      CALL set_tiles ( grid , ids , ide-1 , jds , jde-1 ips , ipe , jps , jpe )

! Open MP loops are in physics drivers
! radiation

         CALL wrf_debug ( 200 , ' call radiation_driver' )

         if(dyn_opt == DYN_EM) then
         CALL radiation_driver(itimestep,dt,AER_DRY,AER_WATER,       &
                    RTHRATENLW,RTHRATENSW,RTHRATEN,GLW,GSW,          &
                    SWDOWN,                                          &
                    XLAT,XLONG,ALBEDO,CLDFRA,EMISS,                  &
                    rho,moist_2,num_3d_m,                            &
                    p8w,p_phy,pb,pi_phy,dz8w,t_phy,t8w,              &
                    GMT,JULDAY,config_flags,RADT,STEPRA,ICLOUD,      &
                    taucldi,taucldc,warm_rain,                       &
                    XLAND,TSK,HTOP,HBOT,CUPPT,VEGFRA,SNOW,           &
                    julyr,                                           &
                    1   ,                                            &
                    TOTSWDN,TOTLWDN,RSWTOA,RLWTOA,CZMEAN,            &
                    CFRACL,CFRACM,CFRACH,                            &
                    ACFRST,NCFRST,ACFRCV,NCFRCV,                     &
                    ids,ide, jds,jde, kds,kde,                       &
                    ims,ime, jms,jme, kms,kme,                       &
                    grid%i_start, min(grid%i_end, ide-1),            &
                    grid%j_start, min(grid%j_end, jde-1),            &
                    k_start    , min(k_end,kde-1) , grid%num_tiles   )
         endif




!********* Surface driver
! surface


      if(dyn_opt == DYN_EM) then
      CALL wrf_debug ( 200 , ' call surface_driver' )
      CALL surface_driver(                                          &
     &           ACSNOM,ACSNOW,AKHS,AKMS,ALBEDO,BR,CANWAT,CAPG        &
     &          ,CHKLOWQ,CONFIG_FLAGS,DT,DX,DZ8W,DZS,EMISS,GLW        &
     &          ,GRDFLX,GSW,GZ1OZ0,HFX,HOL,HT,IFSNOW,ISFFLX           &
     &          ,ISLTYP,ITIMESTEP,IVGTYP,LOWLYR,MAVAIL,MOIST_2,MOL,RMOL&
     &          ,NUM_SOIL_LAYERS,NUM_3D_M,P8W,PBLH,PI_PHY,PSHLTR,PSIH &
     &          ,PSIM,P_PHY,Q10,Q2,QFX,QSFC,QSHLTR,QZ0,RAINBL         &
     &          ,RAINCV,RAINNCV,REGIME,RHO,SFCEVP,SFCEXC,SFCRUNOFF    &
     &          ,SMOIS,SMSTAV,SMSTOT,SNOALB,SNOW,SNOWC,SNOWH,STEPBL   &
     &          ,T2,TH10,TH2,THC,THZ0,TH_PHY,TMN,TSHLTR,TSK,TSLB      &
     &          ,T_PHY,U10,UDRUNOFF,UST,UZ0,U_FRAME,U_PHY,V10,VEGFRA  &
     &          ,VZ0,V_FRAME,V_PHY,WARM_RAIN,WSPD,XICE,XLAND,Z,ZNT,ZS &
     &          ,CT,TKE_MYJ                                           &
     &          ,ALBBCK,LH,SH2O,SHDMAX,SHDMIN,Z0                      &
     &          ,flqc,flhc,qsg,qvg,qcg,soilt1,tsnav                   & ! for RUC LSM
     &          ,SMFR3D,KEEPFR3DFLAG                                  &
     &          ,PSFC                                                 &
     &          ,ids,ide, jds,jde, kds,kde                            &
     &          ,ims,ime, jms,jme, kms,kme                            &
     &          ,grid%i_start, min(grid%i_end, ide-1)                 &
     &          ,grid%j_start, min(grid%j_end, jde-1)                 &
     &          ,k_start    , min(k_end,kde-1) , grid%num_tiles       )
      endif


!*********
! pbl


      if(dyn_opt == DYN_EM) then
      CALL wrf_debug ( 200 , ' call pbl_driver' )
      CALL pbl_driver(itimestep,dt,u_frame,v_frame                    &
     &           ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &           ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &           ,TSK,XLAND,ZNT,HT                                    &
     &           ,UST,HOL,MOL,PBLH                                    &
     &           ,HFX,QFX,REGIME,GRDFLX                               &
     &           ,u_phy,v_phy,th_phy,rho,moist_2                      &
     &           ,p_phy,pi_phy,p8w,t_phy,dz8w,z                       &
     &           ,TKE_MYJ,EXCH_H,AKHS,AKMS                            &
     &           ,THZ0,QZ0,UZ0,VZ0,QSFC,LOWLYR                        &
     &           ,PSIM, PSIH, GZ1OZ0, WSPD, BR, CHKLOWQ               &
     &           ,config_flags,DX,num_3d_m                            &
     &           ,STEPBL,warm_rain                                    &
     &           ,KPBL,CT,LH,SNOW,XICE                                &
     &           ,ids,ide, jds,jde, kds,kde                           &
     &           ,ims,ime, jms,jme, kms,kme                           &
     &           ,grid%i_start, min(grid%i_end,ide-1)                 &
     &           ,grid%j_start, min(grid%j_end,jde-1)                 &
     &           ,k_start    , min(k_end,kde-1) , grid%num_tiles  )
      endif


! cumulus para.


         if(dyn_opt == DYN_EM) then
         CALL wrf_debug ( 200 , ' call cumulus_driver' )
         CALL cumulus_driver(                                          &
                 ! Order dependent args: domain, mem, tile dims
                     ids,ide, jds,jde, kds,kde                         &
                   , ims,ime, jms,jme, kms,kme                         &
                   , grid%i_start, min(grid%i_end, ide-1)              &
                   , grid%j_start, min(grid%j_end, jde-1)              &
                   , k_start  , min(k_end,kde-1) , grid%num_tiles      &
                 ! Prognostic variables
                   , U=u_phy   ,V=v_phy   ,TH=th_phy  ,T=t_phy         &
                   , W=w_2     ,P=p_phy   ,PI=pi_phy  ,RHO=rho         &
                 ! Other arguments
                   , ITIMESTEP=itimestep ,DT=dt      ,DX=dx            &
                   , RAINC=rainc   ,RAINCV=raincv   ,NCA=nca           &
                   , HTOP=htop     ,HBOT=hbot       ,KPBL=kpbl         &
                   , DZ8W=dz8w     ,P8W=p8w                            &
                   , W0AVG=w0avg   ,STEPCU=stepcu                      &
                   , CLDEFI=cldefi ,LOWLYR=lowlyr ,XLAND=xland         &
                   , APR_GR=apr_gr ,APR_W=apr_w   ,APR_MC=apr_mc       &
                   , APR_ST=apr_st ,APR_AS=apr_as ,APR_CAPMA=apr_capma &
                   , APR_CAPME=apr_capme          ,APR_CAPMI=apr_capmi &
                   , MASS_FLUX=mass_flux          ,XF_ENS=xf_ens       &
                   , PR_ENS=pr_ens ,HT=ht                              &
                   , ENSDIM=ensdim ,MAXIENS=maxiens ,MAXENS=maxens     &
                   , MAXENS2=maxens2                ,MAXENS3=maxens3   &
                   , CU_ACT_FLAG=cu_act_flag   ,WARM_RAIN=warm_rain    &
                 ! Selection flag
                   , CU_PHYSICS=config_flags%cu_physics                &
                 ! Moisture tendency arguments
                   , RQVCUTEN=rqvcuten , RQCCUTEN=rqccuten             &
                   , RQRCUTEN=rqrcuten , RQVBLTEN=rqvblten             &
                   , RQVFTEN=rqvften                                   &
                 ! Other tendency arguments
                   , RTHRATEN=rthraten , RTHBLTEN=rthblten             &
                   , RTHCUTEN=rthcuten , RTHFTEN=rthften               &
                 ! Moisture tracer arguments
                   , QV_CURR=moist_2(ims,kms,jms,P_QV), F_QV=F_QV      &
                   , QC_CURR=moist_2(ims,kms,jms,P_QC), F_QC=F_QC      &
                   , QR_CURR=moist_2(ims,kms,jms,P_QR), F_QR=F_QR      &
                   , QI_CURR=moist_2(ims,kms,jms,P_QI), F_QI=F_QI      &
                   , QS_CURR=moist_2(ims,kms,jms,P_QS), F_QS=F_QS      &
                   , QG_CURR=moist_2(ims,kms,jms,P_QG), F_QG=F_QG      &
                                                                       )
         endif


! calculate_phy_tend


      if(dyn_opt == DYN_EM) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
          CALL calculate_phy_tend (config_flags,mut,pi_phy,            &
                     RTHRATEN,                                         &
                     RUBLTEN,RVBLTEN,RTHBLTEN,                         &
                     RQVBLTEN,RQCBLTEN,RQIBLTEN,                       &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                     RQICUTEN,RQSCUTEN,                                &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO
      endif


! tke diffusion

     IF(diff_opt .eq. 2 .OR. diff_opt .eq. 1) THEN


       if(dyn_opt == DYN_EM) then
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
          CALL compute_diff_metrics ( config_flags, ph_2, phb, z, rdz, rdzw, &
                                      zx, zy, rdx, rdy,                      &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO
       endif







       if(dyn_opt == DYN_EM) then
       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
          CALL set_physical_bc3d( rdzw , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( rdz , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( z , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zx , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zy , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )

       ENDDO
       endif







       if(dyn_opt == DYN_EM) then
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
          CALL cal_deform_and_div ( config_flags,u_2,v_2,w_2,div,        &
                                    defor11,defor22,defor33,defor12,     &
                                    defor13,defor23,                     &
                                    u_base, v_base,msfu,msfv,msft,       &
                                    rdx, rdy, dn, dnw, rdz, rdzw,        &
                                    fnm,fnp,cf1,cf2,cf3,zx,zy,           &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    grid%i_start(ij), grid%i_end(ij),    &
                                    grid%j_start(ij), grid%j_end(ij),    &
                                    k_start    , k_end                  )
       ENDDO
       !$OMP END PARALLEL DO
       endif








! calculate tke, kmh, and kmv


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_km_kh' )

!zzma
          print*,'TEST_calculate_km_kh = ',TEST_calculate_km_kh
          if(TEST_calculate_km_kh) &
          CALL t_calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,     &
                                xkmh,xkmhd,xkmv,xkhh,xkhv,BN2,               &
                                khdif,kvdif,div,                             &
                                defor11,defor22,defor33,defor12,             &
                                defor13,defor23,                             &
                                tke_2(ims,kms,jms),p8w,t8w,th_phy,           &
                                t_phy,p_phy,moist_2,dn,dnw,                  &
                                dx,dy,rdz,rdzw,mix_cr_len,num_3d_m,          &
                                cf1, cf2, cf3, warm_rain,                    &
                                kh_tke_upper_bound, kv_tke_upper_bound,      &
                                ids,ide, jds,jde, kds,kde,                   &
                                ims,ime, jms,jme, kms,kme,                   &
                                grid%i_start(ij), grid%i_end(ij),            &
                                grid%j_start(ij), grid%j_end(ij),            &
                                k_start    , k_end                          )

    print*, 'calculate_km_kh zzma04'
!    stop
!zzma added end

          CALL calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,     &
                                xkmh,xkmhd,xkmv,xkhh,xkhv,BN2,               &
                                khdif,kvdif,div,                             &
                                defor11,defor22,defor33,defor12,             &
                                defor13,defor23,                             &
                                tke_2(ims,kms,jms),p8w,t8w,th_phy,           &
                                t_phy,p_phy,moist_2,dn,dnw,                  &
                                dx,dy,rdz,rdzw,mix_cr_len,num_3d_m,          &
                                cf1, cf2, cf3, warm_rain,                    &
                                kh_tke_upper_bound, kv_tke_upper_bound,      &
                                ids,ide, jds,jde, kds,kde,                   &
                                ims,ime, jms,jme, kms,kme,                   &
                                grid%i_start(ij), grid%i_end(ij),            &
                                grid%j_start(ij), grid%j_end(ij),            &
                                k_start    , k_end                          )
       ENDDO
       !$OMP END PARALLEL DO






     ENDIF







     if(dyn_opt == DYN_EM) then
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call phy_bc' )
       CALL phy_bc (config_flags,div,defor11,defor22,defor33,            &
                            defor12,defor13,defor23,                     &
                            xkmh,xkmhd,xkmv,xkhh,xkhv,                   &
                            tke_2(ims,kms,jms),                          &
                            RUBLTEN, RVBLTEN,                            &
                            ids, ide, jds, jde, kds, kde,                &
                            ims, ime, jms, jme, kms, kme,                &
                            ips, ipe, jps, jpe, kps, kpe,                &
                            grid%i_start(ij), grid%i_end(ij),                      &
                            grid%j_start(ij), grid%j_end(ij),                      &
                            k_start    , k_end                           )
     ENDDO
     !$OMP END PARALLEL DO
     endif




      if(dyn_opt == DYN_EM) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call update_phy_ten' )
        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
                          RTHRATEN,RTHBLTEN,RTHCUTEN,RUBLTEN,RVBLTEN,  &
                          RQVBLTEN,RQCBLTEN,RQIBLTEN,                  &
                          RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,&
                          num_3d_m,config_flags,rk_step,              &
                          ids, ide, jds, jde, kds, kde,                &
                          ims, ime, jms, jme, kms, kme,                &
                          grid%i_start(ij), grid%i_end(ij),                      &
                          grid%j_start(ij), grid%j_end(ij),                      &
                          k_start, k_end                               )

      END DO
      !$OMP END PARALLEL DO
      endif


     IF( diff_opt .eq. 2 .and. km_opt .eq. 2 ) THEN


       if(dyn_opt == DYN_EM) then
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL tke_rhs  ( tke_tend,BN2,                               &
                          config_flags,defor11,defor22,defor33,       &
                          defor12,defor13,defor23,u_2,v_2,w_2,div,    &
                          tke_2(ims,kms,jms),mut,                     &
                          th_phy,p_phy,p8w,t8w,z,fnm,fnp,             &
                          cf1,cf2,cf3,msft,xkmh,xkmv,xkhv,rdx,rdy,    &
                          dx,dy,dt,zx,zy,rdz,rdzw,dn,dnw,mix_cr_len,  &
                          ids, ide, jds, jde, kds, kde,               &
                          ims, ime, jms, jme, kms, kme,               &
                          grid%i_start(ij), grid%i_end(ij),           &
                          grid%j_start(ij), grid%j_end(ij),           &
                          k_start    , k_end                         )

       ENDDO
       !$OMP END PARALLEL DO
       endif


     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

     IF(diff_opt .eq. 2) THEN

       IF (bl_pbl_physics .eq. 0) THEN


         if(dyn_opt == DYN_EM) then
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
           CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,              &
                                      t_tendf, tke_tend,                         &
                                      moist_tend, num_3d_m,                      &
                                      chem_tend, num_3d_c,                       &
                                      u_2, v_2,                                  &
                                      t_2,u_base,v_base,t_base,qv_base,          &
                                      mut,tke_2,config_flags,                    &
                                      defor13,defor23,defor33,                   &
                                      div, moist_2, chem_2, xkmv, xkhv, km_opt,  &
                                      fnm, fnp, dn, dnw, rdz, rdzw,              &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start    , k_end                        )

         ENDDO
         !$OMP END PARALLEL DO
         endif


       ENDIF
!

       if(dyn_opt == DYN_EM) then
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
         CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend,                              &
                                      moist_tend, num_3d_m,                  &
                                      chem_tend, num_3d_c,                   &
                                      t_2, th_phy,                           &
                                      mut, tke_2, config_flags,              &
                                      defor11, defor22, defor12,             &
                                      defor13, defor23, div,                 &
                                      moist_2, chem_2,                       &
                                      msfu, msfv, msft, xkmhd, xkhh, km_opt, &
                                      rdx, rdy, rdz, rdzw,                   &
                                      fnm, fnp, cf1, cf2, cf3,               &
                                      zx, zy, dn, dnw,                       &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO
       endif


     ENDIF

     END IF rk_step_is_one


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_tendency' )

!zzma  added
      print*,'TEST_rk_tendency = ',TEST_rk_tendency
      if(TEST_rk_tendency) &
      CALL t_rk_tendency ( config_flags, rk_step,                           &
                         ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, u_save, v_save, w_save, ph_save,        &
                         t_save, mu_save, RTHFTEN,                        &
                         ru, rv, rw, ww,                                  &
                         u_2, v_2, w_2, t_2, ph_2,                        &
                         u_1, v_1, w_1, t_1, ph_1,                        &
                         h_diabatic, phb, t_init,                         &
                         mu_2, mut, muu, muv, mub,                        &
                         al, alt, p, pb, php, cqu, cqv, cqw,              &
                         u_base, v_base, t_base, qv_base, z_base,         &
                         msfu, msfv, msft, f, e, sina, cosa,              &
                         fnm, fnp, rdn, rdnw,                             &
                         dt, rdx, rdy, khdif, kvdif, xkmhd,               &
                         cf1, cf2, cf3, cfn, cfn1, num_3d_m,              &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )

   print*,'rk_tendency zzma05'
!stop
!zzma added end

      CALL rk_tendency ( config_flags, rk_step,                           &
                         ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, u_save, v_save, w_save, ph_save,        &
                         t_save, mu_save, RTHFTEN,                        &
                         ru, rv, rw, ww,                                  &
                         u_2, v_2, w_2, t_2, ph_2,                        &
                         u_1, v_1, w_1, t_1, ph_1,                        &
                         h_diabatic, phb, t_init,                         &
                         mu_2, mut, muu, muv, mub,                        &
                         al, alt, p, pb, php, cqu, cqv, cqw,              &
                         u_base, v_base, t_base, qv_base, z_base,         &
                         msfu, msfv, msft, f, e, sina, cosa,              &
                         fnm, fnp, rdn, rdnw,                             &
                         dt, rdx, rdy, khdif, kvdif, xkmhd,               &
                         cf1, cf2, cf3, cfn, cfn1, num_3d_m,              &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )

!Xiao added
    if(TEST_advect_scalar) &
    CALL t_advect_scalar ( t_2, t_2, t_tend, ru, rv, ww,   &
                         mut, config_flags,            &
                         msfu, msfv, msft, fnm, fnp,   &
                         rdx, rdy, rdnw,               &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )

   print*,'advect_scalar extra_test' 
   !stop
!Xiao added end

   END DO
   !$OMP END PARALLEL DO



   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

!zzma begining
 
       print*,'TEST_relax_bdy_dry = ',TEST_relax_bdy_dry
       if(TEST_relax_bdy_dry) &
       CALL t_relax_bdy_dry ( config_flags,                                &
                            u_save, v_save, ph_save, t_save,             &
                            w_save, mu_tend,                             &
                            ru, rv, ph_2, t_2,                           &
                            w_2, mu_2, mut,                              &
                            u_b, v_b, ph_b, t_b, w_b,                    &
                            mu_b,                                        &
                            u_bt, v_bt, ph_bt, t_bt,                     &
                            w_bt, mu_bt,                                 &
                            spec_bdy_width, spec_zone, relax_zone,       &
                            dtbc, fcx, gcx,             &
                            ijds, ijde,                 &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            ips,ipe, jps,jpe, kps,kpe,  &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                              )

   print*,'relax_bdy_dry zzma06'
!   stop
!zzma end
       CALL relax_bdy_dry ( config_flags,                                &
                            u_save, v_save, ph_save, t_save,             &
                            w_save, mu_tend,                             & 
                            ru, rv, ph_2, t_2,                           &
                            w_2, mu_2, mut,                              &
                            u_b, v_b, ph_b, t_b, w_b,                    &
                            mu_b,                                        &
                            u_bt, v_bt, ph_bt, t_bt,                     &
                            w_bt, mu_bt,                                 &
                            spec_bdy_width, spec_zone, relax_zone,       &
                            dtbc, fcx, gcx,             &
                            ijds, ijde,                 &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            ips,ipe, jps,jpe, kps,kpe,  &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                              )


     ENDIF

!zzma begining

     print*,'TEST_rk_addtend_dry = ',TEST_rk_addtend_dry
     if(TEST_rk_addtend_dry) &
     CALL t_rk_addtend_dry( ru_tend,  rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                          ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                          u_save, v_save, w_save, ph_save, t_save, rk_step,&
                          h_diabatic, mut, msft, msfu, msfv,               &
                          ids,ide, jds,jde, kds,kde,                       &
                          ims,ime, jms,jme, kms,kme,                       &
                          ips,ipe, jps,jpe, kps,kpe,                       &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                                  )

   print*,'rk_addtend_dry zzma07'
!   stop
!zzma end

     CALL rk_addtend_dry( ru_tend,  rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                          ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                          u_save, v_save, w_save, ph_save, t_save, rk_step,&
                          h_diabatic, mut, msft, msfu, msfv,               &
                          ids,ide, jds,jde, kds,kde,                       &
                          ims,ime, jms,jme, kms,kme,                       &
                          ips,ipe, jps,jpe, kps,kpe,                       &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN 


!zzma begining

!write(111,(f10.5)) ru_tend(:,:,:)
!write(112,(f10.5)) rv_tend(:,:,:)
!write(113,(f10.5)) ph_tend(:,:,:)
!write(114,(f10.5)) t_tend(:,:,:)
!write(115,(f10.5)) rw_tend(:,:,:)
!write(116,(f10.5)) mu_tend(:,:)
!write(117,(f10.5)) u_bt(:,:,:,:)
!write(118,(f10.5)) v_bt(:,:,:,:)
!write(119,(f10.5)) ph_bt(:,:,:,:)
!write(120,(f10.5)) t_bt(:,:,:,:)
!write(121,(f10.5)) w_bt(:,:,:,:)
!write(122,(f10.5)) mu_bt(:,:,:,:)
!write(217,(f10.5)) u_b(:,:,:,:)
!write(218,(f10.5)) v_b(:,:,:,:)
!write(219,(f10.5)) ph_b(:,:,:,:)
!write(220,(f10.5)) t_b(:,:,:,:)
!write(221,(f10.5)) w_b(:,:,:,:)
!write(222,(f10.5)) mu_b(:,:,:,:)
       print*,' TEST_spec_bdy_dry = ',TEST_spec_bdy_dry
       if(TEST_spec_bdy_dry) &
       CALL t_spec_bdy_dry ( config_flags,                                    &
                           ru_tend, rv_tend, ph_tend, t_tend,               &
                           rw_tend, mu_tend,                                &
                           u_b, v_b, ph_b, t_b,                             &
                           w_b, mu_b,                                       &
                           u_bt, v_bt, ph_bt, t_bt,                         &
                           w_bt, mu_bt,                                     &
                           spec_bdy_width, spec_zone,                       &
                           ijds, ijde,                 & ! min/max(id,jd)
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                  )

   print*,'spec_bdy_dry zzma08'
!   stop
!zzma end

       CALL spec_bdy_dry ( config_flags,                                    &
                           ru_tend, rv_tend, ph_tend, t_tend,               &
                           rw_tend, mu_tend,                                &
                           u_b, v_b, ph_b, t_b,                             &
                           w_b, mu_b,                                       &
                           u_bt, v_bt, ph_bt, t_bt,                         &
                           w_bt, mu_bt,                                     &
                           spec_bdy_width, spec_zone,                       &
                           ijds, ijde,                 & ! min/max(id,jd)
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                  )
     
     ENDIF

   END DO
   !$OMP END PARALLEL DO


!<DESCRIPTION>
!<pre>
! (3) Small (acoustic,sound) steps.
!
!    Several acoustic steps are taken each RK pass.  A small step 
!    sequence begins with calculating perturbation variables 
!    and coupling them to the column dry-air-mass mu 
!    (call to small_step_prep).  This is followed by computing
!    coefficients for the vertically implicit part of the
!    small timestep (call to calc_coef_w).  
!
!    The small steps are taken
!    in the named loop "small_steps:".  In the small_steps loop, first 
!    the horizontal momentum (u and v) are advanced (call to advance_uv),
!    next mu and theta are advanced (call to advance_mu_t) followed by
!    advancing w and the geopotential (call to advance_w).  Diagnostic
!    values for pressure and inverse density are updated at the end of
!    each small_step.
!
!    The small-step section ends with the change of the perturbation variables
!    back to full variables (call to small_step_finish).
!</pre>
!</DESCRIPTION>


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first 
    ! predictor because we may have changed the small timestep -> dts.

      CALL wrf_debug ( 200 , ' call calc_coef_w' )
!zzma begining 
      print*,' TEST_small_step_prep = ',TEST_small_step_prep
      if(TEST_small_step_prep) &
      CALL t_small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,          &
                            t_1,t_2,ph_1,ph_2,                &
                            mub, mu_1, mu_2,                  &
                            muu, muus, muv, muvs,             &
                            mut, muts, mudf,                  & 
                            u_save, v_save, w_save,           & 
                            t_save, ph_save, mu_save,         &
                            ww, ww1,                          &
                            dnw, c2a, pb, p, alt,             &
                            msfu, msfv, msft,                 &
                            rk_step, leapfrog,                &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

   print*,'small_step_prep zzma09'
!   stop
!zzma end

      CALL small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,          &
                            t_1,t_2,ph_1,ph_2,                &
                            mub, mu_1, mu_2,                  &
                            muu, muus, muv, muvs,             &
                            mut, muts, mudf,                  &
                            u_save, v_save, w_save,           &
                            t_save, ph_save, mu_save,         &
                            ww, ww1,                          &
                            dnw, c2a, pb, p, alt,             &
                            msfu, msfv, msft,                 &
                            rk_step, leapfrog,                &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )


!zzma begining

      print*,' TEST_calc_p_rho = ',TEST_calc_p_rho
      if(TEST_calc_p_rho) &
      CALL t_calc_p_rho( al, p, ph_2,                      &
                       alt, t_2, t_save, c2a, pm1,       &
                       mu_2, muts, znu, t0,              &
                       rdnw, dnw, smdiv,                 &
                       non_hydrostatic, 0,               &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start    , k_end               )

   print*,'calc_p_rho zzma10'
!   stop
!zzma end

      CALL calc_p_rho( al, p, ph_2,                      &
                       alt, t_2, t_save, c2a, pm1,       &
                       mu_2, muts, znu, t0,              &
                       rdnw, dnw, smdiv,                 &
                       non_hydrostatic, 0,               &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start    , k_end               )

!zzma begining

      print*,'TEST_calc_coef_w = ',TEST_calc_coef_w
      if(non_hydrostatic .and. TEST_calc_coef_w) &
      CALL t_calc_coef_w( a,alpha,gamma,                  &
                        mut, cqw,                         &
                        rdn, rdnw, c2a,                   &
                        dts_rk, g, epssm,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )

   print*,'calc_coef_w zzma11'
!   stop
!zzma end
      IF (non_hydrostatic)                                &
      CALL calc_coef_w( a,alpha,gamma,                    &
                        mut, cqw,                         &
                        rdn, rdnw, c2a,                   &
                        dts_rk, g, epssm,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


   ENDDO
   !$OMP END PARALLEL DO





   if(dyn_opt == DYN_EM) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

         CALL set_physical_bc3d( ru_tend, 'u', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( rv_tend, 'v', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( t_1, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( t_save, 't', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc2d( mu_1, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

    END DO
    !$OMP END PARALLEL DO
    endif


   small_steps : DO iteration = 1 , number_of_small_timesteps

   ! Boundary condition time (or communication time).  






      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles



!zzma begining

         print*,'TEST_advance_uv = ',TEST_advance_uv
         if(TEST_advance_uv) &
         CALL t_advance_uv ( u_2, ru_tend, v_2, rv_tend,       &
                           p, pb,                            &
                           ph_2, php, alt, al, mu_2,         &
                           muu, cqu, muv, cqv, mudf,         &
                           rdx, rdy, dts_rk,                 &
                           cf1, cf2, cf3, fnm, fnp,          &
                           emdiv,                            &
                           rdnw, config_flags,spec_zone,     &
                           non_hydrostatic,                  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

   print*,'advance_uv zzma12'
!   stop
!zzma end

         CALL advance_uv ( u_2, ru_tend, v_2, rv_tend,       &
                           p, pb,                            &
                           ph_2, php, alt, al, mu_2,         &
                           muu, cqu, muv, cqv, mudf,         &
                           rdx, rdy, dts_rk,                 &
                           cf1, cf2, cf3, fnm, fnp,          &
                           emdiv,                            &
                           rdnw, config_flags,spec_zone,     &
                           non_hydrostatic,                  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )



         IF( config_flags%specified .or. config_flags%nested ) THEN

!zzma begining

           print*,'TEST_spec_bdyupdate = ',TEST_spec_bdyupdate
           if(TEST_spec_bdyupdate) &
           CALL t_spec_bdyupdate(u_2, ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

   print*,'spec_bdyupdate zzma13'
!   stop
!zzma end

           CALL spec_bdyupdate(u_2, ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(v_2, rv_tend, dts_rk,      &
                               'v'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

         ENDIF


      END DO
      !$OMP END PARALLEL DO


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate ww



!zzma begining
!
        print*,'TEST_advance_mu_t = ',TEST_advance_mu_t
        if(TEST_advance_mu_t) &
        CALL t_advance_mu_t( ww, ww1, u_2, u_save, v_2, v_save, &
                           mu_2, mut, muave, muts, muu, muv,  &
                           mudf, ru_m, rv_m, ww_m,            &
                           t_2, t_save, t_2save, t_tend,      &
                           mu_tend,                           &
                           rdx, rdy, dts_rk, epssm,           &
                           dnw, fnm, fnp, rdnw,               &
                           msfu, msfv, msft,                  &
                           iteration, config_flags,           &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start    , k_end                )

   print*,'advance_mu_t zzma14'
!   stop
!zzma end

        CALL advance_mu_t( ww, ww1, u_2, u_save, v_2, v_save, &
                           mu_2, mut, muave, muts, muu, muv,  &
                           mudf, ru_m, rv_m, ww_m,            &
                           t_2, t_save, t_2save, t_tend,      &
                           mu_tend,                           &
                           rdx, rdy, dts_rk, epssm,           &
                           dnw, fnm, fnp, rdnw,               &
                           msfu, msfv, msft,                  &
                           iteration, config_flags,           &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start    , k_end                )



         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL spec_bdyupdate(t_2, t_tend, dts_rk,      &
                               't'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(mu_2, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )

           CALL spec_bdyupdate(muts, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )
         ENDIF


         ! sumflux accumulates the time-averged mass flux
         ! (time averaged over the acoustic steps) for use
         ! in the scalar advection (flux divergence).  Using
         ! time averaged values gives us exact scalar conservation.



!zzma begining

         print*,'TEST_sumflux = ',TEST_sumflux
         if(TEST_sumflux) &
         CALL t_sumflux ( u_2, v_2, ww,                         &
                        u_save, v_save, ww1,                  &
                        muu, muv,                             &
                        ru_m, rv_m, ww_m, epssm,              &
                        msfu, msfv,                           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )

   print*,'sumflux zzma15'
!   stop
!zzma end

         CALL sumflux ( u_2, v_2, ww,                         &
                        u_save, v_save, ww1,                  &
                        muu, muv,                             &
                        ru_m, rv_m, ww_m, epssm,              &
                        msfu, msfv,                           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )


         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.



        IF ( non_hydrostatic ) THEN

!zzma begining

          print*,'TEST_advance_w = ',TEST_advance_w
          if(TEST_advance_w) &
          CALL t_advance_w( w_2, rw_tend, ww, u_2, v_2,       &
                          mu_2, mut, muave, muts,           &
                          t_2save, t_2, t_save,             &
                          ph_2, ph_save, phb, ph_tend,      &
                          ht, c2a, cqw, alt, alb,           &
                          a, alpha, gamma,                  &
                          rdx, rdy, dts_rk, t0, epssm,      &
                          dnw, fnm, fnp, rdnw, rdn,         &
                          cf1, cf2, cf3, msft,              &
                          config_flags,                     &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

   print*,'advance_w zzma16'
!  stop
!zzma end


          CALL advance_w( w_2, rw_tend, ww, u_2, v_2,       &
                          mu_2, mut, muave, muts,           &
                          t_2save, t_2, t_save,             &
                          ph_2, ph_save, phb, ph_tend,      &
                          ht, c2a, cqw, alt, alb,           &
                          a, alpha, gamma,                  &
                          rdx, rdy, dts_rk, t0, epssm,      &
                          dnw, fnm, fnp, rdnw, rdn,         &
                          cf1, cf2, cf3, msft,              &
                          config_flags,                     &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
        ENDIF


        IF( config_flags%specified .or. config_flags%nested ) THEN


           IF (non_hydrostatic)  THEN

!zzma begining

             print*,'TEST_spec_bdyupdate_ph = ',TEST_spec_bdyupdate_ph
             if(TEST_spec_bdyupdate_ph) &
             CALL t_spec_bdyupdate_ph( ph_save, ph_2, ph_tend, mu_tend, muts, dts_rk, &
                                     'h'         , config_flags, &
                                     spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end             )

   print*,'spec_bdyupdate_ph zzma17'
!   stop
!zzma end


             CALL spec_bdyupdate_ph( ph_save, ph_2, ph_tend, mu_tend, muts, dts_rk, &
                                     'h'         , config_flags, &
                                     spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end             )
             IF( config_flags%specified ) THEN
               if(dyn_opt == DYN_EM) then
               CALL zero_grad_bdy ( w_2,                        &
                                    'w'         , config_flags, &
                                    spec_zone,                  &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),         &
                                    grid%j_start(ij), grid%j_end(ij),         &
                                    k_start    , k_end             )
               endif
             ELSE
               CALL spec_bdyupdate   ( w_2, rw_tend, dts_rk,       &
                                       'h'         , config_flags, &
                                       spec_zone,                  &
                                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                       grid%i_start(ij), grid%i_end(ij),         &
                                       grid%j_start(ij), grid%j_end(ij),         &
                                       k_start    , k_end             )
             ENDIF
          ENDIF

        ENDIF


        CALL calc_p_rho( al, p, ph_2,                      &
                         alt, t_2, t_save, c2a, pm1,       &
                         mu_2, muts, znu, t0,              &
                         rdnw, dnw, smdiv,                 &
                         non_hydrostatic, iteration,       &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         grid%i_start(ij), grid%i_end(ij), &
                         grid%j_start(ij), grid%j_end(ij), &
                         k_start    , k_end               )


   ENDDO
   !$OMP END PARALLEL DO



      if(dyn_opt == DYN_EM) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        ! boundary condition set for next small timestep

         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc2d( muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

      END DO
      !$OMP END PARALLEL DO
      endif


   END DO small_steps

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to 
      ! full perturbation variables.
      ! first get updated mu at u and v points



!zzma begining

      print*,'TEST_calc_mu_uv_1 = ',TEST_calc_mu_uv_1
      if(TEST_calc_mu_uv_1) &
      CALL t_calc_mu_uv_1 ( config_flags,                     &
                          muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

   print*,'calc_mu_uv_1 zzma18'
!   stop
!zzma end

      CALL calc_mu_uv_1 ( config_flags,                     &
                          muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )




!zzma begining

      print*,'TEST_small_step_finish = ',TEST_small_step_finish
      if(TEST_small_step_finish) &
      CALL t_small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,     &
                              t_2, t_1, ph_2, ph_1, ww, ww1,    &
                              mu_2, mu_1,                       &
                              mut, muts, muu, muus, muv, muvs,  &
                              u_save, v_save, w_save,           &
                              t_save, ph_save, mu_save,         &
                              msfu, msfv, msft,                 &
                              h_diabatic,                       &
                              number_of_small_timesteps,dts_rk, &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

   print*,'small_step_finish zzma19'
!   stop
!zzma end

      CALL small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,     &
                              t_2, t_1, ph_2, ph_1, ww, ww1,    &
                              mu_2, mu_1,                       &
                              mut, muts, muu, muus, muv, muvs,  & 
                              u_save, v_save, w_save,           &
                              t_save, ph_save, mu_save,         &
                              msfu, msfv, msft,                 &
                              h_diabatic,                       &
                              number_of_small_timesteps,dts_rk, &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )


   END DO
   !$OMP END PARALLEL DO


!<DESCRIPTION>
!<pre>
! (4) Still within the RK loop, the scalar variables are advanced.
!
!    For the moist and chem variables, each one is advanced
!    individually, using named loops "moist_variable_loop:"
!    and "chem_variable_loop:".  Each RK substep begins by
!    calculating the advective tendency, and, for the first RK step, 
!    3D mixing (calling rk_scalar_tend) followed by an update
!    of the scalar (calling rk_scalar_update).
!</pre>
!</DESCRIPTION>


  moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

   moist_variable_loop: do im = PARAM_FIRST_SCALAR, num_3d_m

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )



!zzma begining

       print*,'TEST_rk_scalar_tend = ',TEST_rk_scalar_tend
       if(TEST_rk_scalar_tend) &
       CALL t_rk_scalar_tend (  im, im, config_flags,             &
                              rk_step, dt_rk,                   &
                              ru_m, rv_m, ww_m,                 &
                              mut, alt,                         &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend,RQVFTEN,              &
                              qv_base, .true., fnm, fnp,        &
                              msfu, msfv, msft,                 &
                              rdx, rdy, rdn, rdnw, khdif,       &
                              kvdif, xkmhd,                     &
                              leapfrog,                         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

   print*,'rk_scalar_tend zzma20'
!   stop
!zzma end

       CALL rk_scalar_tend (  im, im, config_flags,             &
                              rk_step, dt_rk,                   &
                              ru_m, rv_m, ww_m,                 &
                              mut, alt,                         &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend,RQVFTEN,              &
                              qv_base, .true., fnm, fnp,        &
                              msfu, msfv, msft,                 &
                              rdx, rdy, rdn, rdnw, khdif,       &
                              kvdif, xkmhd,                     &
                              leapfrog,                         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )



     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

       IF(im .eq. P_QV)THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqv_b, rqv_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )
!zzma begining 

         print*,' TEST_spec_bdy_scalar =',TEST_spec_bdy_scalar
         if(TEST_spec_bdy_scalar) &
         CALL t_spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqv_b, rqv_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

   print*,'spec_bdy_scalar zzma21'
!   stop
!zzma end


         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqv_b, rqv_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ENDIF

     ENDIF

!  ugly code for nested b.c for moist scalars other than qv

     IF( config_flags%nested .and. (rk_step == 1) ) THEN 

       IF (im .eq. P_QC) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqc_b, rqc_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )


         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqc_b, rqc_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QR) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqr_b, rqr_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqr_b, rqr_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QI) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqi_b, rqi_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqi_b, rqi_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QS) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqs_b, rqs_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqs_b, rqs_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ELSE IF (im .eq. P_QG) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqg_b, rqg_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqg_b, rqg_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ENDIF

     ENDIF ! b.c test for moist nested boundary condition



   ENDDO moist_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )




!zzma begining
       print*,'TEST_rk_update_scalar = ',TEST_rk_update_scalar
       if(TEST_rk_update_scalar) &
       CALL t_rk_update_scalar( im, im,                           &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

   print*,'rk_update_scalar zzma22'
!   stop
!zzma end


       CALL rk_update_scalar( im, im,                           &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )



       if(dyn_opt == DYN_EM) then
       IF( config_flags%specified ) THEN
         IF(im .ne. P_QV)THEN
           CALL flow_dep_bdy  (  moist_2(ims,kms,jms,im),                     &
                               ru_m, rv_m, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),                      &
                               grid%j_start(ij), grid%j_end(ij),                      &
                               k_start, k_end                               )
         ENDIF
       ENDIF
       endif


   ENDDO moist_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO moist_variable_loop

 ENDIF moist_scalar_advance


 if(dyn_opt == DYN_EM) then
 TKE_advance: IF (km_opt .eq. 2) then


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_1: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
     CALL rk_scalar_tend ( 1, 1, config_flags,               &
                           rk_step, dt_rk,                   &
                           ru_m, rv_m, ww_m,                 &
                           mut, alt,                         &
                           tke_1(ims,kms,jms),               &
                           tke_2(ims,kms,jms),               &
                           tke_tend(ims,kms,jms),            &
                           advect_tend,RQVFTEN,              &
                           qv_base, .false., fnm, fnp,       &
                           msfu, msfv, msft,                 &
                           rdx, rdy, rdn, rdnw, khdif,       &
                           kvdif, xkmhd,                     &
                           leapfrog,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

   ENDDO tke_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_update_scalar' )
     CALL rk_update_scalar( 1, 1,                             &
                            tke_1(ims,kms,jms),               &
                            tke_2(ims,kms,jms),               &
                            tke_tend(ims,kms,jms),            &
                            advect_tend,msft,                 &
                            mu_1, mu_2, mub,                  &
                            rk_step, dt_rk, spec_zone,        &
                            epsts, leapfrog,config_flags,     &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               ) 

! bound the tke (greater than 0, less than tke_upper_bound)

     CALL bound_tke( tke_2(ims,kms,jms), tke_upper_bound, &
                     ids, ide, jds, jde, kds, kde,        &
                     ims, ime, jms, jme, kms, kme,        &
                     grid%i_start(ij), grid%i_end(ij),    &
                     grid%j_start(ij), grid%j_end(ij),    &
                     k_start    , k_end                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN
         CALL flow_dep_bdy (  tke_2(ims,kms,jms),                     &
                              ru_m, rv_m, config_flags,               &
                              spec_zone,                              &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              grid%i_start(ij), grid%i_end(ij),       &
                              grid%j_start(ij), grid%j_end(ij),       &
                              k_start, k_end                               )
     ENDIF
   ENDDO tke_tile_loop_2
   !$OMP END PARALLEL DO

   END IF TKE_advance
   endif


!  next the chemical species

  chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

   chem_variable_loop: do ic = PARAM_FIRST_SCALAR, num_3d_c

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
       CALL rk_scalar_tend ( ic, ic, config_flags,                  &
                             rk_step, dt_rk,                   &
                             ru_m, rv_m, ww_m,                 &
                             mut, alt,                         &
                             chem_1(ims,kms,jms,ic),           &
                             chem_2(ims,kms,jms,ic),           &
                             chem_tend(ims,kms,jms,ic),        &
                             advect_tend,RQVFTEN,              &
                             qv_base, .false., fnm, fnp,       &
                             msfu, msfv, msft,                 &
                             rdx, rdy, rdn, rdnw,              &
                             khdif, kvdif, xkmhd,              &
                             leapfrog,                         &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

   ENDDO chem_tile_loop_1
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
       CALL rk_update_scalar( ic, ic,                           &
                              chem_1(ims,kms,jms,ic),           &
                              chem_2(ims,kms,jms,ic),           &
                              chem_tend(ims,kms,jms,ic),        &
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )


       IF( config_flags%specified ) THEN
! come back to this and figure out why two different routines are needed. JM 20041203

           CALL flow_dep_bdy  ( chem_2(ims,kms,jms,ic),     &
                                ru_m, rv_m, config_flags,   &
                                spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),  &
                                grid%j_start(ij), grid%j_end(ij),  &
                                k_start, k_end                    )
       ENDIF


   ENDDO chem_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO chem_variable_loop

 ENDIF chem_scalar_advance

 !  update the pressure and density at the new time level

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles



!zzma begining
     print*,'TEST_calc_p_rho_phi = ',TEST_calc_p_rho_phi
     if(TEST_calc_p_rho_phi) &
     CALL t_calc_p_rho_phi( moist_2, num_3d_m,                &
                          al, alb, mu_2, muts,              &
                          ph_2, p, pb, t_2,                 &
                          p0, t0, znu, dnw, rdnw,           &
                          rdn, non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )


   print*,'calc_p_rho_phi zzma23'
!   stop
!zzma end

     CALL calc_p_rho_phi( moist_2, num_3d_m,                &
                          al, alb, mu_2, muts,              &
                          ph_2, p, pb, t_2,                 &
                          p0, t0, znu, dnw, rdnw,           &
                          rdn, non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )



     IF (.not. non_hydrostatic) THEN

!zzma begining

     print*,'TEST_diagnose_w = ',TEST_diagnose_w
     if(TEST_diagnose_w) &
     CALL t_diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                      u_2, v_2, ht,                           &
                      cf1, cf2, cf3, rdx, rdy, msft,          &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij),       &
                      grid%j_start(ij), grid%j_end(ij),       &
                      k_start    , k_end                     )

   print*,'diagnose_w zzma24'
!   stop
!zzma end


     CALL diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                      u_2, v_2, ht,                           &
                      cf1, cf2, cf3, rdx, rdy, msft,          &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij),       &
                      grid%j_start(ij), grid%j_end(ij),       &
                      k_start    , k_end                     )
     ENDIF


   ENDDO
   !$OMP END PARALLEL DO

!  Reset the boundary conditions if there is another corrector step.
!  (rk_step < rk_order), else well handle it at the end of everything
!  (after the split physics, before exiting the timestep).

   rk_step_1_check: IF ( rk_step < rk_order ) THEN

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  heres where we need a wide comm stencil - these are the 
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!
! u_2                              x
! v_2                              x
! w_2                              x
! t_2                              x
! ph_2                             x
! al         x
!
!  2D variable
! mu_2       x
!
!  4D variable
! moist_2               x
! chem_2                x



   if(dyn_opt == DYN_EM) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    tile_bc_loop_1: DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

      CALL rk_phys_bc_dry_2( config_flags,                         &
                             u_2, v_2, w_2,                    &
                             t_2, ph_2, mu_2,                  &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

        moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
  
          CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,             &
                                   ims, ime, jms, jme, kms, kme,             &
                                   ips, ipe, jps, jpe, kps, kpe,             &
                                   grid%i_start(ij), grid%i_end(ij),                   &
                                   grid%j_start(ij), grid%j_end(ij),                   &
                                   k_start    , k_end                       )
         END DO moisture_loop_bdy_1

      ENDIF

      IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

        chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

          CALL set_physical_bc3d( chem_2(ims,kms,jms,ic), 'p', config_flags,   &
                                  ids, ide, jds, jde, kds, kde,            &
                                  ims, ime, jms, jme, kms, kme,            &
                                  ips, ipe, jps, jpe, kps, kpe,            &
                                  grid%i_start(ij), grid%i_end(ij),                  &
                                  grid%j_start(ij), grid%j_end(ij),                  &
                                  k_start    , k_end-1                    )

        END DO chem_species_bdy_loop_1

      END IF

      IF (km_opt .eq. 2) THEN

        CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                ips, ipe, jps, jpe, kps, kpe,            &
                                grid%i_start(ij), grid%i_end(ij),        &
                                grid%j_start(ij), grid%j_end(ij),        &
                                k_start    , k_end                      )
      END IF

    END DO tile_bc_loop_1
   !$OMP END PARALLEL DO
   endif




   ENDIF rk_step_1_check


!**********************************************************
!
!  end of RK predictor-corrector loop
!
!**********************************************************

 END DO Runge_Kutta_loop

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles


      CALL wrf_debug ( 200 , ' call advance_ppt' )
      CALL advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN, &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,    &
                     CUPPT, config_flags,                   &
                     ids,ide, jds,jde, kds,kde,             &
                     ims,ime, jms,jme, kms,kme,             &
                     grid%i_start(ij), grid%i_end(ij),      &
                     grid%j_start(ij), grid%j_end(ij),      &
                     k_start    , k_end                    )


   ENDDO
   !$OMP END PARALLEL DO

!<DESCRIPTION>
!<pre>
! (5) time-split physics.
!
!     Microphysics are the only time  split physics in the WRF model 
!     at this time.  Split-physics begins with the calculation of
!     needed diagnostic quantities (pressure, temperature, etc.)
!     followed by a call to the microphysics driver, 
!     and finishes with a clean-up, storing off of a diabatic tendency
!     from the moist physics, and a re-calulation of the  diagnostic
!     quantities pressure and density.
!</pre>
!</DESCRIPTION>

  IF (config_flags%mp_physics /= 0)  then

   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF

   if(dyn_opt == DYN_EM) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL wrf_debug ( 200 , ' call moist_physics_prep' )

       CALL moist_physics_prep_em( t_2, t_1, t0, rho,                &
                                   al, alb, p, p8w, p0, pb,          &
                                   ph_2, phb, pi_phy, p_phy,         &
                                   z, z_at_w, dz8w,                  &
                                   dtm, h_diabatic,                  &
                                   config_flags,fnm, fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

   END DO scalar_tile_loop_1a
   !$OMP END PARALLEL DO

       CALL wrf_debug ( 200 , ' call microphysics_driver' )


       sr = 0.
       CALL microphysics_driver(t_2,moist_2, moist_1, w_2,                  &
                                rho, pi_phy, p_phy, RAINNC, RAINNCV,        &
                                z, ht, dz8w, p8w, dtm, dx, dy,              &
                                config_flags, spec_zone,                    &
                                num_3d_m, warm_rain,                        &
                                XLAND,itimestep,                            & 
                                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
                                LOWLYR,SR,                                  &
                                ids, ide, jds, jde, kds, kde,               &
                                ims, ime, jms, jme, kms, kme,               &
                                grid%i_start, min(grid%i_end, ide-1),       &
                                grid%j_start, min(grid%j_end, jde-1),       &
                                k_start    , min(k_end,kde-1) , grid%num_tiles )

   endif

       CALL wrf_debug ( 200 , ' call moist_physics_finish' )

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1b: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL moist_physics_finish_em( t_2, t_1, t0, muts,               &
                                     h_diabatic, dtm, config_flags,    &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte,               &
                                     k_start    , k_end               )


       CALL calc_p_rho_phi( moist_2, num_3d_m,                &
                            al, alb, mu_2, muts,              &
                            ph_2, p, pb, t_2,                 &
                            p0, t0, znu, dnw, rdnw,           &
                            rdn, non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            its, ite, jts, jte,               &
                            k_start    , k_end               )

       IF (.not. non_hydrostatic)                               &
       CALL diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                        u_2, v_2, ht,                           &
                        cf1, cf2, cf3, rdx, rdy, msft,          &
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        its, ite, jts, jte,                     &
                        k_start    , k_end                     )

   END DO scalar_tile_loop_1b
   !$OMP END PARALLEL DO


  ENDIF

   scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call scalar_tile_loop_2' )

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then

!
!  tiled chemistry not here, it is called from solve_interface, and found in chem_driver
!

     END IF

   END DO scalar_tile_loop_2


   if(dyn_opt == DYN_EM) then
   IF (leapfrog ) THEN

    ! do time filter and switch for the dry variables

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    DO ij = 1 , grid%num_tiles


      call time_filter( u_1, u_2, u_save,                 &
                        v_1, v_2, v_save,                 &
                        w_1, w_2, w_save,                 &
                        t_1, t_2, t_save,                 &
                        ph_1, ph_2, ph_save,              &
                        mu_1, mu_2, mu_save,              &
                        epsts,                            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


    ENDDO
   !$OMP END PARALLEL DO

    END IF
    endif

   !  Were finished except for boundary condition (and patch) update

   ! Boundary condition time (or communication time).  At this time, we have
   ! implemented periodic and symmetric physical boundary conditions.

   ! b.c. routine for data within patch.

   ! we need to do both time levels of 
   ! data because the time filter only works in the physical solution space.

   ! First, do patch communications for boundary conditions (periodicity)

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  heres where we need a wide comm stencil - these are the 
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!   u_1                            x
!   u_2                            x
!   v_1                            x
!   v_2                            x
!   w_1                            x
!   w_2                            x
!   t_1                            x
!   t_2                            x
!  ph_1                            x
!  ph_2                            x
!  tke_1                           x
!  tke_2                           x
!
!    2D variables
!  mu_1     x
!  mu_2     x
!
!    4D variables
!  moist_1                         x
!  moist_2                         x
!   chem_1                         x
!   chem_2                         x
!----------------------------------------------------------



!  now set physical b.c on a patch


   if(dyn_opt == DYN_EM) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             u_1, u_2, v_1, v_2, w_1, w_2,           &
                             t_1, t_2, ph_1, ph_2, mu_1, mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL set_physical_bc3d( tke_1(ims,kms,jms), 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )
     CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL set_physical_bc3d( moist_1(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )
       CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

       CALL set_physical_bc3d( chem_1(ims,kms,jms,ic), 'p', config_flags,   &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                    )
       CALL set_physical_bc3d( chem_2(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO
   endif


   IF( config_flags%specified .or. config_flags%nested ) THEN 
     dtbc = dtbc + dt
   ENDIF



! add diagnoses. hans_xyh huang.

   DO ij = 1 , grid%num_tiles

     call calc_dpsdt ( itimestep, dt, psfc, psfcm, 'ps', &
                       avg_abs_dpsdt, dpsdt,             &
                       ids, ide, jds, jde,               &
                       ims, ime, jms, jme,               &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij)  )
     call calc_dpsdt ( itimestep, dt, mu_2, mu_1, 'mu',  &
                       avg_abs_dmudt, dmudt,             &
                       ids, ide, jds, jde,               &
                       ims, ime, jms, jme,               &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij)  )

     call calc_q_conv_3d ( moist_1, moist_2, dt,             &
                           q_conve_3d,                       &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                    )
     call calc_q_conv_2d ( moist_2, u_2, v_2,                &
                           dt, DX, q_conve_2d,               &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                    )

   ENDDO

   CALL wrf_debug ( 200 , ' call end of solve_em' )

! Finish timers if compiled with -DBENCH.



















































! See comment before earlier #include of this file.


!STARTOFREGISTRYGENERATEDINCLUDE inc/em_scalar_derefs.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS



 grid%cfn    = cfn
 grid%cfn1    = cfn1
 grid%epsts    = epsts
 grid%step_number    = step_number
 grid%rdx    = rdx
 grid%rdy    = rdy
 grid%dts    = dts
 grid%dtseps    = dtseps
 grid%resm    = resm
 grid%zetatop    = zetatop
 grid%cf1    = cf1
 grid%cf2    = cf2
 grid%cf3    = cf3
 grid%number_at_same_level    = number_at_same_level
 grid%itimestep    = itimestep
 grid%avg_abs_dpsdt    = avg_abs_dpsdt
 grid%avg_abs_dmudt    = avg_abs_dmudt
 grid%oid    = oid
 grid%auxhist1_oid    = auxhist1_oid
 grid%auxhist2_oid    = auxhist2_oid
 grid%auxhist3_oid    = auxhist3_oid
 grid%auxhist4_oid    = auxhist4_oid
 grid%auxhist5_oid    = auxhist5_oid
 grid%auxinput1_oid    = auxinput1_oid
 grid%auxinput2_oid    = auxinput2_oid
 grid%auxinput3_oid    = auxinput3_oid
 grid%auxinput4_oid    = auxinput4_oid
 grid%auxinput5_oid    = auxinput5_oid
 grid%lbc_fid    = lbc_fid
 grid%tiled    = tiled
 grid%patched    = patched
 grid%dtbc    = dtbc
 grid%ifndsnowh    = ifndsnowh
 grid%ifndsoilw    = ifndsoilw
 grid%u_frame    = u_frame
 grid%v_frame    = v_frame
 grid%p_top    = p_top
 grid%imicrogram    = imicrogram
 grid%em_lat_ll_t = lat_ll_t
 grid%em_lat_ul_t = lat_ul_t
 grid%em_lat_ur_t = lat_ur_t
 grid%em_lat_lr_t = lat_lr_t
 grid%em_lat_ll_u = lat_ll_u
 grid%em_lat_ul_u = lat_ul_u
 grid%em_lat_ur_u = lat_ur_u
 grid%em_lat_lr_u = lat_lr_u
 grid%em_lat_ll_v = lat_ll_v
 grid%em_lat_ul_v = lat_ul_v
 grid%em_lat_ur_v = lat_ur_v
 grid%em_lat_lr_v = lat_lr_v
 grid%em_lat_ll_d = lat_ll_d
 grid%em_lat_ul_d = lat_ul_d
 grid%em_lat_ur_d = lat_ur_d
 grid%em_lat_lr_d = lat_lr_d
 grid%em_lon_ll_t = lon_ll_t
 grid%em_lon_ul_t = lon_ul_t
 grid%em_lon_ur_t = lon_ur_t
 grid%em_lon_lr_t = lon_lr_t
 grid%em_lon_ll_u = lon_ll_u
 grid%em_lon_ul_u = lon_ul_u
 grid%em_lon_ur_u = lon_ur_u
 grid%em_lon_lr_u = lon_lr_u
 grid%em_lon_ll_v = lon_ll_v
 grid%em_lon_ul_v = lon_ul_v
 grid%em_lon_ur_v = lon_ur_v
 grid%em_lon_lr_v = lon_lr_v
 grid%em_lon_ll_d = lon_ll_d
 grid%em_lon_ul_d = lon_ul_d
 grid%em_lon_ur_d = lon_ur_d
 grid%em_lon_lr_d = lon_lr_d
 grid%stepcu    = stepcu
 grid%stepra    = stepra
 grid%stepbl    = stepbl
 grid%warm_rain    = warm_rain
 grid%moved    = moved
 grid%run_days    = run_days
 grid%run_hours    = run_hours
 grid%run_minutes    = run_minutes
 grid%run_seconds    = run_seconds
 grid%start_year    = start_year
 grid%start_month    = start_month
 grid%start_day    = start_day
 grid%start_hour    = start_hour
 grid%start_minute    = start_minute
 grid%start_second    = start_second
 grid%end_year    = end_year
 grid%end_month    = end_month
 grid%end_day    = end_day
 grid%end_hour    = end_hour
 grid%end_minute    = end_minute
 grid%end_second    = end_second
 grid%interval_seconds    = interval_seconds
 grid%input_from_file    = input_from_file
 grid%input_from_hires    = input_from_hires
 grid%history_interval    = history_interval
 grid%frames_per_outfile    = frames_per_outfile
 grid%frames_per_auxhist1    = frames_per_auxhist1
 grid%frames_per_auxhist2    = frames_per_auxhist2
 grid%frames_per_auxhist3    = frames_per_auxhist3
 grid%frames_per_auxhist4    = frames_per_auxhist4
 grid%frames_per_auxhist5    = frames_per_auxhist5
 grid%restart    = restart
 grid%restart_interval    = restart_interval
 grid%io_form_input    = io_form_input
 grid%io_form_history    = io_form_history
 grid%io_form_restart    = io_form_restart
 grid%io_form_boundary    = io_form_boundary
 grid%debug_level    = debug_level
 grid%history_outname    = history_outname
 grid%auxhist1_outname    = auxhist1_outname
 grid%auxhist2_outname    = auxhist2_outname
 grid%auxhist3_outname    = auxhist3_outname
 grid%auxhist4_outname    = auxhist4_outname
 grid%auxhist5_outname    = auxhist5_outname
 grid%history_inname    = history_inname
 grid%auxhist1_inname    = auxhist1_inname
 grid%auxhist2_inname    = auxhist2_inname
 grid%auxhist3_inname    = auxhist3_inname
 grid%auxhist4_inname    = auxhist4_inname
 grid%auxhist5_inname    = auxhist5_inname
 grid%auxinput1_outname    = auxinput1_outname
 grid%auxinput2_outname    = auxinput2_outname
 grid%auxinput3_outname    = auxinput3_outname
 grid%auxinput4_outname    = auxinput4_outname
 grid%auxinput5_outname    = auxinput5_outname
 grid%auxinput1_inname    = auxinput1_inname
 grid%auxinput2_inname    = auxinput2_inname
 grid%auxinput3_inname    = auxinput3_inname
 grid%auxinput4_inname    = auxinput4_inname
 grid%auxinput5_inname    = auxinput5_inname
 grid%history_interval_mo    = history_interval_mo
 grid%history_interval_d    = history_interval_d
 grid%history_interval_h    = history_interval_h
 grid%history_interval_m    = history_interval_m
 grid%history_interval_s    = history_interval_s
 grid%inputout_interval_mo    = inputout_interval_mo
 grid%inputout_interval_d    = inputout_interval_d
 grid%inputout_interval_h    = inputout_interval_h
 grid%inputout_interval_m    = inputout_interval_m
 grid%inputout_interval_s    = inputout_interval_s
 grid%inputout_interval    = inputout_interval
 grid%auxhist1_interval_mo    = auxhist1_interval_mo
 grid%auxhist1_interval_d    = auxhist1_interval_d
 grid%auxhist1_interval_h    = auxhist1_interval_h
 grid%auxhist1_interval_m    = auxhist1_interval_m
 grid%auxhist1_interval_s    = auxhist1_interval_s
 grid%auxhist1_interval    = auxhist1_interval
 grid%auxhist2_interval_mo    = auxhist2_interval_mo
 grid%auxhist2_interval_d    = auxhist2_interval_d
 grid%auxhist2_interval_h    = auxhist2_interval_h
 grid%auxhist2_interval_m    = auxhist2_interval_m
 grid%auxhist2_interval_s    = auxhist2_interval_s
 grid%auxhist2_interval    = auxhist2_interval
 grid%auxhist3_interval_mo    = auxhist3_interval_mo
 grid%auxhist3_interval_d    = auxhist3_interval_d
 grid%auxhist3_interval_h    = auxhist3_interval_h
 grid%auxhist3_interval_m    = auxhist3_interval_m
 grid%auxhist3_interval_s    = auxhist3_interval_s
 grid%auxhist3_interval    = auxhist3_interval
 grid%auxhist4_interval_mo    = auxhist4_interval_mo
 grid%auxhist4_interval_d    = auxhist4_interval_d
 grid%auxhist4_interval_h    = auxhist4_interval_h
 grid%auxhist4_interval_m    = auxhist4_interval_m
 grid%auxhist4_interval_s    = auxhist4_interval_s
 grid%auxhist4_interval    = auxhist4_interval
 grid%auxhist5_interval_mo    = auxhist5_interval_mo
 grid%auxhist5_interval_d    = auxhist5_interval_d
 grid%auxhist5_interval_h    = auxhist5_interval_h
 grid%auxhist5_interval_m    = auxhist5_interval_m
 grid%auxhist5_interval_s    = auxhist5_interval_s
 grid%auxhist5_interval    = auxhist5_interval
 grid%auxinput1_interval_mo    = auxinput1_interval_mo
 grid%auxinput1_interval_d    = auxinput1_interval_d
 grid%auxinput1_interval_h    = auxinput1_interval_h
 grid%auxinput1_interval_m    = auxinput1_interval_m
 grid%auxinput1_interval_s    = auxinput1_interval_s
 grid%auxinput1_interval    = auxinput1_interval
 grid%auxinput2_interval_mo    = auxinput2_interval_mo
 grid%auxinput2_interval_d    = auxinput2_interval_d
 grid%auxinput2_interval_h    = auxinput2_interval_h
 grid%auxinput2_interval_m    = auxinput2_interval_m
 grid%auxinput2_interval_s    = auxinput2_interval_s
 grid%auxinput2_interval    = auxinput2_interval
 grid%auxinput3_interval_mo    = auxinput3_interval_mo
 grid%auxinput3_interval_d    = auxinput3_interval_d
 grid%auxinput3_interval_h    = auxinput3_interval_h
 grid%auxinput3_interval_m    = auxinput3_interval_m
 grid%auxinput3_interval_s    = auxinput3_interval_s
 grid%auxinput3_interval    = auxinput3_interval
 grid%auxinput4_interval_mo    = auxinput4_interval_mo
 grid%auxinput4_interval_d    = auxinput4_interval_d
 grid%auxinput4_interval_h    = auxinput4_interval_h
 grid%auxinput4_interval_m    = auxinput4_interval_m
 grid%auxinput4_interval_s    = auxinput4_interval_s
 grid%auxinput4_interval    = auxinput4_interval
 grid%auxinput5_interval_mo    = auxinput5_interval_mo
 grid%auxinput5_interval_d    = auxinput5_interval_d
 grid%auxinput5_interval_h    = auxinput5_interval_h
 grid%auxinput5_interval_m    = auxinput5_interval_m
 grid%auxinput5_interval_s    = auxinput5_interval_s
 grid%auxinput5_interval    = auxinput5_interval
 grid%restart_interval_mo    = restart_interval_mo
 grid%restart_interval_d    = restart_interval_d
 grid%restart_interval_h    = restart_interval_h
 grid%restart_interval_m    = restart_interval_m
 grid%restart_interval_s    = restart_interval_s
 grid%history_begin_y    = history_begin_y
 grid%history_begin_mo    = history_begin_mo
 grid%history_begin_d    = history_begin_d
 grid%history_begin_h    = history_begin_h
 grid%history_begin_m    = history_begin_m
 grid%history_begin_s    = history_begin_s
 grid%inputout_begin_y    = inputout_begin_y
 grid%inputout_begin_mo    = inputout_begin_mo
 grid%inputout_begin_d    = inputout_begin_d
 grid%inputout_begin_h    = inputout_begin_h
 grid%inputout_begin_m    = inputout_begin_m
 grid%inputout_begin_s    = inputout_begin_s
 grid%auxhist1_begin_y    = auxhist1_begin_y
 grid%auxhist1_begin_mo    = auxhist1_begin_mo
 grid%auxhist1_begin_d    = auxhist1_begin_d
 grid%auxhist1_begin_h    = auxhist1_begin_h
 grid%auxhist1_begin_m    = auxhist1_begin_m
 grid%auxhist1_begin_s    = auxhist1_begin_s
 grid%auxhist2_begin_y    = auxhist2_begin_y
 grid%auxhist2_begin_mo    = auxhist2_begin_mo
 grid%auxhist2_begin_d    = auxhist2_begin_d
 grid%auxhist2_begin_h    = auxhist2_begin_h
 grid%auxhist2_begin_m    = auxhist2_begin_m
 grid%auxhist2_begin_s    = auxhist2_begin_s
 grid%auxhist3_begin_y    = auxhist3_begin_y
 grid%auxhist3_begin_mo    = auxhist3_begin_mo
 grid%auxhist3_begin_d    = auxhist3_begin_d
 grid%auxhist3_begin_h    = auxhist3_begin_h
 grid%auxhist3_begin_m    = auxhist3_begin_m
 grid%auxhist3_begin_s    = auxhist3_begin_s
 grid%auxhist4_begin_y    = auxhist4_begin_y
 grid%auxhist4_begin_mo    = auxhist4_begin_mo
 grid%auxhist4_begin_d    = auxhist4_begin_d
 grid%auxhist4_begin_h    = auxhist4_begin_h
 grid%auxhist4_begin_m    = auxhist4_begin_m
 grid%auxhist4_begin_s    = auxhist4_begin_s
 grid%auxhist5_begin_y    = auxhist5_begin_y
 grid%auxhist5_begin_mo    = auxhist5_begin_mo
 grid%auxhist5_begin_d    = auxhist5_begin_d
 grid%auxhist5_begin_h    = auxhist5_begin_h
 grid%auxhist5_begin_m    = auxhist5_begin_m
 grid%auxhist5_begin_s    = auxhist5_begin_s
 grid%auxinput1_begin_y    = auxinput1_begin_y
 grid%auxinput1_begin_mo    = auxinput1_begin_mo
 grid%auxinput1_begin_d    = auxinput1_begin_d
 grid%auxinput1_begin_h    = auxinput1_begin_h
 grid%auxinput1_begin_m    = auxinput1_begin_m
 grid%auxinput1_begin_s    = auxinput1_begin_s
 grid%auxinput2_begin_y    = auxinput2_begin_y
 grid%auxinput2_begin_mo    = auxinput2_begin_mo
 grid%auxinput2_begin_d    = auxinput2_begin_d
 grid%auxinput2_begin_h    = auxinput2_begin_h
 grid%auxinput2_begin_m    = auxinput2_begin_m
 grid%auxinput2_begin_s    = auxinput2_begin_s
 grid%auxinput3_begin_y    = auxinput3_begin_y
 grid%auxinput3_begin_mo    = auxinput3_begin_mo
 grid%auxinput3_begin_d    = auxinput3_begin_d
 grid%auxinput3_begin_h    = auxinput3_begin_h
 grid%auxinput3_begin_m    = auxinput3_begin_m
 grid%auxinput3_begin_s    = auxinput3_begin_s
 grid%auxinput4_begin_y    = auxinput4_begin_y
 grid%auxinput4_begin_mo    = auxinput4_begin_mo
 grid%auxinput4_begin_d    = auxinput4_begin_d
 grid%auxinput4_begin_h    = auxinput4_begin_h
 grid%auxinput4_begin_m    = auxinput4_begin_m
 grid%auxinput4_begin_s    = auxinput4_begin_s
 grid%auxinput5_begin_y    = auxinput5_begin_y
 grid%auxinput5_begin_mo    = auxinput5_begin_mo
 grid%auxinput5_begin_d    = auxinput5_begin_d
 grid%auxinput5_begin_h    = auxinput5_begin_h
 grid%auxinput5_begin_m    = auxinput5_begin_m
 grid%auxinput5_begin_s    = auxinput5_begin_s
 grid%restart_begin_y    = restart_begin_y
 grid%restart_begin_mo    = restart_begin_mo
 grid%restart_begin_d    = restart_begin_d
 grid%restart_begin_h    = restart_begin_h
 grid%restart_begin_m    = restart_begin_m
 grid%restart_begin_s    = restart_begin_s
 grid%history_end_y    = history_end_y
 grid%history_end_mo    = history_end_mo
 grid%history_end_d    = history_end_d
 grid%history_end_h    = history_end_h
 grid%history_end_m    = history_end_m
 grid%history_end_s    = history_end_s
 grid%inputout_end_y    = inputout_end_y
 grid%inputout_end_mo    = inputout_end_mo
 grid%inputout_end_d    = inputout_end_d
 grid%inputout_end_h    = inputout_end_h
 grid%inputout_end_m    = inputout_end_m
 grid%inputout_end_s    = inputout_end_s
 grid%auxhist1_end_y    = auxhist1_end_y
 grid%auxhist1_end_mo    = auxhist1_end_mo
 grid%auxhist1_end_d    = auxhist1_end_d
 grid%auxhist1_end_h    = auxhist1_end_h
 grid%auxhist1_end_m    = auxhist1_end_m
 grid%auxhist1_end_s    = auxhist1_end_s
 grid%auxhist2_end_y    = auxhist2_end_y
 grid%auxhist2_end_mo    = auxhist2_end_mo
 grid%auxhist2_end_d    = auxhist2_end_d
 grid%auxhist2_end_h    = auxhist2_end_h
 grid%auxhist2_end_m    = auxhist2_end_m
 grid%auxhist2_end_s    = auxhist2_end_s
 grid%auxhist3_end_y    = auxhist3_end_y
 grid%auxhist3_end_mo    = auxhist3_end_mo
 grid%auxhist3_end_d    = auxhist3_end_d
 grid%auxhist3_end_h    = auxhist3_end_h
 grid%auxhist3_end_m    = auxhist3_end_m
 grid%auxhist3_end_s    = auxhist3_end_s
 grid%auxhist4_end_y    = auxhist4_end_y
 grid%auxhist4_end_mo    = auxhist4_end_mo
 grid%auxhist4_end_d    = auxhist4_end_d
 grid%auxhist4_end_h    = auxhist4_end_h
 grid%auxhist4_end_m    = auxhist4_end_m
 grid%auxhist4_end_s    = auxhist4_end_s
 grid%auxhist5_end_y    = auxhist5_end_y
 grid%auxhist5_end_mo    = auxhist5_end_mo
 grid%auxhist5_end_d    = auxhist5_end_d
 grid%auxhist5_end_h    = auxhist5_end_h
 grid%auxhist5_end_m    = auxhist5_end_m
 grid%auxhist5_end_s    = auxhist5_end_s
 grid%auxinput1_end_y    = auxinput1_end_y
 grid%auxinput1_end_mo    = auxinput1_end_mo
 grid%auxinput1_end_d    = auxinput1_end_d
 grid%auxinput1_end_h    = auxinput1_end_h
 grid%auxinput1_end_m    = auxinput1_end_m
 grid%auxinput1_end_s    = auxinput1_end_s
 grid%auxinput2_end_y    = auxinput2_end_y
 grid%auxinput2_end_mo    = auxinput2_end_mo
 grid%auxinput2_end_d    = auxinput2_end_d
 grid%auxinput2_end_h    = auxinput2_end_h
 grid%auxinput2_end_m    = auxinput2_end_m
 grid%auxinput2_end_s    = auxinput2_end_s
 grid%auxinput3_end_y    = auxinput3_end_y
 grid%auxinput3_end_mo    = auxinput3_end_mo
 grid%auxinput3_end_d    = auxinput3_end_d
 grid%auxinput3_end_h    = auxinput3_end_h
 grid%auxinput3_end_m    = auxinput3_end_m
 grid%auxinput3_end_s    = auxinput3_end_s
 grid%auxinput4_end_y    = auxinput4_end_y
 grid%auxinput4_end_mo    = auxinput4_end_mo
 grid%auxinput4_end_d    = auxinput4_end_d
 grid%auxinput4_end_h    = auxinput4_end_h
 grid%auxinput4_end_m    = auxinput4_end_m
 grid%auxinput4_end_s    = auxinput4_end_s
 grid%auxinput5_end_y    = auxinput5_end_y
 grid%auxinput5_end_mo    = auxinput5_end_mo
 grid%auxinput5_end_d    = auxinput5_end_d
 grid%auxinput5_end_h    = auxinput5_end_h
 grid%auxinput5_end_m    = auxinput5_end_m
 grid%auxinput5_end_s    = auxinput5_end_s
 grid%io_form_auxinput1    = io_form_auxinput1
 grid%io_form_auxinput2    = io_form_auxinput2
 grid%io_form_auxinput3    = io_form_auxinput3
 grid%io_form_auxinput4    = io_form_auxinput4
 grid%io_form_auxinput5    = io_form_auxinput5
 grid%io_form_auxhist1    = io_form_auxhist1
 grid%io_form_auxhist2    = io_form_auxhist2
 grid%io_form_auxhist3    = io_form_auxhist3
 grid%io_form_auxhist4    = io_form_auxhist4
 grid%io_form_auxhist5    = io_form_auxhist5
 grid%julyr    = julyr
 grid%julday    = julday
 grid%gmt    = gmt
 grid%input_inname    = input_inname
 grid%input_outname    = input_outname
 grid%bdy_inname    = bdy_inname
 grid%bdy_outname    = bdy_outname
 grid%rst_inname    = rst_inname
 grid%rst_outname    = rst_outname
 grid%write_input    = write_input
 grid%write_restart_at_0h    = write_restart_at_0h
 grid%time_step    = time_step
 grid%time_step_fract_num    = time_step_fract_num
 grid%time_step_fract_den    = time_step_fract_den
 grid%max_dom    = max_dom
 grid%s_we    = s_we
 grid%e_we    = e_we
 grid%s_sn    = s_sn
 grid%e_sn    = e_sn
 grid%s_vert    = s_vert
 grid%e_vert    = e_vert
 grid%dx    = dx
 grid%dy    = dy
 grid%grid_id    = grid_id
 grid%parent_id    = parent_id
 grid%level    = level
 grid%i_parent_start    = i_parent_start
 grid%j_parent_start    = j_parent_start
 grid%parent_grid_ratio    = parent_grid_ratio
 grid%parent_time_step_ratio    = parent_time_step_ratio
 grid%feedback    = feedback
 grid%smooth_option    = smooth_option
 grid%ztop    = ztop
 grid%moad_grid_ratio    = moad_grid_ratio
 grid%moad_time_step_ratio    = moad_time_step_ratio
 grid%shw    = shw
 grid%coral_dist    = coral_dist
 grid%tile_sz_x    = tile_sz_x
 grid%tile_sz_y    = tile_sz_y
 grid%numtiles    = numtiles
 grid%nproc_x    = nproc_x
 grid%nproc_y    = nproc_y
 grid%irand    = irand
 grid%dt    = dt
 grid%num_moves    = num_moves
 grid%move_id    = move_id
 grid%move_interval    = move_interval
 grid%move_cd_x    = move_cd_x
 grid%move_cd_y    = move_cd_y
 grid%test_rk_step_prep    = test_rk_step_prep
 grid%test_init_zero_tendency    = test_init_zero_tendency
 grid%test_phy_prep    = test_phy_prep
 grid%test_calculate_km_kh    = test_calculate_km_kh
 grid%test_rk_tendency    = test_rk_tendency
 grid%test_advect_scalar    = test_advect_scalar
 grid%test_relax_bdy_dry    = test_relax_bdy_dry
 grid%test_rk_addtend_dry    = test_rk_addtend_dry
 grid%test_spec_bdy_dry    = test_spec_bdy_dry
 grid%test_small_step_prep    = test_small_step_prep
 grid%test_calc_p_rho    = test_calc_p_rho
 grid%test_calc_coef_w    = test_calc_coef_w
 grid%test_advance_uv    = test_advance_uv
 grid%test_spec_bdyupdate    = test_spec_bdyupdate
 grid%test_advance_mu_t    = test_advance_mu_t
 grid%test_sumflux    = test_sumflux
 grid%test_advance_w    = test_advance_w
 grid%test_spec_bdyupdate_ph    = test_spec_bdyupdate_ph
 grid%test_calc_mu_uv_1    = test_calc_mu_uv_1
 grid%test_small_step_finish    = test_small_step_finish
 grid%test_rk_scalar_tend    = test_rk_scalar_tend
 grid%test_spec_bdy_scalar    = test_spec_bdy_scalar
 grid%test_rk_update_scalar    = test_rk_update_scalar
 grid%test_calc_p_rho_phi    = test_calc_p_rho_phi
 grid%test_diagnose_w    = test_diagnose_w
 grid%mp_physics    = mp_physics
 grid%ra_lw_physics    = ra_lw_physics
 grid%ra_sw_physics    = ra_sw_physics
 grid%radt    = radt
 grid%sf_sfclay_physics    = sf_sfclay_physics
 grid%sf_surface_physics    = sf_surface_physics
 grid%bl_pbl_physics    = bl_pbl_physics
 grid%bldt    = bldt
 grid%cu_physics    = cu_physics
 grid%cudt    = cudt
 grid%gsmdt    = gsmdt
 grid%isfflx    = isfflx
 grid%ifsnow    = ifsnow
 grid%icloud    = icloud
 grid%surface_input_source    = surface_input_source
 grid%num_soil_layers    = num_soil_layers
 grid%maxiens    = maxiens
 grid%maxens    = maxens
 grid%maxens2    = maxens2
 grid%maxens3    = maxens3
 grid%ensdim    = ensdim
 grid%chem_opt    = chem_opt
 grid%num_land_cat    = num_land_cat
 grid%num_soil_cat    = num_soil_cat
 grid%mp_zero_out    = mp_zero_out
 grid%mp_zero_out_thresh    = mp_zero_out_thresh
 grid%seaice_threshold    = seaice_threshold
 grid%dyn_opt    = dyn_opt
 grid%rk_ord    = rk_ord
 grid%w_damping    = w_damping
 grid%diff_opt    = diff_opt
 grid%km_opt    = km_opt
 grid%damp_opt    = damp_opt
 grid%zdamp    = zdamp
 grid%dampcoef    = dampcoef
 grid%khdif    = khdif
 grid%kvdif    = kvdif
 grid%smdiv    = smdiv
 grid%emdiv    = emdiv
 grid%epssm    = epssm
 grid%non_hydrostatic    = non_hydrostatic
 grid%time_step_sound    = time_step_sound
 grid%h_mom_adv_order    = h_mom_adv_order
 grid%v_mom_adv_order    = v_mom_adv_order
 grid%h_sca_adv_order    = h_sca_adv_order
 grid%v_sca_adv_order    = v_sca_adv_order
 grid%top_radiation    = top_radiation
 grid%mix_cr_len    = mix_cr_len
 grid%tke_upper_bound    = tke_upper_bound
 grid%kh_tke_upper_bound    = kh_tke_upper_bound
 grid%kv_tke_upper_bound    = kv_tke_upper_bound
 grid%tke_drag_coefficient    = tke_drag_coefficient
 grid%tke_heat_flux    = tke_heat_flux
 grid%pert_coriolis    = pert_coriolis
 grid%mix_full_fields    = mix_full_fields
 grid%base_pres    = base_pres
 grid%base_temp    = base_temp
 grid%base_lapse    = base_lapse
 grid%spec_bdy_width    = spec_bdy_width
 grid%spec_zone    = spec_zone
 grid%relax_zone    = relax_zone
 grid%specified    = specified
 grid%periodic_x    = periodic_x
 grid%symmetric_xs    = symmetric_xs
 grid%symmetric_xe    = symmetric_xe
 grid%open_xs    = open_xs
 grid%open_xe    = open_xe
 grid%periodic_y    = periodic_y
 grid%symmetric_ys    = symmetric_ys
 grid%symmetric_ye    = symmetric_ye
 grid%open_ys    = open_ys
 grid%open_ye    = open_ye
 grid%nested    = nested
 grid%real_data_init_type    = real_data_init_type
 grid%cen_lat    = cen_lat
 grid%cen_lon    = cen_lon
 grid%truelat1    = truelat1
 grid%truelat2    = truelat2
 grid%moad_cen_lat    = moad_cen_lat
 grid%stand_lon    = stand_lon
 grid%bdyfrq    = bdyfrq
 grid%iswater    = iswater
 grid%isice    = isice
 grid%isurban    = isurban
 grid%isoilwater    = isoilwater
 grid%map_proj    = map_proj
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN



END SUBROUTINE solve_em_tst

