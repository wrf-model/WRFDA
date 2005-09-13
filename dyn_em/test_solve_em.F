
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

SUBROUTINE test_solve_em ( grid , config_flags , &
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

   USE g_module_small_step_em
   USE g_module_em
   USE g_module_big_step_utilities_em
   USE g_module_bc
   USE g_module_bc_em
   USE g_module_diffusion_em

!   USE a_module_small_step_em
!   USE a_module_em
!   USE a_module_big_step_utilities_em
!   USE a_module_bc
!   USE a_module_bc_em
!   USE a_module_diffusion_em

   IMPLICIT NONE

!  INTERFACE
! include  <../share/solve_em.int>
! include  <../share/solve_em_tl.int>
! include  <../share/solve_em_ad.int>
!  END INTERFACE

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

!---------------------------------------------
!---------------------------------------------


   INTEGER :: kts  !zzma

!---------------------------------------------------------------------------------------
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: S_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: S_moist_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: S_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: S_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: S_rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: S_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: S_gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: S_xkmhd


real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ru_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_u_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rv_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_v_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_w_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ww
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_rw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_ph_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_phb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_php
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_t_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mu_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mub
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mudf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_mut
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: B_muts
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_p
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_al
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_alt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: B_moist_2
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: B_mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: B_mu_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqv_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqv_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqc_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqc_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqr_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqr_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqi_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqi_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqs_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqs_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqg_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: B_rqg_bt
real      ,DIMENSION(grid%spec_bdy_width)           :: B_fcx
real      ,DIMENSION(grid%spec_bdy_width)           :: B_gcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: B_xkmhd

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqu
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_phm
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqv
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_pm1
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_cqw
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_a
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_gamma
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_alpha
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_rho
!real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a_c2a


!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------

   REAL :: SAVE_L, COEF, ALPHA_M, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

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


















































!  zzma: new definition end

   its = grid%sp31-3 ; ite = grid%ep31+3
   kts = grid%sp32 ; kte = grid%ep32
   jts = grid%sp33-3 ; jte = grid%ep33+3
   print*,'its =',its
   print*,'ite =',ite

   print*,'jts =',jts
   print*,'jte =',jte

   print*,'kts =',kts
   print*,'kte =',kte
   print*,'num_moist =',num_moist
   print*,'grid%ed31 =',grid%ed31
   print*,'grid%ed32 =',grid%ed32
   print*,'grid%spec_bdy_width =',grid%spec_bdy_width

   S_u_1 = u_1 
   S_u_2 = u_2 
   S_v_1 = v_1 
   S_v_2 = v_2 
   S_w_1 = w_1 
   S_w_2 = w_2 
   S_ph_1 = ph_1 
   S_ph_2 = ph_2 
   S_t_1 = t_1 
   S_t_2 = t_2 
   S_mu_1 = mu_1 
   S_mu_2 = mu_2 
   S_p = p 
   S_al = al 
   S_moist_1 = moist_1 
   S_moist_2 = moist_2 
!   S_u_b = u_b 
!   S_u_bt = u_bt 
!   S_v_b = v_b 
!   S_v_bt = v_bt 
!   S_w_b = w_b 
!   S_w_bt = w_bt 
!   S_ph_b = ph_b 
!   S_ph_bt = ph_bt 
!   S_t_b = t_b 
!   S_t_bt = t_bt 
!   S_mu_b = mu_b 
!   S_mu_bt = mu_bt 
!   S_rqv_b = rqv_b 
!   S_rqv_bt = rqv_bt 

!-------------
!  goto 1234   !zzma
!-------------

!NLM

   CALL solve_em ( grid , config_flags , &
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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
    B_u_1(i,k,j) = u_1(i,k,j)
    B_u_2(i,k,j) = u_2(i,k,j) 
    B_v_1(i,k,j) = v_1(i,k,j)
    B_v_2(i,k,j) = v_2(i,k,j)
    B_w_1(i,k,j) = w_1(i,k,j)
    B_w_2(i,k,j) = w_2(i,k,j)
    B_ph_1(i,k,j) = ph_1(i,k,j)
    B_ph_2(i,k,j) = ph_2(i,k,j)
    B_t_1(i,k,j) = t_1(i,k,j)
    B_t_2(i,k,j) = t_2(i,k,j)
    B_p(i,k,j) = p(i,k,j)
    B_al(i,k,j) = al(i,k,j)
    B_z(i,k,j) = z(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
    B_mu_1(i,j) =mu_1(i,j)
    B_mu_2(i,j) =mu_2(i,j)
    B_mudf(i,j) =mudf(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
    B_moist_1(i,k,j,h) =moist_1(i,k,j,h)
    B_moist_2(i,k,j,h) =moist_2(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TCL

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!   w_bt = S_w_bt 
!   ph_b = S_ph_b 
!   ph_bt = S_ph_bt 
!   t_b = S_t_b 
!   t_bt = S_t_bt 
!   mu_b = S_mu_b 
!   mu_bt = S_mu_bt 
!   rqv_b = S_rqv_b 
!   rqv_bt = S_rqv_bt 


   g_u_1 = u_1 
   g_u_2 = u_2 
   g_v_1 = v_1 
   g_v_2 = v_2 
   g_w_1 = w_1 
   g_w_2 = w_2 
   g_ph_1 = ph_1 
   g_ph_2 = ph_2 
   g_t_1 = t_1 
   g_t_2 = t_2 
   g_mu_1 = mu_1 
   g_mu_2 = mu_2 
   g_p = p 
   g_al = al 
   g_moist_1 = moist_1 
   g_moist_2 = moist_2 
   g_u_b = 0.0 
   g_u_bt = 0.0 
   g_v_b = 0.0 
   g_v_bt = 0.0 
   g_w_b = 0.0 
   g_w_bt = 0.0 
   g_ph_b = 0.0 
   g_ph_bt = 0.0 
   g_t_b = 0.0 
   g_t_bt = 0.0 
   g_mu_b = 0.0 
   g_mu_bt = 0.0 
   g_rqv_b = 0.0 
   g_rqv_bt = 0.0 

   CALL solve_em_tl ( grid , config_flags , &
! Actual arguments generated from Registry

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_actual_args.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
grid%lu_index,grid%lu_mask,grid%em_u_1,grid%em_u_2,grid%em_g_u_1,grid%em_g_u_2,grid%em_a_u_1,grid%em_a_u_2,grid%em_ru, &
grid%em_g_ru,grid%em_a_ru,grid%em_ru_m,grid%em_g_ru_m,grid%em_a_ru_m,grid%em_ru_tend,grid%em_g_ru_tend,grid%em_a_ru_tend, &
grid%em_u_save,grid%em_g_u_save,grid%em_a_u_save,grid%em_v_1,grid%em_v_2,grid%em_g_v_1,grid%em_g_v_2,grid%em_a_v_1, &
grid%em_a_v_2,grid%em_rv,grid%em_g_rv,grid%em_a_rv,grid%em_rv_m,grid%em_g_rv_m,grid%em_a_rv_m,grid%em_rv_tend,grid%em_g_rv_tend, &
grid%em_a_rv_tend,grid%em_v_save,grid%em_g_v_save,grid%em_a_v_save,grid%em_w_1,grid%em_w_2,grid%em_g_w_1,grid%em_g_w_2, &
grid%em_a_w_1,grid%em_a_w_2,grid%em_ww,grid%em_g_ww,grid%em_a_ww,grid%em_rw,grid%em_g_rw,grid%em_a_rw,grid%em_ph_1,grid%em_ph_2, &
grid%em_g_ph_1,grid%em_g_ph_2,grid%em_a_ph_1,grid%em_a_ph_2,grid%em_phb,grid%em_g_phb,grid%em_a_phb,grid%em_phb_fine, &
grid%em_ph0,grid%em_php,grid%em_g_php,grid%em_a_php,grid%em_t_1,grid%em_t_2,grid%em_g_t_1,grid%em_g_t_2,grid%em_a_t_1, &
grid%em_a_t_2,grid%em_t_init,grid%em_tp_1,grid%em_tp_2,grid%em_t_save,grid%em_g_t_save,grid%em_a_t_save,grid%em_mu_1, &
grid%em_mu_2,grid%em_g_mu_1,grid%em_g_mu_2,grid%em_a_mu_1,grid%em_a_mu_2,grid%em_mub,grid%em_g_mub,grid%em_a_mub, &
grid%em_mub_fine,grid%em_mu0,grid%em_mudf,grid%em_g_mudf,grid%em_a_mudf,grid%em_muu,grid%em_g_muu,grid%em_a_muu,grid%em_muv, &
grid%em_g_muv,grid%em_a_muv,grid%em_mut,grid%em_g_mut,grid%em_a_mut,grid%em_muts,grid%em_g_muts,grid%em_a_muts,grid%nest_pos, &
grid%em_tke_1,grid%em_tke_2,grid%em_p,grid%em_g_p,grid%em_a_p,grid%em_al,grid%em_g_al,grid%em_a_al,grid%em_alt,grid%em_g_alt, &
grid%em_a_alt,grid%em_alb,grid%em_zx,grid%em_zy,grid%em_rdz,grid%em_rdzw,grid%em_pb,grid%em_alpha,grid%em_g_alpha, &
grid%em_a_alpha,grid%em_a,grid%em_g_a,grid%em_a_a,grid%em_gamma,grid%em_g_gamma,grid%em_a_gamma,grid%em_c2a,grid%em_g_c2a, &
grid%em_a_c2a,grid%em_rho,grid%em_g_rho,grid%em_a_rho,grid%em_phm,grid%em_g_phm,grid%em_a_phm,grid%em_cqu,grid%em_g_cqu, &
grid%em_a_cqu,grid%em_cqv,grid%em_g_cqv,grid%em_a_cqv,grid%em_cqw,grid%em_g_cqw,grid%em_a_cqw,grid%em_pm1,grid%em_g_pm1, &
grid%em_a_pm1,grid%em_fnm,grid%em_fnp,grid%em_rdnw,grid%em_rdn,grid%em_dnw,grid%em_dn,grid%em_znu,grid%em_znw,grid%em_t_base, &
grid%em_z,grid%em_g_z,grid%em_a_z,grid%q2,grid%t2,grid%th2,grid%psfc,grid%u10,grid%v10,grid%dpsdt,grid%dmudt,grid%psfcm, &
grid%q_conve_3d,grid%q_conve_2d,grid%imask_nostag,grid%imask_xstag,grid%imask_ystag,grid%imask_xystag,grid%moist_1,grid%moist_2, &
grid%g_moist_1,grid%g_moist_2,grid%a_moist_1,grid%a_moist_2,grid%chem_1,grid%chem_2,grid%em_u_b,grid%em_g_u_b,grid%em_a_u_b, &
grid%em_u_bt,grid%em_g_u_bt,grid%em_a_u_bt,grid%em_v_b,grid%em_g_v_b,grid%em_a_v_b,grid%em_v_bt,grid%em_g_v_bt,grid%em_a_v_bt, &
grid%em_w_b,grid%em_g_w_b,grid%em_a_w_b,grid%em_w_bt,grid%em_g_w_bt,grid%em_a_w_bt,grid%em_ph_b,grid%em_g_ph_b,grid%em_a_ph_b, &
grid%em_ph_bt,grid%em_g_ph_bt,grid%em_a_ph_bt,grid%em_t_b,grid%em_g_t_b,grid%em_a_t_b,grid%em_t_bt,grid%em_g_t_bt, &
grid%em_a_t_bt,grid%em_mu_b,grid%em_g_mu_b,grid%em_a_mu_b,grid%em_mu_bt,grid%em_g_mu_bt,grid%em_a_mu_bt,grid%em_rqv_b, &
grid%em_g_rqv_b,grid%em_a_rqv_b,grid%em_rqv_bt,grid%em_g_rqv_bt,grid%em_a_rqv_bt,grid%rqc_b,grid%g_rqc_b,grid%a_rqc_b, &
grid%rqc_bt,grid%g_rqc_bt,grid%a_rqc_bt,grid%rqr_b,grid%g_rqr_b,grid%a_rqr_b,grid%rqr_bt,grid%g_rqr_bt,grid%a_rqr_bt,grid%rqi_b, &
grid%g_rqi_b,grid%a_rqi_b,grid%rqi_bt,grid%g_rqi_bt,grid%a_rqi_bt,grid%rqs_b,grid%g_rqs_b,grid%a_rqs_b,grid%rqs_bt, &
grid%g_rqs_bt,grid%a_rqs_bt,grid%rqg_b,grid%g_rqg_b,grid%a_rqg_b,grid%rqg_bt,grid%g_rqg_bt,grid%a_rqg_bt,grid%fcx,grid%g_fcx, &
grid%a_fcx,grid%gcx,grid%g_gcx,grid%a_gcx,grid%sm000010,grid%sm010040,grid%sm040100,grid%sm100200,grid%sm010200,grid%soilm000, &
grid%soilm005,grid%soilm020,grid%soilm040,grid%soilm160,grid%soilm300,grid%sw000010,grid%sw010040,grid%sw040100,grid%sw100200, &
grid%sw010200,grid%soilw000,grid%soilw005,grid%soilw020,grid%soilw040,grid%soilw160,grid%soilw300,grid%st000010,grid%st010040, &
grid%st040100,grid%st100200,grid%st010200,grid%soilt000,grid%soilt005,grid%soilt020,grid%soilt040,grid%soilt160,grid%soilt300, &
grid%landmask,grid%topostdv,grid%toposlpx,grid%toposlpy,grid%shdmax,grid%shdmin,grid%snoalb,grid%slopecat,grid%toposoil, &
grid%landusef,grid%soilctop,grid%soilcbot,grid%soilcat,grid%vegcat,grid%tslb,grid%zs,grid%dzs,grid%smois,grid%sh2o,grid%xice, &
grid%smstav,grid%smstot,grid%sfcrunoff,grid%udrunoff,grid%ivgtyp,grid%isltyp,grid%vegfra,grid%sfcevp,grid%grdflx,grid%sfcexc, &
grid%acsnow,grid%acsnom,grid%snow,grid%snowh,grid%canwat,grid%sst,grid%smfr3d,grid%keepfr3dflag,grid%tke_myj,grid%exch_h, &
grid%ct,grid%thz0,grid%z0,grid%qz0,grid%uz0,grid%vz0,grid%qsfc,grid%akhs,grid%akms,grid%kpbl,grid%htop,grid%hbot,grid%cuppt, &
grid%totswdn,grid%totlwdn,grid%rswtoa,grid%rlwtoa,grid%czmean,grid%cfracl,grid%cfracm,grid%cfrach,grid%acfrst,grid%ncfrst, &
grid%acfrcv,grid%ncfrcv,grid%aer_dry,grid%aer_water,grid%f_ice_phy,grid%f_rain_phy,grid%f_rimef_phy,grid%h_diabatic,grid%msft, &
grid%msfu,grid%msfv,grid%f,grid%e,grid%sina,grid%cosa,grid%ht,grid%ht_fine,grid%ht_int,grid%ht_input,grid%tsk,grid%tsk_save, &
grid%u_base,grid%v_base,grid%qv_base,grid%z_base,grid%rthcuten,grid%rqvcuten,grid%rqrcuten,grid%rqccuten,grid%rqscuten, &
grid%rqicuten,grid%w0avg,grid%rainc,grid%rainnc,grid%raincv,grid%rainncv,grid%rainbl,grid%nca,grid%lowlyr,grid%mass_flux, &
grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,grid%apr_capma,grid%apr_capme,grid%apr_capmi,grid%xf_ens,grid%pr_ens, &
grid%rthften,grid%rqvften,grid%rthraten,grid%rthratenlw,grid%rthratensw,grid%cldfra,grid%swdown,grid%gsw,grid%glw,grid%xlat, &
grid%xlong,grid%albedo,grid%albbck,grid%emiss,grid%cldefi,grid%rublten,grid%rvblten,grid%rthblten,grid%rqvblten,grid%rqcblten, &
grid%rqiblten,grid%tmn,grid%xland,grid%znt,grid%ust,grid%rmol,grid%mol,grid%pblh,grid%capg,grid%thc,grid%hfx,grid%qfx,grid%lh, &
grid%flhc,grid%flqc,grid%qsg,grid%qvg,grid%qcg,grid%soilt1,grid%tsnav,grid%snowc,grid%mavail,grid%tkesfcf,grid%taucldi, &
grid%taucldc,grid%defor11,grid%defor22,grid%defor12,grid%defor33,grid%defor13,grid%defor23,grid%xkmv,grid%xkmh,grid%xkmhd, &
grid%g_xkmhd,grid%a_xkmhd,grid%xkhv,grid%xkhh,grid%div,grid%bn2 &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                    )

   SAVE_L =0.0
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L =SAVE_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                     + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                     + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                     + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                     + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                     + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                     + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                     + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                     + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                     + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                     + g_p(i,k,j)  * g_p(i,k,j)    &
                     + g_al(i,k,j)  * g_al(i,k,j)    &
                     + g_z(i,k,j)  * g_z(i,k,j)    

   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      SAVE_L =SAVE_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                     + g_mu_2(i,j)  * g_mu_2(i,j)    &
                     + g_mudf(i,j)  * g_mudf(i,j)    
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      SAVE_L =SAVE_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                     + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h) 
   enddo
   enddo
   enddo
   enddo

   ALPHA_M=1.
   DO NT=1,11
      ALPHA_M=0.1*ALPHA_M
      FACTOR=1.+ALPHA_M
   u_1 = FACTOR*S_u_1 
   u_2 = FACTOR*S_u_2 
   v_1 = FACTOR*S_v_1 
   v_2 = FACTOR*S_v_2 
   w_1 = FACTOR*S_w_1 
   w_2 = FACTOR*S_w_2 
   ph_1 = FACTOR*S_ph_1 
   ph_2 = FACTOR*S_ph_2 
   t_1 = FACTOR*S_t_1 
   t_2 = FACTOR*S_t_2 
   mu_1 = FACTOR*S_mu_1 
   mu_2 = FACTOR*S_mu_2 
   p = FACTOR*S_p 
   al = FACTOR*S_al 
   moist_1 = FACTOR*S_moist_1 
   moist_2 = FACTOR*S_moist_2 
!   u_b = FACTOR*S_u_b 
!   u_bt = FACTOR*S_u_bt 
!   v_b = FACTOR*S_v_b 
!   v_bt = FACTOR*S_v_bt 
!   w_b = FACTOR*S_w_b 
!   w_bt = FACTOR*S_w_bt 
!   ph_b = FACTOR*S_ph_b 
!   ph_bt = FACTOR*S_ph_bt 
!   t_b = FACTOR*S_t_b 
!   t_bt = FACTOR*S_t_bt 
!   mu_b = FACTOR*S_mu_b 
!   mu_bt = FACTOR*S_mu_bt 
!   rqv_b = FACTOR*S_rqv_b 
!   rqv_bt = FACTOR*S_rqv_bt 


   CALL solve_em ( grid , config_flags , &
! Actual arguments generated from Registry

!STARTOFREGISTRYGENERATEDINCLUDE inc/em_actual_args.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
grid%lu_index,grid%lu_mask,grid%em_u_1,grid%em_u_2,grid%em_g_u_1,grid%em_g_u_2,grid%em_a_u_1,grid%em_a_u_2,grid%em_ru, &
grid%em_g_ru,grid%em_a_ru,grid%em_ru_m,grid%em_g_ru_m,grid%em_a_ru_m,grid%em_ru_tend,grid%em_g_ru_tend,grid%em_a_ru_tend, &
grid%em_u_save,grid%em_g_u_save,grid%em_a_u_save,grid%em_v_1,grid%em_v_2,grid%em_g_v_1,grid%em_g_v_2,grid%em_a_v_1, &
grid%em_a_v_2,grid%em_rv,grid%em_g_rv,grid%em_a_rv,grid%em_rv_m,grid%em_g_rv_m,grid%em_a_rv_m,grid%em_rv_tend,grid%em_g_rv_tend, &
grid%em_a_rv_tend,grid%em_v_save,grid%em_g_v_save,grid%em_a_v_save,grid%em_w_1,grid%em_w_2,grid%em_g_w_1,grid%em_g_w_2, &
grid%em_a_w_1,grid%em_a_w_2,grid%em_ww,grid%em_g_ww,grid%em_a_ww,grid%em_rw,grid%em_g_rw,grid%em_a_rw,grid%em_ph_1,grid%em_ph_2, &
grid%em_g_ph_1,grid%em_g_ph_2,grid%em_a_ph_1,grid%em_a_ph_2,grid%em_phb,grid%em_g_phb,grid%em_a_phb,grid%em_phb_fine, &
grid%em_ph0,grid%em_php,grid%em_g_php,grid%em_a_php,grid%em_t_1,grid%em_t_2,grid%em_g_t_1,grid%em_g_t_2,grid%em_a_t_1, &
grid%em_a_t_2,grid%em_t_init,grid%em_tp_1,grid%em_tp_2,grid%em_t_save,grid%em_g_t_save,grid%em_a_t_save,grid%em_mu_1, &
grid%em_mu_2,grid%em_g_mu_1,grid%em_g_mu_2,grid%em_a_mu_1,grid%em_a_mu_2,grid%em_mub,grid%em_g_mub,grid%em_a_mub, &
grid%em_mub_fine,grid%em_mu0,grid%em_mudf,grid%em_g_mudf,grid%em_a_mudf,grid%em_muu,grid%em_g_muu,grid%em_a_muu,grid%em_muv, &
grid%em_g_muv,grid%em_a_muv,grid%em_mut,grid%em_g_mut,grid%em_a_mut,grid%em_muts,grid%em_g_muts,grid%em_a_muts,grid%nest_pos, &
grid%em_tke_1,grid%em_tke_2,grid%em_p,grid%em_g_p,grid%em_a_p,grid%em_al,grid%em_g_al,grid%em_a_al,grid%em_alt,grid%em_g_alt, &
grid%em_a_alt,grid%em_alb,grid%em_zx,grid%em_zy,grid%em_rdz,grid%em_rdzw,grid%em_pb,grid%em_alpha,grid%em_g_alpha, &
grid%em_a_alpha,grid%em_a,grid%em_g_a,grid%em_a_a,grid%em_gamma,grid%em_g_gamma,grid%em_a_gamma,grid%em_c2a,grid%em_g_c2a, &
grid%em_a_c2a,grid%em_rho,grid%em_g_rho,grid%em_a_rho,grid%em_phm,grid%em_g_phm,grid%em_a_phm,grid%em_cqu,grid%em_g_cqu, &
grid%em_a_cqu,grid%em_cqv,grid%em_g_cqv,grid%em_a_cqv,grid%em_cqw,grid%em_g_cqw,grid%em_a_cqw,grid%em_pm1,grid%em_g_pm1, &
grid%em_a_pm1,grid%em_fnm,grid%em_fnp,grid%em_rdnw,grid%em_rdn,grid%em_dnw,grid%em_dn,grid%em_znu,grid%em_znw,grid%em_t_base, &
grid%em_z,grid%em_g_z,grid%em_a_z,grid%q2,grid%t2,grid%th2,grid%psfc,grid%u10,grid%v10,grid%dpsdt,grid%dmudt,grid%psfcm, &
grid%q_conve_3d,grid%q_conve_2d,grid%imask_nostag,grid%imask_xstag,grid%imask_ystag,grid%imask_xystag,grid%moist_1,grid%moist_2, &
grid%g_moist_1,grid%g_moist_2,grid%a_moist_1,grid%a_moist_2,grid%chem_1,grid%chem_2,grid%em_u_b,grid%em_g_u_b,grid%em_a_u_b, &
grid%em_u_bt,grid%em_g_u_bt,grid%em_a_u_bt,grid%em_v_b,grid%em_g_v_b,grid%em_a_v_b,grid%em_v_bt,grid%em_g_v_bt,grid%em_a_v_bt, &
grid%em_w_b,grid%em_g_w_b,grid%em_a_w_b,grid%em_w_bt,grid%em_g_w_bt,grid%em_a_w_bt,grid%em_ph_b,grid%em_g_ph_b,grid%em_a_ph_b, &
grid%em_ph_bt,grid%em_g_ph_bt,grid%em_a_ph_bt,grid%em_t_b,grid%em_g_t_b,grid%em_a_t_b,grid%em_t_bt,grid%em_g_t_bt, &
grid%em_a_t_bt,grid%em_mu_b,grid%em_g_mu_b,grid%em_a_mu_b,grid%em_mu_bt,grid%em_g_mu_bt,grid%em_a_mu_bt,grid%em_rqv_b, &
grid%em_g_rqv_b,grid%em_a_rqv_b,grid%em_rqv_bt,grid%em_g_rqv_bt,grid%em_a_rqv_bt,grid%rqc_b,grid%g_rqc_b,grid%a_rqc_b, &
grid%rqc_bt,grid%g_rqc_bt,grid%a_rqc_bt,grid%rqr_b,grid%g_rqr_b,grid%a_rqr_b,grid%rqr_bt,grid%g_rqr_bt,grid%a_rqr_bt,grid%rqi_b, &
grid%g_rqi_b,grid%a_rqi_b,grid%rqi_bt,grid%g_rqi_bt,grid%a_rqi_bt,grid%rqs_b,grid%g_rqs_b,grid%a_rqs_b,grid%rqs_bt, &
grid%g_rqs_bt,grid%a_rqs_bt,grid%rqg_b,grid%g_rqg_b,grid%a_rqg_b,grid%rqg_bt,grid%g_rqg_bt,grid%a_rqg_bt,grid%fcx,grid%g_fcx, &
grid%a_fcx,grid%gcx,grid%g_gcx,grid%a_gcx,grid%sm000010,grid%sm010040,grid%sm040100,grid%sm100200,grid%sm010200,grid%soilm000, &
grid%soilm005,grid%soilm020,grid%soilm040,grid%soilm160,grid%soilm300,grid%sw000010,grid%sw010040,grid%sw040100,grid%sw100200, &
grid%sw010200,grid%soilw000,grid%soilw005,grid%soilw020,grid%soilw040,grid%soilw160,grid%soilw300,grid%st000010,grid%st010040, &
grid%st040100,grid%st100200,grid%st010200,grid%soilt000,grid%soilt005,grid%soilt020,grid%soilt040,grid%soilt160,grid%soilt300, &
grid%landmask,grid%topostdv,grid%toposlpx,grid%toposlpy,grid%shdmax,grid%shdmin,grid%snoalb,grid%slopecat,grid%toposoil, &
grid%landusef,grid%soilctop,grid%soilcbot,grid%soilcat,grid%vegcat,grid%tslb,grid%zs,grid%dzs,grid%smois,grid%sh2o,grid%xice, &
grid%smstav,grid%smstot,grid%sfcrunoff,grid%udrunoff,grid%ivgtyp,grid%isltyp,grid%vegfra,grid%sfcevp,grid%grdflx,grid%sfcexc, &
grid%acsnow,grid%acsnom,grid%snow,grid%snowh,grid%canwat,grid%sst,grid%smfr3d,grid%keepfr3dflag,grid%tke_myj,grid%exch_h, &
grid%ct,grid%thz0,grid%z0,grid%qz0,grid%uz0,grid%vz0,grid%qsfc,grid%akhs,grid%akms,grid%kpbl,grid%htop,grid%hbot,grid%cuppt, &
grid%totswdn,grid%totlwdn,grid%rswtoa,grid%rlwtoa,grid%czmean,grid%cfracl,grid%cfracm,grid%cfrach,grid%acfrst,grid%ncfrst, &
grid%acfrcv,grid%ncfrcv,grid%aer_dry,grid%aer_water,grid%f_ice_phy,grid%f_rain_phy,grid%f_rimef_phy,grid%h_diabatic,grid%msft, &
grid%msfu,grid%msfv,grid%f,grid%e,grid%sina,grid%cosa,grid%ht,grid%ht_fine,grid%ht_int,grid%ht_input,grid%tsk,grid%tsk_save, &
grid%u_base,grid%v_base,grid%qv_base,grid%z_base,grid%rthcuten,grid%rqvcuten,grid%rqrcuten,grid%rqccuten,grid%rqscuten, &
grid%rqicuten,grid%w0avg,grid%rainc,grid%rainnc,grid%raincv,grid%rainncv,grid%rainbl,grid%nca,grid%lowlyr,grid%mass_flux, &
grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,grid%apr_capma,grid%apr_capme,grid%apr_capmi,grid%xf_ens,grid%pr_ens, &
grid%rthften,grid%rqvften,grid%rthraten,grid%rthratenlw,grid%rthratensw,grid%cldfra,grid%swdown,grid%gsw,grid%glw,grid%xlat, &
grid%xlong,grid%albedo,grid%albbck,grid%emiss,grid%cldefi,grid%rublten,grid%rvblten,grid%rthblten,grid%rqvblten,grid%rqcblten, &
grid%rqiblten,grid%tmn,grid%xland,grid%znt,grid%ust,grid%rmol,grid%mol,grid%pblh,grid%capg,grid%thc,grid%hfx,grid%qfx,grid%lh, &
grid%flhc,grid%flqc,grid%qsg,grid%qvg,grid%qcg,grid%soilt1,grid%tsnav,grid%snowc,grid%mavail,grid%tkesfcf,grid%taucldi, &
grid%taucldc,grid%defor11,grid%defor22,grid%defor12,grid%defor33,grid%defor13,grid%defor23,grid%xkmv,grid%xkmh,grid%xkmhd, &
grid%g_xkmhd,grid%a_xkmhd,grid%xkhv,grid%xkhh,grid%div,grid%bn2 &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                    )
    VAL_N = 0.0

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N  + (u_1(i,k,j)  - B_u_1(i,k,j) ) *(u_1(i,k,j)  - B_u_1(i,k,j))     &
                   + (u_2(i,k,j)  - B_u_2(i,k,j) ) *(u_2(i,k,j)  - B_u_2(i,k,j))     &
                   + (v_1(i,k,j)  - B_v_1(i,k,j) ) *(v_1(i,k,j)  - B_v_1(i,k,j))     &
                   + (v_2(i,k,j)  - B_v_2(i,k,j) ) *(v_2(i,k,j)  - B_v_2(i,k,j))     &
                   + (w_1(i,k,j)  - B_w_1(i,k,j) ) *(w_1(i,k,j)  - B_w_1(i,k,j))     &
                   + (w_2(i,k,j)  - B_w_2(i,k,j) ) *(w_2(i,k,j)  - B_w_2(i,k,j))     &
                   + (ph_1(i,k,j)  - B_ph_1(i,k,j) ) *(ph_1(i,k,j)  - B_ph_1(i,k,j)) &
                   + (ph_2(i,k,j)  - B_ph_2(i,k,j) ) *(ph_2(i,k,j)  - B_ph_2(i,k,j)) &
                   + (t_1(i,k,j)  - B_t_1(i,k,j) ) *(t_1(i,k,j)  - B_t_1(i,k,j))     &
                   + (t_2(i,k,j)  - B_t_2(i,k,j) ) *(t_2(i,k,j)  - B_t_2(i,k,j))     &
                   + (p(i,k,j)  - B_p(i,k,j) ) *(p(i,k,j)  - B_p(i,k,j))     &
                   + (al(i,k,j)  - B_al(i,k,j) ) *(al(i,k,j)  - B_al(i,k,j))     &
                   + (z(i,k,j)  - B_z(i,k,j) ) *(z(i,k,j)  - B_z(i,k,j))     
   enddo
   enddo
   enddo


   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N  + (mu_1(i,j) -B_mu_1(i,j))*(mu_1(i,j) -B_mu_1(i,j))    &
                   + (mu_2(i,j) -B_mu_2(i,j))*(mu_2(i,j) -B_mu_2(i,j))    &
                   + (mudf(i,j) -B_mudf(i,j))*(mudf(i,j) -B_mudf(i,j))  
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_N=VAL_N  + (moist_1(i,k,j,h) -B_moist_1(i,k,j,h))*(moist_1(i,k,j,h) -B_moist_1(i,k,j,h))  &
                   + (moist_2(i,k,j,h) -B_moist_2(i,k,j,h))*(moist_2(i,k,j,h) -B_moist_2(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA_M**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_em_sn: ALPHA_M=',ALPHA_M,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test
!-------------
!1234 continue  !zzma
!-------------

   FACTOR=0.1

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!   w_bt = S_w_bt 
!   ph_b = S_ph_b 
!   ph_bt = S_ph_bt 
!   t_b = S_t_b 
!   t_bt = S_t_bt 
!   mu_b = S_mu_b 
!   mu_bt = S_mu_bt 
!   rqv_b = S_rqv_b 
!   rqv_bt = S_rqv_bt 




   g_u_1 = FACTOR*S_u_1 
   g_u_2 = FACTOR*S_u_2 
   g_v_1 = FACTOR*S_v_1 
   g_v_2 = FACTOR*S_v_2 
   g_w_1 = FACTOR*S_w_1 
   g_w_2 = FACTOR*S_w_2 
   g_ph_1 = FACTOR*S_ph_1 
   g_ph_2 = FACTOR*S_ph_2 
   g_t_1 = FACTOR*S_t_1 
   g_t_2 = FACTOR*S_t_2 
   g_mu_1 = FACTOR*S_mu_1 
   g_mu_2 = FACTOR*S_mu_2 
   g_p = FACTOR*S_p 
   g_al = FACTOR*S_al 
   g_moist_1 = FACTOR*S_moist_1 
   g_moist_2 = FACTOR*S_moist_2 
!   g_u_b = FACTOR*S_u_b 
!   g_u_bt = FACTOR*S_u_bt 
!   g_v_b = FACTOR*S_v_b 
!   g_v_bt = FACTOR*S_v_bt 
!   g_w_b = FACTOR*S_w_b 
!   g_w_bt = FACTOR*S_w_bt 
!   g_ph_b = FACTOR*S_ph_b 
!   g_ph_bt = FACTOR*S_ph_bt 
!   g_t_b = FACTOR*S_t_b 
!   g_t_bt = FACTOR*S_t_bt 
!   g_mu_b = FACTOR*S_mu_b 
!   g_mu_bt = FACTOR*S_mu_bt 
!   g_rqv_b = FACTOR*S_rqv_b 
!   g_rqv_bt = FACTOR*S_rqv_bt 

   g_u_b = 0.
   g_u_bt =0.
   g_v_b = 0.
   g_v_bt =0.
   g_w_b = 0.
   g_w_bt =0.
   g_ph_b =0.
   g_ph_bt =0.
   g_t_b = 0.
   g_t_bt =0.
   g_mu_b =0.
   g_mu_bt =0.
   g_rqv_b =0.
   g_rqv_bt =0.


!   g_u_1 =0.0
!   g_v_1 =0.0
!   g_w_1 =0.0
!   g_ph_1 =0.0
!   g_t_1 = 0.0
!   g_moist_1 =0.0
!   g_w_2 = 0.0
!   g_mu_1 =0.0

!   g_u_2 =0.0
!   g_v_2 =0.0
!   g_ph_2 =0.0
!   g_t_2 =0.0

!   g_mu_2 = 0.0
!   g_p = 0.0
!   g_al = 0.0
!   g_moist_2 = 0.0

   B_u_1 = g_u_1 
   B_u_2 = g_u_2 
   B_v_1 = g_v_1 
   B_v_2 = g_v_2 
   B_w_1 = g_w_1 
   B_w_2 = g_w_2 
   B_ph_1 = g_ph_1 
   B_ph_2 = g_ph_2 
   B_t_1 = g_t_1 
   B_t_2 = g_t_2 
   B_mu_1 = g_mu_1 
   B_mu_2 = g_mu_2 
   B_p = g_p 
   B_al = g_al 
   B_moist_1 = g_moist_1 
   B_moist_2 = g_moist_2 
!   B_u_b = g_u_b 
!   B_u_bt = g_u_bt 
!   B_v_b = g_v_b 
!   B_v_bt = g_v_bt 
!   B_w_b = g_w_b 
!   B_w_bt = g_w_bt 
!   B_ph_b = g_ph_b 
!   B_ph_bt = g_ph_bt 
!   B_t_b = g_t_b 
!   B_t_bt = g_t_bt 
!   B_mu_b = g_mu_b 
!   B_mu_bt = g_mu_bt 
!   B_rqv_b = g_rqv_b 
!   B_rqv_bt = g_rqv_bt 


   CALL solve_em_tl ( grid , config_flags , &
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

   g_z=0.; g_mudf=0.

   VAL_L =0.0
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L =VAL_L + g_u_1(i,k,j)  * g_u_1(i,k,j)    &
                     + g_u_2(i,k,j)  * g_u_2(i,k,j)    &
                     + g_v_1(i,k,j)  * g_v_1(i,k,j)    &
                     + g_v_2(i,k,j)  * g_v_2(i,k,j)    &
                     + g_w_1(i,k,j)  * g_w_1(i,k,j)    &
                     + g_w_2(i,k,j)  * g_w_2(i,k,j)    &
                     + g_ph_1(i,k,j)  * g_ph_1(i,k,j)  &
                     + g_ph_2(i,k,j)  * g_ph_2(i,k,j)  &
                     + g_t_1(i,k,j)  * g_t_1(i,k,j)    &
                     + g_t_2(i,k,j)  * g_t_2(i,k,j)    &
                     + g_p(i,k,j)  * g_p(i,k,j)    &
                     + g_al(i,k,j)  * g_al(i,k,j)    &
                     + g_z(i,k,j)  * g_z(i,k,j)

   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      VAL_L =VAL_L + g_mu_1(i,j)  * g_mu_1(i,j)    &
                     + g_mu_2(i,j)  * g_mu_2(i,j)    &
                     + g_mudf(i,j)  * g_mudf(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_L =VAL_L + g_moist_1(i,k,j,h)  * g_moist_1(i,k,j,h)    &
                     + g_moist_2(i,k,j,h)  * g_moist_2(i,k,j,h)
   enddo
   enddo
   enddo
   enddo


!  ADJ

   u_1 = S_u_1 
   u_2 = S_u_2 
   v_1 = S_v_1 
   v_2 = S_v_2 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   t_1 = S_t_1 
   t_2 = S_t_2 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   p = S_p 
   al = S_al 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
!   u_b = S_u_b 
!   u_bt = S_u_bt 
!   v_b = S_v_b 
!   v_bt = S_v_bt 
!   w_b = S_w_b 
!  w_bt = S_w_bt 
!  ph_b = S_ph_b 
!  ph_bt = S_ph_bt 
!  t_b = S_t_b 
!  t_bt = S_t_bt 
!  mu_b = S_mu_b 
!  mu_bt = S_mu_bt 
!  rqv_b = S_rqv_b 
!  rqv_bt = S_rqv_bt 




   a_u_1 = 0.
   a_u_2 = 0.
   a_v_1 = 0.
   a_v_2 =0.
   a_w_1 =0.
   a_w_2 =0.
   a_ph_1 =0.
   a_ph_2 = 0.
   a_t_1 = 0.
   a_t_2 =0.
   a_mu_1 = 0.
   a_mu_2 =0.
   a_p = 0.
   a_al =0.
   a_moist_1 = 0.
   a_moist_2 =0.
   a_u_b = 0.
   a_u_bt = 0.
   a_v_b = 0.
   a_v_bt =0.
   a_w_b = 0.
   a_w_bt =0.
   a_ph_b =0.
   a_ph_bt =0.
   a_t_b = 0.
   a_t_bt =0.
   a_mu_b =0.
   a_mu_bt =0.
   a_rqv_b =0.
   a_rqv_bt =0.

   a_u_1 = g_u_1 
   a_u_2 = g_u_2 
   a_v_1 = g_v_1 
   a_v_2 = g_v_2 
   a_w_1 = g_w_1 
   a_w_2 = g_w_2 
   a_ph_1 = g_ph_1 
   a_ph_2 = g_ph_2 
   a_t_1 = g_t_1 
   a_t_2 = g_t_2 
   a_mu_1 = g_mu_1 
   a_mu_2 = g_mu_2 
   a_mudf = g_mudf 
   a_p = g_p 
   a_al = g_al 
   a_z = g_z 
   a_moist_1 = g_moist_1 
   a_moist_2 = g_moist_2 

   CALL solve_em_ad ( grid , config_flags , &
!

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

    VAL_A=0.0

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A= VAL_A + a_u_1(i,k,j) * B_u_1(i,k,j)               &
            + a_u_2(i,k,j) * B_u_2(i,k,j)               &
            + a_v_1(i,k,j) * B_v_1(i,k,j)               &
            + a_v_2(i,k,j) * B_v_2(i,k,j)               &
            + a_w_1(i,k,j) * B_w_1(i,k,j)               &
            + a_w_2(i,k,j) * B_w_2(i,k,j)               &
            + a_ph_1(i,k,j) * B_ph_1(i,k,j)             &
            + a_ph_2(i,k,j) * B_ph_2(i,k,j)             &
            + a_t_1(i,k,j) * B_t_1(i,k,j)               &
            + a_t_2(i,k,j) * B_t_2(i,k,j)               &
            + a_p(i,k,j) * B_p(i,k,j)                   &
            + a_al(i,k,j) * B_al(i,k,j)  

   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A= VAL_A + a_mu_1(i,j) * B_mu_1(i,j)                 &
            + a_mu_2(i,j) * B_mu_2(i,j)        
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,num_moist
      VAL_A= VAL_A + a_moist_1(i,k,j,h) * B_moist_1(i,k,j,h)   &
            + a_moist_2(i,k,j,h) * B_moist_2(i,k,j,h)   
   enddo
   enddo
   enddo
   enddo
   do i=1,max(grid%ed31,grid%ed33)
   do k=kts,kte
   do j=1,grid%spec_bdy_width
   do h=1,4
!      VAL_A= VAL_A + a_u_b(i,k,j,h) * B_u_b(i,k,j,h)           &
!            + a_u_bt(i,k,j,h) * B_u_bt(i,k,j,h)         &
!            + a_v_b(i,k,j,h) * B_v_b(i,k,j,h)           &
!            + a_v_bt(i,k,j,h) * B_v_bt(i,k,j,h)         &
!            + a_w_b(i,k,j,h) * B_w_b(i,k,j,h)           &
!            + a_w_bt(i,k,j,h) * B_w_bt(i,k,j,h)         &
!            + a_ph_b(i,k,j,h) * B_ph_b(i,k,j,h)         &
!            + a_ph_bt(i,k,j,h) * B_ph_bt(i,k,j,h)       &
!            + a_t_b(i,k,j,h) * B_t_b(i,k,j,h)           &
!            + a_t_bt(i,k,j,h) * B_t_bt(i,k,j,h)         &
!            + a_mu_b(i,k,j,h) * B_mu_b(i,k,j,h)         &
!            + a_mu_bt(i,k,j,h) * B_mu_bt(i,k,j,h)       &
!            + a_rqv_b(i,k,j,h) * B_rqv_b(i,k,j,h)       &
!            + a_rqv_bt(i,k,j,h) * B_rqv_bt(i,k,j,h)  
   enddo
   enddo
   enddo
   enddo

   print*, '                '
   write(6,*) 'a_em: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

!  RECOVER

   u_1 = S_u_1 
   u_2 = S_u_2 
   ru = S_ru 
   ru_m = S_ru_m 
   ru_tend = S_ru_tend 
   u_save = S_u_save 
   v_1 = S_v_1 
   v_2 = S_v_2 
   rv = S_rv 
   rv_m = S_rv_m 
   rv_tend = S_rv_tend 
   v_save = S_v_save 
   w_1 = S_w_1 
   w_2 = S_w_2 
   ww = S_ww 
   rw = S_rw 
   ph_1 = S_ph_1 
   ph_2 = S_ph_2 
   phb = S_phb 
   php = S_php 
   t_1 = S_t_1 
   t_2 = S_t_2 
   t_save = S_t_save 
   mu_1 = S_mu_1 
   mu_2 = S_mu_2 
   mub = S_mub 
   mudf = S_mudf 
   muu = S_muu 
   muv = S_muv 
   mut = S_mut 
   muts = S_muts 
   p = S_p 
   al = S_al 
   alt = S_alt 
   z = S_z 
   moist_1 = S_moist_1 
   moist_2 = S_moist_2 
   u_b = S_u_b 
   u_bt = S_u_bt 
   v_b = S_v_b 
   v_bt = S_v_bt 
   w_b = S_w_b 
   w_bt = S_w_bt 
   ph_b = S_ph_b 
   ph_bt = S_ph_bt 
   t_b = S_t_b 
   t_bt = S_t_bt 
   mu_b = S_mu_b 
   mu_bt = S_mu_bt 
   rqv_b = S_rqv_b 
   rqv_bt = S_rqv_bt 
   rqc_b = S_rqc_b 
   rqc_bt = S_rqc_bt 
   rqr_b = S_rqr_b 
   rqr_bt = S_rqr_bt 
   rqi_b = S_rqi_b 
   rqi_bt = S_rqi_bt 
   rqs_b = S_rqs_b 
   rqs_bt = S_rqs_bt 
   rqg_b = S_rqg_b 
   rqg_bt = S_rqg_bt 
   fcx = S_fcx 
   gcx = S_gcx 
   xkmhd = S_xkmhd 
!***********************************************************
!***********************************************************
!g_em_sn: ALPHA_M=.1000E+00  COEF=   0.1002139921065E+01  VAL_N= 0.106907E+12  VAL_L= 0.106679E+12
!g_em_sn: ALPHA_M=.1000E-01  COEF=   0.1000033706173E+01  VAL_N= 0.106683E+10  VAL_L= 0.106679E+10
!g_em_sn: ALPHA_M=.1000E-02  COEF=   0.9998179439761E+00  VAL_N= 0.106660E+08  VAL_L= 0.106679E+08
!g_em_sn: ALPHA_M=.1000E-03  COEF=   0.9997966495443E+00  VAL_N= 0.106657E+06  VAL_L= 0.106679E+06
!g_em_sn: ALPHA_M=.1000E-04  COEF=   0.9997944894996E+00  VAL_N= 0.106657E+04  VAL_L= 0.106679E+04
!g_em_sn: ALPHA_M=.1000E-05  COEF=   0.9997942733074E+00  VAL_N= 0.106657E+02  VAL_L= 0.106679E+02
!g_em_sn: ALPHA_M=.1000E-06  COEF=   0.9997942529592E+00  VAL_N= 0.106657E+00  VAL_L= 0.106679E+00
!g_em_sn: ALPHA_M=.1000E-07  COEF=   0.9997942382646E+00  VAL_N= 0.106657E-02  VAL_L= 0.106679E-02
!g_em_sn: ALPHA_M=.1000E-08  COEF=   0.9997943929116E+00  VAL_N= 0.106657E-04  VAL_L= 0.106679E-04
!g_em_sn: ALPHA_M=.1000E-09  COEF=   0.9997942529716E+00  VAL_N= 0.106657E-06  VAL_L= 0.106679E-06
!g_em_sn: ALPHA_M=.1000E-10  COEF=   0.9997953239580E+00  VAL_N= 0.106657E-08  VAL_L= 0.106679E-08

! a_em:
!      VAL_TL:    0.1065110571042E+12
!      VAL_AD:    0.1065110571042E+12
!***********************************************************
!***********************************************************
!---------------------------------------------------------------------------------------

END SUBROUTINE test_solve_em



