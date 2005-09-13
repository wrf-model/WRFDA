!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE solve_interface_tst ( grid ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   USE module_wrf_error

   IMPLICIT NONE

   INTERFACE



SUBROUTINE solve_em_tst ( grid , config_flags ,   &
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

   USE module_domain
   USE module_configure
   USE module_driver_constants

   !  Input data.
   TYPE(domain) , INTENT(INOUT)                  :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)      :: config_flags


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

END SUBROUTINE solve_em_tst


   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   INTEGER     :: idum1, idum2







   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )




   IF ( .FALSE.                         ) THEN


   ELSE IF ( config_flags%dyn_opt == DYN_EM_TST ) THEN

! zz: test solve_em ------------------------------------------------------

     CALL test_solve_em ( grid , config_flags , &

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
               )

! zz: test individual subroutines below solve_em -------------------------

!    CALL solve_em_tst ( grid , config_flags , &
!# include <em_actual_args.inc>
!               )

! zz ----------------------------------------------------------------------



! ###### 4. Edit share/solve_interface.F to add call to experimental core


   ELSE

     WRITE( wrf_err_message , * ) 'Invalid dynamics option: dyn_opt = ',config_flags%dyn_opt
     CALL wrf_error_fatal3 ( "solve_interface_tst.b" , 114 ,  TRIM ( wrf_err_message ) )
     
   END IF

END SUBROUTINE solve_interface_tst

