!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE start_domain ( grid , allowed_to_read )

   USE module_domain
   USE module_configure

   IMPLICIT NONE

   !  Input Arguments.
   TYPE (domain)          :: grid
   LOGICAL, INTENT(IN)    :: allowed_to_read
   !  Local data.
   INTEGER                :: dyn_opt
   INTEGER :: idum1, idum2











   CALL nl_get_dyn_opt( 1, dyn_opt )
  
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   IF (      dyn_opt .eq. DYN_NODYN ) THEN


   ELSE IF ( mod(dyn_opt, 100) .eq. DYN_EM ) THEN
     CALL start_domain_em( grid, allowed_to_read , &
!

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

!### 4a. edit share/start_domain.F to call domain inits for core if any





   ELSE

     WRITE(0,*)' start_domain: unknown or unimplemented dyn_opt = ',dyn_opt
     CALL wrf_error_fatal3 ( "start_domain.b" , 67 ,  ' start_domain: unknown or unimplemented dyn_opt ' ) 
   ENDIF


END SUBROUTINE start_domain

