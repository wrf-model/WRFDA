!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em_sn ( grid , config_flags , &
!
#include "em_dummy_args.inc"
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
#ifdef WRF_CHEM
   USE module_input_chem_data
#endif

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

   !  WRF state bcs
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! WRF state data

!#include "../phys/physics_drive.int"

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
#include <em_i1_decl.inc>


integer , dimension(grid%sm31:grid%em31,grid%sm33:grid%em31) :: xxxxx , yyyyy , zzzzz , xx1 , xx2 , xx3

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk

#include <bench_solve_em_def.h>

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

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

#include <bench_solve_em_init.h>

!
!  set leapfrog or runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  De-reference dimension information stored in the grid data structure.

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
      CALL wrf_error_fatal3 ( "solve_em.b" , 234 ,  wrf_err_message )

   END IF

!
!  Time level t is in the *_2 variable in the first part 
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
   CALL wrf_debug ( 200 , ' call rk_step_prep ' )


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles

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


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   DO ij = 1 , grid%num_tiles

!X       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

!X        CALL rk_phys_bc_dry_1( config_flags, ru, rv, rw, ww,      & 
!X                               muu, muv, mut, php, alt, p,        &
!X                               ids, ide, jds, jde, kds, kde,      &
!X                               ims, ime, jms, jme, kms, kme,      &
!X                               ips, ipe, jps, jpe, kps, kpe,      &
!X                               grid%i_start(ij), grid%i_end(ij),  &
!X                               grid%j_start(ij), grid%j_end(ij),  &
!X                               k_start, k_end                )

!X       CALL set_physical_bc3d( ph_2, 'w', config_flags,            &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                               grid%i_start(ij), grid%i_end(ij),        &
!X                               grid%j_start(ij), grid%j_end(ij),        &
!X                               k_start, k_end                )

!X   END DO
   !$OMP END PARALLEL DO


    rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).
 

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

!$TAF LOOP = PARALLEL
     DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call init_zero_tendency' )
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
!$TAF LOOP = PARALLEL
      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call phy_prep' )
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

!X         CALL radiation_driver(itimestep,dt,                         &
!X                    RTHRATENLW,RTHRATENSW,RTHRATEN,GLW,GSW,          &
!X                    SWDOWN,                                          &
!X                    XLAT,XLONG,ALBEDO,CLDFRA,EMISS,                  &
!X                    rho,moist_2,num_3d_m,                            &
!X                    p8w,p_phy,pb,pi_phy,dz8w,t_phy,t8w,              &
!X                    GMT,JULDAY,config_flags,RADT,STEPRA,ICLOUD,      &
!X                    taucldi,taucldc,warm_rain,                       &
!X                    XLAND,TSK,HTOP,HBOT,CUPPT,VEGFRA,SNOW,           &
!X                    F_ICE_PHY,F_RAIN_PHY,                            & ! for new ETARA
!X                    julyr,                                           &
!X                    1   ,                                            &
!X                    TOTSWDN,TOTLWDN,RSWTOA,RLWTOA,CZMEAN,            &
!X                    CFRACL,CFRACM,CFRACH,                            &
!X                    ACFRST,NCFRST,ACFRCV,NCFRCV,                     &
!X                    ids,ide, jds,jde, kds,kde,                       &
!X                    ims,ime, jms,jme, kms,kme,                       &
!X                    grid%i_start, min(grid%i_end, ide-1),            &
!X                    grid%j_start, min(grid%j_end, jde-1),            &
!X                    k_start    , min(k_end,kde-1) , grid%num_tiles   )




!********* Surface driver
! surface



!X      CALL wrf_debug ( 200 , ' call surface_driver' )
!X      CALL surface_driver(                                          &
!X     &           ACSNOM,ACSNOW,AKHS,AKMS,ALBEDO,BR,CANWAT,CAPG        &
!X     &          ,CHKLOWQ,CONFIG_FLAGS,DT,DX,DZ8W,DZS,EMISS,GLW        &
!X     &          ,GRDFLX,GSW,GZ1OZ0,HFX,HOL,HT,IFSNOW,ISFFLX           &
!X     &          ,ISLTYP,ITIMESTEP,IVGTYP,LOWLYR,MAVAIL,MOIST_2,MOL    &
!X     &          ,NUM_SOIL_LAYERS,NUM_3D_M,P8W,PBLH,PI_PHY,PSHLTR,PSIH &
!X     &          ,PSIM,P_PHY,Q10,Q2,QFX,QSFC,QSHLTR,QZ0,RAINBL         &
!X     &          ,RAINCV,RAINNCV,REGIME,RHO,SFCEVP,SFCEXC,SFCRUNOFF    &
!X     &          ,SMOIS,SMSTAV,SMSTOT,SNOALB,SNOW,SNOWC,SNOWH,STEPBL   &
!X     &          ,T2,TH10,TH2,THC,THZ0,TH_PHY,TMN,TSHLTR,TSK,TSLB      &
!X     &          ,T_PHY,U10,UDRUNOFF,UST,UZ0,U_FRAME,U_PHY,V10,VEGFRA  &
!X     &          ,VZ0,V_FRAME,V_PHY,WARM_RAIN,WSPD,XICE,XLAND,Z,ZNT,ZS &
!X     &          ,CT,TKE_MYJ                                           &
!X     &          ,ALBBCK,LH,SH2O,SHDMAX,SHDMIN,Z0                      &
!X     &          ,flqc,flhc,qsg,qvg,qcg,soilt1,tsnav                   & ! for RUC LSM
!X     &          ,SMFR3D,KEEPFR3DFLAG                                  &
!X     &          ,PSFC                                                 &
!X     &          ,ids,ide, jds,jde, kds,kde                            &
!X     &          ,ims,ime, jms,jme, kms,kme                            &
!X     &          ,grid%i_start, min(grid%i_end, ide-1)                 &
!X     &          ,grid%j_start, min(grid%j_end, jde-1)                 &
!X     &          ,k_start    , min(k_end,kde-1) , grid%num_tiles       )
!X

!*********
! pbl

!X      CALL wrf_debug ( 200 , ' call pbl_driver' )

!X      CALL pbl_driver(itimestep,dt,u_frame,v_frame                    &
!X     &           ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
!X     &           ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
!X     &           ,TSK,XLAND,ZNT,HT                                    &
!X     &           ,UST,HOL,MOL,PBLH                                    &
!X     &           ,HFX,QFX,REGIME,GRDFLX                               &
!X     &           ,u_phy,v_phy,th_phy,rho,moist_2                      &
!X     &           ,p_phy,pi_phy,p8w,t_phy,dz8w,z                       &
!X     &           ,TKE_MYJ,AKHS,AKMS                                   &
!X     &           ,THZ0,QZ0,UZ0,VZ0,QSFC,LOWLYR                        &
!X     &           ,PSIM, PSIH, GZ1OZ0, WSPD, BR, CHKLOWQ               &
!X     &           ,config_flags,DX,num_3d_m                            &
!X     &           ,STEPBL,warm_rain                                    &
!X     &           ,KPBL,CT,LH,SNOW,XICE                                &
!X     &           ,ids,ide, jds,jde, kds,kde                           &
!X     &           ,ims,ime, jms,jme, kms,kme                           &
!X     &           ,grid%i_start, min(grid%i_end,ide-1)                 &
!X     &           ,grid%j_start, min(grid%j_end,jde-1)                 &
!X     &           ,k_start    , min(k_end,kde-1) , grid%num_tiles  )
!X

! cumulus para.

!X          CALL wrf_debug ( 200 , ' call cumulus_driver' )


!X         CALL cumulus_driver(itimestep,dt,DX,num_3d_m,                 &
!X                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
!X                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,               &
!X                     RTHRATEN,RTHBLTEN,RQVBLTEN,                       &
!X                     RTHFTEN,RQVFTEN,                                  &
!X                     u_phy,v_phy,th_phy,t_phy,w_2,moist_2,             &
!X                     dz8w,p8w,p_phy,pi_phy,config_flags,               &
!X                     W0AVG,rho,STEPCU,                                 &
!X                     CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,warm_rain,        &
!X                     apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,      &
!X                     apr_capme,apr_capmi,                              &
!X                     HTOP,HBOT,KPBL,                                   &
!X                     MASS_FLUX,XF_ENS,PR_ENS,ht,                       & ! for Grell-Devenyi
!X                     ensdim,maxiens,maxens,maxens2,maxens3,            &
!X                     ids,ide, jds,jde, kds,kde,                        &
!X                     ims,ime, jms,jme, kms,kme,                        &
!X                     grid%i_start, min(grid%i_end, ide-1),             &
!X                     grid%j_start, min(grid%j_end, jde-1),             &
!X                     k_start    , min(k_end,kde-1) , grid%num_tiles    )


! calculate_phy_tend


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

!X      DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
!X          CALL calculate_phy_tend (config_flags,mut,pi_phy,            &
!X                     RTHRATEN,                                         &
!X                     RUBLTEN,RVBLTEN,RTHBLTEN,                         &
!X                     RQVBLTEN,RQCBLTEN,RQIBLTEN,                       &
!X                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
!X                     RQICUTEN,RQSCUTEN,                                &
!X                     ids,ide, jds,jde, kds,kde,                        &
!X                     ims,ime, jms,jme, kms,kme,                        &
!X                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
!X                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
!X                     k_start    , min(k_end,kde-1)                     )
!X
!X      ENDDO
      !$OMP END PARALLEL DO


! tke diffusion

     IF(diff_opt .eq. 2 .OR. diff_opt .eq. 1) THEN


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

!X       DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
!X          CALL compute_diff_metrics ( config_flags, ph_2, phb, z, rdz, rdzw, &
!X                                      zx, zy, rdx, rdy,                      &
!X                                      ids, ide, jds, jde, kds, kde,          &
!X                                      ims, ime, jms, jme, kms, kme,          &
!X                                      grid%i_start(ij), grid%i_end(ij),      &
!X                                      grid%j_start(ij), grid%j_end(ij),      &
!X                                      k_start    , k_end                    )
!X       ENDDO
       !$OMP END PARALLEL DO





!X       DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
!X          CALL set_physical_bc3d( rdzw , 'w', config_flags,           &
!X                                  ids, ide, jds, jde, kds, kde,       &
!X                                  ims, ime, jms, jme, kms, kme,       &
!X                                  ips, ipe, jps, jpe, kps, kpe,       &
!X                                  grid%i_start(ij), grid%i_end(ij),   &
!X                                  grid%j_start(ij), grid%j_end(ij),   &
!X                                  k_start    , k_end                 )
!X          CALL set_physical_bc3d( rdz , 'w', config_flags,           &
!X                                  ids, ide, jds, jde, kds, kde,       &
!X                                  ims, ime, jms, jme, kms, kme,       &
!X                                  ips, ipe, jps, jpe, kps, kpe,       &
!X                                  grid%i_start(ij), grid%i_end(ij),   &
!X                                  grid%j_start(ij), grid%j_end(ij),   &
!X                                  k_start    , k_end                 )
!X          CALL set_physical_bc3d( z , 'w', config_flags,           &
!X                                  ids, ide, jds, jde, kds, kde,       &
!X                                  ims, ime, jms, jme, kms, kme,       &
!X                                  ips, ipe, jps, jpe, kps, kpe,       &
!X                                  grid%i_start(ij), grid%i_end(ij),   &
!X                                  grid%j_start(ij), grid%j_end(ij),   &
!X                                  k_start    , k_end                 )
!X          CALL set_physical_bc3d( zx , 'w', config_flags,           &
!X                                  ids, ide, jds, jde, kds, kde,       &
!X                                  ims, ime, jms, jme, kms, kme,       &
!X                                  ips, ipe, jps, jpe, kps, kpe,       &
!X                                  grid%i_start(ij), grid%i_end(ij),   &
!X                                  grid%j_start(ij), grid%j_end(ij),   &
!X                                  k_start    , k_end                 )
!X          CALL set_physical_bc3d( zy , 'w', config_flags,           &
!X                                  ids, ide, jds, jde, kds, kde,       &
!X                                  ims, ime, jms, jme, kms, kme,       &
!X                                  ips, ipe, jps, jpe, kps, kpe,       &
!X                                  grid%i_start(ij), grid%i_end(ij),   &
!X                                  grid%j_start(ij), grid%j_end(ij),   &
!X                                  k_start    , k_end                 )
!X
!X       ENDDO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

!X       DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
!X          CALL cal_deform_and_div ( config_flags,u_2,v_2,w_2,div,        &
!X                                    defor11,defor22,defor33,defor12,     &
!X                                    defor13,defor23,                     &
!X                                    u_base, v_base,msfu,msfv,msft,       &
!X                                    rdx, rdy, dn, dnw, rdz, rdzw,        &
!X                                    fnm,fnp,cf1,cf2,cf3,zx,zy,           &
!X                                    ids, ide, jds, jde, kds, kde,        &
!X                                    ims, ime, jms, jme, kms, kme,        &
!X                                    grid%i_start(ij), grid%i_end(ij),    &
!X                                    grid%j_start(ij), grid%j_end(ij),    &
!X                                    k_start    , k_end                  )
!X       ENDDO
       !$OMP END PARALLEL DO






! calculate tke, kmh, and kmv


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
!X
!X The following do-loop is the only loop need for diff_opt .eq. 1 .and. km_opt .eq. 1
!X
!$TAF LOOP = PARALLEL
       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_km_kh' )
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




     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

!X     DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call phy_bc' )
!X       CALL phy_bc (config_flags,div,defor11,defor22,defor33,            &
!X                            defor12,defor13,defor23,                     &
!X                            xkmh,xkmhd,xkmv,xkhh,xkhv,                   &
!X                            tke_2(ims,kms,jms),                          &
!X                            RUBLTEN, RVBLTEN,                            &
!X                            ids, ide, jds, jde, kds, kde,                &
!X                            ims, ime, jms, jme, kms, kme,                &
!X                            ips, ipe, jps, jpe, kps, kpe,                &
!X                            grid%i_start(ij), grid%i_end(ij),                      &
!X                            grid%j_start(ij), grid%j_end(ij),                      &
!X                            k_start    , k_end                           )
!X     ENDDO
     !$OMP END PARALLEL DO





      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

!X      DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call update_phy_ten' )
!X        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
!X                          RTHRATEN,RTHBLTEN,RTHCUTEN,RUBLTEN,RVBLTEN,  &
!X                          RQVBLTEN,RQCBLTEN,RQIBLTEN,                  &
!X                          RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,&
!X                          num_3d_m,config_flags,rk_step,              &
!X                          ids, ide, jds, jde, kds, kde,                &
!X                          ims, ime, jms, jme, kms, kme,                &
!X                          grid%i_start(ij), grid%i_end(ij),                      &
!X                          grid%j_start(ij), grid%j_end(ij),                      &
!X                          k_start, k_end                               )

!X      END DO
      !$OMP END PARALLEL DO


!X     IF( diff_opt .eq. 2 .and. km_opt .eq. 2 ) THEN


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

!X       DO ij = 1 , grid%num_tiles

!X          CALL tke_rhs  ( tke_tend,BN2,                               &
!X                          config_flags,defor11,defor22,defor33,       &
!X                          defor12,defor13,defor23,u_2,v_2,w_2,div,    &
!X                          tke_2(ims,kms,jms),mut,                     &
!X                          th_phy,p_phy,p8w,t8w,z,fnm,fnp,             &
!X                          cf1,cf2,cf3,msft,xkmh,xkmv,xkhv,rdx,rdy,    &
!X                          dx,dy,dt,zx,zy,rdz,rdzw,dn,dnw,mix_cr_len,  &
!X                          ids, ide, jds, jde, kds, kde,               &
!X                          ims, ime, jms, jme, kms, kme,               &
!X                          grid%i_start(ij), grid%i_end(ij),           &
!X                          grid%j_start(ij), grid%j_end(ij),           &
!X                          k_start    , k_end                         )

!X       ENDDO
       !$OMP END PARALLEL DO


!X     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

!X     IF(diff_opt .eq. 2) THEN

!X       IF (bl_pbl_physics .eq. 0) THEN


         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
!X         DO ij = 1 , grid%num_tiles

!X           CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
!X           CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,              &
!X                                      t_tendf, tke_tend,                         &
!X                                      moist_tend, num_3d_m,                      &
!X                                      chem_tend, num_3d_c,                       &
!X                                      u_2, v_2,                                  &
!X                                      t_2,u_base,v_base,t_base,qv_base,          &
!X                                      mut,tke_2,config_flags,                    &
!X                                      defor13,defor23,defor33,                   &
!X                                      div, moist_2, chem_2, xkmv, xkhv, km_opt,  &
!X                                      fnm, fnp, dn, dnw, rdz, rdzw,              &
!X                                      ids, ide, jds, jde, kds, kde,              &
!X                                      ims, ime, jms, jme, kms, kme,              &
!X                                      grid%i_start(ij), grid%i_end(ij),          &
!X                                      grid%j_start(ij), grid%j_end(ij),          &
!X                                      k_start    , k_end                        )
!X
!X         ENDDO
         !$OMP END PARALLEL DO


!X       ENDIF
!

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
!X       DO ij = 1 , grid%num_tiles

!X          CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
!X         CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
!X                                      tke_tend,                              &
!X                                      moist_tend, num_3d_m,                  &
!X                                      chem_tend, num_3d_c,                   &
!X                                      t_2, th_phy,                           &
!X                                      mut, tke_2, config_flags,              &
!X                                      defor11, defor22, defor12,             &
!X                                      defor13, defor23, div,                 &
!X                                      moist_2, chem_2,                       &
!X                                      msfu, msfv, msft, xkmhd, xkhh, km_opt, &
!X                                      rdx, rdy, rdz, rdzw,                   &
!X                                      fnm, fnp, cf1, cf2, cf3,               &
!X                                      zx, zy, dn, dnw,                       &
!X                                      ids, ide, jds, jde, kds, kde,          &
!X                                      ims, ime, jms, jme, kms, kme,          &
!X                                      grid%i_start(ij), grid%i_end(ij),      &
!X                                      grid%j_start(ij), grid%j_end(ij),      &
!X                                      k_start    , k_end                    )
!X       ENDDO
       !$OMP END PARALLEL DO


!X     ENDIF

     END IF rk_step_is_one


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_tendency' )
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
   END DO
   !$OMP END PARALLEL DO



   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

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

!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first 
    ! predictor because we may have changed the small timestep -> dts.

      CALL wrf_debug ( 200 , ' call calc_coef_w' )

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

      IF (non_hydrostatic)                                &
      CALL calc_coef_w( a,alpha,gamma,                    &
                        mut, cqw,                         &
                        rdn, rdnw, c2a,                   &
                        dts, g, epssm,                    &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


   ENDDO
   !$OMP END PARALLEL DO






   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   DO ij = 1 , grid%num_tiles

!X         CALL set_physical_bc3d( ru_tend, 'u', config_flags,          &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )

!X         CALL set_physical_bc3d( rv_tend, 'v', config_flags,            &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )
!X
!X         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )
!X
!X         CALL set_physical_bc3d( al, 'p', config_flags,            &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )

!X         CALL set_physical_bc3d( p, 'p', config_flags,             &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )
!X
!X         CALL set_physical_bc3d( t_1, 'p', config_flags,             &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )
!X
!X         CALL set_physical_bc3d( t_save, 't', config_flags,             &
!X                                 ids, ide, jds, jde, kds, kde, &
!X                                 ims, ime, jms, jme, kms, kme, &
!X                                 ips, ipe, jps, jpe, kps, kpe, &
!X                           grid%i_start(ij), grid%i_end(ij),                 &
!X                           grid%j_start(ij), grid%j_end(ij),                 &
!X                           k_start    , k_end                     )
!X
!X         CALL set_physical_bc2d( mu_1, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )
!X
!X         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )
!X
!X         CALL set_physical_bc2d( mudf, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )
!X
!X    END DO
    !$OMP END PARALLEL DO


   small_steps : DO iteration = 1 , number_of_small_timesteps

   ! Boundary condition time (or communication time).  




      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

!$TAF LOOP = PARALLEL
      DO ij = 1 , grid%num_tiles


         CALL advance_uv ( u_2, ru_tend, v_2, rv_tend,       &
                           p, pb,                            &
                           ph_2, php, alt, al, mu_2,         &
                           muu, cqu, muv, cqv, mudf,         &
                           rdx, rdy, dts,                    &
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

!$TAF LOOP = PARALLEL
      DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate ww


        CALL advance_mu_t( ww, ww1, u_2, u_save, v_2, v_save, &
                           mu_2, mut, muave, muts, muu, muv,  &
                           mudf, ru_m, rv_m, ww_m,            &
                           t_2, t_save, t_2save, t_tend,      &
                           mu_tend,                           &
                           rdx, rdy, dts, epssm,              &
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
          CALL advance_w( w_2, rw_tend, ww, u_2, v_2,       &
                          mu_2, mut, muave, muts,           &
                          t_2save, t_2, t_save,             &
                          ph_2, ph_save, phb, ph_tend,      &
                          ht, c2a, cqw, alt, alb,           &
                          a, alpha, gamma,                  &
                          rdx, rdy, dts, t0, epssm,         &
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
!X               CALL zero_grad_bdy ( w_2,                        &
!X                                    'w'         , config_flags, &
!X                                    spec_zone,                  &
!X                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
!X                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
!X                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
!X                                    grid%i_start(ij), grid%i_end(ij),         &
!X                                    grid%j_start(ij), grid%j_end(ij),         &
!X                                    k_start    , k_end             )
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




      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

!X      DO ij = 1 , grid%num_tiles

        ! boundary condition set for next small timestep

!X         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
!X                                 ids, ide, jds, jde, kds, kde,     &
!X                                 ims, ime, jms, jme, kms, kme,     &
!X                                 ips, ipe, jps, jpe, kps, kpe,     &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij), &
!X                                 k_start    , k_end               )
!X
!X         CALL set_physical_bc3d( al, 'p', config_flags,            &
!X                                 ids, ide, jds, jde, kds, kde,     &
!X                                 ims, ime, jms, jme, kms, kme,     &
!X                                 ips, ipe, jps, jpe, kps, kpe,     &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij), &
!X                                 k_start    , k_end               )

!X         CALL set_physical_bc3d( p, 'p', config_flags,             &
!X                                 ids, ide, jds, jde, kds, kde,     &
!X                                 ims, ime, jms, jme, kms, kme,     &
!X                                 ips, ipe, jps, jpe, kps, kpe,     &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij), &
!X                                 k_start    , k_end               )

!X         CALL set_physical_bc2d( muts, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )

!X         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )
!X
!X         CALL set_physical_bc2d( mudf, 't', config_flags,          &
!X                                 ids, ide, jds, jde,               &
!X                                 ims, ime, jms, jme,               &
!X                                 ips, ipe, jps, jpe,               &
!X                                 grid%i_start(ij), grid%i_end(ij), &
!X                                 grid%j_start(ij), grid%j_end(ij) )
!X
!X      END DO
      !$OMP END PARALLEL DO


   END DO small_steps

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to 
      ! full perturbation variables.
      ! first get updated mu at u and v points


      CALL calc_mu_uv_1 ( config_flags,                     &
                          muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )


      CALL small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,     &
                              t_2, t_1, ph_2, ph_1, ww, ww1,    &
                              mu_2, mu_1,                       &
                              mut, muts, muu, muus, muv, muvs,  & 
                              u_save, v_save, w_save,           &
                              t_save, ph_save, mu_save,         &
                              msfu, msfv, msft,                 &
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

!$TAF LOOP = PARALLEL
   moist_variable_loop: do im = PARAM_FIRST_SCALAR, num_3d_m

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!$TAF LOOP = PARALLEL
   moist_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )


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

!$TAF LOOP = PARALLEL
   moist_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )


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



!X       IF( config_flags%specified ) THEN
!X         IF(im .ne. P_QV)THEN
!X           CALL flow_dep_bdy  (  moist_2(ims,kms,jms,im),                     &
!X                               ru_m, rv_m, config_flags, &
!X                               spec_zone,                  &
!X                               ids,ide, jds,jde, kds,kde,  & ! domain dims
!X                               ims,ime, jms,jme, kms,kme,  & ! memory dims
!X                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
!X                               grid%i_start(ij), grid%i_end(ij),                      &
!X                               grid%j_start(ij), grid%j_end(ij),                      &
!X                               k_start, k_end                               )
!X         ENDIF
!X       ENDIF


   ENDDO moist_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO moist_variable_loop

 ENDIF moist_scalar_advance


!X TKE_advance: IF (km_opt .eq. 2) then



   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   tke_tile_loop_1: DO ij = 1 , grid%num_tiles

!X     CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
!X     CALL rk_scalar_tend ( 1, 1, config_flags,               &
!X                           rk_step, dt_rk,                   &
!X                           ru_m, rv_m, ww_m,                 &
!X                           mut, alt,                         &
!X                           tke_1(ims,kms,jms),               &
!X                           tke_2(ims,kms,jms),               &
!X                           tke_tend(ims,kms,jms),            &
!X                           advect_tend,RQVFTEN,              &
!X                           qv_base, .false., fnm, fnp,       &
!X                           msfu, msfv, msft,                 &
!X                           rdx, rdy, rdn, rdnw, khdif,       &
!X                           kvdif, xkmhd,                     &
!X                           leapfrog,                         &
!X                           ids, ide, jds, jde, kds, kde,     &
!X                           ims, ime, jms, jme, kms, kme,     &
!X                           grid%i_start(ij), grid%i_end(ij), &
!X                           grid%j_start(ij), grid%j_end(ij), &
!X                           k_start    , k_end               )

!X   ENDDO tke_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   tke_tile_loop_2: DO ij = 1 , grid%num_tiles

!X     CALL wrf_debug ( 200 , ' call rk_update_scalar' )
!X     CALL rk_update_scalar( 1, 1,                             &
!X                            tke_1(ims,kms,jms),               &
!X                            tke_2(ims,kms,jms),               &
!X                            tke_tend(ims,kms,jms),            &
!X                            advect_tend,msft,                 &
!X                            mu_1, mu_2, mub,                  &
!X                            rk_step, dt_rk, spec_zone,        &
!X                            epsts, leapfrog,config_flags,     &
!X                            ids, ide, jds, jde, kds, kde,     &
!X                            ims, ime, jms, jme, kms, kme,     &
!X                            grid%i_start(ij), grid%i_end(ij), &
!X                            grid%j_start(ij), grid%j_end(ij), &
!X                            k_start    , k_end               ) 

! bound the tke (greater than 0, less than tke_upper_bound)

!X     CALL bound_tke( tke_2(ims,kms,jms), tke_upper_bound, &
!X                     ids, ide, jds, jde, kds, kde,        &
!X                     ims, ime, jms, jme, kms, kme,        &
!X                     grid%i_start(ij), grid%i_end(ij),    &
!X                     grid%j_start(ij), grid%j_end(ij),    &
!X                     k_start    , k_end                  )

!X     IF( config_flags%specified .or. config_flags%nested ) THEN
!X         CALL flow_dep_bdy (  tke_2(ims,kms,jms),                     &
!X                              ru_m, rv_m, config_flags,               &
!X                              spec_zone,                              &
!X                              ids,ide, jds,jde, kds,kde,  & ! domain dims
!X                              ims,ime, jms,jme, kms,kme,  & ! memory dims
!X                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
!X                              grid%i_start(ij), grid%i_end(ij),       &
!X                              grid%j_start(ij), grid%j_end(ij),       &
!X                              k_start, k_end                               )
!X     ENDIF
!X   ENDDO tke_tile_loop_2
   !$OMP END PARALLEL DO

!X   END IF TKE_advance


!  next the chemical species

!X  chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

!X   chem_variable_loop: do ic = PARAM_FIRST_SCALAR, num_3d_c

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   chem_tile_loop_1: DO ij = 1 , grid%num_tiles

!X       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
!X       CALL rk_scalar_tend ( ic, ic, config_flags,                  &
!X                             rk_step, dt_rk,                   &
!X                             ru_m, rv_m, ww_m,                 &
!X                             mut, alt,                         &
!X                             chem_1(ims,kms,jms,ic),           &
!X                             chem_2(ims,kms,jms,ic),           &
!X                             chem_tend(ims,kms,jms,ic),        &
!X                             advect_tend,RQVFTEN,              &
!X                             qv_base, .false., fnm, fnp,       &
!X                             msfu, msfv, msft,                 &
!X                             rdx, rdy, rdn, rdnw,              &
!X                             khdif, kvdif, xkmhd,              &
!X                             leapfrog,                         &
!X                             ids, ide, jds, jde, kds, kde,     &
!X                             ims, ime, jms, jme, kms, kme,     &
!X                             grid%i_start(ij), grid%i_end(ij), &
!X                             grid%j_start(ij), grid%j_end(ij), &
!X                             k_start    , k_end               )

!X   ENDDO chem_tile_loop_1
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   chem_tile_loop_2: DO ij = 1 , grid%num_tiles

!X       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
!X       CALL rk_update_scalar( ic, ic,                           &
!X                              chem_1(ims,kms,jms,ic),           &
!X                              chem_2(ims,kms,jms,ic),           &
!X                              chem_tend(ims,kms,jms,ic),        &
!X                              advect_tend, msft,                &
!X                              mu_1, mu_2, mub,                  &
!X                              rk_step, dt_rk, spec_zone,        &
!X                              epsts, leapfrog,config_flags,     &
!X                              ids, ide, jds, jde, kds, kde,     &
!X                              ims, ime, jms, jme, kms, kme,     &
!X                              grid%i_start(ij), grid%i_end(ij), &
!X                              grid%j_start(ij), grid%j_end(ij), &
!X                              k_start    , k_end               )


!X       IF( config_flags%specified ) THEN
!X           CALL flow_dep_bdy  ( chem_2(ims,kms,jms,ic),     &
!X                                ru_m, rv_m, config_flags,   &
!X                                spec_zone,                  &
!X                                ids,ide, jds,jde, kds,kde,  & ! domain dims
!X                                ims,ime, jms,jme, kms,kme,  & ! memory dims
!X                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
!X                                grid%i_start(ij), grid%i_end(ij),  &
!X                                grid%j_start(ij), grid%j_end(ij),  &
!X                                k_start, k_end                    )
!X       ENDIF


!X   ENDDO chem_tile_loop_2
   !$OMP END PARALLEL DO

!X   ENDDO chem_variable_loop

!X ENDIF chem_scalar_advance

 !  update the pressure and density at the new time level

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
!$TAF LOOP = PARALLEL
   DO ij = 1 , grid%num_tiles


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

!X   rk_step_1_check: IF ( rk_step < rk_order ) THEN

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




   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X    tile_bc_loop_1: DO ij = 1 , grid%num_tiles

!X      CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

!X      CALL rk_phys_bc_dry_2( config_flags,                         &
!X                             u_2, v_2, w_2,                    &
!X                             t_2, ph_2, mu_2,                  &
!X                             ids, ide, jds, jde, kds, kde,     &
!X                             ims, ime, jms, jme, kms, kme,     &
!X                             ips, ipe, jps, jpe, kps, kpe,     &
!X                             grid%i_start(ij), grid%i_end(ij), &
!X                             grid%j_start(ij), grid%j_end(ij), &
!X                             k_start    , k_end               )
!X
!X      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
!X
!X        moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
!X  
!X          CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p', config_flags,   &
!X                                   ids, ide, jds, jde, kds, kde,             &
!X                                   ims, ime, jms, jme, kms, kme,             &
!X                                   ips, ipe, jps, jpe, kps, kpe,             &
!X                                   grid%i_start(ij), grid%i_end(ij),                   &
!X                                   grid%j_start(ij), grid%j_end(ij),                   &
!X                                   k_start    , k_end                       )
!X         END DO moisture_loop_bdy_1
!X
!X      ENDIF

!X      IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

!X        chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

!X          CALL set_physical_bc3d( chem_2(ims,kms,jms,ic), 'p', config_flags,   &
!X                                  ids, ide, jds, jde, kds, kde,            &
!X                                  ims, ime, jms, jme, kms, kme,            &
!X                                  ips, ipe, jps, jpe, kps, kpe,            &
!X                                  grid%i_start(ij), grid%i_end(ij),                  &
!X                                  grid%j_start(ij), grid%j_end(ij),                  &
!X                                  k_start    , k_end-1                    )

!X        END DO chem_species_bdy_loop_1

!X      END IF

!X      IF (km_opt .eq. 2) THEN

!X        CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
!X                                ids, ide, jds, jde, kds, kde,            &
!X                                ims, ime, jms, jme, kms, kme,            &
!X                                ips, ipe, jps, jpe, kps, kpe,            &
!X                                grid%i_start(ij), grid%i_end(ij),        &
!X                                grid%j_start(ij), grid%j_end(ij),        &
!X                                k_start    , k_end                      )
!X      END IF

!X    END DO tile_bc_loop_1
   !$OMP END PARALLEL DO





!X   ENDIF rk_step_1_check


!**********************************************************
!
!  end of RK predictor-corrector loop
!
!**********************************************************

 END DO Runge_Kutta_loop

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   DO ij = 1 , grid%num_tiles


!X      CALL wrf_debug ( 200 , ' call advance_ppt' )
!X      CALL advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN, &
!X                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,    &
!X                     CUPPT, config_flags,                   &
!X                     ids,ide, jds,jde, kds,kde,             &
!X                     ims,ime, jms,jme, kms,kme,             &
!X                     grid%i_start(ij), grid%i_end(ij),      &
!X                     grid%j_start(ij), grid%j_end(ij),      &
!X                     k_start    , k_end                    )


!X   ENDDO
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

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

!X   scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

!X       its = max(grid%i_start(ij),ids+sz)
!X       ite = min(grid%i_end(ij),ide-1-sz)
!X       jts = max(grid%j_start(ij),jds+sz)
!X       jte = min(grid%j_end(ij),jde-1-sz)

!X       CALL wrf_debug ( 200 , ' call moist_physics_prep' )

!X       CALL moist_physics_prep_em( t_2, t_1, t0, rho,                &
!X                                   al, alb, p, p8w, p0, pb,          &
!X                                   ph_2, phb, pi_phy, p_phy,         &
!X                                   z, z_at_w, dz8w,                  &
!X                                   dtm, h_diabatic,                  &
!X                                   config_flags,fnm, fnp,            &
!X                                   ids, ide, jds, jde, kds, kde,     &
!X                                   ims, ime, jms, jme, kms, kme,     &
!X                                   its, ite, jts, jte,               &
!X                                   k_start    , k_end               )

!X   END DO scalar_tile_loop_1a
   !$OMP END PARALLEL DO

!X       CALL wrf_debug ( 200 , ' call microphysics_driver' )


!X       sr = 0.
!X       CALL microphysics_driver(t_2,moist_2, moist_1, w_2,                  &
!X                                rho, pi_phy, p_phy, RAINNC, RAINNCV,        &
!X                                z, ht, dz8w, p8w, dtm, dx, dy,              &
!X                                config_flags, spec_zone,                    &
!X                                num_3d_m, warm_rain,                        &
!X                                XLAND,itimestep,                            & 
!X                                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
!X                                LOWLYR,SR,                                  &
!X                                ids, ide, jds, jde, kds, kde,               &
!X                                ims, ime, jms, jme, kms, kme,               &
!X                                grid%i_start, min(grid%i_end, ide-1),       &
!X                                grid%j_start, min(grid%j_end, jde-1),       &
!X                                k_start    , min(k_end,kde-1) , grid%num_tiles )
!X

!X       CALL wrf_debug ( 200 , ' call moist_physics_finish' )

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

!$TAF LOOP = PARALLEL
   scalar_tile_loop_1b: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

!X       CALL moist_physics_finish_em( t_2, t_1, t0, muts,               &
!X                                     h_diabatic, dtm, config_flags,    &
!X                                     ids, ide, jds, jde, kds, kde,     &
!X                                     ims, ime, jms, jme, kms, kme,     &
!X                                     its, ite, jts, jte,               &
!X                                     k_start    , k_end               )


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

!$TAF LOOP = PARALLEL
   scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call scalar_tile_loop_2' )

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then

!  tiled chemistry here

     END IF

   END DO scalar_tile_loop_2


!X   IF (leapfrog ) THEN

!X    ! do time filter and switch for the dry variables

!X   !$OMP PARALLEL DO   &
!X   !$OMP PRIVATE ( ij )

!X    DO ij = 1 , grid%num_tiles


!X      call time_filter( u_1, u_2, u_save,                 &
!X                        v_1, v_2, v_save,                 &
!X                        w_1, w_2, w_save,                 &
!X                        t_1, t_2, t_save,                 &
!X                        ph_1, ph_2, ph_save,              &
!X                        mu_1, mu_2, mu_save,              &
!X                        epsts,                            &
!X                        ids, ide, jds, jde, kds, kde,     &
!X                        ims, ime, jms, jme, kms, kme,     &
!X                        grid%i_start(ij), grid%i_end(ij), &
!X                        grid%j_start(ij), grid%j_end(ij), &
!X                        k_start    , k_end               )
!X

!X    ENDDO
!X   !$OMP END PARALLEL DO

!X    END IF

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


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

!X   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

!X     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

!X     CALL set_phys_bc_dry_2( config_flags,                           &
!X                             u_1, u_2, v_1, v_2, w_1, w_2,           &
!X                             t_1, t_2, ph_1, ph_2, mu_1, mu_2,       &
!X                             ids, ide, jds, jde, kds, kde,           &
!X                             ims, ime, jms, jme, kms, kme,           &
!X                             ips, ipe, jps, jpe, kps, kpe,           &
!X                             grid%i_start(ij), grid%i_end(ij),       &
!X                             grid%j_start(ij), grid%j_end(ij),       &
!X                             k_start    , k_end                     )

!X     CALL set_physical_bc3d( tke_1(ims,kms,jms), 'p', config_flags,   &
!X                             ids, ide, jds, jde, kds, kde,            &
!X                             ims, ime, jms, jme, kms, kme,            &
!X                             ips, ipe, jps, jpe, kps, kpe,            &
!X                             grid%i_start(ij), grid%i_end(ij),        &
!X                             grid%j_start(ij), grid%j_end(ij),        &
!X                             k_start    , k_end-1                    )
!X     CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
!X                             ids, ide, jds, jde, kds, kde,            &
!X                             ims, ime, jms, jme, kms, kme,            &
!X                             ips, ipe, jps, jpe, kps, kpe,            &
!X                             grid%i_start(ij), grid%i_end(ij),        &
!X                             grid%j_start(ij), grid%j_end(ij),        &
!X                             k_start    , k_end                      )

!X     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

!X       CALL set_physical_bc3d( moist_1(ims,kms,jms,im), 'p',           &
!X                               config_flags,                           &
!X                               ids, ide, jds, jde, kds, kde,           &
!X                               ims, ime, jms, jme, kms, kme,           &
!X                               ips, ipe, jps, jpe, kps, kpe,           &
!X                               grid%i_start(ij), grid%i_end(ij),       &
!X                               grid%j_start(ij), grid%j_end(ij),       &
!X                               k_start    , k_end                     )
!X       CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p',           &
!X                               config_flags,                           &
!X                               ids, ide, jds, jde, kds, kde,           &
!X                               ims, ime, jms, jme, kms, kme,           &
!X                               ips, ipe, jps, jpe, kps, kpe,           &
!X                               grid%i_start(ij), grid%i_end(ij),       &
!X                               grid%j_start(ij), grid%j_end(ij),       &
!X                               k_start    , k_end                     )

!X     END DO moisture_loop_bdy_2

!X     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

!X       CALL set_physical_bc3d( chem_1(ims,kms,jms,ic), 'p', config_flags,   &
!X                               ids, ide, jds, jde, kds, kde,            &
!X                               ims, ime, jms, jme, kms, kme,            &
!X                               ips, ipe, jps, jpe, kps, kpe,            &
!X                               grid%i_start(ij), grid%i_end(ij),                  &
!X                               grid%j_start(ij), grid%j_end(ij),                  &
!X                               k_start    , k_end-1                    )
!X       CALL set_physical_bc3d( chem_2(ims,kms,jms,ic) , 'p', config_flags,  &
!X                               ids, ide, jds, jde, kds, kde,            &
!X                               ims, ime, jms, jme, kms, kme,            &
!X                               ips, ipe, jps, jpe, kps, kpe,            &
!X                               grid%i_start(ij), grid%i_end(ij),                  &
!X                               grid%j_start(ij), grid%j_end(ij),                  &
!X                               k_start    , k_end                      )

!X     END DO chem_species_bdy_loop_2

!X   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO


   IF( config_flags%specified .or. config_flags%nested ) THEN 
     dtbc = dtbc + dt
   ENDIF





   CALL wrf_debug ( 200 , ' call end of solve_em' )























































!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_scalar_derefs.inc'
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
 grid%nframes    = nframes
 grid%lbc_fid    = lbc_fid
 grid%tiled    = tiled
 grid%patched    = patched
 grid%dtbc    = dtbc
 grid%ifndsnowh    = ifndsnowh
 grid%ifndsoilw    = ifndsoilw
 grid%u_frame    = u_frame
 grid%v_frame    = v_frame
 grid%p_top    = p_top
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
 grid%history_interval    = history_interval
 grid%frames_per_outfile    = frames_per_outfile
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

END SUBROUTINE solve_em_sn
