SUBROUTINE rk_tendency ( config_flags, rk_step,                           &
                         ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, u_save, v_save, w_save, ph_save,        &
                         t_save, mu_save, RTHFTEN,                        &
                         ru, rv, rw, ww,                                  &
                         u, v, w, t, ph,                                  &
                         u_old, v_old, w_old, t_old, ph_old,              &
                         h_diabatic, phb,t_init,                          &
                         mu, mut, muu, muv, mub,                          &
                         al, alt, p, pb, php, cqu, cqv, cqw,              &
                         u_base, v_base, t_base, qv_base, z_base,         &
                         msfu, msfv, msft, f, e, sina, cosa,              &
                         fnm, fnp, rdn, rdnw,                             &
                         dt, rdx, rdy, khdif, kvdif, xkmhd,               &
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         its, ite, jts, jte, kts, kte                    )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type)    ,           INTENT(IN   ) :: config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL ,               INTENT(IN   ) :: non_hydrostatic, leapfrog

   INTEGER ,               INTENT(IN   ) :: n_moist, rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,              &
                                        INTENT(IN   ) :: ru,      &
                                                         rv,      &
                                                         rw,      &
                                                         ww,      & 
                                                         u,       &
                                                         v,       &
                                                         w,       &
                                                         t,       &
                                                         ph,      &
                                                         u_old,   &
                                                         v_old,   &
                                                         w_old,   &
                                                         t_old,   &
                                                         ph_old,  &
                                                         phb,     &
                                                         al,      &
                                                         alt,     &
                                                         p,       &
                                                         pb,      &
                                                         php,     &
                                                         cqu,     &
                                                         cqv,     &
                                                         t_init,  &
                                                         xkmhd,  &
                                                         h_diabatic

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,              &
                                        INTENT(OUT  ) :: ru_tend, &
                                                         rv_tend, &
                                                         rw_tend, &
                                                         t_tend,  &
                                                         ph_tend, &
                                                         RTHFTEN, &
                                                          u_save, &
                                                          v_save, &
                                                          w_save, &
                                                         ph_save, &
                                                          t_save

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,               &
                                        INTENT(INOUT) :: ru_tendf, &
                                                         rv_tendf, &
                                                         rw_tendf, &
                                                         t_tendf,  &
                                                         ph_tendf, &
                                                         cqw

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(  OUT) :: mu_tend, &
                                                                    mu_save

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,    &
                                                                    msfv,    &
                                                                    msft,    &
                                                                    f,       &
                                                                    e,       &
                                                                    sina,    &
                                                                    cosa,    &
                                                                    mu,      &
                                                                    mut,     &
                                                                    mub,     &
                                                                    muu,     &
                                                                    muv

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm,     &
                                                                  fnp,     &
                                                                  rdn,     &
                                                                  rdnw,    &
                                                                  u_base,  &
                                                                  v_base,  &
                                                                  t_base,  &
                                                                  qv_base, &
                                                                  z_base

   REAL ,                                      INTENT(IN   ) :: rdx,     &
                                                                rdy,     &
                                                                dt,      &
                                                                khdif,   &
                                                                kvdif

   REAL    :: kdift, khdq, kvdq, cfn, cfn1, cf1, cf2, cf3
   INTEGER :: i,j,k

!<DESCRIPTION>
!
!  rk_tendency computes the large-timestep tendency terms in the 
!  momentum, thermodynamic (theta), and geopotential equations.  
!  These terms include:
!
!  (1) advection (for u, v, w, theta - calls to advect_u, advect_v,
!                 advect_w, and advact_scalar).
!
!  (2) geopotential equation terms (advection and "gw" - call to rhs_ph).
!
!  (3) buoyancy term in vertical momentum equation (call to pg_buoy_w).
!
!  (4) Coriolis and curvature terms in u,v,w momentum equations
!      (calls to subroutines coriolis, curvature)
!
!  (5) 3D diffusion on coordinate surfaces.
!
!</DESCRIPTION>

! TBH:  OVERWRITE ru_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( ru_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE rv_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( rv_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE rw_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( rw_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE t_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( t_tend,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE ph_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( ph_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE u_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( u_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE v_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( v_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE w_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( w_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE ph_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( ph_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE t_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( t_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE mu_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( mu_tend,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    its, ite, jts, jte, 1, 1 )

! TBH:  OVERWRITE mu_save
! TBH:  HORIZONTAL DEPENDENCE:  NONE
   CALL zero_tend ( mu_save,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    its, ite, jts, jte, 1, 1 )

   IF (.not. leapfrog ) THEN

     !  advection tendencies

! TBH:  OVERWRITE ru_tend
! TBH:  HORIZONTAL DEPENDENCE:  u    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru   ( [i-1,i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv   ( [i-1,i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ww   ( [i-1,i    ], [    j    ] )
     CALL advect_u ( u, u , ru_tend, ru, rv, ww,   &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdnw,     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

! TBH:  OVERWRITE rv_tend
! TBH:  HORIZONTAL DEPENDENCE:  v    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru   ( [    i,i+1], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv   ( [    i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ww   ( [    i    ], [j-1,j    ] )
     CALL advect_v ( v, v , rv_tend, ru, rv, ww,   &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdnw,     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

     IF (non_hydrostatic)                          &
! TBH:  OVERWRITE rw_tend
! TBH:  HORIZONTAL DEPENDENCE:  w    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru   ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv   ( [    i    ], [    j,j+1] )
     CALL advect_w ( w, w, rw_tend, ru, rv, ww,    &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdn,      &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

!  theta flux divergence
! TBH:  OVERWRITE t_tend
! TBH:  HORIZONTAL DEPENDENCE:  t    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru   ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv   ( [    i    ], [    j,j+1] )
     CALL advect_scalar ( t, t, t_tend, ru, rv, ww,     &
                          mut, config_flags,            &
                          msfu, msfv, msft, fnm, fnp,   &
                          rdx, rdy, rdnw,               &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte ) 

     IF ( config_flags%cu_physics == GDSCHEME ) THEN

     ! theta advection only:

! TBH:  OVERWRITE RTHFTEN
! TBH:  HORIZONTAL DEPENDENCE:  NONE
         CALL set_tend( RTHFTEN, t_tend, msft,           &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte     )

     END IF

! TBH:  OVERWRITE ph_tend
! TBH:  HORIZONTAL DEPENDENCE:  ph   ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  phb  ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  u    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  muu  ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  muv  ( [    i    ], [    j,j+1] )
     CALL rhs_ph( ph_tend, u, v, ww, ph, ph, phb, w, &
                  mut, muu, muv,                     &
                  fnm, fnp,                          &
                  rdnw, cfn, cfn1, rdx, rdy, msft,   &
                  non_hydrostatic,                   &
                  config_flags,                      &
                  ids, ide, jds, jde, kds, kde,      &
                  ims, ime, jms, jme, kms, kme,      &
                  its, ite, jts, jte, kts, kte      )

  ELSE  ! leapfrog option
! TBH:  NOT CALLED
    CALL advect_u ( u, u_old, ru_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdnw,      &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )
! TBH:  NOT CALLED
    CALL advect_v ( v, v_old, rv_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdnw,      &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )

    IF (non_hydrostatic)                           &
! TBH:  NOT CALLED
    CALL advect_w ( w, w_old, rw_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdn,       &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )

!  theta flux divergence

! TBH:  NOT CALLED
    CALL advect_scalar ( t, t_old, t_tend, ru, rv, ww, &
                         mut, config_flags,            &
                         msfu, msfv, msft, fnm, fnp,   &
                         rdx, rdy, rdnw,               &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )

! TBH:  NOT CALLED
    CALL rhs_ph( ph_tend, u, v, ww, ph, ph_old, phb, w, &
                 mut, muu, muv,                         &
                 fnm, fnp,                              &
                 rdnw, cfn, cfn1, rdx, rdy, msft,       &
                 non_hydrostatic,                       &
                 config_flags,                          &
                 ids, ide, jds, jde, kds, kde,          &
                 ims, ime, jms, jme, kms, kme,          &
                 its, ite, jts, jte, kts, kte          )

  END IF

! TBH:  OVERWRITE ru_tend,rv_tend
! TBH:  HORIZONTAL DEPENDENCE:  p    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  ph   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  php  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  pb   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  al   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  alt  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  mu   ( [i-1,i    ], [j-1,j    ] )
  CALL horizontal_pressure_gradient( ru_tend,rv_tend,                &
                                     ph,alt,p,pb,al,php,cqu,cqv,     &
                                     muu,muv,mu,fnm,fnp,rdnw,        &
                                     cf1,cf2,cf3,rdx,rdy,msft,       &
                                     config_flags, non_hydrostatic,  &
                                     ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte   )

  IF (non_hydrostatic)                            &
! TBH:  OVERWRITE rw_tend,cqw
! TBH:  HORIZONTAL DEPENDENCE:  NONE
  CALL pg_buoy_w( rw_tend, p, cqw, mu, mub,       &
                  rdnw, rdn, g, msft,             &
                  ids, ide, jds, jde, kds, kde,   &
                  ims, ime, jms, jme, kms, kme,   &
                  its, ite, jts, jte, kts, kte   )

  IF(config_flags%w_damping.eq.1)THEN
! TBH:  OVERWRITE rw_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
    CALL w_damp   ( rw_tend, ww, w, mut, rdnw, dt,  &
                    ids, ide, jds, jde, kds, kde,   &
                    ims, ime, jms, jme, kms, kme,   &
                    its, ite, jts, jte, kts, kte   )
  ENDIF

  IF(config_flags%pert_coriolis) THEN

! TBH:  OVERWRITE ru_tend,rv_tend
! TBH:  HORIZONTAL DEPENDENCE:  p    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  ph   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  ru   ( [    i,i+1], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv   ( [i-1,i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  rw   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  f    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  e    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  sina ( [    i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  cosa ( [i-1,i    ], [    j    ] )
    CALL perturbation_coriolis ( ru, rv, rw,                   &
                                 ru_tend,  rv_tend,  rw_tend,  &
                                 config_flags,                 &
                                 u_base, v_base, z_base,       &
                                 muu, muv, phb, ph,            &
                                 f, e, sina, cosa, fnm, fnp,   &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte )
  ELSE

    CALL coriolis ( ru, rv, rw,                   &
                    ru_tend,  rv_tend,  rw_tend,  &
                    config_flags,                 &
                    f, e, sina, cosa, fnm, fnp,   &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   END IF


  CALL curvature ( ru, rv, rw, u, v, w,            &
                   ru_tend,  rv_tend,  rw_tend,    &
                   config_flags,                   &
                   msfu, msfv, fnm, fnp, rdx, rdy, &
                   ids, ide, jds, jde, kds, kde,   &
                   ims, ime, jms, jme, kms, kme,   &
                   its, ite, jts, jte, kts, kte   )

!**************************************************************
!
!  Next, the terms that we integrate only with forward-in-time
!  (evaluate with time t variables).
!
!**************************************************************

  forward_step: IF( rk_step == 1 ) THEN

    diff_opt1 : IF (config_flags%diff_opt .eq. 1) THEN
   
      leapfrog_test : IF( .not. leapfrog ) THEN

        CALL horizontal_diffusion ('u', u, ru_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        CALL horizontal_diffusion ('v', v, rv_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        CALL horizontal_diffusion ('w', w, rw_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        khdq = 3.*khdif
        CALL horizontal_diffusion_3dmp ( 'm', t, t_tendf, mut,         &
                                         config_flags, t_init,         &
                                         msfu, msfv, msft,             &
                                         khdq , xkmhd, rdx, rdy,       &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )

        pbl_test : IF (config_flags%bl_pbl_physics .eq. 0) THEN

          CALL vertical_diffusion_u ( u, ru_tendf, config_flags,      &
                                      u_base,                         &
                                      alt, muu, rdn, rdnw, kvdif,     &
                                      ids, ide, jds, jde, kds, kde,   &
                                      ims, ime, jms, jme, kms, kme,   &
                                      its, ite, jts, jte, kts, kte   )

          CALL vertical_diffusion_v ( v, rv_tendf, config_flags,      &
                                      v_base,                         &
                                      alt, muv, rdn, rdnw, kvdif,     &
                                      ids, ide, jds, jde, kds, kde,   &
                                      ims, ime, jms, jme, kms, kme,   &
                                      its, ite, jts, jte, kts, kte   )

          IF (non_hydrostatic)                                           &
          CALL vertical_diffusion ( 'w', w, rw_tendf, config_flags,      &
                                    alt, mut, rdn, rdnw, kvdif,          &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    its, ite, jts, jte, kts, kte        )

          kvdq = 3.*kvdif
          CALL vertical_diffusion_3dmp ( t, t_tendf, config_flags, t_init,     &
                                         alt, mut, rdn, rdnw, kvdq ,           &
                                         ids, ide, jds, jde, kds, kde,         &
                                         ims, ime, jms, jme, kms, kme,         &
                                         its, ite, jts, jte, kts, kte         )

        ENDIF pbl_test

   !  Theta tendency computations.

      ELSE ! leapfrog diffusion

        CALL horizontal_diffusion ('u', u_old, ru_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        CALL horizontal_diffusion ('v', v_old, rv_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        IF (non_hydrostatic)                                                &
        CALL horizontal_diffusion ('w', w_old, rw_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        khdq = 3.*khdif
        CALL horizontal_diffusion_3dmp ( 'm', t_old, t_tendf, mut,     &
                                         config_flags, t_init,         &
                                         msfu, msfv, msft,             &
                                         khdq , xkmhd, rdx, rdy,              &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )

        pbl_test_lf : IF (config_flags%bl_pbl_physics .eq. 0) THEN

          CALL vertical_diffusion_u ( u_old, ru_tendf, config_flags,      &
                                      u_base,                             &
                                      alt, muu, rdn, rdnw, kvdif,         &
                                      ids, ide, jds, jde, kds, kde,       &
                                      ims, ime, jms, jme, kms, kme,       &
                                      its, ite, jts, jte, kts, kte       )

          CALL vertical_diffusion_v ( v_old, rv_tendf, config_flags,      &
                                      v_base,                             &
                                      alt, muv, rdn, rdnw, kvdif,         &
                                      ids, ide, jds, jde, kds, kde,       &
                                      ims, ime, jms, jme, kms, kme,       &
                                      its, ite, jts, jte, kts, kte       )

          IF (non_hydrostatic)                                              &
          CALL vertical_diffusion ( 'w', w_old, rw_tendf, config_flags,     &
                                    alt, mut, rdn, rdnw, kvdif,             &
                                    ids, ide, jds, jde, kds, kde,           &
                                    ims, ime, jms, jme, kms, kme,           &
                                    its, ite, jts, jte, kts, kte           )

          kvdq = 3.*kvdif
          CALL vertical_diffusion_3dmp ( t_old, t_tendf, config_flags, t_init, &
                                         alt, mut, rdn, rdnw, kvdq ,           &
                                         ids, ide, jds, jde, kds, kde,         &
                                         ims, ime, jms, jme, kms, kme,         &
                                         its, ite, jts, jte, kts, kte         )
        ENDIF pbl_test_lf

      END IF leapfrog_test
        
    END IF diff_opt1

  END IF forward_step

END SUBROUTINE rk_tendency
