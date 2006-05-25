!WRF:MODEL_LAYER:DYNAMICS
!

MODULE module_check

   USE g_module_advect_em
   USE a_module_advect_em
   USE g_module_em
   USE a_module_em
   USE g_module_big_step_utilities_em
   USE a_module_big_step_utilities_em
   USE g_module_small_step_em
   USE a_module_small_step_em
   USE g_module_diffusion_em
   USE a_module_diffusion_em
   USE g_module_bc_em
   USE a_module_bc_em


CONTAINS

!------------------------------------------------------------------------

SUBROUTINE t_advect_scalar ( field, field_old, tendency, ru, rv, rom, &
                         mut, config_flags,            &
                         msfu, msfv, msft, fzm, fzp,   &
                         rdx, rdy, rdzw,               &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )

!  Input variables: field, field_old, tendency, ru, rv, rom, mut
!  Output variable: tendency
!  Contants: All others
!  Qingnong Xiao, January 25, 2005

   IMPLICIT NONE
  
   ! Input data
  
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme )                 :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
  
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux

   REAL,  DIMENSION( its:ite+1, kts:kte  ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order, vert_order
  
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp


! definition of flux operators, 3rd, 4rth, 5th or 6th order

   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel
   LOGICAL :: specified ! changed by Thomas Nehrkorn, AER

!  Xiao: new definition
 
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: S_field,     &
                                                                      S_field_old, &
                                                                      S_ru,    &
                                                                      S_rv,    &
                                                                      S_rom
   REAL , DIMENSION( ims:ime , jms:jme )                           :: S_mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: S_tendency
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: P_field,     &
                                                                      P_field_old, &
                                                                      P_ru,    &
                                                                      P_rv,    &
                                                                      P_rom
   REAL , DIMENSION( ims:ime , jms:jme )                           :: P_mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: P_tendency
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: B_field,     &
                                                                      B_field_old, &
                                                                      B_ru,    &
                                                                      B_rv,    &
                                                                      B_rom
   REAL , DIMENSION( ims:ime , jms:jme )                           :: B_mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: B_tendency
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )                 :: K_tendency
   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  Xiao: new definition end

!  TGL test

   S_field(:,:,:)=field(:,:,:)
   S_field_old(:,:,:)=field_old(:,:,:)
   S_ru(:,:,:)=ru(:,:,:)
   S_rv(:,:,:)=rv(:,:,:)
   S_rom(:,:,:)=rom(:,:,:)
   S_mut(:,:)=mut(:,:)
   S_tendency(:,:,:)=tendency(:,:,:)
   K_tendency(:,:,:)=tendency(:,:,:)

   P_field(:,:,:)=field(:,:,:)
   P_field_old(:,:,:)=field_old(:,:,:)
   P_ru(:,:,:)=ru(:,:,:)
   P_rv(:,:,:)=rv(:,:,:)
   P_rom(:,:,:)=rom(:,:,:)
   P_mut(:,:)=mut(:,:)
   P_tendency(:,:,:)=tendency(:,:,:)

!  NLM

   CALL advect_scalar   ( field, field_old, tendency,       &
                             ru, rv, rom,                   &
                             mut, config_flags,             &
                             msfu, msfv, msft,              &
                             fzm, fzp,                      &
                             rdx, rdy, rdzw,                &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   B_tendency(:,:,:)=tendency(:,:,:)

!  TGL

   CALL g_advect_scalar (field, P_field, field_old, P_field_old, K_tendency, P_tendency, &
                        ru, P_ru, rv, P_rv, rom, P_rom,  &
                        config_flags,                    &
                        msft,                            &
                        fzm, fzp,                        &
                        rdx, rdy, rdzw,                  &
                        ids, ide, jds, jde, kde,         &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte  )

   SAVE_L=sum(P_tendency(:,:,:)*P_tendency(:,:,:))

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      P_field(:,:,:)=FACTOR*S_field(:,:,:)
      P_field_old(:,:,:)=FACTOR*S_field_old(:,:,:)
      P_ru(:,:,:)=FACTOR*S_ru(:,:,:)
      P_rv(:,:,:)=FACTOR*S_rv(:,:,:)
      P_rom(:,:,:)=FACTOR*S_rom(:,:,:)
      P_mut(:,:)=FACTOR*S_mut(:,:)
      P_tendency(:,:,:)=FACTOR*S_tendency(:,:,:)
      CALL advect_scalar   ( P_field, P_field_old, P_tendency, &
                             P_ru, P_rv, P_rom,                &
                             P_mut, config_flags,              &
                             msfu, msfv, msft,                 &
                             fzm, fzp,                         &
                             rdx, rdy, rdzw,                   &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             its, ite, jts, jte, kts, kte  )
      VAL_N=sum((P_tendency(:,:,:)-B_tendency(:,:,:))*  &
                     (P_tendency(:,:,:)-B_tendency(:,:,:)))
      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L

      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_advect_scalar: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   field(:,:,:)=S_field(:,:,:)
   field_old(:,:,:)=S_field_old(:,:,:)
   ru(:,:,:)=S_ru(:,:,:)
   rv(:,:,:)=S_rv(:,:,:)
   rom(:,:,:)=S_rom(:,:,:)
   mut(:,:)=S_mut(:,:)
   tendency(:,:,:)=S_tendency(:,:,:)

   P_field(:,:,:)=FACTOR*S_field(:,:,:)
   P_field_old(:,:,:)=FACTOR*S_field_old(:,:,:)
   P_ru(:,:,:)=FACTOR*S_ru(:,:,:)
   P_rv(:,:,:)=FACTOR*S_rv(:,:,:)
   P_rom(:,:,:)=FACTOR*S_rom(:,:,:)
   P_mut(:,:)=FACTOR*S_mut(:,:)
   P_tendency(:,:,:)=FACTOR*S_tendency(:,:,:)

   B_field(:,:,:)=P_field(:,:,:)
   B_field_old(:,:,:)=P_field_old(:,:,:)
   B_ru(:,:,:)=P_ru(:,:,:)
   B_rv(:,:,:)=P_rv(:,:,:)
   B_rom(:,:,:)=P_rom(:,:,:)
   B_mut(:,:)=P_mut(:,:)
   B_tendency(:,:,:)=P_tendency(:,:,:)

!  TGL

   call g_advect_scalar (field, P_field, field_old, P_field_old, tendency, P_tendency, &
                        ru, P_ru, rv, P_rv, rom, P_rom,  &
                        config_flags,                    &
                        msft,                            &
                        fzm, fzp,                        &
                        rdx, rdy, rdzw,                  &
                        ids, ide, jds, jde, kde,         &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte  )

   VAL_L=sum(P_tendency(:,:,:)*P_tendency(:,:,:))

   P_field(:,:,:)=0.
   P_field_old(:,:,:)=0.
   P_ru(:,:,:)=0.
   P_rv(:,:,:)=0.
   P_rom(:,:,:)=0.
   P_mut(:,:)=0.

!  ADJ

   call a_advect_scalar (field, P_field, field_old, P_field_old, P_tendency, &
                        ru, P_ru, rv, P_rv, rom, P_rom,  &
                        config_flags,                    &
                        msft,                            &
                        fzm, fzp,                        &
                        rdx, rdy, rdzw,                  &
                        ids, ide, jds, jde, kde,         &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte  )
   VAL_A=sum(P_field(:,:,:)*B_field(:,:,:)) + &
         sum(P_field_old(:,:,:)*B_field_old(:,:,:))+ &
         sum(P_tendency(:,:,:)*B_tendency(:,:,:)) + &
         sum(P_ru(:,:,:)*B_ru(:,:,:)) +  &
         sum(P_rv(:,:,:)*B_rv(:,:,:))+ &
         sum(P_rom(:,:,:)*B_rom(:,:,:))

   write(6,fmt='(A,2E22.13)') 'a_advect_scalar: ', VAL_L,VAL_A

!  RECOVER

   field(:,:,:)=S_field(:,:,:)
   field_old(:,:,:)=S_field_old(:,:,:)
   ru(:,:,:)=S_ru(:,:,:)
   rv(:,:,:)=S_rv(:,:,:)
   rom(:,:,:)=S_rom(:,:,:)
   mut(:,:)=S_mut(:,:)
   tendency(:,:,:)=S_tendency(:,:,:)

END SUBROUTINE t_advect_scalar

!===================================================================================!

SUBROUTINE t_rk_tendency ( config_flags, rk_step,                         &
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
                         dampcoef,zdamp,damp_opt,                         &
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic,                                 &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         its, ite, jts, jte, kts, kte                    )


! Input variables : ru,rv,rw,ww,u,v,w,t,ph,u_old,v_old,w_old,t_old,ph_old
!                 : phb,al,alt,p,pb,php,cqu,cqv,t_init,xkmhd, h_diabatic 

! Output variables: ru_tend, rv_tend, rw_tend, t_tend, ph_tend, RTHFTEN
!                 : u_save, v_save, w_save, ph_save, t_save
!                 : mu_tend, mu_save

! InOut variables : ru_tendf, rv_tendf, rw_tendf, t_tendf, ph_tendf, cqw

! Contants        : All others



! Input variables : ru,rv,rw,ww,u,v,w,t,ph,u_old,v_old,w_old,t_old,ph_old
!                 : al,alt,p,php,cqu,cqv,xkmhd
!                 : mu,mut,muu,muv,

! Output variables: ru_tend, rv_tend, rw_tend, t_tend, ph_tend
!                 : u_save, v_save, w_save, ph_save, t_save
!                 : mu_tend

! InOut variables : ru_tendf, rv_tendf, rw_tendf, t_tendf, cqw




! Zaizhong Ma, March 18,2005

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type)    ,           INTENT(IN   ) :: config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL ,               INTENT(IN   ) :: non_hydrostatic

   INTEGER ,               INTENT(IN   ) :: n_moist, rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: ru,      &
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

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: ru_tendf, &
                                                         rv_tendf, &
                                                         rw_tendf, &
                                                         t_tendf,  &
                                                         ph_tendf, &
                                                         cqw

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(  OUT) :: mu_tend, &
                                                                    mu_save

   REAL , DIMENSION( ims:ime , jms:jme ) ,   INTENT(IN   )       :: msfu,    &
                                                                    msfv,    &
                                                                    msft,    &
                                                                    f,       &
                                                                    e,       &
                                                                    sina,    &
                                                                    cosa,    &
                                                                    mub
   REAL , DIMENSION( ims:ime , jms:jme )                         :: mu,      &
                                                                    mut,     &
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
   INTEGER, INTENT( IN ) :: damp_opt

   REAL, INTENT( IN ) :: zdamp, dampcoef

   REAL    :: kdift, khdq, kvdq, cfn, cfn1, cf1, cf2, cf3
   INTEGER :: i,j,k

!  zzma: new definition

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: S_ru,      &
                                                         S_rv,      &
                                                         S_rw,      &
                                                         S_ww,      &
                                                         S_u,       &
                                                         S_v,       &
                                                         S_w,       &
                                                         S_t,       &
                                                         S_ph,      &
                                                         S_u_old,   &
                                                         S_v_old,   &
                                                         S_w_old,   &
                                                         S_t_old,   &
                                                         S_ph_old,  &
                                                         S_al,      &
                                                         S_alt,     &
                                                         S_p,       &
                                                         S_php,     &
                                                         S_cqu,     &
                                                         S_cqv,     &
                                                         S_xkmhd
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: P_ru,      &
                                                         P_rv,      &
                                                         P_rw,      &
                                                         P_ww,      &
                                                         P_u,       &
                                                         P_v,       &
                                                         P_w,       &
                                                         P_t,       &
                                                         P_ph,      &
                                                         P_u_old,   &
                                                         P_v_old,   &
                                                         P_w_old,   &
                                                         P_t_old,   &
                                                         P_ph_old,  &
                                                         P_al,      &
                                                         P_alt,     &
                                                         P_p,       &
                                                         P_php,     &
                                                         P_cqu,     &
                                                         P_cqv,     &
                                                         P_xkmhd
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: B_ru,      &
                                                         B_rv,      &
                                                         B_rw,      &
                                                         B_ww,      &
                                                         B_u,       &
                                                         B_v,       &
                                                         B_w,       &
                                                         B_t,       &
                                                         B_ph,      &
                                                         B_u_old,   &
                                                         B_v_old,   &
                                                         B_w_old,   &
                                                         B_t_old,   &
                                                         B_ph_old,  &
                                                         B_al,      &
                                                         B_alt,     &
                                                         B_p,       &
                                                         B_php,     &
                                                         B_cqu,     &
                                                         B_cqv,     &
                                                         B_xkmhd

   REAL , DIMENSION( ims:ime , jms:jme )              :: S_mu,      &
                                                         S_mut,     &
                                                         S_muu,     &
                                                         S_muv
   REAL , DIMENSION( ims:ime , jms:jme )              :: P_mu,      &
                                                         P_mut,     &
                                                         P_muu,     &
                                                         P_muv
   REAL , DIMENSION( ims:ime , jms:jme )              :: B_mu,      &
                                                         B_mut,     &
                                                         B_muu,     &
                                                         B_muv
! INOUT varibales

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: S_ru_tendf, &
                                                         S_rv_tendf, &
                                                         S_rw_tendf, &
                                                         S_t_tendf,  &
                                                         S_cqw
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: P_ru_tendf, &
                                                         P_rv_tendf, &
                                                         P_rw_tendf, &
                                                         P_t_tendf,  &
                                                         P_cqw

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: K_ru_tendf, &
                                                         K_rv_tendf, &
                                                         K_rw_tendf, &
                                                         K_t_tendf,  &
                                                         K_cqw

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: B_ru_tendf, &
                                                         B_rv_tendf, &
                                                         B_rw_tendf, &
                                                         B_t_tendf,  &
                                                         B_cqw

!  OUT varibales

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: P_ru_tend, &
                                                         P_rv_tend, &
                                                         P_rw_tend, &
                                                         P_t_tend,  &
                                                         P_ph_tend, &
                                                         P_u_save, &
                                                         P_v_save, &
                                                         P_w_save, &
                                                         P_ph_save, &
                                                         P_t_save
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )    :: B_ru_tend, &
                                                         B_rv_tend, &
                                                         B_rw_tend, &
                                                         B_t_tend,  &
                                                         B_ph_tend, &
                                                         B_u_save, &
                                                         B_v_save, &
                                                         B_w_save, &
                                                         B_ph_save, &
                                                         B_t_save
   REAL , DIMENSION( ims:ime , jms:jme )           :: P_mu_tend
   REAL , DIMENSION( ims:ime , jms:jme )           :: B_mu_tend


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru(i,k,j)=ru(i,k,j)
      S_rv(i,k,j)=rv(i,k,j)
      S_rw(i,k,j)=rw(i,k,j)
      S_ww(i,k,j)=ww(i,k,j)
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)
      S_w(i,k,j)=w(i,k,j)
      S_t(i,k,j)=t(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)
      S_u_old(i,k,j)=u_old(i,k,j)
      S_v_old(i,k,j)=v_old(i,k,j)
      S_w_old(i,k,j)=w_old(i,k,j)
      S_t_old(i,k,j)=t_old(i,k,j)
      S_ph_old(i,k,j)=ph_old(i,k,j)
      S_al(i,k,j)=al(i,k,j)
      S_alt(i,k,j)=alt(i,k,j)
      S_p(i,k,j)=p(i,k,j)
      S_php(i,k,j)=php(i,k,j)
      S_cqu(i,k,j)=cqu(i,k,j)
      S_cqv(i,k,j)=cqv(i,k,j)
      S_xkmhd(i,k,j)=xkmhd(i,k,j)

      P_ru(i,k,j)=ru(i,k,j)
      P_rv(i,k,j)=rv(i,k,j)
      P_rw(i,k,j)=rw(i,k,j)
      P_ww(i,k,j)=ww(i,k,j)
      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)
      P_w(i,k,j)=w(i,k,j)
      P_t(i,k,j)=t(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
      P_u_old(i,k,j)=u_old(i,k,j)
      P_v_old(i,k,j)=v_old(i,k,j)
      P_w_old(i,k,j)=w_old(i,k,j)
      P_t_old(i,k,j)=t_old(i,k,j)
      P_ph_old(i,k,j)=ph_old(i,k,j)
      P_al(i,k,j)=al(i,k,j)
      P_alt(i,k,j)=alt(i,k,j)
      P_p(i,k,j)=p(i,k,j)
      P_php(i,k,j)=php(i,k,j)
      P_cqu(i,k,j)=cqu(i,k,j)
      P_cqv(i,k,j)=cqv(i,k,j)
      P_xkmhd(i,k,j)=xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      S_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      S_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      S_t_tendf(i,k,j)=t_tendf(i,k,j)
      S_cqw(i,k,j)=cqw(i,k,j)

      P_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=t_tendf(i,k,j)
      P_cqw(i,k,j)=cqw(i,k,j)

      K_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      K_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      K_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      K_t_tendf(i,k,j)=t_tendf(i,k,j)
      K_cqw(i,k,j)=cqw(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      S_mut(i,j)=mut(i,j)
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)

      P_mu(i,j)=mu(i,j)
      P_mut(i,j)=mut(i,j)
      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)
   enddo
   enddo


!NLM

   CALL rk_tendency ( config_flags, rk_step,                           &
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
                         dampcoef,zdamp,damp_opt,                        &
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic,                                 &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         its, ite, jts, jte, kts, kte                    )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_tend(i,k,j)=ru_tend(i,k,j)
      B_rv_tend(i,k,j)=rv_tend(i,k,j)
      B_rw_tend(i,k,j)=rw_tend(i,k,j)
      B_t_tend(i,k,j)=t_tend(i,k,j)
      B_ph_tend(i,k,j)=ph_tend(i,k,j)
      B_u_save(i,k,j)=u_save(i,k,j)
      B_v_save(i,k,j)=v_save(i,k,j)
      B_w_save(i,k,j)=w_save(i,k,j)
      B_ph_save(i,k,j)=ph_save(i,k,j)
      B_t_save(i,k,j)=t_save(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
      B_cqw(i,k,j)=cqw(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      B_mu_tend(i,j)=mu_tend(i,j)
   enddo
   enddo


!  TCL

   CALL g_rk_tendency( config_flags, rk_step, ru_tend, P_ru_tend, rv_tend, P_rv_tend, rw_tend, P_rw_tend, ph_tend, P_ph_tend, &
&t_tend, P_t_tend, K_ru_tendf, P_ru_tendf, K_rv_tendf, P_rv_tendf, K_rw_tendf, P_rw_tendf, K_t_tendf, P_t_tendf, mu_tend, P_mu_tend, &
&u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, ph_save, P_ph_save, t_save, P_t_save, ru, P_ru, rv, P_rv, rw, P_rw, ww, &
&P_ww, u, P_u, v, P_v, w, P_w, t, P_t, ph, P_ph, u_old, P_u_old, v_old, P_v_old, w_old, P_w_old, t_old, P_t_old, ph_old, P_ph_old, &
&phb, t_init, mu, P_mu, mut, P_mut, muu, P_muu, muv, P_muv, mub, al, P_al, alt, P_alt, p, P_p, pb, php, P_php, cqu, P_cqu, cqv, &
&P_cqv, K_cqw, P_cqw, u_base, v_base, z_base, msfu, msfv, msft, f, e, sina, cosa, fnm, fnp, rdn, rdnw, dt, rdx, rdy, kvdif, xkmhd, &
&P_xkmhd, &
dampcoef,zdamp,damp_opt,                         &
cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ru_tend(i,k,j)*P_ru_tend(i,k,j)  &
                    + P_rv_tend(i,k,j)*P_rv_tend(i,k,j)  &
                    + P_rw_tend(i,k,j)*P_rw_tend(i,k,j)  &
                    + P_t_tend(i,k,j)*P_t_tend(i,k,j)    &
                    + P_ph_tend(i,k,j)*P_ph_tend(i,k,j)  &
                    + P_u_save(i,k,j)*P_u_save(i,k,j)    &
                    + P_v_save(i,k,j)*P_v_save(i,k,j)    &
                    + P_w_save(i,k,j)*P_w_save(i,k,j)    &
                    + P_ph_save(i,k,j)*P_ph_save(i,k,j)  &
                    + P_t_save(i,k,j)*P_t_save(i,k,j)   
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ru_tendf(i,k,j) *P_ru_tendf(i,k,j) &
                    + P_rv_tendf(i,k,j) *P_rv_tendf(i,k,j) &
                    + P_rw_tendf(i,k,j) *P_rw_tendf(i,k,j) &
                    + P_t_tendf(i,k,j) *P_t_tendf(i,k,j)   &
                    + P_cqw(i,k,j) *P_cqw(i,k,j) 
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_mu_tend(i,j) *P_mu_tend(i,j) 
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_rw(i,k,j)=FACTOR*S_rw(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_u_old(i,k,j)=FACTOR*S_u_old(i,k,j)
      P_v_old(i,k,j)=FACTOR*S_v_old(i,k,j)
      P_w_old(i,k,j)=FACTOR*S_w_old(i,k,j)
      P_t_old(i,k,j)=FACTOR*S_t_old(i,k,j)
      P_ph_old(i,k,j)=FACTOR*S_ph_old(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_php(i,k,j)=FACTOR*S_php(i,k,j)
      P_cqu(i,k,j)=FACTOR*S_cqu(i,k,j)
      P_cqv(i,k,j)=FACTOR*S_cqv(i,k,j)
      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
   enddo
   enddo

   CALL rk_tendency ( config_flags, rk_step,                                        &
                         P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend,      &
                         P_ru_tendf, P_rv_tendf, P_rw_tendf, ph_tendf, P_t_tendf, &
                         P_mu_tend, P_u_save, P_v_save, P_w_save, P_ph_save,        &
                         P_t_save, mu_save, RTHFTEN,                            &
                         P_ru, P_rv, P_rw, P_ww,                                    &
                         P_u, P_v, P_w, P_t, P_ph,                                  &
                         P_u_old, P_v_old, P_w_old, P_t_old, P_ph_old,              &
                         h_diabatic, phb,t_init,                              &
                         P_mu, P_mut, P_muu, P_muv, mub,                                    &
                         P_al, P_alt, P_p, pb, P_php, P_cqu, P_cqv, P_cqw,        &
                         u_base, v_base, t_base, qv_base, z_base,                   &
                         msfu, msfv, msft, f, e, sina, cosa,                        &
                         fnm, fnp, rdn, rdnw,                                       &
                         dt, rdx, rdy, khdif, kvdif, P_xkmhd,                         &
                         dampcoef,zdamp,damp_opt,                        &      
                         cf1, cf2, cf3, cfn, cfn1, n_moist,                         &
                         non_hydrostatic,                                           &
                         ids, ide, jds, jde, kds, kde,                              &
                         ims, ime, jms, jme, kms, kme,                              &
                         its, ite, jts, jte, kts, kte                    )


      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_ru_tend(i,k,j)- B_ru_tend(i,k,j))*(P_ru_tend(i,k,j)- B_ru_tend(i,k,j))  &
                    +(P_rv_tend(i,k,j)- B_rv_tend(i,k,j))*(P_rv_tend(i,k,j)- B_rv_tend(i,k,j))   &
                    +(P_rw_tend(i,k,j)- B_rw_tend(i,k,j))*(P_rw_tend(i,k,j)- B_rw_tend(i,k,j))   &
                    +(P_t_tend(i,k,j)- B_t_tend(i,k,j))*(P_t_tend(i,k,j)- B_t_tend(i,k,j))       &
                    +(P_ph_tend(i,k,j)- B_ph_tend(i,k,j))*(P_ph_tend(i,k,j)- B_ph_tend(i,k,j))   &
                    +(P_u_save(i,k,j)- B_u_save(i,k,j))*(P_u_save(i,k,j)- B_u_save(i,k,j))       &
                    +(P_v_save(i,k,j)- B_v_save(i,k,j))*(P_v_save(i,k,j)- B_v_save(i,k,j))       &
                    +(P_w_save(i,k,j)- B_w_save(i,k,j))*(P_w_save(i,k,j)- B_w_save(i,k,j))       &
                    +(P_ph_save(i,k,j)- B_ph_save(i,k,j))*(P_ph_save(i,k,j)- B_ph_save(i,k,j))    &
                    +(P_t_save(i,k,j)- B_t_save(i,k,j))*(P_t_save(i,k,j)- B_t_save(i,k,j))     
      enddo
      enddo
      enddo

      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_ru_tendf(i,k,j)- B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)- B_ru_tendf(i,k,j))  &
                    +(P_rv_tendf(i,k,j)- B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)- B_rv_tendf(i,k,j))  &
                    +(P_rw_tendf(i,k,j)- B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)- B_rw_tendf(i,k,j))  &
                    +(P_t_tendf(i,k,j)- B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)- B_t_tendf(i,k,j))      &
                    +(P_cqw(i,k,j)- B_cqw(i,k,j))*(P_cqw(i,k,j)- B_cqw(i,k,j))
      enddo
      enddo
      enddo

      do i=ims,ime
      do j=jms,jme
         VAL_N=VAL_N+(P_mu_tend(i,j)- B_mu_tend(i,j))*(P_mu_tend(i,j)- B_mu_tend(i,j))   
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_rk_tendency: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme

      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      rw(i,k,j)=S_rw(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      u_old(i,k,j)=S_u_old(i,k,j)
      v_old(i,k,j)=S_v_old(i,k,j)
      w_old(i,k,j)=S_w_old(i,k,j)
      t_old(i,k,j)=S_t_old(i,k,j)
      ph_old(i,k,j)=S_ph_old(i,k,j)
      al(i,k,j)=S_al(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      p(i,k,j)=S_p(i,k,j)
      php(i,k,j)=S_php(i,k,j)
      cqu(i,k,j)=S_cqu(i,k,j)
      cqv(i,k,j)=S_cqv(i,k,j)
      xkmhd(i,k,j)=S_xkmhd(i,k,j)


      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_rw(i,k,j)=FACTOR*S_rw(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_u_old(i,k,j)=FACTOR*S_u_old(i,k,j)
      P_v_old(i,k,j)=FACTOR*S_v_old(i,k,j)
      P_w_old(i,k,j)=FACTOR*S_w_old(i,k,j)
      P_t_old(i,k,j)=FACTOR*S_t_old(i,k,j)
      P_ph_old(i,k,j)=FACTOR*S_ph_old(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_php(i,k,j)=FACTOR*S_php(i,k,j)
      P_cqu(i,k,j)=FACTOR*S_cqu(i,k,j)
      P_cqv(i,k,j)=FACTOR*S_cqv(i,k,j)
      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)

      B_ru(i,k,j)=P_ru(i,k,j)
      B_rv(i,k,j)=P_rv(i,k,j)
      B_rw(i,k,j)=P_rw(i,k,j)
      B_ww(i,k,j)=P_ww(i,k,j)
      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
      B_w(i,k,j)=P_w(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
      B_u_old(i,k,j)=P_u_old(i,k,j)
      B_v_old(i,k,j)=P_v_old(i,k,j)
      B_w_old(i,k,j)=P_w_old(i,k,j)
      B_t_old(i,k,j)=P_t_old(i,k,j)
      B_ph_old(i,k,j)=P_ph_old(i,k,j)
      B_al(i,k,j)=P_al(i,k,j)
      B_alt(i,k,j)=P_alt(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
      B_php(i,k,j)=P_php(i,k,j)
      B_cqu(i,k,j)=P_cqu(i,k,j)
      B_cqv(i,k,j)=P_cqv(i,k,j)
      B_xkmhd(i,k,j)=P_xkmhd(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme

      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)

      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)

      B_ru_tendf(i,k,j)=P_ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=P_rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=P_rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=P_t_tendf(i,k,j)
      B_cqw(i,k,j)=P_cqw(i,k,j)

      K_cqw(i,k,j)=cqw(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme

      mu(i,j)=S_mu(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)

      B_mu(i,j)=P_mu(i,j)
      B_mut(i,j)=P_mut(i,j)
      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)

   enddo
   enddo

!  TGL

   CALL g_rk_tendency( config_flags, rk_step, ru_tend, P_ru_tend, rv_tend, P_rv_tend, rw_tend, P_rw_tend, ph_tend, P_ph_tend, &
&t_tend, P_t_tend, ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, rw_tendf, P_rw_tendf, t_tendf, P_t_tendf, mu_tend, P_mu_tend, &
&u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, ph_save, P_ph_save, t_save, P_t_save, ru, P_ru, rv, P_rv, rw, P_rw, ww, &
&P_ww, u, P_u, v, P_v, w, P_w, t, P_t, ph, P_ph, u_old, P_u_old, v_old, P_v_old, w_old, P_w_old, t_old, P_t_old, ph_old, P_ph_old, &
&phb, t_init, mu, P_mu, mut, P_mut, muu, P_muu, muv, P_muv, mub, al, P_al, alt, P_alt, p, P_p, pb, php, P_php, cqu, P_cqu, cqv, &
&P_cqv, cqw, P_cqw, u_base, v_base, z_base, msfu, msfv, msft, f, e, sina, cosa, fnm, fnp, rdn, rdnw, dt, rdx, rdy, kvdif, xkmhd, &
&P_xkmhd, &
dampcoef,zdamp,damp_opt,                         &
cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_ru_tend(i,k,j)*P_ru_tend(i,k,j)  &
                    + P_rv_tend(i,k,j)*P_rv_tend(i,k,j)  &
                    + P_rw_tend(i,k,j)*P_rw_tend(i,k,j)  &
                    + P_t_tend(i,k,j)*P_t_tend(i,k,j)    &
                    + P_ph_tend(i,k,j)*P_ph_tend(i,k,j)  &
                    + P_u_save(i,k,j)*P_u_save(i,k,j)    &
                    + P_v_save(i,k,j)*P_v_save(i,k,j)    &
                    + P_w_save(i,k,j)*P_w_save(i,k,j)    &
                    + P_ph_save(i,k,j)*P_ph_save(i,k,j)  &
                    + P_t_save(i,k,j)*P_t_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L  + P_ru_tendf(i,k,j) *P_ru_tendf(i,k,j) &
                    + P_rv_tendf(i,k,j) *P_rv_tendf(i,k,j) &
                    + P_rw_tendf(i,k,j) *P_rw_tendf(i,k,j) &
                    + P_t_tendf(i,k,j) *P_t_tendf(i,k,j)   &
                    + P_cqw(i,k,j) *P_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L + P_mu_tend(i,j) *P_mu_tend(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=0.0
      P_rv(i,k,j)=0.0
      P_rw(i,k,j)=0.0
      P_ww(i,k,j)=0.0
      P_u(i,k,j)=0.0
      P_v(i,k,j)=0.0
      P_w(i,k,j)=0.0
      P_t(i,k,j)=0.0
      P_ph(i,k,j)=0.0
      P_u_old(i,k,j)=0.0
      P_v_old(i,k,j)=0.0
      P_w_old(i,k,j)=0.0
      P_t_old(i,k,j)=0.0
      P_ph_old(i,k,j)=0.0
      P_al(i,k,j)=0.0
      P_alt(i,k,j)=0.0
      P_p(i,k,j)=0.0
      P_php(i,k,j)=0.0
      P_cqu(i,k,j)=0.0
      P_cqv(i,k,j)=0.0
      P_xkmhd(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
      P_mut(i,j)=0.0
      P_muu(i,j)=0.0
      P_muv(i,j)=0.0
   enddo
   enddo


!  ADJ

   CALL a_rk_tendency( config_flags, rk_step, P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend, P_ru_tendf, P_rv_tendf, &
&P_rw_tendf, P_t_tendf, P_mu_tend, P_u_save, P_v_save, P_w_save, P_ph_save, P_t_save, ru, P_ru, rv, P_rv, rw, P_rw, ww, P_ww, u, &
&P_u, v, P_v, w, P_w, t, P_t, ph, P_ph, u_old, P_u_old, v_old, P_v_old, w_old, P_w_old, t_old, P_t_old, ph_old, P_ph_old, phb, &
&t_init, mu, P_mu, mut, P_mut, muu, P_muu, muv, P_muv, mub, al, P_al, alt, P_alt, p, P_p, pb, php, P_php, cqu, P_cqu, cqv, P_cqv, &
&K_cqw, P_cqw, u_base, v_base, z_base, msfu, msfv, msft, f, e, sina, cosa, fnm, fnp, rdn, rdnw, dt, rdx, rdy, kvdif, xkmhd, P_xkmhd, &
dampcoef,zdamp,damp_opt,                         &
&cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&kts, kte )


   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_ru(i,k,j)*B_ru(i,k,j)         &  
               +P_rv(i,k,j)*B_rv(i,k,j)            &
               +P_rw(i,k,j)*B_rw(i,k,j)            &
               +P_ww(i,k,j)*B_ww(i,k,j)            &
               +P_u(i,k,j)*B_u(i,k,j)              &
               +P_v(i,k,j)*B_v(i,k,j)              &
               +P_w(i,k,j)*B_w(i,k,j)              &
               +P_t(i,k,j)*B_t(i,k,j)              &
               +P_ph(i,k,j)*B_ph(i,k,j)            &
               +P_u_old(i,k,j)*B_u_old(i,k,j)      &
               +P_v_old(i,k,j)*B_v_old(i,k,j)      &
               +P_w_old(i,k,j)*B_w_old(i,k,j)      &
               +P_t_old(i,k,j)*B_t_old(i,k,j)      &
               +P_ph_old(i,k,j)*B_ph_old(i,k,j)    &
               +P_al(i,k,j)*B_al(i,k,j)            &
               +P_alt(i,k,j)*B_alt(i,k,j)          &
               +P_p(i,k,j)*B_p(i,k,j)              &
               +P_php(i,k,j)*B_php(i,k,j)          &
               +P_cqu(i,k,j)*B_cqu(i,k,j)          &
               +P_cqv(i,k,j)*B_cqv(i,k,j)          &
               +P_xkmhd(i,k,j)*B_xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)   &
               +P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)      &
               +P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)      &
               +P_t_tendf(i,k,j)*B_t_tendf(i,k,j)        &
               +P_cqw(i,k,j)*B_cqw(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)   &
               +P_mut(i,j)*B_mut(i,j)    &
               +P_muu(i,j)*B_muu(i,j)    &
               +P_muv(i,j)*B_muv(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_advect_scalar: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      rw(i,k,j)=S_rw(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      u_old(i,k,j)=S_u_old(i,k,j)
      v_old(i,k,j)=S_v_old(i,k,j)
      w_old(i,k,j)=S_w_old(i,k,j)
      t_old(i,k,j)=S_t_old(i,k,j)
      ph_old(i,k,j)=S_ph_old(i,k,j)
      al(i,k,j)=S_al(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      p(i,k,j)=S_p(i,k,j)
      php(i,k,j)=S_php(i,k,j)
      cqu(i,k,j)=S_cqu(i,k,j)
      cqv(i,k,j)=S_cqv(i,k,j)
      xkmhd(i,k,j)=S_xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
   enddo
   enddo

!the result of testing rk_tendency, using em_quarter_ss

!g_rk_tendency: ALPHA=.1000E+00  COEF= 0.121735E+01  VAL_N= 0.217612E+12  VAL_L= 0.178758E+12
!g_rk_tendency: ALPHA=.1000E-01  COEF= 0.102015E+01  VAL_N= 0.182361E+10  VAL_L= 0.178758E+10
!g_rk_tendency: ALPHA=.1000E-02  COEF= 0.100215E+01  VAL_N= 0.179143E+08  VAL_L= 0.178758E+08
!g_rk_tendency: ALPHA=.1000E-03  COEF= 0.100113E+01  VAL_N= 0.178961E+06  VAL_L= 0.178758E+06
!g_rk_tendency: ALPHA=.1000E-04  COEF= 0.101263E+01  VAL_N= 0.181017E+04  VAL_L= 0.178758E+04
!g_rk_tendency: ALPHA=.1000E-05  COEF= 0.106717E+01  VAL_N= 0.190766E+02  VAL_L= 0.178758E+02
!g_rk_tendency: ALPHA=.1000E-06  COEF= 0.170886E+01  VAL_N= 0.305473E+00  VAL_L= 0.178758E+00
!g_rk_tendency: ALPHA=.1000E-07  COEF= 0.647516E-15  VAL_N= 0.115749E-17  VAL_L= 0.178758E-02
!g_rk_tendency: ALPHA=.1000E-08  COEF= 0.647516E-13  VAL_N= 0.115749E-17  VAL_L= 0.178758E-04
!g_rk_tendency: ALPHA=.1000E-09  COEF= 0.647516E-11  VAL_N= 0.115749E-17  VAL_L= 0.178758E-06
!g_rk_tendency: ALPHA=.1000E-10  COEF= 0.647516E-09  VAL_N= 0.115749E-17  VAL_L= 0.178758E-08

!a_advect_scalar:    0.1787557969920E+12   0.1787880734720E+12

!g_rk_tendency: ALPHA=.1000E+00  COEF=   0.1217344191204E+01  VAL_N= 0.217614E+12  VAL_L= 0.178761E+12
!g_rk_tendency: ALPHA=.1000E-01  COEF=   0.1020167288022E+01  VAL_N= 0.182366E+10  VAL_L= 0.178761E+10
!g_rk_tendency: ALPHA=.1000E-02  COEF=   0.1002001638191E+01  VAL_N= 0.179119E+08  VAL_L= 0.178761E+08
!g_rk_tendency: ALPHA=.1000E-03  COEF=   0.1000199989221E+01  VAL_N= 0.178797E+06  VAL_L= 0.178761E+06
!g_rk_tendency: ALPHA=.1000E-04  COEF=   0.1000019972902E+01  VAL_N= 0.178765E+04  VAL_L= 0.178761E+04
!g_rk_tendency: ALPHA=.1000E-05  COEF=   0.1000001972593E+01  VAL_N= 0.178761E+02  VAL_L= 0.178761E+02
!g_rk_tendency: ALPHA=.1000E-06  COEF=   0.1000000174417E+01  VAL_N= 0.178761E+00  VAL_L= 0.178761E+00
!g_rk_tendency: ALPHA=.1000E-07  COEF=   0.9999999817357E+00  VAL_N= 0.178761E-02  VAL_L= 0.178761E-02
!g_rk_tendency: ALPHA=.1000E-08  COEF=   0.1000000166722E+01  VAL_N= 0.178761E-04  VAL_L= 0.178761E-04
!g_rk_tendency: ALPHA=.1000E-09  COEF=   0.1000001082845E+01  VAL_N= 0.178761E-06  VAL_L= 0.178761E-06
!g_rk_tendency: ALPHA=.1000E-10  COEF=   0.1000005202259E+01  VAL_N= 0.178762E-08  VAL_L= 0.178761E-08

!a_advect_scalar:    0.1787609594770E+12   0.1787609594770E+12


END SUBROUTINE t_rk_tendency


!---------------------------------------------------------------------------------------------------


SUBROUTINE t_rk_step_prep  ( config_flags, rk_step,           &
                           u, v, w, t, ph, mu,              &
                           moist,                           &
                           ru, rv, rw, ww, php, alt, muu, muv,  &
                           mub, mut, phb, pb, p, al, alb,   &
                           cqu, cqv, cqw,                   &
                           msfu, msfv, msft,                &
                           fnm, fnp, dnw, rdx, rdy,         &
                           n_moist,                         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE


   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   INTEGER ,       INTENT(IN   ) :: n_moist, rk_step

   REAL ,          INTENT(IN   ) :: rdx, rdy

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ),INTENT(IN ):: t,       &
                                                                 phb,     &
                                                                 pb,      &
                                                                 alb

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme )           ::  u,       &
                                                                 v,       &
                                                                 w,       &
                                                                 ph,      &
                                                                 al

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  ) ,                     &
                                               INTENT(  OUT) ::  ru,      &
                                                                 rv,      &
                                                                 rw,      &
                                                                 ww,      &
                                                                 php,     &
                                                                 cqu,     &
                                                                 cqv,     &
                                                                 cqw,     &
                                                                 alt

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme )           ::  p




   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )   :: moist

   REAL , DIMENSION( ims:ime , jms:jme ) ,    INTENT(IN   ) :: msft,   &
                                                               msfu,   &
                                                               msfv,   &
                                                               mub
   REAL , DIMENSION( ims:ime , jms:jme )                    :: mu

   REAL , DIMENSION( ims:ime , jms:jme ) ,    INTENT(  OUT) :: muu,    &
                                                               muv,    &
                                                               mut

   REAL , DIMENSION( kms:kme ) ,    INTENT(IN   ) :: fnm, fnp, dnw

   integer :: i,j,k,h

!  zzma: new definition

!  IN variable

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme )           ::  S_u,       &
                                                                 S_v,       &
                                                                 S_w,       &
                                                                 S_ph,      &
                                                                 S_al
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )    ::  S_moist
   REAL , DIMENSION( ims:ime , jms:jme )                     ::  S_mu
   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme )           ::  P_u,       &
                                                                 P_v,       &
                                                                 P_w,       &
                                                                 P_ph,      &
                                                                 P_al
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )    ::  P_moist
   REAL , DIMENSION( ims:ime , jms:jme )                     ::  P_mu

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme )           ::  B_u,       &
                                                                 B_v,       &
                                                                 B_w,       &
                                                                 B_ph,      &
                                                                 B_al
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )    ::  B_moist
   REAL , DIMENSION( ims:ime , jms:jme )                     ::  B_mu


!OUT variable

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  )          ::  P_ru,      &
                                                                 P_rv,      &
                                                                 P_rw,      &
                                                                 P_ww,      &
                                                                 P_php,     &
                                                                 P_cqu,     &
                                                                 P_cqv,     &
                                                                 P_cqw,     &
                                                                 P_alt
   REAL , DIMENSION( ims:ime , jms:jme )                    :: P_muu,    &
                                                               P_muv,    &
                                                               P_mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  )          ::  B_ru,      &
                                                                 B_rv,      &
                                                                 B_rw,      &
                                                                 B_ww,      &
                                                                 B_php,     &
                                                                 B_cqu,     &
                                                                 B_cqv,     &
                                                                 B_cqw,     &
                                                                 B_alt
   REAL , DIMENSION( ims:ime , jms:jme )                    :: B_muu,    &
                                                               B_muv,    &
                                                               B_mut

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)
      S_w(i,k,j)=w(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)
      S_al(i,k,j)=al(i,k,j)

      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)
      P_w(i,k,j)=w(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
      P_al(i,k,j)=al(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      S_moist(i,k,j,h)=moist(i,k,j,h)

      P_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)

      P_mu(i,j)=mu(i,j)
   enddo
   enddo

!NLM

   CALL rk_step_prep  ( config_flags, rk_step,           &
                           u, v, w, t, ph, mu,              &
                           moist,                           &
                           ru, rv, rw, ww, php, alt, muu, muv,  &
                           mub, mut, phb, pb, p, al, alb,   &
                           cqu, cqv, cqw,                   &
                           msfu, msfv, msft,                &
                           fnm, fnp, dnw, rdx, rdy,         &
                           n_moist,                         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru(i,k,j)=ru(i,k,j)
      B_rv(i,k,j)=rv(i,k,j)
      B_rw(i,k,j)=rw(i,k,j)
      B_ww(i,k,j)=ww(i,k,j)
      B_php(i,k,j)=php(i,k,j)
      B_cqu(i,k,j)=cqu(i,k,j)
      B_cqv(i,k,j)=cqv(i,k,j)
      B_cqw(i,k,j)=cqw(i,k,j)
      B_alt(i,k,j)=alt(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
      B_mut(i,j)=mut(i,j)
   enddo
   enddo

!  TCL

   CALL g_rk_step_prep( config_flags, u, P_u, v, P_v, w, P_w, ph, P_ph, mu, P_mu, moist, P_moist, ru, P_ru, rv, P_rv, rw, P_rw, &
&ww, P_ww, php, P_php, alt, P_alt, muu, P_muu, muv, P_muv, mub, mut, P_mut, phb, al, P_al, alb, cqu, P_cqu, cqv, P_cqv, cqw, P_cqw,&
& msfu, msfv, msft, dnw, rdx, rdy, n_moist, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ru(i,k,j)*P_ru(i,k,j)     &
                    + P_rv(i,k,j)*P_rv(i,k,j)     &
                    + P_rw(i,k,j)*P_rw(i,k,j)     &
                    + P_ww(i,k,j)*P_ww(i,k,j)     &
                    + P_php(i,k,j)*P_php(i,k,j)     &
                    + P_cqu(i,k,j)*P_cqu(i,k,j)     &
                    + P_cqv(i,k,j)*P_cqv(i,k,j)     &
                    + P_cqw(i,k,j)*P_cqw(i,k,j)     &
                    + P_alt(i,k,j)*P_alt(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_muu(i,j)*P_muu(i,j)      &
                    + P_muv(i,j)*P_muv(i,j)      &
                    + P_mut(i,j)*P_mut(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
   enddo
   enddo

   CALL rk_step_prep  ( config_flags, rk_step,           &
                           P_u, P_v, P_w, t, P_ph, P_mu,              &
                           P_moist,                           &
                           P_ru, P_rv, P_rw, P_ww, P_php, P_alt, P_muu, P_muv,  &
                           mub, P_mut, phb, pb, p, P_al, alb,   &
                           P_cqu, P_cqv, P_cqw,                   &
                           msfu, msfv, msft,                &
                           fnm, fnp, dnw, rdx, rdy,         &
                           n_moist,                         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )


      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N +(P_ru(i,k,j) - B_ru(i,k,j))*(P_ru(i,k,j) - B_ru(i,k,j))    &
               +(P_rv(i,k,j) - B_rv(i,k,j))*(P_rv(i,k,j) - B_rv(i,k,j))          &
               +(P_rw(i,k,j) - B_rw(i,k,j))*(P_rw(i,k,j) - B_rw(i,k,j))          &
               +(P_ww(i,k,j) - B_ww(i,k,j))*(P_ww(i,k,j) - B_ww(i,k,j))          &
               +(P_php(i,k,j) - B_php(i,k,j))*(P_php(i,k,j) - B_php(i,k,j))      &
               +(P_cqu(i,k,j) - B_cqu(i,k,j))*(P_cqu(i,k,j) - B_cqu(i,k,j))      &
               +(P_cqv(i,k,j) - B_cqv(i,k,j))*(P_cqv(i,k,j) - B_cqv(i,k,j))      &
               +(P_cqw(i,k,j) - B_cqw(i,k,j))*(P_cqw(i,k,j) - B_cqw(i,k,j))      &
               +(P_alt(i,k,j) - B_alt(i,k,j))*(P_alt(i,k,j) - B_alt(i,k,j))
      enddo
      enddo
      enddo
      do i=ims,ime
      do j=jms,jme
         VAL_N=VAL_N+(P_muu(i,j) - B_muu(i,j))*(P_muu(i,j) - B_muu(i,j))    &
               +(P_muv(i,j) - B_muv(i,j))*(P_muv(i,j) - B_muv(i,j))         &
               +(P_mut(i,j) - B_mut(i,j))*(P_mut(i,j) - B_mut(i,j))
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_rk_step_prep: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      al(i,k,j)=S_al(i,k,j)

      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)

      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
      B_w(i,k,j)=P_w(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
      B_al(i,k,j)=P_al(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)

      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)

      B_moist(i,k,j,h)=P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)

      B_mu(i,j)=P_mu(i,j)
   enddo
   enddo

!  TGL

   CALL g_rk_step_prep( config_flags, u, P_u, v, P_v, w, P_w, ph, P_ph, mu, P_mu, moist, P_moist, ru, P_ru, rv, P_rv, rw, P_rw, &
&ww, P_ww, php, P_php, alt, P_alt, muu, P_muu, muv, P_muv, mub, mut, P_mut, phb, al, P_al, alb, cqu, P_cqu, cqv, P_cqv, cqw, P_cqw,&
& msfu, msfv, msft, dnw, rdx, rdy, n_moist, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L   + P_ru(i,k,j)*P_ru(i,k,j)     &
                    + P_rv(i,k,j)*P_rv(i,k,j)     &
                    + P_rw(i,k,j)*P_rw(i,k,j)     &
                    + P_ww(i,k,j)*P_ww(i,k,j)     &
                    + P_php(i,k,j)*P_php(i,k,j)     &
                    + P_cqu(i,k,j)*P_cqu(i,k,j)     &
                    + P_cqv(i,k,j)*P_cqv(i,k,j)     &
                    + P_cqw(i,k,j)*P_cqw(i,k,j)     &
                    + P_alt(i,k,j)*P_alt(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L   + P_muu(i,j)*P_muu(i,j)      &
                    + P_muv(i,j)*P_muv(i,j)      &
                    + P_mut(i,j)*P_mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=0.0
      P_v(i,k,j)=0.0
      P_w(i,k,j)=0.0
      P_ph(i,k,j)=0.0
      P_al(i,k,j)=0.0
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      P_moist(i,k,j,h)=0.0
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
   enddo
   enddo

!  ADJ

    CALL a_rk_step_prep( config_flags, u, P_u, v, P_v, w, P_w, P_ph, mu, P_mu, moist, P_moist, P_ru, P_rv, P_rw, P_ww, P_php, &
&P_alt, muu, P_muu, muv, P_muv, mub, mut, P_mut, P_al, P_cqu, P_cqv, P_cqw, msfu, msfv, msft, dnw, rdx, rdy, n_moist, ids, ide, &
&jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)      &
                  + P_v(i,k,j)*B_v(i,k,j)      &
                  + P_w(i,k,j)*B_w(i,k,j)      &
                  + P_ph(i,k,j)*B_ph(i,k,j)      &
                  + P_al(i,k,j)*B_al(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      VAL_A=VAL_A + P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_rk_step_prep: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      al(i,k,j)=S_al(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

! the result of testing rk_step_prep, using em_quarter_ss

!g_rk_step_prep: ALPHA=.1000E+00  COEF=   0.9992192983627E+00  VAL_N= 0.338343E+16  VAL_L= 0.338607E+16
!g_rk_step_prep: ALPHA=.1000E-01  COEF=   0.9996247887611E+00  VAL_N= 0.338480E+14  VAL_L= 0.338607E+14
!g_rk_step_prep: ALPHA=.1000E-02  COEF=   0.9993402957916E+00  VAL_N= 0.338384E+12  VAL_L= 0.338607E+12
!g_rk_step_prep: ALPHA=.1000E-03  COEF=   0.9999324679375E+00  VAL_N= 0.338585E+10  VAL_L= 0.338607E+10
!g_rk_step_prep: ALPHA=.1000E-04  COEF=   0.1030537486076E+01  VAL_N= 0.348948E+08  VAL_L= 0.338607E+08
!g_rk_step_prep: ALPHA=.1000E-05  COEF=   0.1312857389450E+01  VAL_N= 0.444543E+06  VAL_L= 0.338608E+06
!g_rk_step_prep: ALPHA=.1000E-06  COEF=   0.3907715606689E+02  VAL_N= 0.132318E+06  VAL_L= 0.338608E+04
!g_rk_step_prep: ALPHA=.1000E-07  COEF=   0.3579903320312E+04  VAL_N= 0.121218E+06  VAL_L= 0.338608E+02
!g_rk_step_prep: ALPHA=.1000E-08  COEF=   0.3579903125000E+06  VAL_N= 0.121218E+06  VAL_L= 0.338608E+00
!g_rk_step_prep: ALPHA=.1000E-09  COEF=   0.3579902800000E+08  VAL_N= 0.121218E+06  VAL_L= 0.338608E-02
!g_rk_step_prep: ALPHA=.1000E-10  COEF=   0.3579902464000E+10  VAL_N= 0.121218E+06  VAL_L= 0.338608E-04
                 
!a_rk_step_prep:    0.3386954569744E+16   0.3385869822067E+16


!g_rk_step_prep: ALPHA=.1000E+00  COEF=   0.9990601883727E+00  VAL_N= 0.338407E+16  VAL_L= 0.338726E+16
!g_rk_step_prep: ALPHA=.1000E-01  COEF=   0.9999060025300E+00  VAL_N= 0.338694E+14  VAL_L= 0.338726E+14
!g_rk_step_prep: ALPHA=.1000E-02  COEF=   0.9999909575619E+00  VAL_N= 0.338723E+12  VAL_L= 0.338726E+12
!g_rk_step_prep: ALPHA=.1000E-03  COEF=   0.1000034846551E+01  VAL_N= 0.338737E+10  VAL_L= 0.338726E+10
!g_rk_step_prep: ALPHA=.1000E-04  COEF=   0.1003578564589E+01  VAL_N= 0.339938E+08  VAL_L= 0.338726E+08
!g_rk_step_prep: ALPHA=.1000E-05  COEF=   0.1357865847341E+01  VAL_N= 0.459944E+06  VAL_L= 0.338726E+06
!g_rk_step_prep: ALPHA=.1000E-06  COEF=   0.3678658566932E+02  VAL_N= 0.124606E+06  VAL_L= 0.338726E+04
!g_rk_step_prep: ALPHA=.1000E-07  COEF=   0.3579658567082E+04  VAL_N= 0.121252E+06  VAL_L= 0.338726E+02
!g_rk_step_prep: ALPHA=.1000E-08  COEF=   0.3578668567080E+06  VAL_N= 0.121219E+06  VAL_L= 0.338726E+00
!g_rk_step_prep: ALPHA=.1000E-09  COEF=   0.3578658667078E+08  VAL_N= 0.121218E+06  VAL_L= 0.338726E-02
!g_rk_step_prep: ALPHA=.1000E-10  COEF=   0.3578658568079E+10  VAL_N= 0.121218E+06  VAL_L= 0.338726E-04
                 
!a_rk_step_prep:    0.3387256544396E+16   0.3387256544395E+16


END SUBROUTINE t_rk_step_prep

!---------------------------------------------------------------------------------------------------

SUBROUTINE t_init_zero_tendency(ru_tendf, rv_tendf, rw_tendf, ph_tendf,  &
                              t_tendf,  tke_tendf,                     &
                              moist_tendf,chem_tendf,scalar_tendf,      &
                              n_moist,n_chem,n_scalar,rk_step,          &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )
   IMPLICIT NONE

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   INTEGER ,       INTENT(IN   ) :: n_moist,n_chem,n_scalar,rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) ::  &
                                                             ru_tendf, &
                                                             rv_tendf, &
                                                             rw_tendf, &
                                                             ph_tendf, &
                                                              t_tendf, &
                                                            tke_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),INTENT(INOUT)::&
                                                          moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem ),INTENT(INOUT)::&
                                                          chem_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar ),INTENT(INOUT)::&
                                                          scalar_tendf

! LOCAL VARS

   INTEGER :: im, ic,i,j,k,h

!  zzma: new definition

! INOUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )        :: S_ru_tendf, &
                                                             S_rv_tendf, &
                                                             S_rw_tendf, &
                                                             S_ph_tendf, &
                                                             S_t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )        :: P_ru_tendf, &
                                                             P_rv_tendf, &
                                                             P_rw_tendf, &
                                                             P_ph_tendf, &
                                                             P_t_tendf, &
                                                             P_tke_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )        :: B_ru_tendf, &
                                                             B_rv_tendf, &
                                                             B_rw_tendf, &
                                                             B_ph_tendf, &
                                                             B_t_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )        :: K_ru_tendf, &
                                                             K_rv_tendf, &
                                                             K_rw_tendf, &
                                                             K_ph_tendf, &
                                                             K_t_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist)  :: S_moist_tendf, &
                                                            P_moist_tendf, &
                                                            B_moist_tendf, &
                                                            K_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem)  :: S_chem_tendf, &
                                                            P_chem_tendf, &
                                                            B_chem_tendf, &
                                                            K_chem_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar)  :: S_scalar_tendf, &
                                                            P_scalar_tendf, &
                                                            B_scalar_tendf, &
                                                            K_scalar_tendf

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
       S_ru_tendf(i,k,j)=ru_tendf(i,k,j)
       S_rv_tendf(i,k,j)=rv_tendf(i,k,j)
       S_rw_tendf(i,k,j)=rw_tendf(i,k,j)
       S_ph_tendf(i,k,j)=ph_tendf(i,k,j)
       S_t_tendf(i,k,j)=t_tendf(i,k,j)

!      P_ru_tendf(i,k,j)=ru_tendf(i,k,j)
!      P_rv_tendf(i,k,j)=rv_tendf(i,k,j)
!      P_rw_tendf(i,k,j)=rw_tendf(i,k,j)
!      P_ph_tendf(i,k,j)=ph_tendf(i,k,j)
!      P_t_tendf(i,k,j)=t_tendf(i,k,j)

!      K_ru_tendf(i,k,j)=ru_tendf(i,k,j)
!      K_rv_tendf(i,k,j)=rv_tendf(i,k,j)
!      K_rw_tendf(i,k,j)=rw_tendf(i,k,j)
!      K_ph_tendf(i,k,j)=ph_tendf(i,k,j)
!      K_t_tendf(i,k,j)=t_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
       S_moist_tendf(i,k,j,h)=moist_tendf(i,k,j,h)
!      P_moist_tendf(i,k,j,h)=moist_tendf(i,k,j,h)
!      K_moist_tendf(i,k,j,h)=moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!NLM

   CALL init_zero_tendency(ru_tendf, rv_tendf, rw_tendf, ph_tendf,  &
                              t_tendf,  tke_tendf,                     &
                              moist_tendf,chem_tendf,scalar_tendf,     &
                              n_moist,n_chem,n_scalar,rk_step,         &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_ph_tendf(i,k,j)=ph_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      B_moist_tendf(i,k,j,h)=moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL
   CALL g_init_zero_tendency( ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, rw_tendf, P_rw_tendf, ph_tendf, P_ph_tendf, t_tendf, &
&P_t_tendf, moist_tendf, P_moist_tendf, &
chem_tendf, P_chem_tendf, scalar_tendf, P_scalar_tendf,n_moist, n_chem, n_scalar, &
ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L+ P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)    &
                   + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)    &
                   + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)    &
                   + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)    &
                   + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      SAVE_L=SAVE_L + P_moist_tendf(i,k,j,h)*P_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA

      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
!          P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
!          P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
!          P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
!          P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)
!          P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      enddo
      enddo
      enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
!          P_moist_tendf(i,k,j,h)=FACTOR*S_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   CALL init_zero_tendency(P_ru_tendf, P_rv_tendf, P_rw_tendf, P_ph_tendf,  &
                              P_t_tendf,  tke_tendf,                     &
                              P_moist_tendf,P_chem_tendf,p_scalar_tendf,        &
                              n_moist,n_chem,n_scalar,rk_step,                  &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )

   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N +(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))   &
                  +(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))      &
                  +(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))      &
                  +(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))*(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))      &
                  +(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      VAL_N=VAL_N +(P_moist_tendf(i,k,j,h)-B_moist_tendf(i,k,j,h))*(P_moist_tendf(i,k,j,h)-B_moist_tendf(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      if(VAL_L == 0.) then
         COEF = 1.
      else
         COEF=VAL_N/VAL_L
      endif

      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_init_zero_tendency: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
!          ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
!          rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
!          rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
!          ph_tendf(i,k,j)=S_ph_tendf(i,k,j)
!          t_tendf(i,k,j)=S_t_tendf(i,k,j)

!          P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
!          P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
!          P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
!          P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)
!          P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)

!          B_ru_tendf(i,k,j)=P_ru_tendf(i,k,j)
!          B_rv_tendf(i,k,j)=P_rv_tendf(i,k,j)
!          B_rw_tendf(i,k,j)=P_rw_tendf(i,k,j)
!          B_ph_tendf(i,k,j)=P_ph_tendf(i,k,j)
!          B_t_tendf(i,k,j)=P_t_tendf(i,k,j)
!
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
!          moist_tendf(i,k,j,h)=S_moist_tendf(i,k,j,h)
!          P_moist_tendf(i,k,j,h)=FACTOR*S_moist_tendf(i,k,j,h)
!          B_moist_tendf(i,k,j,h)=P_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL
   CALL g_init_zero_tendency( ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, rw_tendf, P_rw_tendf, ph_tendf, P_ph_tendf, t_tendf, &
&P_t_tendf, moist_tendf, P_moist_tendf, chem_tendf, P_chem_tendf,scalar_tendf, P_scalar_tendf, &
n_moist, n_chem, n_scalar, &
ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L +P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                  +P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                  +P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                  +P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)   &
                  +P_t_tendf(i,k,j)*P_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      VAL_L=VAL_L +P_moist_tendf(i,k,j,h)*P_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  ADJ

   CALL a_init_zero_tendency( P_ru_tendf, P_rv_tendf, P_rw_tendf, P_ph_tendf, P_t_tendf, p_tke_tendf, &
P_moist_tendf, &
     P_chem_tendf,P_scalar_tendf, n_moist, n_chem, n_scalar, ims, ime, jms, &
&jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   DO I=ims,ime
   DO K=kms,kme
   DO J=jms,jme
!      VAL_A=VAL_A +P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)  &
!                  +P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)  &
!                  +P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)  &
!                  +P_ph_tendf(i,k,j)*B_ph_tendf(i,k,j)  &
!                  +P_t_tendf(i,k,j)*B_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
!      VAL_A=VAL_A +P_moist_tendf(i,k,j,h)*B_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   write(6,fmt='(A,2E22.13)') 'a_init_zero_tendency: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
          ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
          rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
          rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
          ph_tendf(i,k,j)=S_ph_tendf(i,k,j)
          t_tendf(i,k,j)=S_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
          moist_tendf(i,k,j,h)=S_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!g_init_zero_tendency: ALPHA=.1000E+00  COEF=   0.1000000953674E+01  VAL_N= 0.115569E-19  VAL_L= 0.115569E-19
!g_init_zero_tendency: ALPHA=.1000E-01  COEF=   0.1000003337860E+01  VAL_N= 0.115569E-21  VAL_L= 0.115569E-21
!g_init_zero_tendency: ALPHA=.1000E-02  COEF=   0.1000092029572E+01  VAL_N= 0.115579E-23  VAL_L= 0.115569E-23
!g_init_zero_tendency: ALPHA=.1000E-03  COEF=   0.9997125864029E+00  VAL_N= 0.115536E-25  VAL_L= 0.115569E-25
!g_init_zero_tendency: ALPHA=.1000E-04  COEF=   0.9984483122826E+00  VAL_N= 0.115389E-27  VAL_L= 0.115569E-27
!g_init_zero_tendency: ALPHA=.1000E-05  COEF=   0.8998992443085E+00  VAL_N= 0.104000E-29  VAL_L= 0.115569E-29
!g_init_zero_tendency: ALPHA=.1000E-06  COEF=   0.1599820733070E+01  VAL_N= 0.184889E-31  VAL_L= 0.115569E-31
!g_init_zero_tendency: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.115569E-33
!g_init_zero_tendency: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.115569E-35
!g_init_zero_tendency: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.115569E-37
!g_init_zero_tendency: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.115569E-39

!a_init_zero_tendency:    0.1155687153334E-19   0.1155687153334E-19

END SUBROUTINE t_init_zero_tendency

!------------------------------------------------------------------------------------------------------------

SUBROUTINE t_phy_prep ( config_flags,                                &  ! input
                         mu, u, v, p, pb, alt, ph,                    &  ! input
                         phb, t, tsk, moist, n_moist,                 &  ! input
                         mu_3d, rho, th_phy, p_phy , pi_phy ,         &  ! output
                         u_phy, v_phy, p8w, t_phy, t8w,               &  ! output
                         z, z_at_w, dz8w,                             &  ! output
                         fzm, fzp,                                    &  ! params
                         RTHRATEN,                                    &
                         RTHBLTEN, RUBLTEN, RVBLTEN,                  &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,                &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,                &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,                &
                         RTHFTEN,  RQVFTEN,                           &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte                )

   IMPLICIT NONE
!----------------------------------------------------------------------

   TYPE(grid_config_rec_type) ,     INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::   ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   INTEGER ,          INTENT(IN   ) :: n_moist

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), INTENT(IN) :: moist


   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::     TSK, mu

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  ::   u_phy, &
                                                              v_phy, &
                                                             pi_phy, &
                                                              p_phy, &
                                                                p8w, &
                                                              t_phy, &
                                                             th_phy, &
                                                                t8w, &
                                                              mu_3d, &
                                                                rho, &
                                                                  z, &
                                                               dz8w, &
                                                              z_at_w

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(IN   )                                  ::      pb, &
                                                                  u, &
                                                                  v, &
                                                                alt, &
                                                                phb

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )      ::       p,ph,t

   REAL , DIMENSION( kms:kme ) ,           INTENT(IN   ) ::     fzm,   &
                                                                fzp

   REAL,  DIMENSION( ims:ime , kms:kme, jms:jme ),                   &
          INTENT(INOUT)   ::                               RTHRATEN

   REAL,  DIMENSION( ims:ime , kms:kme, jms:jme ),                   &
          INTENT(INOUT)   ::                               RTHCUTEN, &
                                                           RQVCUTEN, &
                                                           RQCCUTEN, &
                                                           RQRCUTEN, &
                                                           RQICUTEN, &
                                                           RQSCUTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                RUBLTEN, &
                                                            RVBLTEN, &
                                                           RTHBLTEN, &
                                                           RQVBLTEN, &
                                                           RQCBLTEN, &
                                                           RQIBLTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                RTHFTEN, &
                                                            RQVFTEN

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k
   REAL    :: w1, w2, z0, z1, z2

!  zzma: new definition

!  IN variables

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )       ::       S_p,  &
                                                                  S_ph, &
                                                                  S_t
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )       ::       P_p,  &
                                                                  P_ph, &
                                                                  P_t
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )       ::       B_p,  &
                                                                  B_ph, &
                                                                  B_t
!  OUT variables

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )       ::     P_pi_phy, &
                                                                P_p_phy,  &
                                                                P_p8w,    &
                                                                P_t_phy,  &
                                                                P_th_phy, &
                                                                P_t8w,    &
                                                                P_z,      &
                                                                P_z_at_w
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme )       ::     B_pi_phy, &
                                                                B_p_phy,  &
                                                                B_p8w,    &
                                                                B_t_phy,  &
                                                                B_th_phy, &
                                                                B_t8w,    &
                                                                B_z,      &
                                                                B_z_at_w

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_p(i,k,j)=p(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)
      S_t(i,k,j)=t(i,k,j)

      P_p(i,k,j)=p(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
      P_t(i,k,j)=t(i,k,j)
   enddo
   enddo
   enddo

!  NLM

   CALL phy_prep ( config_flags,                                &  ! input
                         mu, u, v, p, pb, alt, ph,                    &  ! input
                         phb, t, tsk, moist, n_moist,                 &  ! input
                         mu_3d, rho, th_phy, p_phy , pi_phy ,         &  ! output
                         u_phy, v_phy, p8w, t_phy, t8w,               &  ! output
                         z, z_at_w, dz8w,                             &  ! output
                         fzm, fzp,                                    &  ! params
                         RTHRATEN,                                    &
                         RTHBLTEN, RUBLTEN, RVBLTEN,                  &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,                &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,                &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,                &
                         RTHFTEN,  RQVFTEN,                           &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte                )


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_pi_phy(i,k,j)=pi_phy(i,k,j)
      B_p_phy(i,k,j)=p_phy(i,k,j)
      B_p8w(i,k,j)=p8w(i,k,j)
      B_t_phy(i,k,j)=t_phy(i,k,j)
      B_th_phy(i,k,j)=th_phy(i,k,j)
      B_t8w(i,k,j)=t8w(i,k,j)
      B_z(i,k,j)=z(i,k,j)
      B_z_at_w(i,k,j)=z_at_w(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_phy_prep( p, P_p, pb, ph, P_ph, phb, t, P_t, mu_3d, rho, th_phy, P_th_phy, p_phy, P_p_phy, pi_phy, P_pi_phy, u_phy, &
&v_phy, p8w, P_p8w, t_phy, P_t_phy, t8w, P_t8w, z, P_z, z_at_w, P_z_at_w, dz8w, fzm, fzp, rthraten, rthblten, rublten, rvblten, &
&rqvblten, rqcblten, rqiblten, rthcuten, rqvcuten, rqccuten, rqrcuten, rqicuten, rqscuten, rthften, rqvften, ide, jde, kde, ims, &
&ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_pi_phy(i,k,j)*P_pi_phy(i,k,j)    &
                    + P_p_phy(i,k,j)*P_p_phy(i,k,j)      &
                    + P_p8w(i,k,j)*P_p8w(i,k,j)          &
                    + P_t_phy(i,k,j)*P_t_phy(i,k,j)      &
                    + P_th_phy(i,k,j)*P_th_phy(i,k,j)    &
                    + P_t8w(i,k,j)*P_t8w(i,k,j)          &
                    + P_z(i,k,j)*P_z(i,k,j)              &
                    + P_z_at_w(i,k,j)*P_z_at_w(i,k,j)

   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         P_p(i,k,j)=FACTOR*S_p(i,k,j)
         P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
         P_t(i,k,j)=FACTOR*S_t(i,k,j)
      enddo
      enddo
      enddo
      CALL phy_prep ( config_flags,                                &  ! input
                         mu, u, v, P_p, pb, alt, P_ph,                    &  ! input
                         phb, P_t, tsk, moist, n_moist,                 &  ! input
                         mu_3d, rho, P_th_phy, P_p_phy , P_pi_phy ,         &  ! output
                         u_phy, v_phy, P_p8w, P_t_phy, P_t8w,               &  ! output
                         P_z, P_z_at_w, dz8w,                             &  ! output
                         fzm, fzp,                                    &  ! params
                         RTHRATEN,                                    &
                         RTHBLTEN, RUBLTEN, RVBLTEN,                  &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,                &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,                &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,                &
                         RTHFTEN,  RQVFTEN,                           &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte                )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_pi_phy(i,k,j)-B_pi_phy(i,k,j))*(P_pi_phy(i,k,j)-B_pi_phy(i,k,j))    &
                     + (P_p_phy(i,k,j)-B_p_phy(i,k,j))*(P_p_phy(i,k,j)-B_p_phy(i,k,j))        &
                     + (P_p8w(i,k,j)-B_p8w(i,k,j))*(P_p8w(i,k,j)-B_p8w(i,k,j))                &
                     + (P_t_phy(i,k,j)-B_t_phy(i,k,j))*(P_t_phy(i,k,j)-B_t_phy(i,k,j))        &
                     + (P_th_phy(i,k,j)-B_th_phy(i,k,j))*(P_th_phy(i,k,j)-B_th_phy(i,k,j))    &
                     + (P_t8w(i,k,j)-B_t8w(i,k,j))*(P_t8w(i,k,j)-B_t8w(i,k,j))                &
                     + (P_z(i,k,j)-B_z(i,k,j))*(P_z(i,k,j)-B_z(i,k,j))                        &
                     + (P_z_at_w(i,k,j)-B_z_at_w(i,k,j))*(P_z_at_w(i,k,j)-B_z_at_w(i,k,j))

                     
      enddo
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_phy_prep: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p(i,k,j)=S_p(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      t(i,k,j)=S_t(i,k,j)

      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)

      B_p(i,k,j)=P_p(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_phy_prep( p, P_p, pb, ph, P_ph, phb, t, P_t, mu_3d, rho, th_phy, P_th_phy, p_phy, P_p_phy, pi_phy, P_pi_phy, u_phy, &
&v_phy, p8w, P_p8w, t_phy, P_t_phy, t8w, P_t8w, z, P_z, z_at_w, P_z_at_w, dz8w, fzm, fzp, rthraten, rthblten, rublten, rvblten, &
&rqvblten, rqcblten, rqiblten, rthcuten, rqvcuten, rqccuten, rqrcuten, rqicuten, rqscuten, rthften, rqvften, ide, jde, kde, ims, &
&ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_pi_phy(i,k,j)*P_pi_phy(i,k,j)    &
                    + P_p_phy(i,k,j)*P_p_phy(i,k,j)      &
                    + P_p8w(i,k,j)*P_p8w(i,k,j)          &
                    + P_t_phy(i,k,j)*P_t_phy(i,k,j)      &
                    + P_th_phy(i,k,j)*P_th_phy(i,k,j)    &
                    + P_t8w(i,k,j)*P_t8w(i,k,j)          &
                    + P_z(i,k,j)*P_z(i,k,j)              &
                    + P_z_at_w(i,k,j)*P_z_at_w(i,k,j)

   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_p(i,k,j)=0.0
      P_ph(i,k,j)=0.0
      P_t(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ
   call a_phy_prep( p, P_p, pb, ph, P_ph, phb, t, P_t, th_phy, P_th_phy, p_phy, P_p_phy, pi_phy, P_pi_phy, P_p8w, t_phy, &
&P_t_phy, P_t8w, z, P_z, z_at_w, P_z_at_w, fzm, fzp, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   DO I=ims,ime
   DO K=kms,kme
   DO J=jms,jme
      VAL_A=VAL_A    + P_p(i,k,j)*B_p(i,k,j)      &
                     + P_ph(i,k,j)*B_ph(i,k,j)  &
                     + P_t(i,k,j)*B_t(i,k,j)

   enddo
   enddo
   enddo

   write(6,fmt='(A,2E22.13)') 'a_phy_prep: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p(i,k,j)=S_p(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      t(i,k,j)=S_t(i,k,j)
   enddo
   enddo
   enddo

!g_phy_prep: ALPHA=.1000E+00  COEF=   0.1000007867813E+01  VAL_N= 0.283924E+08  VAL_L= 0.283921E+08
!g_phy_prep: ALPHA=.1000E-01  COEF=   0.1000007271767E+01  VAL_N= 0.283923E+06  VAL_L= 0.283921E+06
!g_phy_prep: ALPHA=.1000E-02  COEF=   0.9970672726631E+00  VAL_N= 0.283089E+04  VAL_L= 0.283921E+04
!g_phy_prep: ALPHA=.1000E-03  COEF=   0.9730178117752E+00  VAL_N= 0.276261E+02  VAL_L= 0.283921E+02
!g_phy_prep: ALPHA=.1000E-04  COEF=   0.2119694232941E+01  VAL_N= 0.601827E+00  VAL_L= 0.283921E+00
!g_phy_prep: ALPHA=.1000E-05  COEF=   0.9598098397255E+00  VAL_N= 0.272511E-02  VAL_L= 0.283921E-02
!g_phy_prep: ALPHA=.1000E-06  COEF=   0.9365414977074E+00  VAL_N= 0.265904E-04  VAL_L= 0.283921E-04
!g_phy_prep: ALPHA=.1000E-07  COEF=   0.4753468885871E-11  VAL_N= 0.134961E-17  VAL_L= 0.283921E-06
!g_phy_prep: ALPHA=.1000E-08  COEF=   0.4753468729746E-09  VAL_N= 0.134961E-17  VAL_L= 0.283921E-08
!g_phy_prep: ALPHA=.1000E-09  COEF=   0.4753468019203E-07  VAL_N= 0.134961E-17  VAL_L= 0.283922E-10
!g_phy_prep: ALPHA=.1000E-10  COEF=   0.4753467692353E-05  VAL_N= 0.134961E-17  VAL_L= 0.283922E-12

!a_phy_prep:    0.2839517000000E+08   0.2839852400000E+08


END SUBROUTINE t_phy_prep

!------------------------------------------------------------------------------------------------------

SUBROUTINE t_calculate_km_kh( config_flags, dt,                        &
                                dampcoef, zdamp, damp_opt,               &
                                xkmh, xkmhd, xkmv, xkhh, xkhv,           &
                                BN2, khdif, kvdif, div,                  &
                                defor11, defor22, defor33,               &
                                defor12, defor13, defor23,               &
                                tke, p8w, t8w, theta, t, p, moist,       &
                                dn, dnw, dx, dy, rdz, rdzw, cr_len,      &
                                n_moist, cf1, cf2, cf3, warm_rain,       &
                                kh_tke_upper_bound, kv_tke_upper_bound,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                its, ite, jts, jte, kts, kte             )

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: n_moist, damp_opt,             &
       ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    LOGICAL, INTENT( IN )  &
    :: warm_rain

    REAL, INTENT( IN )  &
    :: cr_len, dx, dy, zdamp, dt, dampcoef, cf1, cf2, cf3, khdif, kvdif

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: dnw, dn

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )    :: moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: xkmv, xkmh, xkhv, xkhh

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  INTENT( IN )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23,      &
       div, rdz, rdzw

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  p8w, t8w, theta, t, p,xkmhd,BN2

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tke

    REAL, INTENT( IN )  &
    :: kh_tke_upper_bound, kv_tke_upper_bound

! Local variables.

    INTEGER  &
    :: i_start, i_end, j_start, j_end, ktf, i, j, k

!  zzma: new definition

!  IN variables

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  S_p8w, S_t8w, S_theta, S_t, S_p
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  P_p8w, P_t8w, P_theta, P_t, P_p
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  B_p8w, B_t8w, B_theta, B_t, B_p

!  INOUT variables

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )   :: S_moist,P_moist,B_moist,K_moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme )      :: S_xkmhd,S_BN2,P_xkmhd,P_BN2,B_xkmhd,B_BN2,K_xkmhd,K_BN2

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

!  TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_p8w(i,k,j)=p8w(i,k,j)
      S_t8w(i,k,j)=t8w(i,k,j)
      S_theta(i,k,j)=theta(i,k,j)
      S_t(i,k,j)=t(i,k,j)
      S_p(i,k,j)=p(i,k,j)

      P_p8w(i,k,j)=p8w(i,k,j)
      P_t8w(i,k,j)=t8w(i,k,j)
      P_theta(i,k,j)=theta(i,k,j)
      P_t(i,k,j)=t(i,k,j)
      P_p(i,k,j)=p(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      S_moist(i,k,j,h)=moist(i,k,j,h)

      P_moist(i,k,j,h)=moist(i,k,j,h)

      K_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_xkmhd(i,k,j)=xkmhd(i,k,j)
      S_bn2(i,k,j)=bn2(i,k,j)

      P_xkmhd(i,k,j)=xkmhd(i,k,j)
      P_bn2(i,k,j)=bn2(i,k,j)

      K_xkmhd(i,k,j)=xkmhd(i,k,j)
      K_bn2(i,k,j)=bn2(i,k,j)
   enddo
   enddo
   enddo

!  NLM
   CALL calculate_km_kh( config_flags, dt,                        &
                                dampcoef, zdamp, damp_opt,               &
                                xkmh, xkmhd, xkmv, xkhh, xkhv,           &
                                BN2, khdif, kvdif, div,                  &
                                defor11, defor22, defor33,               &
                                defor12, defor13, defor23,               &
                                tke, p8w, t8w, theta, t, p, moist,       &
                                dn, dnw, dx, dy, rdz, rdzw, cr_len,      &
                                n_moist, cf1, cf2, cf3, warm_rain,       &
                                kh_tke_upper_bound, kv_tke_upper_bound,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                its, ite, jts, jte, kts, kte             )
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      B_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_xkmhd(i,k,j)=xkmhd(i,k,j)
      B_bn2(i,k,j)=bn2(i,k,j)
   enddo
   enddo
   enddo

!  TGL
   CALL g_calculate_km_kh( config_flags, dt, dampcoef, zdamp, damp_opt, xkmh, K_xkmhd, P_xkmhd, xkmv, xkhh, xkhv, K_bn2, P_bn2, &
&khdif, defor11, defor22, defor33, defor12, defor13, defor23, tke, p8w, P_p8w, t8w, P_t8w, theta, P_theta, t, P_t, p, P_p, K_moist, &
&P_moist, dx, dy, rdz, rdzw, n_moist, cf1, cf2, cf3, kh_tke_upper_bound, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, &
&its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      SAVE_L=SAVE_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L+P_xkmhd(i,k,j)*P_xkmhd(i,k,j)  +P_bn2(i,k,j)*P_bn2(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
      P_p8w(i,k,j)=FACTOR*S_p8w(i,k,j)
      P_t8w(i,k,j)=FACTOR*S_t8w(i,k,j)
      P_theta(i,k,j)=FACTOR*S_theta(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)
      P_bn2(i,k,j)=FACTOR*S_bn2(i,k,j)
   enddo
   enddo
   enddo

   CALL calculate_km_kh( config_flags, dt,                        &
                                dampcoef, zdamp, damp_opt,               &
                                xkmh, P_xkmhd, xkmv, xkhh, xkhv,           &
                                P_BN2, khdif, kvdif, div,                  &
                                defor11, defor22, defor33,               &
                                defor12, defor13, defor23,               &
                                tke, P_p8w, P_t8w, P_theta, P_t, P_p, P_moist,       &
                                dn, dnw, dx, dy, rdz, rdzw, cr_len,      &
                                n_moist, cf1, cf2, cf3, warm_rain,       &
                                kh_tke_upper_bound, kv_tke_upper_bound,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                its, ite, jts, jte, kts, kte             )
      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
      do h=1  ,n_moist
         VAL_N=VAL_N+(P_moist(i,k,j,h)-B_moist(i,k,j,h))*(P_moist(i,k,j,h)-B_moist(i,k,j,h))
      enddo
      enddo
      enddo
      enddo

      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_xkmhd(i,k,j)-B_xkmhd(i,k,j))*(P_xkmhd(i,k,j)-B_xkmhd(i,k,j))   & 
                     + (P_bn2(i,k,j) -B_bn2(i,k,j))*(P_bn2(i,k,j) -B_bn2(i,k,j))
      enddo
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calculate_km_kh: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p8w(i,k,j)=S_p8w(i,k,j)
      t8w(i,k,j)=S_t8w(i,k,j)
      theta(i,k,j)=S_theta(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      p(i,k,j)=S_p(i,k,j)

      P_p8w(i,k,j)=FACTOR*S_p8w(i,k,j)
      P_t8w(i,k,j)=FACTOR*S_t8w(i,k,j)
      P_theta(i,k,j)=FACTOR*S_theta(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)

      B_p8w(i,k,j)=P_p8w(i,k,j)
      B_t8w(i,k,j)=P_t8w(i,k,j)
      B_theta(i,k,j)=P_theta(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)
      K_moist(i,k,j,h)=S_moist(i,k,j,h)

      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)

      B_moist(i,k,j,h)=P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      xkmhd(i,k,j)=S_xkmhd(i,k,j)
      bn2(i,k,j)=S_bn2(i,k,j)
      K_xkmhd(i,k,j)=S_xkmhd(i,k,j)
      K_bn2(i,k,j)=S_bn2(i,k,j)

      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)
      P_bn2(i,k,j)=FACTOR*S_bn2(i,k,j)

      B_xkmhd(i,k,j)=P_xkmhd(i,k,j)
      B_bn2(i,k,j)=P_bn2(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calculate_km_kh( config_flags, dt, dampcoef, zdamp, damp_opt, xkmh, xkmhd, P_xkmhd, xkmv, xkhh, xkhv, bn2, P_bn2, &
&khdif, defor11, defor22, defor33, defor12, defor13, defor23, tke, p8w, P_p8w, t8w, P_t8w, theta, P_theta, t, P_t, p, P_p, moist, &
&P_moist, dx, dy, rdz, rdzw, n_moist, cf1, cf2, cf3, kh_tke_upper_bound, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, &
&its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
!      VAL_L=VAL_L + P_p8w(i,k,j)*P_p8w(i,k,j)       &
!                  + P_t8w(i,k,j)*P_t8w(i,k,j)       &
!                  + P_theta(i,k,j)*P_theta(i,k,j)   &
!                  + P_t(i,k,j)*P_t(i,k,j)           &
!                  + P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      VAL_L=VAL_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L+P_xkmhd(i,k,j)*P_xkmhd(i,k,j)  +P_bn2(i,k,j)*P_bn2(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_p8w(i,k,j)=0.0
      P_t8w(i,k,j)=0.0
      P_theta(i,k,j)=0.0
      P_t(i,k,j)=0.0
      P_p(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ

   call a_calculate_km_kh( config_flags, dt, dampcoef, zdamp, damp_opt, xkmh, K_xkmhd, P_xkmhd, xkmv, xkhh, xkhv, K_bn2, P_bn2, &
&khdif, div, defor11, defor22, defor33, defor12, defor13, defor23, tke, p8w, P_p8w, t8w, P_t8w, theta, P_theta, t, P_t, p, P_p, &
&K_moist, P_moist, dn, dnw, dx, dy, rdz, rdzw, n_moist, cf1, cf2, cf3, kh_tke_upper_bound, ids, ide, jds, jde, kde, ims, ime, jms, &
&jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   DO I=ims,ime
   DO K=kms,kme
   DO J=jms,jme
      VAL_A=VAL_A + P_p8w(i,k,j)*B_p8w(i,k,j)      & 
                  + P_t8w(i,k,j)*B_t8w(i,k,j)      &
                  + P_theta(i,k,j)*B_theta(i,k,j)  &
                  + P_t(i,k,j) *B_t(i,k,j)         &
                  + P_p(i,k,j) *B_p(i,k,j)
   END DO
   END DO
   END DO

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      VAL_A=VAL_A + P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_xkmhd(i,k,j)*B_xkmhd(i,k,j) +P_bn2(i,k,j)*B_bn2(i,k,j)
   enddo
   enddo
   enddo


   write(6,fmt='(A,2E22.13)') 'a_calculate_km_kh: ', VAL_L,VAL_A

!  RECOVER


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p8w(i,k,j)=S_p8w(i,k,j)
      t8w(i,k,j)=S_t8w(i,k,j)
      theta(i,k,j)=S_theta(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      p(i,k,j)=S_p(i,k,j)

   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1  ,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      xkmhd(i,k,j)=S_xkmhd(i,k,j)
      bn2(i,k,j)=S_bn2(i,k,j)
   enddo
   enddo
   enddo

!g_calculate_km_kh: ALPHA=.1000E+00  COEF=   0.3474252223969E+01  VAL_N= 0.534163E-01  VAL_L= 0.153749E-01
!g_calculate_km_kh: ALPHA=.1000E-01  COEF=   0.1000073671341E+01  VAL_N= 0.153760E-03  VAL_L= 0.153749E-03
!g_calculate_km_kh: ALPHA=.1000E-02  COEF=   0.1000226140022E+01  VAL_N= 0.153784E-05  VAL_L= 0.153749E-05
!g_calculate_km_kh: ALPHA=.1000E-03  COEF=   0.1000114679337E+01  VAL_N= 0.153767E-07  VAL_L= 0.153749E-07
!g_calculate_km_kh: ALPHA=.1000E-04  COEF=   0.1005010604858E+01  VAL_N= 0.154519E-09  VAL_L= 0.153749E-09
!g_calculate_km_kh: ALPHA=.1000E-05  COEF=   0.8787735104561E+00  VAL_N= 0.135111E-11  VAL_L= 0.153749E-11
!g_calculate_km_kh: ALPHA=.1000E-06  COEF=   0.1789559841156E+01  VAL_N= 0.275143E-13  VAL_L= 0.153749E-13
!g_calculate_km_kh: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.153749E-15
!g_calculate_km_kh: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.153749E-17
!g_calculate_km_kh: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.153749E-19
!g_calculate_km_kh: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.153749E-21

!a_calculate_km_kh:    0.1537548657507E-01   0.1537548657507E-01

END SUBROUTINE t_calculate_km_kh

!--------------------------------------------------------------------------------------------------------

SUBROUTINE t_relax_bdy_dry ( config_flags,                                    &
                              ru_tendf, rv_tendf, ph_tendf, t_tendf,           &
                              rw_tendf, mu_tend,                               &
                              ru, rv, ph, t,                                   &
                              w, mu, mut,                                      &
                              u_b, v_b, ph_b, t_b,                             &
                              w_b, mu_b,                                       &
                              u_bt, v_bt, ph_bt, t_bt,                         &
                              w_bt, mu_bt,                                     &
                              spec_bdy_width, spec_zone, relax_zone,           &
                              dtbc, fcx, gcx,             &
                              ijds, ijde,                 & ! min/max(id,jd)
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its, ite, jts, jte, kts, kte)

! Zaizhong Ma, March 24,2005

   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone, relax_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )      :: ru,     &
                                                                      rv,     &
                                                                      ph,     &
                                                                      w,      &
                                                                      t
   REAL , DIMENSION( ims:ime , jms:jme  )               :: mu  , &
                                                                      mut
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )  :: ru_tendf, &
                                                                      rv_tendf, &
                                                                      ph_tendf, &
                                                                      rw_tendf, &
                                                                      t_tendf
   REAL , DIMENSION( ims:ime , jms:jme  )            :: mu_tend
   REAL , DIMENSION( spec_bdy_width) , INTENT(IN   ) :: fcx, gcx

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )   :: u_b, &
                                                                                 v_b, &
                                                                                 ph_b, &
                                                                                  w_b, &
                                                                                 t_b, &
                                                                                 u_bt, &
                                                                                 v_bt, &
                                                                                 ph_bt, &
                                                                                  w_bt, &
                                                                                 t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1     , spec_bdy_width, 4 )     :: mu_b, &
                                                                                 mu_bt
   REAL, INTENT(IN   ) :: dtbc

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: rfield
   INTEGER :: i_start, i_end, j_start, j_end, i, j, k

!  zzma: new definition

!  IN variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: S_ru,     &
                                                                      S_rv,     &
                                                                      S_ph,     &
                                                                      S_w,      &
                                                                      S_t
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: P_ru,     &
                                                                      P_rv,     &
                                                                      P_ph,     &
                                                                      P_w,      &
                                                                      P_t
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: B_ru,     &
                                                                      B_rv,     &
                                                                      B_ph,     &
                                                                      B_w,      &
                                                                      B_t

   REAL , DIMENSION( ims:ime , jms:jme  )         :: S_mu, S_mut,P_mu, P_mut,B_mu, B_mut

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: S_u_b, &
                                                                                 S_v_b, &
                                                                                 S_ph_b, &
                                                                                 S_w_b, &
                                                                                 S_t_b, &
                                                                                 S_u_bt, &
                                                                                 S_v_bt, &
                                                                                 S_ph_bt, &
                                                                                 S_w_bt, &
                                                                                 S_t_bt

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: P_u_b, &
                                                                                 P_v_b, &
                                                                                 P_ph_b, &
                                                                                 P_w_b, &
                                                                                 P_t_b, &
                                                                                 P_u_bt, &
                                                                                 P_v_bt, &
                                                                                 P_ph_bt, &
                                                                                 P_w_bt, &
                                                                                 P_t_bt

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: B_u_b, &
                                                                                 B_v_b, &
                                                                                 B_ph_b, &
                                                                                 B_w_b, &
                                                                                 B_t_b, &
                                                                                 B_u_bt, &
                                                                                 B_v_bt, &
                                                                                 B_ph_bt, &
                                                                                 B_w_bt, &
                                                                                 B_t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1 , spec_bdy_width, 4 )  :: S_mu_b, S_mu_bt,P_mu_b, P_mu_bt,B_mu_b, B_mu_bt

!  INOUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: S_ru_tendf, &
                                                                      S_rv_tendf, &
                                                                      S_ph_tendf, &
                                                                      S_rw_tendf, &
                                                                      S_t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: P_ru_tendf, &
                                                                      P_rv_tendf, &
                                                                      P_ph_tendf, &
                                                                      P_rw_tendf, &
                                                                      P_t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: K_ru_tendf, &
                                                                      K_rv_tendf, &
                                                                      K_ph_tendf, &
                                                                      K_rw_tendf, &
                                                                      K_t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: B_ru_tendf, &
                                                                      B_rv_tendf, &
                                                                      B_ph_tendf, &
                                                                      B_rw_tendf, &
                                                                      B_t_tendf

   REAL , DIMENSION( ims:ime , jms:jme  )  :: S_mu_tend,P_mu_tend,K_mu_tend,B_mu_tend

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru(i,k,j)=ru(i,k,j)
      S_rv(i,k,j)=rv(i,k,j)
      S_w(i,k,j)=w(i,k,j)
      S_t(i,k,j)=t(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)

      P_ru(i,k,j)=ru(i,k,j)
      P_rv(i,k,j)=rv(i,k,j)
      P_w(i,k,j)=w(i,k,j)
      P_t(i,k,j)=t(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      S_mut(i,j)=mut(i,j)

      P_mu(i,j)=mu(i,j)
      P_mut(i,j)=mut(i,j)
   enddo
   enddo

   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      S_u_b(i,k,j,h)=u_b(i,k,j,h)
      S_v_b(i,k,j,h)=v_b(i,k,j,h)
      S_ph_b(i,k,j,h)=ph_b(i,k,j,h)
      S_w_b(i,k,j,h)=w_b(i,k,j,h)
      S_t_b(i,k,j,h)=t_b(i,k,j,h)
      S_u_bt(i,k,j,h)=u_bt(i,k,j,h)
      S_v_bt(i,k,j,h)=v_bt(i,k,j,h)
      S_ph_bt(i,k,j,h)=ph_bt(i,k,j,h)
      S_w_bt(i,k,j,h)=w_bt(i,k,j,h)
      S_t_bt(i,k,j,h)=t_bt(i,k,j,h)

      P_u_b(i,k,j,h)=u_b(i,k,j,h)
      P_v_b(i,k,j,h)=v_b(i,k,j,h)
      P_ph_b(i,k,j,h)=ph_b(i,k,j,h)
      P_w_b(i,k,j,h)=w_b(i,k,j,h)
      P_t_b(i,k,j,h)=t_b(i,k,j,h)
      P_u_bt(i,k,j,h)=u_bt(i,k,j,h)
      P_v_bt(i,k,j,h)=v_bt(i,k,j,h)
      P_ph_bt(i,k,j,h)=ph_bt(i,k,j,h)
      P_w_bt(i,k,j,h)=w_bt(i,k,j,h)
      P_t_bt(i,k,j,h)=t_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      S_mu_b(i,k,j,h)=mu_b(i,k,j,h)
      S_mu_bt(i,k,j,h)=mu_bt(i,k,j,h)

      P_mu_b(i,k,j,h)=mu_b(i,k,j,h)
      P_mu_bt(i,k,j,h)=mu_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo






   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      S_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      S_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      S_t_tendf(i,k,j)=t_tendf(i,k,j)
      S_ph_tendf(i,k,j)=ph_tendf(i,k,j)

      P_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=ph_tendf(i,k,j)

      K_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      K_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      K_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      K_t_tendf(i,k,j)=t_tendf(i,k,j)
      K_ph_tendf(i,k,j)=ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      S_mu_tend(i,j)=mu_tend(i,j)
      P_mu_tend(i,j)=mu_tend(i,j)
      K_mu_tend(i,j)=mu_tend(i,j)
   enddo
   enddo

!NLM

   CALL relax_bdy_dry ( config_flags,                                    &
                              ru_tendf, rv_tendf, ph_tendf, t_tendf,           &
                              rw_tendf, mu_tend,                               &
                              ru, rv, ph, t,                                   &
                              w, mu, mut,                                      &
                              u_b, v_b, ph_b, t_b,                             &
                              w_b, mu_b,                                       &
                              u_bt, v_bt, ph_bt, t_bt,                         &
                              w_bt, mu_bt,                                     &
                              spec_bdy_width, spec_zone, relax_zone,           &
                              dtbc, fcx, gcx,             &
                              ijds, ijde,                 & ! min/max(id,jd)
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its, ite, jts, jte, kts, kte)

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
      B_ph_tendf(i,k,j)=ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      B_mu_tend(i,j)=mu_tend(i,j)
   enddo
   enddo

!  TCL

   CALL g_relax_bdy_dry( config_flags, K_ru_tendf, P_ru_tendf, K_rv_tendf, P_rv_tendf, K_ph_tendf, P_ph_tendf, K_t_tendf, P_t_tendf, &
&K_rw_tendf, P_rw_tendf, K_mu_tend, P_mu_tend, ru, P_ru, rv, P_rv, ph, P_ph, t, P_t, w, P_w, mu, P_mu, mut, P_mut, u_b, P_u_b, v_b, &
&P_v_b, ph_b, P_ph_b, t_b, P_t_b, w_b, P_w_b, mu_b, P_mu_b, u_bt, P_u_bt, v_bt, P_v_bt, ph_bt, P_ph_bt, t_bt, P_t_bt, w_bt, P_w_bt,&
& mu_bt, P_mu_bt, spec_bdy_width, spec_zone, relax_zone, dtbc, fcx, gcx, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)     &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_mu_tend(i,j)*P_mu_tend(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
   enddo
   enddo
   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      P_u_b(i,k,j,h)=FACTOR*S_u_b(i,k,j,h)
      P_v_b(i,k,j,h)=FACTOR*S_v_b(i,k,j,h)
      P_ph_b(i,k,j,h)=FACTOR*S_ph_b(i,k,j,h)
      P_w_b(i,k,j,h)=FACTOR*S_w_b(i,k,j,h)
      P_t_b(i,k,j,h)=FACTOR*S_t_b(i,k,j,h)
      P_u_bt(i,k,j,h)=FACTOR*S_u_bt(i,k,j,h)
      P_v_bt(i,k,j,h)=FACTOR*S_v_bt(i,k,j,h)
      P_ph_bt(i,k,j,h)=FACTOR*S_ph_bt(i,k,j,h)
      P_w_bt(i,k,j,h)=FACTOR*S_w_bt(i,k,j,h)
      P_t_bt(i,k,j,h)=FACTOR*S_t_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      P_mu_b(i,k,j,h)=FACTOR*S_mu_b(i,k,j,h)
      P_mu_bt(i,k,j,h)=FACTOR*S_mu_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
   enddo
   enddo

   CALL relax_bdy_dry ( config_flags,                                    &
                              P_ru_tendf, P_rv_tendf, P_ph_tendf, P_t_tendf,           &
                              P_rw_tendf, P_mu_tend,                               &
                              P_ru, P_rv, P_ph, P_t,                                   &
                              P_w, P_mu, P_mut,                                      &
                              P_u_b, P_v_b, P_ph_b, P_t_b,                             &
                              P_w_b, P_mu_b,                                       &
                              P_u_bt, P_v_bt, P_ph_bt, P_t_bt,                         &
                              P_w_bt, P_mu_bt,                                     &
                              spec_bdy_width, spec_zone, relax_zone,           &
                              dtbc, fcx, gcx,             &
                              ijds, ijde,                 & ! min/max(id,jd)
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its, ite, jts, jte, kts, kte)

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))  &
                     + (P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))  &
                     + (P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))  &
                     + (P_t_tendf(i,k,j)-B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))      &
                     + (P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))*(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
         VAL_N=VAL_N + (P_mu_tend(i,j)-B_mu_tend(i,j))*(P_mu_tend(i,j)-B_mu_tend(i,j))
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_relax_bdy_dry: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)

      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)

      B_ru(i,k,j)=P_ru(i,k,j)
      B_rv(i,k,j)=P_rv(i,k,j)
      B_w(i,k,j)=P_w(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      mut(i,j)=S_mut(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)

      B_mu(i,j)=P_mu(i,j)
      B_mut(i,j)=P_mut(i,j)
   enddo
   enddo

   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      u_b(i,k,j,h)=S_u_b(i,k,j,h)
      v_b(i,k,j,h)=S_v_b(i,k,j,h)
      ph_b(i,k,j,h)=S_ph_b(i,k,j,h)
      w_b(i,k,j,h)=S_w_b(i,k,j,h)
      t_b(i,k,j,h)=S_t_b(i,k,j,h)
      u_bt(i,k,j,h)=S_u_bt(i,k,j,h)
      v_bt(i,k,j,h)=S_v_bt(i,k,j,h)
      ph_bt(i,k,j,h)=S_ph_bt(i,k,j,h)
      w_bt(i,k,j,h)=S_w_bt(i,k,j,h)
      t_bt(i,k,j,h)=S_t_bt(i,k,j,h)

      P_u_b(i,k,j,h)=FACTOR*S_u_b(i,k,j,h)
      P_v_b(i,k,j,h)=FACTOR*S_v_b(i,k,j,h)
      P_ph_b(i,k,j,h)=FACTOR*S_ph_b(i,k,j,h)
      P_w_b(i,k,j,h)=FACTOR*S_w_b(i,k,j,h)
      P_t_b(i,k,j,h)=FACTOR*S_t_b(i,k,j,h)
      P_u_bt(i,k,j,h)=FACTOR*S_u_bt(i,k,j,h)
      P_v_bt(i,k,j,h)=FACTOR*S_v_bt(i,k,j,h)
      P_ph_bt(i,k,j,h)=FACTOR*S_ph_bt(i,k,j,h)
      P_w_bt(i,k,j,h)=FACTOR*S_w_bt(i,k,j,h)
      P_t_bt(i,k,j,h)=FACTOR*S_t_bt(i,k,j,h)

      B_u_b(i,k,j,h)=P_u_b(i,k,j,h)
      B_v_b(i,k,j,h)=P_v_b(i,k,j,h)
      B_ph_b(i,k,j,h)=P_ph_b(i,k,j,h)
      B_w_b(i,k,j,h)=P_w_b(i,k,j,h)
      B_t_b(i,k,j,h)=P_t_b(i,k,j,h)
      B_u_bt(i,k,j,h)=P_u_bt(i,k,j,h)
      B_v_bt(i,k,j,h)=P_v_bt(i,k,j,h)
      B_ph_bt(i,k,j,h)=P_ph_bt(i,k,j,h)
      B_w_bt(i,k,j,h)=P_w_bt(i,k,j,h)
      B_t_bt(i,k,j,h)=P_t_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      mu_b(i,k,j,h)=S_mu_b(i,k,j,h)
      mu_bt(i,k,j,h)=S_mu_bt(i,k,j,h)

      P_mu_b(i,k,j,h)=FACTOR*S_mu_b(i,k,j,h)
      P_mu_bt(i,k,j,h)=FACTOR*S_mu_bt(i,k,j,h)

      B_mu_b(i,k,j,h)=P_mu_b(i,k,j,h)
      B_mu_bt(i,k,j,h)=P_mu_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      ph_tendf(i,k,j)=S_ph_tendf(i,k,j)

      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)

      B_ru_tendf(i,k,j)=P_ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=P_rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=P_rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=P_t_tendf(i,k,j)
      B_ph_tendf(i,k,j)=P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
      B_mu_tend(i,j)=P_mu_tend(i,j)
   enddo
   enddo

!  TGL

   CALL g_relax_bdy_dry( config_flags, ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, ph_tendf, P_ph_tendf, t_tendf, P_t_tendf, &
&rw_tendf, P_rw_tendf, mu_tend, P_mu_tend, ru, P_ru, rv, P_rv, ph, P_ph, t, P_t, w, P_w, mu, P_mu, mut, P_mut, u_b, P_u_b, v_b, &
&P_v_b, ph_b, P_ph_b, t_b, P_t_b, w_b, P_w_b, mu_b, P_mu_b, u_bt, P_u_bt, v_bt, P_v_bt, ph_bt, P_ph_bt, t_bt, P_t_bt, w_bt, P_w_bt,&
& mu_bt, P_mu_bt, spec_bdy_width, spec_zone, relax_zone, dtbc, fcx, gcx, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)     &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L + P_mu_tend(i,j)*P_mu_tend(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=0.0
      P_rv(i,k,j)=0.0
      P_w(i,k,j)=0.0
      P_t(i,k,j)=0.0
      P_ph(i,k,j)=0.0
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
      P_mut(i,j)=0.0
   enddo
   enddo
   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      P_u_b(i,k,j,h)=0.0
      P_v_b(i,k,j,h)=0.0
      P_ph_b(i,k,j,h)=0.0
      P_w_b(i,k,j,h)=0.0
      P_t_b(i,k,j,h)=0.0
      P_u_bt(i,k,j,h)=0.0
      P_v_bt(i,k,j,h)=0.0
      P_ph_bt(i,k,j,h)=0.0
      P_w_bt(i,k,j,h)=0.0
      P_t_bt(i,k,j,h)=0.0
   enddo
   enddo
   enddo
   enddo
   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      P_mu_b(i,k,j,h)=0.0
      P_mu_bt(i,k,j,h)=0.0
   enddo
   enddo
   enddo
   enddo

!  ADJ

   CALL a_relax_bdy_dry( config_flags, P_ru_tendf, P_rv_tendf, P_ph_tendf, P_t_tendf, P_rw_tendf, P_mu_tend, P_ru, P_rv, ph, &
&P_ph, t, P_t, w, P_w, P_mu, mut, P_mut, P_u_b, P_v_b, P_ph_b, P_t_b, P_w_b, P_mu_b, P_u_bt, P_v_bt, P_ph_bt, P_t_bt, P_w_bt, &
&P_mu_bt, spec_bdy_width, spec_zone, relax_zone, dtbc, fcx, gcx, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms,&
& kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ru(i,k,j)*B_ru(i,k,j)    &
              + P_rv(i,k,j)*B_rv(i,k,j)        &
              + P_w(i,k,j)*B_w(i,k,j)          &
              + P_t(i,k,j)*B_t(i,k,j)          &
              + P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)    &
              + P_mut(i,j)*B_mut(i,j)
   enddo
   enddo
   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      VAL_A=VAL_A + P_u_b(i,k,j,h)*B_u_b(i,k,j,h)       &
              + P_v_b(i,k,j,h)*B_v_b(i,k,j,h)       &
              + P_ph_b(i,k,j,h)*B_ph_b(i,k,j,h)       &
              + P_w_b(i,k,j,h)*B_w_b(i,k,j,h)       &
              + P_t_b(i,k,j,h)*B_t_b(i,k,j,h)       &
              + P_u_bt(i,k,j,h)*B_u_bt(i,k,j,h)       &
              + P_v_bt(i,k,j,h)*B_v_bt(i,k,j,h)       &
              + P_ph_bt(i,k,j,h)*B_ph_bt(i,k,j,h)       &
              + P_w_bt(i,k,j,h)*B_w_bt(i,k,j,h)       &
              + P_t_bt(i,k,j,h)*B_t_bt(i,k,j,h)       
   enddo
   enddo
   enddo
   enddo

   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      VAL_A=VAL_A + P_mu_b(i,k,j,h)*B_mu_b(i,k,j,h)    &
              + P_mu_bt(i,k,j,h)*B_mu_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)    &
              + P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)    &
              + P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)    &
              + P_t_tendf(i,k,j)*B_t_tendf(i,k,j)    &
              + P_ph_tendf(i,k,j)*B_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_relax_bdy_dry: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      mut(i,j)=S_mut(i,j)
   enddo
   enddo
   do i=ijds,ijde
   do k=kds,kde
   do j=1,spec_bdy_width
   do h=1,4
      u_b(i,k,j,h)=S_u_b(i,k,j,h)
      v_b(i,k,j,h)=S_v_b(i,k,j,h)
      ph_b(i,k,j,h)=S_ph_b(i,k,j,h)
      w_b(i,k,j,h)=S_w_b(i,k,j,h)
      t_b(i,k,j,h)=S_t_b(i,k,j,h)
      u_bt(i,k,j,h)=S_u_bt(i,k,j,h)
      v_bt(i,k,j,h)=S_v_bt(i,k,j,h)
      ph_bt(i,k,j,h)=S_ph_bt(i,k,j,h)
      w_bt(i,k,j,h)=S_w_bt(i,k,j,h)
      t_bt(i,k,j,h)=S_t_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ijds,ijde
   do k=1,1
   do j=1,spec_bdy_width
   do h=1,4
      mu_b(i,k,j,h)=S_mu_b(i,k,j,h)
      mu_bt(i,k,j,h)=S_mu_bt(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      ph_tendf(i,k,j)=S_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
   enddo
   enddo

! method: using goto clause before if

!g_relax_bdy_dry: ALPHA=.1000E+00  COEF=   0.1000000953674E+01  VAL_N= 0.577844E-20  VAL_L= 0.577844E-20
!g_relax_bdy_dry: ALPHA=.1000E-01  COEF=   0.1000003337860E+01  VAL_N= 0.577846E-22  VAL_L= 0.577844E-22
!g_relax_bdy_dry: ALPHA=.1000E-02  COEF=   0.1000092029572E+01  VAL_N= 0.577897E-24  VAL_L= 0.577844E-24
!g_relax_bdy_dry: ALPHA=.1000E-03  COEF=   0.9997125864029E+00  VAL_N= 0.577678E-26  VAL_L= 0.577844E-26
!g_relax_bdy_dry: ALPHA=.1000E-04  COEF=   0.9984483122826E+00  VAL_N= 0.576947E-28  VAL_L= 0.577844E-28
!g_relax_bdy_dry: ALPHA=.1000E-05  COEF=   0.8998992443085E+00  VAL_N= 0.520001E-30  VAL_L= 0.577844E-30
!g_relax_bdy_dry: ALPHA=.1000E-06  COEF=   0.1599820733070E+01  VAL_N= 0.924446E-32  VAL_L= 0.577844E-32
!g_relax_bdy_dry: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.577844E-34
!g_relax_bdy_dry: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.577844E-36
!g_relax_bdy_dry: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.577844E-38
!g_relax_bdy_dry: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.577839E-40

!a_relax_bdy_dry:    0.5778436170567E-20   0.5778436170567E-20

END SUBROUTINE t_relax_bdy_dry

!---------------------------------------------------------------------------------------------------

SUBROUTINE t_rk_addtend_dry ( ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            u_save, v_save, w_save, ph_save, t_save, rk_step,&
                            h_diabatic, mut, msft, msfu, msfv,               &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            its,ite, jts,jte, kts,kte                       )

! Zaizhong Ma, March 25,2005

   IMPLICIT NONE

   !  Input data.

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: ru_tend, &
                                                                      rv_tend, &
                                                                      rw_tend, &
                                                                      ph_tend, &
                                                                      t_tend,  &
                                                                      ru_tendf, &
                                                                      rv_tendf, &
                                                                      rw_tendf, &
                                                                      ph_tendf, &
                                                                      t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 ::  u_save,  &
                                                                       v_save,  &
                                                                       w_save,  &
                                                                      ph_save,  &
                                                                       t_save
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )     :: h_diabatic

   REAL , DIMENSION( ims:ime , jms:jme )              :: mut
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msft, &
                                                                    msfu, &
                                                                    msfv


! Local
   INTEGER :: i, j, k

!  zzma: new definition

!  IN variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 ::  S_u_save,  &
                                                                       S_v_save,  &
                                                                       S_w_save,  &
                                                                       S_ph_save,  &
                                                                       S_t_save
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 ::  P_u_save,  &
                                                                       P_v_save,  &
                                                                       P_w_save,  &
                                                                       P_ph_save,  &
                                                                       P_t_save
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 ::  B_u_save,  &
                                                                       B_v_save,  &
                                                                       B_w_save,  &
                                                                       B_ph_save,  &
                                                                       B_t_save

   REAL , DIMENSION( ims:ime , jms:jme )                         :: S_mut,P_mut,B_mut

!  INOUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: S_ru_tend, &
                                                                      S_rv_tend, &
                                                                      S_rw_tend, &
                                                                      S_ph_tend, &
                                                                      S_t_tend,  &
                                                                      S_ru_tendf, &
                                                                      S_rv_tendf, &
                                                                      S_rw_tendf, &
                                                                      S_ph_tendf, &
                                                                      S_t_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: P_ru_tend, &
                                                                      P_rv_tend, &
                                                                      P_rw_tend, &
                                                                      P_ph_tend, &
                                                                      P_t_tend,  &
                                                                      P_ru_tendf, &
                                                                      P_rv_tendf, &
                                                                      P_rw_tendf, &
                                                                      P_ph_tendf, &
                                                                      P_t_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: B_ru_tend, &
                                                                      B_rv_tend, &
                                                                      B_rw_tend, &
                                                                      B_ph_tend, &
                                                                      B_t_tend,  &
                                                                      B_ru_tendf, &
                                                                      B_rv_tendf, &
                                                                      B_rw_tendf, &
                                                                      B_ph_tendf, &
                                                                      B_t_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: K_ru_tend, &
                                                                      K_rv_tend, &
                                                                      K_rw_tend, &
                                                                      K_ph_tend, &
                                                                      K_t_tend,  &
                                                                      K_ru_tendf, &
                                                                      K_rv_tendf, &
                                                                      K_rw_tendf, &
                                                                      K_ph_tendf, &
                                                                      K_t_tendf


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u_save(i,k,j)=u_save(i,k,j)
      S_v_save(i,k,j)=v_save(i,k,j)
      S_w_save(i,k,j)=w_save(i,k,j)
      S_t_save(i,k,j)=t_save(i,k,j)
      S_ph_save(i,k,j)=ph_save(i,k,j)

      P_u_save(i,k,j)=u_save(i,k,j)
      P_v_save(i,k,j)=v_save(i,k,j)
      P_w_save(i,k,j)=w_save(i,k,j)
      P_t_save(i,k,j)=t_save(i,k,j)
      P_ph_save(i,k,j)=ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mut(i,j)=mut(i,j)
      P_mut(i,j)=mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru_tend(i,k,j)=ru_tend(i,k,j)
      S_rv_tend(i,k,j)=rv_tend(i,k,j)
      S_rw_tend(i,k,j)=rw_tend(i,k,j)
      S_t_tend(i,k,j)=t_tend(i,k,j)
      S_ph_tend(i,k,j)=ph_tend(i,k,j)
      S_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      S_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      S_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      S_t_tendf(i,k,j)=t_tendf(i,k,j)
      S_ph_tendf(i,k,j)=ph_tendf(i,k,j)

      P_ru_tend(i,k,j)=ru_tend(i,k,j)
      P_rv_tend(i,k,j)=rv_tend(i,k,j)
      P_rw_tend(i,k,j)=rw_tend(i,k,j)
      P_t_tend(i,k,j)=t_tend(i,k,j)
      P_ph_tend(i,k,j)=ph_tend(i,k,j)
      P_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=ph_tendf(i,k,j)

      K_ru_tend(i,k,j)=ru_tend(i,k,j)
      K_rv_tend(i,k,j)=rv_tend(i,k,j)
      K_rw_tend(i,k,j)=rw_tend(i,k,j)
      K_t_tend(i,k,j)=t_tend(i,k,j)
      K_ph_tend(i,k,j)=ph_tend(i,k,j)
      K_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      K_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      K_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      K_t_tendf(i,k,j)=t_tendf(i,k,j)
      K_ph_tendf(i,k,j)=ph_tendf(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL rk_addtend_dry    ( ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            u_save, v_save, w_save, ph_save, t_save, rk_step,&
                            h_diabatic, mut, msft, msfu, msfv,               &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            its,ite, jts,jte, kts,kte                       )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_tend(i,k,j)=ru_tend(i,k,j)
      B_rv_tend(i,k,j)=rv_tend(i,k,j)
      B_rw_tend(i,k,j)=rw_tend(i,k,j)
      B_t_tend(i,k,j)=t_tend(i,k,j)
      B_ph_tend(i,k,j)=ph_tend(i,k,j)
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
      B_ph_tendf(i,k,j)=ph_tendf(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_rk_addtend_dry( K_ru_tend, P_ru_tend, K_rv_tend, P_rv_tend, &
K_rw_tend, P_rw_tend, K_ph_tend, P_ph_tend, K_t_tend, P_t_tend, &
&K_ru_tendf, P_ru_tendf, K_rv_tendf, P_rv_tendf, K_rw_tendf, P_rw_tendf, &
K_ph_tendf, P_ph_tendf, K_t_tendf, P_t_tendf, u_save, P_u_save, &
&v_save, P_v_save, w_save, P_w_save, ph_save, P_ph_save, t_save, &
P_t_save, rk_step, h_diabatic, mut, P_mut, msft, msfu, msfv, ide, &
&jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ru_tend(i,k,j)*P_ru_tend(i,k,j)  &
                    + P_rv_tend(i,k,j)*P_rv_tend(i,k,j)  &
                    + P_rw_tend(i,k,j)*P_rw_tend(i,k,j)  &
                    + P_t_tend(i,k,j)*P_t_tend(i,k,j)    &
                    + P_ph_tend(i,k,j)*P_ph_tend(i,k,j)  &
                    + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)  &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)  &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)  &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)    &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_save(i,k,j)=FACTOR*S_u_save(i,k,j)
      P_v_save(i,k,j)=FACTOR*S_v_save(i,k,j)
      P_w_save(i,k,j)=FACTOR*S_w_save(i,k,j)
      P_t_save(i,k,j)=FACTOR*S_t_save(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mut(i,j)=FACTOR*S_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_tend(i,k,j)=FACTOR*S_ru_tend(i,k,j)
      P_rv_tend(i,k,j)=FACTOR*S_rv_tend(i,k,j)
      P_rw_tend(i,k,j)=FACTOR*S_rw_tend(i,k,j)
      P_t_tend(i,k,j)=FACTOR*S_t_tend(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)
      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   CALL rk_addtend_dry    ( P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend,      &
                            P_ru_tendf, P_rv_tendf, P_rw_tendf, P_ph_tendf, P_t_tendf, &
                            P_u_save, P_v_save, P_w_save, P_ph_save, P_t_save, rk_step,&
                            h_diabatic, P_mut, msft, msfu, msfv,               &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            its,ite, jts,jte, kts,kte                       )
      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_ru_tend(i,k,j)- B_ru_tend(i,k,j))*(P_ru_tend(i,k,j)- B_ru_tend(i,k,j))  &
                    +(P_rv_tend(i,k,j)- B_rv_tend(i,k,j))*(P_rv_tend(i,k,j)- B_rv_tend(i,k,j))   &
                    +(P_rw_tend(i,k,j)- B_rw_tend(i,k,j))*(P_rw_tend(i,k,j)- B_rw_tend(i,k,j))   &
                    +(P_t_tend(i,k,j)- B_t_tend(i,k,j))*(P_t_tend(i,k,j)- B_t_tend(i,k,j))       &
                    +(P_ph_tend(i,k,j)- B_ph_tend(i,k,j))*(P_ph_tend(i,k,j)- B_ph_tend(i,k,j))   &
                    +(P_ru_tendf(i,k,j)- B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)- B_ru_tendf(i,k,j))  &
                    +(P_rv_tendf(i,k,j)- B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)- B_rv_tendf(i,k,j))   &
                    +(P_rw_tendf(i,k,j)- B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)- B_rw_tendf(i,k,j))   &
                    +(P_t_tendf(i,k,j)- B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)- B_t_tendf(i,k,j))       &
                    +(P_ph_tendf(i,k,j)- B_ph_tendf(i,k,j))*(P_ph_tendf(i,k,j)- B_ph_tendf(i,k,j))
      enddo
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_rk_addtend_dry: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_save(i,k,j)=S_u_save(i,k,j)
      v_save(i,k,j)=S_v_save(i,k,j)
      w_save(i,k,j)=S_w_save(i,k,j)
      t_save(i,k,j)=S_t_save(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)

      P_u_save(i,k,j)=FACTOR*S_u_save(i,k,j)
      P_v_save(i,k,j)=FACTOR*S_v_save(i,k,j)
      P_w_save(i,k,j)=FACTOR*S_w_save(i,k,j)
      P_t_save(i,k,j)=FACTOR*S_t_save(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)

      B_u_save(i,k,j)=P_u_save(i,k,j)
      B_v_save(i,k,j)=P_v_save(i,k,j)
      B_w_save(i,k,j)=P_w_save(i,k,j)
      B_t_save(i,k,j)=P_t_save(i,k,j)
      B_ph_save(i,k,j)=P_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mut(i,j)=S_mut(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      B_mut(i,j)=P_mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tend(i,k,j)=S_ru_tend(i,k,j)
      rv_tend(i,k,j)=S_rv_tend(i,k,j)
      rw_tend(i,k,j)=S_rw_tend(i,k,j)
      t_tend(i,k,j)=S_t_tend(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)
      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      ph_tendf(i,k,j)=S_ph_tendf(i,k,j)

      P_ru_tend(i,k,j)=FACTOR*S_ru_tend(i,k,j)
      P_rv_tend(i,k,j)=FACTOR*S_rv_tend(i,k,j)
      P_rw_tend(i,k,j)=FACTOR*S_rw_tend(i,k,j)
      P_t_tend(i,k,j)=FACTOR*S_t_tend(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)
      P_ru_tendf(i,k,j)=FACTOR*S_ru_tendf(i,k,j)
      P_rv_tendf(i,k,j)=FACTOR*S_rv_tendf(i,k,j)
      P_rw_tendf(i,k,j)=FACTOR*S_rw_tendf(i,k,j)
      P_t_tendf(i,k,j)=FACTOR*S_t_tendf(i,k,j)
      P_ph_tendf(i,k,j)=FACTOR*S_ph_tendf(i,k,j)

      B_ru_tend(i,k,j)=P_ru_tend(i,k,j)
      B_rv_tend(i,k,j)=P_rv_tend(i,k,j)
      B_rw_tend(i,k,j)=P_rw_tend(i,k,j)
      B_t_tend(i,k,j)=P_t_tend(i,k,j)
      B_ph_tend(i,k,j)=P_ph_tend(i,k,j)
      B_ru_tendf(i,k,j)=P_ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=P_rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=P_rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=P_t_tendf(i,k,j)
      B_ph_tendf(i,k,j)=P_ph_tendf(i,k,j)

   enddo
   enddo
   enddo

!  TGL

   CALL g_rk_addtend_dry( ru_tend, P_ru_tend, rv_tend, P_rv_tend, rw_tend, P_rw_tend, ph_tend, P_ph_tend, t_tend, P_t_tend, &
&ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, rw_tendf, P_rw_tendf, ph_tendf, P_ph_tendf, t_tendf, P_t_tendf, u_save, P_u_save, &
&v_save, P_v_save, w_save, P_w_save, ph_save, P_ph_save, t_save, P_t_save, rk_step, h_diabatic, mut, P_mut, msft, msfu, msfv, ide, &
&jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_ru_tend(i,k,j)*P_ru_tend(i,k,j)  &
                    + P_rv_tend(i,k,j)*P_rv_tend(i,k,j)  &
                    + P_rw_tend(i,k,j)*P_rw_tend(i,k,j)  &
                    + P_t_tend(i,k,j)*P_t_tend(i,k,j)    &
                    + P_ph_tend(i,k,j)*P_ph_tend(i,k,j)  &
                    + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)  &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)  &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)  &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)    &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_save(i,k,j)=0.0
      P_v_save(i,k,j)=0.0
      P_w_save(i,k,j)=0.0
      P_t_save(i,k,j)=0.0
      P_ph_save(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mut(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_rk_addtend_dry( P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend, P_ru_tendf, P_rv_tendf, P_rw_tendf, P_ph_tendf, &
&P_t_tendf, P_u_save, P_v_save, P_w_save, P_ph_save, P_t_save, rk_step, h_diabatic, P_mut, msft, msfu, msfv, ide, jde, ims, ime, &
&jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_u_save(i,k,j)*B_u_save(i,k,j)      &
               + P_v_save(i,k,j)*B_v_save(i,k,j)         &
               + P_w_save(i,k,j)*B_w_save(i,k,j)         &
               + P_t_save(i,k,j)*B_t_save(i,k,j)         &
               + P_ph_save(i,k,j)*B_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mut(i,j)*B_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ru_tend(i,k,j)*B_ru_tend(i,k,j)      &
               + P_rv_tend(i,k,j)*B_rv_tend(i,k,j)         &
               + P_rw_tend(i,k,j)*B_rw_tend(i,k,j)         &
               + P_t_tend(i,k,j)*B_t_tend(i,k,j)           &
               + P_ph_tend(i,k,j)*B_ph_tend(i,k,j)         &
               + P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)       &
               + P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)       &
               + P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)       &
               + P_t_tendf(i,k,j)*B_t_tendf(i,k,j)         &
               + P_ph_tendf(i,k,j)*B_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_rk_addtend_dry: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_save(i,k,j)=S_u_save(i,k,j)
      v_save(i,k,j)=S_v_save(i,k,j)
      w_save(i,k,j)=S_w_save(i,k,j)
      t_save(i,k,j)=S_t_save(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mut(i,j)=S_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tend(i,k,j)=S_ru_tend(i,k,j)
      rv_tend(i,k,j)=S_rv_tend(i,k,j)
      rw_tend(i,k,j)=S_rw_tend(i,k,j)
      t_tend(i,k,j)=S_t_tend(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)
      ru_tendf(i,k,j)=S_ru_tendf(i,k,j)
      rv_tendf(i,k,j)=S_rv_tendf(i,k,j)
      rw_tendf(i,k,j)=S_rw_tendf(i,k,j)
      t_tendf(i,k,j)=S_t_tendf(i,k,j)
      ph_tendf(i,k,j)=S_ph_tendf(i,k,j)
   enddo
   enddo
   enddo

!g_rk_addtend_dry: ALPHA=.1000E+00  COEF=   0.9999989867210E+00  VAL_N= 0.198619E+11  VAL_L= 0.198619E+11
!g_rk_addtend_dry: ALPHA=.1000E-01  COEF=   0.1000009894371E+01  VAL_N= 0.198621E+09  VAL_L= 0.198619E+09
!g_rk_addtend_dry: ALPHA=.1000E-02  COEF=   0.1000102877617E+01  VAL_N= 0.198640E+07  VAL_L= 0.198619E+07
!g_rk_addtend_dry: ALPHA=.1000E-03  COEF=   0.1000385403633E+01  VAL_N= 0.198696E+05  VAL_L= 0.198619E+05
!g_rk_addtend_dry: ALPHA=.1000E-04  COEF=   0.1002598404884E+01  VAL_N= 0.199135E+03  VAL_L= 0.198619E+03
!g_rk_addtend_dry: ALPHA=.1000E-05  COEF=   0.9058508872986E+00  VAL_N= 0.179919E+01  VAL_L= 0.198619E+01
!g_rk_addtend_dry: ALPHA=.1000E-06  COEF=   0.1524479985237E+01  VAL_N= 0.302791E-01  VAL_L= 0.198619E-01
!g_rk_addtend_dry: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.198619E-03
!g_rk_addtend_dry: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.198619E-05
!g_rk_addtend_dry: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.198619E-07
!g_rk_addtend_dry: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.198619E-09
                 
!a_rk_addtend_dry:    0.1986187673600E+11   0.1986187673600E+11

END SUBROUTINE t_rk_addtend_dry

!---------------------------------------------------------------------------------------------------

SUBROUTINE t_spec_bdy_dry ( config_flags,                        &
                             ru_tend, rv_tend, ph_tend, t_tend,   &
                             rw_tend, mu_tend,                    &
                             u_b, v_b, ph_b, t_b,                 &
                             w_b, mu_b,                           &
                             u_bt, v_bt, ph_bt, t_bt,             &
                             w_bt, mu_bt,                         &
                             spec_bdy_width, spec_zone,           &
                             ijds, ijde,                 & ! min/max(id,jd)
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             its, ite, jts, jte, kts, kte)

! Zaizhong Ma, March 25,2005
! Qingnong Xiao, April 2005, rewritten.

   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: ru_tend, &
                                                                      rv_tend, &
                                                                      ph_tend, &
                                                                      rw_tend, &
                                                                      t_tend
   REAL , DIMENSION( ims:ime , jms:jme  )              :: mu_tend
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: u_b,  &
                                                                                 v_b,  &
                                                                                 ph_b, &
                                                                                  w_b, &
                                                                                 t_b
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: u_bt, &
                                                                                 v_bt, &
                                                                                ph_bt, &
                                                                                 w_bt, &
                                                                                 t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1 ,     spec_bdy_width, 4 ), INTENT(IN   ) :: mu_b
   REAL,  DIMENSION( ijds:ijde , 1:1 ,     spec_bdy_width, 4 )                :: mu_bt

!  zzma: new definition

!  IN variables

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: S_u_bt, &
                                                                                 S_v_bt, &
                                                                                 S_ph_bt, &
                                                                                 S_w_bt, &
                                                                                 S_t_bt
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: P_u_bt, &
                                                                                 P_v_bt, &
                                                                                 P_ph_bt, &
                                                                                 P_w_bt, &
                                                                                 P_t_bt
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )                :: B_u_bt, &
                                                                                 B_v_bt, &
                                                                                 B_ph_bt, &
                                                                                 B_w_bt, &
                                                                                 B_t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1 ,     spec_bdy_width, 4 )     :: S_mu_bt,P_mu_bt, B_mu_bt 

!  OUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: S_ru_tend, &
                                                                      S_rv_tend, &
                                                                      S_ph_tend, &
                                                                      S_rw_tend, &
                                                                      S_t_tend

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: P_ru_tend, &
                                                                      P_rv_tend, &
                                                                      P_ph_tend, &
                                                                      P_rw_tend, &
                                                                      P_t_tend

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )                 :: B_ru_tend, &
                                                                      B_rv_tend, &
                                                                      B_ph_tend, &
                                                                      B_rw_tend, &
                                                                      B_t_tend

   REAL , DIMENSION( ims:ime , jms:jme  )  :: S_mu_tend,P_mu_tend,B_mu_tend

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,i,j,k,h

!  zzma: new definition end

   S_u_bt(:,:,:,:)=u_bt(:,:,:,:)
   S_v_bt(:,:,:,:)=v_bt(:,:,:,:)
   S_ph_bt(:,:,:,:)=ph_bt(:,:,:,:)
   S_w_bt(:,:,:,:)=w_bt(:,:,:,:)
   S_t_bt(:,:,:,:)=t_bt(:,:,:,:)

   P_u_bt(:,:,:,:)=u_bt(:,:,:,:)
   P_v_bt(:,:,:,:)=v_bt(:,:,:,:)
   P_ph_bt(:,:,:,:)=ph_bt(:,:,:,:)
   P_w_bt(:,:,:,:)=w_bt(:,:,:,:)
   P_t_bt(:,:,:,:)=t_bt(:,:,:,:)

   S_mu_bt(:,:,:,:)=mu_bt(:,:,:,:)
   P_mu_bt(:,:,:,:)=mu_bt(:,:,:,:)

   S_ru_tend(:,:,:)=ru_tend(:,:,:)
   S_rv_tend(:,:,:)=rv_tend(:,:,:)
   S_rw_tend(:,:,:)=rw_tend(:,:,:)
   S_t_tend(:,:,:)=t_tend(:,:,:)
   S_ph_tend(:,:,:)=ph_tend(:,:,:)

   P_ru_tend(:,:,:)=ru_tend(:,:,:)
   P_rv_tend(:,:,:)=rv_tend(:,:,:)
   P_rw_tend(:,:,:)=rw_tend(:,:,:)
   P_t_tend(:,:,:)=t_tend(:,:,:)
   P_ph_tend(:,:,:)=ph_tend(:,:,:)

   S_mu_tend(:,:)=mu_tend(:,:)
   P_mu_tend(:,:)=mu_tend(:,:)

!NLM

   CALL spec_bdy_dry ( config_flags,                        &
                             ru_tend, rv_tend, ph_tend, t_tend,   &
                             rw_tend, mu_tend,                    &
                             u_b, v_b, ph_b, t_b,                 &
                             w_b, mu_b,                           &
                             u_bt, v_bt, ph_bt, t_bt,             &
                             w_bt, mu_bt,                         &
                             spec_bdy_width, spec_zone,           &
                             ijds, ijde,                 & ! min/max(id,jd)
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             its, ite, jts, jte, kts, kte)

   B_u_bt(:,:,:,:)=u_bt(:,:,:,:)
   B_v_bt(:,:,:,:)=v_bt(:,:,:,:)
   B_ph_bt(:,:,:,:)=ph_bt(:,:,:,:)
   B_w_bt(:,:,:,:)=w_bt(:,:,:,:)
   B_t_bt(:,:,:,:)=t_bt(:,:,:,:)
   B_mu_bt(:,:,:,:)=mu_bt(:,:,:,:)

   B_ru_tend(:,:,:)=ru_tend(:,:,:)
   B_rv_tend(:,:,:)=rv_tend(:,:,:)
   B_rw_tend(:,:,:)=rw_tend(:,:,:)
   B_t_tend(:,:,:)=t_tend(:,:,:)
   B_ph_tend(:,:,:)=ph_tend(:,:,:)

   B_mu_tend(:,:)=mu_tend(:,:)

!  TCL

   u_bt(:,:,:,:)=S_u_bt(:,:,:,:)
   v_bt(:,:,:,:)=S_v_bt(:,:,:,:)
   ph_bt(:,:,:,:)=S_ph_bt(:,:,:,:)
   w_bt(:,:,:,:)=S_w_bt(:,:,:,:)
   t_bt(:,:,:,:)=S_t_bt(:,:,:,:)
   mu_bt(:,:,:,:)=S_mu_bt(:,:,:,:)

   rv_tend(:,:,:)=S_rv_tend(:,:,:)
   rw_tend(:,:,:)=S_rw_tend(:,:,:)
   t_tend(:,:,:)=S_t_tend(:,:,:)
   ph_tend(:,:,:)=S_ph_tend(:,:,:)
   mu_tend(:,:)=S_mu_tend(:,:)

   CALL g_spec_bdy_dry( config_flags, ru_tend, P_ru_tend, rv_tend, P_rv_tend, ph_tend, P_ph_tend, t_tend, P_t_tend, rw_tend, &
&P_rw_tend, mu_tend, P_mu_tend, u_bt, P_u_bt, v_bt, P_v_bt, ph_bt, P_ph_bt, t_bt, P_t_bt, w_bt, P_w_bt, mu_bt, P_mu_bt, &
&spec_bdy_width, spec_zone, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L = sum(P_u_bt(:,:,:,:)*P_u_bt(:,:,:,:))+  &
            sum(P_v_bt(:,:,:,:)*P_v_bt(:,:,:,:))+  &
            sum(P_ph_bt(:,:,:,:)*P_ph_bt(:,:,:,:))+ &
            sum(P_w_bt(:,:,:,:)*P_w_bt(:,:,:,:))+ &
            sum(P_t_bt(:,:,:,:)*P_t_bt(:,:,:,:))+ &
            sum(P_mu_bt(:,:,:,:)*P_mu_bt(:,:,:,:))

   SAVE_L=SAVE_L + sum(P_ru_tend(:,:,:)*P_ru_tend(:,:,:))+   &
                   sum(P_rv_tend(:,:,:)*P_rv_tend(:,:,:))+   &
                   sum(P_rw_tend(:,:,:)*P_rw_tend(:,:,:))+   &
                   sum(P_t_tend(:,:,:)*P_t_tend(:,:,:))+     &
                   sum(P_ph_tend(:,:,:)*P_ph_tend(:,:,:))+   &
                   sum(P_mu_tend(:,:)*P_mu_tend(:,:))

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      P_u_bt(:,:,:,:)=FACTOR*S_u_bt(:,:,:,:)
      P_v_bt(:,:,:,:)=FACTOR*S_v_bt(:,:,:,:)
      P_ph_bt(:,:,:,:)=FACTOR*S_ph_bt(:,:,:,:)
      P_w_bt(:,:,:,:)=FACTOR*S_w_bt(:,:,:,:)
      P_t_bt(:,:,:,:)=FACTOR*S_t_bt(:,:,:,:)

      P_mu_bt(:,:,:,:)=FACTOR*S_mu_bt(:,:,:,:)

      P_ru_tend(:,:,:)=FACTOR*S_ru_tend(:,:,:)
      P_rv_tend(:,:,:)=FACTOR*S_rv_tend(:,:,:)
      P_rw_tend(:,:,:)=FACTOR*S_rw_tend(:,:,:)
      P_t_tend(:,:,:)=FACTOR*S_t_tend(:,:,:)
      P_ph_tend(:,:,:)=FACTOR*S_ph_tend(:,:,:)

      P_mu_tend(:,:)=FACTOR*S_mu_tend(:,:)

      CALL spec_bdy_dry ( config_flags,                        &
                             P_ru_tend, P_rv_tend, P_ph_tend, P_t_tend,   &
                             P_rw_tend, P_mu_tend,                    &
                             u_b, v_b, ph_b, t_b,                 &
                             w_b, mu_b,                           &
                             P_u_bt, P_v_bt, P_ph_bt, P_t_bt,             &
                             P_w_bt, P_mu_bt,                         &
                             spec_bdy_width, spec_zone,           &
                             ijds, ijde,                 & ! min/max(id,jd)
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             its, ite, jts, jte, kts, kte)

      VAL_N=sum((P_u_bt(:,:,:,:)-B_u_bt(:,:,:,:))*(P_u_bt(:,:,:,:)-B_u_bt(:,:,:,:))) + &
            sum((P_v_bt(:,:,:,:)-B_v_bt(:,:,:,:))*(P_v_bt(:,:,:,:)-B_v_bt(:,:,:,:)))+ &
            sum((P_ph_bt(:,:,:,:)-B_ph_bt(:,:,:,:))*(P_ph_bt(:,:,:,:)-B_ph_bt(:,:,:,:)))+ &
            sum((P_w_bt(:,:,:,:)-B_w_bt(:,:,:,:))*(P_w_bt(:,:,:,:)-B_w_bt(:,:,:,:)))+   &
            sum((P_t_bt(:,:,:,:)-B_t_bt(:,:,:,:))*(P_t_bt(:,:,:,:)-B_t_bt(:,:,:,:)))+ &
            sum((P_mu_bt(:,:,:,:)-B_mu_bt(:,:,:,:))*(P_mu_bt(:,:,:,:)-B_mu_bt(:,:,:,:)))

      VAL_N=VAL_N + sum((P_ru_tend(:,:,:)-B_ru_tend(:,:,:))*(P_ru_tend(:,:,:)-B_ru_tend(:,:,:)))+  &
                    sum((P_rv_tend(:,:,:)-B_rv_tend(:,:,:))*(P_rv_tend(:,:,:)-B_rv_tend(:,:,:)))+  &
                    sum((P_rw_tend(:,:,:)-B_rw_tend(:,:,:))*(P_rw_tend(:,:,:)-B_rw_tend(:,:,:)))+  &
                    sum((P_t_tend(:,:,:)-B_t_tend(:,:,:))*(P_t_tend(:,:,:)-B_t_tend(:,:,:)))+      &
                    sum((P_ph_tend(:,:,:)-B_ph_tend(:,:,:))*(P_ph_tend(:,:,:)-B_ph_tend(:,:,:)))+  &
                    sum((P_mu_tend(:,:)-B_mu_tend(:,:))*(P_mu_tend(:,:)-B_mu_tend(:,:)))

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_spec_bdy_dry: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   u_bt(:,:,:,:)=S_u_bt(:,:,:,:)
   v_bt(:,:,:,:)=S_v_bt(:,:,:,:)
   ph_bt(:,:,:,:)=S_ph_bt(:,:,:,:)
   w_bt(:,:,:,:)=S_w_bt(:,:,:,:)
   t_bt(:,:,:,:)=S_t_bt(:,:,:,:)

   P_u_bt(:,:,:,:)=FACTOR*S_u_bt(:,:,:,:)
   P_v_bt(:,:,:,:)=FACTOR*S_v_bt(:,:,:,:)
   P_ph_bt(:,:,:,:)=FACTOR*S_ph_bt(:,:,:,:)
   P_w_bt(:,:,:,:)=FACTOR*S_w_bt(:,:,:,:)
   P_t_bt(:,:,:,:)=FACTOR*S_t_bt(:,:,:,:)

   B_u_bt(:,:,:,:)=P_u_bt(:,:,:,:)
   B_v_bt(:,:,:,:)=P_v_bt(:,:,:,:)
   B_ph_bt(:,:,:,:)=P_ph_bt(:,:,:,:)
   B_w_bt(:,:,:,:)=P_w_bt(:,:,:,:)
   B_t_bt(:,:,:,:)=P_t_bt(:,:,:,:)

   mu_bt(:,:,:,:)=S_mu_bt(:,:,:,:)
   P_mu_bt(:,:,:,:)=FACTOR*S_mu_bt(:,:,:,:)
   B_mu_bt(:,:,:,:)=P_mu_bt(:,:,:,:)

   ru_tend(:,:,:)=S_ru_tend(:,:,:)
   rv_tend(:,:,:)=S_rv_tend(:,:,:)
   rw_tend(:,:,:)=S_rw_tend(:,:,:)
   t_tend(:,:,:)=S_t_tend(:,:,:)
   ph_tend(:,:,:)=S_ph_tend(:,:,:)

   P_ru_tend(:,:,:)=FACTOR*S_ru_tend(:,:,:)
   P_rv_tend(:,:,:)=FACTOR*S_rv_tend(:,:,:)
   P_rw_tend(:,:,:)=FACTOR*S_rw_tend(:,:,:)
   P_t_tend(:,:,:)=FACTOR*S_t_tend(:,:,:)
   P_ph_tend(:,:,:)=FACTOR*S_ph_tend(:,:,:)

   B_ru_tend(:,:,:)=P_ru_tend(:,:,:)
   B_rv_tend(:,:,:)=P_rv_tend(:,:,:)
   B_rw_tend(:,:,:)=P_rw_tend(:,:,:)
   B_t_tend(:,:,:)=P_t_tend(:,:,:)
   B_ph_tend(:,:,:)=P_ph_tend(:,:,:)

   mu_tend(:,:)=S_mu_tend(:,:)
   P_mu_tend(:,:)=FACTOR*S_mu_tend(:,:)
   B_mu_tend(:,:)=P_mu_tend(:,:)

!  TGL

   CALL g_spec_bdy_dry( config_flags, ru_tend, P_ru_tend, rv_tend, P_rv_tend, ph_tend, P_ph_tend, t_tend, P_t_tend, rw_tend, &
&P_rw_tend, mu_tend, P_mu_tend, u_bt, P_u_bt, v_bt, P_v_bt, ph_bt, P_ph_bt, t_bt, P_t_bt, w_bt, P_w_bt, mu_bt, P_mu_bt, &
&spec_bdy_width, spec_zone, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=sum(P_ru_tend(:,:,:)*P_ru_tend(:,:,:))+   &
         sum(P_rv_tend(:,:,:)*P_rv_tend(:,:,:))+   &
         sum(P_rw_tend(:,:,:)*P_rw_tend(:,:,:))+   &
         sum(P_t_tend(:,:,:)*P_t_tend(:,:,:))+     &
         sum(P_ph_tend(:,:,:)*P_ph_tend(:,:,:))+   &
         sum(P_mu_tend(:,:)*P_mu_tend(:,:))
   VAL_L=VAL_L+sum(P_u_bt(:,:,:,:)*P_u_bt(:,:,:,:))+ &
               sum(P_v_bt(:,:,:,:)*P_v_bt(:,:,:,:))+ &
               sum(P_ph_bt(:,:,:,:)*P_ph_bt(:,:,:,:))+ &
               sum(P_w_bt(:,:,:,:)*P_w_bt(:,:,:,:))+ &
               sum(P_t_bt(:,:,:,:)*P_t_bt(:,:,:,:))+ &
               sum(P_mu_bt(:,:,:,:)*P_mu_bt(:,:,:,:))

!  ADJ

   CALL a_spec_bdy_dry( config_flags, P_ru_tend, P_rv_tend, P_ph_tend, P_t_tend, P_rw_tend, P_mu_tend, P_u_bt, P_v_bt, P_ph_bt, &
&P_t_bt, P_w_bt, P_mu_bt, spec_bdy_width, spec_zone, ijds, ijde, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, &
&ite, jts, jte, kts, kte )

   VAL_A=sum(P_u_bt(:,:,:,:)*B_u_bt(:,:,:,:))+       &
         sum(P_v_bt(:,:,:,:)*B_v_bt(:,:,:,:))+       &
         sum(P_ph_bt(:,:,:,:)*B_ph_bt(:,:,:,:))+       &
         sum(P_w_bt(:,:,:,:)*B_w_bt(:,:,:,:))+       &
         sum(P_t_bt(:,:,:,:)*B_t_bt(:,:,:,:))+       &
         sum(P_mu_bt(:,:,:,:)*B_mu_bt(:,:,:,:))
   VAL_A=VAL_A+sum(P_ru_tend(:,:,:)*B_ru_tend(:,:,:))+ &
               sum(P_rv_tend(:,:,:)*B_rv_tend(:,:,:))+ &
               sum(P_rw_tend(:,:,:)*B_rw_tend(:,:,:))+ &
               sum(P_t_tend(:,:,:)*B_t_tend(:,:,:))+   &
               sum(P_ph_tend(:,:,:)*B_ph_tend(:,:,:))+  &
               sum(P_mu_tend(:,:)*B_mu_tend(:,:))

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_spec_bdy_dry: ', VAL_L,VAL_A

!  RECOVER

   u_bt(:,:,:,:)=S_u_bt(:,:,:,:)
   v_bt(:,:,:,:)=S_v_bt(:,:,:,:)
   ph_bt(:,:,:,:)=S_ph_bt(:,:,:,:)
   w_bt(:,:,:,:)=S_w_bt(:,:,:,:)
   t_bt(:,:,:,:)=S_t_bt(:,:,:,:)

   mu_bt(:,:,:,:)=S_mu_bt(:,:,:,:)

   ru_tend(:,:,:)=S_ru_tend(:,:,:)
   rv_tend(:,:,:)=S_rv_tend(:,:,:)
   rw_tend(:,:,:)=S_rw_tend(:,:,:)
   t_tend(:,:,:)=S_t_tend(:,:,:)
   ph_tend(:,:,:)=S_ph_tend(:,:,:)

   mu_tend(:,:)=S_mu_tend(:,:)

END SUBROUTINE t_spec_bdy_dry

!----------------------------------------------------------------------------------------------

SUBROUTINE t_small_step_prep( u_1, u_2, v_1, v_2, w_1, w_2, &
                            t_1, t_2, ph_1, ph_2,         &
                            mub, mu_1, mu_2,              &
                            muu, muus, muv, muvs,         &
                            mut, muts, mudf,              &
                            u_save, v_save, w_save,       &
                            t_save, ph_save, mu_save,     &
                            ww, ww_save,                  &
                            dnw, c2a, pb, p, alt,         &
                            msfu, msfv, msft,             &
                            rk_step,                      &
                            ids,ide, jds,jde, kds,kde,    &
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

! Zaizhong Ma, March 28,2005

  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: rk_step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_1,   &
                                                              v_1,   &
                                                              w_1,   &
                                                              t_1,   &
                                                              ph_1

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_2,   &
                                                              v_2,   &
                                                              w_2,   &
                                                              t_2,   &
                                                              ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(  OUT) :: c2a, &
                                                               ww_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                ::  pb,  &
                                                                p,   &
                                                                alt, &
                                                                ww

! pjj/cray
! REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INOUT) :: mu_1
  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INOUT) :: mu_1,mu_2

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INout) :: mub,  &
                                                               muu,  &
                                                               muv,  &
                                                               mut,  &
                                                               msfu, &
                                                               msfv, &
                                                               msft

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: muus, &
                                                               muvs, &
                                                               muts, &
!pjj/cray
!                                                              mu_2, &
                                                               mudf
  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: mu_save

  REAL, DIMENSION(kms:kme, jms:jme)         , INTENT(IN   ) :: dnw

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv

!  zzma: new definition

!  IN variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                ::  S_p,   &
                                                                S_alt, &
                                                                S_ww
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                ::  P_p,   &
                                                                P_alt, &
                                                                P_ww
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                ::  B_p,   &
                                                                B_alt, &
                                                                B_ww

! INOUT variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: S_u_1,   &
                                                              S_v_1,   &
                                                              S_w_1,   &
                                                              S_t_1,   &
                                                              S_ph_1,  &
                                                              S_u_2,   &
                                                              S_v_2,   &
                                                              S_w_2,   &
                                                              S_t_2,   &
                                                              S_ph_2
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: P_u_1,   &
                                                              P_v_1,   &
                                                              P_w_1,   &
                                                              P_t_1,   &
                                                              P_ph_1,  &
                                                              P_u_2,   &
                                                              P_v_2,   &
                                                              P_w_2,   &
                                                              P_t_2,   &
                                                              P_ph_2
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: B_u_1,   &
                                                              B_v_1,   &
                                                              B_w_1,   &
                                                              B_t_1,   &
                                                              B_ph_1,  &
                                                              B_u_2,   &
                                                              B_v_2,   &
                                                              B_w_2,   &
                                                              B_t_2,   &
                                                              B_ph_2
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: K_u_1,   &
                                                              K_v_1,   &
                                                              K_w_1,   &
                                                              K_t_1,   &
                                                              K_ph_1,  &
                                                              K_u_2,   &
                                                              K_v_2,   &
                                                              K_w_2,   &
                                                              K_t_2,   &
                                                              K_ph_2


  REAL, DIMENSION(ims:ime, jms:jme)                         :: S_mu_1, &
                                                               S_mu_2, &
                                                               S_muu,  &
                                                               S_muv,  &
                                                               S_mut
  REAL, DIMENSION(ims:ime, jms:jme)                         :: P_mu_1, &
                                                               P_mu_2, &
                                                               P_muu,  &
                                                               P_muv,  &
                                                               P_mut
  REAL, DIMENSION(ims:ime, jms:jme)                         :: B_mu_1, &
                                                               B_mu_2, &
                                                               B_muu,  &
                                                               B_muv,  &
                                                               B_mut
  REAL, DIMENSION(ims:ime, jms:jme)                         :: K_mu_1, &
                                                               K_mu_2, &
                                                               K_muu,  &
                                                               K_muv,  &
                                                               K_mut
! OUT variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: P_u_save,   &
                                                              P_v_save,   &
                                                              P_w_save,   &
                                                              P_t_save,   &
                                                              P_ph_save,  &
                                                              P_c2a,      &
                                                              P_ww_save
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: B_u_save,   &
                                                              B_v_save,   &
                                                              B_w_save,   &
                                                              B_t_save,   &
                                                              B_ph_save,  &
                                                              B_c2a,      &
                                                              B_ww_save

  REAL, DIMENSION(ims:ime, jms:jme)                         :: P_muus, &
                                                               P_muvs, &
                                                               P_muts, &
                                                               P_mu_save ,&
                                                               P_mudf
  REAL, DIMENSION(ims:ime, jms:jme)                         :: B_muus, &
                                                               B_muvs, &
                                                               B_muts, &
                                                               B_mu_save ,&
                                                               B_mudf

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_p(i,k,j)=p(i,k,j)
      S_alt(i,k,j)=alt(i,k,j)
      S_ww(i,k,j)=ww(i,k,j)

      P_p(i,k,j)=p(i,k,j)
      P_alt(i,k,j)=alt(i,k,j)
      P_ww(i,k,j)=ww(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u_1(i,k,j)=u_1(i,k,j)
      S_v_1(i,k,j)=v_1(i,k,j)
      S_w_1(i,k,j)=w_1(i,k,j)
      S_t_1(i,k,j)=t_1(i,k,j)
      S_ph_1(i,k,j)=ph_1(i,k,j)
      S_u_2(i,k,j)=u_2(i,k,j)
      S_v_2(i,k,j)=v_2(i,k,j)
      S_w_2(i,k,j)=w_2(i,k,j)
      S_t_2(i,k,j)=t_2(i,k,j)
      S_ph_2(i,k,j)=ph_2(i,k,j)

      P_u_1(i,k,j)=u_1(i,k,j)
      P_v_1(i,k,j)=v_1(i,k,j)
      P_w_1(i,k,j)=w_1(i,k,j)
      P_t_1(i,k,j)=t_1(i,k,j)
      P_ph_1(i,k,j)=ph_1(i,k,j)
      P_u_2(i,k,j)=u_2(i,k,j)
      P_v_2(i,k,j)=v_2(i,k,j)
      P_w_2(i,k,j)=w_2(i,k,j)
      P_t_2(i,k,j)=t_2(i,k,j)
      P_ph_2(i,k,j)=ph_2(i,k,j)

      K_u_1(i,k,j)=u_1(i,k,j)
      K_v_1(i,k,j)=v_1(i,k,j)
      K_w_1(i,k,j)=w_1(i,k,j)
      K_t_1(i,k,j)=t_1(i,k,j)
      K_ph_1(i,k,j)=ph_1(i,k,j)
      K_u_2(i,k,j)=u_2(i,k,j)
      K_v_2(i,k,j)=v_2(i,k,j)
      K_w_2(i,k,j)=w_2(i,k,j)
      K_t_2(i,k,j)=t_2(i,k,j)
      K_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu_1(i,j)=mu_1(i,j)
      S_mu_2(i,j)=mu_2(i,j)
      S_mut(i,j)=mut(i,j)
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)

      P_mu_1(i,j)=mu_1(i,j)
      P_mu_2(i,j)=mu_2(i,j)
      P_mut(i,j)=mut(i,j)
      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)

      K_mu_1(i,j)=mu_1(i,j)
      K_mu_2(i,j)=mu_2(i,j)
      K_mut(i,j)=mut(i,j)
      K_muu(i,j)=muu(i,j)
      K_muv(i,j)=muv(i,j)

   enddo
   enddo

!NLM

   CALL small_step_prep( u_1, u_2, v_1, v_2, w_1, w_2, &
                            t_1, t_2, ph_1, ph_2,         &
                            mub, mu_1, mu_2,              &
                            muu, muus, muv, muvs,         &
                            mut, muts, mudf,              &
                            u_save, v_save, w_save,       &
                            t_save, ph_save, mu_save,     &
                            ww, ww_save,                  &
                            dnw, c2a, pb, p, alt,         &
                            msfu, msfv, msft,             &
                            rk_step,                      &
                            ids,ide, jds,jde, kds,kde,    &
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_u_1(i,k,j)=u_1(i,k,j)
      B_v_1(i,k,j)=v_1(i,k,j)
      B_w_1(i,k,j)=w_1(i,k,j)
      B_t_1(i,k,j)=t_1(i,k,j)
      B_ph_1(i,k,j)=ph_1(i,k,j)
      B_u_2(i,k,j)=u_2(i,k,j)
      B_v_2(i,k,j)=v_2(i,k,j)
      B_w_2(i,k,j)=w_2(i,k,j)
      B_t_2(i,k,j)=t_2(i,k,j)
      B_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      B_mu_1(i,j)=mu_1(i,j)
      B_mu_2(i,j)=mu_2(i,j)
      B_mut(i,j)=mut(i,j)
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_u_save(i,k,j)=u_save(i,k,j)
      B_v_save(i,k,j)=v_save(i,k,j)
      B_w_save(i,k,j)=w_save(i,k,j)
      B_ph_save(i,k,j)=ph_save(i,k,j)
      B_t_save(i,k,j)=t_save(i,k,j)
      B_c2a(i,k,j)=c2a(i,k,j)
      B_ww_save(i,k,j)=ww_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      B_muus(i,j)=muus(i,j)
      B_muvs(i,j)=muvs(i,j)
      B_muts(i,j)=muts(i,j)
      B_mu_save(i,j)=mu_save(i,j)
      B_mudf(i,j)=mudf(i,j)
   enddo
   enddo

!  TCL

   CALL g_small_step_prep( K_u_1, P_u_1, K_u_2, P_u_2, K_v_1, P_v_1, K_v_2, P_v_2, K_w_1, P_w_1, K_w_2, P_w_2, K_t_1, P_t_1, K_t_2, P_t_2, K_ph_1,&
& P_ph_1, K_ph_2, P_ph_2, mub, K_mu_1, P_mu_1, K_mu_2, P_mu_2, K_muu, P_muu, muus, P_muus, K_muv, P_muv, muvs, P_muvs, K_mut, P_mut, muts, &
&P_muts, mudf, P_mudf, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, P_t_save, ph_save, P_ph_save, mu_save, &
&P_mu_save, ww, P_ww, ww_save, P_ww_save, c2a, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, msft, rk_step, ide, jde, kde, &
&ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_u_1(i,k,j)*P_u_1(i,k,j)         &
                    + P_v_1(i,k,j)*P_v_1(i,k,j)         &
                    + P_w_1(i,k,j)*P_w_1(i,k,j)         &
                    + P_t_1(i,k,j)*P_t_1(i,k,j)         &
                    + P_ph_1(i,k,j)*P_ph_1(i,k,j)       &
                    + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_mu_1(i,j)*P_mu_1(i,j)           &
                    + P_mu_2(i,j)*P_mu_2(i,j)           &
                    + P_mut(i,j)*P_mut(i,j)             &
                    + P_muu(i,j)*P_muu(i,j)             &
                    + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_u_save(i,k,j)*P_u_save(i,k,j)    &
                    + P_v_save(i,k,j)*P_v_save(i,k,j)    &
                    + P_w_save(i,k,j)*P_w_save(i,k,j)    &
                    + P_ph_save(i,k,j)*P_ph_save(i,k,j)  &
                    + P_t_save(i,k,j)*P_t_save(i,k,j)    &
                    + P_c2a(i,k,j)*P_c2a(i,k,j)          &
                    + P_ww_save(i,k,j)*P_ww_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_muus(i,j)*P_muus(i,j)            &
                    + P_muvs(i,j)*P_muvs(i,j)            &
                    + P_muts(i,j)*P_muts(i,j)            &
                    + P_mu_save(i,j)*P_mu_save(i,j)      &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_1(i,k,j)=FACTOR*S_u_1(i,k,j)
      P_v_1(i,k,j)=FACTOR*S_v_1(i,k,j)
      P_w_1(i,k,j)=FACTOR*S_w_1(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_ph_1(i,k,j)=FACTOR*S_ph_1(i,k,j)
      P_u_2(i,k,j)=FACTOR*S_u_2(i,k,j)
      P_v_2(i,k,j)=FACTOR*S_v_2(i,k,j)
      P_w_2(i,k,j)=FACTOR*S_w_2(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_2(i,k,j)=FACTOR*S_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_1(i,j)=FACTOR*S_mu_1(i,j)
      P_mu_2(i,j)=FACTOR*S_mu_2(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
   enddo
   enddo

   CALL small_step_prep( P_u_1, P_u_2, P_v_1, P_v_2, P_w_1, P_w_2, &
                            P_t_1, P_t_2, P_ph_1, P_ph_2,         &
                            mub, P_mu_1, P_mu_2,              &
                            P_muu, P_muus, P_muv, P_muvs,         &
                            P_mut, P_muts, P_mudf,              &
                            P_u_save, P_v_save, P_w_save,       &
                            P_t_save, P_ph_save, P_mu_save,     &
                            P_ww, P_ww_save,                  &
                            dnw, P_c2a, pb, P_p, P_alt,         &
                            msfu, msfv, msft,             &
                            rk_step,              &
                            ids,ide, jds,jde, kds,kde,    &
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_u_1(i,k,j)- B_u_1(i,k,j))*(P_u_1(i,k,j)- B_u_1(i,k,j))        &
                    + (P_v_1(i,k,j)- B_v_1(i,k,j))*(P_v_1(i,k,j)- B_v_1(i,k,j))         &
                    + (P_w_1(i,k,j)- B_w_1(i,k,j))*(P_w_1(i,k,j)- B_w_1(i,k,j))         &
                    + (P_t_1(i,k,j)- B_t_1(i,k,j))*(P_t_1(i,k,j)- B_t_1(i,k,j))         &
                    + (P_ph_1(i,k,j)- B_ph_1(i,k,j))*(P_ph_1(i,k,j)- B_ph_1(i,k,j))     &
                    + (P_u_2(i,k,j)- B_u_2(i,k,j))*(P_u_2(i,k,j)- B_u_2(i,k,j))         &
                    + (P_v_2(i,k,j)- B_v_2(i,k,j))*(P_v_2(i,k,j)- B_v_2(i,k,j))         &
                    + (P_w_2(i,k,j)- B_w_2(i,k,j))*(P_w_2(i,k,j)- B_w_2(i,k,j))         &
                    + (P_t_2(i,k,j)- B_t_2(i,k,j))*(P_t_2(i,k,j)- B_t_2(i,k,j))         &
                    + (P_ph_2(i,k,j)- B_ph_2(i,k,j))*(P_ph_2(i,k,j)- B_ph_2(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
         VAL_N=VAL_N + (P_mu_1(i,j)- B_mu_1(i,j))*(P_mu_1(i,j)- B_mu_1(i,j))         &
                    + (P_mu_2(i,j)- B_mu_2(i,j))*(P_mu_2(i,j)- B_mu_2(i,j))          &
                    + (P_mut(i,j)- B_mut(i,j))*(P_mut(i,j)- B_mut(i,j))              &
                    + (P_muu(i,j)- B_muu(i,j))*(P_muu(i,j)- B_muu(i,j))              &
                    + (P_muv(i,j)- B_muv(i,j))*(P_muv(i,j)- B_muv(i,j))         
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
         VAL_N=VAL_N + (P_u_save(i,k,j)- B_u_save(i,k,j))*(P_u_save(i,k,j)- B_u_save(i,k,j))        &
                    + (P_v_save(i,k,j)- B_v_save(i,k,j))*(P_v_save(i,k,j)- B_v_save(i,k,j))         &
                    + (P_w_save(i,k,j)- B_w_save(i,k,j))*(P_w_save(i,k,j)- B_w_save(i,k,j))         &
                    + (P_ph_save(i,k,j)- B_ph_save(i,k,j))*(P_ph_save(i,k,j)- B_ph_save(i,k,j))     &
                    + (P_t_save(i,k,j)- B_t_save(i,k,j))*(P_t_save(i,k,j)- B_t_save(i,k,j))         &
                    + (P_c2a(i,k,j)- B_c2a(i,k,j))*(P_c2a(i,k,j)- B_c2a(i,k,j))                     &
                    + (P_ww_save(i,k,j)- B_ww_save(i,k,j))*(P_ww_save(i,k,j)- B_ww_save(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
         VAL_N=VAL_N + (P_muus(i,j)- B_muus(i,j))*(P_muus(i,j)- B_muus(i,j))              &
                    + (P_muvs(i,j)- B_muvs(i,j))*(P_muvs(i,j)- B_muvs(i,j))               &
                    + (P_muts(i,j)- B_muts(i,j))*(P_muts(i,j)- B_muts(i,j))               &
                    + (P_mu_save(i,j)- B_mu_save(i,j))*(P_mu_save(i,j)- B_mu_save(i,j))   &
                    + (P_mudf(i,j)- B_mudf(i,j))*(P_mudf(i,j)- B_mudf(i,j))
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_small_step_prep: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p(i,k,j)=S_p(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)

      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)

      B_p(i,k,j)=P_p(i,k,j)
      B_alt(i,k,j)=P_alt(i,k,j)
      B_ww(i,k,j)=P_ww(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_1(i,k,j)=S_u_1(i,k,j)
      v_1(i,k,j)=S_v_1(i,k,j)
      w_1(i,k,j)=S_w_1(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      ph_1(i,k,j)=S_ph_1(i,k,j)
      u_2(i,k,j)=S_u_2(i,k,j)
      v_2(i,k,j)=S_v_2(i,k,j)
      w_2(i,k,j)=S_w_2(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_2(i,k,j)=S_ph_2(i,k,j)

      P_u_1(i,k,j)=FACTOR*S_u_1(i,k,j)
      P_v_1(i,k,j)=FACTOR*S_v_1(i,k,j)
      P_w_1(i,k,j)=FACTOR*S_w_1(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_ph_1(i,k,j)=FACTOR*S_ph_1(i,k,j)
      P_u_2(i,k,j)=FACTOR*S_u_2(i,k,j)
      P_v_2(i,k,j)=FACTOR*S_v_2(i,k,j)
      P_w_2(i,k,j)=FACTOR*S_w_2(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_2(i,k,j)=FACTOR*S_ph_2(i,k,j)

      B_u_1(i,k,j)=P_u_1(i,k,j)
      B_v_1(i,k,j)=P_v_1(i,k,j)
      B_w_1(i,k,j)=P_w_1(i,k,j)
      B_t_1(i,k,j)=P_t_1(i,k,j)
      B_ph_1(i,k,j)=P_ph_1(i,k,j)
      B_u_2(i,k,j)=P_u_2(i,k,j)
      B_v_2(i,k,j)=P_v_2(i,k,j)
      B_w_2(i,k,j)=P_w_2(i,k,j)
      B_t_2(i,k,j)=P_t_2(i,k,j)
      B_ph_2(i,k,j)=P_ph_2(i,k,j)

      K_u_1(i,k,j)=u_1(i,k,j)
      K_v_1(i,k,j)=v_1(i,k,j)
      K_w_1(i,k,j)=w_1(i,k,j)
      K_t_1(i,k,j)=t_1(i,k,j)
      K_ph_1(i,k,j)=ph_1(i,k,j)
      K_u_2(i,k,j)=u_2(i,k,j)
      K_v_2(i,k,j)=v_2(i,k,j)
      K_w_2(i,k,j)=w_2(i,k,j)
      K_t_2(i,k,j)=t_2(i,k,j)
      K_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_1(i,j)=S_mu_1(i,j)
      mu_2(i,j)=S_mu_2(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)

      P_mu_1(i,j)=FACTOR*S_mu_1(i,j)
      P_mu_2(i,j)=FACTOR*S_mu_2(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)

      B_mu_1(i,j)=P_mu_1(i,j)
      B_mu_2(i,j)=P_mu_2(i,j)
      B_mut(i,j)=P_mut(i,j)
      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)

      K_mu_1(i,j)=mu_1(i,j)
      K_mu_2(i,j)=mu_2(i,j)
      K_mut(i,j)=mut(i,j)
      K_muu(i,j)=muu(i,j)
      K_muv(i,j)=muv(i,j)
   enddo
   enddo

!  TGL

   CALL g_small_step_prep( u_1, P_u_1, u_2, P_u_2, v_1, P_v_1, v_2, P_v_2, w_1, P_w_1, w_2, P_w_2, t_1, P_t_1, t_2, P_t_2, ph_1,&
& P_ph_1, ph_2, P_ph_2, mub, mu_1, P_mu_1, mu_2, P_mu_2, muu, P_muu, muus, P_muus, muv, P_muv, muvs, P_muvs, mut, P_mut, muts, &
&P_muts, mudf, P_mudf, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, P_t_save, ph_save, P_ph_save, mu_save, &
&P_mu_save, ww, P_ww, ww_save, P_ww_save, c2a, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, msft, rk_step, ide, jde, kde, &
&ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L  + P_u_1(i,k,j)*P_u_1(i,k,j)         &
                    + P_v_1(i,k,j)*P_v_1(i,k,j)         &
                    + P_w_1(i,k,j)*P_w_1(i,k,j)         &
                    + P_t_1(i,k,j)*P_t_1(i,k,j)         &
                    + P_ph_1(i,k,j)*P_ph_1(i,k,j)       &
                    + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L  + P_mu_1(i,j)*P_mu_1(i,j)           &
                    + P_mu_2(i,j)*P_mu_2(i,j)           &
                    + P_mut(i,j)*P_mut(i,j)             &
                    + P_muu(i,j)*P_muu(i,j)             &
                    + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L  + P_u_save(i,k,j)*P_u_save(i,k,j)    &
                    + P_v_save(i,k,j)*P_v_save(i,k,j)    &
                    + P_w_save(i,k,j)*P_w_save(i,k,j)    &
                    + P_ph_save(i,k,j)*P_ph_save(i,k,j)  &
                    + P_t_save(i,k,j)*P_t_save(i,k,j)    &
                    + P_c2a(i,k,j)*P_c2a(i,k,j)          &
                    + P_ww_save(i,k,j)*P_ww_save(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L  + P_muus(i,j)*P_muus(i,j)            &
                    + P_muvs(i,j)*P_muvs(i,j)            &
                    + P_muts(i,j)*P_muts(i,j)            &
                    + P_mu_save(i,j)*P_mu_save(i,j)      &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_p(i,k,j)=0.0
      P_alt(i,k,j)=0.0
      P_ww(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ

   CALL a_small_step_prep( K_u_1, P_u_1, K_u_2, P_u_2, K_v_1, P_v_1, K_v_2, P_v_2, K_w_1, P_w_1, K_w_2, P_w_2, K_t_1, P_t_1, K_t_2, P_t_2, &
&P_ph_1, P_ph_2, mub, K_mu_1, P_mu_1, K_mu_2, P_mu_2, K_muu, P_muu, muus, P_muus, K_muv, P_muv, muvs, P_muvs, K_mut, P_mut, muts, P_muts, &
&P_mudf, P_u_save, P_v_save, P_w_save, P_t_save, P_ph_save, P_mu_save, P_ww, P_ww_save, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, &
&msft, rk_step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )


   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_p(i,k,j)*B_p(i,k,j)      &
                  +P_alt(i,k,j)*B_alt(i,k,j)  &
                  +P_ww(i,k,j)*B_ww(i,k,j)                     
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_u_1(i,k,j)*B_u_1(i,k,j)         &
                    + P_v_1(i,k,j)*B_v_1(i,k,j)         &
                    + P_w_1(i,k,j)*B_w_1(i,k,j)         &
                    + P_t_1(i,k,j)*B_t_1(i,k,j)         &
                    + P_ph_1(i,k,j)*B_ph_1(i,k,j)       &
                    + P_u_2(i,k,j)*B_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*B_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*B_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*B_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*B_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu_1(i,j)*B_mu_1(i,j)           &
                    + P_mu_2(i,j)*B_mu_2(i,j)           &
                    + P_mut(i,j)*B_mut(i,j)             &
                    + P_muu(i,j)*B_muu(i,j)             &
                    + P_muv(i,j)*B_muv(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_small_step_prep: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      p(i,k,j)=S_p(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_1(i,k,j)=S_u_1(i,k,j)
      v_1(i,k,j)=S_v_1(i,k,j)
      w_1(i,k,j)=S_w_1(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      ph_1(i,k,j)=S_ph_1(i,k,j)
      u_2(i,k,j)=S_u_2(i,k,j)
      v_2(i,k,j)=S_v_2(i,k,j)
      w_2(i,k,j)=S_w_2(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_2(i,k,j)=S_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_1(i,j)=S_mu_1(i,j)
      mu_2(i,j)=S_mu_2(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
   enddo
   enddo

!g_small_step_prep: ALPHA=.1000E+00  COEF=   0.1210182666779E+01  VAL_N= 0.419130E+17  VAL_L= 0.346336E+17
!g_small_step_prep: ALPHA=.1000E-01  COEF=   0.1020382523537E+01  VAL_N= 0.353396E+15  VAL_L= 0.346336E+15
!g_small_step_prep: ALPHA=.1000E-02  COEF=   0.1002328515053E+01  VAL_N= 0.347143E+13  VAL_L= 0.346337E+13
!g_small_step_prep: ALPHA=.1000E-03  COEF=   0.1001530051231E+01  VAL_N= 0.346866E+11  VAL_L= 0.346337E+11
!g_small_step_prep: ALPHA=.1000E-04  COEF=   0.1006592035294E+01  VAL_N= 0.348620E+09  VAL_L= 0.346337E+09
!g_small_step_prep: ALPHA=.1000E-05  COEF=   0.8559100627899E+00  VAL_N= 0.296433E+07  VAL_L= 0.346337E+07
!g_small_step_prep: ALPHA=.1000E-06  COEF=   0.1041831374168E+01  VAL_N= 0.360824E+05  VAL_L= 0.346337E+05
!g_small_step_prep: ALPHA=.1000E-07  COEF=   0.3898711849393E-20  VAL_N= 0.135027E-17  VAL_L= 0.346337E+03
!g_small_step_prep: ALPHA=.1000E-08  COEF=   0.3898711865549E-18  VAL_N= 0.135027E-17  VAL_L= 0.346337E+01
!g_small_step_prep: ALPHA=.1000E-09  COEF=   0.3898711338221E-16  VAL_N= 0.135027E-17  VAL_L= 0.346337E-01
!g_small_step_prep: ALPHA=.1000E-10  COEF=   0.3898710967644E-14  VAL_N= 0.135027E-17  VAL_L= 0.346337E-03
                 
!a_small_step_prep:    0.3464210455042E+17   0.3464191771935E+17


!g_small_step_prep: ALPHA=.1000E+00  COEF=   0.1209974728298E+01  VAL_N= 0.419156E+17  VAL_L= 0.346417E+17
!g_small_step_prep: ALPHA=.1000E-01  COEF=   0.1020097416385E+01  VAL_N= 0.353380E+15  VAL_L= 0.346417E+15
!g_small_step_prep: ALPHA=.1000E-02  COEF=   0.1002000740908E+01  VAL_N= 0.347111E+13  VAL_L= 0.346417E+13
!g_small_step_prep: ALPHA=.1000E-03  COEF=   0.1000199984095E+01  VAL_N= 0.346487E+11  VAL_L= 0.346417E+11
!g_small_step_prep: ALPHA=.1000E-04  COEF=   0.1000019998524E+01  VAL_N= 0.346424E+09  VAL_L= 0.346417E+09
!g_small_step_prep: ALPHA=.1000E-05  COEF=   0.1000002099436E+01  VAL_N= 0.346418E+07  VAL_L= 0.346417E+07
!g_small_step_prep: ALPHA=.1000E-06  COEF=   0.1000010201248E+01  VAL_N= 0.346421E+05  VAL_L= 0.346417E+05
!g_small_step_prep: ALPHA=.1000E-07  COEF=   0.1000999980880E+01  VAL_N= 0.346764E+03  VAL_L= 0.346417E+03
!g_small_step_prep: ALPHA=.1000E-08  COEF=   0.1099997807301E+01  VAL_N= 0.381058E+01  VAL_L= 0.346417E+01
!g_small_step_prep: ALPHA=.1000E-09  COEF=   0.1099973804174E+02  VAL_N= 0.381050E+00  VAL_L= 0.346417E-01
!g_small_step_prep: ALPHA=.1000E-10  COEF=   0.1000973937427E+04  VAL_N= 0.346755E+00  VAL_L= 0.346417E-03
                 
!a_small_step_prep:    0.3464174588297E+17   0.3464174588296E+17

END SUBROUTINE t_small_step_prep

!-----------------------------------------------------------------------------------------------

SUBROUTINE t_calc_p_rho( al, p, ph,                    &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

! Zaizhong Ma, March 28,2005


  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: al,   &
                                                               p
! pjj/cray
!                                                             p,    &
!                                                             pm1

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: alt,   &
                                                              t_2,   &
                                                              t_1,   &
                                                              c2a

! pjj/cray
! REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph, pm1

  REAL, DIMENSION(ims:ime, jms:jme)                         :: mu,   &
                                                               muts

  REAL, DIMENSION(kms:kme)         , INTENT(IN   ) :: dnw,  &
                                                      rdnw, &
                                                      znu

  REAL,                                       INTENT(IN   ) :: t0, smdiv

  LOGICAL, INTENT(IN   )  :: non_hydrostatic

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  REAL    :: ptmp


!  zzma: new definition

!  IN variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme)              :: S_alt,   &
                                                              S_t_2,   &
                                                              S_t_1,   &
                                                              S_c2a
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme)              :: P_alt,   &
                                                              P_t_2,   &
                                                              P_t_1,   &
                                                              P_c2a
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme)              :: B_alt,   &
                                                              B_t_2,   &
                                                              B_t_1,   &
                                                              B_c2a
   REAL, DIMENSION(ims:ime, jms:jme)          :: S_mu, S_muts,P_mu, P_muts,B_mu, B_muts

!  INOUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: S_ph, S_pm1,P_ph, P_pm1,K_ph, K_pm1,B_ph, B_pm1

!  OUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme)   :: P_al, P_p,B_al, B_p

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_alt(i,k,j)=alt(i,k,j)
      S_t_2(i,k,j)=t_2(i,k,j)
      S_t_1(i,k,j)=t_1(i,k,j)
      S_c2a(i,k,j)=c2a(i,k,j)

      P_alt(i,k,j)=alt(i,k,j)
      P_t_2(i,k,j)=t_2(i,k,j)
      P_t_1(i,k,j)=t_1(i,k,j)
      P_c2a(i,k,j)=c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      S_muts(i,j)=muts(i,j)

      P_mu(i,j)=mu(i,j)
      P_muts(i,j)=muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ph(i,k,j)=ph(i,k,j)
      S_pm1(i,k,j)=pm1(i,k,j)

      P_ph(i,k,j)=ph(i,k,j)
      P_pm1(i,k,j)=pm1(i,k,j)

      K_ph(i,k,j)=ph(i,k,j)
      K_pm1(i,k,j)=pm1(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL calc_p_rho( al, p, ph,                    &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_al(i,k,j)=al(i,k,j)
      B_p(i,k,j)=p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ph(i,k,j)=ph(i,k,j)
      B_pm1(i,k,j)=pm1(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_calc_p_rho( al, P_al, p, P_p, K_ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, K_pm1, P_pm1, mu, P_mu, muts, &
&P_muts, znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, &
&kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_al(i,k,j)*P_al(i,k,j) +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_ph(i,k,j)*P_ph(i,k,j) + P_pm1(i,k,j)*P_pm1(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_pm1(i,k,j)=FACTOR*S_pm1(i,k,j)
   enddo
   enddo
   enddo

   CALL calc_p_rho( P_al, P_p, P_ph,                    &
                       P_alt, P_t_2, P_t_1, P_c2a, P_pm1,      &
                       P_mu, P_muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_al(i,k,j)- B_al(i,k,j))*(P_al(i,k,j)- B_al(i,k,j))  &
                     + (P_p(i,k,j) - B_p(i,k,j))*(P_p(i,k,j) - B_p(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
         VAL_N=VAL_N + (P_ph(i,k,j)- B_ph(i,k,j))*(P_ph(i,k,j)- B_ph(i,k,j))  &
                     + (P_pm1(i,k,j)-B_pm1(i,k,j))*(P_pm1(i,k,j)-B_pm1(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calc_p_rho: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      alt(i,k,j)=S_alt(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      c2a(i,k,j)=S_c2a(i,k,j)

      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)

      B_alt(i,k,j)=P_alt(i,k,j)
      B_t_2(i,k,j)=P_t_2(i,k,j)
      B_t_1(i,k,j)=P_t_1(i,k,j)
      B_c2a(i,k,j)=P_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      muts(i,j)=S_muts(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)

      B_mu(i,j)=P_mu(i,j)
      B_muts(i,j)=P_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ph(i,k,j)=S_ph(i,k,j)
      pm1(i,k,j)=S_pm1(i,k,j)

      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_pm1(i,k,j)=FACTOR*S_pm1(i,k,j)

      B_ph(i,k,j)=P_ph(i,k,j)
      B_pm1(i,k,j)=P_pm1(i,k,j)

      K_ph(i,k,j)=ph(i,k,j)
      K_pm1(i,k,j)=pm1(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calc_p_rho( al, P_al, p, P_p, ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, pm1, P_pm1, mu, P_mu, muts, &
&P_muts, znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, &
&kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_al(i,k,j)*P_al(i,k,j) +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_ph(i,k,j)*P_ph(i,k,j) + P_pm1(i,k,j)*P_pm1(i,k,j)
   enddo
   enddo
   enddo


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_alt(i,k,j)=0.0
      P_t_2(i,k,j)=0.0
      P_t_1(i,k,j)=0.0
      P_c2a(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
      P_muts(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_calc_p_rho( al, P_al, p, P_p, K_ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, P_pm1, mu, P_mu, muts, P_muts,&
& znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_alt(i,k,j)*B_alt(i,k,j)   &
                  + P_t_2(i,k,j)*B_t_2(i,k,j)   &
                  + P_t_1(i,k,j)*B_t_1(i,k,j)   &
                  + P_c2a(i,k,j)*B_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)   &
                  + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ph(i,k,j)*B_ph(i,k,j)   &
                  + P_pm1(i,k,j)*B_pm1(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_calc_p_rho: ', VAL_L,VAL_A


!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      alt(i,k,j)=S_alt(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      c2a(i,k,j)=S_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      muts(i,j)=S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ph(i,k,j)=S_ph(i,k,j)
      pm1(i,k,j)=S_pm1(i,k,j)
   enddo
   enddo
   enddo

!g_calc_p_rho: ALPHA=.1000E+00  COEF=   0.1000004291534E+01  VAL_N= 0.171105E+08  VAL_L= 0.171104E+08
!g_calc_p_rho: ALPHA=.1000E-01  COEF=   0.1000008940697E+01  VAL_N= 0.171106E+06  VAL_L= 0.171104E+06
!g_calc_p_rho: ALPHA=.1000E-02  COEF=   0.1001088023186E+01  VAL_N= 0.171290E+04  VAL_L= 0.171104E+04
!g_calc_p_rho: ALPHA=.1000E-03  COEF=   0.1101572036743E+01  VAL_N= 0.188484E+02  VAL_L= 0.171104E+02
!g_calc_p_rho: ALPHA=.1000E-04  COEF=   0.1111159610748E+02  VAL_N= 0.190124E+01  VAL_L= 0.171104E+00
!g_calc_p_rho: ALPHA=.1000E-05  COEF=   0.1011856994629E+04  VAL_N= 0.173133E+01  VAL_L= 0.171104E-02
!g_calc_p_rho: ALPHA=.1000E-06  COEF=   0.1010873671875E+06  VAL_N= 0.172965E+01  VAL_L= 0.171104E-04
!g_calc_p_rho: ALPHA=.1000E-07  COEF=   0.1010873600000E+08  VAL_N= 0.172965E+01  VAL_L= 0.171104E-06
!g_calc_p_rho: ALPHA=.1000E-08  COEF=   0.1010873664000E+10  VAL_N= 0.172965E+01  VAL_L= 0.171104E-08
!g_calc_p_rho: ALPHA=.1000E-09  COEF=   0.1010873548800E+12  VAL_N= 0.172965E+01  VAL_L= 0.171104E-10
!g_calc_p_rho: ALPHA=.1000E-10  COEF=   0.1010873506202E+14  VAL_N= 0.172965E+01  VAL_L= 0.171104E-12
                 
!a_calc_p_rho:    0.1711048600000E+08   0.1711048600000E+08

!g_calc_p_rho: ALPHA=.1000E+00  COEF=   0.1000000101086E+01  VAL_N= 0.171103E+08  VAL_L= 0.171103E+08
!g_calc_p_rho: ALPHA=.1000E-01  COEF=   0.1000010108599E+01  VAL_N= 0.171105E+06  VAL_L= 0.171103E+06
!g_calc_p_rho: ALPHA=.1000E-02  COEF=   0.1001010859867E+01  VAL_N= 0.171276E+04  VAL_L= 0.171103E+04
!g_calc_p_rho: ALPHA=.1000E-03  COEF=   0.1101085986751E+01  VAL_N= 0.188400E+02  VAL_L= 0.171103E+02
!g_calc_p_rho: ALPHA=.1000E-04  COEF=   0.1110859867508E+02  VAL_N= 0.190072E+01  VAL_L= 0.171103E+00
!g_calc_p_rho: ALPHA=.1000E-05  COEF=   0.1011859867506E+04  VAL_N= 0.173133E+01  VAL_L= 0.171103E-02
!g_calc_p_rho: ALPHA=.1000E-06  COEF=   0.1010869867506E+06  VAL_N= 0.172963E+01  VAL_L= 0.171103E-04
!g_calc_p_rho: ALPHA=.1000E-07  COEF=   0.1010859967506E+08  VAL_N= 0.172962E+01  VAL_L= 0.171103E-06
!g_calc_p_rho: ALPHA=.1000E-08  COEF=   0.1010859868506E+10  VAL_N= 0.172962E+01  VAL_L= 0.171103E-08
!g_calc_p_rho: ALPHA=.1000E-09  COEF=   0.1010859867516E+12  VAL_N= 0.172962E+01  VAL_L= 0.171103E-10
!g_calc_p_rho: ALPHA=.1000E-10  COEF=   0.1010859867506E+14  VAL_N= 0.172962E+01  VAL_L= 0.171103E-12
                 
!a_calc_p_rho:    0.1711034017432E+08   0.1711034017432E+08


END SUBROUTINE t_calc_p_rho

!-----------------------------------------------------------------------------------------------

SUBROUTINE t_calc_coef_w( a,alpha,gamma,              &
                        mut, cqw,                   &
                        rdn, rdnw, c2a,             &
                        dts, g, epssm,              &
                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                        its,ite, jts,jte, kts,kte  )  ! tile   dims

! Zaizhong Ma, March 30,2005

      IMPLICIT NONE  ! religion first

!  passed in through the call

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                :: c2a,  &
                                                               cqw

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                :: alpha, &
                                                               gamma, &
                                                               a

  REAL, DIMENSION(ims:ime, jms:jme)                         :: mut

  REAL, DIMENSION(kms:kme),                   INTENT(IN   ) :: rdn,   &
                                                               rdnw

  REAL,                                       INTENT(IN   ) :: epssm, &
                                                               dts,   &
                                                               g

!  Local stack data.

  REAL, DIMENSION(ims:ime)                         :: cof
  REAL  :: b, c

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: ij, ijp, ijm

!  zzma: new definition

!  IN variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)     :: S_c2a, S_cqw,P_c2a, P_cqw,B_c2a, B_cqw
  REAL, DIMENSION(ims:ime, jms:jme)              :: S_mut,P_mut,B_mut

! INOUT variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)  :: S_alpha,S_gamma,S_a,P_alpha,P_gamma,P_a,  &
                                                 K_alpha,K_gamma,K_a,B_alpha,B_gamma,B_a

   REAL :: SAVE_L, COEF, ALPHA_M, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_c2a(i,k,j)=c2a(i,k,j)
      S_cqw(i,k,j)=cqw(i,k,j)

      P_c2a(i,k,j)=c2a(i,k,j)
      P_cqw(i,k,j)=cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mut(i,j)=mut(i,j)

      P_mut(i,j)=mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_alpha(i,k,j)=alpha(i,k,j)
      S_gamma(i,k,j)=gamma(i,k,j)
      S_a(i,k,j)=a(i,k,j)

      P_alpha(i,k,j)=alpha(i,k,j)
      P_gamma(i,k,j)=gamma(i,k,j)
      P_a(i,k,j)=a(i,k,j)

      K_alpha(i,k,j)=alpha(i,k,j)
      K_gamma(i,k,j)=gamma(i,k,j)
      K_a(i,k,j)=a(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL calc_coef_w( a,alpha,gamma,              &
                        mut, cqw,                   &
                        rdn, rdnw, c2a,             &
                        dts, g, epssm,              &
                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                        its,ite, jts,jte, kts,kte  )  ! tile   dims

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_alpha(i,k,j)=alpha(i,k,j)
      B_gamma(i,k,j)=gamma(i,k,j)
      B_a(i,k,j)=a(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_calc_coef_w( K_a, P_a, K_alpha, P_alpha, K_gamma, P_gamma, mut, P_mut, cqw, P_cqw, rdn, rdnw, c2a, P_c2a, dts, g, epssm, &
&ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_alpha(i,k,j)*P_alpha(i,k,j)     &
                    + P_gamma(i,k,j)*P_gamma(i,k,j)     &
                    + P_a(i,k,j)*P_a(i,k,j)
   enddo
   enddo
   enddo

   ALPHA_M=1.
   DO NT=1,11
      ALPHA_M=0.1*ALPHA_M
      FACTOR=1.+ALPHA_M
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mut(i,j)=FACTOR*S_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_alpha(i,k,j)=FACTOR*S_alpha(i,k,j)
      P_gamma(i,k,j)=FACTOR*S_gamma(i,k,j)
      P_a(i,k,j)=FACTOR*S_a(i,k,j)
   enddo
   enddo
   enddo

   CALL calc_coef_w( P_a,P_alpha,P_gamma,              &
                        P_mut, P_cqw,                   &
                        rdn, rdnw, P_c2a,             &
                        dts, g, epssm,              &
                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                        its,ite, jts,jte, kts,kte  )  ! tile   dims

   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N + (P_alpha(i,k,j) -B_alpha(i,k,j))*(P_alpha(i,k,j) -B_alpha(i,k,j))    &
                  + (P_gamma(i,k,j) -B_gamma(i,k,j))*(P_gamma(i,k,j) -B_gamma(i,k,j))    &
                  + (P_a(i,k,j) -B_a(i,k,j))*(P_a(i,k,j) -B_a(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA_M**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calc_coef_w: ALPHA=',ALPHA_M,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      c2a(i,k,j)=S_c2a(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)

      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)

      B_c2a(i,k,j)=P_c2a(i,k,j)
      B_cqw(i,k,j)=P_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mut(i,j)=S_mut(i,j)

      P_mut(i,j)=FACTOR*S_mut(i,j)

      B_mut(i,j)=P_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      alpha(i,k,j)=S_alpha(i,k,j)
      gamma(i,k,j)=S_gamma(i,k,j)
      a(i,k,j)=S_a(i,k,j)

      P_alpha(i,k,j)=FACTOR*S_alpha(i,k,j)
      P_gamma(i,k,j)=FACTOR*S_gamma(i,k,j)
      P_a(i,k,j)=FACTOR*S_a(i,k,j)

      B_alpha(i,k,j)=P_alpha(i,k,j)
      B_gamma(i,k,j)=P_gamma(i,k,j)
      B_a(i,k,j)=P_a(i,k,j)

      K_alpha(i,k,j)=alpha(i,k,j)
      K_gamma(i,k,j)=gamma(i,k,j)
      K_a(i,k,j)=a(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calc_coef_w( a, P_a, alpha, P_alpha, gamma, P_gamma, mut, P_mut, cqw, P_cqw, rdn, rdnw, c2a, P_c2a, dts, g, epssm, &
&ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_alpha(i,k,j)*P_alpha(i,k,j)     &
                    + P_gamma(i,k,j)*P_gamma(i,k,j)     &
                    + P_a(i,k,j)*P_a(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_c2a(i,k,j)=0.0
      P_cqw(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mut(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_calc_coef_w( K_a, P_a, K_alpha, P_alpha, K_gamma, P_gamma, mut, P_mut, cqw, P_cqw, rdn, rdnw, c2a, P_c2a, dts, g, epssm, &
&ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_c2a(i,k,j)*B_c2a(i,k,j)    &
                  + P_cqw(i,k,j)*B_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mut(i,j)*B_mut(i,j) 
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_alpha(i,k,j)*B_alpha(i,k,j)   &
                  + P_gamma(i,k,j)*B_gamma(i,k,j)   &
                  + P_a(i,k,j)*B_a(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_calc_coef_w: ', VAL_L,VAL_A


!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      c2a(i,k,j)=S_c2a(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mut(i,j)=S_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      alpha(i,k,j)=S_alpha(i,k,j)
      gamma(i,k,j)=S_gamma(i,k,j)
      a(i,k,j)=S_a(i,k,j)
   enddo
   enddo
   enddo

!g_calc_coef_w: ALPHA=.1000E+00  COEF=   0.8264269232750E+00  VAL_N= 0.922998E+06  VAL_L= 0.111685E+07
!g_calc_coef_w: ALPHA=.1000E-01  COEF=   0.9802830815315E+00  VAL_N= 0.109483E+05  VAL_L= 0.111685E+05
!g_calc_coef_w: ALPHA=.1000E-02  COEF=   0.9980322718620E+00  VAL_N= 0.111466E+03  VAL_L= 0.111685E+03
!g_calc_coef_w: ALPHA=.1000E-03  COEF=   0.1003263592720E+01  VAL_N= 0.112050E+01  VAL_L= 0.111685E+01
!g_calc_coef_w: ALPHA=.1000E-04  COEF=   0.1013251900673E+01  VAL_N= 0.113165E-01  VAL_L= 0.111685E-01
!g_calc_coef_w: ALPHA=.1000E-05  COEF=   0.1187977194786E+01  VAL_N= 0.132680E-03  VAL_L= 0.111685E-03
!g_calc_coef_w: ALPHA=.1000E-06  COEF=   0.1717434310913E+02  VAL_N= 0.191812E-04  VAL_L= 0.111685E-05
!g_calc_coef_w: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.111685E-07
!g_calc_coef_w: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.111685E-09
!g_calc_coef_w: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.111685E-11
!g_calc_coef_w: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.111685E-13
                 
!a_calc_coef_w:    0.1116833000000E+07   0.1116870750000E+07

!g_calc_coef_w: ALPHA=.1000E+00  COEF=   0.8264462829075E+00  VAL_N= 0.923009E+06  VAL_L= 0.111684E+07
!g_calc_coef_w: ALPHA=.1000E-01  COEF=   0.9802960496253E+00  VAL_N= 0.109484E+05  VAL_L= 0.111684E+05
!g_calc_coef_w: ALPHA=.1000E-02  COEF=   0.9980029960275E+00  VAL_N= 0.111461E+03  VAL_L= 0.111684E+03
!g_calc_coef_w: ALPHA=.1000E-03  COEF=   0.9998000300046E+00  VAL_N= 0.111662E+01  VAL_L= 0.111684E+01
!g_calc_coef_w: ALPHA=.1000E-04  COEF=   0.9999800002875E+00  VAL_N= 0.111682E-01  VAL_L= 0.111684E-01
!g_calc_coef_w: ALPHA=.1000E-05  COEF=   0.9999979995651E+00  VAL_N= 0.111684E-03  VAL_L= 0.111684E-03
!g_calc_coef_w: ALPHA=.1000E-06  COEF=   0.9999998004147E+00  VAL_N= 0.111684E-05  VAL_L= 0.111684E-05
!g_calc_coef_w: ALPHA=.1000E-07  COEF=   0.1000000012124E+01  VAL_N= 0.111684E-07  VAL_L= 0.111684E-07
!g_calc_coef_w: ALPHA=.1000E-08  COEF=   0.1000001026566E+01  VAL_N= 0.111684E-09  VAL_L= 0.111684E-09
!g_calc_coef_w: ALPHA=.1000E-09  COEF=   0.1000007201550E+01  VAL_N= 0.111685E-11  VAL_L= 0.111684E-11
!g_calc_coef_w: ALPHA=.1000E-10  COEF=   0.9999895664473E+00  VAL_N= 0.111683E-13  VAL_L= 0.111684E-13
                 
!a_calc_coef_w:    0.1116841252269E+07   0.1116841252269E+07

END SUBROUTINE t_calc_coef_w
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_advance_uv ( u, ru_tend, v, rv_tend,        &
                        p, pb,                         &
                        ph, php, alt, al, mu,          &
                        muu, cqu, muv, cqv, mudf,      &
                        rdx, rdy, dts,                 &
                        cf1, cf2, cf3, fnm, fnp,       &
                        emdiv,                         &
                        rdnw, config_flags, spec_zone, &
                        non_hydrostatic,               &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

! Zaizhong Ma, March 30,2005

      IMPLICIT NONE  ! religion first

! stuff coming in

      TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

      LOGICAL, INTENT(IN   ) :: non_hydrostatic

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  &
            INTENT(INOUT) ::                          &
                                                  u,  &
                                                  v

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT( IN) :: pb
      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: &
                                             ru_tend, &
                                             rv_tend, &
                                             ph,      &
                                             php,     &
                                             p,       &
                                             alt,     &
                                             al,      &
                                             cqu,     &
                                             cqv


      REAL, DIMENSION( ims:ime , jms:jme )                   :: muu,  &
                                                                muv,  &
                                                                mu,   &
                                                                mudf


      REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                                fnp ,   &
                                                                rdnw

      REAL,                                    INTENT(IN   ) :: rdx,    &
                                                                rdy,    &
                                                                dts,    &
                                            cf1,    &
                                            cf2,    &
                                        cf3,    &
                                      emdiv


!  Local 3d array from the stack (note tile size)

      REAL, DIMENSION (its:ite, kts:kte) :: dpn, dpxy
      REAL, DIMENSION (its:ite) :: mudf_xy
      REAL                      :: dx, dy

      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
      INTEGER :: i_endu, j_endv, k_endw
      INTEGER :: i_start_up, i_end_up, j_start_up, j_end_up
      INTEGER :: i_start_vp, i_end_vp, j_start_vp, j_end_vp
      INTEGER :: i_start_u_tend, i_end_u_tend, j_start_v_tend, j_end_v_tend

!  zzma: new definition

!  IN variables

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  :: S_ru_tend, &
                                                        S_rv_tend, &
                                                        S_ph,      &
                                                        S_php,     &
                                                        S_p,       &
                                                        S_alt,     &
                                                        S_al,      &
                                                        S_cqu,     &
                                                        S_cqv

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  :: P_ru_tend, &
                                                        P_rv_tend, &
                                                        P_ph,      &
                                                        P_php,     &
                                                        P_p,       &
                                                        P_alt,     &
                                                        P_al,      &
                                                        P_cqu,     &
                                                        P_cqv

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  :: B_ru_tend, &
                                                        B_rv_tend, &
                                                        B_ph,      &
                                                        B_php,     &
                                                        B_p,       &
                                                        B_alt,     &
                                                        B_al,      &
                                                        B_cqu,     &
                                                        B_cqv

      REAL, DIMENSION( ims:ime , jms:jme )           :: S_muu,  &
                                                        S_muv,  &
                                                        S_mu,   &
                                                        S_mudf
      REAL, DIMENSION( ims:ime , jms:jme )           :: P_muu,  &
                                                        P_muv,  &
                                                        P_mu,   &
                                                        P_mudf
      REAL, DIMENSION( ims:ime , jms:jme )           :: B_muu,  &
                                                        B_muv,  &
                                                        B_mu,   &
                                                        B_mudf


!  INOUT variables

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  :: S_u, S_v,P_u, P_v,K_u, K_v,B_u, B_v

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru_tend(i,k,j)=ru_tend(i,k,j)
      S_rv_tend(i,k,j)=rv_tend(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)
      S_php(i,k,j)=php(i,k,j)
      S_p(i,k,j)=p(i,k,j)
      S_alt(i,k,j)=alt(i,k,j)
      S_al(i,k,j)=al(i,k,j)
      S_cqu(i,k,j)=cqu(i,k,j)
      S_cqv(i,k,j)=cqv(i,k,j)

      P_ru_tend(i,k,j)=ru_tend(i,k,j)
      P_rv_tend(i,k,j)=rv_tend(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
      P_php(i,k,j)=php(i,k,j)
      P_p(i,k,j)=p(i,k,j)
      P_alt(i,k,j)=alt(i,k,j)
      P_al(i,k,j)=al(i,k,j)
      P_cqu(i,k,j)=cqu(i,k,j)
      P_cqv(i,k,j)=cqv(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)
      S_mu(i,j)=mu(i,j)
      S_mudf(i,j)=mudf(i,j)

      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)
      P_mu(i,j)=mu(i,j)
      P_mudf(i,j)=mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)

      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)

      K_u(i,k,j)=u(i,k,j)
      K_v(i,k,j)=v(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL advance_uv ( u, ru_tend, v, rv_tend,        &
                        p, pb,                         &
                        ph, php, alt, al, mu,          &
                        muu, cqu, muv, cqv, mudf,      &
                        rdx, rdy, dts,                 &
                        cf1, cf2, cf3, fnm, fnp,       &
                        emdiv,                         &
                        rdnw, config_flags, spec_zone, &
                        non_hydrostatic,               &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_u(i,k,j)=u(i,k,j)
      B_v(i,k,j)=v(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_advance_uv( K_u, P_u, ru_tend, P_ru_tend, K_v, P_v, rv_tend, P_rv_tend, p, P_p, pb, ph, P_ph, php, P_php, alt, P_alt, al, &
&P_al, mu, P_mu, muu, P_muu, cqu, P_cqu, muv, P_muv, cqv, P_cqv, mudf, P_mudf, rdx, rdy, dts, cf1, cf2, cf3, fnm, fnp, emdiv, rdnw,&
& config_flags, spec_zone, non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_u(i,k,j)*P_u(i,k,j)    &
                    + P_v(i,k,j)*P_v(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_tend(i,k,j)=FACTOR*S_ru_tend(i,k,j)
      P_rv_tend(i,k,j)=FACTOR*S_rv_tend(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_php(i,k,j)=FACTOR*S_php(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_cqu(i,k,j)=FACTOR*S_cqu(i,k,j)
      P_cqv(i,k,j)=FACTOR*S_cqv(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mudf(i,j)=FACTOR*S_mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
   enddo
   enddo
   enddo

   CALL advance_uv ( P_u, P_ru_tend, P_v, P_rv_tend,        &
                        P_p, pb,                         &
                        P_ph, P_php, P_alt, P_al, P_mu,          &
                        P_muu, P_cqu, P_muv, P_cqv, P_mudf,      &
                        rdx, rdy, dts,                 &
                        cf1, cf2, cf3, fnm, fnp,       &
                        emdiv,                         &
                        rdnw, config_flags, spec_zone, &
                        non_hydrostatic,               &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_u(i,k,j)- B_u(i,k,j))*(P_u(i,k,j)- B_u(i,k,j))       &
                    +(P_v(i,k,j)- B_v(i,k,j))*(P_v(i,k,j)- B_v(i,k,j)) 
      enddo
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_advance_uv: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tend(i,k,j)=S_ru_tend(i,k,j)
      rv_tend(i,k,j)=S_rv_tend(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      php(i,k,j)=S_php(i,k,j)
      p(i,k,j)=S_p(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      al(i,k,j)=S_al(i,k,j)
      cqu(i,k,j)=S_cqu(i,k,j)
      cqv(i,k,j)=S_cqv(i,k,j)

      P_ru_tend(i,k,j)=FACTOR*S_ru_tend(i,k,j)
      P_rv_tend(i,k,j)=FACTOR*S_rv_tend(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      P_php(i,k,j)=FACTOR*S_php(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_cqu(i,k,j)=FACTOR*S_cqu(i,k,j)
      P_cqv(i,k,j)=FACTOR*S_cqv(i,k,j)

      B_ru_tend(i,k,j)=P_ru_tend(i,k,j)
      B_rv_tend(i,k,j)=P_rv_tend(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
      B_php(i,k,j)=P_php(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
      B_alt(i,k,j)=P_alt(i,k,j)
      B_al(i,k,j)=P_al(i,k,j)
      B_cqu(i,k,j)=P_cqu(i,k,j)
      B_cqv(i,k,j)=P_cqv(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
      mu(i,j)=S_mu(i,j)
      mudf(i,j)=S_mudf(i,j)

      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_mudf(i,j)=FACTOR*S_mudf(i,j)

      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)
      B_mu(i,j)=P_mu(i,j)
      B_mudf(i,j)=P_mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)

      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)

      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_advance_uv( u, P_u, ru_tend, P_ru_tend, v, P_v, rv_tend, P_rv_tend, p, P_p, pb, ph, P_ph, php, P_php, alt, P_alt, al, &
&P_al, mu, P_mu, muu, P_muu, cqu, P_cqu, muv, P_muv, cqv, P_cqv, mudf, P_mudf, rdx, rdy, dts, cf1, cf2, cf3, fnm, fnp, emdiv, rdnw,&
& config_flags, spec_zone, non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L +P_u(i,k,j)*P_u(i,k,j)    &
                    + P_v(i,k,j)*P_v(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_tend(i,k,j)=0.0
      P_rv_tend(i,k,j)=0.0
      P_ph(i,k,j)=0.0
      P_php(i,k,j)=0.0
      P_p(i,k,j)=0.0
      P_alt(i,k,j)=0.0
      P_al(i,k,j)=0.0
      P_cqu(i,k,j)=0.0
      P_cqv(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_muu(i,j)=0.0
      P_muv(i,j)=0.0
      P_mu(i,j)=0.0
      P_mudf(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_advance_uv( P_u, P_ru_tend, P_v, P_rv_tend, p, P_p, pb, ph, P_ph, php, P_php, alt, P_alt, al, P_al, mu, P_mu, muu, &
&P_muu, cqu, P_cqu, muv, P_muv, cqv, P_cqv, P_mudf, rdx, rdy, dts, cf1, cf2, cf3, fnm, fnp, emdiv, rdnw, config_flags, spec_zone, &
&non_hydrostatic, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ru_tend(i,k,j)*B_ru_tend(i,k,j)  &
                  + P_rv_tend(i,k,j)*B_rv_tend(i,k,j)  &
                  + P_ph(i,k,j)*B_ph(i,k,j)            &
                  + P_php(i,k,j)*B_php(i,k,j)          &
                  + P_p(i,k,j)*B_p(i,k,j)              &
                  + P_alt(i,k,j)*B_alt(i,k,j)          &
                  + P_al(i,k,j)*B_al(i,k,j)            &
                  + P_cqu(i,k,j)*B_cqu(i,k,j)          &
                  + P_cqv(i,k,j)*B_cqv(i,k,j)          
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)      &
                  + P_mudf(i,j)*B_mudf(i,j)  &
                  + P_muu(i,j)*B_muu(i,j)    &
                  + P_muv(i,j)*B_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)    &
                  + P_v(i,k,j)*B_v(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_advance_uv: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_tend(i,k,j)=S_ru_tend(i,k,j)
      rv_tend(i,k,j)=S_rv_tend(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
      php(i,k,j)=S_php(i,k,j)
      p(i,k,j)=S_p(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
      al(i,k,j)=S_al(i,k,j)
      cqu(i,k,j)=S_cqu(i,k,j)
      cqv(i,k,j)=S_cqv(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
      mu(i,j)=S_mu(i,j)
      mudf(i,j)=S_mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
   enddo
   enddo
   enddo

!g_advance_uv: ALPHA=.1000E+00  COEF=   0.1000012874603E+01  VAL_N= 0.988534E+11  VAL_L= 0.988521E+11
!g_advance_uv: ALPHA=.1000E-01  COEF=   0.1000010371208E+01  VAL_N= 0.988531E+09  VAL_L= 0.988521E+09
!g_advance_uv: ALPHA=.1000E-02  COEF=   0.1000114560127E+01  VAL_N= 0.988634E+07  VAL_L= 0.988521E+07
!g_advance_uv: ALPHA=.1000E-03  COEF=   0.1000334739685E+01  VAL_N= 0.988852E+05  VAL_L= 0.988521E+05
!g_advance_uv: ALPHA=.1000E-04  COEF=   0.1000074028969E+01  VAL_N= 0.988594E+03  VAL_L= 0.988521E+03
!g_advance_uv: ALPHA=.1000E-05  COEF=   0.9045659899712E+00  VAL_N= 0.894183E+01  VAL_L= 0.988521E+01
!g_advance_uv: ALPHA=.1000E-06  COEF=   0.1374753713608E+01  VAL_N= 0.135897E+00  VAL_L= 0.988521E-01
!g_advance_uv: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.988521E-03
!g_advance_uv: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.988521E-05
!g_advance_uv: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.988521E-07
!g_advance_uv: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.988521E-09
                 
!a_advance_uv:    0.9885338009600E+11   0.9885322444800E+11

!g_advance_uv: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.988532E+11  VAL_L= 0.988532E+11
!g_advance_uv: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.988532E+09  VAL_L= 0.988532E+09
!g_advance_uv: ALPHA=.1000E-02  COEF=   0.9999999999998E+00  VAL_N= 0.988532E+07  VAL_L= 0.988532E+07
!g_advance_uv: ALPHA=.1000E-03  COEF=   0.9999999999998E+00  VAL_N= 0.988532E+05  VAL_L= 0.988532E+05
!g_advance_uv: ALPHA=.1000E-04  COEF=   0.1000000000013E+01  VAL_N= 0.988532E+03  VAL_L= 0.988532E+03
!g_advance_uv: ALPHA=.1000E-05  COEF=   0.9999999998322E+00  VAL_N= 0.988532E+01  VAL_L= 0.988532E+01
!g_advance_uv: ALPHA=.1000E-06  COEF=   0.1000000001109E+01  VAL_N= 0.988532E-01  VAL_L= 0.988532E-01
!g_advance_uv: ALPHA=.1000E-07  COEF=   0.9999999865495E+00  VAL_N= 0.988532E-03  VAL_L= 0.988532E-03
!g_advance_uv: ALPHA=.1000E-08  COEF=   0.1000000149405E+01  VAL_N= 0.988532E-05  VAL_L= 0.988532E-05
!g_advance_uv: ALPHA=.1000E-09  COEF=   0.1000000029187E+01  VAL_N= 0.988532E-07  VAL_L= 0.988532E-07
!g_advance_uv: ALPHA=.1000E-10  COEF=   0.9999987059852E+00  VAL_N= 0.988531E-09  VAL_L= 0.988532E-09
                 
!a_advance_uv:    0.9885322606433E+11   0.9885322606433E+11


END SUBROUTINE t_advance_uv
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_advance_mu_t( ww, ww_1, u, u_1, v, v_1,            &
                         mu, mut, muave, muts, muu, muv,      &
                         mudf, uam, vam, wwam, t, t_1,        &
                         t_ave, ft, mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfu, msfv, msft,                    &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )

! Zaizhong Ma, March 31,2005

  IMPLICIT NONE  ! religion first

! stuff coming in

  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) ::  &
                                              u,   &
                                              v,   &
                                              u_1, &
                                              v_1, &
                                              t_1, &
                                              ft

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),      &
            INTENT(INOUT) ::                          &
                                              ww,     &
                                              ww_1,   &
                                              t,      &
                                              t_ave,  &
                                              uam,    &
                                              vam,    &
                                              wwam

  REAL, DIMENSION( ims:ime , jms:jme )                   :: muu,  &
                                                            muv,  &
                                                            mut,  &
                                                            mu_tend

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msfu, &
                                                            msfv, &
                                                            msft

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(  OUT) :: muave, &
                                                            muts,  &
                                                            mudf

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: mu

  REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                            fnp,    &
                                                            dnw,    &
                                                            rdnw

  REAL,                                    INTENT(IN   ) :: rdx,    &
                                                            rdy,    &
                                                            dts,    &
                                                            epssm

!  Local 3d array from the stack (note tile size)

  REAL, DIMENSION (its:ite, kts:kte) :: wdtn, dvdxi
  REAL, DIMENSION (its:ite) :: dmdt

  INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  REAL    :: acc

!  zzma: new definition

!  IN variables

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: &
                                              S_u,   &
                                              S_v,   &
                                              S_u_1, &
                                              S_v_1, &
                                              S_t_1, &
                                              S_ft , &
                                              P_u,   &
                                              P_v,   &
                                              P_u_1, &
                                              P_v_1, &
                                              P_t_1, &
                                              P_ft , &
                                              B_u,   &
                                              B_v,   &
                                              B_u_1, &
                                              B_v_1, &
                                              B_t_1, &
                                              B_ft

  REAL, DIMENSION( ims:ime , jms:jme )                   :: S_muu,  &
                                                            S_muv,  &
                                                            S_mut,  &
                                                            S_mu_tend
  REAL, DIMENSION( ims:ime , jms:jme )                   :: P_muu,  &
                                                            P_muv,  &
                                                            P_mut,  &
                                                            P_mu_tend
  REAL, DIMENSION( ims:ime , jms:jme )                   :: B_muu,  &
                                                            B_muv,  &
                                                            B_mut,  &
                                                            B_mu_tend
! INOUT variables

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) ::    &
                                              S_ww,     &
                                              S_ww_1,   &
                                              S_t,      &
                                              S_t_ave,  &
                                              P_ww,     &
                                              P_ww_1,   &
                                              P_t,      &
                                              P_t_ave,  &
                                              B_ww,     &
                                              B_ww_1,   &
                                              B_t,      &
                                              B_t_ave,  &
                                              K_ww,     &
                                              K_ww_1,   &
                                              K_t,      &
                                              K_t_ave

  REAL, DIMENSION( ims:ime , jms:jme )     :: S_mu,P_mu,K_mu,B_mu

! OUT variables

  REAL, DIMENSION( ims:ime , jms:jme ) :: P_muave, P_muts, P_mudf,B_muave, B_muts, B_mudf


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)
      S_u_1(i,k,j)=u_1(i,k,j)
      S_v_1(i,k,j)=v_1(i,k,j)
      S_t_1(i,k,j)=t_1(i,k,j)
      S_ft(i,k,j)=ft(i,k,j)

      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)
      P_u_1(i,k,j)=u_1(i,k,j)
      P_v_1(i,k,j)=v_1(i,k,j)
      P_t_1(i,k,j)=t_1(i,k,j)
      P_ft(i,k,j)=ft(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu_tend(i,j)=mu_tend(i,j)
      S_mut(i,j)=mut(i,j)
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)

      P_mu_tend(i,j)=mu_tend(i,j)
      P_mut(i,j)=mut(i,j)
      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ww(i,k,j)=ww(i,k,j)
      S_ww_1(i,k,j)=ww_1(i,k,j)
      S_t(i,k,j)=t(i,k,j)
      S_t_ave(i,k,j)=t_ave(i,k,j)

      P_ww(i,k,j)=ww(i,k,j)
      P_ww_1(i,k,j)=ww_1(i,k,j)
      P_t(i,k,j)=t(i,k,j)
      P_t_ave(i,k,j)=t_ave(i,k,j)

      K_ww(i,k,j)=ww(i,k,j)
      K_ww_1(i,k,j)=ww_1(i,k,j)
      K_t(i,k,j)=t(i,k,j)
      K_t_ave(i,k,j)=t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      P_mu(i,j)=mu(i,j)
      K_mu(i,j)=mu(i,j)
   enddo
   enddo


!NLM

   CALL advance_mu_t( ww, ww_1, u, u_1, v, v_1,            &
                         mu, mut, muave, muts, muu, muv,      &
                         mudf, uam, vam, wwam, t, t_1,        &
                         t_ave, ft, mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfu, msfv, msft,                    &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )

   do i=ims,ime
   do j=jms,jme
      B_muave(i,j)=muave(i,j)
      B_muts(i,j)=muts(i,j)
      B_mudf(i,j)=mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ww(i,k,j)=ww(i,k,j)
      B_ww_1(i,k,j)=ww_1(i,k,j)
      B_t(i,k,j)=t(i,k,j)
      B_t_ave(i,k,j)=t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      B_mu(i,j)=mu(i,j)
   enddo
   enddo

!  TCL

   CALL g_advance_mu_t( K_ww, P_ww, K_ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, K_mu, P_mu, mut, P_mut, muave, P_muave, &
&muts, P_muts, muu, P_muu, muv, P_muv, mudf, P_mudf, K_t, P_t, t_1, P_t_1, K_t_ave, P_t_ave, ft, P_ft, mu_tend, P_mu_tend, rdx, rdy, &
&dts, epssm, dnw, fnm, fnp, rdnw, msfu, msfv, msft, config_flags, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_muave(i,j)*P_muave(i,j)       &
                    + P_muts(i,j)*P_muts(i,j)         &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_ww(i,k,j)*P_ww(i,k,j)         &
                    + P_ww_1(i,k,j)*P_ww_1(i,k,j)     &
                    + P_t(i,k,j)*P_t(i,k,j)           &
                    + P_t_ave(i,k,j)*P_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_mu(i,j)*P_mu(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_u_1(i,k,j)=FACTOR*S_u_1(i,k,j)
      P_v_1(i,k,j)=FACTOR*S_v_1(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_ft(i,k,j)=FACTOR*S_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_ww_1(i,k,j)=FACTOR*S_ww_1(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_t_ave(i,k,j)=FACTOR*S_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
   enddo
   enddo

   CALL advance_mu_t( P_ww, P_ww_1, P_u, P_u_1, P_v, P_v_1,            &
                         P_mu, P_mut, P_muave, P_muts, P_muu, P_muv,      &
                         P_mudf, uam, vam, wwam, P_t, P_t_1,        &
                         P_t_ave, P_ft, P_mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfu, msfv, msft,                    &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )

      VAL_N=0.

   do i=ims,ime
   do j=jms,jme
      VAL_N=VAL_N + (P_muave(i,j) -B_muave(i,j))*(P_muave(i,j) -B_muave(i,j))    &
                  + (P_muts(i,j) -B_muts(i,j))*(P_muts(i,j) -B_muts(i,j))        &
                  + (P_mudf(i,j) -B_mudf(i,j))*(P_mudf(i,j) -B_mudf(i,j))
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N + (P_ww(i,k,j) -B_ww(i,k,j))*(P_ww(i,k,j) -B_ww(i,k,j))            &
                  + (P_ww_1(i,k,j) -B_ww_1(i,k,j))*(P_ww_1(i,k,j) -B_ww_1(i,k,j))    &
                  + (P_t(i,k,j) -B_t(i,k,j))*(P_t(i,k,j) -B_t(i,k,j))                &
                  + (P_t_ave(i,k,j) -B_t_ave(i,k,j))*(P_t_ave(i,k,j) -B_t_ave(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_N=VAL_N + (P_mu(i,j) -B_mu(i,j))*(P_mu(i,j) -B_mu(i,j))
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_advance_mu_t: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      u_1(i,k,j)=S_u_1(i,k,j)
      v_1(i,k,j)=S_v_1(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      ft(i,k,j)=S_ft(i,k,j)

      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_u_1(i,k,j)=FACTOR*S_u_1(i,k,j)
      P_v_1(i,k,j)=FACTOR*S_v_1(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_ft(i,k,j)=FACTOR*S_ft(i,k,j)

      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
      B_u_1(i,k,j)=P_u_1(i,k,j)
      B_v_1(i,k,j)=P_v_1(i,k,j)
      B_t_1(i,k,j)=P_t_1(i,k,j)
      B_ft(i,k,j)=P_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)

      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)

      B_mu_tend(i,j)=P_mu_tend(i,j)
      B_mut(i,j)=P_mut(i,j)
      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ww(i,k,j)=S_ww(i,k,j)
      ww_1(i,k,j)=S_ww_1(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      t_ave(i,k,j)=S_t_ave(i,k,j)

      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_ww_1(i,k,j)=FACTOR*S_ww_1(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      P_t_ave(i,k,j)=FACTOR*S_t_ave(i,k,j)

      B_ww(i,k,j)=P_ww(i,k,j)
      B_ww_1(i,k,j)=P_ww_1(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
      B_t_ave(i,k,j)=P_t_ave(i,k,j)

      K_ww(i,k,j)=ww(i,k,j)
      K_ww_1(i,k,j)=ww_1(i,k,j)
      K_t(i,k,j)=t(i,k,j)
      K_t_ave(i,k,j)=t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      P_mu(i,j)=FACTOR*S_mu(i,j)
      B_mu(i,j)=P_mu(i,j)
      K_mu(i,j)=mu(i,j)
   enddo
   enddo

!  TGL

   CALL g_advance_mu_t( ww, P_ww, ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, mu, P_mu, mut, P_mut, muave, P_muave, &
&muts, P_muts, muu, P_muu, muv, P_muv, mudf, P_mudf, t, P_t, t_1, P_t_1, t_ave, P_t_ave, ft, P_ft, mu_tend, P_mu_tend, rdx, rdy, &
&dts, epssm, dnw, fnm, fnp, rdnw, msfu, msfv, msft, config_flags, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L +  P_muave(i,j)*P_muave(i,j)       &
                    + P_muts(i,j)*P_muts(i,j)         &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_ww(i,k,j)*P_ww(i,k,j)         &
                    + P_ww_1(i,k,j)*P_ww_1(i,k,j)     &
                    + P_t(i,k,j)*P_t(i,k,j)           &
                    + P_t_ave(i,k,j)*P_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L + P_mu(i,j)*P_mu(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=0.0
      P_v(i,k,j)=0.0
      P_u_1(i,k,j)=0.0
      P_v_1(i,k,j)=0.0
      P_t_1(i,k,j)=0.0
      P_ft(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_tend(i,j)=0.0
      P_mut(i,j)=0.0
      P_muu(i,j)=0.0
      P_muv(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_advance_mu_t( K_ww, P_ww, K_ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, P_mu, P_mut, P_muave, P_muts, muu, &
&P_muu, muv, P_muv, P_mudf, P_t, t_1, P_t_1, P_t_ave, P_ft, mu_tend, P_mu_tend, rdx, rdy, dts, epssm, dnw, fnm, fnp, rdnw, msfu, &
&msfv, msft, config_flags, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)            &
                  + P_v(i,k,j)*B_v(i,k,j)            &
                  + P_u_1(i,k,j)*B_u_1(i,k,j)        &
                  + P_v_1(i,k,j)*B_v_1(i,k,j)        &
                  + P_t_1(i,k,j)*B_t_1(i,k,j)        &
                  + P_ft(i,k,j)*B_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j)  &
                  + P_mut(i,j)*B_mut(i,j)          &
                  + P_muu(i,j)*B_muu(i,j)          &
                  + P_muv(i,j)*B_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ww(i,k,j)*B_ww(i,k,j)        &
                  + P_ww_1(i,k,j)*B_ww_1(i,k,j)    &
                  + P_t(i,k,j)*B_t(i,k,j)          &
                  + P_t_ave(i,k,j)*B_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_advance_mu_t: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      u_1(i,k,j)=S_u_1(i,k,j)
      v_1(i,k,j)=S_v_1(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      ft(i,k,j)=S_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
      mut(i,j)=S_mut(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ww(i,k,j)=S_ww(i,k,j)
      ww_1(i,k,j)=S_ww_1(i,k,j)
      t(i,k,j)=S_t(i,k,j)
      t_ave(i,k,j)=S_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

!g_advance_mu_t: ALPHA=.1000E+00  COEF=   0.1077146768570E+01  VAL_N= 0.644103E+12  VAL_L= 0.597971E+12
!g_advance_mu_t: ALPHA=.1000E-01  COEF=   0.1007574081421E+01  VAL_N= 0.602500E+10  VAL_L= 0.597971E+10
!g_advance_mu_t: ALPHA=.1000E-02  COEF=   0.1000920295715E+01  VAL_N= 0.598521E+08  VAL_L= 0.597971E+08
!g_advance_mu_t: ALPHA=.1000E-03  COEF=   0.1005400180817E+01  VAL_N= 0.601200E+06  VAL_L= 0.597971E+06
!g_advance_mu_t: ALPHA=.1000E-04  COEF=   0.1377802491188E+01  VAL_N= 0.823886E+04  VAL_L= 0.597971E+04
!g_advance_mu_t: ALPHA=.1000E-05  COEF=   0.1535273933411E+02  VAL_N= 0.918050E+03  VAL_L= 0.597971E+02
!g_advance_mu_t: ALPHA=.1000E-06  COEF=   0.4109736633301E+03  VAL_N= 0.245750E+03  VAL_L= 0.597971E+00
!g_advance_mu_t: ALPHA=.1000E-07  COEF=   0.3229349176482E-16  VAL_N= 0.193106E-18  VAL_L= 0.597971E-02
!g_advance_mu_t: ALPHA=.1000E-08  COEF=   0.3229349110307E-14  VAL_N= 0.193106E-18  VAL_L= 0.597971E-04
!g_advance_mu_t: ALPHA=.1000E-09  COEF=   0.3229348754553E-12  VAL_N= 0.193106E-18  VAL_L= 0.597971E-06
!g_advance_mu_t: ALPHA=.1000E-10  COEF=   0.3229348385925E-10  VAL_N= 0.193106E-18  VAL_L= 0.597971E-08
                 
!a_advance_mu_t:    0.5979716976640E+12   0.5979532820480E+12


END SUBROUTINE t_advance_mu_t
!-----------------------------------------------------------------------------------------------

SUBROUTINE t_sumflux ( ru, rv, ww,                             &
                     u_lin, v_lin, ww_lin,                   &
                     muu, muv,                               &
                     ru_m, rv_m, ww_m, epssm,                &
                     msfu, msfv,                             &
                     iteration , number_of_small_timesteps,  &
                     ids,ide, jds,jde, kds,kde,              &
                     ims,ime, jms,jme, kms,kme,              &
                     its,ite, jts,jte, kts,kte              )


  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: number_of_small_timesteps
  INTEGER,      INTENT(IN   )    :: iteration
  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                 :: ru, &
                                                                rv, &
                                                                ww, &
                                                                u_lin,  &
                                                                v_lin,  &
                                                                ww_lin


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) , INTENT(INOUT) :: ru_m, &
                                                                rv_m, &
                                                                ww_m
  REAL, DIMENSION(ims:ime, jms:jme) , INTENT(IN   ) ::  msfu, msfv
  REAL, DIMENSION(ims:ime, jms:jme)   :: muu, muv

  REAL, INTENT(IN   )  ::  epssm
  INTEGER   :: i,j,k

!  zzma: new definition

!  IN variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                 :: S_ru, &
                                                                S_rv, &
                                                                S_ww, &
                                                                S_u_lin,  &
                                                                S_v_lin,  &
                                                                S_ww_lin
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                 :: P_ru, &
                                                                P_rv, &
                                                                P_ww, &
                                                                P_u_lin,  &
                                                                P_v_lin,  &
                                                                P_ww_lin
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)                 :: B_ru, &
                                                                B_rv, &
                                                                B_ww, &
                                                                B_u_lin,  &
                                                                B_v_lin,  &
                                                                B_ww_lin

  REAL, DIMENSION(ims:ime, jms:jme)      :: S_muu, S_muv,P_muu, P_muv,B_muu, B_muv

!  INOUT variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)  :: S_ru_m, S_rv_m, S_ww_m,P_ru_m, P_rv_m, P_ww_m
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)  :: K_ru_m, K_rv_m, K_ww_m,B_ru_m, B_rv_m, B_ww_m

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru(i,k,j)=ru(i,k,j)
      S_rv(i,k,j)=rv(i,k,j)
      S_ww(i,k,j)=ww(i,k,j)
      S_u_lin(i,k,j)=u_lin(i,k,j)
      S_v_lin(i,k,j)=v_lin(i,k,j)
      S_ww_lin(i,k,j)=ww_lin(i,k,j)

      P_ru(i,k,j)=ru(i,k,j)
      P_rv(i,k,j)=rv(i,k,j)
      P_ww(i,k,j)=ww(i,k,j)
      P_u_lin(i,k,j)=u_lin(i,k,j)
      P_v_lin(i,k,j)=v_lin(i,k,j)
      P_ww_lin(i,k,j)=ww_lin(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)

      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru_m(i,k,j)=ru_m(i,k,j)
      S_rv_m(i,k,j)=rv_m(i,k,j)
      S_ww_m(i,k,j)=ww_m(i,k,j)

      P_ru_m(i,k,j)=ru_m(i,k,j)
      P_rv_m(i,k,j)=rv_m(i,k,j)
      P_ww_m(i,k,j)=ww_m(i,k,j)

      K_ru_m(i,k,j)=ru_m(i,k,j)
      K_rv_m(i,k,j)=rv_m(i,k,j)
      K_ww_m(i,k,j)=ww_m(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL sumflux ( ru, rv, ww,                             &
                     u_lin, v_lin, ww_lin,                   &
                     muu, muv,                               &
                     ru_m, rv_m, ww_m, epssm,                &
                     msfu, msfv,                             &
                     iteration , number_of_small_timesteps,  &
                     ids,ide, jds,jde, kds,kde,              &
                     ims,ime, jms,jme, kms,kme,              &
                     its,ite, jts,jte, kts,kte              )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ru_m(i,k,j)=ru_m(i,k,j)
      B_rv_m(i,k,j)=rv_m(i,k,j)
      B_ww_m(i,k,j)=ww_m(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_sumflux( ru, P_ru, rv, P_rv, ww, P_ww, u_lin, P_u_lin, v_lin, P_v_lin, ww_lin, P_ww_lin, muu, P_muu, muv, P_muv, K_ru_m,&
& P_ru_m, K_rv_m, P_rv_m, K_ww_m, P_ww_m, msfu, msfv, iteration, number_of_small_timesteps, ide, jde, kde, ims, ime, jms, jme, kms, &
&kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_ru_m(i,k,j)*P_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*P_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*P_ww_m(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u_lin(i,k,j)=FACTOR*S_u_lin(i,k,j)
      P_v_lin(i,k,j)=FACTOR*S_v_lin(i,k,j)
      P_ww_lin(i,k,j)=FACTOR*S_ww_lin(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru_m(i,k,j)=FACTOR*S_ru_m(i,k,j)
      P_rv_m(i,k,j)=FACTOR*S_rv_m(i,k,j)
      P_ww_m(i,k,j)=FACTOR*S_ww_m(i,k,j)
   enddo
   enddo
   enddo

   CALL sumflux ( P_ru, P_rv, P_ww,                             &
                     P_u_lin, P_v_lin, P_ww_lin,                   &
                     P_muu, P_muv,                               &
                     P_ru_m, P_rv_m, P_ww_m, epssm,                &
                     msfu, msfv,                             &
                     iteration , number_of_small_timesteps,  &
                     ids,ide, jds,jde, kds,kde,              &
                     ims,ime, jms,jme, kms,kme,              &
                     its,ite, jts,jte, kts,kte              )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N + (P_ru_m(i,k,j)-B_ru_m(i,k,j))*(P_ru_m(i,k,j)-B_ru_m(i,k,j))     &
                     + (P_rv_m(i,k,j)-B_rv_m(i,k,j))*(P_rv_m(i,k,j)-B_rv_m(i,k,j))     &
                     + (P_ww_m(i,k,j)-B_ww_m(i,k,j))*(P_ww_m(i,k,j)-B_ww_m(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_sumflux: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u_lin(i,k,j)=S_u_lin(i,k,j)
      v_lin(i,k,j)=S_v_lin(i,k,j)
      ww_lin(i,k,j)=S_ww_lin(i,k,j)

      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u_lin(i,k,j)=FACTOR*S_u_lin(i,k,j)
      P_v_lin(i,k,j)=FACTOR*S_v_lin(i,k,j)
      P_ww_lin(i,k,j)=FACTOR*S_ww_lin(i,k,j)

      B_ru(i,k,j)=P_ru(i,k,j)
      B_rv(i,k,j)=P_rv(i,k,j)
      B_ww(i,k,j)=P_ww(i,k,j)
      B_u_lin(i,k,j)=P_u_lin(i,k,j)
      B_v_lin(i,k,j)=P_v_lin(i,k,j)
      B_ww_lin(i,k,j)=P_ww_lin(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)

      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)

      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_m(i,k,j)=S_ru_m(i,k,j)
      rv_m(i,k,j)=S_rv_m(i,k,j)
      ww_m(i,k,j)=S_ww_m(i,k,j)

      P_ru_m(i,k,j)=FACTOR*S_ru_m(i,k,j)
      P_rv_m(i,k,j)=FACTOR*S_rv_m(i,k,j)
      P_ww_m(i,k,j)=FACTOR*S_ww_m(i,k,j)

      B_ru_m(i,k,j)=P_ru_m(i,k,j)
      B_rv_m(i,k,j)=P_rv_m(i,k,j)
      B_ww_m(i,k,j)=P_ww_m(i,k,j)

      K_ru_m(i,k,j)=ru_m(i,k,j)
      K_rv_m(i,k,j)=rv_m(i,k,j)
      K_ww_m(i,k,j)=ww_m(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_sumflux( ru, P_ru, rv, P_rv, ww, P_ww, u_lin, P_u_lin, v_lin, P_v_lin, ww_lin, P_ww_lin, muu, P_muu, muv, P_muv, ru_m,&
& P_ru_m, rv_m, P_rv_m, ww_m, P_ww_m, msfu, msfv, iteration, number_of_small_timesteps, ide, jde, kde, ims, ime, jms, jme, kms, &
&kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L   +P_ru_m(i,k,j)*P_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*P_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*P_ww_m(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=0.0
      P_rv(i,k,j)=0.0
      P_ww(i,k,j)=0.0
      P_u_lin(i,k,j)=0.0
      P_v_lin(i,k,j)=0.0
      P_ww_lin(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_muu(i,j)=0.0
      P_muv(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_sumflux( P_ru, P_rv, P_ww, u_lin, P_u_lin, v_lin, P_v_lin, P_ww_lin, muu, P_muu, muv, P_muv, P_ru_m, P_rv_m, P_ww_m, &
&msfu, msfv, iteration, number_of_small_timesteps, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A+P_ru(i,k,j)*B_ru(i,k,j)         &
               +P_rv(i,k,j)*B_rv(i,k,j)            &
               +P_ww(i,k,j)*B_ww(i,k,j)            &
               +P_u_lin(i,k,j)*B_u_lin(i,k,j)              &
               +P_v_lin(i,k,j)*B_v_lin(i,k,j)              &
               +P_ww_lin(i,k,j)*B_ww_lin(i,k,j)  
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_muu(i,j)*B_muu(i,j)    &
               +P_muv(i,j)*B_muv(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_ru_m(i,k,j)*B_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*B_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*B_ww_m(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_sumflux: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u_lin(i,k,j)=S_u_lin(i,k,j)
      v_lin(i,k,j)=S_v_lin(i,k,j)
      ww_lin(i,k,j)=S_ww_lin(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru_m(i,k,j)=S_ru_m(i,k,j)
      rv_m(i,k,j)=S_rv_m(i,k,j)
      ww_m(i,k,j)=S_ww_m(i,k,j)
   enddo
   enddo
   enddo

!g_sumflux: ALPHA=.1000E+00  COEF=   0.1102335453033E+01  VAL_N= 0.150645E+17  VAL_L= 0.136659E+17
!g_sumflux: ALPHA=.1000E-01  COEF=   0.1010020852089E+01  VAL_N= 0.138029E+15  VAL_L= 0.136659E+15
!g_sumflux: ALPHA=.1000E-02  COEF=   0.1001844882965E+01  VAL_N= 0.136912E+13  VAL_L= 0.136659E+13
!g_sumflux: ALPHA=.1000E-03  COEF=   0.1002220749855E+01  VAL_N= 0.136963E+11  VAL_L= 0.136659E+11
!g_sumflux: ALPHA=.1000E-04  COEF=   0.1009521842003E+01  VAL_N= 0.137961E+09  VAL_L= 0.136659E+09
!g_sumflux: ALPHA=.1000E-05  COEF=   0.9413729906082E+00  VAL_N= 0.128648E+07  VAL_L= 0.136659E+07
!g_sumflux: ALPHA=.1000E-06  COEF=   0.1890421867371E+01  VAL_N= 0.258344E+05  VAL_L= 0.136659E+05
!g_sumflux: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.136659E+03
!g_sumflux: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.136659E+01
!g_sumflux: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.136659E-01
!g_sumflux: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.136659E-03
                 
!a_sumflux:    0.1368172140062E+17   0.1367992825178E+17

!g_sumflux: ALPHA=.1000E+00  COEF=   0.1102499259156E+01  VAL_N= 0.150786E+17  VAL_L= 0.136768E+17
!g_sumflux: ALPHA=.1000E-01  COEF=   0.1010024927542E+01  VAL_N= 0.138139E+15  VAL_L= 0.136768E+15
!g_sumflux: ALPHA=.1000E-02  COEF=   0.1001000242770E+01  VAL_N= 0.136905E+13  VAL_L= 0.136768E+13
!g_sumflux: ALPHA=.1000E-03  COEF=   0.1000100001778E+01  VAL_N= 0.136782E+11  VAL_L= 0.136768E+11
!g_sumflux: ALPHA=.1000E-04  COEF=   0.1000009999971E+01  VAL_N= 0.136769E+09  VAL_L= 0.136768E+09
!g_sumflux: ALPHA=.1000E-05  COEF=   0.1000000999870E+01  VAL_N= 0.136768E+07  VAL_L= 0.136768E+07
!g_sumflux: ALPHA=.1000E-06  COEF=   0.1000000101149E+01  VAL_N= 0.136768E+05  VAL_L= 0.136768E+05
!g_sumflux: ALPHA=.1000E-07  COEF=   0.1000000009549E+01  VAL_N= 0.136768E+03  VAL_L= 0.136768E+03
!g_sumflux: ALPHA=.1000E-08  COEF=   0.1000000163077E+01  VAL_N= 0.136768E+01  VAL_L= 0.136768E+01
!g_sumflux: ALPHA=.1000E-09  COEF=   0.1000000280416E+01  VAL_N= 0.136768E-01  VAL_L= 0.136768E-01
!g_sumflux: ALPHA=.1000E-10  COEF=   0.1000000491168E+01  VAL_N= 0.136768E-03  VAL_L= 0.136768E-03
                 
!a_sumflux:    0.1367678888613E+17   0.1367678888613E+17

END SUBROUTINE t_sumflux
!-----------------------------------------------------------------------------------------------

SUBROUTINE t_advance_w( w, rw_tend, ww, u, v,       &
                      mu1, mut, muave, muts,      &
                      t_2ave, t_2, t_1,           &
                      ph, ph_1, phb, ph_tend,     &
                      ht, c2a, cqw, alt, alb,     &
                      a, alpha, gamma,            &
                      rdx, rdy, dts, t0, epssm,   &
                      dnw, fnm, fnp, rdnw, rdn,   &
                      cf1, cf2, cf3, msft,        &
                      config_flags,               &
                      ids,ide, jds,jde, kds,kde,  & ! domain dims
                      ims,ime, jms,jme, kms,kme,  & ! memory dims
                      its,ite, jts,jte, kts,kte  )  ! tile   dims

  IMPLICIT NONE ! religion first
  
! stuff coming in


  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), &
            INTENT(INOUT) ::                          &
                                             t_2ave,  &
                                             w,       &
                                             ph


      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), &
            INTENT(IN   ) ::                          &
                                             phb,     &
                                             alb
      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme )  :: &
                                             rw_tend, &
                                             ww,     &
                                             u,       &
                                             v,       &
                                             t_2,     &
                                             t_1,     &
                                             ph_1,    &
                                             ph_tend, &
                                             alpha,   &
                                             gamma,   &
                                             a,       &
                                             c2a,     &
                                             cqw,     &
                                             alt

      REAL, DIMENSION( ims:ime , jms:jme ), &
            INTENT(IN   )  ::               &
                                   ht,      &
                                   msft
      REAL, DIMENSION( ims:ime , jms:jme ) :: &
                                   mu1,     &
                                   mut,     &
                                   muave,   &
                                   muts

      REAL, DIMENSION( kms:kme ),  INTENT(IN   )  :: fnp,     &
                                                     fnm,     &
                                                     rdnw,    &
                                                     rdn,     &
                                                     dnw

      REAL,   INTENT(IN   )  :: rdx,     &
                                rdy,     &
                                dts,     &
                                cf1,     &
                                cf2,     &
                                cf3,     &
                                t0,      &
                                epssm

!  Stack based 3d data, tile size.

      REAL, DIMENSION( its:ite ) :: mut_inv, msft_inv
      REAL, DIMENSION( its:ite, kts:kte ) :: rhs, wdwn
      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end

!  zzma: new definition

!  IN variables

      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ) :: &
                                             S_rw_tend, &
                                             S_ww,     &
                                             S_u,       &
                                             S_v,       &
                                             S_t_2,     &
                                             S_t_1,     &
                                             S_ph_1,    &
                                             S_ph_tend, &
                                             S_alpha,   &
                                             S_gamma,   &
                                             S_a,       &
                                             S_c2a,     &
                                             S_cqw,     &
                                             S_alt
      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ) :: &
                                             P_rw_tend, &
                                             P_ww,     &
                                             P_u,       &
                                             P_v,       &
                                             P_t_2,     &
                                             P_t_1,     &
                                             P_ph_1,    &
                                             P_ph_tend, &
                                             P_alpha,   &
                                             P_gamma,   &
                                             P_a,       &
                                             P_c2a,     &
                                             P_cqw,     &
                                             P_alt
      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ) :: &
                                             B_rw_tend, &
                                             B_ww,     &
                                             B_u,       &
                                             B_v,       &
                                             B_t_2,     &
                                             B_t_1,     &
                                             B_ph_1,    &
                                             B_ph_tend, &
                                             B_alpha,   &
                                             B_gamma,   &
                                             B_a,       &
                                             B_c2a,     &
                                             B_cqw,     &
                                             B_alt

      REAL, DIMENSION( ims:ime , jms:jme ) :: S_mu1, S_mut, S_muave,S_muts,P_mu1, P_mut, P_muave,P_muts,  &
                                              B_mu1, B_mut, B_muave,B_muts

!  INOUT variables

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) :: S_t_2ave, S_w, S_ph,P_t_2ave, P_w, P_ph
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) :: K_t_2ave, K_w, K_ph,B_t_2ave, B_w, B_ph



   REAL :: SAVE_L, COEF, ALPHA_M, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_rw_tend(i,k,j)=rw_tend(i,k,j)
      S_ww(i,k,j)=ww(i,k,j)
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)
      S_t_1(i,k,j)=t_1(i,k,j)
      S_t_2(i,k,j)=t_2(i,k,j)
      S_ph_1(i,k,j)=ph_1(i,k,j)
      S_ph_tend(i,k,j)=ph_tend(i,k,j)
      S_alpha(i,k,j)=alpha(i,k,j)
      S_gamma(i,k,j)=gamma(i,k,j)
      S_a(i,k,j)=a(i,k,j)
      S_c2a(i,k,j)=c2a(i,k,j)
      S_cqw(i,k,j)=cqw(i,k,j)
      S_alt(i,k,j)=alt(i,k,j)

      P_rw_tend(i,k,j)=rw_tend(i,k,j)
      P_ww(i,k,j)=ww(i,k,j)
      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)
      P_t_1(i,k,j)=t_1(i,k,j)
      P_t_2(i,k,j)=t_2(i,k,j)
      P_ph_1(i,k,j)=ph_1(i,k,j)
      P_ph_tend(i,k,j)=ph_tend(i,k,j)
      P_alpha(i,k,j)=alpha(i,k,j)
      P_gamma(i,k,j)=gamma(i,k,j)
      P_a(i,k,j)=a(i,k,j)
      P_c2a(i,k,j)=c2a(i,k,j)
      P_cqw(i,k,j)=cqw(i,k,j)
      P_alt(i,k,j)=alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu1(i,j)=mu1(i,j)
      S_mut(i,j)=mut(i,j)
      S_muave(i,j)=muave(i,j)
      S_muts(i,j)=muts(i,j)

      P_mu1(i,j)=mu1(i,j)
      P_mut(i,j)=mut(i,j)
      P_muave(i,j)=muave(i,j)
      P_muts(i,j)=muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_t_2ave(i,k,j)=t_2ave(i,k,j)
      S_w(i,k,j)=w(i,k,j)
      S_ph(i,k,j)=ph(i,k,j)

      P_t_2ave(i,k,j)=t_2ave(i,k,j)
      P_w(i,k,j)=w(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)

      K_t_2ave(i,k,j)=t_2ave(i,k,j)
      K_w(i,k,j)=w(i,k,j)
      K_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL advance_w( w, rw_tend, ww, u, v,       &
                      mu1, mut, muave, muts,      &
                      t_2ave, t_2, t_1,           &
                      ph, ph_1, phb, ph_tend,     &
                      ht, c2a, cqw, alt, alb,     &
                      a, alpha, gamma,            &
                      rdx, rdy, dts, t0, epssm,   &
                      dnw, fnm, fnp, rdnw, rdn,   &
                      cf1, cf2, cf3, msft,        &
                      config_flags,               &
                      ids,ide, jds,jde, kds,kde,  & ! domain dims
                      ims,ime, jms,jme, kms,kme,  & ! memory dims
                      its,ite, jts,jte, kts,kte  )  ! tile   dims


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_t_2ave(i,k,j)=t_2ave(i,k,j)
      B_w(i,k,j)=w(i,k,j)
      B_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_advance_w( K_w, P_w, rw_tend, P_rw_tend, ww, P_ww, u, P_u, v, P_v, mu1, P_mu1, mut, P_mut, muave, P_muave, muts, P_muts,&
& K_t_2ave, P_t_2ave, t_2, P_t_2, t_1, P_t_1, K_ph, P_ph, ph_1, P_ph_1, phb, ph_tend, P_ph_tend, ht, c2a, P_c2a, cqw, P_cqw, alt, &
&P_alt, alb, a, P_a, alpha, P_alpha, gamma, P_gamma, rdx, rdy, dts, t0, epssm, fnm, fnp, rdnw, rdn, cf1, cf2, cf3, msft, &
&config_flags, ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_t_2ave(i,k,j)*P_t_2ave(i,k,j)   &
                    +P_w(i,k,j)*P_w(i,k,j)             &
                    +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

   ALPHA_M=1.
   DO NT=1,11
      ALPHA_M=0.1*ALPHA_M
      FACTOR=1.+ALPHA_M
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_rw_tend(i,k,j)=FACTOR*S_rw_tend(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_1(i,k,j)=FACTOR*S_ph_1(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)
      P_alpha(i,k,j)=FACTOR*S_alpha(i,k,j)
      P_gamma(i,k,j)=FACTOR*S_gamma(i,k,j)
      P_a(i,k,j)=FACTOR*S_a(i,k,j)
      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu1(i,j)=FACTOR*S_mu1(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muave(i,j)=FACTOR*S_muave(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_t_2ave(i,k,j)=FACTOR*S_t_2ave(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
   enddo
   enddo
   enddo

   CALL advance_w( P_w, P_rw_tend, P_ww, P_u, P_v,       &
                      P_mu1, P_mut, P_muave, P_muts,      &
                      P_t_2ave, P_t_2, P_t_1,           &
                      P_ph, P_ph_1, phb, P_ph_tend,     &
                      ht, P_c2a, P_cqw, P_alt, alb,     &
                      P_a, P_alpha, P_gamma,            &
                      rdx, rdy, dts, t0, epssm,   &
                      dnw, fnm, fnp, rdnw, rdn,   &
                      cf1, cf2, cf3, msft,        &
                      config_flags,               &
                      ids,ide, jds,jde, kds,kde,  & ! domain dims
                      ims,ime, jms,jme, kms,kme,  & ! memory dims
                      its,ite, jts,jte, kts,kte  )  ! tile   dims

      VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N+(P_t_2ave(i,k,j)-B_t_2ave(i,k,j))*(P_t_2ave(i,k,j)-B_t_2ave(i,k,j))   &
                 +(P_w(i,k,j)-B_w(i,k,j))*(P_w(i,k,j)-B_w(i,k,j))             &
                 +(P_ph(i,k,j)-B_ph(i,k,j))*(P_ph(i,k,j)-B_ph(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA_M**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_advance_w: ALPHA=',ALPHA_M,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      rw_tend(i,k,j)=S_rw_tend(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_1(i,k,j)=S_ph_1(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)
      alpha(i,k,j)=S_alpha(i,k,j)
      gamma(i,k,j)=S_gamma(i,k,j)
      a(i,k,j)=S_a(i,k,j)
      c2a(i,k,j)=S_c2a(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)

      P_rw_tend(i,k,j)=FACTOR*S_rw_tend(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_t_1(i,k,j)=FACTOR*S_t_1(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_1(i,k,j)=FACTOR*S_ph_1(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)
      P_alpha(i,k,j)=FACTOR*S_alpha(i,k,j)
      P_gamma(i,k,j)=FACTOR*S_gamma(i,k,j)
      P_a(i,k,j)=FACTOR*S_a(i,k,j)
      P_c2a(i,k,j)=FACTOR*S_c2a(i,k,j)
      P_cqw(i,k,j)=FACTOR*S_cqw(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)

      B_rw_tend(i,k,j)=P_rw_tend(i,k,j)
      B_ww(i,k,j)=P_ww(i,k,j)
      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
      B_t_1(i,k,j)=P_t_1(i,k,j)
      B_t_2(i,k,j)=P_t_2(i,k,j)
      B_ph_1(i,k,j)=P_ph_1(i,k,j)
      B_ph_tend(i,k,j)=P_ph_tend(i,k,j)
      B_alpha(i,k,j)=P_alpha(i,k,j)
      B_gamma(i,k,j)=P_gamma(i,k,j)
      B_a(i,k,j)=P_a(i,k,j)
      B_c2a(i,k,j)=P_c2a(i,k,j)
      B_cqw(i,k,j)=P_cqw(i,k,j)
      B_alt(i,k,j)=P_alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu1(i,j)=S_mu1(i,j)
      mut(i,j)=S_mut(i,j)
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)

      P_mu1(i,j)=FACTOR*S_mu1(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muave(i,j)=FACTOR*S_muave(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)

      B_mu1(i,j)=P_mu1(i,j)
      B_mut(i,j)=P_mut(i,j)
      B_muave(i,j)=P_muave(i,j)
      B_muts(i,j)=P_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      t_2ave(i,k,j)=S_t_2ave(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)

      P_t_2ave(i,k,j)=FACTOR*S_t_2ave(i,k,j)
      P_w(i,k,j)=FACTOR*S_w(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)

      B_t_2ave(i,k,j)=P_t_2ave(i,k,j)
      B_w(i,k,j)=P_w(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)

      K_t_2ave(i,k,j)=t_2ave(i,k,j)
      K_w(i,k,j)=w(i,k,j)
      K_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo


!  TGL

   CALL g_advance_w( w, P_w, rw_tend, P_rw_tend, ww, P_ww, u, P_u, v, P_v, mu1, P_mu1, mut, P_mut, muave, P_muave, muts, P_muts,&
& t_2ave, P_t_2ave, t_2, P_t_2, t_1, P_t_1, ph, P_ph, ph_1, P_ph_1, phb, ph_tend, P_ph_tend, ht, c2a, P_c2a, cqw, P_cqw, alt, &
&P_alt, alb, a, P_a, alpha, P_alpha, gamma, P_gamma, rdx, rdy, dts, t0, epssm, fnm, fnp, rdnw, rdn, cf1, cf2, cf3, msft, &
&config_flags, ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_t_2ave(i,k,j)*P_t_2ave(i,k,j)   &
                  + P_w(i,k,j)*P_w(i,k,j)             &
                  + P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_rw_tend(i,k,j)=0.0
      P_ww(i,k,j)=0.0
      P_u(i,k,j)=0.0
      P_v(i,k,j)=0.0
      P_t_1(i,k,j)=0.0
      P_t_2(i,k,j)=0.0
      P_ph_1(i,k,j)=0.0
      P_ph_tend(i,k,j)=0.0
      P_alpha(i,k,j)=0.0
      P_gamma(i,k,j)=0.0
      P_a(i,k,j)=0.0
      P_c2a(i,k,j)=0.0
      P_cqw(i,k,j)=0.0
      P_alt(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu1(i,j)=0.0
      P_mut(i,j)=0.0
      P_muave(i,j)=0.0
      P_muts(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL  a_advance_w( K_w, P_w, rw_tend, P_rw_tend, ww, P_ww, u, P_u, v, P_v, mu1, P_mu1, mut, P_mut, muave, P_muave, muts, P_muts,&
& K_t_2ave, P_t_2ave, t_2, P_t_2, t_1, P_t_1, K_ph, P_ph, ph_1, P_ph_1, phb, ph_tend, P_ph_tend, ht, c2a, P_c2a, cqw, P_cqw, alt, &
&P_alt, alb, a, P_a, alpha, P_alpha, gamma, P_gamma, rdx, rdy, dts, t0, epssm, fnm, fnp, rdnw, rdn, cf1, cf2, cf3, msft, &
&config_flags, ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_rw_tend(i,k,j)*B_rw_tend(i,k,j)      &
             + P_ww(i,k,j)*B_ww(i,k,j)                     &
             + P_u(i,k,j)*B_u(i,k,j)                       &
             + P_v(i,k,j)*B_v(i,k,j)                       &
             + P_t_1(i,k,j)*B_t_1(i,k,j)                   &
             + P_t_2(i,k,j)*B_t_2(i,k,j)                   &
             + P_ph_1(i,k,j)*B_ph_1(i,k,j)                 &
             + P_ph_tend(i,k,j)*B_ph_tend(i,k,j)           &
             + P_alpha(i,k,j)*B_alpha(i,k,j)               &
             + P_gamma(i,k,j)*B_gamma(i,k,j)               &
             + P_a(i,k,j)*B_a(i,k,j)                       &
             + P_c2a(i,k,j)*B_c2a(i,k,j)                   &
             + P_cqw(i,k,j)*B_cqw(i,k,j)                   &
             + P_alt(i,k,j)*B_alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu1(i,j)*B_mu1(i,j)                  &
             + P_mut(i,j)*B_mut(i,j)                       &
             + P_muave(i,j)*B_muave(i,j)                   &
             + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_t_2ave(i,k,j)*B_t_2ave(i,k,j)        &
             + P_w(i,k,j)*B_w(i,k,j)                       &
             + P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_advance_w: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      rw_tend(i,k,j)=S_rw_tend(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      t_1(i,k,j)=S_t_1(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_1(i,k,j)=S_ph_1(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)
      alpha(i,k,j)=S_alpha(i,k,j)
      gamma(i,k,j)=S_gamma(i,k,j)
      a(i,k,j)=S_a(i,k,j)
      c2a(i,k,j)=S_c2a(i,k,j)
      cqw(i,k,j)=S_cqw(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu1(i,j)=S_mu1(i,j)
      mut(i,j)=S_mut(i,j)
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      t_2ave(i,k,j)=S_t_2ave(i,k,j)
      w(i,k,j)=S_w(i,k,j)
      ph(i,k,j)=S_ph(i,k,j)
   enddo
   enddo
   enddo

!g_advance_w: ALPHA=.1000E+00  COEF=   0.4203023437500E+05  VAL_N= 0.447786E+18  VAL_L= 0.106539E+14
!g_advance_w: ALPHA=.1000E-01  COEF=   0.8862628340721E+00  VAL_N= 0.944215E+11  VAL_L= 0.106539E+12
!g_advance_w: ALPHA=.1000E-02  COEF=   0.9950740337372E+00  VAL_N= 0.106014E+10  VAL_L= 0.106539E+10
!g_advance_w: ALPHA=.1000E-03  COEF=   0.1000316143036E+01  VAL_N= 0.106573E+08  VAL_L= 0.106539E+08
!g_advance_w: ALPHA=.1000E-04  COEF=   0.1005530953407E+01  VAL_N= 0.107128E+06  VAL_L= 0.106539E+06
!g_advance_w: ALPHA=.1000E-05  COEF=   0.9066505432129E+00  VAL_N= 0.965936E+03  VAL_L= 0.106539E+04
!g_advance_w: ALPHA=.1000E-06  COEF=   0.1328220486641E+01  VAL_N= 0.141507E+02  VAL_L= 0.106539E+02
!g_advance_w: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.106539E+00
!g_advance_w: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.106539E-02
!g_advance_w: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.106539E-04
!g_advance_w: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.106539E-06
                 
!a_advance_w:    0.1065481876275E+14   0.2132781144474E+14

!g_advance_w: ALPHA=.1000E+00  COEF=   0.4202960279442E+05  VAL_N= 0.447787E+18  VAL_L= 0.106541E+14
!g_advance_w: ALPHA=.1000E-01  COEF=   0.8863898444207E+00  VAL_N= 0.944368E+11  VAL_L= 0.106541E+12
!g_advance_w: ALPHA=.1000E-02  COEF=   0.9950242040299E+00  VAL_N= 0.106011E+10  VAL_L= 0.106541E+10
!g_advance_w: ALPHA=.1000E-03  COEF=   0.9995589489485E+00  VAL_N= 0.106494E+08  VAL_L= 0.106541E+08
!g_advance_w: ALPHA=.1000E-04  COEF=   0.9999564508007E+00  VAL_N= 0.106536E+06  VAL_L= 0.106541E+06
!g_advance_w: ALPHA=.1000E-05  COEF=   0.9999956504520E+00  VAL_N= 0.106540E+04  VAL_L= 0.106541E+04
!g_advance_w: ALPHA=.1000E-06  COEF=   0.9999995659454E+00  VAL_N= 0.106541E+02  VAL_L= 0.106541E+02
!g_advance_w: ALPHA=.1000E-07  COEF=   0.9999999434296E+00  VAL_N= 0.106541E+00  VAL_L= 0.106541E+00
!g_advance_w: ALPHA=.1000E-08  COEF=   0.1000000115664E+01  VAL_N= 0.106541E-02  VAL_L= 0.106541E-02
!g_advance_w: ALPHA=.1000E-09  COEF=   0.1000000381680E+01  VAL_N= 0.106541E-04  VAL_L= 0.106541E-04
!g_advance_w: ALPHA=.1000E-10  COEF=   0.1000001224183E+01  VAL_N= 0.106541E-06  VAL_L= 0.106541E-06
                 
!a_advance_w:    0.1065409474212E+14   0.2136289875657E+14

END SUBROUTINE t_advance_w
!-----------------------------------------------------------------------------------------------

SUBROUTINE t_spec_bdyupdate_ph( ph_save, field,      &
                               field_tend, mu_tend, muts, dt,     &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

! Zaizhong Ma, April 1,2005

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )                :: field_tend, ph_save
      REAL,  DIMENSION( ims:ime , jms:jme )                          :: mu_tend, muts
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist

!     Local array

      REAL,  DIMENSION( its:ite , jts:jte ) :: mu_old

!  zzma: new definition

!  IN variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field_tend, S_ph_save,P_field_tend, P_ph_save,B_field_tend, B_ph_save
      REAL,  DIMENSION( ims:ime , jms:jme )             :: S_mu_tend, S_muts,P_mu_tend, P_muts,B_mu_tend, B_muts

!  INOUT variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: S_field,P_field,B_field,K_field

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_field_tend(i,k,j)=field_tend(i,k,j)
      S_ph_save(i,k,j)=ph_save(i,k,j)

      P_field_tend(i,k,j)=field_tend(i,k,j)
      P_ph_save(i,k,j)=ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu_tend(i,j)=mu_tend(i,j)
      S_muts(i,j)=muts(i,j)

      P_mu_tend(i,j)=mu_tend(i,j)
      P_muts(i,j)=muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_field(i,k,j)=field(i,k,j)
      P_field(i,k,j)=field(i,k,j)
      K_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL spec_bdyupdate_ph( ph_save, field,      &
                               field_tend, mu_tend, muts, dt,     &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_spec_bdyupdate_ph( ph_save, P_ph_save, K_field, P_field, field_tend, P_field_tend, mu_tend, P_mu_tend, muts, P_muts, dt,&
& variable_in, spec_zone, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_field_tend(i,k,j)=FACTOR*S_field_tend(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_field(i,k,j)=FACTOR*S_field(i,k,j)
   enddo
   enddo
   enddo

   CALL spec_bdyupdate_ph( P_ph_save, P_field,      &
                               P_field_tend, P_mu_tend, P_muts, dt,     &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_field(i,k,j) -B_field(i,k,j))*(P_field(i,k,j) -B_field(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_spec_bdyupdate_ph: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field_tend(i,k,j)=S_field_tend(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)

      P_field_tend(i,k,j)=FACTOR*S_field_tend(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)

      B_field_tend(i,k,j)=P_field_tend(i,k,j)
      B_ph_save(i,k,j)=P_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
      muts(i,j)=S_muts(i,j)

      P_mu_tend(i,j)=FACTOR*S_mu_tend(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)

      B_mu_tend(i,j)=P_mu_tend(i,j)
      B_muts(i,j)=P_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field(i,k,j)=S_field(i,k,j)
      P_field(i,k,j)=FACTOR*S_field(i,k,j)
      B_field(i,k,j)=P_field(i,k,j)
      K_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

   CALL g_spec_bdyupdate_ph( ph_save, P_ph_save, field, P_field, field_tend, P_field_tend, mu_tend, P_mu_tend, muts, P_muts, dt,&
& variable_in, spec_zone, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_field_tend(i,k,j)=0.0
      P_ph_save(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_tend(i,j)=0.0
      P_muts(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_spec_bdyupdate_ph( ph_save, P_ph_save, K_field, P_field, field_tend, P_field_tend, mu_tend, P_mu_tend, muts, P_muts, dt,&
& variable_in, spec_zone, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_field_tend(i,k,j)*B_field_tend(i,k,j) &
                    + P_ph_save(i,k,j)*B_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j) &
                    + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_field(i,k,j)*B_field(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_spec_bdyupdate_ph: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field_tend(i,k,j)=S_field_tend(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_tend(i,j)=S_mu_tend(i,j)
      muts(i,j)=S_muts(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field(i,k,j)=S_field(i,k,j)
   enddo
   enddo
   enddo

!g_spec_bdyupdate_ph: ALPHA=.1000E+00  COEF=   0.1000000119209E+01  VAL_N= 0.143960E+07  VAL_L= 0.143960E+07
!g_spec_bdyupdate_ph: ALPHA=.1000E-01  COEF=   0.9999946355820E+00  VAL_N= 0.143960E+05  VAL_L= 0.143960E+05
!g_spec_bdyupdate_ph: ALPHA=.1000E-02  COEF=   0.1000091433525E+01  VAL_N= 0.143973E+03  VAL_L= 0.143960E+03
!g_spec_bdyupdate_ph: ALPHA=.1000E-03  COEF=   0.1000368714333E+01  VAL_N= 0.144013E+01  VAL_L= 0.143960E+01
!g_spec_bdyupdate_ph: ALPHA=.1000E-04  COEF=   0.1002776384354E+01  VAL_N= 0.144360E-01  VAL_L= 0.143960E-01
!g_spec_bdyupdate_ph: ALPHA=.1000E-05  COEF=   0.9063574671745E+00  VAL_N= 0.130480E-03  VAL_L= 0.143960E-03
!g_spec_bdyupdate_ph: ALPHA=.1000E-06  COEF=   0.1363814473152E+01  VAL_N= 0.196335E-05  VAL_L= 0.143960E-05
!g_spec_bdyupdate_ph: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.143960E-07
!g_spec_bdyupdate_ph: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.143960E-09
!g_spec_bdyupdate_ph: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.143960E-11
!g_spec_bdyupdate_ph: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.143960E-13
                 
!a_spec_bdyupdate_ph:    0.1439602750000E+07   0.1439628875000E+07

!g_spec_bdyupdate_ph: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.143976E+07  VAL_L= 0.143976E+07
!g_spec_bdyupdate_ph: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.143976E+05  VAL_L= 0.143976E+05
!g_spec_bdyupdate_ph: ALPHA=.1000E-02  COEF=   0.9999999999998E+00  VAL_N= 0.143976E+03  VAL_L= 0.143976E+03
!g_spec_bdyupdate_ph: ALPHA=.1000E-03  COEF=   0.9999999999999E+00  VAL_N= 0.143976E+01  VAL_L= 0.143976E+01
!g_spec_bdyupdate_ph: ALPHA=.1000E-04  COEF=   0.1000000000014E+01  VAL_N= 0.143976E-01  VAL_L= 0.143976E-01
!g_spec_bdyupdate_ph: ALPHA=.1000E-05  COEF=   0.9999999998296E+00  VAL_N= 0.143976E-03  VAL_L= 0.143976E-03
!g_spec_bdyupdate_ph: ALPHA=.1000E-06  COEF=   0.1000000001183E+01  VAL_N= 0.143976E-05  VAL_L= 0.143976E-05
!g_spec_bdyupdate_ph: ALPHA=.1000E-07  COEF=   0.9999999881095E+00  VAL_N= 0.143976E-07  VAL_L= 0.143976E-07
!g_spec_bdyupdate_ph: ALPHA=.1000E-08  COEF=   0.1000000163263E+01  VAL_N= 0.143976E-09  VAL_L= 0.143976E-09
!g_spec_bdyupdate_ph: ALPHA=.1000E-09  COEF=   0.1000000083661E+01  VAL_N= 0.143976E-11  VAL_L= 0.143976E-11
!g_spec_bdyupdate_ph: ALPHA=.1000E-10  COEF=   0.9999994281817E+00  VAL_N= 0.143976E-13  VAL_L= 0.143976E-13
                 
!a_spec_bdyupdate_ph:    0.1439763904620E+07   0.1439763904620E+07


END SUBROUTINE t_spec_bdyupdate_ph
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_calc_mu_uv_1 ( config_flags,                 &
                          mu, muu, muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

! Zaizhong Ma, April 1,2005

   IMPLICIT NONE

   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(  OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme )                 :: mu

   !  local stuff

   INTEGER :: i, j, itf, jtf, im, jm

!  zzma: new definition

!  IN variables

   REAL, DIMENSION( ims:ime , jms:jme )  :: S_mu,P_mu,B_mu

!  OUT variables

   REAL, DIMENSION( ims:ime , jms:jme )  :: P_muu, P_muv,B_muu, B_muv

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      P_mu(i,j)=mu(i,j)
      B_mu(i,j)=mu(i,j)
   enddo
   enddo

!NLM

   CALL calc_mu_uv_1 ( config_flags,                 &
                          mu, muu, muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   do i=ims,ime
   do j=jms,jme
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
   enddo
   enddo

!  TCL

   CALL g_calc_mu_uv_1( config_flags, mu, P_mu, muu, P_muu, muv, P_muv, ids, ide, jds, jde, ims, ime, jms, jme, its, ite, jts, &
&jte )

   SAVE_L=0.
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_muu(i,j)*P_muu(i,j) + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
   enddo
   enddo

   CALL calc_mu_uv_1 ( config_flags,                 &
                          P_mu, P_muu, P_muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

      VAL_N=0.
   do i=ims,ime
   do j=jms,jme
         VAL_N=VAL_N+ (P_muu(i,j)-B_muu(i,j))*(P_muu(i,j)-B_muu(i,j))    &
                    + (P_muv(i,j)-B_muv(i,j))*(P_muv(i,j)-B_muv(i,j))
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calc_mu_uv_1: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)

      B_mu(i,j)=P_mu(i,j)
   enddo
   enddo

!  TGL

   CALL g_calc_mu_uv_1( config_flags, mu, P_mu, muu, P_muu, muv, P_muv, ids, ide, jds, jde, ims, ime, jms, jme, its, ite, jts, &
&jte )

   VAL_L=0.
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L +P_muu(i,j)*P_muu(i,j) + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_calc_mu_uv_1( config_flags, P_mu, P_muu, P_muv, ids, ide, jds, jde, ims, ime, jms, jme, its, ite, jts, jte )

   VAL_A=0.
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_calc_mu_uv_1: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

!g_calc_mu_uv_1: ALPHA=.1000E+00  COEF=   0.5623352050781E+01  VAL_N= 0.178737E+13  VAL_L= 0.317848E+12
!g_calc_mu_uv_1: ALPHA=.1000E-01  COEF=   0.4679619445801E+03  VAL_N= 0.148741E+13  VAL_L= 0.317848E+10
!g_calc_mu_uv_1: ALPHA=.1000E-02  COEF=   0.4670370312500E+05  VAL_N= 0.148447E+13  VAL_L= 0.317848E+08
!g_calc_mu_uv_1: ALPHA=.1000E-03  COEF=   0.4670370000000E+07  VAL_N= 0.148447E+13  VAL_L= 0.317848E+06
!g_calc_mu_uv_1: ALPHA=.1000E-04  COEF=   0.4670369920000E+09  VAL_N= 0.148447E+13  VAL_L= 0.317848E+04
!g_calc_mu_uv_1: ALPHA=.1000E-05  COEF=   0.4670369792000E+11  VAL_N= 0.148447E+13  VAL_L= 0.317848E+02
!g_calc_mu_uv_1: ALPHA=.1000E-06  COEF=   0.4670369038336E+13  VAL_N= 0.148447E+13  VAL_L= 0.317848E+00
!g_calc_mu_uv_1: ALPHA=.1000E-07  COEF=   0.4670369248051E+15  VAL_N= 0.148447E+13  VAL_L= 0.317848E-02
!g_calc_mu_uv_1: ALPHA=.1000E-08  COEF=   0.4670368912507E+17  VAL_N= 0.148447E+13  VAL_L= 0.317848E-04
!g_calc_mu_uv_1: ALPHA=.1000E-09  COEF=   0.4670368603269E+19  VAL_N= 0.148447E+13  VAL_L= 0.317848E-06
!g_calc_mu_uv_1: ALPHA=.1000E-10  COEF=   0.4670368273416E+21  VAL_N= 0.148447E+13  VAL_L= 0.317848E-08
                 
!a_calc_mu_uv_1:    0.1787371847680E+13   0.3030103490560E+12

!g_calc_mu_uv_1: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.303010E+12  VAL_L= 0.303010E+12
!g_calc_mu_uv_1: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.303010E+10  VAL_L= 0.303010E+10
!g_calc_mu_uv_1: ALPHA=.1000E-02  COEF=   0.9999999999998E+00  VAL_N= 0.303010E+08  VAL_L= 0.303010E+08
!g_calc_mu_uv_1: ALPHA=.1000E-03  COEF=   0.1000000000001E+01  VAL_N= 0.303010E+06  VAL_L= 0.303010E+06
!g_calc_mu_uv_1: ALPHA=.1000E-04  COEF=   0.1000000000010E+01  VAL_N= 0.303010E+04  VAL_L= 0.303010E+04
!g_calc_mu_uv_1: ALPHA=.1000E-05  COEF=   0.9999999997959E+00  VAL_N= 0.303010E+02  VAL_L= 0.303010E+02
!g_calc_mu_uv_1: ALPHA=.1000E-06  COEF=   0.1000000001036E+01  VAL_N= 0.303010E+00  VAL_L= 0.303010E+00
!g_calc_mu_uv_1: ALPHA=.1000E-07  COEF=   0.9999999973814E+00  VAL_N= 0.303010E-02  VAL_L= 0.303010E-02
!g_calc_mu_uv_1: ALPHA=.1000E-08  COEF=   0.1000000270797E+01  VAL_N= 0.303010E-04  VAL_L= 0.303010E-04
!g_calc_mu_uv_1: ALPHA=.1000E-09  COEF=   0.1000000267344E+01  VAL_N= 0.303010E-06  VAL_L= 0.303010E-06
!g_calc_mu_uv_1: ALPHA=.1000E-10  COEF=   0.1000003235117E+01  VAL_N= 0.303011E-08  VAL_L= 0.303010E-08
                 
!a_calc_mu_uv_1:    0.3030098328208E+12   0.3030098328208E+12


END SUBROUTINE t_calc_mu_uv_1
!-----------------------------------------------------------------------------------------------

SUBROUTINE t_small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,    &
                              t_2, t_1, ph_2, ph_1, ww, ww1,   &
                              mu_2, mu_1,                      &
                              mut, muts, muu, muus, muv, muvs, &
                              u_save, v_save, w_save,          &
                              t_save, ph_save, mu_save,        &
                              msfu, msfv, msft,                &
                              h_diabatic,                      &
                              number_of_small_timesteps,dts,   &
                              rk_step, rk_order,               &
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

! Zaizhong Ma, April 4,2005

  IMPLICIT NONE  ! religion first

!  stuff passed in

  INTEGER,                  INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,                  INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,                  INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  INTEGER,                  INTENT(IN   ) :: number_of_small_timesteps
  INTEGER,                  INTENT(IN   ) :: rk_step, rk_order
  REAL,                     INTENT(IN   ) :: dts

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: u_1, &
                                                                 v_1, &
                                                                 w_1, &
                                                                 t_1, &
                                                                 ww1, &
                                                                 ph_1

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: u_2, &
                                                                 v_2, &
                                                                 w_2, &
                                                                 t_2, &
                                                                 ww,  &
                                                                 ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: h_diabatic
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save

  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: muus, muvs
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mu_2, mu_1
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mut, muts, &
                                                        muu, muv, mu_save
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: msfu, msfv, msft


! local stuff

  INTEGER         :: i,j,k
  INTEGER :: i_start, i_end, j_start, j_end, i_endu, j_endv

!  zzma: new definition

! IN variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: S_u_save,   &
                                                              S_v_save,   &
                                                              S_w_save,   &
                                                              S_t_save,   &
                                                              S_ph_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: P_u_save,   &
                                                              P_v_save,   &
                                                              P_w_save,   &
                                                              P_t_save,   &
                                                              P_ph_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)               :: B_u_save,   &
                                                              B_v_save,   &
                                                              B_w_save,   &
                                                              B_t_save,   &
                                                              B_ph_save

!  INOUT variables
  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme)                :: S_u_2, &
                                                                 S_v_2, &
                                                                 S_w_2, &
                                                                 S_t_2, &
                                                                 S_ph_2
  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme)                :: P_u_2, &
                                                                 P_v_2, &
                                                                 P_w_2, &
                                                                 P_t_2, &
                                                                 P_ph_2
  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme)                :: K_u_2, &
                                                                 K_v_2, &
                                                                 K_w_2, &
                                                                 K_t_2, &
                                                                 K_ph_2
  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme)                :: B_u_2, &
                                                                 B_v_2, &
                                                                 B_w_2, &
                                                                 B_t_2, &
                                                                 B_ph_2

  REAL,   DIMENSION(ims:ime, jms:jme) :: S_muus, S_muvs,S_mu_2,S_mut, S_muts,S_muu, S_muv, S_mu_save
  REAL,   DIMENSION(ims:ime, jms:jme) :: P_muus, P_muvs,P_mu_2,P_mut, P_muts,P_muu, P_muv, P_mu_save
  REAL,   DIMENSION(ims:ime, jms:jme) :: K_muus, K_muvs,K_mu_2,K_mut, K_muts,K_muu, K_muv, K_mu_save
  REAL,   DIMENSION(ims:ime, jms:jme) :: B_muus, B_muvs,B_mu_2,B_mut, B_muts,B_muu, B_muv, B_mu_save

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u_save(i,k,j)=u_save(i,k,j)
      S_v_save(i,k,j)=v_save(i,k,j)
      S_w_save(i,k,j)=w_save(i,k,j)
      S_t_save(i,k,j)=t_save(i,k,j)
      S_ph_save(i,k,j)=ph_save(i,k,j)

      P_u_save(i,k,j)=u_save(i,k,j)
      P_v_save(i,k,j)=v_save(i,k,j)
      P_w_save(i,k,j)=w_save(i,k,j)
      P_t_save(i,k,j)=t_save(i,k,j)
      P_ph_save(i,k,j)=ph_save(i,k,j)
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u_2(i,k,j)=u_2(i,k,j)
      S_v_2(i,k,j)=v_2(i,k,j)
      S_w_2(i,k,j)=w_2(i,k,j)
      S_t_2(i,k,j)=t_2(i,k,j)
      S_ph_2(i,k,j)=ph_2(i,k,j)

      P_u_2(i,k,j)=u_2(i,k,j)
      P_v_2(i,k,j)=v_2(i,k,j)
      P_w_2(i,k,j)=w_2(i,k,j)
      P_t_2(i,k,j)=t_2(i,k,j)
      P_ph_2(i,k,j)=ph_2(i,k,j)

      K_u_2(i,k,j)=u_2(i,k,j)
      K_v_2(i,k,j)=v_2(i,k,j)
      K_w_2(i,k,j)=w_2(i,k,j)
      K_t_2(i,k,j)=t_2(i,k,j)
      K_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_muus(i,j)=muus(i,j)
      S_muvs(i,j)=muvs(i,j)
      S_mu_2(i,j)=mu_2(i,j)
      S_mut(i,j)=mut(i,j)
      S_muts(i,j)=muts(i,j)
      S_muu(i,j)=muu(i,j)
      S_muv(i,j)=muv(i,j)
      S_mu_save(i,j)=mu_save(i,j)

      P_muus(i,j)=muus(i,j)
      P_muvs(i,j)=muvs(i,j)
      P_mu_2(i,j)=mu_2(i,j)
      P_mut(i,j)=mut(i,j)
      P_muts(i,j)=muts(i,j)
      P_muu(i,j)=muu(i,j)
      P_muv(i,j)=muv(i,j)
      P_mu_save(i,j)=mu_save(i,j)

      K_muus(i,j)=muus(i,j)
      K_muvs(i,j)=muvs(i,j)
      K_mu_2(i,j)=mu_2(i,j)
      K_mut(i,j)=mut(i,j)
      K_muts(i,j)=muts(i,j)
      K_muu(i,j)=muu(i,j)
      K_muv(i,j)=muv(i,j)
      K_mu_save(i,j)=mu_save(i,j)
   enddo
   enddo

!NLM

   CALL small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,    &
                              t_2, t_1, ph_2, ph_1, ww, ww1,   &
                              mu_2, mu_1,                      &
                              mut, muts, muu, muus, muv, muvs, &
                              u_save, v_save, w_save,          &
                              t_save, ph_save, mu_save,        &
                              msfu, msfv, msft,                &
                              h_diabatic,                      &
                              number_of_small_timesteps,dts,   &
                              rk_step, rk_order,               &
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_u_2(i,k,j)=u_2(i,k,j)
      B_v_2(i,k,j)=v_2(i,k,j)
      B_w_2(i,k,j)=w_2(i,k,j)
      B_t_2(i,k,j)=t_2(i,k,j)
      B_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      B_muus(i,j)=muus(i,j)
      B_muvs(i,j)=muvs(i,j)
      B_mu_2(i,j)=mu_2(i,j)
      B_mut(i,j)=mut(i,j)
      B_muts(i,j)=muts(i,j)
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
      B_mu_save(i,j)=mu_save(i,j)
   enddo
   enddo

!  TCL

   CALL g_small_step_finish( K_u_2, P_u_2, K_v_2, P_v_2, K_w_2, P_w_2, K_t_2, P_t_2, K_ph_2, P_ph_2, ww, K_mu_2, P_mu_2, & 
&K_mut, P_mut, K_muts, &
&P_muts, K_muu, P_muu, K_muus, P_muus, K_muv, P_muv, K_muvs, P_muvs, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, &
&P_t_save, ph_save, P_ph_save, K_mu_save, P_mu_save, msfu, msfv, msft, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      SAVE_L=SAVE_L + P_muus(i,j)*P_muus(i,j)            &
                    + P_muvs(i,j)*P_muvs(i,j)            &
                    + P_mu_2(i,j)*P_mu_2(i,j)            &
                    + P_mut(i,j)*P_mut(i,j)            &
                    + P_muts(i,j)*P_muts(i,j)            &
                    + P_muu(i,j)*P_muu(i,j)            &
                    + P_muv(i,j)*P_muv(i,j)            &
                    + P_mu_save(i,j)*P_mu_save(i,j)
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_save(i,k,j)=FACTOR*S_u_save(i,k,j)
      P_v_save(i,k,j)=FACTOR*S_v_save(i,k,j)
      P_w_save(i,k,j)=FACTOR*S_w_save(i,k,j)
      P_t_save(i,k,j)=FACTOR*S_t_save(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_2(i,k,j)=FACTOR*S_u_2(i,k,j)
      P_v_2(i,k,j)=FACTOR*S_v_2(i,k,j)
      P_w_2(i,k,j)=FACTOR*S_w_2(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_2(i,k,j)=FACTOR*S_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_muus(i,j)=FACTOR*S_muus(i,j)
      P_muvs(i,j)=FACTOR*S_muvs(i,j)
      P_mu_2(i,j)=FACTOR*S_mu_2(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
      P_mu_save(i,j)=FACTOR*S_mu_save(i,j)
   enddo
   enddo

   CALL small_step_finish( P_u_2, u_1, P_v_2, v_1, P_w_2, w_1,    &
                              P_t_2, t_1, P_ph_2, ph_1, ww, ww1,   &
                              P_mu_2, mu_1,                      &
                              P_mut, P_muts, P_muu, P_muus, P_muv, P_muvs, &
                              P_u_save, P_v_save, P_w_save,          &
                              P_t_save, P_ph_save, P_mu_save,        &
                              msfu, msfv, msft,                &
                              h_diabatic,                      &
                              number_of_small_timesteps,dts,   &
                              rk_step, rk_order,               &
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N + (P_u_2(i,k,j)- B_u_2(i,k,j))*(P_u_2(i,k,j)- B_u_2(i,k,j))         &
                  + (P_v_2(i,k,j)- B_v_2(i,k,j))*(P_v_2(i,k,j)- B_v_2(i,k,j))         &
                  + (P_w_2(i,k,j)- B_w_2(i,k,j))*(P_w_2(i,k,j)- B_w_2(i,k,j))         &
                  + (P_t_2(i,k,j)- B_t_2(i,k,j))*(P_t_2(i,k,j)- B_t_2(i,k,j))         &
                  + (P_ph_2(i,k,j)- B_ph_2(i,k,j))*(P_ph_2(i,k,j)- B_ph_2(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_N=VAL_N   + (P_muus(i,j)- B_muus(i,j))*(P_muus(i,j)- B_muus(i,j))               & 
                    + (P_muvs(i,j)- B_muvs(i,j))*(P_muvs(i,j)- B_muvs(i,j))               &
                    + (P_mu_2(i,j)- B_mu_2(i,j))*(P_mu_2(i,j)- B_mu_2(i,j))               &
                    + (P_mut(i,j)- B_mut(i,j))*(P_mut(i,j)- B_mut(i,j))                   &
                    + (P_muts(i,j)- B_muts(i,j))*(P_muts(i,j)- B_muts(i,j))               &
                    + (P_muu(i,j)- B_muu(i,j))*(P_muu(i,j)- B_muu(i,j))                   &
                    + (P_muv(i,j)- B_muv(i,j))*(P_muv(i,j)- B_muv(i,j))                   &
                    + (P_mu_save(i,j)- B_mu_save(i,j))*(P_mu_save(i,j)- B_mu_save(i,j)) 
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_small_step_finish: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_save(i,k,j)=S_u_save(i,k,j)
      v_save(i,k,j)=S_v_save(i,k,j)
      w_save(i,k,j)=S_w_save(i,k,j)
      t_save(i,k,j)=S_t_save(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)

      P_u_save(i,k,j)=FACTOR*S_u_save(i,k,j)
      P_v_save(i,k,j)=FACTOR*S_v_save(i,k,j)
      P_w_save(i,k,j)=FACTOR*S_w_save(i,k,j)
      P_t_save(i,k,j)=FACTOR*S_t_save(i,k,j)
      P_ph_save(i,k,j)=FACTOR*S_ph_save(i,k,j)

      B_u_save(i,k,j)=P_u_save(i,k,j)
      B_v_save(i,k,j)=P_v_save(i,k,j)
      B_w_save(i,k,j)=P_w_save(i,k,j)
      B_t_save(i,k,j)=P_t_save(i,k,j)
      B_ph_save(i,k,j)=P_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_2(i,k,j)=S_u_2(i,k,j)
      v_2(i,k,j)=S_v_2(i,k,j)
      w_2(i,k,j)=S_w_2(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_2(i,k,j)=S_ph_2(i,k,j)

      P_u_2(i,k,j)=FACTOR*S_u_2(i,k,j)
      P_v_2(i,k,j)=FACTOR*S_v_2(i,k,j)
      P_w_2(i,k,j)=FACTOR*S_w_2(i,k,j)
      P_t_2(i,k,j)=FACTOR*S_t_2(i,k,j)
      P_ph_2(i,k,j)=FACTOR*S_ph_2(i,k,j)

      B_u_2(i,k,j)=P_u_2(i,k,j)
      B_v_2(i,k,j)=P_v_2(i,k,j)
      B_w_2(i,k,j)=P_w_2(i,k,j)
      B_t_2(i,k,j)=P_t_2(i,k,j)
      B_ph_2(i,k,j)=P_ph_2(i,k,j)

      K_u_2(i,k,j)=u_2(i,k,j)
      K_v_2(i,k,j)=v_2(i,k,j)
      K_w_2(i,k,j)=w_2(i,k,j)
      K_t_2(i,k,j)=t_2(i,k,j)
      K_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muus(i,j)=S_muus(i,j)
      muvs(i,j)=S_muvs(i,j)
      mu_2(i,j)=S_mu_2(i,j)
      mut(i,j)=S_mut(i,j)
      muts(i,j)=S_muts(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
      mu_save(i,j)=S_mu_save(i,j)

      P_muus(i,j)=FACTOR*S_muus(i,j)
      P_muvs(i,j)=FACTOR*S_muvs(i,j)
      P_mu_2(i,j)=FACTOR*S_mu_2(i,j)
      P_mut(i,j)=FACTOR*S_mut(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
      P_muu(i,j)=FACTOR*S_muu(i,j)
      P_muv(i,j)=FACTOR*S_muv(i,j)
      P_mu_save(i,j)=FACTOR*S_mu_save(i,j)

      B_muus(i,j)=P_muus(i,j)
      B_muvs(i,j)=P_muvs(i,j)
      B_mu_2(i,j)=P_mu_2(i,j)
      B_mut(i,j)=P_mut(i,j)
      B_muts(i,j)=P_muts(i,j)
      B_muu(i,j)=P_muu(i,j)
      B_muv(i,j)=P_muv(i,j)
      B_mu_save(i,j)=P_mu_save(i,j)

      K_muus(i,j)=muus(i,j)
      K_muvs(i,j)=muvs(i,j)
      K_mu_2(i,j)=mu_2(i,j)
      K_mut(i,j)=mut(i,j)
      K_muts(i,j)=muts(i,j)
      K_muu(i,j)=muu(i,j)
      K_muv(i,j)=muv(i,j)
      K_mu_save(i,j)=mu_save(i,j)
   enddo
   enddo

!  TGL

   CALL g_small_step_finish( u_2, P_u_2, v_2, P_v_2, w_2, P_w_2, t_2, P_t_2, ph_2, P_ph_2, ww, mu_2, P_mu_2, &
&mut, P_mut, muts, &
&P_muts, muu, P_muu, muus, P_muus, muv, P_muv, muvs, P_muvs, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, &
&P_t_save, ph_save, P_ph_save, mu_save, P_mu_save, msfu, msfv, msft, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L   + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_L=VAL_L +P_muus(i,j)*P_muus(i,j)            &
                    + P_muvs(i,j)*P_muvs(i,j)            &
                    + P_mu_2(i,j)*P_mu_2(i,j)            &
                    + P_mut(i,j)*P_mut(i,j)            &
                    + P_muts(i,j)*P_muts(i,j)            &
                    + P_muu(i,j)*P_muu(i,j)            &
                    + P_muv(i,j)*P_muv(i,j)            &
                    + P_mu_save(i,j)*P_mu_save(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u_save(i,k,j)=0.0
      P_v_save(i,k,j)=0.0
      P_w_save(i,k,j)=0.0
      P_t_save(i,k,j)=0.0
      P_ph_save(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ

  CALL a_small_step_finish(K_u_2,P_u_2, K_v_2, P_v_2, K_w_2, P_w_2, K_t_2, P_t_2, P_ph_2, P_mu_2, K_mut, P_mut, K_muts, P_muts, K_muu, &
&P_muu,K_muus,P_muus, K_muv, P_muv, K_muvs, P_muvs, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, P_t_save, P_ph_save, &
&P_mu_save, msfu, msfv, msft, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
     VAL_A=VAL_A +B_u_save(i,k,j)*P_u_save(i,k,j)       &
                 +B_v_save(i,k,j)*P_v_save(i,k,j)       &
                 +B_w_save(i,k,j)*P_w_save(i,k,j)       &
                 +B_t_save(i,k,j)*P_t_save(i,k,j)       &
                 +B_ph_save(i,k,j)*P_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
     VAL_A=VAL_A    + P_u_2(i,k,j)*B_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*B_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*B_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*B_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*B_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
     VAL_A=VAL_A    + P_muus(i,j)*B_muus(i,j)            &
                    + P_muvs(i,j)*B_muvs(i,j)            &
                    + P_mu_2(i,j)*B_mu_2(i,j)            &
                    + P_mut(i,j)*B_mut(i,j)            &
                    + P_muts(i,j)*B_muts(i,j)            &
                    + P_muu(i,j)*B_muu(i,j)            &
                    + P_muv(i,j)*B_muv(i,j)            &
                    + P_mu_save(i,j)*B_mu_save(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_small_step_finish: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_save(i,k,j)=S_u_save(i,k,j)
      v_save(i,k,j)=S_v_save(i,k,j)
      w_save(i,k,j)=S_w_save(i,k,j)
      t_save(i,k,j)=S_t_save(i,k,j)
      ph_save(i,k,j)=S_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u_2(i,k,j)=S_u_2(i,k,j)
      v_2(i,k,j)=S_v_2(i,k,j)
      w_2(i,k,j)=S_w_2(i,k,j)
      t_2(i,k,j)=S_t_2(i,k,j)
      ph_2(i,k,j)=S_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      muus(i,j)=S_muus(i,j)
      muvs(i,j)=S_muvs(i,j)
      mu_2(i,j)=S_mu_2(i,j)
      mut(i,j)=S_mut(i,j)
      muts(i,j)=S_muts(i,j)
      muu(i,j)=S_muu(i,j)
      muv(i,j)=S_muv(i,j)
      mu_save(i,j)=S_mu_save(i,j)
   enddo
   enddo

!g_small_step_finish: ALPHA=.1000E+00  COEF=   0.9997525215149E+00  VAL_N= 0.959912E+12  VAL_L= 0.960150E+12
!g_small_step_finish: ALPHA=.1000E-01  COEF=   0.9998036623001E+00  VAL_N= 0.959961E+10  VAL_L= 0.960150E+10
!g_small_step_finish: ALPHA=.1000E-02  COEF=   0.1000095129013E+01  VAL_N= 0.960241E+08  VAL_L= 0.960150E+08
!g_small_step_finish: ALPHA=.1000E-03  COEF=   0.1000633716583E+01  VAL_N= 0.960758E+06  VAL_L= 0.960150E+06
!g_small_step_finish: ALPHA=.1000E-04  COEF=   0.9989603161812E+00  VAL_N= 0.959152E+04  VAL_L= 0.960150E+04
!g_small_step_finish: ALPHA=.1000E-05  COEF=   0.8403349518776E+00  VAL_N= 0.806847E+02  VAL_L= 0.960150E+02
!g_small_step_finish: ALPHA=.1000E-06  COEF=   0.6937835216522E+00  VAL_N= 0.666136E+00  VAL_L= 0.960150E+00
!g_small_step_finish: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.960150E-02
!g_small_step_finish: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.960150E-04
!g_small_step_finish: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.960150E-06
!g_small_step_finish: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.960150E-08
                 
!a_small_step_finish:    0.9599120834560E+12   0.9599578931200E+12

!g_small_step_finish: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.960067E+12  VAL_L= 0.960067E+12
!g_small_step_finish: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.960067E+10  VAL_L= 0.960067E+10
!g_small_step_finish: ALPHA=.1000E-02  COEF=   0.9999999999998E+00  VAL_N= 0.960067E+08  VAL_L= 0.960067E+08
!g_small_step_finish: ALPHA=.1000E-03  COEF=   0.1000000000001E+01  VAL_N= 0.960067E+06  VAL_L= 0.960067E+06
!g_small_step_finish: ALPHA=.1000E-04  COEF=   0.1000000000010E+01  VAL_N= 0.960067E+04  VAL_L= 0.960067E+04
!g_small_step_finish: ALPHA=.1000E-05  COEF=   0.9999999997936E+00  VAL_N= 0.960067E+02  VAL_L= 0.960067E+02
!g_small_step_finish: ALPHA=.1000E-06  COEF=   0.1000000001029E+01  VAL_N= 0.960067E+00  VAL_L= 0.960067E+00
!g_small_step_finish: ALPHA=.1000E-07  COEF=   0.9999999976363E+00  VAL_N= 0.960067E-02  VAL_L= 0.960067E-02
!g_small_step_finish: ALPHA=.1000E-08  COEF=   0.1000000273437E+01  VAL_N= 0.960067E-04  VAL_L= 0.960067E-04
!g_small_step_finish: ALPHA=.1000E-09  COEF=   0.1000000269678E+01  VAL_N= 0.960067E-06  VAL_L= 0.960067E-06
!g_small_step_finish: ALPHA=.1000E-10  COEF=   0.1000003270830E+01  VAL_N= 0.960070E-08  VAL_L= 0.960067E-08
                 
!a_small_step_finish:    0.9600668356832E+12   0.9600668356832E+12

END SUBROUTINE t_small_step_finish
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            ru, rv, ww, mut, alt,         &
                            scalar,           &
                            scalar_tends, advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, xkmhd,          &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

! Zaizhong Ma, April 4,2005

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: rk_step, scs, sce
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   LOGICAL , INTENT(IN   ) :: moist_step

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce ),                &
                                         INTENT(INOUT)  :: scalar

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce ),                      &
                                         INTENT(  OUT)  :: scalar_tends

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  ) :: advect_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  ), INTENT(OUT  ) :: RQVFTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  )                ::     ru,  &
                                                                      rv,  &
                                                                      ww,  &
                                                                      xkmhd,  &
                                                                      alt


   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm,  &
                                                                  fnp,  &
                                                                  rdn,  &
                                                                  rdnw, &
                                                                  base

   REAL , DIMENSION( ims:ime , jms:jme ) ,       INTENT(IN   ) :: msfu,    &
                                                                  msfv,    &
                                                                  msft
   REAL , DIMENSION( ims:ime , jms:jme )          :: mut


   REAL ,                                        INTENT(IN   ) :: rdx,     &
                                                                  rdy,     &
                                                                  khdif,   &
                                                                  kvdif

   REAL ,                                        INTENT(IN   ) :: dt

   ! Local data

   INTEGER :: im, i,j,k

   REAL    :: khdq, kvdq, tendency

!  zzma: new definition

!  IN variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  )                ::     S_ru,  &
                                                                      S_rv,  &
                                                                      S_ww,  &
                                                                      S_advect_tend,  &
                                                                      S_xkmhd,  &
                                                                      S_alt
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  )                ::     P_ru,  &
                                                                      P_rv,  &
                                                                      P_ww,  &
                                                                      P_advect_tend,  &
                                                                      P_xkmhd,  &
                                                                      P_alt
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  )                ::     B_ru,  &
                                                                      B_rv,  &
                                                                      B_ww,  &
                                                                      B_advect_tend,  &
                                                                      B_xkmhd,  &
                                                                      B_alt
   REAL , DIMENSION( ims:ime , jms:jme )     :: S_mut,P_mut,B_mut

!  INOUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: S_scalar, P_scalar
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: K_scalar, B_scalar

!  OUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: P_scalar_tends,B_scalar_tends

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ru(i,k,j)=ru(i,k,j)
      S_rv(i,k,j)=rv(i,k,j)
      S_ww(i,k,j)=ww(i,k,j)
      S_advect_tend(i,k,j)=advect_tend(i,k,j)
      S_xkmhd(i,k,j)=xkmhd(i,k,j)
      S_alt(i,k,j)=alt(i,k,j)

      P_ru(i,k,j)=ru(i,k,j)
      P_rv(i,k,j)=rv(i,k,j)
      P_ww(i,k,j)=ww(i,k,j)
      P_advect_tend(i,k,j)=advect_tend(i,k,j)
      P_xkmhd(i,k,j)=xkmhd(i,k,j)
      P_alt(i,k,j)=alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      S_scalar(i,k,j,h)=scalar(i,k,j,h)

      P_scalar(i,k,j,h)=scalar(i,k,j,h)

      K_scalar(i,k,j,h)=scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!NLM

   CALL rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            ru, rv, ww, mut, alt,         &
                            scalar,           &
                            scalar_tends, advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, xkmhd,          &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      B_scalar_tends(i,k,j,h)=scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      B_scalar(i,k,j,h)=scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TCL

   CALL g_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, &
&K_scalar, P_scalar, scalar_tends, P_scalar_tends, advect_tend, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, &
&msft, rdx, rdy, rdn, rdnw, kvdif, xkmhd, P_xkmhd, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, &
&jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      SAVE_L=SAVE_L + P_scalar_tends(i,k,j,h)*P_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      SAVE_L=SAVE_L + P_scalar(i,k,j,h)*P_scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_advect_tend(i,k,j)=FACTOR*S_advect_tend(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      P_scalar(i,k,j,h)=FACTOR*S_scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   CALL rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            P_ru, P_rv, P_ww, P_mut, P_alt,         &
                            P_scalar,           &
                            P_scalar_tends, P_advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, P_xkmhd,          &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_N=VAL_N+(P_scalar_tends(i,k,j,h) -B_scalar_tends(i,k,j,h))*(P_scalar_tends(i,k,j,h) -B_scalar_tends(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_N=VAL_N+(P_scalar(i,k,j,h) -B_scalar(i,k,j,h))*(P_scalar(i,k,j,h) -B_scalar(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_rk_scalar_tend: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      advect_tend(i,k,j)=S_advect_tend(i,k,j)
      xkmhd(i,k,j)=S_xkmhd(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)

      P_ru(i,k,j)=FACTOR*S_ru(i,k,j)
      P_rv(i,k,j)=FACTOR*S_rv(i,k,j)
      P_ww(i,k,j)=FACTOR*S_ww(i,k,j)
      P_advect_tend(i,k,j)=FACTOR*S_advect_tend(i,k,j)
      P_alt(i,k,j)=FACTOR*S_alt(i,k,j)
      P_xkmhd(i,k,j)=FACTOR*S_xkmhd(i,k,j)

      B_ru(i,k,j)=P_ru(i,k,j)
      B_rv(i,k,j)=P_rv(i,k,j)
      B_advect_tend(i,k,j)=P_advect_tend(i,k,j)
      B_ww(i,k,j)=P_ww(i,k,j)
      B_alt(i,k,j)=P_alt(i,k,j)
      B_xkmhd(i,k,j)=P_xkmhd(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar(i,k,j,h)=S_scalar(i,k,j,h)

      P_scalar(i,k,j,h)=FACTOR*S_scalar(i,k,j,h)

      B_scalar(i,k,j,h)=P_scalar(i,k,j,h)

      K_scalar(i,k,j,h)=scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL

   CALL g_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, &
&scalar, P_scalar, scalar_tends, P_scalar_tends, advect_tend, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, &
&msft, rdx, rdy, rdn, rdnw, kvdif, xkmhd, P_xkmhd, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, &
&jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_L=VAL_L + P_scalar_tends(i,k,j,h)*P_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_L=VAL_L + P_scalar(i,k,j,h)*P_scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ru(i,k,j)=0.0
      P_rv(i,k,j)=0.0
      P_ww(i,k,j)=0.0
      P_advect_tend(i,k,j)=0.0
      P_alt(i,k,j)=0.0
      P_xkmhd(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ

   CALL a_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, &
&K_scalar, P_scalar, P_scalar_tends, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, msft, rdx, rdy, rdn, rdnw, &
&kvdif, xkmhd, P_xkmhd, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )


   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_ru(i,k,j)*B_ru(i,k,j)         &
               +P_rv(i,k,j)*B_rv(i,k,j)            &
               +P_ww(i,k,j)*B_ww(i,k,j)            &
               +P_advect_tend(i,k,j)*B_advect_tend(i,k,j)            &
               +P_alt(i,k,j)*B_alt(i,k,j)          &
               +P_xkmhd(i,k,j)*B_xkmhd(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_A=VAL_A +P_scalar(i,k,j,h)*B_scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_rk_scalar_tend: ', VAL_L,VAL_A

!  RECOVER


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ru(i,k,j)=S_ru(i,k,j)
      rv(i,k,j)=S_rv(i,k,j)
      ww(i,k,j)=S_ww(i,k,j)
      advect_tend(i,k,j)=S_advect_tend(i,k,j)
      xkmhd(i,k,j)=S_xkmhd(i,k,j)
      alt(i,k,j)=S_alt(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar(i,k,j,h)=S_scalar(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!g_rk_scalar_tend: ALPHA=.1000E+00  COEF=   0.9997231364250E+00  VAL_N= 0.307437E-01  VAL_L= 0.307522E-01
!g_rk_scalar_tend: ALPHA=.1000E-01  COEF=   0.9999352693558E+00  VAL_N= 0.307502E-03  VAL_L= 0.307522E-03
!g_rk_scalar_tend: ALPHA=.1000E-02  COEF=   0.9999766945839E+00  VAL_N= 0.307515E-05  VAL_L= 0.307522E-05
!g_rk_scalar_tend: ALPHA=.1000E-03  COEF=   0.1000588893890E+01  VAL_N= 0.307703E-07  VAL_L= 0.307522E-07
!g_rk_scalar_tend: ALPHA=.1000E-04  COEF=   0.1067813396454E+01  VAL_N= 0.328376E-09  VAL_L= 0.307522E-09
!g_rk_scalar_tend: ALPHA=.1000E-05  COEF=   0.7152854919434E+01  VAL_N= 0.219966E-10  VAL_L= 0.307522E-11
!g_rk_scalar_tend: ALPHA=.1000E-06  COEF=   0.6295269775391E+03  VAL_N= 0.193594E-10  VAL_L= 0.307522E-13
!g_rk_scalar_tend: ALPHA=.1000E-07  COEF=   0.6282780078125E+05  VAL_N= 0.193210E-10  VAL_L= 0.307522E-15
!g_rk_scalar_tend: ALPHA=.1000E-08  COEF=   0.6282780000000E+07  VAL_N= 0.193210E-10  VAL_L= 0.307522E-17
!g_rk_scalar_tend: ALPHA=.1000E-09  COEF=   0.6282778880000E+09  VAL_N= 0.193210E-10  VAL_L= 0.307522E-19
!g_rk_scalar_tend: ALPHA=.1000E-10  COEF=   0.6282778624000E+11  VAL_N= 0.193210E-10  VAL_L= 0.307522E-21
                 
!a_rk_scalar_tend:    0.3074371442199E-01   0.3074371442199E-01

!g_rk_scalar_tend: ALPHA=.1000E+00  COEF=   0.1000000000628E+01  VAL_N= 0.307533E-01  VAL_L= 0.307533E-01
!g_rk_scalar_tend: ALPHA=.1000E-01  COEF=   0.1000000062825E+01  VAL_N= 0.307533E-03  VAL_L= 0.307533E-03
!g_rk_scalar_tend: ALPHA=.1000E-02  COEF=   0.1000006282450E+01  VAL_N= 0.307535E-05  VAL_L= 0.307533E-05
!g_rk_scalar_tend: ALPHA=.1000E-03  COEF=   0.1000628245062E+01  VAL_N= 0.307726E-07  VAL_L= 0.307533E-07
!g_rk_scalar_tend: ALPHA=.1000E-04  COEF=   0.1062824506169E+01  VAL_N= 0.326854E-09  VAL_L= 0.307533E-09
!g_rk_scalar_tend: ALPHA=.1000E-05  COEF=   0.7282450615443E+01  VAL_N= 0.223959E-10  VAL_L= 0.307533E-11
!g_rk_scalar_tend: ALPHA=.1000E-06  COEF=   0.6292450615583E+03  VAL_N= 0.193514E-10  VAL_L= 0.307533E-13
!g_rk_scalar_tend: ALPHA=.1000E-07  COEF=   0.6282550615548E+05  VAL_N= 0.193209E-10  VAL_L= 0.307533E-15
!g_rk_scalar_tend: ALPHA=.1000E-08  COEF=   0.6282451615549E+07  VAL_N= 0.193206E-10  VAL_L= 0.307533E-17
!g_rk_scalar_tend: ALPHA=.1000E-09  COEF=   0.6282450625547E+09  VAL_N= 0.193206E-10  VAL_L= 0.307533E-19
!g_rk_scalar_tend: ALPHA=.1000E-10  COEF=   0.6282450615649E+11  VAL_N= 0.193206E-10  VAL_L= 0.307533E-21
                 
!a_rk_scalar_tend:    0.3075329603091E-01   0.3075329603091E-01

END SUBROUTINE t_rk_scalar_tend
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_spec_bdy_scalar ( scalar_tend,    &
                               scalar_b, scalar_bt,             &
                          spec_bdy_width, spec_zone,                   &
                          ijds, ijde,                 & ! min/max(id,jd)
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its, ite, jts, jte, kts, kte)

! Zaizhong Ma, April 5,2005
! Qingnong Xiao, rewritten, April 2005.

   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: scalar_tend
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: scalar_b
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )   :: scalar_bt
!Local
   INTEGER :: i,j,k

!  zzma: new definition

!  IN variables

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )   :: S_scalar_bt,P_scalar_bt,B_scalar_bt

!  OUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )   :: S_scalar_tend,P_scalar_tend,B_scalar_tend

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

!TGL test

   S_scalar_bt(:,:,:,:)=scalar_bt(:,:,:,:)
   P_scalar_bt(:,:,:,:)=scalar_bt(:,:,:,:)

   S_scalar_tend(:,:,:)=scalar_tend(:,:,:)
   p_scalar_tend(:,:,:)=scalar_tend(:,:,:)

!NLM

   CALL spec_bdy_scalar ( scalar_tend,    &
                          scalar_b, scalar_bt,             &
                          spec_bdy_width, spec_zone,                   &
                          ijds, ijde,                 & ! min/max(id,jd)
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its, ite, jts, jte, kts, kte)

   B_scalar_bt(:,:,:,:)=scalar_bt(:,:,:,:)
   B_scalar_tend(:,:,:)=scalar_tend(:,:,:)

!  TCL

   scalar_bt(:,:,:,:)=S_scalar_bt(:,:,:,:)
   scalar_tend(:,:,:)=S_scalar_tend(:,:,:)

   CALL g_spec_bdy_scalar( scalar_tend, P_scalar_tend, scalar_bt, P_scalar_bt, spec_bdy_width, spec_zone, ijds, ijde, ids, ide, &
&jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=sum(P_scalar_tend(:,:,:)*P_scalar_tend(:,:,:)) + &
          sum(P_scalar_bt(:,:,:,:)*P_scalar_bt(:,:,:,:))

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      P_scalar_bt(:,:,:,:)=FACTOR*S_scalar_bt(:,:,:,:)
      P_scalar_tend(:,:,:)=FACTOR*S_scalar_tend(:,:,:)

      CALL spec_bdy_scalar ( P_scalar_tend,    &
                             scalar_b, P_scalar_bt,             &
                          spec_bdy_width, spec_zone,                   &
                          ijds, ijde,                 & ! min/max(id,jd)
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its, ite, jts, jte, kts, kte)


      VAL_N= sum((P_scalar_tend(:,:,:)-B_scalar_tend(:,:,:))*(P_scalar_tend(:,:,:)-B_scalar_tend(:,:,:))) + &
             sum((P_scalar_bt(:,:,:,:)-B_scalar_bt(:,:,:,:))*(P_scalar_bt(:,:,:,:)-B_scalar_bt(:,:,:,:)))

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_spec_bdy_scalar: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   scalar_bt(:,:,:,:)=S_scalar_bt(:,:,:,:)
   scalar_tend(:,:,:)=S_scalar_tend(:,:,:)
   P_scalar_bt(:,:,:,:)=FACTOR*S_scalar_bt(:,:,:,:)
   P_scalar_tend(:,:,:)=FACTOR*S_scalar_tend(:,:,:)
   B_scalar_bt(:,:,:,:)=P_scalar_bt(:,:,:,:)
   B_scalar_tend(:,:,:)=P_scalar_tend(:,:,:)

   CALL g_spec_bdy_scalar( scalar_tend, P_scalar_tend, scalar_bt, P_scalar_bt, spec_bdy_width, spec_zone, ijds, ijde, ids, ide, &
&jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=sum(P_scalar_tend(:,:,:)*P_scalar_tend(:,:,:))+ &
         sum(P_scalar_bt(:,:,:,:)*P_scalar_bt(:,:,:,:))

!  ADJ

   CALL a_spec_bdy_scalar( P_scalar_tend, P_scalar_bt, spec_bdy_width, spec_zone, ijds, ijde, ids, ide, jds, jde, kds, kde, ims,&
& ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=sum(P_scalar_bt(:,:,:,:)*B_scalar_bt(:,:,:,:)) + &
         sum(P_scalar_tend(:,:,:)*B_scalar_tend(:,:,:))

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_spec_bdy_scalar: ', VAL_L,VAL_A

!  RECOVER

   scalar_bt(:,:,:,:)=S_scalar_bt(:,:,:,:)
   scalar_tend(:,:,:)=S_scalar_tend(:,:,:)

END SUBROUTINE t_spec_bdy_scalar

!-----------------------------------------------------------------------------------------------

SUBROUTINE t_rk_update_scalar( scs, sce,                      &
                             scalar_1, scalar_2, sc_tend,   &
                             advect_tend, msft,             &
                             mu_old, mu_new, mu_base,       &
                             rk_step, dt, spec_zone,        &
                             config_flags,           &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

! Zaizhong Ma, April 5,2005

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: scs, sce, rk_step, spec_zone
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   REAL,                    INTENT(IN   ) :: dt

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: scalar_1,  &
                                                           scalar_2,  &
                                                           sc_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme )        :: advect_tend
   REAL, DIMENSION(ims:ime, jms:jme  )                :: mu_old, mu_new

   REAL, DIMENSION(ims:ime, jms:jme  ), INTENT(IN   ) :: mu_base, &
                                                          msft

   INTEGER :: i,j,k,im
   REAL    :: sc_middle, msfsq
   REAL, DIMENSION(its:ite) :: muold, r_munew

   REAL, DIMENSION(its:ite, kts:kte, jts:jte  ) :: tendency

   INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

!  zzma: new definition

!  IN variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme )    :: S_advect_tend,P_advect_tend,B_advect_tend
   REAL, DIMENSION(ims:ime, jms:jme  )            :: S_mu_old, S_mu_new,P_mu_old, P_mu_new,B_mu_old, B_mu_new

!  INOUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce)  :: S_scalar_1, S_scalar_2, S_sc_tend,P_scalar_1, P_scalar_2, P_sc_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce)  :: K_scalar_1, K_scalar_2, K_sc_tend,B_scalar_1, B_scalar_2, B_sc_tend


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

!TGL test


   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_advect_tend(i,k,j)=advect_tend(i,k,j)

      P_advect_tend(i,k,j)=advect_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu_old(i,j)=mu_old(i,j)
      S_mu_new(i,j)=mu_new(i,j)

      P_mu_old(i,j)=mu_old(i,j)
      P_mu_new(i,j)=mu_new(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      S_scalar_1(i,k,j,h)=scalar_1(i,k,j,h)
      S_scalar_2(i,k,j,h)=scalar_2(i,k,j,h)
      S_sc_tend(i,k,j,h)=sc_tend(i,k,j,h)

      P_scalar_1(i,k,j,h)=scalar_1(i,k,j,h)
      P_scalar_2(i,k,j,h)=scalar_2(i,k,j,h)
      P_sc_tend(i,k,j,h)=sc_tend(i,k,j,h)

      K_scalar_1(i,k,j,h)=scalar_1(i,k,j,h)
      K_scalar_2(i,k,j,h)=scalar_2(i,k,j,h)
      K_sc_tend(i,k,j,h)=sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!NLM

   CALL rk_update_scalar( scs, sce,                      &
                             scalar_1, scalar_2, sc_tend,   &
                             advect_tend, msft,             &
                             mu_old, mu_new, mu_base,       &
                             rk_step, dt, spec_zone,        &
                             config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      B_scalar_1(i,k,j,h)=scalar_1(i,k,j,h)
      B_scalar_2(i,k,j,h)=scalar_2(i,k,j,h)
      B_sc_tend(i,k,j,h)=sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TCL

   CALL g_rk_update_scalar( scs, sce, K_scalar_1, P_scalar_1, K_scalar_2, P_scalar_2, K_sc_tend, P_sc_tend, advect_tend, &
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      SAVE_L=SAVE_L + P_scalar_1(i,k,j,h)*P_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*P_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*P_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_advect_tend(i,k,j)=FACTOR*S_advect_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_old(i,j)=FACTOR*S_mu_old(i,j)
      P_mu_new(i,j)=FACTOR*S_mu_new(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      P_scalar_1(i,k,j,h)=FACTOR*S_scalar_1(i,k,j,h)
      P_scalar_2(i,k,j,h)=FACTOR*S_scalar_2(i,k,j,h)
      P_sc_tend(i,k,j,h)=FACTOR*S_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   CALL rk_update_scalar( scs, sce,                      &
                             P_scalar_1, P_scalar_2, P_sc_tend,   &
                             P_advect_tend, msft,             &
                             P_mu_old, P_mu_new, mu_base,       &
                             rk_step, dt, spec_zone,        &
                             config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

      VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
         VAL_N=VAL_N+(P_scalar_1(i,k,j,h)-B_scalar_1(i,k,j,h))*(P_scalar_1(i,k,j,h)-B_scalar_1(i,k,j,h))    &
                    +(P_scalar_2(i,k,j,h)-B_scalar_2(i,k,j,h))*(P_scalar_2(i,k,j,h)-B_scalar_2(i,k,j,h))    &
                    +(P_sc_tend(i,k,j,h) -B_sc_tend(i,k,j,h))*(P_sc_tend(i,k,j,h) -B_sc_tend(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_rk_update_scalar: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      advect_tend(i,k,j)=S_advect_tend(i,k,j)

      P_advect_tend(i,k,j)=FACTOR*S_advect_tend(i,k,j)

      B_advect_tend(i,k,j)=P_advect_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_old(i,j)=S_mu_old(i,j)
      mu_new(i,j)=S_mu_new(i,j)

      P_mu_old(i,j)=FACTOR*S_mu_old(i,j)
      P_mu_new(i,j)=FACTOR*S_mu_new(i,j)

      B_mu_old(i,j)=P_mu_old(i,j)
      B_mu_new(i,j)=P_mu_new(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_1(i,k,j,h)=S_scalar_1(i,k,j,h)
      scalar_2(i,k,j,h)=S_scalar_2(i,k,j,h)
      sc_tend(i,k,j,h)=S_sc_tend(i,k,j,h)

      P_scalar_1(i,k,j,h)=FACTOR*S_scalar_1(i,k,j,h)
      P_scalar_2(i,k,j,h)=FACTOR*S_scalar_2(i,k,j,h)
      P_sc_tend(i,k,j,h)=FACTOR*S_sc_tend(i,k,j,h)

      B_scalar_1(i,k,j,h)=P_scalar_1(i,k,j,h)
      B_scalar_2(i,k,j,h)=P_scalar_2(i,k,j,h)
      B_sc_tend(i,k,j,h)=P_sc_tend(i,k,j,h)

      K_scalar_1(i,k,j,h)=scalar_1(i,k,j,h)
      K_scalar_2(i,k,j,h)=scalar_2(i,k,j,h)
      K_sc_tend(i,k,j,h)=sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL

   CALL g_rk_update_scalar( scs, sce, scalar_1, P_scalar_1, scalar_2, P_scalar_2, sc_tend, P_sc_tend, advect_tend, &
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_L=VAL_L + P_scalar_1(i,k,j,h)*P_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*P_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*P_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_advect_tend(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu_old(i,j)=0.0
      P_mu_new(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_rk_update_scalar( scs, sce, K_scalar_1, P_scalar_1, K_scalar_2, P_scalar_2, K_sc_tend, P_sc_tend, advect_tend, &
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_advect_tend(i,k,j)*B_advect_tend(i,k,j)     
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_mu_old(i,j)*B_mu_old(i,j)                   &
                  +P_mu_new(i,j)*B_mu_new(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      VAL_A=VAL_A + P_scalar_1(i,k,j,h)*B_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*B_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*B_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_rk_update_scalar: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      advect_tend(i,k,j)=S_advect_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu_old(i,j)=S_mu_old(i,j)
      mu_new(i,j)=S_mu_new(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_1(i,k,j,h)=S_scalar_1(i,k,j,h)
      scalar_2(i,k,j,h)=S_scalar_2(i,k,j,h)
      sc_tend(i,k,j,h)=S_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!g_rk_update_scalar: ALPHA=.1000E+00  COEF=   0.9997228980064E+00  VAL_N= 0.307440E-01  VAL_L= 0.307526E-01
!g_rk_update_scalar: ALPHA=.1000E-01  COEF=   0.9999256134033E+00  VAL_N= 0.307503E-03  VAL_L= 0.307526E-03
!g_rk_update_scalar: ALPHA=.1000E-02  COEF=   0.9999616742134E+00  VAL_N= 0.307514E-05  VAL_L= 0.307526E-05
!g_rk_update_scalar: ALPHA=.1000E-03  COEF=   0.9997097849846E+00  VAL_N= 0.307436E-07  VAL_L= 0.307526E-07
!g_rk_update_scalar: ALPHA=.1000E-04  COEF=   0.1002490162849E+01  VAL_N= 0.308291E-09  VAL_L= 0.307526E-09
!g_rk_update_scalar: ALPHA=.1000E-05  COEF=   0.8739548921585E+00  VAL_N= 0.268764E-11  VAL_L= 0.307526E-11
!g_rk_update_scalar: ALPHA=.1000E-06  COEF=   0.1635856509209E+01  VAL_N= 0.503068E-13  VAL_L= 0.307526E-13
!g_rk_update_scalar: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.307526E-15
!g_rk_update_scalar: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.307526E-17
!g_rk_update_scalar: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.307526E-19
!g_rk_update_scalar: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.307526E-21
                 
!a_rk_update_scalar:    0.3074402920902E-01   0.3075052797794E-01

!g_rk_update_scalar: ALPHA=.1000E+00  COEF=   0.1000001003829E+01  VAL_N= 0.307535E-01  VAL_L= 0.307535E-01
!g_rk_update_scalar: ALPHA=.1000E-01  COEF=   0.1000000096620E+01  VAL_N= 0.307535E-03  VAL_L= 0.307535E-03
!g_rk_update_scalar: ALPHA=.1000E-02  COEF=   0.1000000009624E+01  VAL_N= 0.307535E-05  VAL_L= 0.307535E-05
!g_rk_update_scalar: ALPHA=.1000E-03  COEF=   0.1000000000962E+01  VAL_N= 0.307535E-07  VAL_L= 0.307535E-07
!g_rk_update_scalar: ALPHA=.1000E-04  COEF=   0.1000000000106E+01  VAL_N= 0.307535E-09  VAL_L= 0.307535E-09
!g_rk_update_scalar: ALPHA=.1000E-05  COEF=   0.9999999999299E+00  VAL_N= 0.307535E-11  VAL_L= 0.307535E-11
!g_rk_update_scalar: ALPHA=.1000E-06  COEF=   0.1000000001150E+01  VAL_N= 0.307535E-13  VAL_L= 0.307535E-13
!g_rk_update_scalar: ALPHA=.1000E-07  COEF=   0.9999999881787E+00  VAL_N= 0.307535E-15  VAL_L= 0.307535E-15
!g_rk_update_scalar: ALPHA=.1000E-08  COEF=   0.1000000050476E+01  VAL_N= 0.307535E-17  VAL_L= 0.307535E-17
!g_rk_update_scalar: ALPHA=.1000E-09  COEF=   0.1000000785292E+01  VAL_N= 0.307535E-19  VAL_L= 0.307535E-19
!g_rk_update_scalar: ALPHA=.1000E-10  COEF=   0.1000006304873E+01  VAL_N= 0.307537E-21  VAL_L= 0.307535E-21
                 
!a_rk_update_scalar:    0.3075349138757E-01   0.3075349138757E-01


END SUBROUTINE t_rk_update_scalar
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_calc_p_rho_phi ( moist, n_moist,                &
                            al, alb, mu, muts, ph, p, pb,  &
                            t, p0, t0, znu, dnw, rdnw,     &
                            rdn, non_hydrostatic,          &
                            ids, ide, jds, jde, kds, kde,  &
                            ims, ime, jms, jme, kms, kme,  &
                            its, ite, jts, jte, kts, kte  )


! Zaizhong Ma, April 5,2005

  IMPLICIT NONE
  
   ! Input data

  LOGICAL ,          INTENT(IN   ) :: non_hydrostatic

  INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

  INTEGER ,          INTENT(IN   ) :: n_moist

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme )   :: t

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ) :: moist
  REAL, DIMENSION( ims:ime , jms:jme )   :: mu, muts

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alb,  &
                                                                   pb


  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(  OUT) :: al, p

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: ph


  REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: znu, dnw, rdnw, rdn

  REAL,   INTENT(IN   ) :: t0, p0

  ! Local stuff

  INTEGER :: i, j, k, itf, jtf, ktf, ispe
  REAL    :: qvf, qtot, qf1, qf2

!  zzma: new definition

!  IN variables

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_t,P_t,B_t

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ) :: S_moist,P_moist,B_moist
  REAL, DIMENSION( ims:ime , jms:jme )   :: S_mu, S_muts,P_mu, P_muts,B_mu, B_muts
!  INOUT

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme )  :: S_ph,P_ph,K_ph,B_ph

!  OUT variables

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) :: P_al, P_p,B_al, B_p

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT,h

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_t(i,k,j)=t(i,k,j)
      P_t(i,k,j)=t(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      S_moist(i,k,j,h)=moist(i,k,j,h)
      P_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      S_muts(i,j)=muts(i,j)

      P_mu(i,j)=mu(i,j)
      P_muts(i,j)=muts(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_ph(i,k,j)=ph(i,k,j)
      P_ph(i,k,j)=ph(i,k,j)
      K_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL calc_p_rho_phi ( moist, n_moist,                &
                            al, alb, mu, muts, ph, p, pb,  &
                            t, p0, t0, znu, dnw, rdnw,     &
                            rdn, non_hydrostatic,          &
                            ids, ide, jds, jde, kds, kde,  &
                            ims, ime, jms, jme, kms, kme,  &
                            its, ite, jts, jte, kts, kte  )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_al(i,k,j)=al(i,k,j)
      B_p(i,k,j)=p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, K_ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_al(i,k,j)*P_al(i,k,j)  +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
   enddo
   enddo
   enddo

   CALL calc_p_rho_phi ( P_moist, n_moist,                &
                            P_al, alb, P_mu, P_muts, P_ph, P_p, pb,  &
                            P_t, p0, t0, znu, dnw, rdnw,     &
                            rdn, non_hydrostatic,          &
                            ids, ide, jds, jde, kds, kde,  &
                            ims, ime, jms, jme, kms, kme,  &
                            its, ite, jts, jte, kts, kte  )


   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
         VAL_N=VAL_N+(P_ph(i,k,j)-B_ph(i,k,j))*(P_ph(i,k,j)-B_ph(i,k,j))
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
         VAL_N=VAL_N+(P_al(i,k,j)-B_al(i,k,j))*(P_al(i,k,j)-B_al(i,k,j))   &
                    +(P_p(i,k,j) -B_p(i,k,j))*(P_p(i,k,j) -B_p(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calc_p_rho_phi: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      t(i,k,j)=S_t(i,k,j)
      P_t(i,k,j)=FACTOR*S_t(i,k,j)
      B_t(i,k,j)=P_t(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)
      P_moist(i,k,j,h)=FACTOR*S_moist(i,k,j,h)
      B_moist(i,k,j,h)=P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      muts(i,j)=S_muts(i,j)

      P_mu(i,j)=FACTOR*S_mu(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)

      B_mu(i,j)=P_mu(i,j)
      B_muts(i,j)=P_muts(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ph(i,k,j)=S_ph(i,k,j)
      P_ph(i,k,j)=FACTOR*S_ph(i,k,j)
      B_ph(i,k,j)=P_ph(i,k,j)
      K_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L +P_al(i,k,j)*P_al(i,k,j)  +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_t(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      P_moist(i,k,j,h)=0.0
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
      P_muts(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, K_ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_t(i,k,j) *B_t(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      VAL_A=VAL_A +P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)  +P_muts(i,j)*B_muts(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_calc_p_rho_phi: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      t(i,k,j)=S_t(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=1,n_moist
      moist(i,k,j,h)=S_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      ph(i,k,j)=S_ph(i,k,j)
   enddo
   enddo
   enddo

!g_calc_p_rho_phi: ALPHA=.1000E+00  COEF=   0.1006161451340E+01  VAL_N= 0.101051E+11  VAL_L= 0.100433E+11
!g_calc_p_rho_phi: ALPHA=.1000E-01  COEF=   0.1000751256943E+01  VAL_N= 0.100508E+09  VAL_L= 0.100433E+09
!g_calc_p_rho_phi: ALPHA=.1000E-02  COEF=   0.9996134638786E+00  VAL_N= 0.100394E+07  VAL_L= 0.100433E+07
!g_calc_p_rho_phi: ALPHA=.1000E-03  COEF=   0.9992095828056E+00  VAL_N= 0.100353E+05  VAL_L= 0.100433E+05
!g_calc_p_rho_phi: ALPHA=.1000E-04  COEF=   0.1008291125298E+01  VAL_N= 0.101265E+03  VAL_L= 0.100433E+03
!g_calc_p_rho_phi: ALPHA=.1000E-05  COEF=   0.2676813364029E+01  VAL_N= 0.268839E+01  VAL_L= 0.100433E+01
!g_calc_p_rho_phi: ALPHA=.1000E-06  COEF=   0.1743072204590E+03  VAL_N= 0.175061E+01  VAL_L= 0.100433E-01
!g_calc_p_rho_phi: ALPHA=.1000E-07  COEF=   0.1722197265625E+05  VAL_N= 0.172965E+01  VAL_L= 0.100433E-03
!g_calc_p_rho_phi: ALPHA=.1000E-08  COEF=   0.1722197125000E+07  VAL_N= 0.172965E+01  VAL_L= 0.100433E-05
!g_calc_p_rho_phi: ALPHA=.1000E-09  COEF=   0.1722196960000E+09  VAL_N= 0.172965E+01  VAL_L= 0.100433E-07
!g_calc_p_rho_phi: ALPHA=.1000E-10  COEF=   0.1722196787200E+11  VAL_N= 0.172965E+01  VAL_L= 0.100433E-09
                 
!a_calc_p_rho_phi:    0.1004590899200E+11   0.1003517337600E+11

!g_calc_p_rho_phi: ALPHA=.1000E+00  COEF=   0.1006000807140E+01  VAL_N= 0.101045E+11  VAL_L= 0.100442E+11
!g_calc_p_rho_phi: ALPHA=.1000E-01  COEF=   0.1000601117766E+01  VAL_N= 0.100502E+09  VAL_L= 0.100442E+09
!g_calc_p_rho_phi: ALPHA=.1000E-02  COEF=   0.1000061842375E+01  VAL_N= 0.100448E+07  VAL_L= 0.100442E+07
!g_calc_p_rho_phi: ALPHA=.1000E-03  COEF=   0.1000178212958E+01  VAL_N= 0.100460E+05  VAL_L= 0.100442E+05
!g_calc_p_rho_phi: ALPHA=.1000E-04  COEF=   0.1017220683002E+01  VAL_N= 0.102171E+03  VAL_L= 0.100442E+03
!g_calc_p_rho_phi: ALPHA=.1000E-05  COEF=   0.2722008232576E+01  VAL_N= 0.273403E+01  VAL_L= 0.100442E+01
!g_calc_p_rho_phi: ALPHA=.1000E-06  COEF=   0.1732008172974E+03  VAL_N= 0.173966E+01  VAL_L= 0.100442E-01
!g_calc_p_rho_phi: ALPHA=.1000E-07  COEF=   0.1722108172876E+05  VAL_N= 0.172972E+01  VAL_L= 0.100442E-03
!g_calc_p_rho_phi: ALPHA=.1000E-08  COEF=   0.1722009172880E+07  VAL_N= 0.172962E+01  VAL_L= 0.100442E-05
!g_calc_p_rho_phi: ALPHA=.1000E-09  COEF=   0.1722008182880E+09  VAL_N= 0.172962E+01  VAL_L= 0.100442E-07
!g_calc_p_rho_phi: ALPHA=.1000E-10  COEF=   0.1722008172980E+11  VAL_N= 0.172962E+01  VAL_L= 0.100442E-09
                 
!a_calc_p_rho_phi:    0.1004417776524E+11   0.1004417776524E+11


END SUBROUTINE t_calc_p_rho_phi
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_diagnose_w( ph_tend, ph_new, ph_old, w, mu, dt,  &
                       u, v, ht,                            &
                       cf1, cf2, cf3, rdx, rdy, msft,       &
                       ids, ide, jds, jde, kds, kde,        &
                       ims, ime, jms, jme, kms, kme,        &
                       its, ite, jts, jte, kts, kte        )


! Zaizhong Ma, April 6,2005

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme )                ::   ph_tend, &
                                                                     ph_new,  &
                                                                     ph_old,  &
                                                                     u,       &
                                                                     v


   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(  OUT) :: w

   REAL, DIMENSION( ims:ime, jms:jme )    :: mu
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: ht, msft

   REAL, INTENT(IN   ) :: dt, cf1, cf2, cf3, rdx, rdy

   INTEGER :: i, j, k, itf, jtf

!  zzma: new definition

!  IN variables

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme )                ::   S_ph_tend, &
                                                                     S_ph_new,  &
                                                                     S_ph_old,  &
                                                                     S_u,       &
                                                                     S_v
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme )                ::   P_ph_tend, &
                                                                     P_ph_new,  &
                                                                     P_ph_old,  &
                                                                     P_u,       &
                                                                     P_v
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme )                ::   B_ph_tend, &
                                                                     B_ph_new,  &
                                                                     B_ph_old,  &
                                                                     B_u,       &
                                                                     B_v

   REAL, DIMENSION( ims:ime, jms:jme )    :: S_mu,P_mu,B_mu

!  OUT variables

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme )        :: P_w,B_w

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_u(i,k,j)=u(i,k,j)
      S_v(i,k,j)=v(i,k,j)
      S_ph_new(i,k,j)=ph_new(i,k,j)
      S_ph_old(i,k,j)=ph_old(i,k,j)
      S_ph_tend(i,k,j)=ph_tend(i,k,j)

      P_u(i,k,j)=u(i,k,j)
      P_v(i,k,j)=v(i,k,j)
      P_ph_new(i,k,j)=ph_new(i,k,j)
      P_ph_old(i,k,j)=ph_old(i,k,j)
      P_ph_tend(i,k,j)=ph_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)

      P_mu(i,j)=mu(i,j)
   enddo
   enddo

!NLM

   CALL diagnose_w( ph_tend, ph_new, ph_old, w, mu, dt,  &
                       u, v, ht,                            &
                       cf1, cf2, cf3, rdx, rdy, msft,       &
                       ids, ide, jds, jde, kds, kde,        &
                       ims, ime, jms, jme, kms, kme,        &
                       its, ite, jts, jte, kts, kte        )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_w(i,k,j)=w(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_diagnose_w( ph_tend, P_ph_tend, ph_new, P_ph_new, ph_old, P_ph_old, w, P_w, mu, P_mu, dt, u, P_u, v, P_v, ht, cf1, &
&cf2, cf3, rdx, rdy, msft, ide, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L +P_w(i,k,j)*P_w(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_ph_new(i,k,j)=FACTOR*S_ph_new(i,k,j)
      P_ph_old(i,k,j)=FACTOR*S_ph_old(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=FACTOR*S_mu(i,j)
   enddo
   enddo

   CALL diagnose_w( P_ph_tend, P_ph_new, P_ph_old, P_w, P_mu, dt,  &
                       P_u, P_v, ht,                            &
                       cf1, cf2, cf3, rdx, rdy, msft,       &
                       ids, ide, jds, jde, kds, kde,        &
                       ims, ime, jms, jme, kms, kme,        &
                       its, ite, jts, jte, kts, kte        )

      VAL_N=0.
      do i=ims,ime
      do k=kms,kme
      do j=jms,jme
         VAL_N=VAL_N+(P_w(i,k,j)- B_w(i,k,j))*(P_w(i,k,j)- B_w(i,k,j)) 
      enddo
      enddo
      enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_diagnose_w: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      ph_new(i,k,j)=S_ph_new(i,k,j)
      ph_old(i,k,j)=S_ph_old(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)

      P_u(i,k,j)=FACTOR*S_u(i,k,j)
      P_v(i,k,j)=FACTOR*S_v(i,k,j)
      P_ph_new(i,k,j)=FACTOR*S_ph_new(i,k,j)
      P_ph_old(i,k,j)=FACTOR*S_ph_old(i,k,j)
      P_ph_tend(i,k,j)=FACTOR*S_ph_tend(i,k,j)

      B_u(i,k,j)=P_u(i,k,j)
      B_v(i,k,j)=P_v(i,k,j)
      B_ph_new(i,k,j)=P_ph_new(i,k,j)
      B_ph_old(i,k,j)=P_ph_old(i,k,j)
      B_ph_tend(i,k,j)=P_ph_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
      P_mu(i,j)=FACTOR*S_mu(i,j)
      B_mu(i,j)=P_mu(i,j)
   enddo
   enddo

!  TGL

   CALL  g_diagnose_w( ph_tend, P_ph_tend, ph_new, P_ph_new, ph_old, P_ph_old, w, P_w, mu, P_mu, dt, u, P_u, v, P_v, ht, cf1, &
&cf2, cf3, rdx, rdy, msft, ide, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_w(i,k,j)*P_w(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_u(i,k,j)=0.0
      P_v(i,k,j)=0.0
      P_ph_new(i,k,j)=0.0
      P_ph_old(i,k,j)=0.0
      P_ph_tend(i,k,j)=0.0
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_diagnose_w( ph_tend, P_ph_tend, P_ph_new, P_ph_old, P_w, mu, P_mu, dt, P_u, P_v, ht, cf1, cf2, cf3, rdx, rdy, msft, &
&ide, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A +P_u(i,k,j)*B_u(i,k,j)              &
               +P_v(i,k,j)*B_v(i,k,j)              &
               +P_ph_new(i,k,j)*B_ph_new(i,k,j)    &
               +P_ph_old(i,k,j)*B_ph_old(i,k,j)    &
               +P_ph_tend(i,k,j)*B_ph_tend(i,k,j) 
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_diagnose_w: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      u(i,k,j)=S_u(i,k,j)
      v(i,k,j)=S_v(i,k,j)
      ph_new(i,k,j)=S_ph_new(i,k,j)
      ph_old(i,k,j)=S_ph_old(i,k,j)
      ph_tend(i,k,j)=S_ph_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

!g_diagnose_w: ALPHA=.1000E+00  COEF=   0.1000005245209E+01  VAL_N= 0.670413E+01  VAL_L= 0.670409E+01
!g_diagnose_w: ALPHA=.1000E-01  COEF=   0.1000001430511E+01  VAL_N= 0.670410E-01  VAL_L= 0.670409E-01
!g_diagnose_w: ALPHA=.1000E-02  COEF=   0.1000097274780E+01  VAL_N= 0.670475E-03  VAL_L= 0.670409E-03
!g_diagnose_w: ALPHA=.1000E-03  COEF=   0.1000367045403E+01  VAL_N= 0.670656E-05  VAL_L= 0.670409E-05
!g_diagnose_w: ALPHA=.1000E-04  COEF=   0.1005415797234E+01  VAL_N= 0.674040E-07  VAL_L= 0.670409E-07
!g_diagnose_w: ALPHA=.1000E-05  COEF=   0.9407595992088E+00  VAL_N= 0.630694E-09  VAL_L= 0.670410E-09
!g_diagnose_w: ALPHA=.1000E-06  COEF=   0.8932945728302E+00  VAL_N= 0.598873E-11  VAL_L= 0.670410E-11
!g_diagnose_w: ALPHA=.1000E-07  COEF=   0.2442521740420E-08  VAL_N= 0.163749E-21  VAL_L= 0.670410E-13
!g_diagnose_w: ALPHA=.1000E-08  COEF=   0.2442521633839E-06  VAL_N= 0.163749E-21  VAL_L= 0.670410E-15
!g_diagnose_w: ALPHA=.1000E-09  COEF=   0.2442521326884E-04  VAL_N= 0.163749E-21  VAL_L= 0.670410E-17
!g_diagnose_w: ALPHA=.1000E-10  COEF=   0.2442521275952E-02  VAL_N= 0.163749E-21  VAL_L= 0.670410E-19
                 
!a_diagnose_w:    0.6704125404358E+01   0.6704235553741E+01


!g_diagnose_w: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.670470E+01  VAL_L= 0.670470E+01
!g_diagnose_w: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.670470E-01  VAL_L= 0.670470E-01
!g_diagnose_w: ALPHA=.1000E-02  COEF=   0.9999999999999E+00  VAL_N= 0.670470E-03  VAL_L= 0.670470E-03
!g_diagnose_w: ALPHA=.1000E-03  COEF=   0.9999999999994E+00  VAL_N= 0.670470E-05  VAL_L= 0.670470E-05
!g_diagnose_w: ALPHA=.1000E-04  COEF=   0.1000000000010E+01  VAL_N= 0.670470E-07  VAL_L= 0.670470E-07
!g_diagnose_w: ALPHA=.1000E-05  COEF=   0.9999999998131E+00  VAL_N= 0.670470E-09  VAL_L= 0.670470E-09
!g_diagnose_w: ALPHA=.1000E-06  COEF=   0.1000000001353E+01  VAL_N= 0.670470E-11  VAL_L= 0.670470E-11
!g_diagnose_w: ALPHA=.1000E-07  COEF=   0.9999999833020E+00  VAL_N= 0.670470E-13  VAL_L= 0.670470E-13
!g_diagnose_w: ALPHA=.1000E-08  COEF=   0.1000000134166E+01  VAL_N= 0.670470E-15  VAL_L= 0.670470E-15
!g_diagnose_w: ALPHA=.1000E-09  COEF=   0.1000000233388E+01  VAL_N= 0.670470E-17  VAL_L= 0.670470E-17
!g_diagnose_w: ALPHA=.1000E-10  COEF=   0.9999982203515E+00  VAL_N= 0.670469E-19  VAL_L= 0.670470E-19
                 
!a_diagnose_w:    0.6704701074044E+01   0.6704701074044E+01

END SUBROUTINE t_diagnose_w
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_spec_bdyupdate(  field,      &
                               field_tend, dt,            &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )


      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: field_tend
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist

!  zzma: new definition

!IN variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field_tend,P_field_tend,B_field_tend

!INOUT variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field,P_field,B_field,K_field

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   INTEGER :: NT

!  zzma: new definition end

!TGL test

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_field_tend(i,k,j)=field_tend(i,k,j)

      P_field_tend(i,k,j)=field_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_field(i,k,j)=field(i,k,j)

      P_field(i,k,j)=field(i,k,j)

      K_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!NLM

   CALL spec_bdyupdate(  field,      &
                               field_tend, dt,            &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      B_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_spec_bdyupdate( K_field, P_field, field_tend, P_field_tend, dt, variable_in, spec_zone, ids, ide, jds, jde, kde, ims, &
&ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      SAVE_L=SAVE_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

   ALPHA=1.
   DO NT=1,11
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_field_tend(i,k,j)=FACTOR*S_field_tend(i,k,j)
      P_field(i,k,j)=FACTOR*S_field(i,k,j)
   enddo
   enddo
   enddo

   CALL spec_bdyupdate(  P_field,      &
                               P_field_tend, dt,            &
                               variable_in, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   VAL_N=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_N=VAL_N+(P_field(i,k,j) -B_field(i,k,j))*(P_field(i,k,j) -B_field(i,k,j))
   enddo
   enddo
   enddo

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_spec_bdyupdate: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

!  ADJ test

   FACTOR=0.1
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field_tend(i,k,j)=S_field_tend(i,k,j)
      P_field_tend(i,k,j)=FACTOR*S_field_tend(i,k,j)
      B_field_tend(i,k,j)=P_field_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field(i,k,j)=S_field(i,k,j)
      P_field(i,k,j)=FACTOR*S_field(i,k,j)
      B_field(i,k,j)=P_field(i,k,j)
      K_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

   CALL g_spec_bdyupdate( field, P_field, field_tend, P_field_tend, dt, variable_in, spec_zone, ids, ide, jds, jde, kde, ims, &
&ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_L=VAL_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_field_tend(i,k,j)=0.0
   enddo
   enddo
   enddo

!  ADJ

   CALL a_spec_bdyupdate( P_field, P_field_tend, dt, variable_in, spec_zone, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, &
&kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_field_tend(i,k,j)*B_field_tend(i,k,j) 
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      VAL_A=VAL_A + P_field(i,k,j)*B_field(i,k,j)
   enddo
   enddo
   enddo

   print*, '                '
   write(6,fmt='(A,2E22.13)') 'a_spec_bdyupdate: ', VAL_L,VAL_A

!  RECOVER

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field_tend(i,k,j)=S_field_tend(i,k,j)
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      field(i,k,j)=S_field(i,k,j)
   enddo
   enddo
   enddo

!g_spec_bdyupdate: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.494271E+11  VAL_L= 0.494271E+11
!g_spec_bdyupdate: ALPHA=.1000E-01  COEF=   0.9999986886978E+00  VAL_N= 0.494271E+09  VAL_L= 0.494272E+09
!g_spec_bdyupdate: ALPHA=.1000E-02  COEF=   0.1000093936920E+01  VAL_N= 0.494318E+07  VAL_L= 0.494272E+07
!g_spec_bdyupdate: ALPHA=.1000E-03  COEF=   0.1000092506409E+01  VAL_N= 0.494317E+05  VAL_L= 0.494272E+05
!g_spec_bdyupdate: ALPHA=.1000E-04  COEF=   0.1003426194191E+01  VAL_N= 0.495965E+03  VAL_L= 0.494272E+03
!g_spec_bdyupdate: ALPHA=.1000E-05  COEF=   0.9386174082756E+00  VAL_N= 0.463932E+01  VAL_L= 0.494272E+01
!g_spec_bdyupdate: ALPHA=.1000E-06  COEF=   0.1423480510712E+01  VAL_N= 0.703586E-01  VAL_L= 0.494272E-01
!g_spec_bdyupdate: ALPHA=.1000E-07  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.494272E-03
!g_spec_bdyupdate: ALPHA=.1000E-08  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.494272E-05
!g_spec_bdyupdate: ALPHA=.1000E-09  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.494272E-07
!g_spec_bdyupdate: ALPHA=.1000E-10  COEF=   0.0000000000000E+00  VAL_N= 0.000000E+00  VAL_L= 0.494272E-09
                 
!a_spec_bdyupdate:    0.4942713241600E+11   0.4942713241600E+11

!g_spec_bdyupdate: ALPHA=.1000E+00  COEF=   0.1000000000000E+01  VAL_N= 0.494266E+11  VAL_L= 0.494266E+11
!g_spec_bdyupdate: ALPHA=.1000E-01  COEF=   0.1000000000000E+01  VAL_N= 0.494266E+09  VAL_L= 0.494266E+09
!g_spec_bdyupdate: ALPHA=.1000E-02  COEF=   0.9999999999998E+00  VAL_N= 0.494266E+07  VAL_L= 0.494266E+07
!g_spec_bdyupdate: ALPHA=.1000E-03  COEF=   0.9999999999999E+00  VAL_N= 0.494266E+05  VAL_L= 0.494266E+05
!g_spec_bdyupdate: ALPHA=.1000E-04  COEF=   0.1000000000013E+01  VAL_N= 0.494266E+03  VAL_L= 0.494266E+03
!g_spec_bdyupdate: ALPHA=.1000E-05  COEF=   0.9999999998369E+00  VAL_N= 0.494266E+01  VAL_L= 0.494266E+01
!g_spec_bdyupdate: ALPHA=.1000E-06  COEF=   0.1000000001169E+01  VAL_N= 0.494266E-01  VAL_L= 0.494266E-01
!g_spec_bdyupdate: ALPHA=.1000E-07  COEF=   0.9999999879249E+00  VAL_N= 0.494266E-03  VAL_L= 0.494266E-03
!g_spec_bdyupdate: ALPHA=.1000E-08  COEF=   0.1000000181960E+01  VAL_N= 0.494266E-05  VAL_L= 0.494266E-05
!g_spec_bdyupdate: ALPHA=.1000E-09  COEF=   0.9999999928643E+00  VAL_N= 0.494266E-07  VAL_L= 0.494266E-07
!g_spec_bdyupdate: ALPHA=.1000E-10  COEF=   0.9999993623163E+00  VAL_N= 0.494266E-09  VAL_L= 0.494266E-09
                 
!a_spec_bdyupdate:    0.4942663099544E+11   0.4942663099544E+11

END SUBROUTINE t_spec_bdyupdate
!-----------------------------------------------------------------------------------------------

END MODULE module_check
