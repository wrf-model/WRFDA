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

#ifdef DM_PARALLEL
   include "mpif.h"
   REAL     ::  nsum
   INTEGER  ::  comm, ierror
#endif

   ! bp - 090904; Initial ALPHA value in computing TL test perturbation
   real, parameter :: alphaInit = 1.0 
   ! bp - 090904; Numer of iterations in TL differencing test
   integer, parameter :: Ndx = 11 
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
   REAL :: PERTURBATION                 ! bp 090903 - perturbation value specified in namelist.input
   INTEGER :: NT

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

   SAVE_L=sum(P_tendency(its:ite,kts:kte,jts:jte)*P_tendency(its:ite,kts:kte,jts:jte))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      VAL_N=sum((P_tendency(its:ite,kts:kte,jts:jte)-B_tendency(its:ite,kts:kte,jts:jte))*  &
                     (P_tendency(its:ite,kts:kte,jts:jte)-B_tendency(its:ite,kts:kte,jts:jte)))
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif
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

   VAL_L=sum(P_tendency(its:ite,kts:kte,jts:jte)*P_tendency(its:ite,kts:kte,jts:jte))
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

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
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte  )
   VAL_A=sum(P_field(its:ite,kts:kte,jts:jte)*B_field(its:ite,kts:kte,jts:jte)) + &
         sum(P_field_old(its:ite,kts:kte,jts:jte)*B_field_old(its:ite,kts:kte,jts:jte))+ &
         sum(P_tendency(its:ite,kts:kte,jts:jte)*B_tendency(its:ite,kts:kte,jts:jte)) + &
         sum(P_ru(its:ite,kts:kte,jts:jte)*B_ru(its:ite,kts:kte,jts:jte)) +  &
         sum(P_rv(its:ite,kts:kte,jts:jte)*B_rv(its:ite,kts:kte,jts:jte))+ &
         sum(P_rom(its:ite,kts:kte,jts:jte)*B_rom(its:ite,kts:kte,jts:jte))
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_advect_scalar: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic, leapfrog,                       &
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


   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type)    ,           INTENT(IN   ) :: config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL ,               INTENT(IN   ) :: non_hydrostatic, leapfrog

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

   REAL    :: kdift, khdq, kvdq, cfn, cfn1, cf1, cf2, cf3
   INTEGER :: i,j,k

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
   REAL :: PERTURBATION
   INTEGER :: NT

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
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic, leapfrog,                       &
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
&P_xkmhd, cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, leapfrog, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
                         cf1, cf2, cf3, cfn, cfn1, n_moist,                         &
                         non_hydrostatic, leapfrog,                                 &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

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
&P_xkmhd, cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, leapfrog, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

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

   CALL a_rk_tendency1( config_flags, rk_step, P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend, P_ru_tendf, P_rv_tendf, &
&P_rw_tendf, P_t_tendf, P_mu_tend, P_u_save, P_v_save, P_w_save, P_ph_save, P_t_save, ru, P_ru, rv, P_rv, rw, P_rw, ww, P_ww, u, &
&P_u, v, P_v, w, P_w, t, P_t, ph, P_ph, u_old, P_u_old, v_old, P_v_old, w_old, P_w_old, t_old, P_t_old, ph_old, P_ph_old, phb, &
&t_init, mu, P_mu, mut, P_mut, muu, P_muu, muv, P_muv, mub, al, P_al, alt, P_alt, p, P_p, pb, php, P_php, cqu, P_cqu, cqv, P_cqv, &
&K_cqw, P_cqw, u_base, v_base, z_base, msfu, msfv, msft, f, e, sina, cosa, fnm, fnp, rdn, rdnw, dt, rdx, rdy, kvdif, xkmhd, P_xkmhd, &
&cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, leapfrog, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&kts, kte )

#ifdef DM_PARALLEL
!lineno = __LINE__
!#    include "HALO_3803.inc"

#endif

   CALL a_rk_tendency2( config_flags, rk_step, P_ru_tend, P_rv_tend, P_rw_tend, P_ph_tend, P_t_tend, P_ru_tendf, P_rv_tendf, &
&P_rw_tendf, P_t_tendf, P_mu_tend, P_u_save, P_v_save, P_w_save, P_ph_save, P_t_save, ru, P_ru, rv, P_rv, rw, P_rw, ww, P_ww, u, &
&P_u, v, P_v, w, P_w, t, P_t, ph, P_ph, u_old, P_u_old, v_old, P_v_old, w_old, P_w_old, t_old, P_t_old, ph_old, P_ph_old, phb, &
&t_init, mu, P_mu, mut, P_mut, muu, P_muu, muv, P_muv, mub, al, P_al, alt, P_alt, p, P_p, pb, php, P_php, cqu, P_cqu, cqv, P_cqv, &
&K_cqw, P_cqw, u_base, v_base, z_base, msfu, msfv, msft, f, e, sina, cosa, fnm, fnp, rdn, rdnw, dt, rdx, rdy, kvdif, xkmhd, P_xkmhd, &
&cf1, cf2, cf3, cfn, cfn1, non_hydrostatic, leapfrog, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_rk_tendency: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

   do i=its,ite
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L + P_muu(i,j)*P_muu(i,j)      &
                    + P_muv(i,j)*P_muv(i,j)      &
                    + P_mut(i,j)*P_mut(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL      
   call wrf_get_dm_communicator(comm)
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum        
#endif                  

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
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
      do i=its,ite
      do j=jts,jte
         VAL_N=VAL_N+(P_muu(i,j) - B_muu(i,j))*(P_muu(i,j) - B_muu(i,j))    &
               +(P_muv(i,j) - B_muv(i,j))*(P_muv(i,j) - B_muv(i,j))         &
               +(P_mut(i,j) - B_mut(i,j))*(P_mut(i,j) - B_mut(i,j))
      enddo
      enddo

#ifdef DM_PARALLEL      
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum        
#endif                  

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L   + P_muu(i,j)*P_muu(i,j)      &
                    + P_muv(i,j)*P_muv(i,j)      &
                    + P_mut(i,j)*P_mut(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL      
      call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_L = nsum
#endif  

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
&jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)      &
                  + P_v(i,k,j)*B_v(i,k,j)      &
                  + P_w(i,k,j)*B_w(i,k,j)      &
                  + P_ph(i,k,j)*B_ph(i,k,j)      &
                  + P_al(i,k,j)*B_al(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      VAL_A=VAL_A + P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL      
      call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_A = nsum
#endif  

   print*, '                '
   write(6,*) 'a_rk_step_prep: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_rk_step_prep

!---------------------------------------------------------------------------------------------------

SUBROUTINE t_init_zero_tendency(ru_tendf, rv_tendf, rw_tendf, ph_tendf,  &
                              t_tendf,  tke_tendf,                     &
                              moist_tendf,chem_tendf,                  &
                              n_moist,n_chem,rk_step,                  &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )
   IMPLICIT NONE

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   INTEGER ,       INTENT(IN   ) :: n_moist,n_chem,rk_step

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

! LOCAL VARS

   INTEGER :: im, ic,i,j,k,h

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
                                                             P_t_tendf
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

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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
                              moist_tendf,chem_tendf,                  &
                              n_moist,n_chem,rk_step,                  &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_ph_tendf(i,k,j)=ph_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      B_moist_tendf(i,k,j,h)=moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL
   CALL g_init_zero_tendency( ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, rw_tendf, P_rw_tendf, ph_tendf, P_ph_tendf, t_tendf, &
&P_t_tendf, moist_tendf, P_moist_tendf, n_moist, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L+ P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)    &
                   + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)    &
                   + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)    &
                   + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)    &
                   + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      SAVE_L=SAVE_L + P_moist_tendf(i,k,j,h)*P_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
                              P_moist_tendf,chem_tendf,                  &
                              n_moist,n_chem,rk_step,                  &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )

   VAL_N=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N +(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))   &
                  +(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))      &
                  +(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))      &
                  +(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))*(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))      &
                  +(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      VAL_N=VAL_N +(P_moist_tendf(i,k,j,h)-B_moist_tendf(i,k,j,h))*(P_moist_tendf(i,k,j,h)-B_moist_tendf(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

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
&P_t_tendf, moist_tendf, P_moist_tendf, n_moist, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                  +P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                  +P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                  +P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)   &
                  +P_t_tendf(i,k,j)*P_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      VAL_L=VAL_L +P_moist_tendf(i,k,j,h)*P_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, & 
                       comm, IERROR )
   VAL_L = nsum              
#endif                 

!  ADJ

   CALL a_init_zero_tendency( P_ru_tendf, P_rv_tendf, P_rw_tendf, P_ph_tendf, P_t_tendf, P_moist_tendf, n_moist, ims, ime, jms, &
&jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   DO I=its,ite
   DO K=kts,kte
   DO J=jts,jte
!      VAL_A=VAL_A +P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)  &
!                  +P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)  &
!                  +P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)  &
!                  +P_ph_tendf(i,k,j)*B_ph_tendf(i,k,j)  &
!                  +P_t_tendf(i,k,j)*B_t_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
!      VAL_A=VAL_A +P_moist_tendf(i,k,j,h)*B_moist_tendf(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, & 
                       comm, IERROR )
   VAL_A = nsum              
#endif                 

   print*, '                '
   write(6,*) 'a_init_zero_tendency: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
   REAL :: PERTURBATION
   INTEGER :: NT

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


   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, & 
                       comm, IERROR )
   SAVE_L = nsum              
#endif                 

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, & 
                       comm, IERROR )
   VAL_N = nsum              
#endif                 

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   DO I=its,ite
   DO K=kts,kte
   DO J=jts,jte
      VAL_A=VAL_A    + P_p(i,k,j)*B_p(i,k,j)      &
                     + P_ph(i,k,j)*B_ph(i,k,j)  &
                     + P_t(i,k,j)*B_t(i,k,j)

   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_phy_prep: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

!  IN variables

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  S_p8w, S_t8w, S_theta, S_t, S_p
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  P_p8w, P_t8w, P_theta, P_t, P_p
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme )     ::  B_p8w, B_t8w, B_theta, B_t, B_p

!  INOUT variables

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist )   :: S_moist,P_moist,B_moist,K_moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme )      :: S_xkmhd,S_BN2,P_xkmhd,P_BN2,B_xkmhd,B_BN2,K_xkmhd,K_BN2

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      B_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      SAVE_L=SAVE_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L+P_xkmhd(i,k,j)*P_xkmhd(i,k,j)  +P_bn2(i,k,j)*P_bn2(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
      do h=1  ,n_moist
         VAL_N=VAL_N+(P_moist(i,k,j,h)-B_moist(i,k,j,h))*(P_moist(i,k,j,h)-B_moist(i,k,j,h))
      enddo
      enddo
      enddo
      enddo

      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N + (P_xkmhd(i,k,j)-B_xkmhd(i,k,j))*(P_xkmhd(i,k,j)-B_xkmhd(i,k,j))   & 
                     + (P_bn2(i,k,j) -B_bn2(i,k,j))*(P_bn2(i,k,j) -B_bn2(i,k,j))
      enddo
      enddo
      enddo

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
!      VAL_L=VAL_L + P_p8w(i,k,j)*P_p8w(i,k,j)       &
!                  + P_t8w(i,k,j)*P_t8w(i,k,j)       &
!                  + P_theta(i,k,j)*P_theta(i,k,j)   &
!                  + P_t(i,k,j)*P_t(i,k,j)           &
!                  + P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      VAL_L=VAL_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L+P_xkmhd(i,k,j)*P_xkmhd(i,k,j)  +P_bn2(i,k,j)*P_bn2(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   DO I=its,ite
   DO K=kts,kte
   DO J=jts,jte
      VAL_A=VAL_A + P_p8w(i,k,j)*B_p8w(i,k,j)      & 
                  + P_t8w(i,k,j)*B_t8w(i,k,j)      &
                  + P_theta(i,k,j)*B_theta(i,k,j)  &
                  + P_t(i,k,j) *B_t(i,k,j)         &
                  + P_p(i,k,j) *B_p(i,k,j)
   END DO
   END DO
   END DO

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1  ,n_moist
      VAL_A=VAL_A + P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_xkmhd(i,k,j)*B_xkmhd(i,k,j) +P_bn2(i,k,j)*B_bn2(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_calculate_km_kh: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
   REAL :: PERTURBATION
   INTEGER :: NT,h

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ru_tendf(i,k,j)=ru_tendf(i,k,j)
      B_rv_tendf(i,k,j)=rv_tendf(i,k,j)
      B_rw_tendf(i,k,j)=rw_tendf(i,k,j)
      B_t_tendf(i,k,j)=t_tendf(i,k,j)
      B_ph_tendf(i,k,j)=ph_tendf(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)     &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L + P_mu_tend(i,j)*P_mu_tend(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N + (P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))*(P_ru_tendf(i,k,j)-B_ru_tendf(i,k,j))  &
                     + (P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))*(P_rv_tendf(i,k,j)-B_rv_tendf(i,k,j))  &
                     + (P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))*(P_rw_tendf(i,k,j)-B_rw_tendf(i,k,j))  &
                     + (P_t_tendf(i,k,j)-B_t_tendf(i,k,j))*(P_t_tendf(i,k,j)-B_t_tendf(i,k,j))      &
                     + (P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))*(P_ph_tendf(i,k,j)-B_ph_tendf(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
         VAL_N=VAL_N + (P_mu_tend(i,j)-B_mu_tend(i,j))*(P_mu_tend(i,j)-B_mu_tend(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_ru_tendf(i,k,j)*P_ru_tendf(i,k,j)   &
                    + P_rv_tendf(i,k,j)*P_rv_tendf(i,k,j)   &
                    + P_rw_tendf(i,k,j)*P_rw_tendf(i,k,j)   &
                    + P_t_tendf(i,k,j)*P_t_tendf(i,k,j)     &
                    + P_ph_tendf(i,k,j)*P_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L + P_mu_tend(i,j)*P_mu_tend(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_ru(i,k,j)*B_ru(i,k,j)    &
              + P_rv(i,k,j)*B_rv(i,k,j)        &
              + P_w(i,k,j)*B_w(i,k,j)          &
              + P_t(i,k,j)*B_t(i,k,j)          &
              + P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_ru_tendf(i,k,j)*B_ru_tendf(i,k,j)    &
              + P_rv_tendf(i,k,j)*B_rv_tendf(i,k,j)    &
              + P_rw_tendf(i,k,j)*B_rw_tendf(i,k,j)    &
              + P_t_tendf(i,k,j)*B_t_tendf(i,k,j)    &
              + P_ph_tendf(i,k,j)*B_ph_tendf(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_relax_bdy_dry: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

   CALL g_rk_addtend_dry( K_ru_tend, P_ru_tend, K_rv_tend, P_rv_tend, K_rw_tend, P_rw_tend, K_ph_tend, P_ph_tend, K_t_tend, P_t_tend, &
&K_ru_tendf, P_ru_tendf, K_rv_tendf, P_rv_tendf, K_rw_tendf, P_rw_tendf, K_ph_tendf, P_ph_tendf, K_t_tendf, P_t_tendf, u_save, P_u_save, &
&v_save, P_v_save, w_save, P_w_save, ph_save, P_ph_save, t_save, P_t_save, rk_step, h_diabatic, mut, P_mut, msft, msfu, msfv, ide, &
&jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
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

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_u_save(i,k,j)*B_u_save(i,k,j)      &
               + P_v_save(i,k,j)*B_v_save(i,k,j)         &
               + P_w_save(i,k,j)*B_w_save(i,k,j)         &
               + P_t_save(i,k,j)*B_t_save(i,k,j)         &
               + P_ph_save(i,k,j)*B_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mut(i,j)*B_mut(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_rk_addtend_dry: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
   REAL :: PERTURBATION
   INTEGER :: NT,i,j,k,h

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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum 
#endif

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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_spec_bdy_dry: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
                            rk_step, leapfrog,            &
                            ids,ide, jds,jde, kds,kde,    &
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

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

  LOGICAL, INTENT(IN   ) :: leapfrog

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv

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
   REAL :: PERTURBATION
   INTEGER :: NT

!TGL test

   P_muus = 0.0
   P_muvs = 0.0
   P_muts = 0.0
   P_mu_save = 0.0
   P_mudf = 0.0

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
                            rk_step, leapfrog,            &
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
&P_mu_save, ww, P_ww, ww_save, P_ww_save, c2a, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, msft, rk_step, leapfrog, ide, jde, kde, &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
                            rk_step, leapfrog,            &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
&P_mu_save, ww, P_ww, ww_save, P_ww_save, c2a, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, msft, rk_step, leapfrog, ide, jde, kde, &
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
&msft, rk_step, leapfrog, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

!   CALL a_small_step_prep( K_u_1, P_u_1, K_u_2, P_u_2, K_v_1, P_v_1, K_v_2, P_v_2, K_w_1, P_w_1, K_w_2, P_w_2, K_t_1, P_t_1, K_t_2, P_t_2, ph_1,&
!& P_ph_1, ph_2, P_ph_2, mub, K_mu_1, P_mu_1, K_mu_2, P_mu_2, K_muu, P_muu, muus, P_muus, K_muv, P_muv, muvs, P_muvs, K_mut, P_mut, muts, &
!&P_muts, mudf, P_mudf, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, P_t_save, ph_save, P_ph_save, mu_save, &
!&P_mu_save, ww, P_ww, ww_save, P_ww_save, c2a, P_c2a, pb, p, P_p, alt, P_alt, msfu, msfv, msft, rk_step, leapfrog, ide, jde, kde, &
!&ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_small_step_prep: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme)   :: P_al, P_p,B_al, B_p,S_al,S_p

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_al(i,k,j)=al(i,k,j)
      S_p(i,k,j)=p(i,k,j)

      P_al(i,k,j)=al(i,k,j)
      P_p(i,k,j)=p(i,k,j)
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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_al(i,k,j)=al(i,k,j)
      B_p(i,k,j)=p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ph(i,k,j)=ph(i,k,j)
      B_pm1(i,k,j)=pm1(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_alt(i,k,j)=alt(i,k,j)
      B_t_2(i,k,j)=t_2(i,k,j)
      B_t_1(i,k,j)=t_1(i,k,j)
      B_c2a(i,k,j)=c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      B_mu(i,j)=mu(i,j)
      B_muts(i,j)=muts(i,j)
   enddo
   enddo

!  TCL

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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      al(i,k,j)=S_al(i,k,j)
      p(i,k,j)=S_p(i,k,j)
   enddo
   enddo
   enddo


   CALL g_calc_p_rho( al, P_al, p, P_p, ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, pm1, P_pm1, mu, P_mu, muts, &
&P_muts, znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, &
&kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_al(i,k,j)*P_al(i,k,j) +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_ph(i,k,j)*P_ph(i,k,j) + P_pm1(i,k,j)*P_pm1(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_alt(i,k,j)*P_alt(i,k,j)   &
                  + P_t_2(i,k,j)*P_t_2(i,k,j)   &
                  + P_t_1(i,k,j)*P_t_1(i,k,j)   &
                  + P_c2a(i,k,j)*P_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L + P_mu(i,j)*P_mu(i,j)   &
                  + P_muts(i,j)*P_muts(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N + (P_al(i,k,j)- B_al(i,k,j))*(P_al(i,k,j)- B_al(i,k,j))  &
                  + (P_p(i,k,j) - B_p(i,k,j))*(P_p(i,k,j) - B_p(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
         VAL_N=VAL_N + (P_ph(i,k,j)- B_ph(i,k,j))*(P_ph(i,k,j)- B_ph(i,k,j))  &
                     + (P_pm1(i,k,j)-B_pm1(i,k,j))*(P_pm1(i,k,j)-B_pm1(i,k,j))
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N + (P_alt(i,k,j)-B_alt(i,k,j))*(P_alt(i,k,j)-B_alt(i,k,j))   &
                  + (P_t_2(i,k,j)-B_t_2(i,k,j))*(P_t_2(i,k,j)-B_t_2(i,k,j))   &
                  + (P_t_1(i,k,j)-B_t_1(i,k,j))*(P_t_1(i,k,j)-B_t_1(i,k,j))   &
                  + (P_c2a(i,k,j)-B_c2a(i,k,j))*(P_c2a(i,k,j)-B_c2a(i,k,j))
   enddo
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N +(P_mu(i,j)-B_mu(i,j))*(P_mu(i,j)-B_mu(i,j))     &
                  +(P_muts(i,j)-B_muts(i,j))*(P_muts(i,j)-B_muts(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif


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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      al(i,k,j)=S_al(i,k,j)
      p(i,k,j)=S_p(i,k,j)

      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)

      B_al(i,k,j)=P_al(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calc_p_rho( al, P_al, p, P_p, ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, pm1, P_pm1, mu, P_mu, muts, &
&P_muts, znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, &
&kte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_al(i,k,j)*P_al(i,k,j) +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_ph(i,k,j)*P_ph(i,k,j) + P_pm1(i,k,j)*P_pm1(i,k,j)
   enddo
   enddo
   enddo


   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_alt(i,k,j)*B_alt(i,k,j)   &
                  + P_t_2(i,k,j)*B_t_2(i,k,j)   &
                  + P_t_1(i,k,j)*B_t_1(i,k,j)   &
                  + P_c2a(i,k,j)*B_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L  + P_mu(i,j)*B_mu(i,j)   &
                  + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

!  ADJ

   CALL a_calc_p_rho( al, P_al, p, P_p, ph, P_ph, alt, P_alt, t_2, P_t_2, t_1, P_t_1, c2a, P_c2a, P_pm1, mu, P_mu, muts, P_muts,&
& znu, t0, rdnw, dnw, smdiv, non_hydrostatic, step, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_alt(i,k,j)*B_alt(i,k,j)   &
                  + P_t_2(i,k,j)*B_t_2(i,k,j)   &
                  + P_t_1(i,k,j)*B_t_1(i,k,j)   &
                  + P_c2a(i,k,j)*B_c2a(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)   &
                  + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_ph(i,k,j)*B_ph(i,k,j)   &
                  + P_pm1(i,k,j)*B_pm1(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_al(i,k,j)*P_al(i,k,j) +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_calc_p_rho: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A


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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      al(i,k,j)=S_al(i,k,j)
      p(i,k,j)=S_p(i,k,j)
   enddo
   enddo
   enddo

END SUBROUTINE t_calc_p_rho

!-----------------------------------------------------------------------------------------------

SUBROUTINE t_calc_coef_w( a,alpha,gamma,              &
                        mut, cqw,                   &
                        rdn, rdnw, c2a,             &
                        dts, g, epssm,              &
                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                        its,ite, jts,jte, kts,kte  )  ! tile   dims

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

!  IN variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)     :: S_c2a, S_cqw,P_c2a, P_cqw,B_c2a, B_cqw
  REAL, DIMENSION(ims:ime, jms:jme)              :: S_mut,P_mut,B_mut

! INOUT variables

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme)  :: S_alpha,S_gamma,S_a,P_alpha,P_gamma,P_a,  &
                                                 K_alpha,K_gamma,K_a,B_alpha,B_gamma,B_a

   REAL :: SAVE_L, COEF, ALPHA_P, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_alpha(i,k,j)*P_alpha(i,k,j)     &
                    + P_gamma(i,k,j)*P_gamma(i,k,j)     &
                    + P_a(i,k,j)*P_a(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA_P= alphaInit + PERTURBATION
   DO NT=1, Ndx
      ALPHA_P=0.1*ALPHA_P
      FACTOR=1.+ALPHA_P
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N + (P_alpha(i,k,j) -B_alpha(i,k,j))*(P_alpha(i,k,j) -B_alpha(i,k,j))    &
                  + (P_gamma(i,k,j) -B_gamma(i,k,j))*(P_gamma(i,k,j) -B_gamma(i,k,j))    &
                  + (P_a(i,k,j) -B_a(i,k,j))*(P_a(i,k,j) -B_a(i,k,j))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

      VAL_L=SAVE_L*ALPHA_P**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_calc_coef_w: ALPHA_P=',ALPHA_P,'  COEF=',COEF, &
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_alpha(i,k,j)*P_alpha(i,k,j)     &
                    + P_gamma(i,k,j)*P_gamma(i,k,j)     &
                    + P_a(i,k,j)*P_a(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_c2a(i,k,j)*B_c2a(i,k,j)    &
                  + P_cqw(i,k,j)*B_cqw(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mut(i,j)*B_mut(i,j) 
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_alpha(i,k,j)*B_alpha(i,k,j)   &
                  + P_gamma(i,k,j)*B_gamma(i,k,j)   &
                  + P_a(i,k,j)*B_a(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_calc_coef_w: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A


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
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_u(i,k,j)*P_u(i,k,j)    &
                    + P_v(i,k,j)*P_v(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N+(P_u(i,k,j)- B_u(i,k,j))*(P_u(i,k,j)- B_u(i,k,j))       &
                    +(P_v(i,k,j)- B_v(i,k,j))*(P_v(i,k,j)- B_v(i,k,j)) 
      enddo
      enddo
      enddo

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_u(i,k,j)*P_u(i,k,j)    &
                    + P_v(i,k,j)*P_v(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)      &
                  + P_mudf(i,j)*B_mudf(i,j)  &
                  + P_muu(i,j)*B_muu(i,j)    &
                  + P_muv(i,j)*B_muv(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)    &
                  + P_v(i,k,j)*B_v(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_advance_uv: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
                                              B_ft , &

                                              K_u,   &
                                              K_v,   &
                                              K_u_1, &
                                              K_v_1, &
                                              K_t_1, &
                                              K_ft

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

  REAL, DIMENSION( ims:ime , jms:jme )                   :: K_muu,  &
                                                            K_muv,  &
                                                            K_mut,  &
                                                            K_mu_tend
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
  REAL, DIMENSION( ims:ime , jms:jme ) :: S_muave, S_muts, S_mudf,K_muave, K_muts, K_mudf


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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

   enddo
   enddo
   enddo
   do i=ims,ime
   do j=jms,jme
      S_mu(i,j)=mu(i,j)
      P_mu(i,j)=mu(i,j)
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      muave(i,j)= 0.0
   enddo
   enddo

   do i=ims,ime
   do j=jms,jme
      S_muave(i,j)=muave(i,j)
      S_muts(i,j)=muts(i,j)
      S_mudf(i,j)=mudf(i,j)

      P_muave(i,j)=muave(i,j)
      P_muts(i,j)=muts(i,j)
      P_mudf(i,j)=mudf(i,j)
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

   do i=its,ite
   do j=jts,jte
      B_muave(i,j)=muave(i,j)
      B_muts(i,j)=muts(i,j)
      B_mudf(i,j)=mudf(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ww(i,k,j)=ww(i,k,j)
      B_ww_1(i,k,j)=ww_1(i,k,j)
      B_t(i,k,j)=t(i,k,j)
      B_t_ave(i,k,j)=t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      B_mu(i,j)=mu(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_u(i,k,j)=u(i,k,j)
      B_v(i,k,j)=v(i,k,j)
      B_u_1(i,k,j)=u_1(i,k,j)
      B_v_1(i,k,j)=v_1(i,k,j)
      B_t_1(i,k,j)=t_1(i,k,j)
      B_ft(i,k,j)=ft(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      B_mu_tend(i,j)=mu_tend(i,j)
      B_mut(i,j)=mut(i,j)
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
   enddo
   enddo

!  TCL

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

   do i=ims,ime
   do j=jms,jme
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)
      mudf(i,j)=S_mudf(i,j)
   enddo
   enddo


   CALL g_advance_mu_t( ww, P_ww, ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, mu, P_mu, mut, P_mut, muave, P_muave, &
&muts, P_muts, muu, P_muu, muv, P_muv, mudf, P_mudf, t, P_t, t_1, P_t_1, t_ave, P_t_ave, ft, P_ft, mu_tend, P_mu_tend, rdx, rdy, &
&dts, epssm, dnw, fnm, fnp, rdnw, msfu, msfv, msft, config_flags, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L + P_muave(i,j)*P_muave(i,j)       &
                    + P_muts(i,j)*P_muts(i,j)         &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo
          
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_ww(i,k,j)*P_ww(i,k,j)         &
                    + P_ww_1(i,k,j)*P_ww_1(i,k,j)     &
                    + P_t(i,k,j)*P_t(i,k,j)           &
                    + P_t_ave(i,k,j)*P_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L + P_mu(i,j)*P_mu(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +  P_u(i,k,j)*P_u(i,k,j)    &
                    +  P_v(i,k,j)*P_v(i,k,j)    &
                    +  P_u_1(i,k,j)*P_u_1(i,k,j)    &
                    +  P_v_1(i,k,j)*P_v_1(i,k,j)    &
                    +  P_t_1(i,k,j)*P_t_1(i,k,j)    &
                    +  P_ft(i,k,j)*P_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L +P_mu_tend(i,j)*P_mu_tend(i,j)    &
                    +P_mut(i,j)*P_mut(i,j)            &
                    +P_muu(i,j)*P_muu(i,j)            &
                    +P_muv(i,j)*P_muv(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
   do i=ims,ime
   do j=jms,jme
      P_muave(i,j)=FACTOR*S_muave(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
      P_mudf(i,j)=FACTOR*S_mudf(i,j)
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

   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N + (P_muave(i,j) -B_muave(i,j))*(P_muave(i,j) -B_muave(i,j))    &
                  + (P_muts(i,j) -B_muts(i,j))*(P_muts(i,j) -B_muts(i,j))        &
                  + (P_mudf(i,j) -B_mudf(i,j))*(P_mudf(i,j) -B_mudf(i,j))
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N + (P_ww(i,k,j) -B_ww(i,k,j))*(P_ww(i,k,j) -B_ww(i,k,j))            &
                  + (P_ww_1(i,k,j) -B_ww_1(i,k,j))*(P_ww_1(i,k,j) -B_ww_1(i,k,j))    &
                  + (P_t(i,k,j) -B_t(i,k,j))*(P_t(i,k,j) -B_t(i,k,j))                &
                  + (P_t_ave(i,k,j) -B_t_ave(i,k,j))*(P_t_ave(i,k,j) -B_t_ave(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N + (P_mu(i,j) -B_mu(i,j))*(P_mu(i,j) -B_mu(i,j))
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N   +  (P_u(i,k,j)-B_u(i,k,j))*(P_u(i,k,j)-B_u(i,k,j))    &
                    +  (P_v(i,k,j)-B_v(i,k,j))*(P_v(i,k,j)-B_v(i,k,j))    &
                    +  (P_u_1(i,k,j)-B_u_1(i,k,j))*(P_u_1(i,k,j)-B_u_1(i,k,j))    &
                    +  (P_v_1(i,k,j)-B_v_1(i,k,j))*(P_v_1(i,k,j)-B_v_1(i,k,j))    &
                    +  (P_t_1(i,k,j)-B_t_1(i,k,j))*(P_t_1(i,k,j)-B_t_1(i,k,j))    &
                    +  (P_ft(i,k,j)-B_ft(i,k,j))*(P_ft(i,k,j)-B_ft(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_N=VAL_N   +  (P_mu_tend(i,j)-B_mu_tend(i,j))*(P_mu_tend(i,j)-B_mu_tend(i,j))    &
                    +  (P_mut(i,j)-B_mut(i,j))*(P_mut(i,j)-B_mut(i,j))            &
                    +  (P_muu(i,j)-B_muu(i,j))*(P_muu(i,j)-B_muu(i,j))            &
                    +  (P_muv(i,j)-B_muv(i,j))*(P_muv(i,j)-B_muv(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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

   do i=ims,ime
   do j=jms,jme
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)
      mudf(i,j)=S_mudf(i,j)

      P_muave(i,j)=FACTOR*S_muave(i,j)
      P_muts(i,j)=FACTOR*S_muts(i,j)
      P_mudf(i,j)=FACTOR*S_mudf(i,j)

      B_muave(i,j)=P_muave(i,j)
      B_muts(i,j)=P_muts(i,j)
      B_mudf(i,j)=P_mudf(i,j)
   enddo
   enddo

!  TGL

   CALL g_advance_mu_t( ww, P_ww, ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, mu, P_mu, mut, P_mut, muave, P_muave, &
&muts, P_muts, muu, P_muu, muv, P_muv, mudf, P_mudf, t, P_t, t_1, P_t_1, t_ave, P_t_ave, ft, P_ft, mu_tend, P_mu_tend, rdx, rdy, &
&dts, epssm, dnw, fnm, fnp, rdnw, msfu, msfv, msft, config_flags, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte, kts, kte )

   VAL_L=0.
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L +  P_muave(i,j)*P_muave(i,j)       &
                    + P_muts(i,j)*P_muts(i,j)         &
                    + P_mudf(i,j)*P_mudf(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_ww(i,k,j)*P_ww(i,k,j)         &
                    + P_ww_1(i,k,j)*P_ww_1(i,k,j)     &
                    + P_t(i,k,j)*P_t(i,k,j)           &
                    + P_t_ave(i,k,j)*P_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L + P_mu(i,j)*P_mu(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L   +  P_u(i,k,j)*P_u(i,k,j)    &
                    +   P_v(i,k,j)*P_v(i,k,j)    &
                    +   P_u_1(i,k,j)*P_u_1(i,k,j)    &
                    +   P_v_1(i,k,j)*P_v_1(i,k,j)    &
                    +   P_t_1(i,k,j)*P_t_1(i,k,j)    &
                    +   P_ft(i,k,j)*P_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
       VAL_L=VAL_L  +P_mu_tend(i,j)*P_mu_tend(i,j)    &
                    + P_mut(i,j)*P_mut(i,j)            &
                    + P_muu(i,j)*P_muu(i,j)            &
                    + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

!  ADJ
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

   do i=ims,ime
   do j=jms,jme
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)
      mudf(i,j)=S_mudf(i,j)
   enddo
   enddo


   CALL a_advance_mu_t( ww, P_ww, ww_1, P_ww_1, u, P_u, u_1, P_u_1, v, P_v, v_1, P_v_1, P_mu, P_mut, P_muave, P_muts, muu, &
&P_muu, muv, P_muv, P_mudf, P_t, t_1, P_t_1, P_t_ave, P_ft, mu_tend, P_mu_tend, rdx, rdy, dts, epssm, dnw, fnm, fnp, rdnw, msfu, &
&msfv, msft, config_flags, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_u(i,k,j)*B_u(i,k,j)            &
                  + P_v(i,k,j)*B_v(i,k,j)            &
                  + P_u_1(i,k,j)*B_u_1(i,k,j)        &
                  + P_v_1(i,k,j)*B_v_1(i,k,j)        &
                  + P_t_1(i,k,j)*B_t_1(i,k,j)        &
                  + P_ft(i,k,j)*B_ft(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j)  &
                  + P_mut(i,j)*B_mut(i,j)          &
                  + P_muu(i,j)*B_muu(i,j)          &
                  + P_muv(i,j)*B_muv(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_ww(i,k,j)*B_ww(i,k,j)        &
                  + P_ww_1(i,k,j)*B_ww_1(i,k,j)    &
                  + P_t(i,k,j)*B_t(i,k,j)          &
                  + P_t_ave(i,k,j)*B_t_ave(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A   + P_muave(i,j)*B_muave(i,j)       &
                    + P_muts(i,j)*B_muts(i,j)         &
                    + P_mudf(i,j)*B_mudf(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_advance_mu_t: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

   do i=ims,ime
   do j=jms,jme
      muave(i,j)=S_muave(i,j)
      muts(i,j)=S_muts(i,j)
      mudf(i,j)=S_mudf(i,j)
   enddo
   enddo

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
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_ru_m(i,k,j)*P_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*P_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*P_ww_m(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N + (P_ru_m(i,k,j)-B_ru_m(i,k,j))*(P_ru_m(i,k,j)-B_ru_m(i,k,j))     &
                     + (P_rv_m(i,k,j)-B_rv_m(i,k,j))*(P_rv_m(i,k,j)-B_rv_m(i,k,j))     &
                     + (P_ww_m(i,k,j)-B_ww_m(i,k,j))*(P_ww_m(i,k,j)-B_ww_m(i,k,j))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L   +P_ru_m(i,k,j)*P_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*P_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*P_ww_m(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A+P_ru(i,k,j)*B_ru(i,k,j)         &
               +P_rv(i,k,j)*B_rv(i,k,j)            &
               +P_ww(i,k,j)*B_ww(i,k,j)            &
               +P_u_lin(i,k,j)*B_u_lin(i,k,j)              &
               +P_v_lin(i,k,j)*B_v_lin(i,k,j)              &
               +P_ww_lin(i,k,j)*B_ww_lin(i,k,j)  
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A +P_muu(i,j)*B_muu(i,j)    &
               +P_muv(i,j)*B_muv(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_ru_m(i,k,j)*B_ru_m(i,k,j)      &
                    +P_rv_m(i,k,j)*B_rv_m(i,k,j)      &
                    +P_ww_m(i,k,j)*B_ww_m(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_sumflux: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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



   REAL :: SAVE_L, COEF, ALPHA_P, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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


   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_t_2ave(i,k,j)*P_t_2ave(i,k,j)   &
                    +P_w(i,k,j)*P_w(i,k,j)             &
                    +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA_P= alphaInit + PERTURBATION
   DO NT=1, Ndx
      ALPHA_P=0.1*ALPHA_P
      FACTOR=1.+ALPHA_P
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N+(P_t_2ave(i,k,j)-B_t_2ave(i,k,j))*(P_t_2ave(i,k,j)-B_t_2ave(i,k,j))   &
                 +(P_w(i,k,j)-B_w(i,k,j))*(P_w(i,k,j)-B_w(i,k,j))             &
                 +(P_ph(i,k,j)-B_ph(i,k,j))*(P_ph(i,k,j)-B_ph(i,k,j))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

      VAL_L=SAVE_L*ALPHA_P**2
      COEF=VAL_N/VAL_L
      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_advance_w: ALPHA_P=',ALPHA_P,'  COEF=',COEF, &
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_t_2ave(i,k,j)*P_t_2ave(i,k,j)   &
                  + P_w(i,k,j)*P_w(i,k,j)             &
                  + P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu1(i,j)*B_mu1(i,j)                  &
             + P_mut(i,j)*B_mut(i,j)                       &
             + P_muave(i,j)*B_muave(i,j)                   &
             + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_t_2ave(i,k,j)*B_t_2ave(i,k,j)        &
             + P_w(i,k,j)*B_w(i,k,j)                       &
             + P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_advance_w: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

!  IN variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field_tend, S_ph_save,P_field_tend, P_ph_save,B_field_tend, B_ph_save
      REAL,  DIMENSION( ims:ime , jms:jme )             :: S_mu_tend, S_muts,P_mu_tend, P_muts,B_mu_tend, B_muts

!  INOUT variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: S_field,P_field,B_field,K_field

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_spec_bdyupdate_ph( ph_save, P_ph_save, K_field, P_field, field_tend, P_field_tend, mu_tend, P_mu_tend, muts, P_muts, dt,&
& variable_in, spec_zone, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N+(P_field(i,k,j) -B_field(i,k,j))*(P_field(i,k,j) -B_field(i,k,j))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_field_tend(i,k,j)*B_field_tend(i,k,j) &
                    + P_ph_save(i,k,j)*B_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A + P_mu_tend(i,j)*B_mu_tend(i,j) &
                    + P_muts(i,j)*B_muts(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_field(i,k,j)*B_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_spec_bdyupdate_ph: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_spec_bdyupdate_ph
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_calc_mu_uv_1 ( config_flags,                 &
                          mu, muu, muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

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

!  IN variables

   REAL, DIMENSION( ims:ime , jms:jme )  :: S_mu,P_mu,B_mu

!  OUT variables

   REAL, DIMENSION( ims:ime , jms:jme )  :: P_muu, P_muv,B_muu, B_muv

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

!TGL test

   P_muu = 0.0
   P_muv = 0.0

   do j=jms,jme
   do i=ims,ime
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

   do j=jms,jme
   do i=ims,ime
      B_muu(i,j)=muu(i,j)
      B_muv(i,j)=muv(i,j)
   enddo
   enddo

!  TCL

   CALL g_calc_mu_uv_1( config_flags, mu, P_mu, muu, P_muu, muv, P_muv, ids, ide, jds, jde, ims, ime, jms, jme, its, ite, jts, &
&jte )

   SAVE_L=0.
   do j=jts,jte
   do i=its,ite
      SAVE_L=SAVE_L + P_muu(i,j)*P_muu(i,j) + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
   do j=jms,jme
   do i=ims,ime
      P_mu(i,j)=FACTOR*S_mu(i,j)
   enddo
   enddo

   CALL calc_mu_uv_1 ( config_flags,                 &
                          P_mu, P_muu, P_muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   VAL_N=0.
   do j=jts,jte
   do i=its,ite
         VAL_N=VAL_N+ (P_muu(i,j)-B_muu(i,j))*(P_muu(i,j)-B_muu(i,j))    &
                    + (P_muv(i,j)-B_muv(i,j))*(P_muv(i,j)-B_muv(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L +P_muu(i,j)*P_muu(i,j) + P_muv(i,j)*P_muv(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

   do i=ims,ime
   do j=jms,jme
      P_mu(i,j)=0.0
   enddo
   enddo

!  ADJ

   CALL a_calc_mu_uv_1( config_flags, P_mu, P_muu, P_muv, ids, ide, jds, jde, ims, ime, jms, jme, its, ite, jts, jte )

   VAL_A=0.
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_calc_mu_uv_1: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

!  RECOVER

   do i=ims,ime
   do j=jms,jme
      mu(i,j)=S_mu(i,j)
   enddo
   enddo

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
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

  IMPLICIT NONE  ! religion first

!  stuff passed in




  INTEGER,                  INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,                  INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,                  INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  INTEGER,                  INTENT(IN   ) :: number_of_small_timesteps
  REAL,                     INTENT(IN   ) :: dts

!  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme)  ::P_ww1,P_ww
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
   REAL :: PERTURBATION
   INTEGER :: NT

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
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_u_2(i,k,j)=u_2(i,k,j)
      B_v_2(i,k,j)=v_2(i,k,j)
      B_w_2(i,k,j)=w_2(i,k,j)
      B_t_2(i,k,j)=t_2(i,k,j)
      B_ph_2(i,k,j)=ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
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
&P_t_save, ph_save, P_ph_save, K_mu_save, P_mu_save, msfu, msfv, msft, number_of_small_timesteps, dts, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte )


   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )

   VAL_N=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N + (P_u_2(i,k,j)- B_u_2(i,k,j))*(P_u_2(i,k,j)- B_u_2(i,k,j))         &
                  + (P_v_2(i,k,j)- B_v_2(i,k,j))*(P_v_2(i,k,j)- B_v_2(i,k,j))         &
                  + (P_w_2(i,k,j)- B_w_2(i,k,j))*(P_w_2(i,k,j)- B_w_2(i,k,j))         &
                  + (P_t_2(i,k,j)- B_t_2(i,k,j))*(P_t_2(i,k,j)- B_t_2(i,k,j))         &
                  + (P_ph_2(i,k,j)- B_ph_2(i,k,j))*(P_ph_2(i,k,j)- B_ph_2(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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

   CALL g_small_step_finish( u_2, P_u_2, v_2, P_v_2, w_2, P_w_2, t_2, P_t_2, ph_2, P_ph_2, ww, mu_2, P_mu_2,mut, P_mut, muts, &
&P_muts, muu, P_muu, muus, P_muus, muv, P_muv, muvs, P_muvs, u_save, P_u_save, v_save, P_v_save, w_save, P_w_save, t_save, &
&P_t_save, ph_save, P_ph_save, mu_save, P_mu_save, msfu, msfv, msft, number_of_small_timesteps, dts, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, &
&jts, jte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L   + P_u_2(i,k,j)*P_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*P_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*P_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*P_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*P_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
&P_mu_save, msfu, msfv, msft, number_of_small_timesteps, dts, ide, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
     VAL_A=VAL_A +B_u_save(i,k,j)*P_u_save(i,k,j)       &
                 +B_v_save(i,k,j)*P_v_save(i,k,j)       &
                 +B_w_save(i,k,j)*P_w_save(i,k,j)       &
                 +B_t_save(i,k,j)*P_t_save(i,k,j)       &
                 +B_ph_save(i,k,j)*P_ph_save(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
     VAL_A=VAL_A    + P_u_2(i,k,j)*B_u_2(i,k,j)         &
                    + P_v_2(i,k,j)*B_v_2(i,k,j)         &
                    + P_w_2(i,k,j)*B_w_2(i,k,j)         &
                    + P_t_2(i,k,j)*B_t_2(i,k,j)         &
                    + P_ph_2(i,k,j)*B_ph_2(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
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

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_small_step_finish: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_small_step_finish
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            ru, rv, ww, mut, alt,         &
                            scalar_old, scalar,           &
                            scalar_tends, advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, xkmhd,          &
                            leapfrog,                     &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: rk_step, scs, sce
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   LOGICAL , INTENT(IN   ) :: moist_step

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce ),                &
                                         INTENT(INOUT)  :: scalar,     &
                                                           scalar_old

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

   LOGICAL, INTENT(IN   ) :: leapfrog


   ! Local data

   INTEGER :: im, i,j,k

   REAL    :: khdq, kvdq, tendency

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

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: S_scalar, S_scalar_old,P_scalar, P_scalar_old
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: K_scalar, K_scalar_old,B_scalar, B_scalar_old

!  OUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce )  :: P_scalar_tends,B_scalar_tends,S_scalar_tends

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

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

   do j=jms,jme
   do i=ims,ime
      S_mut(i,j)=mut(i,j)

      P_mut(i,j)=mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      S_scalar(i,k,j,h)=scalar(i,k,j,h)
      S_scalar_old(i,k,j,h)=scalar_old(i,k,j,h)

      P_scalar(i,k,j,h)=scalar(i,k,j,h)
      P_scalar_old(i,k,j,h)=scalar_old(i,k,j,h)

      K_scalar(i,k,j,h)=scalar(i,k,j,h)
      K_scalar_old(i,k,j,h)=scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   scalar_tends = 0.0

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      S_scalar_tends(i,k,j,h)=scalar_tends(i,k,j,h)

      P_scalar_tends(i,k,j,h)=scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo


!NLM

   CALL rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            ru, rv, ww, mut, alt,         &
                            scalar_old, scalar,           &
                            scalar_tends, advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, xkmhd,          &
                            leapfrog,                     &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      B_scalar_tends(i,k,j,h)=scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do j=jms,jme
   do i=ims,ime
      B_mut(i,j)=mut(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      B_scalar(i,k,j,h)=scalar(i,k,j,h)
      B_scalar_old(i,k,j,h)=scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ru(i,k,j)=ru(i,k,j)
      B_rv(i,k,j)=rv(i,k,j)
      B_ww(i,k,j)=ww(i,k,j)
      B_advect_tend(i,k,j)=advect_tend(i,k,j)
      B_xkmhd(i,k,j)=xkmhd(i,k,j)
      B_alt(i,k,j)=alt(i,k,j)
   enddo
   enddo
   enddo

!  TCL

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

   do j=jms,jme
   do i=ims,ime
      mut(i,j)=S_mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar(i,k,j,h)=S_scalar(i,k,j,h)
      scalar_old(i,k,j,h)=S_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_tends(i,k,j,h)=S_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   CALL g_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, scalar_old, &
&P_scalar_old, scalar, P_scalar, scalar_tends, P_scalar_tends, advect_tend, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, &
&msft, rdx, rdy, rdn, rdnw, kvdif, xkmhd, P_xkmhd, leapfrog, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, &
&jte, kts, kte )

   SAVE_L=0.
   do h=scs,sce
   do j=jts,jte
   do k=kts,kte
   do i=its,ite
      SAVE_L=SAVE_L + P_scalar_tends(i,k,j,h)*P_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do h=scs,sce
   do j=jts,jte
   do k=kts,kte
   do i=its,ite
      SAVE_L=SAVE_L + P_scalar(i,k,j,h)*P_scalar(i,k,j,h)            &
                    + P_scalar_old(i,k,j,h)*P_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo


   do j=jts,jte
   do k=kts,kte
   do i=its,ite
      SAVE_L=SAVE_L+P_ru(i,k,j)*P_ru(i,k,j)         &
               +P_rv(i,k,j)*P_rv(i,k,j)            &
               +P_ww(i,k,j)*P_ww(i,k,j)            &
               +P_advect_tend(i,k,j)*P_advect_tend(i,k,j)            &
               +P_alt(i,k,j)*P_alt(i,k,j)          &
               +P_xkmhd(i,k,j)*P_xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do j=jts,jte
   do i=its,ite
      SAVE_L=SAVE_L+P_mut(i,j)*P_mut(i,j)
   enddo
   enddo


#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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

   do j=jms,jme
   do i=ims,ime
      P_mut(i,j)=FACTOR*S_mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      P_scalar(i,k,j,h)=FACTOR*S_scalar(i,k,j,h)
      P_scalar_old(i,k,j,h)=FACTOR*S_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      P_scalar_tends(i,k,j,h)=FACTOR*S_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   CALL rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            P_ru, P_rv, P_ww, P_mut, P_alt,         &
                            P_scalar_old, P_scalar,           &
                            P_scalar_tends, P_advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, P_xkmhd,          &
                            leapfrog,                     &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   VAL_N=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_N=VAL_N+(P_scalar_tends(i,k,j,h) -B_scalar_tends(i,k,j,h))*(P_scalar_tends(i,k,j,h) -B_scalar_tends(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_N=VAL_N+(P_scalar(i,k,j,h) -B_scalar(i,k,j,h))*(P_scalar(i,k,j,h) -B_scalar(i,k,j,h))               &
                 +(P_scalar_old(i,k,j,h) -B_scalar_old(i,k,j,h))*(P_scalar_old(i,k,j,h) -B_scalar_old(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N +(P_ru(i,k,j)-B_ru(i,k,j))*(P_ru(i,k,j)-B_ru(i,k,j))         &
                  +(P_rv(i,k,j)-B_rv(i,k,j))*(P_rv(i,k,j)-B_rv(i,k,j))            &
                  +(P_ww(i,k,j)-B_ww(i,k,j))*(P_ww(i,k,j)-B_ww(i,k,j))            &
                  +(P_advect_tend(i,k,j)-B_advect_tend(i,k,j))*(P_advect_tend(i,k,j)-B_advect_tend(i,k,j))            &
                  +(P_alt(i,k,j)-B_alt(i,k,j))*(P_alt(i,k,j)-B_alt(i,k,j))          &
                  +(P_xkmhd(i,k,j)-B_xkmhd(i,k,j))*(P_xkmhd(i,k,j)-B_xkmhd(i,k,j))
   enddo
   enddo
   enddo

   do j=jts,jte
   do i=its,ite
      VAL_N=VAL_N+(P_mut(i,j)-B_mut(i,j))*(P_mut(i,j)-B_mut(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

      VAL_L=SAVE_L*ALPHA**2

      if(VAL_L == 0.) then
         COEF = 1.
      else
         COEF=VAL_N/VAL_L
      endif

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

   do j=jms,jme
   do i=ims,ime
      mut(i,j)=S_mut(i,j)

      P_mut(i,j)=FACTOR*S_mut(i,j)

      B_mut(i,j)=P_mut(i,j)
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar(i,k,j,h)=S_scalar(i,k,j,h)
      scalar_old(i,k,j,h)=S_scalar_old(i,k,j,h)

      P_scalar(i,k,j,h)=FACTOR*S_scalar(i,k,j,h)
      P_scalar_old(i,k,j,h)=FACTOR*S_scalar_old(i,k,j,h)

      B_scalar(i,k,j,h)=P_scalar(i,k,j,h)
      B_scalar_old(i,k,j,h)=P_scalar_old(i,k,j,h)

      K_scalar(i,k,j,h)=scalar(i,k,j,h)
      K_scalar_old(i,k,j,h)=scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_tends(i,k,j,h)=S_scalar_tends(i,k,j,h)

      P_scalar_tends(i,k,j,h)=FACTOR*S_scalar_tends(i,k,j,h)

      B_scalar_tends(i,k,j,h)=P_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

!  TGL

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
   do j=jms,jme
   do i=ims,ime
      mut(i,j)=S_mut(i,j)
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar(i,k,j,h)=S_scalar(i,k,j,h)
      scalar_old(i,k,j,h)=S_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_tends(i,k,j,h)=S_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo


   CALL g_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, scalar_old, &
&P_scalar_old, scalar, P_scalar, scalar_tends, P_scalar_tends, advect_tend, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, &
&msft, rdx, rdy, rdn, rdnw, kvdif, xkmhd, P_xkmhd, leapfrog, ids, ide, jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, &
&jte, kts, kte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_L=VAL_L + P_scalar_tends(i,k,j,h)*P_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_L=VAL_L + P_scalar(i,k,j,h)*P_scalar(i,k,j,h)            &
                    + P_scalar_old(i,k,j,h)*P_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L+P_ru(i,k,j)*P_ru(i,k,j)         &
               +P_rv(i,k,j)*P_rv(i,k,j)            &
               +P_ww(i,k,j)*P_ww(i,k,j)            &
               +P_advect_tend(i,k,j)*P_advect_tend(i,k,j)            &
               +P_alt(i,k,j)*P_alt(i,k,j)          &
               +P_xkmhd(i,k,j)*P_xkmhd(i,k,j)
   enddo
   enddo
   enddo

   do j=jts,jte
   do i=its,ite
      VAL_L=VAL_L+P_mut(i,j)*P_mut(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

!  ADJ

   CALL a_rk_scalar_tend( scs, sce, config_flags, rk_step, ru, P_ru, rv, P_rv, ww, P_ww, mut, P_mut, alt, P_alt, scalar_old, &
&P_scalar_old, scalar, P_scalar, P_scalar_tends, P_advect_tend, base, moist_step, fnm, fnp, msfu, msfv, msft, rdx, rdy, rdn, rdnw, &
&kvdif, xkmhd, P_xkmhd, leapfrog, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )


   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_ru(i,k,j)*B_ru(i,k,j)         &
               +P_rv(i,k,j)*B_rv(i,k,j)            &
               +P_ww(i,k,j)*B_ww(i,k,j)            &
               +P_advect_tend(i,k,j)*B_advect_tend(i,k,j)            &
               +P_alt(i,k,j)*B_alt(i,k,j)          &
               +P_xkmhd(i,k,j)*B_xkmhd(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_A=VAL_A +P_scalar(i,k,j,h)*B_scalar(i,k,j,h)   &
                  +P_scalar_old(i,k,j,h)*B_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_A=VAL_A +P_scalar_tends(i,k,j,h)*B_scalar_tends(i,k,j,h)   
   enddo
   enddo
   enddo
   enddo
   do j=jts,jte
   do i=its,ite
      VAL_A=VAL_A+P_mut(i,j)*B_mut(i,j)
   enddo
   enddo


#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_rk_scalar_tend: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
      scalar_old(i,k,j,h)=S_scalar_old(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
   do h=scs,sce
      scalar_tends(i,k,j,h)=S_scalar_tends(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

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

!  IN variables

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 )   :: S_scalar_bt,P_scalar_bt,B_scalar_bt

!  OUT variables

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )   :: S_scalar_tend,P_scalar_tend,B_scalar_tend

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

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

   SAVE_L=sum(P_scalar_tend(its:ite,kts:kte,jts:jte)*P_scalar_tend(its:ite,kts:kte,jts:jte)) + &
          sum(P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)*P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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


      VAL_N= sum((P_scalar_tend(its:ite,kts:kte,jts:jte)-B_scalar_tend(its:ite,kts:kte,jts:jte))*(P_scalar_tend(its:ite,kts:kte,jts:jte)-B_scalar_tend(its:ite,kts:kte,jts:jte))) + &
             sum((P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)-B_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4))*(P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)-B_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)))

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                          comm, IERROR )
      VAL_N = nsum 
#endif

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

   VAL_L=sum(P_scalar_tend(its:ite,kts:kte,jts:jte)*P_scalar_tend(its:ite,kts:kte,jts:jte))+ &
         sum(P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)*P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

!  ADJ

   CALL a_spec_bdy_scalar( P_scalar_tend, P_scalar_bt, spec_bdy_width, spec_zone, ijds, ijde, ids, ide, jds, jde, kds, kde, ims,&
& ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=sum(P_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)*B_scalar_bt(ijds:ijde,kts:kte,1:spec_bdy_width,1:4)) + &
         sum(P_scalar_tend(its:ite,kts:kte,jts:jte)*B_scalar_tend(its:ite,kts:kte,jts:jte))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_spec_bdy_scalar: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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
                             epsts, leapfrog, config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: scs, sce, rk_step, spec_zone
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   REAL,                    INTENT(IN   ) :: dt, epsts

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: scalar_1,  &
                                                           scalar_2,  &
                                                           sc_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme )        :: advect_tend
   REAL, DIMENSION(ims:ime, jms:jme  )                :: mu_old, mu_new

   REAL, DIMENSION(ims:ime, jms:jme  ), INTENT(IN   ) :: mu_base, &
                                                          msft

   LOGICAL, INTENT(IN   ) :: leapfrog

   INTEGER :: i,j,k,im
   REAL    :: sc_middle, msfsq
   REAL, DIMENSION(its:ite) :: muold, r_munew

   REAL, DIMENSION(its:ite, kts:kte, jts:jte  ) :: tendency

   INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

!  IN variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme )    :: S_advect_tend,P_advect_tend,B_advect_tend
   REAL, DIMENSION(ims:ime, jms:jme  )            :: S_mu_old, S_mu_new,P_mu_old, P_mu_new,B_mu_old, B_mu_new

!  INOUT variables

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce)  :: S_scalar_1, S_scalar_2, S_sc_tend,P_scalar_1, P_scalar_2, P_sc_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce)  :: K_scalar_1, K_scalar_2, K_sc_tend,B_scalar_1, B_scalar_2, B_sc_tend


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

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
                             epsts, leapfrog, config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
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
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, epsts, leapfrog, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      SAVE_L=SAVE_L + P_scalar_1(i,k,j,h)*P_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*P_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*P_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
                             epsts, leapfrog, config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

      VAL_N=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
         VAL_N=VAL_N+(P_scalar_1(i,k,j,h)-B_scalar_1(i,k,j,h))*(P_scalar_1(i,k,j,h)-B_scalar_1(i,k,j,h))    &
                    +(P_scalar_2(i,k,j,h)-B_scalar_2(i,k,j,h))*(P_scalar_2(i,k,j,h)-B_scalar_2(i,k,j,h))    &
                    +(P_sc_tend(i,k,j,h) -B_sc_tend(i,k,j,h))*(P_sc_tend(i,k,j,h) -B_sc_tend(i,k,j,h))
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

      VAL_L=SAVE_L*ALPHA**2

      if(VAL_L == 0.) then
         COEF = 1.
      else
         COEF=VAL_N/VAL_L
      endif

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
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, epsts, leapfrog, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_L=VAL_L + P_scalar_1(i,k,j,h)*P_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*P_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*P_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

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
&P_advect_tend, msft, mu_old, P_mu_old, mu_new, P_mu_new, mu_base, rk_step, dt, spec_zone, epsts, leapfrog, config_flags, ids, ide,&
& jds, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_advect_tend(i,k,j)*B_advect_tend(i,k,j)     
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A +P_mu_old(i,j)*B_mu_old(i,j)                   &
                  +P_mu_new(i,j)*B_mu_new(i,j)
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=scs,sce
      VAL_A=VAL_A + P_scalar_1(i,k,j,h)*B_scalar_1(i,k,j,h)   &
                    + P_scalar_2(i,k,j,h)*B_scalar_2(i,k,j,h)   &
                    + P_sc_tend(i,k,j,h)*B_sc_tend(i,k,j,h)
   enddo
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_rk_update_scalar: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_rk_update_scalar
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_calc_p_rho_phi ( moist, n_moist,                &
                            al, alb, mu, muts, ph, p, pb,  &
                            t, p0, t0, znu, dnw, rdnw,     &
                            rdn, non_hydrostatic,          &
                            ids, ide, jds, jde, kds, kde,  &
                            ims, ime, jms, jme, kms, kme,  &
                            its, ite, jts, jte, kts, kte  )


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

!  IN variables

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_t,P_t,B_t

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ) :: S_moist,P_moist,B_moist
  REAL, DIMENSION( ims:ime , jms:jme )   :: S_mu, S_muts,P_mu, P_muts,B_mu, B_muts
!  INOUT

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme )  :: S_ph,P_ph,K_ph,B_ph

!  OUT variables

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) :: P_al, P_p,B_al, B_p,S_al,S_p

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      S_al(i,k,j)=al(i,k,j)
      S_p(i,k,j)=p(i,k,j)

      P_al(i,k,j)=al(i,k,j)
      P_p(i,k,j)=p(i,k,j)
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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_al(i,k,j)=al(i,k,j)
      B_p(i,k,j)=p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_ph(i,k,j)=ph(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_t(i,k,j)=t(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,n_moist
      B_moist(i,k,j,h)=moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      B_mu(i,j)=mu(i,j)
      B_muts(i,j)=muts(i,j)
   enddo
   enddo

!  TCL

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

   CALL g_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_al(i,k,j)*P_al(i,k,j)  +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_t(i,k,j) *P_t(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,n_moist
      SAVE_L=SAVE_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      SAVE_L=SAVE_L+P_mu(i,j)*P_mu(i,j)  +P_muts(i,j)*P_muts(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum 
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
         VAL_N=VAL_N+(P_ph(i,k,j)-B_ph(i,k,j))*(P_ph(i,k,j)-B_ph(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
         VAL_N=VAL_N+(P_al(i,k,j)-B_al(i,k,j))*(P_al(i,k,j)-B_al(i,k,j))   &
                    +(P_p(i,k,j) -B_p(i,k,j))*(P_p(i,k,j) -B_p(i,k,j))
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N+(P_t(i,k,j) -B_t(i,k,j))*(P_t(i,k,j) -B_t(i,k,j))
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,n_moist
     VAL_N=VAL_N  +(P_moist(i,k,j,h)-B_moist(i,k,j,h))*(P_moist(i,k,j,h)-B_moist(i,k,j,h))
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
     VAL_N=VAL_N  +(P_mu(i,j)-B_mu(i,j))*(P_mu(i,j)-B_mu(i,j))           &
                  +(P_muts(i,j)-B_muts(i,j))*(P_muts(i,j)-B_muts(i,j))
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum 
#endif

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
   enddo
   enddo
   enddo

   do i=ims,ime
   do k=kms,kme
   do j=jms,jme
      al(i,k,j)=S_al(i,k,j)
      p(i,k,j)=S_p(i,k,j)

      P_al(i,k,j)=FACTOR*S_al(i,k,j)
      P_p(i,k,j)=FACTOR*S_p(i,k,j)

      B_al(i,k,j)=P_al(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
   enddo
   enddo
   enddo

!  TGL

   CALL g_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_al(i,k,j)*P_al(i,k,j)  +P_p(i,k,j)*P_p(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_ph(i,k,j)*P_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L +P_t(i,k,j) *P_t(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,n_moist
      VAL_L=VAL_L+P_moist(i,k,j,h)*P_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_L=VAL_L+P_mu(i,j)*P_mu(i,j)  +P_muts(i,j)*P_muts(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum 
#endif

!  ADJ

   CALL a_calc_p_rho_phi( moist, P_moist, n_moist, al, P_al, alb, mu, P_mu, muts, P_muts, ph, P_ph, p, P_p, pb, t, P_t, p0, t0, &
&dnw, rdnw, rdn, non_hydrostatic, ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   VAL_A=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_t(i,k,j) *B_t(i,k,j)
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
   do h=1,n_moist
      VAL_A=VAL_A +P_moist(i,k,j,h)*B_moist(i,k,j,h)
   enddo
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)  +P_muts(i,j)*B_muts(i,j)
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_ph(i,k,j)*B_ph(i,k,j)
   enddo
   enddo
   enddo

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_al(i,k,j)*B_al(i,k,j)  +P_p(i,k,j)*B_p(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum 
#endif

   print*, '                '
   write(6,*) 'a_calc_p_rho_phi: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_calc_p_rho_phi
!-----------------------------------------------------------------------------------------------
SUBROUTINE t_diagnose_w( ph_tend, ph_new, ph_old, w, mu, dt,  &
                       u, v, ht,                            &
                       cf1, cf2, cf3, rdx, rdy, msft,       &
                       ids, ide, jds, jde, kds, kde,        &
                       ims, ime, jms, jme, kms, kme,        &
                       its, ite, jts, jte, kts, kte        )


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
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_w(i,k,j)=w(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_diagnose_w( ph_tend, P_ph_tend, ph_new, P_ph_new, ph_old, P_ph_old, w, P_w, mu, P_mu, dt, u, P_u, v, P_v, ht, cf1, &
&cf2, cf3, rdx, rdy, msft, ide, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L +P_w(i,k,j)*P_w(i,k,j)
   enddo
   enddo
   enddo

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
      do i=its,ite
      do k=kts,kte
      do j=jts,jte
         VAL_N=VAL_N+(P_w(i,k,j)- B_w(i,k,j))*(P_w(i,k,j)- B_w(i,k,j)) 
      enddo
      enddo
      enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_w(i,k,j)*P_w(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A +P_u(i,k,j)*B_u(i,k,j)              &
               +P_v(i,k,j)*B_v(i,k,j)              &
               +P_ph_new(i,k,j)*B_ph_new(i,k,j)    &
               +P_ph_old(i,k,j)*B_ph_old(i,k,j)    &
               +P_ph_tend(i,k,j)*B_ph_tend(i,k,j) 
   enddo
   enddo
   enddo
   do i=its,ite
   do j=jts,jte
      VAL_A=VAL_A +P_mu(i,j)*B_mu(i,j)
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_diagnose_w: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

!IN variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field_tend,P_field_tend,B_field_tend

!INOUT variables

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )   :: S_field,P_field,B_field,K_field

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT

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

   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      B_field(i,k,j)=field(i,k,j)
   enddo
   enddo
   enddo

!  TCL

   CALL g_spec_bdyupdate( K_field, P_field, field_tend, P_field_tend, dt, variable_in, spec_zone, ids, ide, jds, jde, kde, ims, &
&ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

   SAVE_L=0.
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      SAVE_L=SAVE_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_N=VAL_N+(P_field(i,k,j) -B_field(i,k,j))*(P_field(i,k,j) -B_field(i,k,j))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_L=VAL_L + P_field(i,k,j)*P_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

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
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_field_tend(i,k,j)*B_field_tend(i,k,j) 
   enddo
   enddo
   enddo
   do i=its,ite
   do k=kts,kte
   do j=jts,jte
      VAL_A=VAL_A + P_field(i,k,j)*B_field(i,k,j)
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_spec_bdyupdate: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

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

END SUBROUTINE t_spec_bdyupdate
!-----------------------------------------------------------------------------------------------

   SUBROUTINE t_surface_drag ( ru_tendf, rv_tendf, u, v, xland,             &
                         muu, muv, z, z_at_w,                   &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte   )
   
   USE module_big_step_utilities_em
   implicit none

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )  :: ru_tendf, rv_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  )  :: u, v, z,  z_at_w
   REAL , DIMENSION( ims:ime , jms:jme  ) :: muu,muv,xland
! Local
   REAL :: V0_u, V0_v, tao_xz, tao_yz, cd, zu, zv, zwt
   INTEGER :: i, j, i_start, i_end, i_endu, j_start, j_end, j_endv, k

   REAL :: SAVE_L, ALPHA, FACTOR, VAL_N, VAL_L, COEF,VAL_A
   REAL :: PERTURBATION

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: K_ru_tendf,K_rv_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: g_ru_tendf,g_rv_tendf, g_u,g_v, g_z,g_z_at_w
   REAL , DIMENSION( ims:ime , jms:jme  )          :: g_muu, g_muv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: P_ru_tendf,P_rv_tendf, P_u,P_v, P_z,P_z_at_w
   REAL , DIMENSION( ims:ime , jms:jme  )          :: P_muu, P_muv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: S_ru_tendf,S_rv_tendf, S_u,S_v, S_z,S_z_at_w
   REAL , DIMENSION( ims:ime , jms:jme  )          :: S_muu, S_muv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: B_ru_tendf,B_rv_tendf, B_u,B_v, B_z,B_z_at_w
   REAL , DIMENSION( ims:ime , jms:jme  )          :: B_muu, B_muv
    INTEGER:: NT

! End declarations.
!-----------------------------------------------------------------------

   K_ru_tendf=ru_tendf
   K_rv_tendf=rv_tendf

   g_ru_tendf=ru_tendf
   g_rv_tendf=rv_tendf
   g_u=u
   g_v=v
   g_muu=muu
   g_muv=muv
   g_z=z
   g_z_at_w=z_at_w

   S_ru_tendf=ru_tendf
   S_rv_tendf=rv_tendf
   S_u=u
   S_v=v
   S_muu=muu
   S_muv=muv
   S_z=z
   S_z_at_w=z_at_w

!call FWD
  call   surface_drag( ru_tendf, rv_tendf, u, v, xland,             &
                         muu, muv, z, z_at_w,                   &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte)

        B_ru_tendf=ru_tendf
        B_rv_tendf=rv_tendf

!call TL
  call   g_surface_drag( K_ru_tendf, g_ru_tendf, K_rv_tendf, g_rv_tendf, u, g_u, v, g_v, xland, muu, g_muu, muv, &
  &g_muv, z, g_z, z_at_w, g_z_at_w, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts , jte, kts, kte )

      SAVE_L=0.
      SAVE_L=sum(g_ru_tendf**2)+sum(g_rv_tendf**2)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif


   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
  DO NT=1, Ndx
    ALPHA=0.1*ALPHA
    FACTOR=1.+ALPHA

    P_ru_tendf(:,:,:) = FACTOR * S_ru_tendf(:,:,:)
    P_rv_tendf(:,:,:) = FACTOR * S_rv_tendf(:,:,:)
    P_u(:,:,:) = FACTOR * S_u(:,:,:)
    P_v(:,:,:) = FACTOR * S_v(:,:,:)
    P_muu(:,:) = FACTOR *S_muu(:,:)
    P_muv(:,:) = FACTOR *S_muv (:,:)
    P_z(:,:,:)   = FACTOR *S_z(:,:,:)
    P_z_at_w(:,:,:) = FACTOR * S_z_at_w(:,:,:)

    call  surface_drag( P_ru_tendf, P_rv_tendf, P_u, P_v, xland,             &
                         P_muu, P_muv, P_z, P_z_at_w,                   &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte   )
    VAL_N=0.
    VAL_N= sum ((P_ru_tendf-B_ru_tendf)**2)&
          +sum ((P_rv_tendf-B_rv_tendf)**2)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

    VAL_L=SAVE_L*ALPHA**2
    COEF=VAL_N/VAL_L
    
    WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_mod_sfcdiffs: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L

    ENDDO

! ADJ Test

   FACTOR=0.01
   ru_tendf=S_ru_tendf
   rv_tendf=S_rv_tendf
   u=S_u
   v=S_v
   muu=S_muu
   muv=S_muv
   z=S_z
   z_at_w=S_z_at_w

   P_ru_tendf=FACTOR*S_ru_tendf
   P_rv_tendf=FACTOR*S_rv_tendf
   P_u=FACTOR*S_u
   P_v=FACTOR*S_v
   P_muu=FACTOR*S_muu
   P_muv=FACTOR*S_muv
   P_z=FACTOR*S_z
   P_z_at_w=FACTOR*S_z_at_w

   B_ru_tendf=P_ru_tendf
   B_rv_tendf=P_rv_tendf
   B_u=P_u
   B_v=P_v
   B_muu=P_muu
   B_muv=P_muv
   B_z=P_z
   B_z_at_w=P_z_at_w
   
  call   g_surface_drag( ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, u, P_u, v, P_v, xland, muu, P_muu, muv, &
  &P_muv,z,P_z, z_at_w, P_z_at_w, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts , jte, kts, kte )

   VAL_L=0.0
   VAL_L=sum(P_ru_tendf**2) + sum(P_rv_tendf**2)
   
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L= nsum
#endif

   P_u=0.0
   P_v=0.0
   P_muu=0.0
   P_muv=0.0
   P_z=0.0
   P_z_at_w=0.0
   ru_tendf=S_ru_tendf
   rv_tendf=S_rv_tendf

   call a_surface_drag(ru_tendf, P_ru_tendf, rv_tendf, P_rv_tendf, u, P_u, v, P_v, xland, muu, P_muu, muv, P_muv, z, &
&P_z, z_at_w, P_z_at_w, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

    VAL_A=0.0
    VAL_A= sum(P_ru_tendf*B_ru_tendf) +sum(P_rv_tendf*B_rv_tendf) &
          +sum(P_u*B_u) +sum(P_v*B_v) +sum(P_muu*B_muu) +sum(P_muv*B_muv) &
          +sum(P_z*B_z) +sum(P_z_at_w*B_z_at_w)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_surface_drag: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

! RECOVER

   ru_tendf=S_ru_tendf
   rv_tendf=S_rv_tendf
   u=S_u
   v=S_v
   muu=S_muu
   muv=S_muv
   z=S_z
   z_at_w=S_z_at_w
END SUBROUTINE t_surface_drag

!=======================================================================================================

  SUBROUTINE t_lscond( th_in,p,qv_in,rho,pii,r_v,xlv,cp,EP2,SVP1,SVP2,SVP3,SVPT0,dz8w,RAINNC_in,RAINNCV_in, &
  ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte,caller )

   USE module_mp_nconvp    
   IMPLICIT NONE
  
  ! Declare dummy arguments
    INTEGER, INTENT(IN) :: ids
    INTEGER, INTENT(IN) :: ide
    INTEGER, INTENT(IN) :: jds
    INTEGER, INTENT(IN) :: jde
    INTEGER, INTENT(IN) :: kds
    INTEGER, INTENT(IN) :: kde
    INTEGER, INTENT(IN) :: ims
    INTEGER, INTENT(IN) :: ime
    INTEGER, INTENT(IN) :: jms
    INTEGER, INTENT(IN) :: jme
    INTEGER, INTENT(IN) :: kms
    INTEGER, INTENT(IN) :: kme
    INTEGER, INTENT(IN) :: its
    INTEGER, INTENT(IN) :: ite
    INTEGER, INTENT(IN) :: jts
    INTEGER, INTENT(IN) :: jte
    INTEGER, INTENT(IN) :: kts
    INTEGER, INTENT(IN) :: kte
    REAL, INTENT(IN) :: th_in(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: p(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: qv_in(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: rho(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: pii(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: r_v
    REAL, INTENT(IN) :: xlv
    REAL, INTENT(IN) :: cp
    REAL, INTENT(IN) :: EP2
    REAL, INTENT(IN) :: SVP1
    REAL, INTENT(IN) :: SVP2
    REAL, INTENT(IN) :: SVP3
    REAL, INTENT(IN) :: SVPT0
    REAL, INTENT(IN) :: dz8w(ims:ime,kms:kme,jms:jme)
    REAL, INTENT(IN) :: RAINNC_in(ims:ime,jms:jme)
    REAL, INTENT(IN) :: RAINNCV_in(ims:ime,jms:jme)

    CHARACTER*(*), INTENT(IN) :: caller

  ! Declare local variables
    REAL :: th(ims:ime,kms:kme,jms:jme)
    REAL :: qv(ims:ime,kms:kme,jms:jme)
    REAL :: RAINNC(ims:ime,jms:jme)
    REAL :: RAINNCV(ims:ime,jms:jme)

  ! IN variables
    REAL :: S_p(ims:ime,kms:kme,jms:jme)
    REAL :: P_p(ims:ime,kms:kme,jms:jme)
    REAL :: B_p(ims:ime,kms:kme,jms:jme)
    REAL :: S_rho(ims:ime,kms:kme,jms:jme)
    REAL :: P_rho(ims:ime,kms:kme,jms:jme)
    REAL :: B_rho(ims:ime,kms:kme,jms:jme)
    REAL :: S_pii(ims:ime,kms:kme,jms:jme)
    REAL :: P_pii(ims:ime,kms:kme,jms:jme)
    REAL :: B_pii(ims:ime,kms:kme,jms:jme)
    REAL :: S_dz8w(ims:ime,kms:kme,jms:jme)
    REAL :: P_dz8w(ims:ime,kms:kme,jms:jme)
    REAL :: B_dz8w(ims:ime,kms:kme,jms:jme)

  ! INOUT variables
    REAL :: S_th(ims:ime,kms:kme,jms:jme)
    REAL :: P_th(ims:ime,kms:kme,jms:jme)
    REAL :: B_th(ims:ime,kms:kme,jms:jme)
    REAL :: S_qv(ims:ime,kms:kme,jms:jme)
    REAL :: P_qv(ims:ime,kms:kme,jms:jme)
    REAL :: B_qv(ims:ime,kms:kme,jms:jme)
    REAL :: S_RAINNC(ims:ime,jms:jme)
    REAL :: P_RAINNC(ims:ime,jms:jme)
    REAL :: B_RAINNC(ims:ime,jms:jme)
    REAL :: S_RAINNCV(ims:ime,jms:jme)
    REAL :: P_RAINNCV(ims:ime,jms:jme)
    REAL :: B_RAINNCV(ims:ime,jms:jme)

  ! OUT variables

  ! Other local declarations

    REAL :: SAVE_L, COEF_TST, ALPHA, FACTOR_TST, VAL_N, VAL_L, VAL_A
    REAL :: PERTURBATION
    INTEGER :: NT
    INTEGER :: i, j, k, n, s
    INTEGER :: i1, i2, i3, i4, i5, i6, i7

  ! Begin executable code

  ! Copy intent(out) and intent(inout) args

    th = th_in
    qv = qv_in
    RAINNC = RAINNC_in
    RAINNCV = RAINNCV_in

! TGL test

    S_p = p
    P_p = p
    S_rho = rho
    P_rho = rho
    S_pii = pii
    P_pii = pii
    S_dz8w = dz8w
    P_dz8w = dz8w
    S_th = th
    P_th = th
    S_qv = qv
    P_qv = qv
    S_RAINNC = RAINNC
    P_RAINNC = RAINNC
    S_RAINNCV = RAINNCV
    P_RAINNCV = RAINNCV

! NLM

  CALL lscond ( th,p,qv,rho,pii,r_v,xlv,cp,EP2,SVP1,SVP2,SVP3,SVPT0,dz8w,RAINNC,RAINNCV,ids,ide,jds,jde,kds, &
  kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte )

!   B_p(its:ite,kts:kte,jts:jte) = p(its:ite,kts:kte,jts:jte)
!   B_rho(its:ite,kts:kte,jts:jte) = rho(its:ite,kts:kte,jts:jte)
!   B_pii(its:ite,kts:kte,jts:jte) = pii(its:ite,kts:kte,jts:jte)
!   B_dz8w(its:ite,kts:kte,jts:jte) = dz8w(its:ite,kts:kte,jts:jte)
    B_th(its:ite,kts:kte,jts:jte) = th(its:ite,kts:kte,jts:jte)
    B_qv(its:ite,kts:kte,jts:jte) = qv(its:ite,kts:kte,jts:jte)
    B_RAINNC(its:ite,jts:jte) = RAINNC(its:ite,jts:jte)
    B_RAINNCV(its:ite,jts:jte) = RAINNCV(its:ite,jts:jte)

! TL

    th = S_th
    qv = S_qv
    RAINNC = S_RAINNC
    RAINNCV = S_RAINNCV
  CALL g_lscond ( th,P_th,p,P_p,qv,P_qv,rho,P_rho,pii,P_pii,r_v,xlv,cp,ep2,svp1,svp2,svp3,svpt0,dz8w,P_dz8w, &
  rainnc,P_rainnc,rainncv,P_rainncv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte  &
  )

    SAVE_L=0.
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      SAVE_L = SAVE_L + (P_th(i,k,j) * P_th(i,k,j))
    ENDDO
    ENDDO
    ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     SAVE_L = SAVE_L + (P_p(i,k,j) * P_p(i,k,j))
!   ENDDO
!   ENDDO
!   ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      SAVE_L = SAVE_L + (P_qv(i,k,j) * P_qv(i,k,j))
    ENDDO
    ENDDO
    ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     SAVE_L = SAVE_L + (P_rho(i,k,j) * P_rho(i,k,j))
!   ENDDO
!   ENDDO
!   ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     SAVE_L = SAVE_L + (P_pii(i,k,j) * P_pii(i,k,j))
!   ENDDO
!   ENDDO
!   ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     SAVE_L = SAVE_L + (P_dz8w(i,k,j) * P_dz8w(i,k,j))
!   ENDDO
!   ENDDO
!   ENDDO
    DO j = jts,jte
    DO i = its,ite
      SAVE_L = SAVE_L + (P_rainnc(i,j) * P_rainnc(i,j))
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      SAVE_L = SAVE_L + (P_rainncv(i,j) * P_rainncv(i,j))
    ENDDO
    ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
    DO NT=1, Ndx
      ALPHA=0.1*ALPHA
      FACTOR_TST=1.+ALPHA
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_th(i,k,j) = FACTOR_TST * S_th(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_p(i,k,j) = FACTOR_TST * S_p(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_qv(i,k,j) = FACTOR_TST * S_qv(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_rho(i,k,j) = FACTOR_TST * S_rho(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_pii(i,k,j) = FACTOR_TST * S_pii(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_dz8w(i,k,j) = FACTOR_TST * S_dz8w(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      P_rainnc(i,j) = FACTOR_TST * S_rainnc(i,j)
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      P_rainncv(i,j) = FACTOR_TST * S_rainncv(i,j)
    ENDDO
    ENDDO

  CALL lscond ( P_th,P_p,P_qv,P_rho,P_pii,r_v,xlv,cp,EP2,SVP1,SVP2,SVP3,SVPT0,P_dz8w,P_RAINNC,P_RAINNCV,ids,ide, &
  jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte )

    VAL_N=0.
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_N = VAL_N + ((P_th(i,k,j) - B_th(i,k,j)) * (P_th(i,k,j) - B_th(i,k,j)))
    ENDDO
    ENDDO
    ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     VAL_N = VAL_N + ((P_p(i,k,j) - B_p(i,k,j)) * (P_p(i,k,j) - B_p(i,k,j)))
!   ENDDO
!   ENDDO
!   ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_N = VAL_N + ((P_qv(i,k,j) - B_qv(i,k,j)) * (P_qv(i,k,j) - B_qv(i,k,j)))
    ENDDO
    ENDDO
    ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     VAL_N = VAL_N + ((P_rho(i,k,j) - B_rho(i,k,j)) * (P_rho(i,k,j) - B_rho(i,k,j)))
!   ENDDO
!   ENDDO
!   ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     VAL_N = VAL_N + ((P_pii(i,k,j) - B_pii(i,k,j)) * (P_pii(i,k,j) - B_pii(i,k,j)))
!   ENDDO
!   ENDDO
!   ENDDO
!   DO j = jts,jte
!   DO k = kts,kte
!   DO i = its,ite
!     VAL_N = VAL_N + ((P_dz8w(i,k,j) - B_dz8w(i,k,j)) * (P_dz8w(i,k,j) - B_dz8w(i,k,j)))
!   ENDDO
!   ENDDO
!   ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_N = VAL_N + ((P_rainnc(i,j) - B_rainnc(i,j)) * (P_rainnc(i,j) - B_rainnc(i,j)))
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_N = VAL_N + ((P_rainncv(i,j) - B_rainncv(i,j)) * (P_rainncv(i,j) - B_rainncv(i,j)))
    ENDDO
    ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

      VAL_L=SAVE_L*ALPHA**2
      IF (VAL_N .EQ. VAL_L) THEN
        COEF_TST=1.0   ! handle case where VAL_N==VAL_L==0.0
      ELSE
        COEF_TST=VAL_N/VAL_L
      ENDIF
      WRITE(6, fmt='(3A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'lscond_tl from ',TRIM(caller),': ALPHA=',ALPHA,'  COEF=',COEF_TST, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
    ENDDO

! ADJ test

  FACTOR_TST = 0.1
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      th(i,k,j)=S_th(i,k,j)
      P_th(i,k,j)=FACTOR_TST*S_th(i,k,j)
      B_th(i,k,j)=P_th(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_p(i,k,j)=FACTOR_TST*S_p(i,k,j)
      B_p(i,k,j)=P_p(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      qv(i,k,j)=S_qv(i,k,j)
      P_qv(i,k,j)=FACTOR_TST*S_qv(i,k,j)
      B_qv(i,k,j)=P_qv(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_rho(i,k,j)=FACTOR_TST*S_rho(i,k,j)
      B_rho(i,k,j)=P_rho(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_pii(i,k,j)=FACTOR_TST*S_pii(i,k,j)
      B_pii(i,k,j)=P_pii(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      P_dz8w(i,k,j)=FACTOR_TST*S_dz8w(i,k,j)
      B_dz8w(i,k,j)=P_dz8w(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      rainnc(i,j)=S_rainnc(i,j)
      P_rainnc(i,j)=FACTOR_TST*S_rainnc(i,j)
      B_rainnc(i,j)=P_rainnc(i,j)
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      rainncv(i,j)=S_rainncv(i,j)
      P_rainncv(i,j)=FACTOR_TST*S_rainncv(i,j)
      B_rainncv(i,j)=P_rainncv(i,j)
    ENDDO
    ENDDO
! TL

  CALL g_lscond ( th,P_th,p,P_p,qv,P_qv,rho,P_rho,pii,P_pii,r_v,xlv,cp,ep2,svp1,svp2,svp3,svpt0,dz8w,P_dz8w, &
  rainnc,P_rainnc,rainncv,P_rainncv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte  &
  )

    VAL_L=0.
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_th(i,k,j) * P_th(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_p(i,k,j) * P_p(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_qv(i,k,j) * P_qv(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_rho(i,k,j) * P_rho(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_pii(i,k,j) * P_pii(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_L = VAL_L + (P_dz8w(i,k,j) * P_dz8w(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_L = VAL_L + (P_rainnc(i,j) * P_rainnc(i,j))
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_L = VAL_L + (P_rainncv(i,j) * P_rainncv(i,j))
    ENDDO
    ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      th(i,k,j)=S_th(i,k,j)
      qv(i,k,j)=S_qv(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      rainnc(i,j)=S_rainnc(i,j)
      rainncv(i,j)=S_rainncv(i,j)
    ENDDO
    ENDDO


! ADJ

  CALL a_lscond ( th,P_th,p,P_p,qv,P_qv,rho,P_rho,pii,P_pii,r_v,xlv,cp,ep2,svp1,svp2,svp3,svpt0,dz8w,P_dz8w, &
  rainnc,P_rainnc,rainncv,P_rainncv,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kte )

    VAL_A=0.
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_th(i,k,j) * B_th(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_p(i,k,j) * B_p(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_qv(i,k,j) * B_qv(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_rho(i,k,j) * B_rho(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_pii(i,k,j) * B_pii(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      VAL_A = VAL_A + (P_dz8w(i,k,j) * B_dz8w(i,k,j))
    ENDDO
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_A = VAL_A + (P_rainnc(i,j) * B_rainnc(i,j))
    ENDDO
    ENDDO
    DO j = jts,jte
    DO i = its,ite
      VAL_A = VAL_A + (P_rainncv(i,j) * B_rainncv(i,j))
    ENDDO
    ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   WRITE(6,FMT='(3A,E22.13)') 'lscond_ad from ',TRIM(caller),' VAL_TL: ', VAL_L
   WRITE(6,FMT='(3A,E22.13)') 'lscond_ad from ',TRIM(caller),' VAL_AD: ', VAL_A

  END SUBROUTINE t_lscond

!---------------------------------------------------------------------------------
   SUBROUTINE t_moist_physics_prep_em( t_new, t_old, t0, rho, al, alb, &
                                     p, p8w, p0, pb, ph, phb, pii, pf,    &
                                     z, z_at_w, dz8w,                &
                                     dt,h_diabatic,                  &
                                     config_flags,fzm, fzp,          &
                                     ids,ide, jds,jde, kds,kde,      &
                                     ims,ime, jms,jme, kms,kme,      &
                                     its,ite, jts,jte, kts,kte      )

   IMPLICIT NONE

! Here we construct full fields

   TYPE(grid_config_rec_type),    INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   )  ::  dt

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: al,  &
                                                    alb, &
                                                    p,   &
                                                    pb,  &
                                                    ph,  &
                                                    phb


   REAL , DIMENSION( kms:kme )                  ::   fzm, &
                                                              fzp

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme )::rho,  &
                                                  pii,  &
                                                  pf,   &
                                                    z,  &
                                               z_at_w,  &
                                                 dz8w,  &
                                                  p8w
! pjj/cray
!                                                 p8w,  &
!                                          h_diabatic

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),       &
         INTENT(INOUT) ::                         h_diabatic

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(INOUT) ::                         t_new, &
                                                  t_old

   REAL, INTENT(IN   ) :: t0, p0
   REAL                :: z0,z1,z2,w1,w2

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k

!  IN variables
   REAL,  DIMENSION(ims:ime,kms:kme,jms:jme) ::   S_al, S_p, S_ph
   REAL,  DIMENSION(ims:ime,kms:kme,jms:jme) ::   P_al, P_p, P_ph
   REAL,  DIMENSION(ims:ime,kms:kme,jms:jme) ::   B_al, B_p, B_ph


!  OUT variables
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: P_rho, P_pii, P_pf, P_z, P_z_at_w, P_dz8w, P_p8w
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: B_rho, B_pii, B_pf, B_z, B_z_at_w, B_dz8w, B_p8w

! INOUT variables
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  ::  S_h_diabatic, S_t_new, S_t_old
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  ::  P_h_diabatic, P_t_new, P_t_old
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  ::  B_h_diabatic, B_t_new, B_t_old
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme )  ::  K_h_diabatic, K_t_new, K_t_old


   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

    i_start = its
    i_end   = min( ite,ide-1 )
    j_start = jts
    j_end   = min( jte,jde-1 )

    k_start = kts
    k_end = min( kte, kde-1 )

 S_al  =al
 S_p   =p
 S_ph  =ph

 P_al  =al
 P_p   =p
 P_ph  =ph

 S_h_diabatic =h_diabatic
 S_t_new =t_new
 S_t_old =t_old

 P_h_diabatic =h_diabatic
 P_t_new =t_new
 P_t_old =t_old

 K_h_diabatic =h_diabatic
 K_t_new =t_new
 K_t_old =t_old

!  NLM

    CALL  moist_physics_prep_em( t_new, t_old, t0, rho, al, alb, &
                                      p, p8w, p0, pb, ph, phb, pii, pf,    &
                                      z, z_at_w, dz8w,                &
                                      dt,h_diabatic,                  &
                                      config_flags,fzm, fzp,          &
                                      ids,ide, jds,jde, kds,kde,      &
                                      ims,ime, jms,jme, kms,kme,      &
                                      its,ite, jts,jte, kts,kte      )

 B_rho =rho
 B_pii =pii
 B_pf  =pf
 B_z   =z
 B_z_at_w =z_at_w
 B_dz8w   =dz8w

 B_h_diabatic =h_diabatic
 B_t_new =t_new
 B_t_old =t_old

  CALL  g_moist_physics_prep_em( K_t_new, P_t_new, K_t_old, P_t_old, t0, rho, P_rho, al, P_al, alb, p, P_p, p8w, P_p8w, p0, &
&pb, ph, P_ph, phb, pii, P_pii, pf, P_pf, z, P_z, z_at_w, P_z_at_w, dz8w, P_dz8w, dt, K_h_diabatic, P_h_diabatic, &
&config_flags, fzm, fzp, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

     SAVE_L=0.
     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       SAVE_L=SAVE_L+P_rho(i,k,j)**2+P_pii(i,k,j)**2+P_pf(i,k,j)**2+P_z(i,k,j)**2+  &
              P_z_at_w(i,k,j)**2+P_dz8w(i,k,j)**2+P_t_new(i,k,j)**2+P_t_old(i,k,j)**2
     ENDDO
     ENDDO
     ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif

   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA
      P_al  =FACTOR*S_al
      P_p   =FACTOR*S_p
      P_ph  =FACTOR*S_ph

      P_h_diabatic =FACTOR*S_h_diabatic
      P_t_new =FACTOR*S_t_new
      P_t_old =FACTOR*S_t_old

   CALL  moist_physics_prep_em( P_t_new, P_t_old, t0, P_rho, P_al, alb, &
                                     P_p, P_p8w, p0, pb, P_ph, phb, P_pii, P_pf,    &
                                     P_z, P_z_at_w, P_dz8w,                &
                                     dt,P_h_diabatic,                  &
                                     config_flags,fzm, fzp,          &
                                     ids,ide, jds,jde, kds,kde,      &
                                     ims,ime, jms,jme, kms,kme,      &
                                     its,ite, jts,jte, kts,kte      )

     VAL_N=0.
     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       VAL_N=VAL_N+(P_rho(i,k,j)-B_rho(i,k,j))**2+(P_pii(i,k,j)-B_pii(i,k,j))**2+(P_pf(i,k,j)-B_pf(i,k,j))**2+  &
             (P_z(i,k,j)-B_z(i,k,j))**2+(P_z_at_w(i,k,j)-B_z_at_w(i,k,j))**2+(P_dz8w(i,k,j)-B_dz8w(i,k,j))**2+  &
             (P_h_diabatic(i,k,j)-B_h_diabatic(i,k,j))**2+(P_t_new(i,k,j) -B_t_new(i,k,j))**2+  &
             (P_t_old(i,k,j)-B_t_old(i,k,j))**2
     ENDDO
     ENDDO
     ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L

      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_prep_em: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO


!  ADJ test

   FACTOR=0.1
 al  =S_al
 p   =S_p
 ph  =S_ph
 h_diabatic =S_h_diabatic
 t_new =S_t_new
 t_old =S_t_old

 P_al  =FACTOR*S_al
 P_p   =FACTOR*S_p
 P_ph  =FACTOR*S_ph
 P_h_diabatic =FACTOR*S_h_diabatic
 P_t_new =FACTOR*S_t_new
 P_t_old =FACTOR*S_t_old

 B_al  =P_al
 B_p   =P_p
 B_ph  =P_ph
 B_h_diabatic =P_h_diabatic
 B_t_new =P_t_new
 B_t_old =P_t_old

 K_h_diabatic =h_diabatic
 K_t_new =t_new
 K_t_old =t_old

call g_moist_physics_prep_em( t_new, P_t_new, t_old, P_t_old, t0, rho, P_rho, al, P_al, alb, p, P_p, p8w, P_p8w, p0, &
&pb, ph, P_ph, phb, pii, P_pii, pf, P_pf, z, P_z, z_at_w, P_z_at_w, dz8w, P_dz8w, dt, h_diabatic, P_h_diabatic, &
&config_flags, fzm, fzp, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

     VAL_L=0.
     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       VAL_L=VAL_L+P_rho(i,k,j)**2+P_pii(i,k,j)**2+P_pf(i,k,j)**2+P_z(i,k,j)**2+  &
              P_z_at_w(i,k,j)**2+P_dz8w(i,k,j)**2+P_h_diabatic(i,k,j)**2+  &
              P_t_new(i,k,j)**2+P_t_old(i,k,j)**2
     ENDDO
     ENDDO
     ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

 P_al  =0.0
 P_p   =0.0
 P_ph  =0.0

call a_moist_physics_prep_em( K_t_new, P_t_new, K_t_old, P_t_old, t0, rho, P_rho, al, P_al, alb, p, P_p, p8w, P_p8w, p0, &
&pb, ph, P_ph, phb, pii, P_pii, pf, P_pf, z, P_z, z_at_w, P_z_at_w, dz8w, P_dz8w, K_h_diabatic, P_h_diabatic, fzm, fzp, &
&ide, jde, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

     VAL_A=0.
     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       VAL_A=VAL_A+P_al(i,k,j)*B_al(i,k,j)+P_p(i,k,j)*B_p(i,k,j)+P_ph(i,k,j)*B_ph(i,k,j)+  &
              P_h_diabatic(i,k,j)*B_h_diabatic(i,k,j)+P_t_new(i,k,j)*B_t_new(i,k,j)+  &
              P_t_old(i,k,j)*B_t_old(i,k,j)
     ENDDO
     ENDDO
     ENDDO

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_prep_em: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

END SUBROUTINE t_moist_physics_prep_em

!---------------------------------------------------------------------------------
   SUBROUTINE t_DUCU(                                          &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,rho,RAINCV,NCA                                 &
             ,U,V,TH,T,W,dz8w,Z,Pcps,pi                      &
             ,W0AVG,XLV                                      &
             ,CP,RD,RV,G                                      &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            ! optionals
             ,RTHCUTEN,RQVCUTEN                              &
                                                             )


   USE module_cu_du , only : DUCU, DUCU_D, DUCU_B
!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER       ::                            &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER   :: STEPCU
   LOGICAL    :: warm_rain

   REAL          :: XLV
   REAL          :: CP,RD,RV,G,EP2
   REAL          :: SVP1,SVP2,SVP3,SVPT0

   INTEGER       :: KTAU

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         :: &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                          T, &
                                                         QV, &
                                                       dz8w, &
                                                          z, &
                                                       Pcps, &
                                                        rho, &
                                                         pi
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )        :: W0AVG
  REAL   :: DT, DX
!
   REAL, DIMENSION( ims:ime , jms:jme )                  :: RAINCV

   REAL,    DIMENSION( ims:ime , jms:jme )               :: NCA

   REAL, DIMENSION( ims:ime , jms:jme )                ::  CUBOT, &
                                                      CUTOP

   LOGICAL, DIMENSION( ims:ime , jms:jme )            :: CU_ACT_FLAG

!
! Optional arguments
!

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme )     :: RTHCUTEN, &
                                                   RQVCUTEN

!  xzhang: new definition
!  IN variables
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ::  S_TH, S_T, S_QV, S_dz8w, S_z, S_Pcps, S_rho 
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ::  P_TH, P_T, P_QV, P_dz8w, P_z, P_Pcps, P_rho 
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ::  B_TH, B_T, B_QV, B_dz8w, B_z, B_Pcps, B_rho 

! INOUT variables
   REAL, DIMENSION( ims:ime , jms:jme )           ::  S_RAINCV,P_RAINCV,B_RAINCV,K_RAINCV
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ::  S_RTHCUTEN, S_RQVCUTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ::  P_RTHCUTEN, P_RQVCUTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ::  B_RTHCUTEN, B_RQVCUTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ::  K_RTHCUTEN, K_RQVCUTEN

   REAL :: SAVE_L, COEF, ALPHA, FACTOR, VAL_N, VAL_L, VAL_A
   REAL :: PERTURBATION
   INTEGER :: NT,h

S_TH =TH
S_T =T
S_QV=QV
S_dz8w =dz8w
S_z =z
S_Pcps =Pcps
S_rho =rho

P_TH =TH
P_T =T
P_QV=QV
P_dz8w =dz8w
P_z =z
P_Pcps =Pcps
P_rho =rho

!S_RAINCV =RAINCV
!S_RTHCUTEN =RTHCUTEN
!S_RQVCUTEN =RQVCUTEN

!P_RAINCV =RAINCV
!P_RTHCUTEN =RTHCUTEN
!P_RQVCUTEN =RQVCUTEN

!K_RAINCV =RAINCV
!K_RTHCUTEN =RTHCUTEN
!K_RQVCUTEN =RQVCUTEN

!  NLM

    CALL  DUCU(                                          &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,rho,RAINCV,NCA                                 &
             ,U,V,TH,T,W,dz8w,Z,Pcps,pi                      &
             ,W0AVG,XLV                                      &
             ,CP,RD,RV,G                                      &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            ! optionals
             ,RTHCUTEN,RQVCUTEN                              &
                                                             )

B_RAINCV =RAINCV
B_RTHCUTEN =RTHCUTEN
B_RQVCUTEN =RQVCUTEN

!   CALL DUCU_D(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
! &  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, rhod, raincv&
! &  , raincvd, nca, u, v, th, thd, t, td, w, dz8w, dz8wd, z, zd, pcps, &
! &  pcpsd, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
! &  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, qvd, rthcuten, &
! &  rthcutend, rqvcuten, rqvcutend)

  CALL DUCU_D(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, P_rho, K_raincv&
&  , P_raincv, nca, u, v, th, P_th, t, P_t, w, dz8w, P_dz8w, z, P_z, pcps, &
&  P_pcps, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, P_qv, K_rthcuten, &
&  P_rthcuten, K_rqvcuten, P_rqvcuten)

     SAVE_L=0.
     SAVE_L= sum(P_RAINCV**2) + sum(P_RTHCUTEN **2)+ sum(P_RQVCUTEN **2)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( SAVE_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   SAVE_L = nsum
#endif


   call nl_get_tl_perturbation( 1, PERTURBATION )  
   ALPHA= alphaInit + PERTURBATION
   DO NT=1, Ndx
      ALPHA=0.1*ALPHA
      FACTOR=1.+ALPHA

	P_TH =FACTOR*S_TH
	P_T =FACTOR*S_T
	P_QV=FACTOR*S_QV
	P_dz8w =FACTOR*S_dz8w
	P_z =FACTOR*S_z
	P_Pcps =FACTOR*S_pcps
	P_rho =FACTOR*S_rho

!	P_RAINCV =FACTOR*S_RAINCV
!	P_RTHCUTEN =FACTOR*S_RTHCUTEN
!	P_RQVCUTEN =FACTOR*S_RQVCUTEN

    CALL  DUCU(                                          &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,P_rho,P_RAINCV,NCA                                 &
             ,U,V,P_TH,P_T,W,P_dz8w,P_Z,P_Pcps,pi                      &
             ,W0AVG,XLV                                      &
             ,CP,RD,RV,G                                      &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,P_QV                                             &
            ! optionals
             ,P_RTHCUTEN,P_RQVCUTEN                              &
                                                             )


     VAL_N=0.
     VAL_N= sum((P_RAINCV -B_RAINCV)**2) + sum((P_RTHCUTEN -B_RTHCUTEN)**2) + sum((P_RQVCUTEN -B_RQVCUTEN)**2) 

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_N, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_N = nsum
#endif

      VAL_L=SAVE_L*ALPHA**2
      COEF=VAL_N/VAL_L

      WRITE(6, fmt='(A,E9.4,A,E22.13,A,E13.6,A,E13.6)') &
         'g_cudu: ALPHA=',ALPHA,'  COEF=',COEF, &
         '  VAL_N=',VAL_N,'  VAL_L=',VAL_L
   ENDDO

! ADJ test

  FACTOR = 0.1

TH =S_TH
T =S_T
QV=S_QV
dz8w =S_dz8w
z =S_z
Pcps =S_Pcps
rho =S_rho

!RAINCV =S_RAINCV
!RTHCUTEN =S_RTHCUTEN
!RQVCUTEN =S_RQVCUTEN

P_TH =FACTOR*S_TH
P_T =FACTOR*S_T
P_QV=FACTOR*S_QV
P_dz8w =FACTOR*S_dz8w
P_z =FACTOR*S_z
P_Pcps =FACTOR*S_Pcps
P_rho =FACTOR*S_rho

!P_RAINCV =FACTOR*S_RAINCV
!P_RTHCUTEN =FACTOR*S_RTHCUTEN
!P_RQVCUTEN =FACTOR*S_RQVCUTEN

B_TH =P_TH
B_T =P_T
B_QV=P_QV
B_dz8w =P_dz8w
B_z =P_z
B_Pcps =P_Pcps
B_rho =P_rho

!B_RAINCV =P_RAINCV
!B_RTHCUTEN =P_RTHCUTEN
!B_RQVCUTEN =P_RQVCUTEN

!K_RAINCV =RAINCV
!K_RTHCUTEN =RTHCUTEN
!K_RQVCUTEN =RQVCUTEN

  CALL DUCU_D(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, P_rho, raincv&
&  , P_raincv, nca, u, v, th, P_th, t, P_t, w, dz8w, P_dz8w, z, P_z, pcps, &
&  P_pcps, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, P_qv, rthcuten, &
&  P_rthcuten, rqvcuten, P_rqvcuten)

     VAL_L=0.
     VAL_L= sum(P_RAINCV**2) + sum(P_RTHCUTEN **2)+ sum(P_RQVCUTEN **2)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_L, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_L = nsum
#endif

P_TH =0.0
P_T =0.0
P_QV=0.0
P_dz8w =0.0
P_z =0.0
P_Pcps =0.0
P_rho =0.0

!  call  DUCU_B(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
!&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, P_rho, K_raincv&
!&  , P_raincv, nca, u, v, th, P_th, t, P_t, w, dz8w, P_dz8w, z, P_z, pcps, &
!&  P_pcps, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
!&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, P_qv, K_rthcuten, &
!&  P_rthcuten, K_rqvcuten, P_rqvcuten)

  call  DUCU_B(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, P_rho, raincv&
&  , P_raincv, nca, u, v, th, P_th, t, P_t, w, dz8w, P_dz8w, z, P_z, pcps, &
&  P_pcps, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, P_qv, rthcuten, &
&  P_rthcuten, rqvcuten, P_rqvcuten)

     VAL_A=0.
     VAL_A= sum(P_TH*B_TH)+ sum(P_T*B_T)+ sum(P_QV*B_QV)+ sum(P_dz8w*B_dz8w)  &
            + sum(P_z*B_z)+ sum(P_Pcps*B_Pcps)+ sum(P_rho*B_rho) 
!            + sum(P_RAINCV*B_RAINCV) +sum(P_RTHCUTEN*B_RTHCUTEN) +sum(P_RQVCUTEN*B_RQVCUTEN)

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( VAL_A, nsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                       comm, IERROR )
   VAL_A = nsum
#endif

   print*, '                '
   write(6,*) 'a_CUDU: '
   write(6,fmt='(A,E22.13)') '      VAL_TL: ', VAL_L
   write(6,fmt='(A,E22.13)') '      VAL_AD: ', VAL_A

    END SUBROUTINE t_DUCU
!---------------------------------------------------------------------------------
!============================================================================================================

END MODULE module_check
