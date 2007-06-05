
!------------------------------
!------------------------------

BENCH_START(comp107)
#ifndef FAST_CPY
  xkmhd(:,:,:) = xkmhdh(:,:,:)
  w_2(:,:,:) = w_2h(:,:,:)
  w_1(:,:,:) = w_1h(:,:,:)
  v_2(:,:,:) = v_2h(:,:,:)
  v_1(:,:,:) = v_1h(:,:,:)
  u_2(:,:,:) = u_2h(:,:,:)
  u_1(:,:,:) = u_1h(:,:,:)
  t_tendf(:,:,:) = t_tendfh(:,:,:)
  t_2(:,:,:) = t_2h(:,:,:)
  t_1(:,:,:) = t_1h(:,:,:)
  rw_tendf(:,:,:) = rw_tendfh(:,:,:)
  rv_tendf(:,:,:) = rv_tendfh(:,:,:)
  ru_tendf(:,:,:) = ru_tendfh(:,:,:)
  pm1(:,:,:) = pm1h(:,:,:)
  ph_tendf(:,:,:) = ph_tendfh(:,:,:)
  ph_2(:,:,:) = ph_2h(:,:,:)
  ph_1(:,:,:) = ph_1h(:,:,:)
  p(:,:,:) = ph(:,:,:)
  mu_2(:,:) = mu_2h(:,:)
  mu_1(:,:) = mu_1h(:,:)
  moist_tend(:,:,:,:) = moist_tendh(:,:,:,:)
  moist_2(:,:,:,:) = moist_2h(:,:,:,:)
  moist_1(:,:,:,:) = moist_1h(:,:,:,:)
  dnw(:) = dnwh(:)
  al(:,:,:) = alh(:,:,:)
#else
  call fast_copy_lint(  xkmhd , xkmhdh, n3d_elem )
  call fast_copy_lint(  w_2 , w_2h, n3d_elem )
  call fast_copy_lint(  w_1 , w_1h, n3d_elem )
  call fast_copy_lint(  v_2 , v_2h, n3d_elem )
  call fast_copy_lint(  v_1 , v_1h, n3d_elem )
  call fast_copy_lint(  u_2 , u_2h, n3d_elem )
  call fast_copy_lint(  u_1 , u_1h, n3d_elem )
  call fast_copy_lint(  t_tendf , t_tendfh, n3d_elem )
  call fast_copy_lint(  t_2 , t_2h, n3d_elem )
  call fast_copy_lint(  t_1 , t_1h, n3d_elem )
  call fast_copy_lint(  rw_tendf , rw_tendfh, n3d_elem )
  call fast_copy_lint(  rv_tendf , rv_tendfh, n3d_elem )
  call fast_copy_lint(  ru_tendf , ru_tendfh, n3d_elem )
  call fast_copy_lint(  pm1 , pm1h, n3d_elem )
  call fast_copy_lint(  ph_tendf , ph_tendfh, n3d_elem )
  call fast_copy_lint(  ph_2 , ph_2h, n3d_elem )
  call fast_copy_lint(  ph_1 , ph_1h, n3d_elem )
  call fast_copy_lint(  p , ph, n3d_elem )
  call fast_copy_lint(  mu_2 , mu_2h, n2d_elem )
  call fast_copy_lint(  mu_1 , mu_1h, n2d_elem )
  call fast_copy_lint(  moist_tend(ims,kms,jms,2) , moist_tendh(ims,kms,jms,2), n3d_elem*(num_moist-1) )
  call fast_copy_lint(  moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2), n3d_elem*(num_moist-1) )
  call fast_copy_lint(  moist_1(ims,kms,jms,2) , moist_1h(ims,kms,jms,2), n3d_elem*(num_moist-1) )
  call fast_copy_lint(  dnw , dnwh, kme-kms+1 )
  call fast_copy_lint(  al , alh, n3d_elem )
#endif
!----------------------------------------

  dtm = dt
  if (rk_order .eq. 1) then
    if (step_number .ne. 1) then
      number_of_small_timesteps = 2*time_step_sound
      dt_rk = dt
      dtm = 2*dt
    else
      number_of_small_timesteps = time_step_sound
      dt_rk = dt/2.
      dtm = dt
    endif
    dts_rk = dts
  else if (rk_order .eq. 2) then
    if (rk_step .eq. 1) then
      dt_rk = 0.5*dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound/2
    else
      dt_rk = dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound
    endif
  else if (rk_order .eq. 3) then
    if (rk_step .eq. 1) then
      dt_rk = dt/3.
      dts_rk = dt_rk
      number_of_small_timesteps = 1
    else if (rk_step .eq. 2) then
      dt_rk = 0.5*dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound/2
    else
      dt_rk = dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound
    endif
  endif
! recompute : dt_rk,dts_rk,number_of_small_timesteps
BENCH_END(comp107)

BENCH_START(exch102)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_ADJ_A1.inc"
! halo HALO_ADJ_A1 dyn_em  48:ru,rv,rw,ww,php,alt,al,p,muu,muv,mut
#endif
BENCH_END(exch102)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 102
#include <all_debug_halo.h>
#endif

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepb4asd: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepb4asd:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepb4asd

BENCH_START(comp108)
! TBH:  OVERWRITE ru,rv,rw,ww,alt,muu,muv,mut,php,cqu,cqv,cqw
! TBH:  HORIZONTAL DEPENDENCE:  mu_2     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mub      ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  moist_2  ( [i-1,i    ], [j-1,j    ] )
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp108)

 endif restore_rk_step_prepb4asd

BENCH_START(exch103)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3723.inc"
! HALO_3723 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3728.inc"
! HALO_3728 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch103)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 103
#include <all_debug_halo.h>
#endif

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww
  rk_step_is_onzr: if (rk_step .eq. 1) then

BENCH_START(comp109)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp109)

BENCH_START(exch104)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch104)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 104
#include <all_debug_halo.h>
#endif

BENCH_START(comp110)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp110)

BENCH_START(exch105)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch105)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 105
#include <all_debug_halo.h>
#endif

BENCH_START(comp111)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    endif
BENCH_END(comp111)
  endif rk_step_is_onzr

#ifdef DM_PARALLEL
lineno = __LINE__




#endif

!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

BENCH_START(exch106)
#ifdef DM_PARALLEL
lineno = __LINE__
! TBH:  Note that exchange of z,z_at_w,a_z,a_z_at_w here only 
! TBH:  needs to be done when rk_step==1 .  Also, some of these halo 
! TBH:  thicknesses are HUGE!  Reduce if possible.  
! TBH:  Also, it appears that only the MAXIMUM halo size is used!!  What is 
! TBH:  up with that?  Registry appears to roll out maximum size halo for 
! TBH:  all arrays in an RSL_LITE halo update.  We should be able to improve 
! TBH:  on that!!  Also, RSL_LITE packs/unpacks TWICE -- once for "X" and 
! TBH:  once again for "Y".  Another opportunity for speedup.  Recall that 
! TBH:  this kind of optimization allowed SMS to beat hand-coded MPI for Eta 
! TBH:  which has many more halo updates than ARW, but not nearly as many as 
! TBH:  solve_em_ad()...  
#    include "HALO_3784.inc"
! HALO_3784 dyn_em 80:a_advect_tend,a_alt,a_cqu,a_cqv,a_cqw,a_moist_tend,a_mu_2,a_mu_tend,a_mut,a_muu,a_muv,a_p,a_ph_1,a_ph_2,a_ph_tend,a_php,a_ru,a_ru_tend,a_ru_tendf,a_rv,a_rv_tend,a_rv_tendf,a_rw,a_rw_tend,a_rw_tendf,a_t_1,a_t_2,a_t_tend,a_t_tendf,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww,al,alt,cosa,cqu,cqv,cqw,e,f,msft,msfu,msfv,mu_2,mub,mut,muu,muv,p,pb,ph_1,ph_2,phb,php,ru,rv,rw,t_1,t_2,t_init,u_1,u_2,v_1,v_2,w_1,w_2,ww,z,z_at_w,a_z,a_z_at_w,xland
#endif
BENCH_END(exch106)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 106
#include <all_debug_halo.h>
#endif

BENCH_START(comp_a_surface_drag)
!---------------------------------------------------------------------------------------------
!Zhang Xiaoyan add the AD of surface_drag on 12/05/2006
!---------------------------------------------------------------------------------------------
  IF (rk_step == 1 ) then

 call a_surface_drag(ru_tendf, a_ru_tendf, rv_tendf, a_rv_tendf, u_2, a_u_2, v_2, a_v_2, xland, muu, a_muu, muv, a_muv, z, &
&a_z, z_at_w, a_z_at_w, &
        ids, ide, jds, jde, kds, kde, &
        ims, ime, jms, jme, kms, kme, &
  grid%i_start(ij), grid%i_end(ij),                &
  grid%j_start(ij), grid%j_end(ij),                &
  k_start, k_end                                  )
  ENDIF
BENCH_END(comp_a_surface_drag)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 107
#include <all_debug_halo.h>
#endif

!---------------------------------------------------------------------------------------------
!                 END
!---------------------------------------------------------------------------------------------

BENCH_START(comp_a_rk_tendency1)
! TBH:  OVERWRITE a_al,a_alt,a_cqu,a_cqv,a_cqw,a_mu_2,a_mut,a_muu,a_muv,a_p,a_ph_2,a_ph_tend,a_php,a_ru,a_ru_tend,a_ru_tendf,a_rv,a_rv_tend,a_rv_tendf,a_rw,a_rw_tend,a_rw_tendf,a_t_2,a_t_1,a_t_tendf,a_u_2,a_u_1,a_v_2,a_v_1,a_w_2,a_w_1,a_ww,a_xkmhd
! TBH:  HORIZONTAL DEPENDENCE:  a_al        ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_alt       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  al          ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  alt         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_mu_2      ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_mut       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_muu       ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_muv       ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ph_2      ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  a_php       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ph_tend   ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  a_p         ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ru        ( [    i,i+1], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ru_tendf  ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ru_tend   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rv        ( [i-1,i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rv_tendf  ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rv_tend   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rw        ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rw_tendf  ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rw_tend   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_tendf   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_u_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_u_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_v_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_v_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_w_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_w_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_xkmhd     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  cosa        ( [i-1,i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  cqu         ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  cqv         ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  e           ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  f           ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msft        ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu        ( [i-1,i,i+2], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv        ( [i-1,i,i+1], [j-1,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2        ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  mut         ( [i-2,i,i+1], [j-2,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  muu         ( [i-4,i,i+4], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  muv         ( [i-1,i,i+1], [j-4,j,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  pb          ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  phb         ( [i-4,i,i+4], [j-4,j,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  ph          ( [i-4,i,i+4], [j-4,j,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  php         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  p           ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ru          ( [i-1,i,i+1], [j-2,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  rv          ( [i-2,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  rw          ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  sina        ( [    i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  t_2         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  t_init      ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  t_1         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2         ( [i-4,i,i+4], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_1         ( [i-1,i,i+2], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2         ( [i-1,i,i+1], [j-4,j,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  v_1         ( [i-1,i,i+1], [j-1,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  w_2         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  w_1         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  xkmhd       ( [i-1,i,i+1], [j-1,j,j+1] )
    call a_rk_tendency1( config_flags,rk_step,a_ru_tend,a_rv_tend,a_rw_tend,a_ph_tend,a_t_tend,a_ru_tendf,a_rv_tendf,a_rw_tendf,&
&a_t_tendf,a_mu_tend,a_u_save,a_v_save,a_w_save,a_ph_save,a_t_save,ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,u_2,a_u_2,v_2,a_v_2,w_2,&
&a_w_2,t_2,a_t_2,ph_2,a_ph_2,u_1,a_u_1,v_1,a_v_1,w_1,a_w_1,t_1,a_t_1,ph_1,a_ph_1,phb,t_init,mu_2,a_mu_2,mut,a_mut,muu,a_muu,&
&muv,a_muv,mub,al,a_al,alt,a_alt,p,a_p,pb,php,a_php,cqu,a_cqu,cqv,a_cqv,cqw,a_cqw,u_base,v_base,z_base,msfu,msfv,msft,f,e,sina,&
&cosa,fnm,fnp,rdn,rdnw,dt,rdx,rdy,kvdif,xkmhd,a_xkmhd,cf1,cf2,cf3,cfn,cfn1,non_hydrostatic,leapfrog,ids,ide,jds,jde,kde,ims,&
&ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-3,ids),  &
        min(grid%i_end(ij)+3,ide),    &
        max(grid%j_start(ij)-3,jds),  &
        min(grid%j_end(ij)+3,jde),k_start,k_end )
BENCH_END(comp_a_rk_tendency1)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 108
#include <all_debug_halo.h>
#endif

BENCH_START(exch107)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3803.inc"
! HALO_3803 dyn_em 80:a_alt,a_cqu,a_cqv,a_cqw,a_mu_2,a_mu_tend,a_mut,a_muu,a_muv,a_p,a_ph_1,a_ph_2,a_ph_tend,a_php,a_ru,a_ru_tend,a_ru_tendf,a_rv,a_rv_tend,a_rv_tendf,a_rw,a_rw_tend,a_rw_tendf,a_t_1,a_t_2,a_t_tend,a_t_tendf,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww,al,cqu,cqv,cqw,mu_2,muu,muv,p,ph_1,ph_2,php,ru,rv,rw,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww

#endif
BENCH_END(exch107)

BENCH_START(comp_a_rk_tendency2)
    call a_rk_tendency2( config_flags,rk_step,a_ru_tend,a_rv_tend,a_rw_tend,a_ph_tend,a_t_tend,a_ru_tendf,a_rv_tendf,a_rw_tendf,&
&a_t_tendf,a_mu_tend,a_u_save,a_v_save,a_w_save,a_ph_save,a_t_save,ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,u_2,a_u_2,v_2,a_v_2,w_2,&
&a_w_2,t_2,a_t_2,ph_2,a_ph_2,u_1,a_u_1,v_1,a_v_1,w_1,a_w_1,t_1,a_t_1,ph_1,a_ph_1,phb,t_init,mu_2,a_mu_2,mut,a_mut,muu,a_muu,&
&muv,a_muv,mub,al,a_al,alt,a_alt,p,a_p,pb,php,a_php,cqu,a_cqu,cqv,a_cqv,cqw,a_cqw,u_base,v_base,z_base,msfu,msfv,msft,f,e,sina,&
&cosa,fnm,fnp,rdn,rdnw,dt,rdx,rdy,kvdif,xkmhd,a_xkmhd,cf1,cf2,cf3,cfn,cfn1,non_hydrostatic,leapfrog,ids,ide,jds,jde,kde,ims,&
&ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-3,ids),  &
        min(grid%i_end(ij)+3,ide),    &
        max(grid%j_start(ij)-3,jds),  &
        min(grid%j_end(ij)+3,jde),k_start,k_end )
BENCH_END(comp_a_rk_tendency2)
BENCH_START(comp113)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 113
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

#ifndef FAST_CPY
  xkmhd(:,:,:) = xkmhdh(:,:,:)
  w_2(:,:,:) = w_2h(:,:,:)
  w_1(:,:,:) = w_1h(:,:,:)
  v_2(:,:,:) = v_2h(:,:,:)
  v_1(:,:,:) = v_1h(:,:,:)
  u_2(:,:,:) = u_2h(:,:,:)
  u_1(:,:,:) = u_1h(:,:,:)
  t_tendf(:,:,:) = t_tendfh(:,:,:)
  t_2(:,:,:) = t_2h(:,:,:)
  t_1(:,:,:) = t_1h(:,:,:)
  rw_tendf(:,:,:) = rw_tendfh(:,:,:)
  rv_tendf(:,:,:) = rv_tendfh(:,:,:)
  ru_tendf(:,:,:) = ru_tendfh(:,:,:)
  pm1(:,:,:) = pm1h(:,:,:)
  ph_tendf(:,:,:) = ph_tendfh(:,:,:)
  ph_2(:,:,:) = ph_2h(:,:,:)
  ph_1(:,:,:) = ph_1h(:,:,:)
  p(:,:,:) = ph(:,:,:)
  mu_2(:,:) = mu_2h(:,:)
  mu_1(:,:) = mu_1h(:,:)
  moist_tend(:,:,:,:) = moist_tendh(:,:,:,:)
  moist_2(:,:,:,:) = moist_2h(:,:,:,:)
  moist_1(:,:,:,:) = moist_1h(:,:,:,:)
  dnw(:) = dnwh(:)
  al(:,:,:) = alh(:,:,:)
#else
  call fast_copy_lint(  xkmhd , xkmhdh , n3d_elem )
  call fast_copy_lint(  w_2 , w_2h , n3d_elem )
  call fast_copy_lint(  w_1 , w_1h , n3d_elem )
  call fast_copy_lint(  v_2 , v_2h , n3d_elem )
  call fast_copy_lint(  v_1 , v_1h , n3d_elem )
  call fast_copy_lint(  u_2 , u_2h , n3d_elem )
  call fast_copy_lint(  u_1 , u_1h , n3d_elem )
  call fast_copy_lint(  t_tendf , t_tendfh , n3d_elem )
  call fast_copy_lint(  t_2 , t_2h , n3d_elem )
  call fast_copy_lint(  t_1 , t_1h , n3d_elem )
  call fast_copy_lint(  rw_tendf , rw_tendfh , n3d_elem )
  call fast_copy_lint(  rv_tendf , rv_tendfh , n3d_elem )
  call fast_copy_lint(  ru_tendf , ru_tendfh , n3d_elem )
  call fast_copy_lint(  pm1 , pm1h , n3d_elem )
  call fast_copy_lint(  ph_tendf , ph_tendfh , n3d_elem )
  call fast_copy_lint(  ph_2 , ph_2h , n3d_elem )
  call fast_copy_lint(  ph_1 , ph_1h , n3d_elem )
  call fast_copy_lint(  p , ph , n3d_elem )
  call fast_copy_lint(  mu_2 , mu_2h , n2d_elem )
  call fast_copy_lint(  mu_1 , mu_1h , n2d_elem )
  call fast_copy_lint(  moist_tend(ims,kms,jms,2) , moist_tendh(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
  call fast_copy_lint(  moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
  call fast_copy_lint(  moist_1(ims,kms,jms,2) , moist_1h(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
  call fast_copy_lint(  dnw , dnwh , kme-kms+1 )
  call fast_copy_lint(  al , alh , n3d_elem )
#endif
!----------------------------------------
  dtm = dt
  if (rk_order .eq. 1) then
    if (step_number .ne. 1) then
      number_of_small_timesteps = 2*time_step_sound
      dt_rk = dt
      dtm = 2*dt
    else
      number_of_small_timesteps = time_step_sound
      dt_rk = dt/2.
      dtm = dt
    endif
    dts_rk = dts
  else if (rk_order .eq. 2) then
    if (rk_step .eq. 1) then
      dt_rk = 0.5*dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound/2
    else
      dt_rk = dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound
    endif
  else if (rk_order .eq. 3) then
    if (rk_step .eq. 1) then
      dt_rk = dt/3.
      dts_rk = dt_rk
      number_of_small_timesteps = 1
    else if (rk_step .eq. 2) then
      dt_rk = 0.5*dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound/2
    else
      dt_rk = dt
      dts_rk = dts
      number_of_small_timesteps = time_step_sound
    endif
  endif
BENCH_END(comp113)

BENCH_START(exch108)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3886.inc"
! HALO_3886 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch108)

! recompute : dt_rk,dts_rk,number_of_small_timesteps

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepb4ackk: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepb4ackk:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepb4ackk

BENCH_START(comp114)
! TBH:  OVERWRITE ru,rv,rw,ww,alt,muu,muv,mut,php,cqu,cqv,cqw
! TBH:  HORIZONTAL DEPENDENCE:  mu_2     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mub      ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  moist_2  ( [i-1,i    ], [j-1,j    ] )
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp114)

 endif restore_rk_step_prepb4ackk

BENCH_START(exch109)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3898.inc"
! HALO_3898 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww 
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3903.inc"
! HALO_3903 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch109)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww

  a_rk_step_is_one: if (rk_step .eq. 1) then
! recompute : p8w,p_phy,t8w,t_phy,th_phy
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then


BENCH_START(exch110)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch110)

BENCH_START(comp115)
        call a_calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,a_xkmhd,xkmv,xkhh,xkhv,bn2,a_bn2h,khdif,div,&
&defor11,defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,a_p8w,t8w,a_t8w,th_phy,a_th_phy,t_phy,a_t_phy,&
&p_phy,a_p_phy,moist_2,a_moist_2,dn,dnw,dx,dy,rdz,rdzw,num_3d_m,cf1,cf2,cf3,kh_tke_upper_bound,ids,ide,jds,jde,kde,ims,ime,&
&jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp115)

    endif

BENCH_START(exch111)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3935.inc"
! TBH CHANGE:  Move update of a_mu_2 and a_moist_2 to HALO_3982
! HALO_3935 dyn_em 48:a_alt,a_moist_2,a_moist_tend,a_mu_2,a_p,a_ph_2,a_ph_tendf,a_php,a_ru_tendf,a_rv_tendf,a_rw_tendf,a_t_2,a_t_tendf,a_u_2,a_v_2,a_w_2,a_ww,al,alt,muu,muv,p,php,ru,rv,rw,ww



#endif
BENCH_END(exch111)

BENCH_START(comp116)
      call a_phy_prep( p,a_p,pb,ph_2,a_ph_2,phb,t_2,a_t_2,th_phy,a_th_phy,p_phy,a_p_phy,pi_phy,a_pi_phyh,a_p8w,t_phy,a_t_phy,a_t8w,&
&z,a_z,z_at_w,a_z_at_wh,fnm,fnp,ide,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )

      call a_init_zero_tendency( a_ru_tendf,a_rv_tendf,a_rw_tendf,a_ph_tendf,a_t_tendf,a_moist_tend,num_3d_m,ims,ime,jms,jme,kms,&
&kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp116)

  endif a_rk_step_is_one

BENCH_START(exch112)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3963.inc"
! TBH CHANGE:  Move update of a_mu_2 and msft to HALO_3982
! TBH ???:  Does msft ever need update?  
! HALO_3963 dyn_em 48:a_alt,a_moist_tend,a_mu_1,a_mu_2,a_p,a_ph_1,a_ph_2,a_ph_tendf,a_php,a_ru_tendf,a_rv_tendf,a_rw_tendf,a_t_1,a_t_2,a_t_tendf,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww,msft






#endif
BENCH_END(exch112)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 116
#include <all_debug_halo.h>
#endif

BENCH_START(comp117)
#ifndef FAST_CPY
  dnw(:) = dnwh(:)
  moist_2(:,:,:,:) = moist_2h(:,:,:,:)
  mu_2(:,:) = mu_2h(:,:)
  u_2(:,:,:) = u_2h(:,:,:)
  v_2(:,:,:) = v_2h(:,:,:)
  w_2(:,:,:) = w_2h(:,:,:)
#else
  call fast_copy_lint( dnw , dnwh , kme-kms+1 )
  call fast_copy_lint( moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
  call fast_copy_lint( mu_2 , mu_2h , n2d_elem )
  call fast_copy_lint( u_2 , u_2h , n3d_elem )
  call fast_copy_lint( v_2 , v_2h , n3d_elem )
  call fast_copy_lint( w_2 , w_2h , n3d_elem )
#endif
BENCH_END(comp117)

BENCH_START(exch113)
#ifdef DM_PARALLEL
lineno = __LINE__
# include "HALO_3982.inc"
! TBH CHANGE:  Remove moist_2, mu_2, u_2, v_2, w_2 due to restore above
! TBH CHANGE:  Remove a_cqu, a_cqv, a_cqw, a_mut, a_ru, a_rv, a_w_2, since 
! TBH CHANGE   they have no dependence below
! TBH CHANGE:  Reduce extent to 8-point for ALL FIELDS
! TBH CHANGE:  Remove msfu, msfv, mub since they are not overwritten above 
! TBH CHANGE:  since their most recent halo update.  
! OLD: HALO_3982 dyn_em 48:a_cqu,a_cqv,a_cqw,a_mut,a_muu,a_muv,a_ru,a_rv,a_rw,a_u_2,a_v_2,a_w_2,a_ww,moist_2,msfu,msfv,mu_2,mub,u_2,v_2,w_2
! NEW: HALO_3982 dyn_em 8:a_muu,a_muv,a_rw,a_u_2,a_v_2,a_ww

#endif
BENCH_END(exch113)

BENCH_START(comp118)
! TBH:  OVERWRITE muu,muv,mut,a_al,a_alt,a_cqu,a_cqv,a_cqw,a_moist_2,a_mu,a_mut,a_muu,a_muv,a_ph,a_php,a_ru,a_rv,a_rw,a_u_2,a_v,a_w,a_ww
! TBH:  HORIZONTAL DEPENDENCE:  moist_2    ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_moist_2  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mub        ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2        ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2        ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_v_2      ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_mu_2       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_ww       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msft       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu       ( [i-1,i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv       ( [    i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_muu      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_muv      ( [    i    ], [    j,j+1] )
    call a_rk_step_prep( config_flags,u_2,a_u_2,v_2,a_v_2,w_2,a_w_2,a_ph_2,mu_2,a_mu_2,moist_2,a_moist_2,a_ru,a_rv,a_rw,a_ww,a_php,&
&a_alt,muu,a_muu,muv,a_muv,mub,mut,a_mut,a_al,a_cqu,a_cqv,a_cqw,msfu,msfv,msft,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kde,ims,&
&ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp118)

BENCH_START(exch114)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3997.inc"
! TBH CHANGE:  Look at removing these last...  
! HALO_3997 dyn_em 48:a_al,a_alt,a_moist_2,a_mu_2,a_ph_2,a_ph_tendf,a_php,a_t_2,a_u_2,a_v_2,a_w_2,a_ww 
#endif
BENCH_END(exch114)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 118
#include <all_debug_halo.h>
#endif

end do a_runge_kutta_loop

#include <bench_solve_em_a_end.h>

!----------------------------------------------
! FREE DYNAMIC MEMORY
!----------------------------------------------
#endif

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 99999
#include <all_debug_halo.h>
  call wrf_message ( 'DEBUG:  EARLY EXIT FOR HALO TESTS' )
  call wrf_shutdown
#endif

CALL module_4dvaropt_end
CALL wrf_debug ( 10 , 'DEBUG:  end solve_em_ad()' )

end subroutine solve_em_ad


