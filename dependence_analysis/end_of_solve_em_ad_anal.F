
!------------------------------
!------------------------------

a_runge_kutta_loop: do rk_step = rk_order, 1, -1

BENCH_START(comp15)

! TBH:  Eliminate this copy when rk_step > 1 and 
! TBH:  config_flags%remove_runge_kutta_loor == .TRUE.
if ( .not. ( config_flags%remove_runge_kutta_loor .and. &
           ( rk_step > 1 ) ) ) then
#ifndef FAST_CPY
  a(:,:,:) = ah(:,:,:)
  al(:,:,:) = alj(:,:,:)
  alpha(:,:,:) = alphah(:,:,:)
  gamma(:,:,:) = gammah(:,:,:)
  moist_1(:,:,:,:) = moist_1i(:,:,:,:)
  moist_2(:,:,:,:) = moist_2i(:,:,:,:)
  moist_tend(:,:,:,:) = moist_tendi(:,:,:,:)
  mu_1(:,:) = mu_1i(:,:)
  mu_2(:,:) = mu_2j(:,:)
  p(:,:,:) = pj(:,:,:)
  ph_1(:,:,:) = ph_1i(:,:,:)
  ph_2(:,:,:) = ph_2j(:,:,:)
  ph_tendf(:,:,:) = ph_tendfi(:,:,:)
  pm1(:,:,:) = pm1i(:,:,:)
  ru_m(:,:,:) = ru_mh(:,:,:)
  ru_tendf(:,:,:) = ru_tendfi(:,:,:)
  rv_m(:,:,:) = rv_mh(:,:,:)
  rv_tendf(:,:,:) = rv_tendfi(:,:,:)
  rw_tendf(:,:,:) = rw_tendfi(:,:,:)
  t_1(:,:,:) = t_1i(:,:,:)
  t_2(:,:,:) = t_2j(:,:,:)
  t_tendf(:,:,:) = t_tendfi(:,:,:)
  u_1(:,:,:) = u_1i(:,:,:)
  u_2(:,:,:) = u_2i(:,:,:)
  v_1(:,:,:) = v_1i(:,:,:)
  v_2(:,:,:) = v_2i(:,:,:)
  w_1(:,:,:) = w_1i(:,:,:)
  w_2(:,:,:) = w_2j(:,:,:)
  ww_m(:,:,:) = ww_mh(:,:,:)
  xkmhd(:,:,:) = xkmhdi(:,:,:)
#else
 call fast_copy_lint(  a , ah , n3d_elem )
 call fast_copy_lint(  al , alj , n3d_elem )
 call fast_copy_lint(  alpha , alphah , n3d_elem )
 call fast_copy_lint(  gamma , gammah , n3d_elem )
 call fast_copy_lint(  moist_1(ims,kms,jms,2) , moist_1i(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
 call fast_copy_lint(  moist_2(ims,kms,jms,2) , moist_2i(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
 call fast_copy_lint(  moist_tend(ims,kms,jms,2) , moist_tendi(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
 call fast_copy_lint(  mu_1 , mu_1i , n2d_elem )
 call fast_copy_lint(  mu_2 , mu_2j , n2d_elem )
 call fast_copy_lint(  p , pj , n3d_elem )
 call fast_copy_lint(  ph_1 , ph_1i , n3d_elem )
 call fast_copy_lint(  ph_2 , ph_2j , n3d_elem )
 call fast_copy_lint(  ph_tendf , ph_tendfi , n3d_elem )
 call fast_copy_lint(  pm1 , pm1i , n3d_elem )
 call fast_copy_lint(  ru_m , ru_mh , n3d_elem )
 call fast_copy_lint(  ru_tendf , ru_tendfi , n3d_elem )
 call fast_copy_lint(  rv_m , rv_mh , n3d_elem )
 call fast_copy_lint(  rv_tendf , rv_tendfi , n3d_elem )
 call fast_copy_lint(  rw_tendf , rw_tendfi , n3d_elem )
 call fast_copy_lint(  t_1 , t_1i , n3d_elem )
 call fast_copy_lint(  t_2 , t_2j , n3d_elem )
 call fast_copy_lint(  t_tendf , t_tendfi , n3d_elem )
 call fast_copy_lint(  u_1 , u_1i , n3d_elem )
 call fast_copy_lint(  u_2 , u_2i , n3d_elem )
 call fast_copy_lint(  v_1 , v_1i , n3d_elem )
 call fast_copy_lint(  v_2 , v_2i , n3d_elem )
 call fast_copy_lint(  w_1 , w_1i , n3d_elem )
 call fast_copy_lint(  w_2 , w_2j , n3d_elem )
 call fast_copy_lint(  ww_m , ww_mh , n3d_elem )
 call fast_copy_lint(  xkmhd , xkmhdi , n3d_elem )
#endif
endif

BENCH_END(comp15)

! TBH:  restore_runge_kutta_loor
! TBH:  HALO UPDATE of:  ww_m,rv_m,ru_m,a,alpha,gamma,xkmhd,w_2,w_1,v_2,v_1,u_2,u_1,t_tendf,t_2,t_1,rw_tendf,rv_tendf,ru_tendf,pm1,ph_tendf,ph_2,ph_1,p,mu_2,mu_1,moist_tend,moist_2,moist_1,dnw,al,mudf

BENCH_START(exch26)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1290.inc"
! TBH:  move this inside restore_runge_kutta_loor "if"
! HALO_1290 dyn_em 48:a,al,alpha,gamma,moist_1,moist_2,mu_1,mu_2,mudf,p,ph_1,ph_2,pm1,ru_m,rv_m,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww_m
#endif
BENCH_END(exch26)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 26
#include <all_debug_halo.h>
#endif

! TBH:  how much of this copy is redundant?  
BENCH_START(comp27)

#ifndef FAST_CPY
!------------------------------
  ww_m_keep3(:,:,:) = ww_m(:,:,:)
  rv_m_keep3(:,:,:) = rv_m(:,:,:)
  ru_m_keep3(:,:,:) = ru_m(:,:,:)
  a_keep3(:,:,:) = a(:,:,:)
  alpha_keep3(:,:,:) = alpha(:,:,:)
  gamma_keep3(:,:,:) = gamma(:,:,:)
!------------------------------

  xkmhdh(:,:,:) = xkmhd(:,:,:)
  w_2h(:,:,:) = w_2(:,:,:)
  w_1h(:,:,:) = w_1(:,:,:)
  v_2h(:,:,:) = v_2(:,:,:)
  v_1h(:,:,:) = v_1(:,:,:)
  u_2h(:,:,:) = u_2(:,:,:)
  u_1h(:,:,:) = u_1(:,:,:)
  t_tendfh(:,:,:) = t_tendf(:,:,:)
  t_2h(:,:,:) = t_2(:,:,:)
  t_1h(:,:,:) = t_1(:,:,:)
  rw_tendfh(:,:,:) = rw_tendf(:,:,:)
  rv_tendfh(:,:,:) = rv_tendf(:,:,:)
  ru_tendfh(:,:,:) = ru_tendf(:,:,:)
  pm1h(:,:,:) = pm1(:,:,:)
  ph_tendfh(:,:,:) = ph_tendf(:,:,:)
  ph_2h(:,:,:) = ph_2(:,:,:)
  ph_1h(:,:,:) = ph_1(:,:,:)
  ph(:,:,:) = p(:,:,:)
  mu_2h(:,:) = mu_2(:,:)
  mu_1h(:,:) = mu_1(:,:)
  moist_tendh(:,:,:,:) = moist_tend(:,:,:,:)
  moist_2h(:,:,:,:) = moist_2(:,:,:,:)
  moist_1h(:,:,:,:) = moist_1(:,:,:,:)
  dnwh(:) = dnw(:)
  alh(:,:,:) = al(:,:,:)
!----------------------------------------
  mudf_ma(:,:) = mudf(:,:)
!----------------------------------------
#else
!------------------------------
  call fast_copy_lint(  ww_m_keep3 , ww_m , n3d_elem )
  call fast_copy_lint(  rv_m_keep3 , rv_m , n3d_elem )
  call fast_copy_lint(  ru_m_keep3 , ru_m , n3d_elem )
  call fast_copy_lint(  a_keep3 , a , n3d_elem )
  call fast_copy_lint(  alpha_keep3 , alpha , n3d_elem )
  call fast_copy_lint(  gamma_keep3 , gamma , n3d_elem )
!------------------------------

  call fast_copy_lint(  xkmhdh , xkmhd , n3d_elem )
  call fast_copy_lint(  w_2h , w_2 , n3d_elem )
  call fast_copy_lint(  w_1h , w_1 , n3d_elem )
  call fast_copy_lint(  v_2h , v_2 , n3d_elem )
  call fast_copy_lint(  v_1h , v_1 , n3d_elem )
  call fast_copy_lint(  u_2h , u_2 , n3d_elem )
  call fast_copy_lint(  u_1h , u_1 , n3d_elem )
  call fast_copy_lint(  t_tendfh , t_tendf , n3d_elem )
  call fast_copy_lint(  t_2h , t_2 , n3d_elem )
  call fast_copy_lint(  t_1h , t_1 , n3d_elem )
  call fast_copy_lint(  rw_tendfh , rw_tendf , n3d_elem )
  call fast_copy_lint(  rv_tendfh , rv_tendf , n3d_elem )
  call fast_copy_lint(  ru_tendfh , ru_tendf , n3d_elem )
  call fast_copy_lint(  pm1h , pm1 , n3d_elem )
  call fast_copy_lint(  ph_tendfh , ph_tendf , n3d_elem )
  call fast_copy_lint(  ph_2h , ph_2 , n3d_elem )
  call fast_copy_lint(  ph_1h , ph_1 , n3d_elem )
  call fast_copy_lint(  ph , p , n3d_elem )
  call fast_copy_lint(  mu_2h , mu_2 , n2d_elem )
  call fast_copy_lint(  mu_1h , mu_1 , n2d_elem )
  call fast_copy_lint(  moist_tendh(ims,kms,jms,2) , moist_tend(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
  call fast_copy_lint(  moist_2h(ims,kms,jms,2) , moist_2(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
  call fast_copy_lint(  moist_1h(ims,kms,jms,2) , moist_1(ims,kms,jms,2) , n3d_elem * ( num_moist - 1 ) )
  call fast_copy_lint(  dnwh , dnw , kme-kms+1 )
  call fast_copy_lint(  alh , al , n3d_elem )
!----------------------------------------
  call fast_copy_lint(  mudf_ma , mudf , n2d_elem )
!----------------------------------------
#endif

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
BENCH_END(comp27)
! recompute : dt_rk,dts_rk,number_of_small_timesteps

BENCH_START(exch23)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1373.inc"
! HALO_1373 dyn_em 48:alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch23)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepzq: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepzq:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepzq

BENCH_START(comp28)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp28)

 endif restore_rk_step_prepzq

BENCH_START(exch24)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1384.inc"
! HALO_1384 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1389.inc"
! HALO_1389 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1394.inc"
! HALO_1394 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch24)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww
  rk_step_is_onzc: if (rk_step .eq. 1) then

BENCH_START(comp29)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp29)

BENCH_START(exch25)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1407.inc"
! HALO_1407 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch25)

BENCH_START(comp30)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp30)

BENCH_START(exch26)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1419.inc"
! HALO_1419 dyn_em 8:z


#endif
BENCH_END(exch26)

BENCH_START(comp31)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    endif
BENCH_END(comp31)
  endif rk_step_is_onzc
!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

BENCH_START(exch27)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1441.inc"
! HALO_1441 dyn_em 8:moist_2


#endif
BENCH_END(exch27)
WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_small_stepzq, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier between the call to rk_tendency and 
! TBH:  the end of small_stepu loop.  
 restore_rktend_thru_sszq: if ( config_flags%remove_rktend_thru_ss ) then
BENCH_START(comprestore_rktend_thru_ss_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktend_thru_sszq:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND_THRU_SS)
#include "restore_rktend_thru_ss.h"
BENCH_END(comprestore_rktend_thru_ss_tim)
 else  ! restore_rktend_thru_sszq

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendzq: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendzq:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendzq

BENCH_START(comp7773)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp7773)

 endif restore_rktendzq

!  end do
BENCH_START(comp32)

!  recompute : cqw,mu_tend,ph_save,ph_tend,ru_tend,ru_tendf,rv_tend,rv_t
! endf,rw_tend,rw_tendf,t_save,t_tend,t_tendf,u_save,v_save,w_save
!  do ij = 1, grid%num_tiles
    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (config_flags%specified .or. config_flags%nested) then
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif

! recompute : mu_tend,ph_tend,ru_tend,rv_tend,rw_tend,t_tend

    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (non_hydrostatic) then
      call calc_coef_w( a,alpha,gamma,mut,cqw,rdn,rdnw,c2a,dts,g,epssm,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp32)

BENCH_START(exch28)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_1487.inc"
! HALO_1487 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,ph_2,php,t_1,t_save,u_save,v_save 
#endif
BENCH_END(exch28)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by small_stepu loop.  
 restore_small_stepzq: if (config_flags%remove_small_step) then
BENCH_START(comprestore_small_step_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_small_stepzq:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_SMALL_STEP)
#include "restore_small_step.h"
BENCH_END(comprestore_small_step_tim)
 else  ! restore_small_stepzq

  small_stepzq: do iteration = 1, number_of_small_timesteps
WRITE(wrf_err_message,*) 'DEBUG:  begin small_stepzq, iteration = ',iteration
CALL wrf_debug( 10, TRIM(wrf_err_message) )

BENCH_START(comp33)
      call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
BENCH_END(comp33)

BENCH_START(exch29)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1507.inc"
! HALO_1507 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch29)

BENCH_START(comp34)
      call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp34)

BENCH_START(exch30)
#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_1518.inc"
! HALO_1518 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww 



#endif
BENCH_END(exch30)

BENCH_START(comp35)
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
      endif
      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (non_hydrostatic) then
        call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,alb,&
&a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then
          call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          if (config_flags%specified) then
            call zero_grad_bdy( w_2,'w',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,&
&kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else
            call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
      endif
      call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,iteration,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp35)

BENCH_START(exch31)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_1560.inc"
! HALO_1560 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save

#endif
BENCH_END(exch31)

  end do small_stepzq

 endif restore_small_stepzq

 endif restore_rktend_thru_sszq

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 35
#include <all_debug_halo.h>
#endif

! recompute : mu_2,mudf,muts,ph_2,pm1,ru_m,rv_m,t_2,u_2,v_2,w_2,ww_m

BENCH_START(comp36)
    call calc_mu_uv_1( config_flags,muts,muus,muvs,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
!    call small_step_finish( u_2,u_1,v_2,v_1,w_2,w_1,t_2,t_1,ph_2,ph_1,ww,ww1,mu_2,mu_1,mut,muts,muu,muus,muv,muvs,u_save,v_save,&
!&w_save,t_save,ph_save,mu_save,msfu,msfv,msft,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
!&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
!-----The original
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
BENCH_END(comp36)

BENCH_START(exch32)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_1592.inc"
! HALO_1592 dyn_em 8:ru_m,rv_m
#endif
BENCH_END(exch32)

! recompute : mu_2,ph_2,t_2

BENCH_START(comp37)
  moist_scalar_advanch: if (num_3d_m .ge. param_first_scalar) then
    moist_variable_loos: do im = param_first_scalar, num_3d_m

        call rk_scalar_tend( im,im,config_flags,rk_step,dt_rk,ru_m,rv_m,ww_m,mut,alt,moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,&
&im),moist_tend(ims,kms,jms,im),advect_tend,rqvften,qv_base, .true. ,fnm,fnp,msfu,msfv,msft,rdx,rdy,rdn,rdnw,khdif,kvdif,&
&xkmhd,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%&
&j_end(ij),k_start,k_end )
        if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
          if (im .eq. p_qv) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqv_b,rqv_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
        if (config_flags%nested .and. rk_step .eq. 1) then
          if (im .eq. p_qc) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqc_b,rqc_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qr) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqr_b,rqr_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qi) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqi_b,rqi_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qs) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqs_b,rqs_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qg) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqg_b,rqg_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif

        call rk_update_scalar( im,im,moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,im),moist_tend(ims,kms,jms,im),advect_tend,msft,&
&mu_1,mu_2,mub,rk_step,dt_rk,spec_zone,epsts,leapfrog,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    end do moist_variable_loos
  endif moist_scalar_advanch
BENCH_END(comp37)

! recompute : moist_2

BENCH_START(comp37_calc_p_rho_phi)
    call calc_p_rho_phi( moist_2,num_3d_m,al,alb,mu_2,muts,ph_2,p,pb,t_2,p0,t0,znu,dnw,rdnw,rdn,non_hydrostatic,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp37_calc_p_rho_phi)

BENCH_START(comp37_a_diagnose_w)
    if ( .not. non_hydrostatic) then
      call a_diagnose_w( ph_tend,a_ph_tend,a_ph_2,a_ph_1,a_w_2,muts,a_muts,dt_rk,a_u_2,a_v_2,ht,cf1,cf2,cf3,rdx,rdy,msft,ide,jde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_end )
    endif
BENCH_END(comp37_a_diagnose_w)

BENCH_START(comp37_a_calc_p_rho_phi)
    call a_calc_p_rho_phi( moist_2,a_moist_2,num_3d_m,al,a_al,alb,mu_2,a_mu_2,muts,a_muts,ph_2,a_ph_2,p,a_p,pb,t_2,a_t_2,p0,t0,dnw,&
&rdnw,rdn,non_hydrostatic,ide,jde,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),&
&k_start,k_end )
BENCH_END(comp37_a_calc_p_rho_phi)

BENCH_START(exch33)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1654.inc"
! HALO_1654 dyn_em 48:a_al,a_moist_2,a_mu_2,a_p,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,al,alt,mut,muu,muv,p,php,ru,rv,rw,ww





#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1664.inc"
! HALO_1664 dyn_em 48:al,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
#endif
BENCH_END(exch33)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 37
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

BENCH_START(comp38)
#ifndef FAST_CPY
  ww_m(:,:,:) = ww_m_keep3(:,:,:)
  rv_m(:,:,:) = rv_m_keep3(:,:,:)
  ru_m(:,:,:) = ru_m_keep3(:,:,:)
  a(:,:,:) = a_keep3(:,:,:)
  alpha(:,:,:) = alpha_keep3(:,:,:)
  gamma(:,:,:) = gamma_keep3(:,:,:)
!------------------------------

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
!----------------------------------------
  mudf(:,:) = mudf_ma(:,:)
!----------------------------------------
#else
 call fast_copy_lint(  ww_m , ww_m_keep3 , n3d_elem ) 
 call fast_copy_lint(  rv_m , rv_m_keep3 , n3d_elem ) 
 call fast_copy_lint(  ru_m , ru_m_keep3 , n3d_elem ) 
 call fast_copy_lint(  a , a_keep3 , n3d_elem ) 
 call fast_copy_lint(  alpha , alpha_keep3 , n3d_elem ) 
 call fast_copy_lint(  gamma , gamma_keep3 , n3d_elem ) 
!------------------------------

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
 call fast_copy_lint(  moist_tend(ims,kms,jms,2) , moist_tendh(ims,kms,jms,2) , n3d_elem * (num_moist-1) ) 
 call fast_copy_lint(  moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2) , n3d_elem * (num_moist-1) ) 
 call fast_copy_lint(  moist_1(ims,kms,jms,2) , moist_1h(ims,kms,jms,2) , n3d_elem * (num_moist-1) ) 
 call fast_copy_lint(  dnw , dnwh , kme-kms+1 ) 
 call fast_copy_lint(  al , alh , n3d_elem ) 
!----------------------------------------
 call fast_copy_lint(  mudf , mudf_ma , n2d_elem ) 
!----------------------------------------
#endif

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
BENCH_END(comp38)
! recompute : dt_rk,dts_rk,number_of_small_timesteps

BENCH_START(exch34)
#ifdef DM_PARALLEL
lineno = __LINE__
#   include "HALO_1748.inc"
! HALO_1748 dyn_em 48:al,p,ww
#endif
BENCH_END(exch34)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepzw: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepzw:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepzw

BENCH_START(comp39)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp39)

 endif restore_rk_step_prepzw

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1757.inc"
! HALO_1757 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

BENCH_START(exch35)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_1763.inc"
! HALO_1763 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch35)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww
  rk_step_is_onzg: if (rk_step .eq. 1) then

BENCH_START(comp40)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp40)

BENCH_START(exch36)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch36)

BENCH_START(comp41)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp41)

BENCH_START(exch37)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch37)

    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

BENCH_START(comp42)
        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp42)

    endif

  endif rk_step_is_onzg

BENCH_START(exch38)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1809.inc"
! HALO_1809 dyn_em 8:moist_2



#endif
BENCH_END(exch38)

!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier between the call to rk_tendency and 
! TBH:  the end of small_stepu loop.  
 restore_rktend_thru_sszw: if ( config_flags%remove_rktend_thru_ss ) then
BENCH_START(comprestore_rktend_thru_ss_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktend_thru_sszw:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND_THRU_SS)
#include "restore_rktend_thru_ss.h"
BENCH_END(comprestore_rktend_thru_ss_tim)
 else  ! restore_rktend_thru_sszw

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendzw: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendzw:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendzw

WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_small_stepzw, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )
BENCH_START(comp7774)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp7774)

 endif restore_rktendzw

BENCH_START(comp43)
!  recompute : cqw,mu_tend,ph_save,ph_tend,ru_tend,ru_tendf,rv_tend,rv_t
! endf,rw_tend,rw_tendf,t_save,t_tend,t_tendf,u_save,v_save,w_save

    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (config_flags%specified .or. config_flags%nested) then
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif

! recompute : mu_tend,ph_tend,ru_tend,rv_tend,rw_tend,t_tend

    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (non_hydrostatic) then
      call calc_coef_w( a,alpha,gamma,mut,cqw,rdn,rdnw,c2a,dts,g,epssm,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp43)

BENCH_START(exch39)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_1857.inc"
! HALO_1857 dyn_em 8:al,alt,mu_2,mudf,muts,p,ph_2,php

#endif
BENCH_END(exch39)

!------------------------------
!------------------------------

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by small_stepu loop.  
 restore_small_stepzw: if (config_flags%remove_small_step) then
BENCH_START(comprestore_small_step_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_small_stepzw:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_SMALL_STEP)
#include "restore_small_step.h"
BENCH_END(comprestore_small_step_tim)
 else  ! restore_small_stepzw

  small_stepzw: do iteration = 1, number_of_small_timesteps
WRITE(wrf_err_message,*) 'DEBUG:  begin small_stepzw, iteration = ',iteration
CALL wrf_debug( 10, TRIM(wrf_err_message) )

BENCH_START(comp44)
      call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
BENCH_END(comp44)

BENCH_START(exch40)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_1880.inc"
! HALO_1880 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch40)
BENCH_START(comp444)
      call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp444)
BENCH_START(exch41)
#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_1889.inc"
! HALO_1889 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww




#endif
BENCH_END(exch41)
BENCH_START(comp45)
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
      endif
      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (non_hydrostatic) then
        call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,alb,&
&a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then
          call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          if (config_flags%specified) then
            call zero_grad_bdy( w_2,'w',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,&
&kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else
            call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
      endif
      call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,iteration,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp45)

BENCH_START(exch42)
#ifdef DM_PARALLEL
lineno = __LINE__
#   include "HALO_1930.inc"
! HALO_1930 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,p,pb,ph_2,php,t_1

#endif
BENCH_END(exch42)

  end do small_stepzw

 endif restore_small_stepzw

 endif restore_rktend_thru_sszw

! recompute : mu_2,mudf,muts,ph_2,pm1,ru_m,rv_m,t_2,u_2,v_2,w_2,ww_m

BENCH_START(exch43)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch43)

BENCH_START(comp46)
    call calc_mu_uv_1( config_flags,muts,muus,muvs,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
!    call small_step_finish( u_2,u_1,v_2,v_1,w_2,w_1,t_2,t_1,ph_2,ph_1,ww,ww1,mu_2,mu_1,mut,muts,muu,muus,muv,muvs,u_save,v_save,&
!&w_save,t_save,ph_save,mu_save,msfu,msfv,msft,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
!&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
!-----The original
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
BENCH_END(comp46)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 46
#include <all_debug_halo.h>
#endif

! recompute : mu_2,ph_2,t_2
!---------------------------------------------------------------------------------

  a_moist_scalar_advance: if (num_3d_m .ge. param_first_scalar) then

BENCH_START(exch44)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_1974.inc"
! HALO_1974 dyn_em 48:al,mu_1,mu_2,p,ph_2,ru_m,rv_m,t_1,t_2,u_2,v_2,w_2
#endif
BENCH_END(exch44)
BENCH_START(comp47)
!----------------------------------------
!1111
!saved by Zaizhong Ma
!----------------------------------------
#ifndef FAST_CPY
u_1_keep4(:,:,:) = u_1(:,:,:)
u_2_keep4(:,:,:) = u_2(:,:,:)
v_1_keep4(:,:,:) = v_1(:,:,:)
v_2_keep4(:,:,:) = v_2(:,:,:)
w_1_keep4(:,:,:) = w_1(:,:,:)
w_2_keep4(:,:,:) = w_2(:,:,:)
ph_1_keep4(:,:,:) = ph_1(:,:,:)
ph_2_keep4(:,:,:) = ph_2(:,:,:)
t_1_keep4(:,:,:) = t_1(:,:,:)
t_2_keep4(:,:,:) = t_2(:,:,:)
mu_1_keep4(:,:) = mu_1(:,:)
mu_2_keep4(:,:) = mu_2(:,:)
p_keep4(:,:,:) = p(:,:,:)
al_keep4(:,:,:) = al(:,:,:)
moist_1_keep4(:,:,:,:) = moist_1(:,:,:,:)
moist_2_keep4(:,:,:,:) = moist_2(:,:,:,:)
!----------------------------------------
rv_mz(:,:,:) = rv_m(:,:,:)
ru_tendfz(:,:,:) = ru_tendf(:,:,:)
ru_mz(:,:,:) = ru_m(:,:,:)
moist_tendz(:,:,:,:) = moist_tend(:,:,:,:)
#else
 call fast_copy_lint(u_1_keep4 , u_1 , n3d_elem )
 call fast_copy_lint(u_2_keep4 , u_2 , n3d_elem )
 call fast_copy_lint(v_1_keep4 , v_1 , n3d_elem )
 call fast_copy_lint(v_2_keep4 , v_2 , n3d_elem )
 call fast_copy_lint(w_1_keep4 , w_1 , n3d_elem )
 call fast_copy_lint(w_2_keep4 , w_2 , n3d_elem )
 call fast_copy_lint(ph_1_keep4 , ph_1 , n3d_elem )
 call fast_copy_lint(ph_2_keep4 , ph_2 , n3d_elem )
 call fast_copy_lint(t_1_keep4 , t_1 , n3d_elem )
 call fast_copy_lint(t_2_keep4 , t_2 , n3d_elem )
 call fast_copy_lint(mu_1_keep4 , mu_1 , n2d_elem )
 call fast_copy_lint(mu_2_keep4 , mu_2 , n2d_elem )
 call fast_copy_lint(p_keep4 , p , n3d_elem )
 call fast_copy_lint(al_keep4 , al , n3d_elem )
 call fast_copy_lint(moist_1_keep4(ims,kms,jms,2) , moist_1(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
 call fast_copy_lint(moist_2_keep4(ims,kms,jms,2) , moist_2(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
!----------------------------------------
 call fast_copy_lint(rv_mz , rv_m , n3d_elem )
 call fast_copy_lint(ru_tendfz , ru_tendf , n3d_elem )
 call fast_copy_lint(ru_mz , ru_m , n3d_elem )
 call fast_copy_lint(moist_tendz(ims,kms,jms,2) , moist_tend(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
#endif
!----------------------------------------
BENCH_END(comp47)

    do im = num_3d_m, param_first_scalar, -1
BENCH_START(comp48)
!----------------------------------------
!  1111
!recovered by Zaizhong Ma
!----------------------------------------
#ifndef FAST_CPY
u_1(:,:,:) = u_1_keep4(:,:,:)
u_2(:,:,:) = u_2_keep4(:,:,:)
v_1(:,:,:) = v_1_keep4(:,:,:)
v_2(:,:,:) = v_2_keep4(:,:,:)
w_1(:,:,:) = w_1_keep4(:,:,:)
w_2(:,:,:) = w_2_keep4(:,:,:)
ph_1(:,:,:) = ph_1_keep4(:,:,:)
ph_2(:,:,:) = ph_2_keep4(:,:,:)
t_1(:,:,:) = t_1_keep4(:,:,:)
t_2(:,:,:) = t_2_keep4(:,:,:)
mu_1(:,:) = mu_1_keep4(:,:)
mu_2(:,:) = mu_2_keep4(:,:)
p(:,:,:) = p_keep4(:,:,:)
al(:,:,:) = al_keep4(:,:,:)
moist_1(:,:,:,:) = moist_1_keep4(:,:,:,:)
moist_2(:,:,:,:) = moist_2_keep4(:,:,:,:)
!----------------------------------------
!----------------------------------------
rv_m(:,:,:) = rv_mz(:,:,:)
ru_tendf(:,:,:) = ru_tendfz(:,:,:)
ru_m(:,:,:) = ru_mz(:,:,:)
moist_tend(:,:,:,:) = moist_tendz(:,:,:,:)
!----------------------------------------
#else
  call fast_copy_lint(u_1 , u_1_keep4 , n3d_elem )
  call fast_copy_lint(u_2 , u_2_keep4 , n3d_elem )
  call fast_copy_lint(v_1 , v_1_keep4 , n3d_elem )
  call fast_copy_lint(v_2 , v_2_keep4 , n3d_elem )
  call fast_copy_lint(w_1 , w_1_keep4 , n3d_elem )
  call fast_copy_lint(w_2 , w_2_keep4 , n3d_elem )
  call fast_copy_lint(ph_1 , ph_1_keep4 , n3d_elem )
  call fast_copy_lint(ph_2 , ph_2_keep4 , n3d_elem )
  call fast_copy_lint(t_1 , t_1_keep4 , n3d_elem )
  call fast_copy_lint(t_2 , t_2_keep4 , n3d_elem )
  call fast_copy_lint(mu_1 , mu_1_keep4 , n2d_elem )
  call fast_copy_lint(mu_2 , mu_2_keep4 , n2d_elem )
  call fast_copy_lint(p , p_keep4 , n3d_elem )
  call fast_copy_lint(al , al_keep4 , n3d_elem )
  call fast_copy_lint(moist_1(ims,kms,jms,2) , moist_1_keep4(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
  call fast_copy_lint(moist_2(ims,kms,jms,2) , moist_2_keep4(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
!----------------------------------------
!----------------------------------------
  call fast_copy_lint(rv_m , rv_mz , n3d_elem )
  call fast_copy_lint(ru_tendf , ru_tendfz , n3d_elem )
  call fast_copy_lint(ru_m , ru_mz , n3d_elem )
  call fast_copy_lint(moist_tend(ims,kms,jms,2) , moist_tendz(ims,kms,jms,2) , n3d_elem*(num_moist-1) )
!----------------------------------------
#endif
BENCH_END(comp48)

BENCH_START(exch45)
#ifdef DM_PARALLEL
lineno = __LINE__
#   include "HALO_2036.inc"
! HALO_2036 dyn_em 8:ru_m,rv_m;48:a_mu_2,a_mudf,a_muts,a_ph_2,a_t_2,al,alt,cqu,cqv,mu_2,muts,muus,muvs,p,pb,ph_2,php,t_2,t_save,u_2,u_save,v_2,v_save,w_2,w_save

#endif
BENCH_END(exch45)

BENCH_START(comp49)
     moist_variable_loor2:  do iy = param_first_scalar, num_3d_m - 1

        call rk_scalar_tend( iy,iy,config_flags,rk_step,dt_rk,ru_m,rv_m,ww_m,mut,alt,moist_1(ims,kms,jms,iy),moist_2(ims,kms,jms,&
&iy),moist_tend(ims,kms,jms,iy),advect_tend,rqvften,qv_base, .true. ,fnm,fnp,msfu,msfv,msft,rdx,rdy,rdn,rdnw,khdif,kvdif,&
&xkmhd,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%&
&j_end(ij),k_start,k_end )
        if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
          if (iy .eq. p_qv) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqv_b,rqv_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
        if (config_flags%nested .and. rk_step .eq. 1) then
          if (iy .eq. p_qc) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqc_b,rqc_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (iy .eq. p_qr) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqr_b,rqr_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (iy .eq. p_qi) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqi_b,rqi_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (iy .eq. p_qs) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqs_b,rqs_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (iy .eq. p_qg) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,iy),rqg_b,rqg_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif

        call rk_update_scalar( iy,iy,moist_1(ims,kms,jms,iy),moist_2(ims,kms,jms,iy),moist_tend(ims,kms,jms,iy),advect_tend,msft,&
&mu_1,mu_2,mub,rk_step,dt_rk,spec_zone,epsts,leapfrog,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    end do moist_variable_loor2

        call rk_scalar_tend( im,im,config_flags,rk_step,dt_rk,ru_m,rv_m,ww_m,mut,alt,moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,&
&im),moist_tend(ims,kms,jms,im),advect_tend,rqvften,qv_base, .true. ,fnm,fnp,msfu,msfv,msft,rdx,rdy,rdn,rdnw,khdif,kvdif,&
&xkmhd,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%&
&j_end(ij),k_start,k_end )
        if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
          if (im .eq. p_qv) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqv_b,rqv_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
        if (config_flags%nested .and. rk_step .eq. 1) then
          if (im .eq. p_qc) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqc_b,rqc_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qr) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqr_b,rqr_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qi) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqi_b,rqi_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qs) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqs_b,rqs_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qg) then
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqg_b,rqg_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
BENCH_END(comp49)

!-------------------------------------------------------

BENCH_START(exch46)
#ifdef DM_PARALLEL
lineno = __LINE__
#   include "HALO_2112.inc"
! HALO_2112 dyn_em 48:a_muts,muts,muus,muvs
#endif
BENCH_END(exch46)

! recompute : advect_tend,moist_tend

BENCH_START(comp50)
        call a_rk_update_scalar( im,im,moist_1(ims,kms,jms,im),a_moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,im),a_moist_2(ims,kms,&
&jms,im),moist_tend(ims,kms,jms,im),a_moist_tend(ims,kms,jms,im),advect_tend,a_advect_tend,msft,mu_1,a_mu_1,mu_2,a_mu_2,&
&mub,rk_step,dt_rk,spec_zone,epsts,leapfrog,config_flags,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )

        if (config_flags%nested .and. rk_step .eq. 1) then
          if (im .eq. p_qc) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqc_bth,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qr) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqr_bth,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qi) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqi_bth,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qs) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqs_bth,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qg) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqg_bth,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
        if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
          if (im .eq. p_qv) then
            call a_spec_bdy_scalar( a_moist_tend(ims,kms,jms,im),a_rqv_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
BENCH_END(comp50)

BENCH_START(exch47)
#ifdef DM_PARALLEL
lineno = __LINE__
# include "HALO_2154.inc"
! HALO_2154 dyn_em 80:a_advect_tend,a_alt,a_moist_1,a_moist_2,a_moist_tend,alt,moist_1,moist_2,msft,msfu,msfv,mut,ru_m,rv_m,ww_m
#endif
BENCH_END(exch47)

BENCH_START(comp51)
        call a_rk_scalar_tend( im,im,config_flags,rk_step,ru_m,a_ru_m,rv_m,a_rv_m,ww_m,a_ww_m,mut,a_mut,alt,a_alt,moist_1(ims,kms,&
&jms,im),a_moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,im),a_moist_2(ims,kms,jms,im),a_moist_tend(ims,kms,jms,im),&
&a_advect_tend,qv_base, .true. ,fnm,fnp,msfu,msfv,msft,rdx,rdy,rdn,rdnw,kvdif,xkmhd,a_xkmhd,leapfrog,ids,ide,jds,jde,kde,&
&ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-3,ids),  &
        min(grid%i_end(ij)+3,ide),    &
        max(grid%j_start(ij)-3,jds),  &
        min(grid%j_end(ij)+3,jde),k_start,k_end )
BENCH_END(comp51)

    end do
  endif a_moist_scalar_advance

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 51
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

BENCH_START(comp52)
#ifndef FAST_CPY
!------------------------------
  ww_m(:,:,:) = ww_m_keep3(:,:,:)
  rv_m(:,:,:) = rv_m_keep3(:,:,:)
  ru_m(:,:,:) = ru_m_keep3(:,:,:)
  a(:,:,:) = a_keep3(:,:,:)
  alpha(:,:,:) = alpha_keep3(:,:,:)
  gamma(:,:,:) = gamma_keep3(:,:,:)
!------------------------------

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
!----------------------------------------
  mudf(:,:) = mudf_ma(:,:)
!----------------------------------------
#else
!------------------------------
 call fast_copy_lint(  ww_m , ww_m_keep3, n3d_elem )
 call fast_copy_lint(  rv_m , rv_m_keep3, n3d_elem )
 call fast_copy_lint(  ru_m , ru_m_keep3, n3d_elem )
 call fast_copy_lint(  a , a_keep3, n3d_elem )
 call fast_copy_lint(  alpha , alpha_keep3, n3d_elem )
 call fast_copy_lint(  gamma , gamma_keep3, n3d_elem )
!------------------------------

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
 call fast_copy_lint(  dnw , dnwh , kme-kms+1 )
 call fast_copy_lint(  al , alh, n3d_elem )
!----------------------------------------
 call fast_copy_lint(  mudf , mudf_ma, n2d_elem )
!----------------------------------------
#endif
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
BENCH_END(comp52)

BENCH_START(exch48)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_2249.inc"
! HALO_2249 dyn_em 48:al,alt,mut,p,ww
#endif
BENCH_END(exch48)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepzz: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepzz:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepzz

BENCH_START(comp53)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp53)

 endif restore_rk_step_prepzz

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2258.inc"
! HALO_2258 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

BENCH_START(exch49)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2264.inc"
! HALO_2264 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch49)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww
  rk_step_is_onzz: if (rk_step .eq. 1) then

BENCH_START(comp54)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp54)

BENCH_START(exch50)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch50)

BENCH_START(comp55)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp55)

BENCH_START(exch51)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch51)

BENCH_START(comp56)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    endif
BENCH_END(comp56)
  endif rk_step_is_onzz

BENCH_START(exch52)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2309.inc"
! HALO_2309 dyn_em 8:moist_2



#endif
BENCH_END(exch52)

!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier between the call to rk_tendency and 
! TBH:  the end of small_stepu loop.  
 restore_rktend_thru_sszz: if ( config_flags%remove_rktend_thru_ss ) then
BENCH_START(comprestore_rktend_thru_ss_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktend_thru_sszz:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND_THRU_SS)
#include "restore_rktend_thru_ss.h"
BENCH_END(comprestore_rktend_thru_ss_tim)
 else  ! restore_rktend_thru_sszz

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendzz: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendzz:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendzz

WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_small_stepzz, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )
BENCH_START(comp7775)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp7775)

 endif restore_rktendzz

BENCH_START(comp57)
!  recompute : cqw,mu_tend,ph_save,ph_tend,ru_tend,ru_tendf,rv_tend,rv_t
! endf,rw_tend,rw_tendf,t_save,t_tend,t_tendf,u_save,v_save,w_save

    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (config_flags%specified .or. config_flags%nested) then
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif

! recompute : mu_tend,ph_tend,ru_tend,rv_tend,rw_tend,t_tend

    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (non_hydrostatic) then
      call calc_coef_w( a,alpha,gamma,mut,cqw,rdn,rdnw,c2a,dts,g,epssm,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp57)

BENCH_START(exch53)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2358.inc"
! HALO_2358 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save

#endif
BENCH_END(exch53)

!  recompute : a,al,alpha,c2a,gamma,mu_1,mu_2,mu_save,mudf,p,ph_1,ph_2,p
! h_save,pm1,t_1,t_2,t_save,u_1,u_2,u_save,v_1,v_2,v_save,w_1,w_2,w_save
! ,ww1

!------------------------------
!------------------------------

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by small_stepu loop.  
 restore_small_stepzz: if (config_flags%remove_small_step) then
BENCH_START(comprestore_small_step_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_small_stepzz:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_SMALL_STEP)
#include "restore_small_step.h"
BENCH_END(comprestore_small_step_tim)
 else  ! restore_small_stepzz

  small_stepzz: do iteration = 1, number_of_small_timesteps
WRITE(wrf_err_message,*) 'DEBUG:  begin small_stepzz, iteration = ',iteration
CALL wrf_debug( 10, TRIM(wrf_err_message) )

BENCH_START(comp58)
      call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
BENCH_END(comp58)

BENCH_START(exch54)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2386.inc"
! HALO_2386 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch54)

BENCH_START(comp59)
      call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp59)

BENCH_START(exch55)
#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_2397.inc"
! HALO_2397 dyn_em 48:al,alt,moist_1,moist_2,msft,msfu,msfv,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww




#endif
BENCH_END(exch55)

BENCH_START(comp60)
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
      endif

      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

      if (non_hydrostatic) then
        call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,alb,&
&a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then
          call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          if (config_flags%specified) then
            call zero_grad_bdy( w_2,'w',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,&
&kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else
            call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
      endif
      call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,iteration,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp60)

BENCH_START(exch56)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2441.inc"
! HALO_2441 dyn_em 8:mu_1,mudf,ru_m,rv_m,t_1;48:a_mu_2,al,cqu,cqv,mu_2,muts,muus,muvs,p,pb,ph_2,php,t_2,t_save,u_2,u_save,v_2,v_save,w_2,w_save

#endif
BENCH_END(exch56)

  end do small_stepzz

 endif restore_small_stepzz

 endif restore_rktend_thru_sszz

!------------------------------------------------------

BENCH_START(comp61)
    call calc_mu_uv_1( config_flags,muts,muus,muvs,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp61)

BENCH_START(exch57)
#ifdef DM_PARALLEL


#endif
BENCH_END(exch57)

! recompute : muus,muvs

BENCH_START(comp62)
    call a_small_step_finish( u_2,a_u_2,v_2,a_v_2,w_2,a_w_2,t_2,a_t_2,a_ph_2,a_mu_2,mut,a_mut,muts,a_muts,muu,a_muu,muus,a_muus,&
&muv,a_muv,muvs,a_muvs,u_save,a_u_save,v_save,a_v_save,w_save,a_w_save,t_save,a_t_save,a_ph_save,a_mu_save,msfu,msfv,msft,ide,&
&jde,kds,kde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde) )
BENCH_END(comp62)


BENCH_START(exch58)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_2474.inc"
! HALO_2474 dyn_em 48:a_muts,a_muus,a_muvs
#endif
BENCH_END(exch58)

BENCH_START(comp63)
    call a_calc_mu_uv_1( config_flags,a_muts,a_muus,a_muvs,ids,ide,jds,jde,ims,ime,jms,jme, &
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde) )
BENCH_END(comp63)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 63
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

  a_small_steps: do iteration = number_of_small_timesteps, 1, -1

BENCH_START(comp64)
#ifndef FAST_CPY
!------------------------------
  ww_m(:,:,:) = ww_m_keep3(:,:,:)
  rv_m(:,:,:) = rv_m_keep3(:,:,:)
  ru_m(:,:,:) = ru_m_keep3(:,:,:)
  a(:,:,:) = a_keep3(:,:,:)
  alpha(:,:,:) = alpha_keep3(:,:,:)
  gamma(:,:,:) = gamma_keep3(:,:,:)
!------------------------------

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

!------------------------------
  call fast_copy_lint( ww_m , ww_m_keep3 , n3d_elem )
  call fast_copy_lint( rv_m , rv_m_keep3 , n3d_elem )
  call fast_copy_lint( ru_m , ru_m_keep3 , n3d_elem )
  call fast_copy_lint( a , a_keep3 , n3d_elem )
  call fast_copy_lint( alpha , alpha_keep3 , n3d_elem )
  call fast_copy_lint( gamma , gamma_keep3 , n3d_elem )
!------------------------------

  call fast_copy_lint( xkmhd , xkmhdh , n3d_elem )
  call fast_copy_lint( w_2 , w_2h , n3d_elem )
  call fast_copy_lint( w_1 , w_1h , n3d_elem )
  call fast_copy_lint( v_2 , v_2h , n3d_elem )
  call fast_copy_lint( v_1 , v_1h , n3d_elem )
  call fast_copy_lint( u_2 , u_2h , n3d_elem )
  call fast_copy_lint( u_1 , u_1h , n3d_elem )
  call fast_copy_lint( t_tendf , t_tendfh , n3d_elem )
  call fast_copy_lint( t_2 , t_2h , n3d_elem )
  call fast_copy_lint( t_1 , t_1h , n3d_elem )
  call fast_copy_lint( rw_tendf , rw_tendfh , n3d_elem )
  call fast_copy_lint( rv_tendf , rv_tendfh , n3d_elem )
  call fast_copy_lint( ru_tendf , ru_tendfh , n3d_elem )
  call fast_copy_lint( pm1 , pm1h , n3d_elem )
  call fast_copy_lint( ph_tendf , ph_tendfh , n3d_elem )
  call fast_copy_lint( ph_2 , ph_2h , n3d_elem )
  call fast_copy_lint( ph_1 , ph_1h , n3d_elem )
  call fast_copy_lint( p , ph , n3d_elem )
  call fast_copy_lint( mu_2 , mu_2h , n2d_elem )
  call fast_copy_lint( mu_1 , mu_1h , n2d_elem )
  call fast_copy_lint( moist_tend(ims,kms,jms,2) , moist_tendh(ims,kms,jms,2) , n3d_elem * (num_moist - 1) )
  call fast_copy_lint( moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2) , n3d_elem  * (num_moist - 1) )
  call fast_copy_lint( moist_1(ims,kms,jms,2) , moist_1h(ims,kms,jms,2) , n3d_elem  * (num_moist - 1) )
  call fast_copy_lint( dnw , dnwh, kme-kms+1 )
  call fast_copy_lint( al , alh , n3d_elem )

#endif
BENCH_END(comp64)

BENCH_START(exch59)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2527.inc"
! HALO_2527 dyn_em 48:a_moist_1,a_moist_2,a_moist_tend,a_mu_1,a_mu_2,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,al,alt,mut,muu,muv,p,php,ru,rv,rw,ww





#endif
BENCH_END(exch59)

BENCH_START(comp65)
!----------------------------------------
  mudf(:,:) = mudf_ma(:,:)
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
BENCH_END(comp65)

BENCH_START(exch60)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch60)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepzr: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepzr:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepzr

BENCH_START(comp66)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp66)

 endif restore_rk_step_prepzr

BENCH_START(exch61)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2588.inc"
! HALO_2588 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2593.inc"
! HALO_2593 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch61)

  rk_step_is_onzd: if (rk_step .eq. 1) then

BENCH_START(comp67)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp67)

BENCH_START(exch62)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2605.inc"
! HALO_2605 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch62)

BENCH_START(comp68)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp68)

    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

BENCH_START(exch63)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch63)
BENCH_START(comp69)
        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp69)

    endif
  endif rk_step_is_onzd

BENCH_START(exch64)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2636.inc"
! HALO_2636 dyn_em 8:moist_2



#endif
BENCH_END(exch64)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendzr: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendzr:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendzr

WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_small_stepzr, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )
BENCH_START(comp7776)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp7776)

 endif restore_rktendzr

BENCH_START(comp70_relax_bdy_dry)
    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp70_relax_bdy_dry)
BENCH_START(comp70_rk_addtend_dry)
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp70_rk_addtend_dry)
BENCH_START(comp70_spec_bdy_dry)
    if (config_flags%specified .or. config_flags%nested) then
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp70_spec_bdy_dry)

BENCH_START(comp70_small_step_prep)
    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp70_small_step_prep)
BENCH_START(comp70_calc_p_rho)
    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp70_calc_p_rho)
BENCH_START(comp70_calc_coef_w)
    if (non_hydrostatic) then
      call calc_coef_w( a,alpha,gamma,mut,cqw,rdn,rdnw,c2a,dts,g,epssm,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp70_calc_coef_w)

BENCH_START(exch65)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2676.inc"
! HALO_2676 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save

#endif
BENCH_END(exch65)

    small_stepzr: do iteration1 = 1, iteration-1
WRITE(wrf_err_message,*) 'DEBUG:  begin small_stepzr, iteration1 = ',iteration1
CALL wrf_debug( 10, TRIM(wrf_err_message) )

BENCH_START(comp71)
        call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        if (config_flags%specified .or. config_flags%nested) then
          call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        endif
BENCH_END(comp71)

BENCH_START(exch66)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2696.inc"
! HALO_2696 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch66)

BENCH_START(comp72)
        call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration1,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        if (config_flags%specified .or. config_flags%nested) then
          call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
          call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        endif
BENCH_END(comp72)

BENCH_START(exch67)
#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_2715.inc"
! HALO_2715 dyn_em 48:al,alt,moist_1,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww




#endif
BENCH_END(exch67)

BENCH_START(comp73)
      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration1,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

        if (non_hydrostatic) then
          call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,&
&alb,a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        endif

        if (config_flags%specified .or. config_flags%nested) then
          if (non_hydrostatic) then
            call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
            if (config_flags%specified) then

               CALL zero_grad_bdy ( w_2,                        &
                                    'w'         , config_flags, &
                                    spec_zone,                  &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),         &
                                    grid%j_start(ij), grid%j_end(ij),         &
                                    k_start    , k_end             )

            else
              call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,&
&ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
            endif
          endif
        endif

        call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,iteration1,ids,ide,jds,&
&jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp73)


BENCH_START(exch68)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2762.inc"
! HALO_2762 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1

#endif
BENCH_END(exch68)

    end do small_stepzr

!----------------------------------------
BENCH_START(exch69)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2773.inc"
! HALO_2773 dyn_em 48:al,mu_2,p,ph_2
#endif
BENCH_END(exch69)
BENCH_START(comp74)
#ifndef FAST_CPY
alq(:,:,:) = al(:,:,:)
mu_2q(:,:) = mu_2(:,:)
pq(:,:,:) = p(:,:,:)
ph_2q(:,:,:) = ph_2(:,:,:)
#else
 call fast_copy_lint ( alq , al , n3d_elem ) 
 call fast_copy_lint ( mu_2q , mu_2 , n2d_elem ) 
 call fast_copy_lint ( pq , p , n3d_elem ) 
 call fast_copy_lint ( ph_2q , ph_2 , n3d_elem ) 
#endif
BENCH_END(comp74)

BENCH_START(exch70)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_2784.inc"
! HALO_2784 dyn_em 8:al,mu_2,mudf,p,ph_2
#endif
BENCH_END(exch70)

BENCH_START(comp75)
      call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
BENCH_END(comp75)

!----------------------------------------
BENCH_START(exch71)
#ifdef DM_PARALLEL
lineno = __LINE__
#       include "HALO_2802.inc"
! HALO_2802 dyn_em 48:u_2,v_2,ww
#endif
BENCH_END(exch71)
BENCH_START(comp76)
#ifndef FAST_CPY
wwr(:,:,:) = ww(:,:,:)
u_2r(:,:,:) = u_2(:,:,:)
v_2r(:,:,:) = v_2(:,:,:)
#else
  call fast_copy_lint ( wwr , ww , n3d_elem )
  call fast_copy_lint ( u_2r , u_2 , n3d_elem )
  call fast_copy_lint ( v_2r , v_2 , n3d_elem )
#endif
BENCH_END(comp76)

BENCH_START(exch71b)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_2810.inc"
! HALO_2810 dyn_em 8:u_2,v_2
#endif
BENCH_END(exch71b)

BENCH_START(comp77)
      call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp77)

BENCH_START(exch72)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2821.inc"
! HALO_2821 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww




#endif
BENCH_END(exch72)

BENCH_START(comp78)
      if (config_flags%specified .or. config_flags%nested) then
        call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
      endif

      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp78)

BENCH_START(exch73)
#ifdef DM_PARALLEL
lineno = __LINE__
#       include "HALO_2844.inc"
! HALO_2844 dyn_em 48:a,mu_2,ph_2,t_2,t_2save,u_2,v_2,w_2,ww
#endif
BENCH_END(exch73)
!----------------------------------------
#ifndef FAST_CPY
w_2s(:,:,:) = w_2(:,:,:)
wws(:,:,:) = ww(:,:,:)
u_2s(:,:,:) = u_2(:,:,:)
v_2s(:,:,:) = v_2(:,:,:)
mu_2s(:,:) = mu_2(:,:)
t_2s(:,:,:) = t_2(:,:,:)
ph_2s(:,:,:) = ph_2(:,:,:)
as(:,:,:) = a(:,:,:)
t_2saves(:,:,:) = t_2save(:,:,:) 
#else
  call fast_copy_lint( w_2s , w_2 , n3d_elem )
  call fast_copy_lint( wws , ww , n3d_elem )
  call fast_copy_lint( u_2s , u_2 , n3d_elem )
  call fast_copy_lint( v_2s , v_2 , n3d_elem )
  call fast_copy_lint( mu_2s , mu_2 , n2d_elem )
  call fast_copy_lint( t_2s , t_2 , n3d_elem )
  call fast_copy_lint( ph_2s , ph_2 , n3d_elem )
  call fast_copy_lint( as , a , n3d_elem )
  call fast_copy_lint( t_2saves , t_2save , n3d_elem )
#endif

BENCH_START(comp79)
      if (non_hydrostatic) then
        call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,alb,&
&a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then

          call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          if (config_flags%specified) then
            call zero_grad_bdy( w_2,'w',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,&
&kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else
            call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,&
&ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
      endif
BENCH_END(comp79)

BENCH_START(exch74)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2881.inc"
! HALO_2881 dyn_em 48:a_moist_2,a_mu_1,a_t_2,a_u_2,a_v_2,a_w_2,al








#endif
BENCH_END(exch74)

BENCH_START(comp80)
      call a_calc_p_rho( al,a_al,p,a_p,ph_2,a_ph_2,alt,a_alt,t_2,a_t_2,t_save,a_t_save,c2a,a_c2a,a_pm1,mu_2,a_mu_2,muts,a_muts,znu,&
&t0,rdnw,dnw,smdiv,non_hydrostatic,iteration,ide,jde,kde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp80)

BENCH_START(exch75)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_2903.inc"
! HALO_2903 dyn_em 48:al








#endif
BENCH_END(exch75)

BENCH_START(comp81)
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then
          if (config_flags%specified) then
            call a_zero_grad_bdy( a_w_2,'w',spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start)
          else
            call a_spec_bdyupdate( a_w_2,a_rw_tend,dts_rk,'h',spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
          endif

          call a_spec_bdyupdate_ph( ph_save,a_ph_save,ph_2,a_ph_2,ph_tend,a_ph_tend,mu_tend,a_mu_tend,muts,a_muts,dts_rk,'h',&
&spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
        endif
      endif

!----------------------------------------
#ifndef FAST_CPY
w_2(:,:,:) = w_2s(:,:,:)
ww(:,:,:) = wws(:,:,:)
u_2(:,:,:) = u_2s(:,:,:)
v_2(:,:,:) = v_2s(:,:,:)
mu_2(:,:) = mu_2s(:,:)
t_2(:,:,:) = t_2s(:,:,:)
ph_2(:,:,:) = ph_2s(:,:,:)
a(:,:,:) = as(:,:,:)
t_2save(:,:,:) = t_2saves(:,:,:) 
#else
  call fast_copy_lint( w_2 , w_2s , n3d_elem )
  call fast_copy_lint( ww , wws , n3d_elem )
  call fast_copy_lint( u_2 , u_2s , n3d_elem )
  call fast_copy_lint( v_2 , v_2s , n3d_elem )
  call fast_copy_lint( mu_2 , mu_2s , n2d_elem )
  call fast_copy_lint( t_2 , t_2s , n3d_elem )
  call fast_copy_lint( ph_2 , ph_2s , n3d_elem )
  call fast_copy_lint( a , as , n3d_elem )
  call fast_copy_lint( t_2save , t_2saves , n3d_elem )
#endif
BENCH_END(comp81)


BENCH_START(exch76)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_2955.inc"
! HALO_2955 dyn_em 48:a_mut,a_muts,a_t_save,al,alpha,t_save,u_2,u_save,v_2,v_save,ww1









#endif
BENCH_END(exch76)

      if (non_hydrostatic) then

BENCH_START(exch77)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_2973.inc"
! HALO_2973 dyn_em 48:a,a_mut,a_muts,a_ph_save,a_t_save,alpha,c2a,cqw,gamma,mu_2,muave,muts,ph_2,ph_save,ph_tend,rw_tend,t_2,t_2save,w_2
#endif
BENCH_END(exch77)

BENCH_START(comp82)
        call a_advance_w( w_2,a_w_2,rw_tend,a_rw_tend,ww,a_ww,u_2,a_u_2,v_2,a_v_2,mu_2,a_mu_2,mut,a_mut,muave,a_muave,muts,a_muts,&
&t_2save,a_t_2save,t_2,a_t_2,t_save,a_t_save,ph_2,a_ph_2,ph_save,a_ph_save,phb,ph_tend,a_ph_tend,ht,c2a,a_c2a,cqw,a_cqw,&
&alt,a_alt,alb,a,a_a,alpha,a_alpha,gamma,a_gamma,rdx,rdy,dts,t0,epssm,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,&
&ide,jds,jde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp82)

      endif

BENCH_START(exch78)
#ifdef DM_PARALLEL
lineno = __LINE__
# include "HALO_2991.inc"
! HALO_2991 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,a_w_2,a_ww








#endif
BENCH_END(exch78)

BENCH_START(comp83)
      call a_sumflux( a_u_2,a_v_2,a_ww,u_save,a_u_save,v_save,a_v_save,a_ww1,muu,a_muu,muv,a_muv,a_ru_m,a_rv_m,a_ww_m,msfu,msfv,&
&iteration,number_of_small_timesteps,ide,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp83)

BENCH_START(exch79)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3013.inc"
! HALO_3013 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,a_ww








#endif
BENCH_END(exch79)

BENCH_START(comp84)
      if (config_flags%specified .or. config_flags%nested) then
        call a_spec_bdyupdate( a_muts,a_mu_tend,dts_rk,'m',spec_zone,ids,ide,jds,jde,1,ims,ime,jms,jme,1,1,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call a_spec_bdyupdate( a_mu_2,a_mu_tend,dts_rk,'m',spec_zone,ids,ide,jds,jde,1,ims,ime,jms,jme,1,1,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
        call a_spec_bdyupdate( a_t_2,a_t_tend,dts_rk,'t',spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),&
&grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif

!----------------------------------------
#ifndef FAST_CPY
u_2(:,:,:) = u_2r(:,:,:)
v_2(:,:,:) = v_2r(:,:,:)
ww(:,:,:) = wwr(:,:,:)
#else
  call fast_copy_lint( u_2 , u_2r , n3d_elem )
  call fast_copy_lint( v_2 , v_2r , n3d_elem )
  call fast_copy_lint( ww , wwr , n3d_elem )
#endif
BENCH_END(comp84)

BENCH_START(exch80)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3042.inc"
! HALO_3042 dyn_em 48:a_a,a_alpha,a_c2a,a_cqw,a_gamma,a_muave,a_mut,a_muts,a_ph_tend,a_rw_tend,a_t_2save,a_t_save,a_u_2,a_v_2,a_ww,ww,ww1









#endif
BENCH_END(exch80)

BENCH_START(comp85_a_advance_mu_t)
      call a_advance_mu_t( ww,a_ww,ww1,a_ww1,u_2,a_u_2,u_save,a_u_save,v_2,a_v_2,v_save,a_v_save,a_mu_2,a_mut,a_muave,a_muts,muu,&
&a_muu,muv,a_muv,a_mudf,a_t_2,t_save,a_t_save,a_t_2save,a_t_tend,mu_tend,a_mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,&
&msfv,msft,config_flags,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-2,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+2,ide),    &
        max(grid%j_start(ij)-2,jds),  &
        min(grid%j_end(ij)+2,jde),k_start,k_end )
BENCH_END(comp85_a_advance_mu_t)

BENCH_START(comp85)
!----------------------------------------
#ifndef FAST_CPY
    al(:,:,:) = alq(:,:,:)
    mu_2(:,:) = mu_2q(:,:)
    p(:,:,:) = pq(:,:,:)
    ph_2(:,:,:) = ph_2q(:,:,:)
#else
    call fast_copy_lint ( al , alq , n3d_elem )
    call fast_copy_lint ( mu_2 , mu_2q , n2d_elem )
    call fast_copy_lint ( p , pq , n3d_elem )
    call fast_copy_lint ( ph_2 , ph_2q , n3d_elem )
#endif
BENCH_END(comp85)

BENCH_START(exch81)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3072.inc"
! HALO_3072 dyn_em 48:a_mu_2,a_t_2,a_u_2,a_v_2,a_ww,al,ww








#endif
BENCH_END(exch81)

BENCH_START(comp86)
      if (config_flags%specified .or. config_flags%nested) then
        call a_spec_bdyupdate( a_v_2,a_rv_tend,dts_rk,'v',spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),&
&grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
        call a_spec_bdyupdate( a_u_2,a_ru_tend,dts_rk,'u',spec_zone,ids,ide,jds,jde,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),&
&grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

      endif
BENCH_END(comp86)







BENCH_START(exch82)



















#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_3120.inc"
! HALO_3120 dyn_em 48:a_mu_2,a_mu_save,a_mut,a_muts,a_muu,a_muus,a_muv,a_muvs,a_ph_save,a_t_2,a_t_save,a_u_2,a_u_save,a_v_2,a_v_save,a_w_save,a_ww,cqu,cqv,mu_2,muts,muus,muvs,ru_m,rv_m,w_save,ww
#endif

BENCH_END(exch82)
BENCH_START(comp87)
      call a_advance_uv( a_u_2,a_ru_tend,a_v_2,a_rv_tend,p,a_p,pb,ph_2,a_ph_2,php,a_php,alt,a_alt,al,a_al,mu_2,a_mu_2,muu,a_muu,&
&cqu,a_cqu,muv,a_muv,cqv,a_cqv,a_mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,&
&ide,jds,jde,kde,ims,ime,jms,jme,kms,kme, &
        max(grid%i_start(ij)-2,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+2,ide),    &
        max(grid%j_start(ij)-2,jds),  &
        min(grid%j_end(ij)+2,jde),k_start,k_end )
BENCH_END(comp87)

  end do a_small_steps

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 87
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

BENCH_START(comp88)
#ifndef FAST_CPY
  ww_m(:,:,:) = ww_m_keep3(:,:,:)
  rv_m(:,:,:) = rv_m_keep3(:,:,:)
  ru_m(:,:,:) = ru_m_keep3(:,:,:)
  a(:,:,:) = a_keep3(:,:,:)
  alpha(:,:,:) = alpha_keep3(:,:,:)
  gamma(:,:,:) = gamma_keep3(:,:,:)
!------------------------------

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
!----------------------------------------
  mudf(:,:) = mudf_ma(:,:)
!----------------------------------------
#else
  call fast_copy_lint(  ww_m , ww_m_keep3 , n3d_elem )
  call fast_copy_lint(  rv_m , rv_m_keep3 , n3d_elem )
  call fast_copy_lint(  ru_m , ru_m_keep3 , n3d_elem )
  call fast_copy_lint(  a , a_keep3 , n3d_elem )
  call fast_copy_lint(  alpha , alpha_keep3 , n3d_elem )
  call fast_copy_lint(  gamma , gamma_keep3 , n3d_elem )
!------------------------------

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
!----------------------------------------
  call fast_copy_lint(  mudf , mudf_ma , n2d_elem )
!----------------------------------------
#endif
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
BENCH_END(comp88)

BENCH_START(exch83)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3212.inc"
! HALO_3212 dyn_em 48:al,p,ww
#endif
BENCH_END(exch83)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepb4acc: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepb4acc:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepb4acc

BENCH_START(comp89)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp89)

 endif restore_rk_step_prepb4acc

BENCH_START(exch84)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3222.inc"
! HALO_3222 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3227.inc"
! HALO_3227 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch84)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww

  rk_step_is_onzf: if (rk_step .eq. 1) then

BENCH_START(comp90)
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp90)

BENCH_START(exch85)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch85)

BENCH_START(comp91)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp91)

BENCH_START(exch86)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch86)

BENCH_START(comp92)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    endif
BENCH_END(comp92)
  endif rk_step_is_onzf

BENCH_START(exch87)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_3273.inc"
! HALO_3273 dyn_em 8:moist_2



#endif
BENCH_END(exch87)

!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendb4acc: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendb4acc:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendb4acc

WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_a_calc_coef_w, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )
BENCH_START(comp7777)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp7777)

 endif restore_rktendb4acc

BENCH_START(comp93)
!  recompute : cqw,mu_tend,ph_save,ph_tend,ru_tend,ru_tendf,rv_tend,rv_t
! endf,rw_tend,rw_tendf,t_save,t_tend,t_tendf,u_save,v_save,w_save

    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (config_flags%specified .or. config_flags%nested) then
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp93)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 93
#include <all_debug_halo.h>
#endif

!------------------------------
!------------------------------

!----------------------------------------
BENCH_START(exch88)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_3313.inc"
! HALO_3313 dyn_em 48:mu_2,t_2,u_2,v_2,w_2
#endif
BENCH_END(exch88)
BENCH_START(comp94)
#ifndef FAST_CPY
u_1o(:,:,:) = u_1(:,:,:)
u_2o(:,:,:) = u_2(:,:,:)
v_1o(:,:,:) = v_1(:,:,:)
v_2o(:,:,:) = v_2(:,:,:)
w_1o(:,:,:) = w_1(:,:,:)
w_2o(:,:,:) = w_2(:,:,:)
t_1o(:,:,:) = t_1(:,:,:)
t_2o(:,:,:) = t_2(:,:,:)
mu_1o(:,:) = mu_1(:,:)
mu_2o(:,:) = mu_2(:,:)
po(:,:,:) = p(:,:,:)
#else
 call fast_copy_lint(u_1o , u_1, n3d_elem )
 call fast_copy_lint(u_2o , u_2, n3d_elem )
 call fast_copy_lint(v_1o , v_1, n3d_elem )
 call fast_copy_lint(v_2o , v_2, n3d_elem )
 call fast_copy_lint(w_1o , w_1, n3d_elem )
 call fast_copy_lint(w_2o , w_2, n3d_elem )
 call fast_copy_lint(t_1o , t_1, n3d_elem )
 call fast_copy_lint(t_2o , t_2, n3d_elem )
 call fast_copy_lint(mu_1o , mu_1, n2d_elem )
 call fast_copy_lint(mu_2o , mu_2, n2d_elem )
 call fast_copy_lint(po , p, n3d_elem )
#endif

    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp94)

BENCH_START(exch89)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_3335.inc"
! HALO_3335 dyn_em 8:al,alt,mu_1,mu_2,mudf,p,pb,ph_2,php,t_1,t_save,u_save,v_save
#endif

!----------------------------------------
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_3341.inc"
! HALO_3341 dyn_em 48:al,mu_2,p,ph_2,t_2
#endif
BENCH_END(exch89)
BENCH_START(comp95)

#ifndef FAST_CPY
alp(:,:,:) = al(:,:,:)
pp(:,:,:) = p(:,:,:)
ph_2p(:,:,:) = ph_2(:,:,:)
t_2p(:,:,:) = t_2(:,:,:)
mu_2p(:,:) = mu_2(:,:)
#else
  call fast_copy_lint( alp , al, n3d_elem )
  call fast_copy_lint( pp , p, n3d_elem )
  call fast_copy_lint( ph_2p , ph_2, n3d_elem )
  call fast_copy_lint( t_2p , t_2, n3d_elem )
  call fast_copy_lint( mu_2p , mu_2, n2d_elem )
#endif

    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp95)

BENCH_START(exch90)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3356.inc"
! HALO_3356 dyn_em 8:mu_2,ph_2,u_2,v_2;48:a_alt,a_mu_2,a_p,a_ph_2,a_php,a_t_2,a_u_2,a_v_2,a_w_2,a_ww,al,alt,p,php,ww







#endif
BENCH_END(exch90)

BENCH_START(comp96)
    if (non_hydrostatic) then
      call a_calc_coef_w( a,a_a,alpha,a_alpha,gamma,a_gamma,mut,a_mut,cqw,a_cqw,rdn,rdnw,c2a,a_c2a,dts,g,epssm,ide,jde,kde,ims,ime,&
&jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde) )
    endif

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 96
#include <all_debug_halo.h>
#endif

!----------------------------------------
#ifndef FAST_CPY
al(:,:,:) = alp(:,:,:)
p(:,:,:) = pp(:,:,:)
ph_2(:,:,:) = ph_2p(:,:,:)
t_2(:,:,:) = t_2p(:,:,:)
mu_2(:,:) = mu_2p(:,:)
#else
  call fast_copy_lint( al , alp , n3d_elem )
  call fast_copy_lint( p , pp , n3d_elem )
  call fast_copy_lint( ph_2 , ph_2p , n3d_elem )
  call fast_copy_lint( t_2 , t_2p , n3d_elem )
  call fast_copy_lint( mu_2 , mu_2p , n2d_elem )
#endif
BENCH_END(comp96)

BENCH_START(exch91)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3386.inc"
! HALO_3386 dyn_em 48:a_alt,a_mu_2,a_p,a_ph_2,a_php,al,p





#endif
BENCH_END(exch91)

BENCH_START(comp97)
    call a_calc_p_rho( al,a_al,p,a_p,ph_2,a_ph_2,alt,a_alt,t_2,a_t_2,t_save,a_t_save,c2a,a_c2a,a_pm1,mu_2,a_mu_2,muts,a_muts,znu,&
&t0,rdnw,dnw,smdiv,non_hydrostatic,0,ide,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 97
#include <all_debug_halo.h>
#endif

!----------------------------------------
#ifndef FAST_CPY
u_1(:,:,:) = u_1o(:,:,:)
u_2(:,:,:) = u_2o(:,:,:)
v_1(:,:,:) = v_1o(:,:,:)
v_2(:,:,:) = v_2o(:,:,:)
w_1(:,:,:) = w_1o(:,:,:)
w_2(:,:,:) = w_2o(:,:,:)
t_1(:,:,:) = t_1o(:,:,:)
t_2(:,:,:) = t_2o(:,:,:)
mu_1(:,:) = mu_1o(:,:)
mu_2(:,:) = mu_2o(:,:)
p(:,:,:) = po(:,:,:)
#else
  call fast_copy_lint ( u_1 , u_1o , n3d_elem )
  call fast_copy_lint ( u_2 , u_2o , n3d_elem )
  call fast_copy_lint ( v_1 , v_1o , n3d_elem )
  call fast_copy_lint ( v_2 , v_2o , n3d_elem )
  call fast_copy_lint ( w_1 , w_1o , n3d_elem )
  call fast_copy_lint ( w_2 , w_2o , n3d_elem )
  call fast_copy_lint ( t_1 , t_1o , n3d_elem )
  call fast_copy_lint ( t_2 , t_2o , n3d_elem )
  call fast_copy_lint ( mu_1 , mu_1o , n2d_elem )
  call fast_copy_lint ( mu_2 , mu_2o , n2d_elem )
  call fast_copy_lint ( p , po , n3d_elem )
#endif
BENCH_END(comp97)


BENCH_START(exch92)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3419.inc"
! HALO_3419 dyn_em 48:a_alt,a_c2a,a_mu_2,a_mu_save,a_mudf,a_mut,a_muts,a_muu,a_muv,a_p,a_ph_2,a_ph_save,a_t_2,a_t_save,a_u_save,a_v_save,a_w_save,a_ww1,mu_1,mu_2,muts,muus,muvs,pb,t_1,t_2,u_2,v_2
#endif
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3423.inc"
! HALO_3423 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,al





#endif
BENCH_END(exch92)

BENCH_START(comp98_a_small_step_prep)
    call a_small_step_prep( u_1,a_u_1,u_2,a_u_2,v_1,a_v_1,v_2,a_v_2,w_1,a_w_1,w_2,a_w_2,t_1,a_t_1,t_2,a_t_2,a_ph_1,a_ph_2,mub,mu_1,&
&a_mu_1,mu_2,a_mu_2,muu,a_muu,muus,a_muus,muv,a_muv,muvs,a_muvs,mut,a_mut,muts,a_muts,a_mudf,a_u_save,a_v_save,a_w_save,&
&a_t_save,a_ph_save,a_mu_save,a_ww,a_ww1,a_c2a,pb,p,a_p,alt,a_alt,msfu,msfv,msft,rk_step,leapfrog,ide,jde,kde,ims,ime,jms,jme,&
&kms,kme, &
        max(grid%i_start(ij)-1,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp98_a_small_step_prep)

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 98
#include <all_debug_halo.h>
#endif

BENCH_START(comp98)
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
  call fast_copy_lint ( xkmhd , xkmhdh , n3d_elem ) 
  call fast_copy_lint ( w_2 , w_2h , n3d_elem ) 
  call fast_copy_lint ( w_1 , w_1h , n3d_elem ) 
  call fast_copy_lint ( v_2 , v_2h , n3d_elem ) 
  call fast_copy_lint ( v_1 , v_1h , n3d_elem ) 
  call fast_copy_lint ( u_2 , u_2h , n3d_elem ) 
  call fast_copy_lint ( u_1 , u_1h , n3d_elem ) 
  call fast_copy_lint ( t_tendf , t_tendfh , n3d_elem ) 
  call fast_copy_lint ( t_2 , t_2h , n3d_elem ) 
  call fast_copy_lint ( t_1 , t_1h , n3d_elem ) 
  call fast_copy_lint ( rw_tendf , rw_tendfh , n3d_elem ) 
  call fast_copy_lint ( rv_tendf , rv_tendfh , n3d_elem ) 
  call fast_copy_lint ( ru_tendf , ru_tendfh , n3d_elem ) 
  call fast_copy_lint ( pm1 , pm1h , n3d_elem ) 
  call fast_copy_lint ( ph_tendf , ph_tendfh , n3d_elem ) 
  call fast_copy_lint ( ph_2 , ph_2h , n3d_elem ) 
  call fast_copy_lint ( ph_1 , ph_1h , n3d_elem ) 
  call fast_copy_lint ( p , ph , n3d_elem ) 
  call fast_copy_lint ( mu_2 , mu_2h , n2d_elem ) 
  call fast_copy_lint ( mu_1 , mu_1h , n2d_elem ) 
  call fast_copy_lint ( moist_tend(ims,kms,jms,2) , moist_tendh(ims,kms,jms,2) , n3d_elem*(num_moist-1) ) 
  call fast_copy_lint ( moist_2(ims,kms,jms,2) , moist_2h(ims,kms,jms,2) , n3d_elem*(num_moist-1) ) 
  call fast_copy_lint ( moist_1(ims,kms,jms,2) , moist_1h(ims,kms,jms,2) , n3d_elem*(num_moist-1) ) 
  call fast_copy_lint ( dnw , dnwh , kme-kms+1 ) 
  call fast_copy_lint ( al , alh , n3d_elem ) 
#endif
!----------------------------------------
!---------- added by zzma for recomputing basic states -------------
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
BENCH_END(comp98)

BENCH_START(exch93)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3510.inc"
! HALO_3510 dyn_em 48:al
#endif
BENCH_END(exch93)

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepb4asbd: if ( config_flags%remove_rk_step_prep ) then
BENCH_START(comprestore_rk_step_prep_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepb4asbd:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
#include "restore_rk_step_prep.h"
BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepb4asbd

BENCH_START(comp99)
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp99)

 endif restore_rk_step_prepb4asbd

BENCH_START(exch94)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3520.inc"
! HALO_3520 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3525.inc"
! HALO_3525 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch94)

! recompute : alt,cqu,cqv,cqw,mut,muu,muv,php,ru,rv,rw,ww

BENCH_START(comp100)
  rk_step_is_onzs: if (rk_step .eq. 1) then

      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp100)

BENCH_START(exch95)
#ifdef DM_PARALLEL
lineno = __LINE__

#endif
BENCH_END(exch95)

BENCH_START(comp101)
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp101)

BENCH_START(exch96)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch96)

BENCH_START(comp102)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then

        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )

    endif
BENCH_END(comp102)
  endif rk_step_is_onzs

BENCH_START(exch97)
#ifdef DM_PARALLEL
lineno = __LINE__




#endif
BENCH_END(exch97)

!  recompute : moist_tend,ph_tendf,ru_tendf,rv_tendf,rw_tendf,t_tendf,xk
! mhd

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_tendency.  
 restore_rktendb4asbd: if ( config_flags%remove_rktend ) then
BENCH_START(comprestore_rktend_tim)
   WRITE(wrf_err_message,*)'DEBUG:  restore_rktendb4asbd:  rk_step, rk_order = ',rk_step,rk_order
   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
   ! store fields
   CALL select_frame(rk_step, BUF_RKTEND)
#include "restore_rktend.h"
BENCH_END(comprestore_rktend_tim)
 else  ! restore_rktendb4asbd

WRITE(wrf_err_message,*) 'DEBUG:  calling rk_tendency before_a_spec_bdy_dry, rk_step = ',rk_step
CALL wrf_debug( 10, TRIM(wrf_err_message) )
BENCH_START(comp103)
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp103)

 endif restore_rktendb4asbd

!------------------------------
!------------------------------

BENCH_START(exch98)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3593.inc"
! HALO_3593 dyn_em 48:a_alt,a_mu_1,a_mu_2,a_p,a_ph_1,a_ph_2,a_t_1,a_t_2,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww





#endif
BENCH_END(exch98)

    if (config_flags%specified .or. config_flags%nested) then
BENCH_START(exch99)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3606.inc"
! HALO_3606 dyn_em 48:a_mu_tend,a_p,a_ph_2,a_ph_tend,a_ru_tend,a_rv_tend,a_rw_tend,a_t_tend,a_u_2,a_v_2,ph_2
#endif
BENCH_END(exch99)
BENCH_START(comp104)
      call a_spec_bdy_dry( config_flags,a_ru_tend,a_rv_tend,a_ph_tend,a_t_tend,a_rw_tend,a_mu_tend,a_u_bt,a_v_bt,a_ph_bt,a_t_bt,&
&a_w_bt,a_mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-2,ids),  &
        min(grid%i_end(ij)+2,ide),    &
        max(grid%j_start(ij)-2,jds),  &
        min(grid%j_end(ij)+2,jde),k_start,k_end )
BENCH_END(comp104)
    endif
BENCH_START(exch100)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3619.inc"
! HALO_3619 dyn_em 48:a_mut,a_ph_save,a_ru_tend,a_rv_tend,a_t_save,a_t_tend,a_u_save,a_v_save,a_w_save
#endif
BENCH_END(exch100)
BENCH_START(comp105)
    call a_rk_addtend_dry( a_ru_tend,a_rv_tend,a_rw_tend,a_ph_tend,a_t_tend,a_ru_tendf,a_rv_tendf,a_rw_tendf,a_ph_tendf,a_t_tendf,&
&a_u_save,a_v_save,a_w_save,a_ph_save,a_t_save,rk_step,h_diabatic,a_mut,msft,msfu,msfv,ide,jde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-2,ids),  &
        min(grid%i_end(ij)+2,ide),    &
        max(grid%j_start(ij)-2,jds),  &
        min(grid%j_end(ij)+2,jde),k_start,k_end )
BENCH_END(comp105)
    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
BENCH_START(exch101)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_3632.inc"
! HALO_3632 dyn_em 48:a_mu_2,a_mu_tend,a_mut,a_ph_2,a_ph_save,a_ru,a_rv,a_t_2,a_t_save,a_u_save,a_v_save,a_w_2,a_w_save,mut,ph_2,t_2,w_2
#endif
BENCH_END(exch101)
BENCH_START(comp106)
      call a_relax_bdy_dry( config_flags,a_u_save,a_v_save,a_ph_save,a_t_save,a_w_save,a_mu_tend,a_ru,a_rv,ph_2,a_ph_2,t_2,a_t_2,&
&w_2,a_w_2,a_mu_2,mut,a_mut,a_u_b,a_v_b,a_ph_b,a_t_b,a_w_b,a_mu_b,a_u_bt,a_v_bt,a_ph_bt,a_t_bt,a_w_bt,a_mu_bt,spec_bdy_width,&
&spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-2,ids),  &
        min(grid%i_end(ij)+2,ide),    &
        max(grid%j_start(ij)-2,jds),  &
        min(grid%j_end(ij)+2,jde),k_start,k_end )
BENCH_END(comp106)
    endif

! TBH:  turn this off once halo debugging is finished
#ifndef NO_HALO_FIX
  filecount = 10000 + ((rk_order-rk_step+1)*1000) + 101
#include <all_debug_halo.h>
#endif

! TBH:  $$$here...  doing dependence analysis above...  

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
! HALO_ADJ_A1 dyn_em  48:ru,rv,rw,ww,php,alt,al,p,muu,muv,mut
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
! TBH:  OVERWRITE ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
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
! TBH:  OVERWRITE mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,p8w,t_phy,t8w,z,z_at_w,dz8w,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,rqrcuten,rqicuten,rqscuten,rthften,rqvften
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
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

! TBH:  (not called with current namelist settings)
! TBH:  OVERWRITE xkmv,xkmh,xkmhd,xkhv,xkhh,BN2
! TBH:  HORIZONTAL DEPENDENCE:  defor12  ( [    i,i+1], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  defor13  ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  defor23  ( [    i    ], [    j,j+1] )
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
$$$ add HALO_3783
! ??? check fields in HALO_3783 and HALO_3784 for (R)
! ??? remove halo update of muu,muv above?
! ??? remove mub,pb,phb here?  
! ??? only exchange z,z_at_w,a_z,a_z_at_w here only when rk_step .eq. 1?
$$$ BUG BELOW HERE
! HALO_3783 dyn_em 8:a_alt,a_mu_2,a_mut,a_muu,a_muv,a_p,a_php,a_ru,a_ru_tendf,a_rv,a_rv_tendf,a_rw,a_rw_tendf,a_t_1,a_t_tendf,a_u_1,a_v_1,a_w_1,cqu,cqv,cqw,mub,pb,php,rw,t_init,z,z_at_w,a_z,a_z_at_w,xland
! HALO_3784 dyn_em 80:a_ph_2,a_ph_tend,a_ru_tend,a_rv_tend,a_rw_tend,a_t_2,a_t_tend,a_u_2,a_v_2,a_w_2,muu,muv,phb,ru,rv
$$$ BUG ABOVE HERE
! OLD: HALO_3784 dyn_em 80:a_advect_tend,a_alt,a_cqu,a_cqv,a_cqw,a_moist_tend,a_mu_2,a_mu_tend,a_mut,a_muu,a_muv,a_p,a_ph_1,a_ph_2,a_ph_tend,a_php,a_ru,a_ru_tend,a_ru_tendf,a_rv,a_rv_tend,a_rv_tendf,a_rw,a_rw_tend,a_rw_tendf,a_t_1,a_t_2,a_t_tend,a_t_tendf,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww,al,alt,cosa,cqu,cqv,cqw,e,f,msft,msfu,msfv,mu_2,mub,mut,muu,muv,p,pb,ph_1,ph_2,phb,php,ru,rv,rw,t_1,t_2,t_init,u_1,u_2,v_1,v_2,w_1,w_2,ww,z,z_at_w,a_z,a_z_at_w,xland
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

! TBH:  OVERWRITE a_muu, a_muv, a_z, a_z_at_w, a_u_2, a_v_2
! TBH:  HORIZONTAL DEPENDENCE:  z         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  z_at_w    ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z_at_w  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  xland     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2       ( [i-1,i,i+1], [j-1,j,j+1] )
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
! TBH:  OVERWRITE a_al,a_alt,a_cqu,a_cqv,a_cqw,a_mu_2,a_mut,a_muu,a_muv,a_p,a_ph_2,a_ph_tend,a_php,a_ru,a_ru_tendf,a_rv,a_rv_tendf,a_rw,a_rw_tendf,a_t_2,a_t_1,a_t_tendf,a_u_2,a_u_1,a_v_2,a_v_1,a_w_2,a_w_1,a_ww,a_xkmhd
! TBH:  NOTE:  "(R)" indicates that a dependence only exists due to
! TBH:  NOTE:  computation in the halo regions.  This must be accounted
! TBH:  NOTE:  for at some previous time, but no actual dependence on
! TBH:  NOTE:  horizontally adjacent values exists here.
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
! TBH:  HORIZONTAL DEPENDENCE:  a_t_2       ( [i-1,i,i+1], [j-1,j,j+1] ) (R)
! TBH:  HORIZONTAL DEPENDENCE:  a_t_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_tendf   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_u_2       ( [i-1,i,i+1], [j-1,j,j+1] ) (R)
! TBH:  HORIZONTAL DEPENDENCE:  a_u_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_v_2       ( [i-1,i,i+1], [j-1,j,j+1] ) (R)
! TBH:  HORIZONTAL DEPENDENCE:  a_v_1       ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_w_2       ( [i-1,i,i+1], [j-1,j,j+1] ) (R)
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
! #    include "HALO_3803.inc"
! TBH CHANGE:  Removed
#endif
BENCH_END(exch107)

BENCH_START(comp_a_rk_tendency2)
! TBH:  OVERWRITE a_mu_tend,a_mut,a_ph_save,a_ph_tend,a_ru,a_ru_tend,a_rv,a_rv_tend,a_rw_tend,a_t_2,a_t_save,a_t_tend,a_u_2,a_u_save,a_v_2,a_v_save,a_w_2,a_w_save,a_ww
! TBH:  HORIZONTAL DEPENDENCE:  a_ru_tend  ( [i-3,i,i+4], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rv_tend  ( [i-3,i,i+3], [j-3,j,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  a_rw_tend  ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_2      ( [i-3,i,i+2], [j-3,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  a_t_tend   ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  a_u_2      ( [i-3,i,i+2], [j-3,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  a_v_2      ( [i-3,i,i+2], [j-3,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  a_w_2      ( [i-3,i,i+2], [j-3,j,j+2] )
! TBH:  HORIZONTAL DEPENDENCE:  msft       ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu       ( [i-4,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv       ( [i-3,i,i+3], [j-4,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru         ( [i-3,i,i+3], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv         ( [i-1,i    ], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  t          ( [i-3,i,i+3], [j-3,i,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  u          ( [i-3,i,i+4], [j-3,i,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  v          ( [i-3,i,i+3], [j-3,i,j+4] )
! TBH:  HORIZONTAL DEPENDENCE:  w          ( [i-3,i,i+3], [j-3,i,j+3] )
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
! TBH:  remove t_1?
  t_1(:,:,:) = t_1h(:,:,:)
  rw_tendf(:,:,:) = rw_tendfh(:,:,:)
  rv_tendf(:,:,:) = rv_tendfh(:,:,:)
  ru_tendf(:,:,:) = ru_tendfh(:,:,:)
  pm1(:,:,:) = pm1h(:,:,:)
  ph_tendf(:,:,:) = ph_tendfh(:,:,:)
  ph_2(:,:,:) = ph_2h(:,:,:)
! TBH:  remove ph_1?
  ph_1(:,:,:) = ph_1h(:,:,:)
! TBH:  remove p?
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
! #    include "HALO_3886.inc"
! TBH CHANGE:  Removed
#endif
BENCH_END(exch108)

! recompute : dt_rk,dts_rk,number_of_small_timesteps

! TBH:  Trade redundant computation for memory.  
! TBH:  Restore fields computed earlier by calls to rk_step_prep.  
 restore_rk_step_prepb4ackk: if ( config_flags%remove_rk_step_prep ) then
!BENCH_START(comprestore_rk_step_prep_tim)
!   WRITE(wrf_err_message,*)'DEBUG:  restore_rk_step_prepb4ackk:  rk_step, rk_order = ',rk_step,rk_order
!   CALL wrf_debug ( 10 , TRIM(wrf_err_message) )
!   ! store fields
!   CALL select_frame(rk_step, BUF_RK_STEP_PREP)
!#include "restore_rk_step_prep.h"
!BENCH_END(comprestore_rk_step_prep_tim)
 else  ! restore_rk_step_prepb4ackk

BENCH_START(comp114)
! TBH:  OVERWRITE ru,rv,rw,ww,alt,muu,muv,mut,php,cqu,cqv,cqw
! TBH:  HORIZONTAL DEPENDENCE:  mu_2     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mub      ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  moist_2  ( [i-1,i    ], [j-1,j    ] )

! TBH:  removed this call
!    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
!&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
!&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
!BENCH_END(comp114)

 endif restore_rk_step_prepb4ackk

BENCH_START(exch109)
#ifdef DM_PARALLEL
lineno = __LINE__
! #    include "HALO_3898.inc"
! TBH CHANGE:  Removed
#endif

#ifdef DM_PARALLEL
lineno = __LINE__
! #    include "HALO_3903.inc"
! TBH CHANGE:  Removed
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
! TBH:  (not called with current namelist settings)
! TBH:  OVERWRITE a_bn2h,a_moist_2,a_p_phy,a_p8w,a_t_phy,a_t8w,a_th_phy,a_xkmhd,bn2
! TBH:  HORIZONTAL DEPENDENCE:  defor12  ( [    i,i+1], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  defor13  ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  defor23  ( [    i    ], [    j,j+1] )
        call a_calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,a_xkmhd,xkmv,xkhh,xkhv,bn2,a_bn2h,khdif,div,&
&defor11,defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,a_p8w,t8w,a_t8w,th_phy,a_th_phy,t_phy,a_t_phy,&
&p_phy,a_p_phy,moist_2,a_moist_2,dn,dnw,dx,dy,rdz,rdzw,num_3d_m,cf1,cf2,cf3,kh_tke_upper_bound,ids,ide,jds,jde,kde,ims,ime,&
&jms,jme,kms,kme, &
        max(grid%i_start(ij)-1,ids),  &   ! compute out an extra row/col on halo
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )
BENCH_END(comp115)

BENCH_START(exch111)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_3935.inc"
! TBH CHANGE:  Removed all except for a_moist_2 and moved into "if".  
! HALO_3935 dyn_em 8:a_moist_2



#endif
BENCH_END(exch111)

    endif

BENCH_START(comp116)
! TBH:  OVERWRITE a_p,a_p8w,a_p_phy,a_ph_2,a_pi_phyh,a_t_2,a_t8w,a_t_phy,a_th_phy,a_z,a_z_at_wh,p_phy,pi_phy,t_phy,th_phy,z,z_at_w
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call a_phy_prep( p,a_p,pb,ph_2,a_ph_2,phb,t_2,a_t_2,th_phy,a_th_phy,p_phy,a_p_phy,pi_phy,a_pi_phyh,a_p8w,t_phy,a_t_phy,a_t8w,&
&z,a_z,z_at_w,a_z_at_wh,fnm,fnp,ide,jde,kde,ims,ime,jms,jme,kms,kme,&
        max(grid%i_start(ij)-1,ids),  &
        min(grid%i_end(ij)+1,ide),    &
        max(grid%j_start(ij)-1,jds),  &
        min(grid%j_end(ij)+1,jde),k_start,k_end )

! TBH:  OVERWRITE a_ru_tendf,a_rv_tendf,a_rw_tendf,a_ph_tendf,a_t_tendf,a_moist_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
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
! TBH:  removed #    include "HALO_3963.inc"
! TBH CHANGE:  Reduce extent to 8-point for ALL FIELDS
! TBH CHANGE:  Moved a_mu_2 to HALO_3982
! TBH CHANGE:  Remove all other fields.  
! HALO_3963 REMOVED






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
! ??? TBH:  avoid resotring mu_2 here?  
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
! HALO_3982 dyn_em 8:a_muu,a_muv,a_rw,a_u_2,a_v_2,a_ww,a_mu_2

#endif
BENCH_END(exch113)

BENCH_START(comp118)
! TBH:  OVERWRITE muu,muv,mut,a_al,a_alt,a_cqu,a_cqv,a_cqw,a_moist_2,a_mu_2,a_mut,a_muu,a_muv,a_ph_2,a_php,a_ru,a_rv,a_rw,a_u_2,a_v_2,a_w_2,a_ww
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


