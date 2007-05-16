! DEPENDENCE ANALYSIS of runge_kutta_loop until the end of small_step loop.  
! Tom Henderson          4/17/07.  

runge_kutta_looq: do rk_step = 1, rk_order

! TBH:  OK to always recompute "comp2".  
BENCH_START(comp2)
  dtm=dt
  if (rk_order .eq. 1) then
    if (step_number .ne. 1) then
      number_of_small_timesteps = 2*time_step_sound
      dt_rk = dt
      dtm=2*dt
    else
      number_of_small_timesteps = time_step_sound
      dt_rk = dt/2.
      dtm=dt
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
BENCH_END(comp2)

BENCH_START(exch2)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_0616.inc"  ! 48pt!  
! TBH:  OVERWRITE mut,muu,muv,ru,rv,rw





#endif

#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_0626.inc"
! TBH:  OVERWRITE al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
#endif
BENCH_END(exch2)

BENCH_START(comp3)
! TBH:  OVERWRITE ru,rv,rw,ww,php,alt,muu,muv,mut,cqu,cqv,cqw
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  moist_2  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  mub      ( [i-1,i,i+1], [j-1,j,j+1] )
    call rk_step_prep( config_flags,rk_step,u_2,v_2,w_2,t_2,ph_2,mu_2,moist_2,ru,rv,rw,ww,php,alt,muu,muv,mub,mut,phb,pb,p,al,alb,&
&cqu,cqv,cqw,msfu,msfv,msft,fnm,fnp,dnw,rdx,rdy,num_3d_m,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp3)

BENCH_START(exch3)
#ifdef DM_PARALLEL
lineno = __LINE__
#    include "HALO_0637.inc"
! TBH:  OVERWRITE mut,muu,muv,php,ru,rv,rw,ww
#endif
BENCH_END(exch3)

  rk_step_is_ong: if (rk_step .eq. 1) then
BENCH_START(comp4)
! TBH:  OVERWRITE ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call init_zero_tendency( ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,tke_tend,moist_tend,chem_tend,num_3d_m,num_3d_c,rk_step,&
&ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp4)

BENCH_START(exch4)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_0648.inc"
! TBH:  OVERWRITE u_2,v_2
#endif
BENCH_END(exch4)

BENCH_START(comp5)
! TBH:  OVERWRITE mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,p8w,t_phy,t8w,z,z_at_w,dz8w,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,rqrcuten,rqicuten,rqscuten,rthften,rqvften
! TBH:  HORIZONTAL DEPENDENCE:  u_2      ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2      ( [    i    ], [    j,j+1] )
      call phy_prep( config_flags,mut,u_2,v_2,p,pb,alt,ph_2,phb,t_2,tsk,moist_2,num_3d_m,mu_3d,rho,th_phy,p_phy,pi_phy,u_phy,v_phy,&
&p8w,t_phy,t8w,z,z_at_w,dz8w,fnm,fnp,rthraten,rthblten,rublten,rvblten,rqvblten,rqcblten,rqiblten,rthcuten,rqvcuten,rqccuten,&
&rqrcuten,rqicuten,rqscuten,rthften,rqvften,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp5)

BENCH_START(exch5)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_0660.inc"
! TBH:  OVERWRITE z



#endif
BENCH_END(exch5)

BENCH_START(comp6)
    if (diff_opt .eq. 2 .or. diff_opt .eq. 1) then
! TBH:  OVERWRITE xkmv,xkmh,xkmhd,xkhv,xkhh,BN2
! TBH:  HORIZONTAL DEPENDENCE:  defor12  ( [    i,i+1], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  defor13  ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  defor23  ( [    i    ], [    j,j+1] )
        call calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,xkmh,xkmhd,xkmv,xkhh,xkhv,bn2,khdif,kvdif,div,defor11,&
&defor22,defor33,defor12,defor13,defor23,tke_2(ims,kms,jms),p8w,t8w,th_phy,t_phy,p_phy,moist_2,dn,dnw,dx,dy,rdz,rdzw,&
&mix_cr_len,num_3d_m,cf1,cf2,cf3,warm_rain,kh_tke_upper_bound,kv_tke_upper_bound,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp6)

  endif rk_step_is_ong

BENCH_START(exch6)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_0679.inc"
! TBH:  OVERWRITE moist_2



#endif
BENCH_END(exch6)

BENCH_START(comp7)
! TBH:  OVERWRITE ru_tend,rv_tend,rw_tend,t_tend,ph_tend,RTHFTEN,u_save,v_save,w_save,ph_save,t_save,ru_tendf,rv_tendf,rw_tendf,t_tendf,cqw,mu_tend,mu_save
! TBH:  HORIZONTAL DEPENDENCE:  al     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  alt    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  cosa   ( [i-1,i    ], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  e      ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  f      ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msft   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2   ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  mut    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  muu    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  muv    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  pb     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  phb    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ph_2   ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  php    ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  p      ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  ru     ( [i-1,i,i+1], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv     ( [i-1,i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  rw     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  sina   ( [    i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  t_2    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  t_init ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  u_2    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  w_2    ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ww     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  xkmhd  ( [i-1,i,i+1], [j-1,j,j+1] )
    call rk_tendency( config_flags,rk_step,ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,&
&mu_tend,u_save,v_save,w_save,ph_save,t_save,mu_save,rthften,ru,rv,rw,ww,u_2,v_2,w_2,t_2,ph_2,u_1,v_1,w_1,t_1,ph_1,h_diabatic,&
&phb,t_init,mu_2,mut,muu,muv,mub,al,alt,p,pb,php,cqu,cqv,cqw,u_base,v_base,t_base,qv_base,z_base,msfu,msfv,msft,f,e,sina,cosa,&
&fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,cf1,cf2,cf3,cfn,cfn1,num_3d_m,non_hydrostatic,leapfrog,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
! TBH:  OVERWRITE u_save,v_save,ph_save,t_save,w_save,mu_tend
! TBH:  HORIZONTAL DEPENDENCE:  ru     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  rv     ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ph_2   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  t_2    ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  w_2    ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2   ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  mut    ( [i-1,i,i+1], [j-1,j,j+1] )
      call relax_bdy_dry( config_flags,u_save,v_save,ph_save,t_save,w_save,mu_tend,ru,rv,ph_2,t_2,w_2,mu_2,mut,u_b,v_b,ph_b,t_b,&
&w_b,mu_b,u_bt,v_bt,ph_bt,t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,relax_zone,dtbc,fcx,gcx,ijds,ijde,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
! TBH:  OVERWRITE ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf
! TBH:  HORIZONTAL DEPENDENCE:  NONE
    call rk_addtend_dry( ru_tend,rv_tend,rw_tend,ph_tend,t_tend,ru_tendf,rv_tendf,rw_tendf,ph_tendf,t_tendf,u_save,v_save,w_save,&
&ph_save,t_save,rk_step,h_diabatic,mut,msft,msfu,msfv,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (config_flags%specified .or. config_flags%nested) then
! TBH:  OVERWRITE ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call spec_bdy_dry( config_flags,ru_tend,rv_tend,ph_tend,t_tend,rw_tend,mu_tend,u_b,v_b,ph_b,t_b,w_b,mu_b,u_bt,v_bt,ph_bt,&
&t_bt,w_bt,mu_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
&grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif

! TBH:  OVERWRITE u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mu_1,mu_2,muus,muvs,muts,mudf,u_save,v_save,w_save,t_save,ph_save,mu_save,c2a,ww1
! TBH:  HORIZONTAL DEPENDENCE:  mu_1 ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  mub  ( [i-1,i], [j-1,j] )
    call small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,mub,mu_1,mu_2,muu,muus,muv,muvs,mut,muts,mudf,u_save,v_save,&
&w_save,t_save,ph_save,mu_save,ww,ww1,dnw,c2a,pb,p,alt,msfu,msfv,msft,rk_step,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
! TBH:  OVERWRITE al,p,ph_2,pm1
! TBH:  HORIZONTAL DEPENDENCE:  NONE
    call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,0,ids,ide,jds,jde,kds,kde,&
&ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if (non_hydrostatic) then
! TBH:  OVERWRITE a,alpha,gamma
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call calc_coef_w( a,alpha,gamma,mut,cqw,rdn,rdnw,c2a,dts,g,epssm,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp7)

BENCH_START(exch7)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_0718.inc"
! TBH:  OVERWRITE al,mu_1,mu_2,p,pb,ph_2,php,t_1,t_save,u_save,v_save
#endif
BENCH_END(exch7)

! TBH:  OVERWRITE BEGIN small_step LOOP

  small_stepu: do iteration = 1, number_of_small_timesteps
BENCH_START(comp8)
! TBH:  OVERWRITE u_2,v_2
! TBH:  HORIZONTAL DEPENDENCE:  mudf ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  ph_2 ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  alt  ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  al   ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  p    ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  pb   ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  php  ( [i-1,i], [j-1,j] )
! TBH:  HORIZONTAL DEPENDENCE:  mu_2 ( [i-1,i], [j-1,j] )
      call advance_uv( u_2,ru_tend,v_2,rv_tend,p,pb,ph_2,php,alt,al,mu_2,muu,cqu,muv,cqv,mudf,rdx,rdy,dts,cf1,cf2,cf3,fnm,fnp,&
&emdiv,rdnw,config_flags,spec_zone,non_hydrostatic,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%&
&i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (config_flags%specified .or. config_flags%nested) then
! TBH:  OVERWRITE u_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call spec_bdyupdate( u_2,ru_tend,dts_rk,'u',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
! TBH:  OVERWRITE v_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call spec_bdyupdate( v_2,rv_tend,dts_rk,'v',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
BENCH_END(comp8)

BENCH_START(exch8)
#ifdef DM_PARALLEL
lineno = __LINE__
#     include "HALO_0736.inc"
! TBH:  OVERWRITE u_2,v_2
#endif
BENCH_END(exch8)

BENCH_START(comp9)
! TBH:  OVERWRITE ww,ww1,mu_2,muave,muts,mudf,ru_m,rv_m,ww_m,t_2,t_2save
! TBH:  HORIZONTAL DEPENDENCE:  u_2    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  u_save ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  muu    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu   ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v_save ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  muv    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv   ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  t_save ( [i-1,i,i+1], [j-1,j,j+1] )
      call advance_mu_t( ww,ww1,u_2,u_save,v_2,v_save,mu_2,mut,muave,muts,muu,muv,mudf,ru_m,rv_m,ww_m,t_2,t_save,t_2save,t_tend,&
&mu_tend,rdx,rdy,dts,epssm,dnw,fnm,fnp,rdnw,msfu,msfv,msft,iteration,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,&
&kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp9)

BENCH_START(exch9)
#ifdef DM_PARALLEL
lineno = __LINE__
#include "HALO_0747.inc"  ! 48pt!  
! TBH:  OVERWRITE al,alt,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww




#endif
BENCH_END(exch9)

BENCH_START(comp10)
      if (config_flags%specified .or. config_flags%nested) then
! TBH:  OVERWRITE t_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call spec_bdyupdate( t_2,t_tend,dts_rk,'t',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,&
&jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
! TBH:  OVERWRITE mu_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call spec_bdyupdate( mu_2,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
! TBH:  OVERWRITE muts
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call spec_bdyupdate( muts,mu_tend,dts_rk,'m',config_flags,spec_zone,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,ips,ipe,jps,&
&jpe,1,1,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),1,1 )
      endif

! TBH:  OVERWRITE ru_m,rv_m,ww_m
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call sumflux( u_2,v_2,ww,u_save,v_save,ww1,muu,muv,ru_m,rv_m,ww_m,epssm,msfu,msfv,iteration,number_of_small_timesteps,ids,&
&ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      if (non_hydrostatic) then
! TBH:  OVERWRITE w_2,t_2save,ph_2
! TBH:  HORIZONTAL DEPENDENCE:  u_2    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ht     ( [i-1,i,i+1], [j-1,j,j+1] )
        call advance_w( w_2,rw_tend,ww,u_2,v_2,mu_2,mut,muave,muts,t_2save,t_2,t_save,ph_2,ph_save,phb,ph_tend,ht,c2a,cqw,alt,alb,&
&a,alpha,gamma,rdx,rdy,dts,t0,epssm,dnw,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
      endif
      if (config_flags%specified .or. config_flags%nested) then
        if (non_hydrostatic) then
! TBH:  OVERWRITE ph_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
          call spec_bdyupdate_ph( ph_save,ph_2,ph_tend,mu_tend,muts,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,&
&ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          if (config_flags%specified) then
! TBH:  OVERWRITE w_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call zero_grad_bdy( w_2,'w',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,&
&kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else
! TBH:  OVERWRITE w_2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdyupdate( w_2,rw_tend,dts_rk,'h',config_flags,spec_zone,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,&
&ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
      endif
! TBH:  OVERWRITE al,p,ph_2,pm1
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      call calc_p_rho( al,p,ph_2,alt,t_2,t_save,c2a,pm1,mu_2,muts,znu,t0,rdnw,dnw,smdiv,non_hydrostatic,iteration,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
BENCH_END(comp10)

BENCH_START(exch10)
#ifdef DM_PARALLEL
lineno = __LINE__
#      include "HALO_0790.inc"
! TBH:  OVERWRITE al,mu_2,mudf,muts,muus,muvs,p,ph_2
! TBH:  $$$ why update halo of muus here?  
! TBH:  $$$ why update halo of muvs here?  
#endif
BENCH_END(exch10)

  end do small_stepu

! TBH:  OVERWRITE END small_step LOOP

BENCH_START(comp11)
! TBH:  OVERWRITE muus,muvs
! TBH:  HORIZONTAL DEPENDENCE:  muts ( [i-1,i], [j-1,j] )
    call calc_mu_uv_1( config_flags,muts,muus,muvs,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),&
&grid%j_start(ij),grid%j_end(ij),k_start,k_end )

! TBH:  OVERWRITE u_2,v_2,w_2,t_2,ww,ph_2,mu_2,mu_1
! TBH:  HORIZONTAL DEPENDENCE:  NONE
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
BENCH_END(comp11)

BENCH_START(exch11)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_0821.inc"
! TBH:  OVERWRITE ru_m,rv_m
#endif
BENCH_END(exch11)

BENCH_START(comp12)
  moist_scalar_advancg: if (num_3d_m .ge. param_first_scalar) then
    moist_variable_loor: do im = param_first_scalar, num_3d_m
! TBH:  OVERWRITE moist_tend(:,:,:,im),rqvften
! TBH:  HORIZONTAL DEPENDENCE:  moist_2 ( [i-3,i,i+3], [j-3,j,j+3] )
! TBH:  HORIZONTAL DEPENDENCE:  ru_m    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  rv_m    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  msfu    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  msfv    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  xkmhd   ( [i-1,i,i+1], [j-1,j,j+1] )
        call rk_scalar_tend( im,im,config_flags,rk_step,dt_rk,ru_m,rv_m,ww_m,mut,alt,moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,&
&im),moist_tend(ims,kms,jms,im),advect_tend,rqvften,qv_base, .true. ,fnm,fnp,msfu,msfv,msft,rdx,rdy,rdn,rdnw,khdif,kvdif,&
&xkmhd,leapfrog,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%&
&j_end(ij),k_start,k_end )
        if ((config_flags%specified .or. config_flags%nested) .and. rk_step .eq. 1) then
          if (im .eq. p_qv) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qv)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqv_b,rqv_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
        if (config_flags%nested .and. rk_step .eq. 1) then
          if (im .eq. p_qc) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qc)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqc_b,rqc_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qr) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qr)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqr_b,rqr_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qi) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qi)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqi_b,rqi_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qs) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qs)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqs_b,rqs_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          else if (im .eq. p_qg) then
! TBH:  OVERWRITE moist_tend(:,:,:,p_qg)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            call spec_bdy_scalar( moist_tend(ims,kms,jms,im),rqg_b,rqg_bt,spec_bdy_width,spec_zone,ijds,ijde,ids,ide,jds,jde,kds,&
&kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
          endif
        endif
! TBH:  OVERWRITE moist_1(:,:,:,im),moist_2(:,:,:,im)
! TBH:  HORIZONTAL DEPENDENCE:  NONE
        call rk_update_scalar( im,im,moist_1(ims,kms,jms,im),moist_2(ims,kms,jms,im),moist_tend(ims,kms,jms,im),advect_tend,msft,&
&mu_1,mu_2,mub,rk_step,dt_rk,spec_zone,epsts,leapfrog,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,grid%&
&i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    end do moist_variable_loor
  endif moist_scalar_advancg

! TBH:  OVERWRITE al,ph_2,p
! TBH:  HORIZONTAL DEPENDENCE:  NONE
    call calc_p_rho_phi( moist_2,num_3d_m,al,alb,mu_2,muts,ph_2,p,pb,t_2,p0,t0,znu,dnw,rdnw,rdn,non_hydrostatic,ids,ide,jds,jde,&
&kds,kde,ims,ime,jms,jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    if ( .not. non_hydrostatic) then
! TBH:  NOTE:  non_hydrostatic=.true. so this is never executed
! TBH:  OVERWRITE w_2
! TBH:  HORIZONTAL DEPENDENCE:  u_2    ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  v_2    ( [    i    ], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  ht     ( [   ,i,i+1], [j-1,j,j+1] )
      call diagnose_w( ph_tend,ph_2,ph_1,w_2,muts,dt_rk,u_2,v_2,ht,cf1,cf2,cf3,rdx,rdy,msft,ids,ide,jds,jde,kds,kde,ims,ime,jms,&
&jme,kms,kme,grid%i_start(ij),grid%i_end(ij),grid%j_start(ij),grid%j_end(ij),k_start,k_end )
    endif
BENCH_END(comp12)

BENCH_START(exch12)
#ifdef DM_PARALLEL
lineno = __LINE__
#  include "HALO_0871.inc"
! TBH:   8pt OVERWRITE mudf,muts,muus,muvs,p
! TBH:  $$$ why update halo of muts here?  
! TBH:  $$$ why update halo of muus here?  
! TBH:  $$$ why update halo of muvs here?  
! TBH:  48pt OVERWRITE al,moist_1,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
#endif
BENCH_END(exch12)

end do runge_kutta_looq
