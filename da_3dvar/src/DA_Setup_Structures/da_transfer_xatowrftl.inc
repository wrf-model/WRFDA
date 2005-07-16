SUBROUTINE DA_Transfer_XatoWRFTL( grid, config_flags, filnam, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Convert analysis increments into WRFTL increments 
!           (following xatowrf, but only keep the increments)
!                                                Hans Huang, 8 March 2005
!           
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md

!---------------------------------------------------------------------------

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!=>ARRAYS TO HOLD WRF INCREMENTS ON THE C-grid 

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: u_cgrid,  v_cgrid, &
                                                                 p_cgrid

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme)               :: mu_cgrid

   REAL    :: t_full  , p_full, rho_full, q_full, ph_full , ph_xb_hd, &
              qvf1, qvf2, qvf1_b, qvf2_b

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: new_t_2, new_ph_2

   integer ndynopt

#ifdef WRFVAR

!---------------------------------------------------------------------------
   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

!------------------------------------------------------------------------------
!  [1.0] Get the mixing ratio of moisture first, as it is easy.
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      g_moist(i,j,k,P_QV) = xa%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
   enddo
   enddo
   enddo

#if 0
   print *, 'xa%q(41,1,1)=', xa%q(41,1,1)
   print *, 'xa%q(42,1,1)=', xa%q(42,1,1)

   print *, 'xb%q(41,1,1)=', xb%q(41,1,1)
   print *, 'xb%q(42,1,1)=', xb%q(42,1,1)
#endif

!------------------------------------------------------------------------------
!  [2.0] COMPUTE increments of dry-column air mass per unit area
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         sdmd=sdmd+g_moist(i,j,k,P_QV)*dnw(k)
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*dnw(k)
      enddo

      mu_cgrid(i,j)=-(xa%psfc(i,j)+xb%psac(i,j)*sdmd)/s1md
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] COMPUTE pressure increments 
!------------------------------------------------------------------------------

! Tangent linear code for p_cgrid (based on WRF "real.init.code") 
! developed by Y.-R. Guo 05/13/2004:

   do j=js,je
   do i=is,ie
     k = ke
     qvf1   = g_moist(i,j,k,P_QV)
     qvf1_b = moist(i,j,k,P_QV)
     qvf2   = - qvf1 / ((1.+qvf1_b)*(1.+qvf1_b))
     qvf2_b = 1./(1.+qvf1)
     qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
     qvf1_b = qvf1_b*qvf2_b
     p_cgrid(i,j,k) = (-0.5/rdnw(k)) * &
                ( (mu_cgrid(i,j)+qvf1*mub(i,j)) / qvf2_b &
                 -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b) )

     do k = ke-1,1,-1
       qvf1   = 0.5*(g_moist(i,j,k,P_QV)+g_moist(i,j,k,P_QV))
       qvf1_b = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k+1,P_QV))
       qvf2   = - qvf1 / ((1.+qvf1_b)*(1.+qvf1_b))
       qvf2_b = 1./(1.+qvf1_b)
       qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
       qvf1_b = qvf1_b*qvf2_b
       p_cgrid(i,j,k) = p_cgrid(i,j,k+1)  &
                  - (1./rdn(k+1)) * &
                  ( (mu_cgrid(i,j)+qvf1*mub(i,j)) / qvf2_b &
                   -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b) )
     enddo
   enddo
   enddo

! ----------------------------------------------------------------------------

!==> CONVERT FROM A-GRID TO C-GRID

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

   do j=js,je
   do k=ks,ke
      do i=is,ie
         u_cgrid(i,j,k)=0.5*(xa%u(i-1,j  ,k)+xa%u(i,j,k))
         v_cgrid(i,j,k)=0.5*(xa%v(i  ,j-1,k)+xa%v(i,j,k))
      enddo
   enddo
   enddo

!--Fill the boundary

!--The southern boundary
   if(js == xp%jds) then
      v_cgrid(is:ie,js,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                             -    xa%v(is:ie,js+1,ks:ke)
   endif

!--The northern boundary
   if(je == xp%jde) then
      v_cgrid(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                               -    xa%v(is:ie,je-1,ks:ke)
   endif

!--The western boundary
   if(is == xp%ids) then
      u_cgrid(is,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                             -    xa%u(is+1,js:je,ks:ke)
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      u_cgrid(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                               -    xa%u(ie-1,js:je,ks:ke)
   endif

#else

!--The eastern boundary
   u_cgrid(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                            -    xa%u(ie-1,js:je,ks:ke)

!--The northern boundary
   v_cgrid(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                            -    xa%v(is:ie,je-1,ks:ke)

   do k=ks,ke
      do j=js,je
      do i=is+1,ie
         u_cgrid(i,j,k)=0.5*(xa%u(i-1,j,k)+xa%u(i,j,k))
      enddo
      enddo

      do j=js+1,je
      do i=is,ie
         v_cgrid(i,j,k)=0.5*(xa%v(i,j-1,k)+xa%v(i,j,k))
      enddo
      enddo
   enddo

!--The western boundary
   u_cgrid(is,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                          -    xa%u(is+1,js:je,ks:ke)

!--The southern boundary
   v_cgrid(is:ie,js,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                          -    xa%v(is:ie,js+1,ks:ke)
#endif

!------------------------------------------------------------------------------
!  [4.0] CONVERT TEMPERATURE INCREMENTS INTO THETA INCREMENTS 
!        EVALUATE ALSO THE INCREMENTS OF (1/rho) AND GEOPOTENTIAL
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie

     ph_full  = ht(i,j) * gravity
     ph_xb_hd = ht(i,j) * gravity

     new_ph_2(i,j,ks) = 0.

     do k = ks, ke
! To obtain all of the full fields: t, p, q(mixing ratio), rho

      t_full   = xa%t(i,j,k) + xb%t(i,j,k)
      p_full   = p_cgrid(i,j,k) + xb%p(i,j,k)
      q_full   = moist(i,j,k,P_QV) + g_moist(i,j,k,P_QV)

! Note: According to WRF, this is the dry air density used to
!       compute the geopotential height: 
      rho_full = p_full / (gas_constant*t_full*(1.0+q_full/rd_over_rv))

! To compute the theta increment with the full fields:

        new_t_2(i,j,k) = t_full*(ps0/p_full)**kappa - ts0

! The full field of analysis ph:
         ph_full  = ph_full  &
                  - xb%dnw(k) * (xb%psac(i,j)+mu_cgrid(i,j)) / rho_full
!
! background hydrostatic phi:
         ph_xb_hd  = ph_xb_hd  &
                  - xb%dnw(k) * xb%psac(i,j) / xb%rho(i,j,k)
    
! The analysis perturbation = Hydro_ph - base_ph + nonhydro_xb_ph:

         new_ph_2(i,j,k+1) = ph_full - phb(i,j,k+1) &
                           + (xb%hf(i,j,k+1)*gravity - ph_xb_hd)

     enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [5.0] SAVE INCREMENT 
!------------------------------------------------------------------------------

      do j=js,je
      do i=is,ie
         g_mu_2(i,j) =  mu_cgrid(i,j)
         g_w_2(i,j,ke+1)=   xa%w(i,j,ke+1)
         g_ph_2(i,j,ke+1)=  new_ph_2(i,j,ke+1) - ph_2(i,j,ke+1)
      enddo

      do k=ks,ke
      do i=is,ie
         g_u_2(i,j,k) =  u_cgrid(i,j,k)
         g_v_2(i,j,k) =  v_cgrid(i,j,k)
         g_t_2(i,j,k) =  new_t_2(i,j,k) - t_2(i,j,k)
         g_ph_2(i,j,k)=  new_ph_2(i,j,k) - ph_2(i,j,k)
         g_w_2(i,j,k) =     xa%w(i,j,k)
      enddo
      enddo
      enddo

#if 0
      if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qcw) =  xa%qcw(i,j,k)
         g_moist(i,j,k,p_qrn) =  xa%qrn(i,j,k)
      enddo
      enddo
      enddo
      endif

      if ( size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qci) =  xa%qci(i,j,k)
         g_moist(i,j,k,p_qsn) =  xa%qsn(i,j,k)
      enddo
      enddo
      enddo
      endif

      if ( size(moist,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qgr) =  xa%qgr(i,j,k)
      enddo
      enddo
      enddo
      endif
#endif

!--The northern boundary
   if(je == xp%jde) then
      j=je+1
      do k=ks,ke
      do i=is,ie
         g_v_2(i,j,k) =  v_cgrid(i,j,k)
      enddo
      enddo
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      i=ie+1
      do k=ks,ke
      do j=js,je
         g_u_2(i,j,k) =  u_cgrid(i,j,k)
      enddo
      enddo
   endif

!------------------------------------------------------------------------------
!  [6.0] OUTPUT
!------------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt ( 1 , DYN_EM_TL )

   call med_initialdata_output_3dvar( grid , config_flags, filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

#endif

END SUBROUTINE DA_Transfer_XatoWRFTL

