SUBROUTINE DA_Transfer_XatoWRFTL( grid, config_flags, filnam, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Convert analysis increments into WRFTL increments 
!           (based on an early subroutine by Yong-Run Guo and Wei Huang)
!                                                Hans Huang, 10 August 2005
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
   real    :: theta, rho_cgrid

!---------------------------------------------------------------------------

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: g_press

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
      g_moist_2(i,j,k,P_QV) = xa%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [2.0] COMPUTE increments of dry-column air mass per unit area
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         sdmd=sdmd+g_moist_2(i,j,k,P_QV)*dnw(k)
         s1md=s1md+(1.0+moist_2(i,j,k,P_QV))*dnw(k)
      enddo

      g_mu_2(i,j)=-(xa%psfc(i,j)+xb%psac(i,j)*sdmd)/s1md
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] COMPUTE pressure increments (for computing theta increments)
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      g_press(i,j,ke+1)=0.0

      do k=ke,ks,-1
         g_press(i,j,k)=g_press(i,j,k+1) &
                 -(g_mu_2(i,j)*(1.0+moist_2(i,j,k,P_QV)) &
                 +(mu_2(i,j)+mub(i,j))*g_moist_2(i,j,k,P_QV))*dn(k)

         xa%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))

      enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [4.0] CONVERT TEMPERATURE INCREMENTS INTO THETA INCREMENTS
!        EVALUATE ALSO THE INCREMENTS OF (1/rho) AND GEOPOTENTIAL
!------------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke
         do i=is,ie
            theta = 300.0 + t_2(i,j,k)
            g_t_2(i,j,k) = theta * ( xa%t(i,j,k) / xb%t(i,j,k) &
                           - kappa * xa%p(i,j,k) / xb%p(i,j,k) )
         enddo
      enddo
   enddo
   do j=js,je
      g_ph_2(is:ie,j,ks)=0.
      do k=ks,ke
      do i=is,ie
          rho_cgrid = xb%rho(i,j,k) &
                     *(xa%p(i,j,k)/xb%p(i,j,k) &
                     - xa%t(i,j,k)/xb%t(i,j,k) &
                     - 0.61*xa%q(i,j,k)/(1.+0.61*xb%q(i,j,k)))
         g_ph_2(i,j,k+1) = g_ph_2(i,j,k) &
                          -(g_press(i,j,k+1)-g_press(i,j,k) &
                          +(ph_2(i,j,k+1)-ph_2(i,j,k))*rho_cgrid) &
                          /xb%rho(i,j,k)
      enddo
      enddo
   enddo


!------------------------------------------------------------------------------
!  [5.0] CONVERT FROM A-GRID TO C-GRID
!------------------------------------------------------------------------------

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!--The southern boundary (fill A-GRID boundaries)
!--To keep the gradient, A(0) = 2A(1)-A(2)
   if(js == xp%jds) then
      xa%v(is:ie,js-1,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                            -    xa%v(is:ie,js+1,ks:ke)
   endif

!--The western boundary (fill A-GRID boundaries)
!--To keep the gradient, A(0) = 2A(1)-A(2)
   if(is == xp%ids) then
      xa%u(is-1,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                            -    xa%u(is+1,js:je,ks:ke)
   endif

   do j=js,je
   do k=ks,ke
   do i=is,ie
      g_u_2(i,j,k)=0.5*(xa%u(i-1,j  ,k)+xa%u(i,j,k))
      g_v_2(i,j,k)=0.5*(xa%v(i  ,j-1,k)+xa%v(i,j,k))
   enddo
   enddo
   enddo

!--The northern boundary (compute C-GRID boundaries)
!--To keep the gradient, A(N+1) = 2A(N)-A(N-1)
!--and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2
   if(je == xp%jde) then
      g_v_2(is:ie,je+1,ks:ke)=(3.0*xa%v(is:ie,je  ,ks:ke) &
                               -   xa%v(is:ie,je-1,ks:ke)) /2.
   endif

!--The eastern boundary (compute C-GRID boundaries)
!--To keep the gradient, A(N+1) = 2A(N)-A(N-1)
!--and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2
   if(ie == xp%ide) then
      g_u_2(ie+1,js:je,ks:ke)=(3.0*xa%u(ie  ,js:je,ks:ke) &
                               -   xa%u(ie-1,js:je,ks:ke)) /2.
   endif

#else

   do k=ks,ke
      do j=js,je
      do i=is,ie
         g_u_2(i,j,k)=0.5*(xa%u(i-1,j,k)+xa%u(i,j,k))
         g_v_2(i,j,k)=0.5*(xa%v(i,j-1,k)+xa%v(i,j,k))
      enddo
      enddo
   enddo

!--To keep the gradient, A(N+1) = 2A(N)-A(N-1)
!--and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2

!--The eastern boundary
   g_u_2(ie+1,js:je,ks:ke)=(3.0*xa%u(ie  ,js:je,ks:ke) &
                            -   xa%u(ie-1,js:je,ks:ke)) /2.

!--The northern boundary
   g_v_2(is:ie,je+1,ks:ke)=(3.0*xa%v(is:ie,je  ,ks:ke) &
                            -    xa%v(is:ie,je-1,ks:ke)) /2.

!--To keep the gradient, A(0) = 2A(1)-A(2)
!--and on C-Grid, this will lead to C(1)=(A(0)+A(1))/2=(3A(1)-A(2))/2

!--The western boundary
   g_u_2(is,js:je,ks:ke)=(3.0*xa%u(is  ,js:je,ks:ke) &
                          -   xa%u(is+1,js:je,ks:ke)) /2.

!--The southern boundary
   g_v_2(is:ie,js,ks:ke)=(3.0*xa%v(is:ie,js  ,ks:ke) &
                          -   xa%v(is:ie,js+1,ks:ke)) /2.
#endif

!------------------------------------------------------------------------------
!  [6.0] SAVE OTHERINCREMENT 
!------------------------------------------------------------------------------

      do j=js,je

      do k=ks,ke+1
      do i=is,ie
         g_w_2(i,j,k)  = xa%w(i,j,k)
      enddo
      enddo

      enddo

#if 0
! should be considered later
      if ( size(moist_2,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist_2(i,j,k,p_qcw) =  xa%qcw(i,j,k)
         g_moist_2(i,j,k,p_qrn) =  xa%qrn(i,j,k)
      enddo
      enddo
      enddo
      endif

      if ( size(moist_2,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist_2(i,j,k,p_qci) =  xa%qci(i,j,k)
         g_moist_2(i,j,k,p_qsn) =  xa%qsn(i,j,k)
      enddo
      enddo
      enddo
      endif

      if ( size(moist_2,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist_2(i,j,k,p_qgr) =  xa%qgr(i,j,k)
      enddo
      enddo
      enddo
      endif
#endif

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

