SUBROUTINE DA_Transfer_XatoWRFTL_Adj( grid, config_flags, filnam, &
#include <em_dummy_args.inc>
                              )

   !---------------------------------------------------------------------------
   ! PURPOSE: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

!  Local variables

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: rho_cgrid

!---------------------------------------------------------------------------

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: a_press

   integer ndynopt

   IF (trace_use) CALL da_trace_entry("da_transfer_xatowrftl_adj")


#ifdef VAR4DPOSS

   is=xp%its
   ie=xp%ite
   js=xp%jts
   je=xp%jte
   ks=xp%kts
   ke=xp%kte

!------------------------------------------------------------------------------
!  [7.0] Adjoint of OUTPUT (INPUT)
!------------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_AD
   call nl_set_dyn_opt ( 1 , DYN_EM_AD )

   call med_initialdata_input_3dvar( grid , config_flags, in_filename=filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

!------------------------------------------------------------------------------
!  [6.0] Adjoint of SAVE OTHERINCREMENT
!------------------------------------------------------------------------------

   do k=ks,ke+1
   do j=js,je
   do i=is,ie
      xa%w(i,j,k)=a_w_2(i,j,k)
   enddo
   enddo
   enddo

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
   if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qcw(i,j,k)=a_moist(i,j,k,p_qcw)
         xa%qrn(i,j,k)=a_moist(i,j,k,p_qrn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qci(i,j,k)=a_moist(i,j,k,p_qci)
         xa%qsn(i,j,k)=a_moist(i,j,k,p_qsn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qgr(i,j,k)=a_moist(i,j,k,p_qgr) 
      enddo
      enddo
      enddo
   endif

#endif


!------------------------------------------------------------------------------
!  [5.0] Adjoint of CONVERT FROM A-GRID TO C-GRID
!------------------------------------------------------------------------------

!--Fill the halo region for u and v.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%u(i,j,k)=0.5*(a_u_2(i+1,j  ,k)+a_u_2(i,j,k))
      xa%v(i,j,k)=0.5*(a_v_2(i  ,j+1,k)+a_v_2(i,j,k))
   enddo
   enddo
   enddo

#ifdef DM_PARALLEL

!--The western boundary
   if(is == xp%ids) then
      xa%u(is  ,js:je,ks:ke)=xa%u(is  ,js:je,ks:ke)+a_u_2(is,js:je,ks:ke)
      xa%u(is+1,js:je,ks:ke)=xa%u(is+1,js:je,ks:ke)-a_u_2(is,js:je,ks:ke)/2.
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      xa%u(ie  ,js:je,ks:ke)=xa%u(ie  ,js:je,ks:ke)+a_u_2(ie+1,js:je,ks:ke)
      xa%u(ie-1,js:je,ks:ke)=xa%u(ie-1,js:je,ks:ke)-a_u_2(ie+1,js:je,ks:ke)/2.
   endif

!--The southern boundary
   if(js == xp%jds) then
      xa%v(is:ie,js  ,ks:ke)=xa%v(is:ie,js  ,ks:ke)+a_v_2(is:ie,js,ks:ke)
      xa%v(is:ie,js+1,ks:ke)=xa%v(is:ie,js+1,ks:ke)-a_v_2(is:ie,js,ks:ke)/2.
   endif

!--The northern boundary
   if(je == xp%jde) then
      xa%v(is:ie,je  ,ks:ke)=xa%v(is:ie,je  ,ks:ke)+a_v_2(is:ie,je+1,ks:ke)
      xa%v(is:ie,js-1,ks:ke)=xa%v(is:ie,je-1,ks:ke)-a_v_2(is:ie,je+1,ks:ke)/2.
   endif

#else

!--The western boundary
      xa%u(is  ,js:je,ks:ke)=xa%u(is  ,js:je,ks:ke)+a_u_2(is,js:je,ks:ke)
      xa%u(is+1,js:je,ks:ke)=xa%u(is+1,js:je,ks:ke)-a_u_2(is,js:je,ks:ke)/2.

!--The eastern boundary
      xa%u(ie  ,js:je,ks:ke)=xa%u(ie  ,js:je,ks:ke)+a_u_2(ie+1,js:je,ks:ke)
      xa%u(ie-1,js:je,ks:ke)=xa%u(ie-1,js:je,ks:ke)-a_u_2(ie+1,js:je,ks:ke)/2.

!--The southern boundary
      xa%v(is:ie,js  ,ks:ke)=xa%v(is:ie,js  ,ks:ke)+a_v_2(is:ie,js,ks:ke)
      xa%v(is:ie,js+1,ks:ke)=xa%v(is:ie,js+1,ks:ke)-a_v_2(is:ie,js,ks:ke)/2.

!--The northern boundary
      xa%v(is:ie,je  ,ks:ke)=xa%v(is:ie,je  ,ks:ke)+a_v_2(is:ie,je+1,ks:ke)
      xa%v(is:ie,js-1,ks:ke)=xa%v(is:ie,je-1,ks:ke)-a_v_2(is:ie,je+1,ks:ke)/2.

#endif

!------------------------------------------------------------------------------
!  [4.0] Adjoint of CONVERT TEMPERATURE INCREMENTS INTO THETA INCREMENTS
!        EVALUATE ALSO THE INCREMENTS OF (1/rho) AND GEOPOTENTIAL
!------------------------------------------------------------------------------

   a_press(is:ie,js:je,ks:ke+1)=0.
   do k=ke,ks,-1
   do j=js,je
   do i=is,ie
      rho_cgrid=-(ph_2(i,j,k+1)-ph_2(i,j,k))*a_ph_2(i,j,k+1)/xb%rho(i,j,k)
      a_press(i,j,k  )=a_press(i,j,k  )+a_ph_2(i,j,k+1)/xb%rho(i,j,k)
      a_press(i,j,k+1)=a_press(i,j,k+1)-a_ph_2(i,j,k+1)/xb%rho(i,j,k)
      a_ph_2(i,j,k  ) =a_ph_2(i,j,k)   +a_ph_2(i,j,k+1)
      xa%q(i,j,k)=-xb%rho(i,j,k)*0.61*rho_cgrid/(1.+0.61*xb%q(i,j,k))
      xa%t(i,j,k)=-xb%rho(i,j,k)*rho_cgrid/xb%t(i,j,k)
      xa%p(i,j,k)= xb%rho(i,j,k)*rho_cgrid/xb%p(i,j,k)
   enddo
   enddo
   enddo

   do k=ks,ke
   do j=js,je
   do i=is,ie 
      xa%p(i,j,k)=xa%p(i,j,k)-(300.0+t_2(i,j,k))*kappa*a_t_2(i,j,k)/xb%p(i,j,k)
      xa%t(i,j,k)=xa%t(i,j,k)+(300.0+t_2(i,j,k))*a_t_2(i,j,k)/xb%t(i,j,k)
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] Adjoint of COMPUTE pressure increments (for computing theta increments)
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      a_press(i,j,k+1)=a_press(i,j,k+1)+0.5*xa%p(i,j,k)
      a_press(i,j,k  )=a_press(i,j,k  )+0.5*xa%p(i,j,k)
      xa%p(i,j,k)=0.
      a_moist(i,j,k,P_QV)=a_moist(i,j,k,P_QV)-(mu_2(i,j)+mub(i,j))*a_press(i,j,k)*dn(k)
      a_mu_2(i,j)=a_mu_2(i,j)-a_press(i,j,k)*(1.0+moist(i,j,k,P_QV))*dn(k)
      a_press(i,j,k+1)=a_press(i,j,k+1)+a_press(i,j,k)
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [2.0] Adjoint of COMPUTE increments of dry-column air mass per unit area
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*dnw(k)
      enddo
      sdmd=sdmd-xb%psac(i,j)*a_mu_2(i,j)/s1md
      xa%psfc(i,j)=-a_mu_2(i,j)/s1md
      do k=ks,ke
         a_moist(i,j,k,P_QV)=a_moist(i,j,k,P_QV)+sdmd*dnw(k)
      enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [1.0] Adjoint of Get the mixing ratio of moisture 
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%q(i,j,k)=xa%q(i,j,k)+a_moist(i,j,k,P_QV)/(1.0-xb%q(i,j,k))**2
   enddo
   enddo
   enddo

#endif  

   IF (trace_use) CALL da_trace_exit("da_transfer_xatowrftl_adj")


END SUBROUTINE DA_Transfer_XatoWRFTL_Adj

