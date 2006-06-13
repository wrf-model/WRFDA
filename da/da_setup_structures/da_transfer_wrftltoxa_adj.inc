SUBROUTINE da_transfer_wrftltoxa_adj( grid, config_flags, filnam, &
#include "em_dummy_args.inc"
                              )

   !---------------------------------------------------------------------------
   ! PURPOSE: Convert analysis increments into WRFAD increments 
   !          (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include "em_dummy_decl.inc"

   integer :: i, j, k, n, ndynopt
   integer :: is, ie, js, je, ks, ke

   real    :: sdmd, s1md, qvf1, qvf2, qvf1_b, qvf2_b
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: g_press

   IF (trace_use) CALL da_trace_entry("da_transfer_wrftltoxa_adj")

!---------------------------------------------------------------------------

   is=xp%its
   ie=xp%ite
   js=xp%jts
   je=xp%jte
   ks=xp%kts
   ke=xp%kte

!------------------------------------------------------------------------------
!  [6.0] ALL THE SIMPLE ONES
!------------------------------------------------------------------------------

   do k=ks,ke+1
   do j=js,je
   do i=is,ie
      g_w_2(i,j,k)=xa%w(i,j,k)
   enddo
   enddo
   enddo

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
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


! ----------------------------------------------------------------------------
![5.0] CONVERT FROM C-GRID TO A-GRID
! ----------------------------------------------------------------------------

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!--The western boundary
   if(is == xp%ids) xa%u(is-1,js:je,ks:ke)=0.

!--The southern boundary
   if(js == xp%jds) xa%v(is:ie,js-1,ks:ke)=0.

   do k=ks,ke
   do j=js,je
   do i=is,ie
      g_u_2(i,j,k)=0.5*(xa%u(i-1,j  ,k)+xa%u(i,j,k))
      g_v_2(i,j,k)=0.5*(xa%v(i  ,j-1,k)+xa%v(i,j,k))
   enddo
   enddo
   enddo

!--The eastern boundary
   if(ie == xp%ide) g_u_2(ie+1,js:je,ks:ke)=xa%u(ie,js:je,ks:ke)/2.

!--The northern boundary
   if(je == xp%jde) g_v_2(is:ie,je+1,ks:ke)=xa%v(is:ie,je,ks:ke)/2.

#else

   do k=ks,ke
      do j=js,je
      do i=is+1,ie
         g_u_2(i,j,k)=0.5*(xa%u(i-1,j,k)+xa%u(i,j,k))
      enddo
      enddo
      do j=js+1,je
      do i=is,ie
         g_v_2(i,j,k)=0.5*(xa%v(i,j-1,k)+xa%v(i,j,k))
      enddo
      enddo
   enddo

!--The western boundary
   g_u_2(is,js:je,ks:ke)=xa%u(is,js:je,ks:ke)/2.

!--The eastern boundary
   g_u_2(ie+1,js:je,ks:ke)=xa%u(ie,js:je,ks:ke)/2.

!--The southern boundary
   g_v_2(is:ie,js,ks:ke)=xa%v(is:ie,js,ks:ke)/2.

!--The northern boundary
   g_v_2(is:ie,je+1,ks:ke)=xa%v(is:ie,je,ks:ke)/2.

#endif

!------------------------------------------------------------------------------
!  [4.0] CONVERT THETA INCREMENTS TO T INCREMENTS
!------------------------------------------------------------------------------

! In the inverse, g_ph information is lost. This should be investigated later!!!
! However, in this adjoint problem, a_ph should be set to 0. Otherwise, a_ph 
! will be initialized randomly!

   do k=ks,ke+1
   do j=js,je
   do i=is,ie
      g_ph_2(i,j,k)=0.
   enddo
   enddo
   enddo

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%p(i,j,k)=xb%t(i,j,k)*kappa*xa%t(i,j,k)/xb%p(i,j,k)
      g_t_2(i,j,k)=xb%t(i,j,k)*xa%t(i,j,k)/(300.0+t_2(i,j,k))
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] COMPUTE pressure increments 
!------------------------------------------------------------------------------

   g_press(is:ie,js:je,ks:ke+1)=0.
   do k=ks,ke
   do j=js,je
   do i=is,ie
      g_press(i,j,k+1)=g_press(i,j,k+1)+0.5*xa%p(i,j,k)
      g_press(i,j,k  )=g_press(i,j,k  )+0.5*xa%p(i,j,k)
      g_moist(i,j,k,P_QV)=-(mu_2(i,j)+mub(i,j))*g_press(i,j,k)*dn(k)
      g_mu_2(i,j)=-g_press(i,j,k)*(1.0+moist(i,j,k,P_QV))*dn(k)
      g_press(i,j,k+1)=g_press(i,j,k+1)+g_press(i,j,k)
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [2.0] COMPUTE psfc increments from mu-increments
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      s1md=0.0
      do k=ks,ke
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*dnw(k)
      enddo
      g_mu_2(i,j)=g_mu_2(i,j)-xa%psfc(i,j)*s1md
      sdmd=-xb%psac(i,j)*xa%psfc(i,j)
      do k=ks,ke
         g_moist(i,j,k,P_QV)=g_moist(i,j,k,P_QV)+sdmd*dnw(k)
      enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [1.0] Get the specific humidity increments from mixing ratio increments
!------------------------------------------------------------------------------
   
   do k=ks,ke
   do j=js,je
   do i=is,ie
      g_moist(i,j,k,P_QV)=g_moist(i,j,k,P_QV)+xa%q(i,j,k)*(1.0-xb%q(i,j,k))**2
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt ( 1 , DYN_EM_TL )

   call da_med_initialdata_output( grid , config_flags, filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

IF (trace_use) CALL da_trace_exit("da_transfer_wrftltoxa_adj")


END SUBROUTINE da_transfer_wrftltoxa_adj

