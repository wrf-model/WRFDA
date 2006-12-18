subroutine da_transfer_wrftltoxa( grid, config_flags, filnam, &
#include "em_dummy_new_args.inc"
                              )

   !---------------------------------------------------------------------------
   ! PURPOSE: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Definitions of dummy arguments to solve

#include "em_dummy_new_decl.inc"

   ! Local variables

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke

   real    :: sdmd, s1md, qvf1, qvf2, qvf1_b, qvf2_b
   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: g_press

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_wrftltoxa")

   !---------------------------------------------------------------------------
   ! [0.0] inPUT
   !---------------------------------------------------------------------------

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt ( 1 , DYN_EM_TL )

   call da_med_initialdata_input( grid , config_flags, filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

   !---------------------------------------------------------------------------
   ! [1.0] Get the specific humidity increments from mixing ratio increments
   !---------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      grid%xa%q(i,j,k)=g_moist(i,j,k,P_QV)*(1.0-grid%xb%q(i,j,k))**2
   end do
   end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE psfc increments from mu-increments
   !---------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         sdmd=sdmd+g_moist(i,j,k,P_QV)*grid%em_dnw(k)
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*grid%em_dnw(k)
      end do
      grid%xa%psfc(i,j)=-grid%xb%psac(i,j)*sdmd-grid%em_g_mu_2(i,j)*s1md
   end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] COMPUTE pressure increments 
   !---------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      g_press(i,j,ke+1)=0.0
      do k=ke,ks,-1
         g_press(i,j,k)=g_press(i,j,k+1) &
                 -(grid%em_g_mu_2(i,j)*(1.0+moist(i,j,k,P_QV)) &
                 +(grid%em_mu_2(i,j)+grid%em_mub(i,j))*g_moist(i,j,k,P_QV))*grid%em_dn(k)
         grid%xa%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
      end do
   end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] CONVERT THETA inCREMENTS TO T inCREMENTS
   !---------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      grid%xa%t(i,j,k)=grid%xb%t(i,j,k)*(grid%em_g_t_2(i,j,k)/(300.0+grid%em_t_2(i,j,k)) &
                         +kappa*grid%xa%p(i,j,k)/grid%xb%p(i,j,k))
   end do
   end do
   end do

   ! FIX. In the inverse, g_ph information is lost. This should be investigated 
   ! later.

   !-------------------------------------------------------------------------
   ! [5.0] CONVERT FROM C-GRID TO A-GRID
   !-------------------------------------------------------------------------

   ! Fill the halo region for u and v.
   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id3)

   do k=ks,ke
   do j=js,je
   do i=is,ie
      grid%xa%u(i,j,k)=0.5*(grid%em_g_u_2(i+1,j  ,k)+grid%em_g_u_2(i,j,k))
      grid%xa%v(i,j,k)=0.5*(grid%em_g_v_2(i  ,j+1,k)+grid%em_g_v_2(i,j,k))
   end do
   end do
   end do

   !---------------------------------------------------------------------------
   ! [6.0] ALL THE SIMPLE ONES
   !---------------------------------------------------------------------------

   do j=js,je
   do k=ks,ke+1
   do i=is,ie
      grid%xa%w(i,j,k)=grid%em_g_w_2(i,j,k)
   end do
   end do
   end do

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
   if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         grid%xa%qcw(i,j,k)=g_moist(i,j,k,p_qcw)
         grid%xa%qrn(i,j,k)=g_moist(i,j,k,p_qrn)
      end do
      end do
      end do
   end if

   if (size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         grid%xa%qci(i,j,k)=g_moist(i,j,k,p_qci)
         grid%xa%qsn(i,j,k)=g_moist(i,j,k,p_qsn)
      end do
      end do
      end do
   end if

   if (size(moist_2,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         grid%xa%qgr(i,j,k)=g_moist(i,j,k,p_qgr) 
      end do
      end do
      end do
   end if
#endif

   if (trace_use) call da_trace_exit("da_transfer_wrftltoxa")

end subroutine da_transfer_wrftltoxa


