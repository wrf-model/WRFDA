subroutine da_transfer_wrftltoxa_adj(grid, config_flags, filnam, &
#include "em_dummy_new_args.inc"
                             )

   !---------------------------------------------------------------------------
   ! PURPOSE: Convert analysis increments into WRFAD increments 
   !          (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Definitions of dummy arguments to solve

#include "em_dummy_new_decl.inc"

   integer :: i, j, k, n, ndynopt
   integer :: is, ie, js, je, ks, ke

   real    :: sdmd, s1md, qvf1, qvf2, qvf1_b, qvf2_b
   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: g_press

   if (trace_use) call da_trace_entry("da_transfer_wrftltoxa_adj")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   !---------------------------------------------------------------------------
   ! [6.0] ALL THE SIMPLE ONES
   !---------------------------------------------------------------------------

   do k=ks,ke+1
      do j=js,je
         do i=is,ie
            grid%em_g_w_2(i,j,k)=grid%xa%w(i,j,k)
         end do
      end do
   end do

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
   if (size(moist,dim=4) >= 4) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               g_moist(i,j,k,p_qcw) =  grid%xa%qcw(i,j,k)
               g_moist(i,j,k,p_qrn) =  grid%xa%qrn(i,j,k)
            end do
         end do
      end do
   end if

   if (size(moist,dim=4) >= 6) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               g_moist(i,j,k,p_qci) =  grid%xa%qci(i,j,k)
               g_moist(i,j,k,p_qsn) =  grid%xa%qsn(i,j,k)
            end do
         end do
      end do
   end if

   if (size(moist,dim=4) >= 7) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               g_moist(i,j,k,p_qgr) =  grid%xa%qgr(i,j,k)
            end do
         end do
      end do
   end if

#endif


! ----------------------------------------------------------------------------
![5.0] CONVERT FROM C-GRID TO A-GRID
! ----------------------------------------------------------------------------

#ifdef DM_PARALLEL
   ! Fill the halo region for u and v.

   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id3)

   ! The western boundary
   if (is == grid%xp%ids) grid%xa%u(is-1,js:je,ks:ke)=0.

   ! The southern boundary
   if (js == grid%xp%jds) grid%xa%v(is:ie,js-1,ks:ke)=0.

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%em_g_u_2(i,j,k)=0.5*(grid%xa%u(i-1,j  ,k)+grid%xa%u(i,j,k))
           grid%em_g_v_2(i,j,k)=0.5*(grid%xa%v(i  ,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   ! The eastern boundary
   if (ie == grid%xp%ide) grid%em_g_u_2(ie+1,js:je,ks:ke)=grid%xa%u(ie,js:je,ks:ke)/2.

   ! The northern boundary
   if (je == grid%xp%jde) grid%em_g_v_2(is:ie,je+1,ks:ke)=grid%xa%v(is:ie,je,ks:ke)/2.

#else

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            grid%em_g_u_2(i,j,k)=0.5*(grid%xa%u(i-1,j,k)+grid%xa%u(i,j,k))
         end do
      end do
      do j=js+1,je
         do i=is,ie
            grid%em_g_v_2(i,j,k)=0.5*(grid%xa%v(i,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   ! The western boundary
   grid%em_g_u_2(is,js:je,ks:ke)=grid%xa%u(is,js:je,ks:ke)/2.

   ! The eastern boundary
   grid%em_g_u_2(ie+1,js:je,ks:ke)=grid%xa%u(ie,js:je,ks:ke)/2.

   ! The southern boundary
   grid%em_g_v_2(is:ie,js,ks:ke)=grid%xa%v(is:ie,js,ks:ke)/2.

   ! The northern boundary
   grid%em_g_v_2(is:ie,je+1,ks:ke)=grid%xa%v(is:ie,je,ks:ke)/2.

#endif

   !---------------------------------------------------------------------------
   ! [4.0] CONVERT THETA inCREMENTS TO T inCREMENTS
   !---------------------------------------------------------------------------

   ! In the inverse, g_ph information is lost. This should be investigated later!!!
   ! However, in this adjoint problem, a_ph should be set to 0. Otherwise, a_ph 
   ! will be initialized randomly!

   do k=ks,ke+1
      do j=js,je
         do i=is,ie
            grid%em_g_ph_2(i,j,k)=0.
         end do
      end do
   end do

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%p(i,j,k)=grid%xb%t(i,j,k)*kappa*grid%xa%t(i,j,k)/grid%xb%p(i,j,k)
            grid%em_g_t_2(i,j,k)=grid%xb%t(i,j,k)*grid%xa%t(i,j,k)/(300.0+grid%em_t_2(i,j,k))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] COMPUTE pressure increments 
   !---------------------------------------------------------------------------

   g_press(is:ie,js:je,ks:ke+1)=0.
   do k=ks,ke
      do j=js,je
         do i=is,ie
            g_press(i,j,k+1)=g_press(i,j,k+1)+0.5*grid%xa%p(i,j,k)
            g_press(i,j,k )=g_press(i,j,k )+0.5*grid%xa%p(i,j,k)
            g_moist(i,j,k,P_QV)=-(grid%em_mu_2(i,j)+grid%em_mub(i,j))*g_press(i,j,k)*grid%em_dn(k)
            grid%em_g_mu_2(i,j)=-g_press(i,j,k)*(1.0+moist(i,j,k,P_QV))*grid%em_dn(k)
            g_press(i,j,k+1)=g_press(i,j,k+1)+g_press(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE psfc increments from mu-increments
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         s1md=0.0
         do k=ks,ke
            s1md=s1md+(1.0+moist(i,j,k,P_QV))*grid%em_dnw(k)
         end do
         grid%em_g_mu_2(i,j)=grid%em_g_mu_2(i,j)-grid%xa%psfc(i,j)*s1md
         sdmd=-grid%xb%psac(i,j)*grid%xa%psfc(i,j)
         do k=ks,ke
            g_moist(i,j,k,P_QV)=g_moist(i,j,k,P_QV)+sdmd*grid%em_dnw(k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [1.0] Get the specific humidity increments from mixing ratio increments
   !---------------------------------------------------------------------------
   
   do k=ks,ke
      do j=js,je
         do i=is,ie
            g_moist(i,j,k,P_QV)=g_moist(i,j,k,P_QV)+grid%xa%q(i,j,k)* &
               (1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt (1 , DYN_EM_TL)

   call da_med_initialdata_output(grid , config_flags, filnam)

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt (1 , DYN_EM)

   if (trace_use) call da_trace_exit("da_transfer_wrftltoxa_adj")

end subroutine da_transfer_wrftltoxa_adj


