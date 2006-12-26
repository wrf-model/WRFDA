subroutine da_transfer_xatowrftl(grid, config_flags, filnam, &
#include "em_dummy_new_args.inc"
                             )

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRFTL increments 
   !           (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Definitions of dummy arguments to solve

#include "em_dummy_new_decl.inc"

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: rho_cgrid

   real :: g_press(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, &
      grid%xp%kms:grid%xp%kme)

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it is easy.
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            g_moist(i,j,k,P_QV)=grid%xa%q(i,j,k)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+g_moist(i,j,k,P_QV)*grid%em_dnw(k)
            s1md=s1md+(1.0+moist(i,j,k,P_QV))*grid%em_dnw(k)
         end do
         grid%em_g_mu_2(i,j)=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] compute pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         g_press(i,j,ke+1)=0.0
         do k=ke,ks,-1
            g_press(i,j,k)=g_press(i,j,k+1) &
               -(grid%em_g_mu_2(i,j)*(1.0+moist(i,j,k,P_QV)) &
               +(grid%em_mu_2(i,j)+grid%em_mub(i,j))*g_moist(i,j,k,P_QV))* &
               grid%em_dn(k)
            grid%xa%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] convert temperature increments into theta increments
   !       evaluate also the increments of (1/rho) and geopotential
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%em_g_t_2(i,j,k)=(300.0+grid%em_t_2(i,j,k))* &
               (grid%xa%t(i,j,k)/grid%xb%t(i,j,k) &
                        -kappa*grid%xa%p(i,j,k)/grid%xb%p(i,j,k))
         end do
      end do
   end do
   do j=js,je
      do i=is,ie
         grid%em_g_ph_2(i,j,ks)=0.
         do k=ks,ke
            rho_cgrid=grid%xb%rho(i,j,k) &
                      *(grid%xa%p(i,j,k)/grid%xb%p(i,j,k) &
                      -grid%xa%t(i,j,k)/grid%xb%t(i,j,k) &
                      -0.61*grid%xa%q(i,j,k)/(1.+0.61*grid%xb%q(i,j,k)))
            grid%em_g_ph_2(i,j,k+1)=grid%em_g_ph_2(i,j,k) &
               -(g_press(i,j,k+1)-g_press(i,j,k) &
               +(grid%em_ph_2(i,j,k+1)-grid%em_ph_2(i,j,k))*rho_cgrid) &
               /grid%xb%rho(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [5.0] convert from a-grid to c-grid
   !---------------------------------------------------------------------------

#ifdef DM_PARALLEL
   ! Fill the halo region for u and v.

   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id3)

   ! The southern boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (js == grid%xp%jds) &
      grid%xa%v(is:ie,js-1,ks:ke)=2.0*grid%xa%v(is:ie,js,ks:ke)-grid%xa%v(is:ie,js+1,ks:ke)

   ! The western boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (is == grid%xp%ids) &
      grid%xa%u(is-1,js:je,ks:ke)=2.0*grid%xa%u(is,js:je,ks:ke)-grid%xa%u(is+1,js:je,ks:ke)

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%em_g_u_2(i,j,k)=0.5*(grid%xa%u(i-1,j  ,k)+grid%xa%u(i,j,k))
            grid%em_g_v_2(i,j,k)=0.5*(grid%xa%v(i  ,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   ! The northern boundary (compute C-GRID boundaries)
   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2
   if (je == grid%xp%jde) &
      grid%em_g_v_2(is:ie,je+1,ks:ke)=&
         (3.0*grid%xa%v(is:ie,je,ks:ke)-grid%xa%v(is:ie,je-1,ks:ke))/2.

   ! The eastern boundary (compute C-GRID boundaries)
   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2
   if (ie == grid%xp%ide) &
      grid%em_g_u_2(ie+1,js:je,ks:ke)=&
         (3.0*grid%xa%u(ie,js:je,ks:ke)- grid%xa%u(ie-1,js:je,ks:ke))/2.

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

   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2

   ! The eastern boundary
   grid%em_g_u_2(ie+1,js:je,ks:ke)=(3.0*grid%xa%u(ie,js:je,ks:ke)-grid%xa%u(ie-1,js:je,ks:ke))/2.

   ! The northern boundary
   grid%em_g_v_2(is:ie,je+1,ks:ke)=(3.0*grid%xa%v(is:ie,je,ks:ke)-grid%xa%v(is:ie,je-1,ks:ke))/2.

   ! To keep the gradient, A(0) = 2A(1)-A(2)
   ! and on C-Grid, this will lead to C(1)=(A(0)+A(1))/2=(3A(1)-A(2))/2

   ! The western boundary
   grid%em_g_u_2(is,js:je,ks:ke)=(3.0*grid%xa%u(is,js:je,ks:ke)-grid%xa%u(is+1,js:je,ks:ke))/2.

   ! The southern boundary
   grid%em_g_v_2(is:ie,js,ks:ke)=(3.0*grid%xa%v(is:ie,js,ks:ke)-grid%xa%v(is:ie,js+1,ks:ke))/2.

#endif

   !---------------------------------------------------------------------------
   ! [6.0] save OTHERinCREMENT 
   !---------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke+1
         do i=is,ie
            grid%em_g_w_2(i,j,k)=grid%xa%w(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [5.0] save inCREMENT 
   !---------------------------------------------------------------------------

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

   !---------------------------------------------------------------------------
   ! [7.0] output
   !---------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt (1 , DYN_EM_TL)

   call da_med_initialdata_output(grid , config_flags, filnam)

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt (1 , DYN_EM)

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl")

end subroutine da_transfer_xatowrftl


