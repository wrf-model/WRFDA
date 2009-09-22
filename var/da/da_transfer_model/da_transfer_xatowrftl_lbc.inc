subroutine da_transfer_xatowrftl_lbc(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRFTL increments 
   !           (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: rho_cgrid, dummytmp

#ifdef A2C
   real, allocatable, dimension(:,:,:) :: g_press
#else
   real, allocatable, dimension(:,:,:) :: utmp, vtmp, g_press
   real, allocatable, dimension(:,:,:) :: g_u, g_v, g_ph
   real, allocatable, dimension(:,:) :: mut, muu, muv
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_lbc")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   allocate (g_press(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme))

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it is easy.
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=1, grid%spec_bdy_width
            dummytmp = grid%x6a%q(i,j,k)/(1.0-grid%xb%q(i,j,k))**2 - grid%g_moist(i,j,k,P_G_QV)
            grid%g_moist_bxs(j,k,i,P_G_QV) = grid%jlbc_gama*grid%g_moist(i,j,k,P_G_QV) * (grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_moist_btxs(j,k,i,P_G_QV) = grid%jlbc_gama*dummytmp*(grid%mu_2(i,j) + grid%mub(i,j))/21600.
            dummytmp = grid%x6a%q(ie-grid%spec_bdy_width+i,j,k)/(1.0-grid%xb%q(ie-grid%spec_bdy_width+i,j,k))**2 - grid%g_moist(ie-grid%spec_bdy_width+i,j,k,P_G_QV)
            grid%g_moist_bxe(j,k,i,P_G_QV) = grid%jlbc_gama*grid%g_moist(ie-grid%spec_bdy_width+i,j,k,P_G_QV) * (grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))
            grid%g_moist_btxe(j,k,i,P_G_QV) = grid%jlbc_gama*dummytmp*(grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))/21600.
         end do
      end do
   end do
   do k=ks,ke
      do j=1,grid%spec_bdy_width
         do i=is, ie 
            dummytmp = grid%x6a%q(i,j,k)/(1.0-grid%xb%q(i,j,k))**2 - grid%g_moist(i,j,k,P_G_QV)
            grid%g_moist_bys(i,k,j,P_G_QV) = grid%jlbc_gama*grid%g_moist(i,j,k,P_G_QV) * (grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_moist_btys(i,k,j,P_G_QV) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(i,j) + grid%mub(i,j))
            dummytmp = grid%x6a%q(i,je-grid%spec_bdy_width+j,k)/(1.0-grid%xb%q(i,je-grid%spec_bdy_width+j,k))**2 - grid%g_moist(i,je-grid%spec_bdy_width+j,k,P_G_QV)
            grid%g_moist_bye(i,k,j,P_G_QV) = grid%jlbc_gama*grid%g_moist(i,je-grid%spec_bdy_width+j,k,P_G_QV) * (grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
            grid%g_moist_btye(i,k,j,P_G_QV) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=js,je
      do i=1, grid%spec_bdy_width
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(i,j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         dummytmp = -(grid%x6a%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md - grid%g_mu_2(i,j)
         grid%g_mu_bxs(j,1,i) = grid%jlbc_gama*grid%g_mu_2(i,j)
         grid%g_mu_btxs(j,1,i) =  grid%jlbc_gama*dummytmp/21600.
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(ie-grid%spec_bdy_width+i,j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(ie-grid%spec_bdy_width+i,j,k,P_QV))*grid%dnw(k)
         end do
         dummytmp = -(grid%x6a%psfc(ie-grid%spec_bdy_width+i,j)+grid%xb%psac(ie-grid%spec_bdy_width+i,j)*sdmd)/s1md - grid%g_mu_2(ie-grid%spec_bdy_width+i,j)
         grid%g_mu_bxe(j,1,i) = grid%jlbc_gama*grid%g_mu_2(ie-grid%spec_bdy_width+i,j)
         grid%g_mu_btxe(j,1,i) =  grid%jlbc_gama*dummytmp/21600.
      end do
   end do
   do j=1,grid%spec_bdy_width
      do i=is, ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(i,j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         dummytmp = -(grid%x6a%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md - grid%g_mu_2(i,j)
         grid%g_mu_bys(i,1,j) = grid%jlbc_gama*grid%g_mu_2(i,j)
         grid%g_mu_btys(i,1,j) =  grid%jlbc_gama*dummytmp/21600.
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(i,je-grid%spec_bdy_width+j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,je-grid%spec_bdy_width+j,k,P_QV))*grid%dnw(k)
         end do
         dummytmp = -(grid%x6a%psfc(i,je-grid%spec_bdy_width+j)+grid%xb%psac(i,je-grid%spec_bdy_width+j)*sdmd)/s1md - grid%g_mu_2(i,je-grid%spec_bdy_width+j)
         grid%g_mu_bye(i,1,j) = grid%jlbc_gama*grid%g_mu_2(i,je-grid%spec_bdy_width+j)
         grid%g_mu_btye(i,1,j) =  grid%jlbc_gama*dummytmp/21600.
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
               -(grid%g_mu_2(i,j)*(1.0+grid%moist(i,j,k,P_QV)) &
               +(grid%mu_2(i,j)+grid%mub(i,j))*grid%g_moist(i,j,k,P_G_QV))* &
               grid%dn(k)
            grid%x6a%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] convert temperature increments into theta increments
   !       evaluate also the increments of (1/rho) and geopotential
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=1, grid%spec_bdy_width
            dummytmp = (t0+grid%t_2(i,j,k))* &
               (grid%x6a%t(i,j,k)/grid%xb%t(i,j,k) &
                        -kappa*grid%x6a%p(i,j,k)/grid%xb%p(i,j,k)) - grid%g_t_2(i,j,k)
            grid%g_t_bxs(j,k,i) = grid%jlbc_gama*grid%g_t_2(i,j,k)*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_t_btxs(j,k,i) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(i,j) + grid%mub(i,j))
            dummytmp = (t0+grid%t_2(ie-grid%spec_bdy_width+i,j,k))* &
               (grid%x6a%t(ie-grid%spec_bdy_width+i,j,k)/grid%xb%t(ie-grid%spec_bdy_width+i,j,k) &
                        -kappa*grid%x6a%p(ie-grid%spec_bdy_width+i,j,k)/grid%xb%p(ie-grid%spec_bdy_width+i,j,k)) - grid%g_t_2(ie-grid%spec_bdy_width+i,j,k)
            grid%g_t_bxe(j,k,i) = grid%jlbc_gama*grid%g_t_2(ie-grid%spec_bdy_width+i,j,k)*(grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))
            grid%g_t_btxe(j,k,i) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))
         end do
      end do
   end do
   do k=ks,ke
      do j=1,grid%spec_bdy_width
         do i=is, ie
            dummytmp = (t0+grid%t_2(i,j,k))* &
               (grid%x6a%t(i,j,k)/grid%xb%t(i,j,k) &
                        -kappa*grid%x6a%p(i,j,k)/grid%xb%p(i,j,k)) - grid%g_t_2(i,j,k)
            grid%g_t_bys(i,k,j) = grid%jlbc_gama*grid%g_t_2(i,j,k)*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_t_btys(i,k,j) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(i,j) + grid%mub(i,j))
            dummytmp = (t0+grid%t_2(i,je-grid%spec_bdy_width+j,k))* &
               (grid%x6a%t(i,je-grid%spec_bdy_width+j,k)/grid%xb%t(i,je-grid%spec_bdy_width+j,k) &
                        -kappa*grid%x6a%p(i,je-grid%spec_bdy_width+j,k)/grid%xb%p(i,je-grid%spec_bdy_width+j,k)) - grid%g_t_2(i,je-grid%spec_bdy_width+j,k)
            grid%g_t_bye(i,k,j) = grid%jlbc_gama*grid%g_t_2(i,je-grid%spec_bdy_width+j,k)*(grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
            grid%g_t_btye(i,k,j) = grid%jlbc_gama*dummytmp/21600.*(grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
         end do
      end do
   end do

   allocate ( g_ph(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )

   do j=js,je
      do i=is,ie
         g_ph(i,j,ks)=0.0
         do k=ks,ke
            rho_cgrid=grid%xb%rho(i,j,k) &
                      *(grid%x6a%p(i,j,k)/grid%xb%p(i,j,k) &
                      -grid%x6a%t(i,j,k)/grid%xb%t(i,j,k) &
                      -0.61*grid%x6a%q(i,j,k)/(1.0+0.61*grid%xb%q(i,j,k)))
            g_ph(i,j,k+1)=g_ph(i,j,k) &
               -(g_press(i,j,k+1)-g_press(i,j,k) &
               +(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*rho_cgrid) &
               /grid%xb%rho(i,j,k)
         end do
      end do
   end do

   do k=ks,ke
      do j=js,je
         do i=1, grid%spec_bdy_width
            grid%g_ph_bxs(j,k,i) = grid%jlbc_gama*grid%g_ph_2(i,j,k)*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_ph_btxs(j,k,i) = grid%jlbc_gama*(g_ph(i,j,k) - grid%g_ph_2(i,j,k))/21600.*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_ph_bxe(j,k,i) = grid%jlbc_gama*grid%g_ph_2(ie-grid%spec_bdy_width+i,j,k)*(grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))
            grid%g_ph_btxe(j,k,i) = grid%jlbc_gama*(g_ph(ie-grid%spec_bdy_width+i,j,k) - grid%g_ph_2(ie-grid%spec_bdy_width+i,j,k))/21600.*(grid%mu_2(ie-grid%spec_bdy_width+i,j) + grid%mub(ie-grid%spec_bdy_width+i,j))
         end do
      end do
   end do
   do k=ks,ke
      do j=1, grid%spec_bdy_width
         do i=is, ie
            grid%g_ph_bys(i,k,j) = grid%jlbc_gama*grid%g_ph_2(i,j,k)*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_ph_btys(i,k,j) = grid%jlbc_gama*(g_ph(i,j,k) - grid%g_ph_2(i,j,k))/21600.*(grid%mu_2(i,j) + grid%mub(i,j))
            grid%g_ph_bye(i,k,j) = grid%jlbc_gama*grid%g_ph_2(i,je-grid%spec_bdy_width+j,k)*(grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
            grid%g_ph_btye(i,k,j) = grid%jlbc_gama*(g_ph(i,je-grid%spec_bdy_width+j,k) - grid%g_ph_2(i,je-grid%spec_bdy_width+j,k))/21600.*(grid%mu_2(i,je-grid%spec_bdy_width+j) + grid%mub(i,je-grid%spec_bdy_width+j))
         end do
      end do
   end do

   deallocate (g_press)
   deallocate (g_ph)

   !---------------------------------------------------------------------------
   ! [5.0] convert from a-grid to c-grid
   !---------------------------------------------------------------------------

   allocate ( g_u(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )
   allocate ( g_v(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )
   allocate ( mut(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme) )
   allocate ( muu(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme) )
   allocate ( muv(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme) )

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"

   allocate ( utmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )
   allocate ( vtmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )

   utmp = grid%x6a%u
   vtmp = grid%x6a%v

   ! The southern boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (js == grid%xp%jds) then
      vtmp(is:ie,js-1,ks:ke)=2.0*grid%x6a%v(is:ie,js  ,ks:ke) &
                            -    grid%x6a%v(is:ie,js+1,ks:ke)
   end if

   ! The northern boundary
   if (je == grid%xp%jde) then
      vtmp(is:ie,je+1,ks:ke)=2.0*grid%x6a%v(is:ie,je  ,ks:ke) &
                            -    grid%x6a%v(is:ie,je-1,ks:ke)
   end if

   ! The western boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (is == grid%xp%ids) then
      utmp(is-1,js:je,ks:ke)=2.0*grid%x6a%u(is  ,js:je,ks:ke) &
                            -    grid%x6a%u(is+1,js:je,ks:ke)
   end if

   ! The eastern boundary
   if (ie == grid%xp%ide) then
      utmp(ie+1,js:je,ks:ke)=2.0*grid%x6a%u(ie  ,js:je,ks:ke) &
                            -    grid%x6a%u(ie-1,js:je,ks:ke)
   end if

   do k=ks,ke
      do j=js,je
         do i=is,ie+1
            grid%g_u_2(i,j,k)=0.5*(utmp(i-1,j  ,k)+utmp(i,j,k))
         end do
      end do
      do j=js,je+1
         do i=is,ie
            grid%g_v_2(i,j,k)=0.5*(vtmp(i  ,j-1,k)+vtmp(i,j,k))
         end do
      end do
   end do


   deallocate (utmp)
   deallocate (vtmp)
#else

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            g_u(i,j,k)=0.5*(grid%x6a%u(i-1,j,k)+grid%x6a%u(i,j,k))
         end do
      end do
      do j=js+1,je
         do i=is,ie
            g_v(i,j,k)=0.5*(grid%x6a%v(i,j-1,k)+grid%x6a%v(i,j,k))
         end do
      end do
   end do

   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2

   ! The eastern boundary
   g_u(ie+1,js:je,ks:ke)=(3.0*grid%x6a%u(ie,js:je,ks:ke)-grid%x6a%u(ie-1,js:je,ks:ke))/2.0

   ! The northern boundary
   g_v(is:ie,je+1,ks:ke)=(3.0*grid%x6a%v(is:ie,je,ks:ke)-grid%x6a%v(is:ie,je-1,ks:ke))/2.0

   ! To keep the gradient, A(0) = 2A(1)-A(2)
   ! and on C-Grid, this will lead to C(1)=(A(0)+A(1))/2=(3A(1)-A(2))/2

   ! The western boundary
   g_u(is,js:je,ks:ke)=(3.0*grid%x6a%u(is,js:je,ks:ke)-grid%x6a%u(is+1,js:je,ks:ke))/2.0

   ! The southern boundary
   g_v(is:ie,js,ks:ke)=(3.0*grid%x6a%v(is:ie,js,ks:ke)-grid%x6a%v(is:ie,js+1,ks:ke))/2.0

   do j=js,je
      do i=is,ie
         mut(i,j) = grid%mu_2(i,j)+grid%mub(i,j)
      end do

      mut(is-1,j) = mut(is,j)
      mut(ie+1,j) = mut(ie,j)
   end do

   do i=is-1,ie+1
      mut(i,js-1)=mut(i,js)
      mut(i,je+1)=mut(i,je)
   end do

   do j=js,je
      do i=is,ie+1
         muu(i,j) = 0.5*(mut(i,j)+mut(i-1,j))
      end do
   end do

   do j=js,je+1
      do i=is,ie
         muv(i,j) = 0.5*(mut(i,j)+mut(i,j-1))
      end do
   end do


   do k=ks,ke
      do j=js,je
         do i=1, grid%spec_bdy_width
            grid%g_u_bxs(j,k,i) = grid%jlbc_gama*grid%g_u_2(i,j,k)*muu(i,j)/grid%msfu(i,j)
            grid%g_u_btxs(j,k,i) = grid%jlbc_gama*(g_u(i,j,k) - grid%g_u_2(i,j,k)) /21600.*muu(i,j)/grid%msfu(i,j)
            grid%g_u_bxe(j,k,i) = grid%jlbc_gama*grid%g_u_2(ie-grid%spec_bdy_width+1+i,j,k)*muu(ie-grid%spec_bdy_width+1+i,j)/grid%msfu(ie-grid%spec_bdy_width+1+i,j)
            grid%g_u_btxe(j,k,i) = grid%jlbc_gama*(g_u(ie-grid%spec_bdy_width+1+i,j,k) - grid%g_u_2(ie-grid%spec_bdy_width+1+i,j,k)) /21600.*muu(ie-grid%spec_bdy_width+1+i,j)/grid%msfu(ie-grid%spec_bdy_width+1+i,j)
         end do
      end do
   end do
   do k=ks,ke
      do j=1, grid%spec_bdy_width
         do i=is, ie+1
            grid%g_u_bys(i,k,j) = grid%jlbc_gama*grid%g_u_2(i,j,k)*muu(i,j)/grid%msfu(i,j)
            grid%g_u_btys(i,k,j) = grid%jlbc_gama*(g_u(i,j,k) - grid%g_u_2(i,j,k)) /21600.*muu(i,j)/grid%msfu(i,j)
            grid%g_u_bye(i,k,j) = grid%jlbc_gama*grid%g_u_2(i,je-grid%spec_bdy_width+j,k)*muu(i,je-grid%spec_bdy_width+j)/grid%msfu(i,je-grid%spec_bdy_width+j)
            grid%g_u_btye(i,k,j) = grid%jlbc_gama*(g_u(i,je-grid%spec_bdy_width+j,k) - grid%g_u_2(i,je-grid%spec_bdy_width+j,k)) /21600.*muu(i,je-grid%spec_bdy_width+j)/grid%msfu(i,je-grid%spec_bdy_width+j)
         end do
      end do
   end do

   do k=ks,ke
      do j=js,je+1
         do i=1, grid%spec_bdy_width
            grid%g_v_bxs(j,k,i) = grid%jlbc_gama*grid%g_v_2(i,j,k)*muv(i,j)/grid%msfv(i,j)
            grid%g_v_btxs(j,k,i) = grid%jlbc_gama*(g_v(i,j,k) - grid%g_v_2(i,j,k)) /21600.*muv(i,j)/grid%msfv(i,j)
            grid%g_v_bxe(j,k,i) = grid%jlbc_gama*grid%g_v_2(ie-grid%spec_bdy_width+i,j,k)*muv(ie-grid%spec_bdy_width+i,j)/grid%msfv(ie-grid%spec_bdy_width+i,j)
            grid%g_v_btxe(j,k,i) = grid%jlbc_gama*(g_v(ie-grid%spec_bdy_width+i,j,k) - grid%g_v_2(ie-grid%spec_bdy_width+i,j,k)) /21600.*muv(ie-grid%spec_bdy_width+i,j)/grid%msfv(ie-grid%spec_bdy_width+i,j)
         end do
      end do
   end do
   do k=ks,ke
      do j=1, grid%spec_bdy_width
         do i=is, ie
            grid%g_v_bys(i,k,j) = grid%jlbc_gama*grid%g_v_2(i,j,k)*muv(i,j)/grid%msfv(i,j)
            grid%g_v_btys(i,k,j) = grid%jlbc_gama*(g_v(i,j,k) - grid%g_v_2(i,j,k)) /21600.*muv(i,j)/grid%msfv(i,j)
            grid%g_v_bye(i,k,j) = grid%jlbc_gama*grid%g_v_2(i,je-grid%spec_bdy_width+1+j,k)*muv(i,je-grid%spec_bdy_width+1+j)/grid%msfv(i,je-grid%spec_bdy_width+1+j)
            grid%g_v_btye(i,k,j) = grid%jlbc_gama*(g_v(i,je-grid%spec_bdy_width+1+j,k) - grid%g_v_2(i,je-grid%spec_bdy_width+1+j,k)) /21600.*muv(i,je-grid%spec_bdy_width+1+j)/grid%msfv(i,je-grid%spec_bdy_width+1+j)
         end do
      end do
   end do
#endif

   !---------------------------------------------------------------------------
   ! [6.0] save OTHERinCREMENT 
   !---------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke+1
         do i=is,ie
            grid%g_w_2(i,j,k)=grid%x6a%w(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [7.0] output
   !---------------------------------------------------------------------------
   deallocate (g_u)
   deallocate (g_v)
   deallocate (mut)
   deallocate (muu)
   deallocate (muv)

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt (1 , DYN_EM_TL)

   call da_med_initialdata_output_lbc(grid , config_flags, filnam)

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt (1 , DYN_EM)

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_lbc")

end subroutine da_transfer_xatowrftl_lbc


