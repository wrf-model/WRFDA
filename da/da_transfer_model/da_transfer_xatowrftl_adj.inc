subroutine da_transfer_xatowrftl_adj(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   ! Purpose: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Local variables

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: rho_cgrid

   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: a_press
   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: utmp
   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: vtmp


   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_adj")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   !---------------------------------------------------------------------------
   ! [7.0] Adjoint of outPUT (inPUT)
   !---------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_AD
   call nl_set_dyn_opt (1 , DYN_EM_AD)

   call da_med_initialdata_input(grid , config_flags, filnam)

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt (1 , DYN_EM)

   !---------------------------------------------------------------------------
   ! [6.0] Adjoint of save OTHERinCREMENT
   !---------------------------------------------------------------------------

   do k=ks,ke+1
      do j=js,je
         do i=is,ie
            grid%xa%w(i,j,k)=grid%xa%w(i,j,k)+grid%em_a_w_2(i,j,k)
         end do
      end do
   end do

   grid%em_a_w_2 = 0.0

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
   if (size(grid%moist,dim=4) >= 4) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qcw(i,j,k)=grid%a_moist(i,j,k,p_qcw)
               grid%xa%qrn(i,j,k)=grid%a_moist(i,j,k,p_qrn)
            end do
         end do
      end do
   end if

   if (size(grid%moist,dim=4) >= 6) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qci(i,j,k)=grid%a_moist(i,j,k,p_qci)
               grid%xa%qsn(i,j,k)=grid%a_moist(i,j,k,p_qsn)
            end do
         end do
      end do
   end if

   if (size(grid%moist,dim=4) >= 7) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qgr(i,j,k)=grid%a_moist(i,j,k,p_qgr) 
            end do
         end do
      end do
   end if

#endif
   !---------------------------------------------------------------------------
   ! [5.0] Adjoint of CONVERT FROM A-GRID TO C-GRID
   !---------------------------------------------------------------------------

   ! Fill the halo region for a_u and a_v.
   utmp=grid%xa%u
   vtmp=grid%xa%v
   grid%xa%u=grid%em_a_u_2
   grid%xa%v=grid%em_a_v_2
   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,HALO_PSICHI_UV_ADJ)
   grid%em_a_u_2=grid%xa%u
   grid%em_a_v_2=grid%xa%v
   grid%xa%u=utmp
   grid%xa%v=vtmp
   utmp=0.0
   vtmp=0.0

   do k=ks,ke
      do j=js,je
         do i=is,ie
            utmp(i,j,k)=utmp(i,j,k)+0.5*(grid%em_a_u_2(i+1,j  ,k)+grid%em_a_u_2(i,j,k))
            vtmp(i,j,k)=vtmp(i,j,k)+0.5*(grid%em_a_v_2(i  ,j+1,k)+grid%em_a_v_2(i,j,k))
         end do
      end do
   end do

   utmp(is-1,js:je,ks:ke)=utmp(is-1,js:je,ks:ke)+0.5*grid%em_a_u_2(is,js:je,ks:ke)
   utmp(ie+1,js:je,ks:ke)=utmp(ie+1,js:je,ks:ke)+0.5*grid%em_a_u_2(ie+1,js:je,ks:ke)
   vtmp(is:ie,js-1,ks:ke)=vtmp(is:ie,js-1,ks:ke)+0.5*grid%em_a_v_2(is:ie,js,ks:ke)
   vtmp(is:ie,je+1,ks:ke)=vtmp(is:ie,je+1,ks:ke)+0.5*grid%em_a_v_2(is:ie,je+1,ks:ke)

   ! The western boundary
   if (is == grid%xp%ids) then
      grid%xa%u(is  ,js:je,ks:ke)=grid%xa%u(is  ,js:je,ks:ke)+2.0*utmp(is-1,js:je,ks:ke)
      grid%xa%u(is+1,js:je,ks:ke)=grid%xa%u(is+1,js:je,ks:ke)-utmp(is-1,js:je,ks:ke)
   end if

   ! The eastern boundary
   if (ie == grid%xp%ide) then
      grid%xa%u(ie  ,js:je,ks:ke)=grid%xa%u(ie  ,js:je,ks:ke)+2.0*utmp(ie+1,js:je,ks:ke)
      grid%xa%u(ie-1,js:je,ks:ke)=grid%xa%u(ie-1,js:je,ks:ke)-utmp(ie+1,js:je,ks:ke)
   end if

   grid%xa%u=grid%xa%u+utmp

   ! The southern boundary
   if (js == grid%xp%jds) then
      grid%xa%v(is:ie,js  ,ks:ke)=grid%xa%v(is:ie,js  ,ks:ke)+2.0*vtmp(is:ie,js-1,ks:ke)
      grid%xa%v(is:ie,js+1,ks:ke)=grid%xa%v(is:ie,js+1,ks:ke)-vtmp(is:ie,js-1,ks:ke)
   end if

   ! The northern boundary
   if (je == grid%xp%jde) then
      grid%xa%v(is:ie,je  ,ks:ke)=grid%xa%v(is:ie,je  ,ks:ke)+2.0*vtmp(is:ie,je+1,ks:ke)
      grid%xa%v(is:ie,je-1,ks:ke)=grid%xa%v(is:ie,je-1,ks:ke)-vtmp(is:ie,je+1,ks:ke)
   end if

   grid%xa%v=grid%xa%v+vtmp

   grid%em_a_u_2 = 0.0
   grid%em_a_v_2 = 0.0

   !---------------------------------------------------------------------------
   ! [4.0] Adjoint of CONVERT TEMPERATURE inCREMENTS inTO THETA inCREMENTS
   !       EVALUATE ALSO THE inCREMENTS OF (1/rho) AND GEOPOTENTIAL
   !---------------------------------------------------------------------------

   a_press(is:ie,js:je,ks:ke+1)=0.
   do k=ke,ks,-1
      do j=js,je
         do i=is,ie
            rho_cgrid=-(grid%em_ph_2(i,j,k+1)-grid%em_ph_2(i,j,k))*grid%em_a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+grid%em_a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k+1)=a_press(i,j,k+1)-grid%em_a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            grid%em_a_ph_2(i,j,k ) =grid%em_a_ph_2(i,j,k)   +grid%em_a_ph_2(i,j,k+1)
            grid%xa%q(i,j,k)=grid%xa%q(i,j,k)-grid%xb%rho(i,j,k)*0.61*rho_cgrid/(1.+0.61*grid%xb%q(i,j,k))
            grid%xa%t(i,j,k)=grid%xa%t(i,j,k)-grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%t(i,j,k)
            grid%xa%p(i,j,k)= grid%xa%p(i,j,k)+grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%p(i,j,k)
         end do
      end do
   end do

   do k=ks,ke
      do j=js,je
         do i=is,ie 
            grid%xa%p(i,j,k)=grid%xa%p(i,j,k)-(t0+grid%em_t_2(i,j,k))*kappa*grid%em_a_t_2(i,j,k)/grid%xb%p(i,j,k)
            grid%xa%t(i,j,k)=grid%xa%t(i,j,k)+(t0+grid%em_t_2(i,j,k))*grid%em_a_t_2(i,j,k)/grid%xb%t(i,j,k)
         end do
      end do
   end do

   grid%em_a_t_2 = 0.0
   grid%em_a_ph_2 = 0.0
 
   !---------------------------------------------------------------------------
   ! [3.0] Adjoint of COMPUTE pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            a_press(i,j,k+1)=a_press(i,j,k+1)+0.5*grid%xa%p(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+0.5*grid%xa%p(i,j,k)
            grid%xa%p(i,j,k)=0.0
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)-(grid%em_mu_2(i,j)+grid%em_mub(i,j))*a_press(i,j,k)*grid%em_dn(k)
            grid%em_a_mu_2(i,j)=grid%em_a_mu_2(i,j)-a_press(i,j,k)*(1.0+grid%moist(i,j,k,P_QV))*grid%em_dn(k)
            a_press(i,j,k+1)=a_press(i,j,k+1)+a_press(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] Adjoint of COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%em_dnw(k)
         end do
         sdmd=sdmd-grid%xb%psac(i,j)*grid%em_a_mu_2(i,j)/s1md
         grid%xa%psfc(i,j)=grid%xa%psfc(i,j)-grid%em_a_mu_2(i,j)/s1md
         do k=ks,ke
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)+sdmd*grid%em_dnw(k)
         end do
      end do
   end do

   grid%em_a_mu_2 = 0.0
   !---------------------------------------------------------------------------
   ! [1.0] Adjoint of Get the mixing ratio of moisture 
   !---------------------------------------------------------------------------
   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%q(i,j,k)=grid%xa%q(i,j,k)+grid%a_moist(i,j,k,P_A_QV)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   grid%a_moist = 0.0

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_adj")

end subroutine da_transfer_xatowrftl_adj


