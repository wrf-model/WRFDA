subroutine da_add_flow_dependence_xa_adj (ne, ep, xa, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (xa).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (x_type), intent(in)            :: xa  ! Analysis increments.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   
   integer                              :: n, k! Loop counters.

   vp % alpha(:,:,:) = 0.0

   do n = ne, 1, -1
      ! ps:
      vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                      ep % v5(its:ite,jts:jte,1,n) * & ! v5 = ps
                                      xa % psfc(its:ite,jts:jte)

      do k = kte, kts, -1

         ! q:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v4(its:ite,jts:jte,k,n) * & ! v4 = q
                                         xa % q(its:ite,jts:jte,k)

         ! t:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v3(its:ite,jts:jte,k,n) * & ! v3 = t
                                         xa % t(its:ite,jts:jte,k)

         ! v:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v2(its:ite,jts:jte,k,n) * & ! v2 = v
                                         xa % v(its:ite,jts:jte,k)

         ! u:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v1(its:ite,jts:jte,k,n) * & ! v1 = u
                                         xa % u(its:ite,jts:jte,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa_adj")

end subroutine da_add_flow_dependence_xa_adj


>= 6) then
      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%qci(i,j,k)=grid%a_moist(i,j,k,p_qci)
               grid%xa%qsn(i,j,k)=grid%a_moist(i,j,k,p_qsn)
            end do
         end do
      end do
   end if

   if (size(grid%moist,dim=4) >= 7) then
      do k=kts,kte
         do j=jts,jte
            do i=its,ite
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
   grid%xa%u=grid%a_u_2
   grid%xa%v=grid%a_v_2
#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif
   grid%a_u_2=grid%xa%u
   grid%a_v_2=grid%xa%v
   grid%xa%u=utmp
   grid%xa%v=vtmp
   utmp=0.0
   vtmp=0.0

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            utmp(i,j,k)=utmp(i,j,k)+0.5*(grid%a_u_2(i+1,j  ,k)+grid%a_u_2(i,j,k))
            vtmp(i,j,k)=vtmp(i,j,k)+0.5*(grid%a_v_2(i  ,j+1,k)+grid%a_v_2(i,j,k))
         end do
      end do
   end do

   utmp(its-1,jts:jte,kts:kte)=utmp(its-1,jts:jte,kts:kte)+0.5*grid%a_u_2(its,jts:jte,kts:kte)
   utmp(ite+1,jts:jte,kts:kte)=utmp(ite+1,jts:jte,kts:kte)+0.5*grid%a_u_2(ite+1,jts:jte,kts:kte)
   vtmp(its:ite,jts-1,kts:kte)=vtmp(its:ite,jts-1,kts:kte)+0.5*grid%a_v_2(its:ite,jts,kts:kte)
   vtmp(its:ite,jte+1,kts:kte)=vtmp(its:ite,jte+1,kts:kte)+0.5*grid%a_v_2(its:ite,jte+1,kts:kte)

   ! The western boundary
   if (its == grid%xp%ids) then
      grid%xa%u(its  ,jts:jte,kts:kte)=grid%xa%u(its  ,jts:jte,kts:kte)+2.0*utmp(its-1,jts:jte,kts:kte)
      grid%xa%u(its+1,jts:jte,kts:kte)=grid%xa%u(its+1,jts:jte,kts:kte)-utmp(its-1,jts:jte,kts:kte)
   end if

   ! The eastern boundary
   if (ite == grid%xp%ide) then
      grid%xa%u(ite  ,jts:jte,kts:kte)=grid%xa%u(ite  ,jts:jte,kts:kte)+2.0*utmp(ite+1,jts:jte,kts:kte)
      grid%xa%u(ite-1,jts:jte,kts:kte)=grid%xa%u(ite-1,jts:jte,kts:kte)-utmp(ite+1,jts:jte,kts:kte)
   end if

   grid%xa%u=grid%xa%u+utmp

   ! The southern boundary
   if (jts == grid%xp%jds) then
      grid%xa%v(its:ite,jts  ,kts:kte)=grid%xa%v(its:ite,jts  ,kts:kte)+2.0*vtmp(its:ite,jts-1,kts:kte)
      grid%xa%v(its:ite,jts+1,kts:kte)=grid%xa%v(its:ite,jts+1,kts:kte)-vtmp(its:ite,jts-1,kts:kte)
   end if

   ! The northern boundary
   if (jte == grid%xp%jde) then
      grid%xa%v(its:ite,jte  ,kts:kte)=grid%xa%v(its:ite,jte  ,kts:kte)+2.0*vtmp(its:ite,jte+1,kts:kte)
      grid%xa%v(its:ite,jte-1,kts:kte)=grid%xa%v(its:ite,jte-1,kts:kte)-vtmp(its:ite,jte+1,kts:kte)
   end if

   grid%xa%v=grid%xa%v+vtmp

   grid%a_u_2 = 0.0
   grid%a_v_2 = 0.0

   !---------------------------------------------------------------------------
   ! [4.0] Adjoint of CONVERT TEMPERATURE inCREMENTS inTO THETA inCREMENTS
   !       EVALUATE ALSO THE inCREMENTS OF (1/rho) AND GEOPOTENTIAL
   !---------------------------------------------------------------------------

   a_press(its:ite,jts:jte,kts:kte+1)=0.0
   do k=kte,kts,-1
      do j=jts,jte
         do i=its,ite
            rho_cgrid=-(grid%em_ph_2(i,j,k+1)-grid%em_ph_2(i,j,k))*grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k+1)=a_press(i,j,k+1)-grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            grid%a_ph_2(i,j,k ) =grid%a_ph_2(i,j,k)   +grid%a_ph_2(i,j,k+1)
            grid%xa%q(i,j,k)=grid%xa%q(i,j,k)-grid%xb%rho(i,j,k)*0.61*rho_cgrid/(1.0+0.61*grid%xb%q(i,j,k))
            grid%xa%t(i,j,k)=grid%xa%t(i,j,k)-grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%t(i,j,k)
            grid%xa%p(i,j,k)= grid%xa%p(i,j,k)+grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%p(i,j,k)
         end do
      end do
   end do

   do k=kts,kte
      do j=jts,jte
         do i=its,ite 
            grid%xa%p(i,j,k)=grid%xa%p(i,j,k)-(t0+grid%em_t_2(i,j,k))*kappa*grid%a_t_2(i,j,k)/grid%xb%p(i,j,k)
            grid%xa%t(i,j,k)=grid%xa%t(i,j,k)+(t0+grid%em_t_2(i,j,k))*grid%a_t_2(i,j,k)/grid%xb%t(i,j,k)
         end do
      end do
   end do

   grid%a_t_2 = 0.0
   grid%a_ph_2 = 0.0
 
   !---------------------------------------------------------------------------
   ! [3.0] Adjoint of COMPUTE pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            a_press(i,j,k+1)=a_press(i,j,k+1)+0.5*grid%xa%p(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+0.5*grid%xa%p(i,j,k)
            grid%xa%p(i,j,k)=0.0
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)-(grid%em_mu_2(i,j)+grid%em_mub(i,j))*a_press(i,j,k)*grid%em_dn(k)
            grid%a_mu_2(i,j)=grid%a_mu_2(i,j)-a_press(i,j,k)*(1.0+grid%moist(i,j,k,P_QV))*grid%em_dn(k)
            a_press(i,j,k+1)=a_press(i,j,k+1)+a_press(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] Adjoint of COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         sdmd=0.0
         s1md=0.0
         do k=kts,kte
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%em_dnw(k)
         end do
         sdmd=sdmd-grid%xb%psac(i,j)*grid%a_mu_2(i,j)/s1md
         grid%xa%psfc(i,j)=grid%xa%psfc(i,j)-grid%a_mu_2(i,j)/s1md
         do k=kts,kte
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)+sdmd*grid%em_dnw(k)
         end do
      end do
   end do

   grid%a_mu_2 = 0.0
   !---------------------------------------------------------------------------
   ! [1.0] Adjoint of Get the mixing ratio of moisture 
   !---------------------------------------------------------------------------
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            grid%xa%q(i,j,k)=grid%xa%q(i,j,k)+grid%a_moist(i,j,k,P_A_QV)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   grid%a_moist = 0.0

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_adj")

end subroutine da_transfer_xatowrftl_adj


