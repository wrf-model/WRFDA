subroutine da_transform_vptox_adj(grid, xb, xa, vp, be, ep, xp)

   !--------------------------------------------------------------------------
   ! Purpose: Adjoint for Physical transform of variables 
   !--------------------------------------------------------------------------

   implicit none

   type (domain)                        :: grid

   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (x_type), intent(inout)         :: xa  ! Grad_x(Jo)
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.

   integer         :: k, j, k1              ! Loop counters.

   if (trace_use) call da_trace_entry("da_transform_vptox_adj")

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (xa):
   !---------------------------------------------------------------------------
      
   if (be % ne > 0 .and. alphacv_method == 2) then
      call da_add_flow_dependence_xa_adj(be % ne, ep, xa, vp)
   end if

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------

   if (use_radarobs .and. use_radar_rf) then

      ! Pseudo RH --> Total water mixing ratio:

      vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) + &
                                        xa%qt(its:ite,jts:jte,kts:kte) * &
                                        xb%qs(its:ite,jts:jte,kts:kte)

   else 

   ! Pseudo RH --> Water vapor mixing ratio:

      vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) + &
                                        xa%q (its:ite,jts:jte,kts:kte) * &
                                        xb%qs(its:ite,jts:jte,kts:kte)

   endif

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif

   ! Transform psi and chi to u and v:
   call da_psichi_to_uv_adj(xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2)

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   ! Surface Pressure
   do k=kts,kte
      do j=jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k) + &
                              be%reg_ps(j,k)*xa%psfc(its:ite,j)
      end do
   end do
   vp%v5(its:ite,jts:jte,1) = xa%psfc(its:ite,jts:jte) 

   ! Temperature
   do k1 = kts,kte
      do k = kts,kte
         do j = jts,jte
            vp%v1(its:ite,j,k1) = vp%v1(its:ite,j,k1) + &
                                  be%reg_t(j,k,k1)*xa%t(its:ite,j,k)
         end do
      end do
   end do
   vp%v3(its:ite,jts:jte,kts:kte) = xa%t(its:ite,jts:jte,kts:kte)

   ! Chi
   do k = kts,kte
      do j = jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k)  + &
                              be%reg_chi(j,k)*vp%v2(its:ite,j,k)
 
      end do
   end do

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------
   
   if (be % ne > 0 .and. alphacv_method == 1) then
      call da_add_flow_dependence_vp_adj(be % ne, ep, vp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox_adj")

end subroutine da_transform_vptox_adj


