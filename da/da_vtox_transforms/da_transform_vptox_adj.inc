subroutine da_transform_vptox_adj(xb, xbx, xa, vp, be, ep, xp, &
                                   ids, ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte)

   !--------------------------------------------------------------------------
   ! Purpose : Adjoint for Physical transform of variables 
   !--------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (xbx_type),intent(in)           :: xbx ! Header & non-gridded vars.
   type (x_type), intent(inout)         :: xa  ! Grad_x(Jo)
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   integer, intent(in)             :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)             :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)             :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer         :: n, k, j, k1              ! Loop counters.

   if (trace_use) call da_trace_entry("da_transform_vptox_adj")

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (xa):
   !---------------------------------------------------------------------------
      
   if (be % ne > 0 .and. alphacv_method == 2) then
      call da_add_flow_dependence_xa_adj(be % ne, ep, xa, vp, &
                                          its,ite, jts,jte, kts,kte)
   end if

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------
 
   ! Pseudo RH --> Specific humidity:
   vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) + &
                                     xa%q (its:ite,jts:jte,kts:kte) * &
                                     xb%qs(its:ite,jts:jte,kts:kte)
                                   
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

   ! Transform psi and chi to u and v:
   call da_psichi_to_uv_adj(xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2, &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte)

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
      call da_add_flow_dependence_vp_adj(be % ne, ep, vp, &
                                          its,ite, jts,jte, kts,kte)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox_adj")

end subroutine da_transform_vptox_adj


