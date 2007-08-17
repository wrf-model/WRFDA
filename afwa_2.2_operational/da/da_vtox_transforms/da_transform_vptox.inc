subroutine da_transform_vptox(xb, vp, xp, xa, be, ep)

   !-----------------------------------------------------------------------
   ! Purpose: Physical transform of analysis increment variables.
   !-----------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)           :: xb  ! First guess structure.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (xpose_type), intent(inout)     :: xp  ! Transpose variables.
   type (x_type), intent(out)           :: xa  ! Analysis increments.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (ep_type), intent(in), optional :: ep  ! Ensemble perturbations.

   integer              :: k, j, k1            ! Loop counters.

   IF (trace_use) call da_trace_entry("da_transform_vptox") 

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == 1) then
      call da_add_flow_dependence_vp(be % ne, ep, vp, &
                                      its,ite, jts,jte, kts,kte)
   end if

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   ! Chi:
   do k = kts, kte
      do j = jts, jte
         vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) + &
                              be%reg_chi(j,k)* vp%v1(its:ite,j,k)
      end do
   end do
  
   ! Temperature:
   xa%t(its:ite,jts:jte,kts:kte) = vp%v3(its:ite,jts:jte,kts:kte)
   do k1 = kts, kte
      do k = kts, kte
         do j = jts, jte
            xa%t(its:ite,j,k) = xa%t(its:ite,j,k)  + &
                                be%reg_t(j,k,k1)*vp%v1(its:ite,j,k1)
         end do
     end do
   end do

   ! Surface Pressure
   xa%psfc(its:ite,jts:jte) = vp%v5(its:ite,jts:jte,1) 
   do k = kts,kte
      do j = jts,jte
         xa%psfc(its:ite,j) = xa%psfc(its:ite,j) + &
            be%reg_ps(j,k)*vp%v1(its:ite,j,k)
      end do
   end do

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------

   ! Communicate halo region.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

   ! Psi and chi to u and v:

   call da_psichi_to_uv(vp % v1, vp % v2, xb % coefx, &
                         xb % coefy , xa % u, xa % v)

   if (use_radarobs .and. use_radar_rf) then

   ! Pseudo RH --> Total water mixing ratio:

      xa % qt(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte) * &
                                         xb%qs(its:ite,jts:jte,kts:kte)

   else

   ! Pseudo RH --> Water vapor mixing ratio:

      xa % q(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte) * &
                                        xb%qs(its:ite,jts:jte,kts:kte)
   endif

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (xa):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == 2) then
      call da_add_flow_dependence_xa(be % ne, ep, vp, xa, &
                                      its,ite, jts,jte, kts,kte)
   end if

   IF (trace_use) call da_trace_exit("da_transform_vptox") 
 
end subroutine da_transform_vptox

