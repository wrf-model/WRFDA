subroutine da_interp_obs_lin_2d(fm2d, loc, fo2d, z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(in)  :: fm2d(ims:ime,jms:jme,kms:kme)
   type (model_loc_type), intent(in)  :: loc
   real,                  intent(out) :: fo2d  ! Output variable
   real,                  intent(in)  :: z

   integer :: i,j,k, kk
   real    :: dx, dxm, dy, dym, dz, dzm
   real    :: fmz(kms:kme)

   if (trace_use_frequent) call da_trace_entry("da_interp_obs_lin_2d")

   i   = loc%i
   j   = loc%j
   dx  = loc%dx
   dy  = loc%dy
   dxm = loc%dxm
   dym = loc%dym

   call da_togrid(z, kts, kte, k, dz, dzm)

   do kk = k,k+1 
      fmz(kk) = dym*(dxm*fm2d(i,j  ,kk) + dx*fm2d(i+1,j  ,kk)) &
              + dy *(dxm*fm2d(i,j+1,kk) + dx*fm2d(i+1,j+1,kk))
   end do

   fo2d = dzm*fmz(k) + dz*fmz(k+1)

   if (trace_use_frequent) call da_trace_exit("da_interp_obs_lin_2d")

end subroutine da_interp_obs_lin_2d


