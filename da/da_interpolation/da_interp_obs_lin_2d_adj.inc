subroutine da_interp_obs_lin_2d_adj(fm2d, loc, fo2d, z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(inout) :: fm2d(ims:ime,jms:jme,kms:kme)
   type (model_loc_type), intent(in)    :: loc
   real,                  intent(in)    :: z, fo2d

   integer :: i, j, k, kk
   real    :: dx, dxm, dy, dym, dz, dzm
   real    :: fmz(kms:kme) 

   if (trace_use_frequent) call da_trace_entry("da_interp_obs_lin_2d_adj")

   i   = loc%i
   j   = loc%j
   dx  = loc%dx
   dy  = loc%dy
   dxm = loc%dxm
   dym = loc%dym

   call da_togrid(z, kts, kte, k, dz, dzm)

   fmz(k)   = dzm*fo2d
   fmz(k+1) = dz *fo2d

   do kk = k,k+1
      fm2d(i  ,j  ,kk) = dym*dxm* fmz(kk) + fm2d(i  ,j  ,kk)
      fm2d(i+1,j  ,kk) = dym*dx * fmz(kk) + fm2d(i+1,j  ,kk)
      fm2d(i  ,j+1,kk) = dy *dxm* fmz(kk) + fm2d(i  ,j+1,kk)
      fm2d(i+1,j+1,kk) = dy *dx * fmz(kk) + fm2d(i+1,j+1,kk)
   end do

   if (trace_use_frequent) call da_trace_exit("da_interp_obs_lin_2d_adj")

end subroutine da_interp_obs_lin_2d_adj


