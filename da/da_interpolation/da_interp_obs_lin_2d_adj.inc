subroutine da_interp_obs_lin_2d_adj(fm2d, i, j, dx, dy, dxm, dym, fo2d, z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: i, j
   real,    intent(in)    :: dx, dxm, dy, dym, z, fo2d

   real,    intent(inout) :: fm2d(ims:ime,jms:jme,kms:kme)    ! Input variable

   integer                              :: k, kk
   real                                 :: dz, dzm
   real, dimension(kms:kme)       :: fmz

   call da_togrid(z, kts, kte, k, dz, dzm)

   fmz(k)   = dzm*fo2d
   fmz(k+1) = dz *fo2d

   do kk = k,k+1
      fm2d(i  ,j  ,kk) = dym*dxm* fmz(kk) + fm2d(i  ,j  ,kk)
      fm2d(i+1,j  ,kk) = dym*dx * fmz(kk) + fm2d(i+1,j  ,kk)
      fm2d(i  ,j+1,kk) = dy *dxm* fmz(kk) + fm2d(i  ,j+1,kk)
      fm2d(i+1,j+1,kk) = dy *dx * fmz(kk) + fm2d(i+1,j+1,kk)
   end do

end subroutine da_interp_obs_lin_2d_adj


