subroutine da_interp_obs_lin_2d(fm2d, i, j, dx, dy, dxm, dym, fo2d, z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,               intent(in)  :: i, j
   real   ,               intent(in)  :: dx, dxm, dy, dym
   real, dimension(ims:ime,jms:jme,kms:kme), &
                          intent(in)  :: fm2d    ! Input variable
   real   ,               intent(in)  :: z
   real   ,               intent(out) :: fo2d  ! Output variable

   integer                            :: k, kk
   real                               :: dz, dzm
   real, dimension(kms:kme)     :: fmz

   call da_togrid(z, kts, kte, k, dz, dzm)

   do kk = k,k+1 
      fmz(kk) = dym*(dxm*fm2d(i,j  ,kk) + dx*fm2d(i+1,j  ,kk)) &
              + dy *(dxm*fm2d(i,j+1,kk) + dx*fm2d(i+1,j+1,kk))
   end do

   fo2d = dzm*fmz(k) + dz*fmz(k+1)

end subroutine da_interp_obs_lin_2d


