subroutine Interp_Obs_lin_2D(fm2d, xp, i, j, dx, dy, dxm, dym, fo2d, z)

  implicit none

  type (xpose_type),     intent(in)  :: xp      ! Dimensions and xpose buffers.
  integer,               intent(in)  :: i, j
  real   ,               intent(in)  :: dx, dxm, dy, dym
  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                         intent(in)  :: fm2d    ! Input variable
  real   ,               intent(in)  :: z
  real   ,               intent(out) :: fo2d  ! Output variable

  integer                            :: k, kk
  real                               :: dz, dzm
  real, dimension(xp%kms:xp%kme)     :: fmz

  call toGrid(z, xp%kts, xp%kte, k, dz, dzm)

  do kk = k,k+1 
     fmz(kk) = dym*(dxm*fm2d(i,j  ,kk) + dx*fm2d(i+1,j  ,kk)) &
             + dy *(dxm*fm2d(i,j+1,kk) + dx*fm2d(i+1,j+1,kk))
  enddo

  fo2d = dzm*fmz(k) + dz*fmz(k+1)

end subroutine Interp_Obs_lin_2D

