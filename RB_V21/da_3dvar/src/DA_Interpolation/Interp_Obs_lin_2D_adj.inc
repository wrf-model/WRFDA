subroutine Interp_Obs_lin_2D_adj(fm2d, xp, i, j, dx, dy, dxm, dym, fo2d, z)

  implicit none

  type (xpose_type),     intent(in)    :: xp      ! Dimensions and xpose buffers.
  integer,               intent(in)    :: i, j
  real,                  intent(in)    :: dx, dxm, dy, dym, z, fo2d

  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                         intent(inout) :: fm2d    ! Input variable

  integer                              :: k, kk
  real                                 :: dz, dzm
  real, dimension(xp%kms:xp%kme)       :: fmz

  call toGrid(z, xp%kts, xp%kte, k, dz, dzm)

  fmz(k)   = dzm*fo2d
  fmz(k+1) = dz *fo2d

  do kk = k,k+1
    fm2d(i  ,j  ,kk) = dym*dxm* fmz(kk) + fm2d(i  ,j  ,kk)
    fm2d(i+1,j  ,kk) = dym*dx * fmz(kk) + fm2d(i+1,j  ,kk)
    fm2d(i  ,j+1,kk) = dy *dxm* fmz(kk) + fm2d(i  ,j+1,kk)
    fm2d(i+1,j+1,kk) = dy *dx * fmz(kk) + fm2d(i+1,j+1,kk)
  enddo

end subroutine Interp_Obs_lin_2D_adj

