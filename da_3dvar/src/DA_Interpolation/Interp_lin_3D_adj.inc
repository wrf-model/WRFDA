subroutine Interp_lin_3D_adj(fm3d, xp, &
                             i, j, dx, dy, dxm, dym, &
                             fo3d, ml, zk, nl)

  implicit none

  type (xpose_type),      intent(in)    :: xp      ! Dimensions and xpose buffers.
  integer,                intent(in)    :: i, j
  real,                   intent(in)    :: dx, dxm, dy, dym
  integer,                intent(in)    :: ml, nl
  real, dimension(ml),    intent(in)    :: fo3d   
  real, dimension(nl),    intent(in)    :: zk
  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                          intent(inout) :: fm3d    ! Input/Output variable

  integer                :: kk, k
  real                   :: dz, dzm
  real, dimension(xp%kms:xp%kme) :: fmz

  fmz = 0.0

  do kk = 1, nl
     if(zk(kk) > 0.0) then
        call toGrid(zk(kk), xp%kts, xp%kte, k, dz, dzm)
        fmz(k)   = dzm*fo3d(kk) + fmz(k)
        fmz(k+1) = dz *fo3d(kk) + fmz(k+1)
     endif
  enddo

  fm3d(i  ,j  ,xp%kts:xp%kte) = dym*dxm*fmz(xp%kts:xp%kte) &
                              + fm3d(i  ,j  ,xp%kts:xp%kte)
  fm3d(i+1,j  ,xp%kts:xp%kte) = dym*dx *fmz(xp%kts:xp%kte) &
                              + fm3d(i+1,j  ,xp%kts:xp%kte)
  fm3d(i  ,j+1,xp%kts:xp%kte) = dy *dxm*fmz(xp%kts:xp%kte) &
                              + fm3d(i  ,j+1,xp%kts:xp%kte)
  fm3d(i+1,j+1,xp%kts:xp%kte) = dy *dx *fmz(xp%kts:xp%kte) &
                              + fm3d(i+1,j+1,xp%kts:xp%kte)

end subroutine Interp_lin_3D_adj

