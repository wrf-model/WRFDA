subroutine Interp_lin_3D(fm3d, xp, &
                         i, j, dx, dy, dxm, dym, &
                         fo3d, ml, zk, nl)

  implicit none

  TYPE (xpose_type),      intent(in)  :: xp      ! Dimensions and xpose buffers.
  integer,                intent(in)  :: i, j
  real,                   intent(in)  :: dx, dxm, dy, dym
  real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                          intent(in)  :: fm3d    ! Input variable
  integer,                intent(in)  :: ml, nl                        
  real,    dimension(ml), intent(out) :: fo3d  ! Output variable 
  real,    dimension(nl), intent(in)  :: zk

  integer                             :: kk, k
  real                                :: dz, dzm
  real, dimension(xp%kms:xp%kme)      :: fmz

  fmz(xp%kts:xp%kte) = dym*(dxm*fm3d(i  ,j  ,xp%kts:xp%kte) &
                           +dx *fm3d(i+1,j  ,xp%kts:xp%kte)) &
                     + dy *(dxm*fm3d(i  ,j+1,xp%kts:xp%kte) &
                           +dx *fm3d(i+1,j+1,xp%kts:xp%kte))

  fo3d(:) = 0.0

  do kk = 1, nl
     if(zk(kk) > 0.0 .or. &
       (ANALYSIS_TYPE == 'VERIFY' .or. ANALYSIS_TYPE == 'verify')) then
! Guo... for 'VERIFY', any of zk(kk) values are allowed:
    
        call toGrid(zk(kk), xp%kts, xp%kte, k, dz, dzm)
        fo3d(kk) = dzm*fmz(k) + dz*fmz(k+1)
     endif
  enddo

end subroutine Interp_lin_3D

