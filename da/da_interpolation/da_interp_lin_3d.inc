subroutine da_interp_lin_3d(fm3d, xp, &
                         i, j, dx, dy, dxm, dym, &
                         fo3d, ml, zk, nl)

   implicit none

   type (xpose_type),      intent(in)  :: xp    ! Dimensions and xpose buffers.
   integer,                intent(in)  :: i, j
   real,                   intent(in)  :: dx, dxm, dy, dym
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                           intent(in)  :: fm3d    ! Input variable
   integer,                intent(in)  :: ml, nl                        
   real,    dimension(ml), intent(inout) :: fo3d  ! Output variable 
   real,    dimension(nl), intent(in)  :: zk

   integer                             :: kk, k
   real                                :: dz, dzm
   real, dimension(xp%kms:xp%kme)      :: fmz

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_3d")

   fmz(:) = 0.0

   fmz(xp%kts:xp%kte) = dym*(dxm*fm3d(i  ,j  ,xp%kts:xp%kte) &
                            +dx *fm3d(i+1,j  ,xp%kts:xp%kte)) &
                      + dy *(dxm*fm3d(i  ,j+1,xp%kts:xp%kte) &
                            +dx *fm3d(i+1,j+1,xp%kts:xp%kte))

   fo3d(:) = 0.0

   do kk = 1, nl
      if (zk(kk) > 0.0) then
         call da_togrid(zk(kk), xp%kts, xp%kte, k, dz, dzm)
         fo3d(kk) = dzm*fmz(k) + dz*fmz(k+1)
      end if
   end do

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_3d")

end subroutine da_interp_lin_3d


