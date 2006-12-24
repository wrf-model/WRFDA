subroutine da_transform_xtoy_ssmt1 (xa, iv, xp, y)

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   

   if (iv%num_ssmt1 > 0) then
      do n=1, iv % num_ssmt1
         y%ssmt1(n)%t(:) = 0.0

         k = iv % ssmt1(n) % info % levels

         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt1(n)%loc%i
         dy = iv%ssmt1(n)%loc%dy
         dym = iv%ssmt1(n)%loc%dym
         j = iv%ssmt1(n)%loc%j
         dx = iv%ssmt1(n)%loc%dx
         dxm = iv%ssmt1(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#else
         call da_interp_lin_3d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#endif
      end do
   end if

end subroutine da_transform_xtoy_ssmt1


