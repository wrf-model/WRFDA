subroutine da_transform_xtoy_pseudo(xa, iv, xp, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   
   if (iv%num_pseudo > 0) then
      ! xyh
      ! y%pseudo(:)%u = 0.0
      ! y%pseudo(:)%v = 0.0
      ! y%pseudo(:)%t = 0.0
      ! y%pseudo(:)%p = 0.0
      ! y%pseudo(:)%q = 0.0

      do n=iv%ob_numb(iv%current_ob_time-1)%pseudo + 1, &
         iv%ob_numb(iv%current_ob_time)%pseudo
 
         ! [1.1] Get horizontal interpolation weights:

         i = iv%pseudo(n)%loc%i
         dy = iv%pseudo(n)%loc%dy
         dym = iv%pseudo(n)%loc%dym
         j = iv%pseudo(n)%loc%j
         dx = iv%pseudo(n)%loc%dx
         dxm = iv%pseudo(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%u, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%v, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%t, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d(xa % p, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%p, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%q, iv%pseudo(n)%zk)
      end do
   end if

end subroutine da_transform_xtoy_pseudo


