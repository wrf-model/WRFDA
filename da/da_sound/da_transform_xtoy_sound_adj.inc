subroutine da_transform_xtoy_sound_adj(iv, xp, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

  implicit none

  type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
  type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
  type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
  type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

  integer                       :: n        ! Loop counter.
  integer                       :: i, j, k  ! Index dimension.
  real                          :: dx, dxm  ! 
  real                          :: dy, dym  !


   if (iv%num_sound > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

         ! [1.1] Get horizontal interpolation weights:

         i = iv%sound(n)%loc%i
         j = iv%sound(n)%loc%j
         dx = iv%sound(n)%loc%dx
         dy = iv%sound(n)%loc%dy
         dxm = iv%sound(n)%loc%dxm
         dym = iv%sound(n)%loc%dym

         k = iv % sound(n) % info % levels

         if (k < 1) cycle

         ! [1.2] Interpolate horizontally:
         call da_interp_lin_3d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%u, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%v, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%q, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%t, k, iv%sound(n)%zk, k)
      end do
   end if

end subroutine da_transform_xtoy_sound_adj


