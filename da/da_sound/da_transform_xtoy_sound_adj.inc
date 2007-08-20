subroutine da_transform_xtoy_sound_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_sound_adj")

   do n=iv%plocal(iv%time-1,sound) + 1, iv%plocal(iv%time,sound)
       k = iv % sound(n) % info % levels
       if (k < 1) cycle
       ! [1.2] Interpolate horizontally:
       call da_interp_lin_3d_adj(jo_grad_x%u, iv%sound(n)%loc, jo_grad_y%sound(n)%u, k, iv%sound(n)%zk, k)
       call da_interp_lin_3d_adj(jo_grad_x%v, iv%sound(n)%loc, jo_grad_y%sound(n)%v, k, iv%sound(n)%zk, k)
       call da_interp_lin_3d_adj(jo_grad_x%q, iv%sound(n)%loc, jo_grad_y%sound(n)%q, k, iv%sound(n)%zk, k)
       call da_interp_lin_3d_adj(jo_grad_x%t, iv%sound(n)%loc, jo_grad_y%sound(n)%t, k, iv%sound(n)%zk, k)
    end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_sound_adj")

end subroutine da_transform_xtoy_sound_adj


