subroutine da_transform_xtoy_polaramv_adj (iv, xp, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Polar AMVs 
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n, k     ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_polaramv > 0) then
      ! xyh
      !  do n=1, iv % num_polaramv
      do n=iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, &
         iv%ob_numb(iv%current_ob_time)%polaramv

         k = iv % polaramv(n) % info % levels

         if (k < 1) cycle

         i = iv%polaramv(n)%loc%i
         dy = iv%polaramv(n)%loc%dy
         dym = iv%polaramv(n)%loc%dym
         j = iv%polaramv(n)%loc%j
         dx = iv%polaramv(n)%loc%dx
         dxm = iv%polaramv(n)%loc%dxm

         call da_interp_lin_3d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
            jo_grad_y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
            jo_grad_y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
      end do
   end if

end subroutine da_transform_xtoy_polaramv_adj


