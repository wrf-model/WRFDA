subroutine da_transform_xtoy_profiler_adj(iv, jo_grad_y, jo_grad_x)

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

   if (iv%num_profiler > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%profiler + 1, &
         iv%ob_numb(iv%current_ob_time)%profiler
         k = iv % profiler(n) % info % levels

         if (k < 1) cycle
      
         ! [1.3] Get horizontal interpolation weights:

         i = iv%profiler(n)%loc%i
         dy = iv%profiler(n)%loc%dy
         dym = iv%profiler(n)%loc%dym
         j = iv%profiler(n)%loc%j
         dx = iv%profiler(n)%loc%dx
         dxm = iv%profiler(n)%loc%dxm

         call da_interp_lin_3d_adj(jo_grad_x % u, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%profiler(n)%u, k, iv%profiler(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % v, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%profiler(n)%v, k, iv%profiler(n)%zk, k)
      end do 
   end if

end subroutine da_transform_xtoy_profiler_adj


