subroutine da_transform_xtoy_pilot_adj( iv, xp, jo_grad_y, jo_grad_x )

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j, k  ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if ( iv%num_pilot > 0 ) then
      do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot
         k = iv % pilot(n) % info % levels

         if ( k < 1 ) cycle

         ! [1.3] Get horizontal interpolation weights:

         i = iv%pilot(n)%loc%i
         dy = iv%pilot(n)%loc%dy
         dym = iv%pilot(n)%loc%dym
         j = iv%pilot(n)%loc%j
         dx = iv%pilot(n)%loc%dx
         dxm = iv%pilot(n)%loc%dxm

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%pilot(n)%u, k, iv%pilot(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%pilot(n)%v, k, iv%pilot(n)%zk, k)
#else
         call da_interp_lin_3d_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%pilot(n)%u(1), k, iv%pilot(n)%zk(1), k)
         call da_interp_lin_3d_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%pilot(n)%v(1), k, iv%pilot(n)%zk(1), k)
#endif
      end do
   end if

end subroutine da_transform_xtoy_pilot_adj

