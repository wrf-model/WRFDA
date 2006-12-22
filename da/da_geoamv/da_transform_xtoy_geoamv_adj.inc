subroutine da_transform_xtoy_geoamv_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Geo. AMV's
   !
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

   if ( iv%num_geoamv > 0 ) then
!xyh      do n=1, iv % num_geoamv
      do n=iv%ob_numb(iv%current_ob_time-1)%geoamv+1, iv%ob_numb(iv%current_ob_time)%geoamv

         k = iv % geoamv(n) % info % levels

         if ( k < 1 ) cycle

         i = iv%geoamv(n)%loc%i
         dy = iv%geoamv(n)%loc%dy
         dym = iv%geoamv(n)%loc%dym
         j = iv%geoamv(n)%loc%j
         dx = iv%geoamv(n)%loc%dx
         dxm = iv%geoamv(n)%loc%dxm

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                   jo_grad_y%geoamv(n)%u, k, iv%geoamv(n)%zk, k)
         call da_interp_lin_3d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                   jo_grad_y%geoamv(n)%v, k, iv%geoamv(n)%zk, k)
#else
         call da_interp_lin_3d_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, &
                                   i, j, dx, dy, dxm, dym, &
                                   jo_grad_y%geoamv(n)%u(1), k, &
                                   iv%geoamv(n)%zk(1), k)
         call da_interp_lin_3d_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, &
                                   i, j, dx, dy, dxm, dym, &
                                   jo_grad_y%geoamv(n)%v(1), k, &
                                   iv%geoamv(n)%zk(1), k)
#endif
      end do
   end if

end subroutine da_transform_xtoy_geoamv_adj


