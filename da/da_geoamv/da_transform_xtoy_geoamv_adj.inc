subroutine da_transform_xtoy_geoamv_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Geo. AMV's
   !
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n, k     ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_geoamv > 0 ) THEN
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
      END DO
   END IF

end subroutine da_transform_xtoy_geoamv_adj
