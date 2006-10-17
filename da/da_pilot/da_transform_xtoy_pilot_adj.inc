subroutine da_transform_xtoy_pilot_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_pilot > 0 ) THEN
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
      END DO
   END IF

end subroutine da_transform_xtoy_pilot_adj

