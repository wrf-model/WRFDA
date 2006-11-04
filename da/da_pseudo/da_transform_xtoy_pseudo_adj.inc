subroutine da_transform_xtoy_pseudo_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   if (trace_use) call da_trace_entry("da_transform_xtoy_pseudo_adj")

   IF ( iv%num_pseudo > 0 ) THEN
!xyh      DO n=1, iv % num_pseudo
      do n=iv%ob_numb(iv%current_ob_time-1)%pseudo + 1, iv%ob_numb(iv%current_ob_time)%pseudo
         ! [1.1] Get horizontal interpolation weights:

         i = iv%pseudo(n)%loc%i
         dy = iv%pseudo(n)%loc%dy
         dym = iv%pseudo(n)%loc%dym
         j = iv%pseudo(n)%loc%j
         dx = iv%pseudo(n)%loc%dx
         dxm = iv%pseudo(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_obs_lin_2d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%u, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%v, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%q, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % p, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%p, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%t, iv%pseudo(n)%zk)
#else
         call da_interp_obs_lin_2d_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%u, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%v, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%q, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % p(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%pseudo(n)%p, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%pseudo(n)%t, iv%pseudo(n)%zk)
#endif
      END DO
   END IF

   if (trace_use) call da_trace_exit("da_transform_xtoy_pseudo_adj")

end subroutine da_transform_xtoy_pseudo_adj

