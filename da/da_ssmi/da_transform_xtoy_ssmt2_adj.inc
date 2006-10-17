subroutine da_transform_xtoy_ssmt2_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_ssmt2 > 0 ) THEN
      DO n=1, iv % num_ssmt2
         k = iv % ssmt2(n) % info % levels

         if ( k < 1 ) cycle
      
         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt2(n)%loc%i
         dy = iv%ssmt2(n)%loc%dy
         dym = iv%ssmt2(n)%loc%dym
         j = iv%ssmt2(n)%loc%j
         dx = iv%ssmt2(n)%loc%dx
         dxm = iv%ssmt2(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d_adj(jo_grad_x % rh, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#else
         call da_interp_lin_3d_adj(jo_grad_x % rh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#endif
      END DO 
   END IF

end subroutine da_transform_xtoy_ssmt2_adj


