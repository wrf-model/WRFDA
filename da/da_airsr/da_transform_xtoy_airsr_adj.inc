subroutine da_transform_xtoy_airsr_adj( iv, xp, jo_grad_y, jo_grad_x )
!-------------------------------------------------------------------------
!  History:
!
!     Creation:        03/07/2006                Syed RH Rizvi        
!
!     Purpose: 
!              Does adjoint computation at AIRS retrieval locations
!-------------------------------------------------------------------------

  IMPLICIT NONE

  TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
  TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
  TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
  TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

  INTEGER                       :: n        ! Loop counter.
  INTEGER                       :: i, j, k  ! Index dimension.
  REAL                          :: dx, dxm  ! 
  REAL                          :: dy, dym  !
!-------------------------------------------------------------------------
  IF ( iv%num_airsr > 0 ) THEN
    do n=iv%ob_numb(iv%current_ob_time-1)%airsr + 1, iv%ob_numb(iv%current_ob_time)%airsr
      if(iv%airsr(n)%loc%proc_domain_with_halo) then

        ! [1.1] Get horizontal interpolation weights:

        i = iv%airsr(n)%loc%i
        j = iv%airsr(n)%loc%j
        dx = iv%airsr(n)%loc%dx
        dy = iv%airsr(n)%loc%dy
        dxm = iv%airsr(n)%loc%dxm
        dym = iv%airsr(n)%loc%dym

        k = iv % airsr(n) % info % levels

        if ( k < 1 ) cycle

        ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
        call da_interp_lin_3d_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airsr(n)%t, k, iv%airsr(n)%zk, k)
        call da_interp_lin_3d_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airsr(n)%q, k, iv%airsr(n)%zk, k)
#else
        call da_interp_lin_3d_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airsr(n)%q(1), k, iv%airsr(n)%zk(1), k)
        call da_interp_lin_3d_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airsr(n)%t(1), k, iv%airsr(n)%zk(1), k)
#endif
      endif
    END DO
  END IF

end subroutine da_transform_xtoy_airsr_adj
