subroutine da_transform_xtoy_gpsref_adj(iv, xp, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  !
   real                          :: dy, dym  !
   integer                       :: num_levs ! obs vertical levels

   if (iv%num_gpsref < 1) return

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
      iv%ob_numb(iv%current_ob_time)%gpsref

      num_levs = iv % gpsref(n) % info % levels

      i = iv%gpsref(n)%loc%i
      j = iv%gpsref(n)%loc%j
      dx = iv%gpsref(n)%loc%dx
      dy = iv%gpsref(n)%loc%dy
      dxm = iv%gpsref(n)%loc%dxm
      dym = iv%gpsref(n)%loc%dym

#ifndef DEREF_KLUDGE
      call da_interp_lin_3d_adj(jo_grad_x %ref, xp, i, j, dx, dy, dxm, dym, &
                             jo_grad_y%gpsref(n)%ref, num_levs, &
                             iv%gpsref(n)%zk, num_levs)
#else
      call da_interp_lin_3d_adj(jo_grad_x %ref(xp%ims,xp%jms,xp%kms), xp, &
                             i, j, dx, dy, dxm, dym, &
                             jo_grad_y%gpsref(n)%ref(1), num_levs, &
                             iv%gpsref(n)%zk(1), num_levs)
#endif
   end do

end subroutine da_transform_xtoy_gpsref_adj


