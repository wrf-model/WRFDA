subroutine da_transform_xtoy_qscat( xa, iv, xp, y )

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   
   if ( iv%num_qscat > 0 ) then
      !xyh y%qscat(:)%u = 0.0
      !xyh y%qscat(:)%v = 0.0

      do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat
         ! [1.1] Get horizontal interpolation weights:

         i = iv%qscat(n)%loc%i
         dy = iv%qscat(n)%loc%dy
         dym = iv%qscat(n)%loc%dym
         j = iv%qscat(n)%loc%j
         dx = iv%qscat(n)%loc%dx
         dxm = iv%qscat(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%u, iv%qscat(n)%zk)
         call da_Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%v, iv%qscat(n)%zk)
#else
         call da_Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%u, iv%qscat(n)%zk)
         call da_Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%v, iv%qscat(n)%zk)
#endif
      end do
   end if

end subroutine da_transform_xtoy_qscat


