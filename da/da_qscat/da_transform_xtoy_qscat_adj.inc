subroutine da_transform_xtoy_qscat_adj(iv, xp, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_qscat > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat
      
         ! [1.1] Get horizontal interpolation weights:

         i = iv%qscat(n)%loc%i
         dy = iv%qscat(n)%loc%dy
         dym = iv%qscat(n)%loc%dym
         j = iv%qscat(n)%loc%j
         dx = iv%qscat(n)%loc%dx
         dxm = iv%qscat(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_Interp_Obs_lin_2D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%u, iv%qscat(n)%zk)
         call da_Interp_Obs_lin_2D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%v, iv%qscat(n)%zk)
#else
         call da_Interp_Obs_lin_2D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%u, iv%qscat(n)%zk)
         call da_Interp_Obs_lin_2D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%v, iv%qscat(n)%zk)
#endif
      end do
   end if

end subroutine da_transform_xtoy_qscat_adj


