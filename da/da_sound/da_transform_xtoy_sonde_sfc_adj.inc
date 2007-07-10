subroutine da_transform_xtoy_sonde_sfc_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_sound > 0) then
      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
            iv%ob_numb(iv%current_ob_time)%sound
            ! [1.1] Get horizontal interpolation weights:

            i = iv%sonde_sfc(n)%loc%i
            j = iv%sonde_sfc(n)%loc%j
            dx = iv%sonde_sfc(n)%loc%dx
            dy = iv%sonde_sfc(n)%loc%dy
            dxm = iv%sonde_sfc(n)%loc%dxm
            dym = iv%sonde_sfc(n)%loc%dym

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, ims, ime, &
               jms, jme, i, j, dx, dy, dxm, dym, &
               jo_grad_y%sonde_sfc(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
            iv%ob_numb(iv%current_ob_time)%sound
            call da_transform_xtopsfc_adj(grid,iv%sonde_sfc(n), &
               jo_grad_y%sonde_sfc(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_sonde_sfc_adj


