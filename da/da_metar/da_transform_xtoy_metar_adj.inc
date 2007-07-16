subroutine da_transform_xtoy_metar_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid          ! first guess state.
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (trace_use) call da_trace_entry("da_transform_xtoy_metar_adj")

   if (iv%num_metar > 0) then
      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, &
            iv%ob_numb(iv%current_ob_time)%metar

            ! [1.1] Get horizontal interpolation weights:

            i = iv%metar(n)%loc%i
            dy = iv%metar(n)%loc%dy
            dym = iv%metar(n)%loc%dym
            j = iv%metar(n)%loc%j
            dx = iv%metar(n)%loc%dx
            dxm = iv%metar(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, i, j, dx, dy, &
               dxm, dym, jo_grad_y%metar(n)%u, iv%metar(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, i, j, dx, dy, &
               dxm, dym, jo_grad_y%metar(n)%v, iv%metar(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, i, j, dx, dy, &
               dxm, dym, jo_grad_y%metar(n)%t, iv%metar(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, i, j, dx, dy, &
               dxm, dym, jo_grad_y%metar(n)%q, iv%metar(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, ims, ime, &
               jms, jme, i, j, dx, dy, dxm, dym, jo_grad_y%metar(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, &
            iv%ob_numb(iv%current_ob_time)%metar
            call da_transform_xtopsfc_adj(grid,iv%metar(n), &
                                            jo_grad_y%metar(n),jo_grad_x)
         end do
      end if
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_metar_adj")

end subroutine da_transform_xtoy_metar_adj


