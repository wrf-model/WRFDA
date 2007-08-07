subroutine da_transform_xtoy_ships (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout)  :: grid
   type (ob_type),    intent(in)     :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout)  :: y        ! y = h (grid%xa) (linear)

   integer :: n ! Loop counter.


   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
         call da_interp_obs_lin_2d(grid%xa % u, iv%ships(n)%loc, y%ships(n)%u, iv%ships(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % v, iv%ships(n)%loc, y%ships(n)%v, iv%ships(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % t, iv%ships(n)%loc, y%ships(n)%t, iv%ships(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % q, iv%ships(n)%loc, y%ships(n)%q, iv%ships(n)%zk)

         call da_interp_lin_2d(grid%xa % psfc, iv%ships(n)%loc, y%ships(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
       ! [2.0] Surface assimilation approach 2
       do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
          iv%ob_numb(iv%current_ob_time)%ships
          call da_transform_xtopsfc(grid,iv%ships(n),y%ships(n))
      end do
   end if

end subroutine da_transform_xtoy_ships


