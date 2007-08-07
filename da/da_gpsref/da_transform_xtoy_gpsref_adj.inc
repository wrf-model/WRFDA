subroutine da_transform_xtoy_gpsref_adj(iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: num_levs ! obs vertical levels

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
      iv%ob_numb(iv%current_ob_time)%gpsref
      num_levs = iv % gpsref(n) % info % levels
      call da_interp_lin_3d_adj(jo_grad_x %ref, iv%gpsref(n)%loc,jo_grad_y%gpsref(n)%ref, num_levs, &
         iv%gpsref(n)%zk, num_levs)
   end do

end subroutine da_transform_xtoy_gpsref_adj


