subroutine da_transform_xtoy_geoamv_adj (iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Geo. AMVs
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n, k     ! Loop counter.

   do n=iv%plocal(iv%time-1,geoamv)+1, iv%plocal(iv%time,geoamv)
      k = iv % geoamv(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d_adj(jo_grad_x%u,iv%geoamv(n)%loc, jo_grad_y%geoamv(n)%u,k, iv%geoamv(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%v,iv%geoamv(n)%loc, jo_grad_y%geoamv(n)%v,k, iv%geoamv(n)%zk, k)
   end do

end subroutine da_transform_xtoy_geoamv_adj


