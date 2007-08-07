subroutine da_interp_lin_2d_adj(fm2d, loc, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(inout) :: fm2d(ims:ime,jms:jme) ! Input variable
   type (model_loc_type), intent(in)    :: loc
   real,                  intent(in)    :: fo2d   

   integer :: i, j
   real    :: dx, dxm, dy, dym

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d_adj")

   i   = loc%i
   dy  = loc%dy
   dym = loc%dym
   j   = loc%j
   dx  = loc%dx
   dxm = loc%dxm

   fm2d(i  ,j ) = dym*dxm* fo2d + fm2d(i  ,j )
   fm2d(i+1,j ) = dym*dx * fo2d + fm2d(i+1,j )
   fm2d(i  ,j+1) = dy *dxm* fo2d + fm2d(i  ,j+1)
   fm2d(i+1,j+1) = dy *dx * fo2d + fm2d(i+1,j+1)

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d_adj")

end subroutine da_interp_lin_2d_adj


