subroutine da_interp_lin_2d(fm2d, loc, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(in)  :: fm2d(ims:ime,jms:jme) ! Input variable
   type (model_loc_type), intent(in)  :: loc
   real,                  intent(out) :: fo2d  ! Output variable  


   integer :: i, j
   real    :: dx, dxm, dy, dym

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d")

   i   = loc%i
   dy  = loc%dy
   dym = loc%dym
   j   = loc%j
   dx  = loc%dx
   dxm = loc%dxm

   fo2d = dym*(dxm*fm2d(i,j) + dx*fm2d(i+1,j)) + dy*(dxm*fm2d(i,j+1) + dx*fm2d(i+1,j+1))

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d")

end subroutine da_interp_lin_2d


