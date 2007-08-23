subroutine da_interp_lin_2d_newer(fm2d, loc, k, n1, n2, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                 intent(in)  :: fm2d(ims:ime,jms:jme) ! Input variable
   type(model_loc_type_new), intent(in)  :: loc
   integer,              intent(in)  :: k                     ! level
   integer,              intent(in)  :: n1,n2                 ! Range of obs
   real,                 intent(out) :: fo2d(n1:n2)           ! Output variable 
   
   integer :: n

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d_newer")

   do n=n1,n2
      fo2d(n) = loc%dym(k,n)*(loc%dxm(k,n)*fm2d(loc%i(k,n),loc%j(k,n)) + loc%dx(k,n)*fm2d(loc%i(k,n)+1,loc%j(k,n))) &
         + loc%dy(k,n) *(loc%dxm(k,n)*fm2d(loc%i(k,n),loc%j(k,n)+1) + loc%dx(k,n)*fm2d(loc%i(k,n)+1,loc%j(k,n)+1))
   end do

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d_newer")

end subroutine da_interp_lin_2d_newer


