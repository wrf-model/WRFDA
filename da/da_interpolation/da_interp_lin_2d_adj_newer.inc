subroutine da_interp_lin_2d_adj_newer(fm2d, loc, k, n1,n2, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                 intent(inout) :: fm2d(ims:ime,jms:jme) ! 'Input' variable modified by adjoint
   type(model_loc_type_new), intent(in)    :: loc
   integer,              intent(in)    :: k                     ! level
   integer,              intent(in)    :: n1,n2                 ! Range of obs
   real,                 intent(in)    :: fo2d(n1:n2)           ! 'Output' variable unchanged by adjoint

   integer :: n

   if (trace_use) call da_trace_entry("da_interp_lin_2d_adj_newer")

   do n=n1,n2
      fm2d(loc%i(k,n)  ,loc%j(k,n))   = loc%dym(k,n) * loc%dxm(k,n) * fo2d(n) + fm2d(loc%i(k,n)  ,loc%j(k,n))
      fm2d(loc%i(k,n)+1,loc%j(k,n))   = loc%dym(k,n) * loc%dx(k,n)  * fo2d(n) + fm2d(loc%i(k,n)+1,loc%j(k,n))
      fm2d(loc%i(k,n)  ,loc%j(k,n)+1) = loc%dy(k,n)  * loc%dxm(k,n) * fo2d(n) + fm2d(loc%i(k,n)  ,loc%j(k,n)+1)
      fm2d(loc%i(k,n)+1,loc%j(k,n)+1) = loc%dy(k,n)  * loc%dx(k,n)  * fo2d(n) + fm2d(loc%i(k,n)+1,loc%j(k,n)+1)
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_2d_adj_newer")

end subroutine da_interp_lin_2d_adj_newer


