subroutine da_radial_velocity_new(rv,p,u,v,w,qrn,ps,x,y,z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(out) :: rv

   real :: r, alpha, vt

   if (trace_use) call da_trace_entry("da_radial_velocity_new")

   r=sqrt(x*x+y*y+z*z)

   if (qrn <= 0.0) then
      vt=0.0
   else
      alpha=(ps/p)**0.4
      vt=5.4*alpha*qrn**0.125
   end if

   rv=u*x+v*y+(w-vt)*z
   rv=rv/r

   if (trace_use) call da_trace_exit("da_radial_velocity_new")

end subroutine da_radial_velocity_new


