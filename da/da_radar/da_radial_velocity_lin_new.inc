subroutine da_radial_velocity_lin_new(rv,p,u,v,w,qrn,ps,x,y,z,qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(in)  :: qrn9
   real, intent(out) :: rv

   real    :: r, alpha, vt

   if (trace_use) call da_trace_entry("da_radial_velocity_lin_new")

   r     = sqrt(x*x+y*y+z*z)

   if (qrn9 <= 0.0) then
      vt=0.0
   else
      alpha = (ps/p)**0.4
      vt=0.675*alpha*qrn9**(-0.875)*qrn
   end if

   rv = u*x+v*y+(w-vt)*z
   rv = rv/r

   if (trace_use) call da_trace_exit("da_radial_velocity_lin_new")

end subroutine da_radial_velocity_lin_new


