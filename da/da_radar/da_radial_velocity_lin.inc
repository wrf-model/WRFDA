subroutine da_radial_velocity_lin(rv,p,u,v,w,qrn,ps,x,y,z,qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(in)  :: qrn9
   real, intent(out) :: rv

   real    :: r, alpha, vt

   r=sqrt(x*x+y*y+z*z)
   alpha=(ps/p)**0.4

   if (qrn9 <= 0.0) then
      vt=0.0
   end if

   if (qrn9 >  0.0) then
      vt=0.675*alpha*qrn9**(-0.875)*qrn
   end if

   rv=u*x+v*y+(w-vt)*z
   rv=rv/r

end subroutine da_radial_velocity_lin


