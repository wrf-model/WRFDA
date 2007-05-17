subroutine da_radial_velocity(rv,p,u,v,w,qrn,ps,x,y,z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(out) :: rv

   real              :: r, alpha, vt

   r=sqrt(x*x+y*y+z*z)
   alpha=(ps/p)**0.4

   if (qrn <= 0.0) vt=0.
   if (qrn >  0.0) vt=5.4*alpha*qrn**0.125
   rv=u*x+v*y+(w-vt)*z
   rv=rv/r

end subroutine da_radial_velocity


