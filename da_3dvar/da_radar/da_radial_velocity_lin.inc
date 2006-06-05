SUBROUTINE da_radial_velocity_Lin(rv,p,u,v,w,qrn,ps0,x,y,z,qrn9)

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps0
   real, intent(in)  :: qrn9
   real, intent(out) :: rv

   real    :: r, alpha, vt

   r=sqrt(x*x+y*y+z*z)
   alpha=(ps0/p)**0.4

   if(qrn9 <= 0.) then
      vt=0.
   end if

   if(qrn9 >  0.) then
      vt=0.675*alpha*qrn9**(-0.875)*qrn
   end if

   rv=u*x+v*y+(w-vt)*z
   rv=rv/r

END SUBROUTINE da_radial_velocity_Lin
