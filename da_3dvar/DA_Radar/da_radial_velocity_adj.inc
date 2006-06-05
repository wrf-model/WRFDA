SUBROUTINE da_radial_velocity_Adj(rv,p,u,v,w,qrn,ps0,x,y,z,qrn9)

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p
   real, intent(in)  :: qrn9
   real, intent(in)  :: ps0
   real, intent(inout) :: rv
   real, intent(inout) :: u, v, w, qrn

   real    :: r, alpha, vt

   r=sqrt(x*x+y*y+z*z)
   alpha=(ps0/p)**0.4

   rv = rv/r
   u = u + rv*x
   v = v + rv*y
   w = w + rv*z
   vt = -rv*z

   if(qrn9 >  0.) then
      qrn = qrn + vt*0.675*alpha*qrn9**(-0.875)
   end if

END SUBROUTINE da_radial_velocity_Adj
