SUBROUTINE DA_Transform_XToY_GeoAMV ( xa, iv, xp, y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: X to Y Transform operator for Geo. AMV's
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j , k ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_geoamv > 0 ) THEN


      do n=1, iv % num_geoamv

       if(iv%geoamv(n)%loc%proc_domain_with_halo) then

          y%geoamv(n)%u(:) = 0.0
          y%geoamv(n)%v(:) = 0.0

       k = iv % geoamv(n) % info % levels
         if ( k < 1 ) cycle

         i = iv%geoamv(n)%loc%i
         dy = iv%geoamv(n)%loc%dy
         dym = iv%geoamv(n)%loc%dym
         j = iv%geoamv(n)%loc%j
         dx = iv%geoamv(n)%loc%dx
         dxm = iv%geoamv(n)%loc%dxm

!   Interpolate horizontally:

#ifndef DEREF_KLUDGE
         call Interp_lin_3D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%geoamv(n)%u, k, iv%geoamv(n)%zk, k)
         call Interp_lin_3D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%geoamv(n)%v, k, iv%geoamv(n)%zk, k)
#else
         call Interp_lin_3D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%geoamv(n)%u(1), k, iv%geoamv(n)%zk(1), k)
         call Interp_lin_3D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%geoamv(n)%v(1), k, iv%geoamv(n)%zk(1), k)
#endif

       endif

      end do

   ENDIF

END SUBROUTINE DA_Transform_XToY_GeoAMV

