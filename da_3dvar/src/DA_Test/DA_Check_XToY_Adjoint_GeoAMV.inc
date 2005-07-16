SUBROUTINE DA_Check_XToY_Adjoint_GeoAMV ( oi, y, adjtest_lhs, pertile_lhs )
!----------------------------------------------------------------------------
!  History:
!    Created:      03/04/2005                            Syed  RH  Rizvi           
!    Purpose:  Adjoint Test for Geo. AMVs Obs
!----------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(in)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(inout)  :: y             ! y = h (xa)
   REAL          , INTENT(inout)  :: adjtest_lhs, pertile_lhs

   INTEGER                        :: n, k          ! Loop counter.

!
!----------------------------------------------------------------------------

   IF ( oi%num_geoamv > 0 ) THEN

   DO n=1, oi%num_geoamv
      if( oi%geoamv(n)%loc%proc_domain_with_halo ) then
         if( oi%geoamv(n)%loc%proc_domain ) then
            do k=1, oi%geoamv(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           ( y%geoamv(n)%u(k)/typical_u_rms )**2 + &
                           ( y%geoamv(n)%v(k)/typical_v_rms )**2

            end do
         end if

         do k=1, oi%geoamv(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        ( y%geoamv(n)%u(k)/typical_u_rms )**2 + &
                        ( y%geoamv(n)%v(k)/typical_v_rms )**2

            y%geoamv(n)%u(k)= y%geoamv(n)%u(k) / typical_u_rms ** 2
            y%geoamv(n)%v(k)= y%geoamv(n)%v(k) / typical_v_rms ** 2
         end do
         end if
   END DO

   END IF

END SUBROUTINE DA_Check_XToY_Adjoint_GeoAMV

