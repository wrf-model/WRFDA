SUBROUTINE DA_Calculate_Residual_GPSRef( oi, y, re,               &
                                        np_missing, np_bad_data,  &
                                        np_obs_used, np_available )
! -------------------------------------------------------------------------
!  Called by DA_Minimisation/da_calculate_residual.inc
!
!                                     Y.-R. Guo  06/02/2004
! -------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used, &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n, k

   IF ( oi%num_GPSRef < 1 ) return

   n_obs_bad % gpsref % num = number(0, 0, 0)

   DO n=1, oi%num_GPSRef

      if( oi%gpsref(n)%loc%proc_domain_with_halo ) then

         do k=1, oi%gpsref(n)%info%levels
            np_available = np_available + 1
            re%GPSRef(n)%ref(k) = residual( n, k, y%GPSRef(n)%ref(k), &
                                  oi%GPSRef(n)%ref(k), n_obs_bad%gpsref )
         end do

      endif

   END DO

   np_missing = np_missing + n_obs_bad % gpsref % num % miss
   np_bad_data = np_bad_data + n_obs_bad % gpsref % num % bad
   np_obs_used = np_obs_used + n_obs_bad % gpsref % num % use
   
END SUBROUTINE DA_Calculate_Residual_GPSRef

