subroutine da_residual_gpsref(oi, y, re,               &
                                        np_missing, np_bad_data,  &
                                        np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residiual for gpsref obs
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used, &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (oi%num_GPSRef < 1) return

   n_obs_bad % gpsref % num = number(0, 0, 0)

   do n=1, oi%num_GPSRef
      do k=1, oi%gpsref(n)%info%levels
         np_available = np_available + 1
         re%GPSRef(n)%ref(k) = da_residual(n, k, y%GPSRef(n)%ref(k), &
                                  oi%GPSRef(n)%ref(k), n_obs_bad%gpsref)
      end do
   end do

   np_missing = np_missing + n_obs_bad % gpsref % num % miss
   np_bad_data = np_bad_data + n_obs_bad % gpsref % num % bad
   np_obs_used = np_obs_used + n_obs_bad % gpsref % num % use
   
end subroutine da_residual_gpsref


