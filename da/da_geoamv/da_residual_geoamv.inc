subroutine da_residual_geoamv(oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates residual vector for Geo. CMV's
   !               
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual vector (O-A).

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (oi%num_geoamv > 0) then

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)

      do n=1, oi%num_geoamv
         do k=1, oi%geoamv(n)%info%levels
            np_available = np_available + 2

            re%geoamv(n)%u(k) = da_residual(n, 0, y%geoamv(n)%u(k), &
                                          oi%geoamv(n)%u(k), n_obs_bad % u)
            re%geoamv(n)%v (k)= da_residual(n, 0, y%geoamv(n)%v(k), &
                                       oi%geoamv(n)%v(k), n_obs_bad % v)
         end do
      end do
      
      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use

   end if

end subroutine da_residual_geoamv


