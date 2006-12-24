subroutine da_residual_airep(oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (oi%num_Airep > 0) then
      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)

      do n=1, oi%num_Airep
         do k=1, oi%Airep(n)%info%levels
            np_available = np_available + 3
            re%Airep(n)%u(k) = da_residual(n, k, y%Airep(n)%u(k), &
                                    oi%Airep(n)%u(k), n_obs_bad % u)
            re%Airep(n)%v(k) = da_residual(n, k, y%Airep(n)%v(k), &
                                    oi%Airep(n)%v(k), n_obs_bad % v)
            re%Airep(n)%t(k) = da_residual(n, k, y%Airep(n)%t(k), &
                                    oi%Airep(n)%t(k), n_obs_bad % t)
         end do
      end do

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                    n_obs_bad % v % num % miss + n_obs_bad % t % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                      n_obs_bad % v % num % bad + n_obs_bad % t % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                     n_obs_bad % v % num % use + n_obs_bad % t % num % use

   end if

end subroutine da_residual_airep


