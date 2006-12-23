subroutine da_calculate_residual_qscat( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   if (oi%num_qscat > 0) then

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)

      do n=1, oi%num_qscat
         np_available = np_available + 2

         re%qscat(n)%u = da_residual( n, 0, y%qscat(n)%u, &
                                      oi%qscat(n)%u, n_obs_bad % u)
         re%qscat(n)%v = da_residual( n, 0, y%qscat(n)%v, &
                                   oi%qscat(n)%v, n_obs_bad % v)
      end do

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use
   
   end if

end subroutine da_calculate_residual_qscat


