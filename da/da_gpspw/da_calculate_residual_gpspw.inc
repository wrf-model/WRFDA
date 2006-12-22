subroutine da_calculate_residual_gpspw( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   if ( oi%num_Gpspw > 0 ) then
      n_obs_bad % q % num = number(0, 0, 0)
      do n=1, oi%num_Gpspw
         np_available = np_available + 1
         re%gpspw(n)%tpw = da_residual(n, 0, y%gpspw(n)%tpw, &
            oi%gpspw(n)%tpw, n_obs_bad % q )
      end do

      np_missing  = np_missing  + n_obs_bad % q % num % miss 
      np_bad_data = np_bad_data + n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % q % num % use

   end if

end subroutine da_calculate_residual_gpspw


