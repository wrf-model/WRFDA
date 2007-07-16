subroutine da_residual_radar(oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for radar obs
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (oi%num_radar > 0) then

      n_obs_bad % rv % num = number_type(0, 0, 0)
      n_obs_bad % rf % num = number_type(0, 0, 0)

      do n=1, oi%num_radar

         do k=1, oi%radar(n)%info%levels

           if (use_radar_rv) then

             np_available = np_available + 1

             re%radar(n)%rv(k) = da_residual(n, k, y%radar(n)%rv(k), &
                                           oi%radar(n)%rv(k), n_obs_bad % rv)
           end if

           if (use_radar_rf) then
             np_available = np_available + 1

             re%radar(n)%rf(k) = da_residual(n, k, y%radar(n)%rf(k), &
                                           oi%radar(n)%rf(k), n_obs_bad % rf)
           end if
         end do
      end do

      np_missing  = np_missing  + n_obs_bad % rv % num % miss  &
                                + n_obs_bad % rf % num % miss
      np_bad_data = np_bad_data + n_obs_bad % rv % num % bad   &
                                + n_obs_bad % rf % num % bad
      np_obs_used = np_obs_used + n_obs_bad % rv % num % use   &
                                + n_obs_bad % rf % num % use
   end if

end subroutine da_residual_radar


