subroutine da_calculate_residual_radar( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if ( oi%num_Radar > 0 ) then

      n_obs_bad % rv % num = number(0, 0, 0)
      n_obs_bad % rf % num = number(0, 0, 0)

      do n=1, oi%num_Radar

         do k=1, oi%Radar(n)%info%levels

           if ( use_Radar_rv ) then

             np_available = np_available + 1

             re%Radar(n)%rv(k) = da_residual( n, k, y%Radar(n)%rv(k), &
                                           oi%Radar(n)%rv(k), n_obs_bad % rv )
           end if

           if ( use_Radar_rf ) then
             np_available = np_available + 1

             re%Radar(n)%rf(k) = da_residual( n, k, y%Radar(n)%rf(k), &
                                           oi%Radar(n)%rf(k), n_obs_bad % rf )
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

end subroutine da_calculate_residual_radar
