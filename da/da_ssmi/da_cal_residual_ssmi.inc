subroutine da_cal_residual_ssmi( oi, y, re,                           &
                                       np_missing, np_bad_data,             &
                                       np_obs_used, np_available            )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Obs increment vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(out)       :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   if ( oi%num_ssmi_tb .gt. 0 ) then

      call da_cal_residual_ssmi_tb( oi, y, re,  &
                                         np_missing, np_bad_data, &
                                         np_obs_used, np_available )

   else if ( oi%num_ssmi_retrieval .gt. 0) then

      call da_cal_residual_ssmi_rv( oi, y, re,  &
                                         np_missing, np_bad_data, &
                                         np_obs_used, np_available )
   end if

end subroutine da_cal_residual_ssmi


