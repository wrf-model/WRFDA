SUBROUTINE DA_Cal_Residual_SSMI( oi, y, re,                           &
                                       np_missing, np_bad_data,             &
                                       np_obs_used, np_available            )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Obs increment vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   IF ( oi%num_ssmi_tb .gt. 0 ) THEN

      CALL DA_Cal_Residual_SSMI_Tb( oi, y, re,  &
                                         np_missing, np_bad_data, &
                                         np_obs_used, np_available )

   ELSE IF ( oi%num_ssmi_retrieval .gt. 0) THEN

      CALL DA_Cal_Residual_SSMI_Rv( oi, y, re,  &
                                         np_missing, np_bad_data, &
                                         np_obs_used, np_available )
   ENDIF

END SUBROUTINE DA_Cal_Residual_SSMI
