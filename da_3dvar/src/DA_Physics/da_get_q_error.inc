subroutine da_get_q_error( p, t, q, t_error, rh_error, q_error )

   implicit none

   real, intent(in)          :: p        ! Observed pressure.
   real, intent(in)          :: t        ! Observed temperature.
   real, intent(in)          :: q        ! Observed specific humidity.
   real, intent(in)          :: t_error  ! Temperature observation error.
   real, intent(in)          :: rh_error ! RH observation error.
   real, intent(out)         :: q_error  ! q observation error.

   real                      :: es    ! Saturation vapor pressure.
   real                      :: qs    ! Saturation specific humidity.
   real                      :: rh    ! Relative humidity.

   IF ( ABS(p - missing_r) > 1. .AND. ABS(t - missing_r) > 1. .AND. &
        ABS(q - missing_r) > 1 ) THEN

!     Calculate rh:

      CALL DA_TP_To_Qs( t, p, es, qs )

      rh = 100.0 * q / qs
      IF ( rh > 100.0 ) rh = 100.0

!     Get observation error for qv. Is this right?

      q_error = da_e_qv_from_rh ( rh_error, t_error, rh, t, p )

   ELSE
      q_error = missing_r
   END IF

   if (q_error == 0.0) q_error = missing_r

end subroutine da_get_q_error
