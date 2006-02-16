SUBROUTINE da_calculate_residual_rad( iv, y, re,                 &
                                        np_missing, np_bad_data, &
                                        np_obs_used, np_available )
!------------------------------------------------------------------------------
!  PURPOSE: Calculate Obs Residual and counting obs number.
!
!  METHOD:  re = (d - H delta_x)
!
!  HISTORY: 08/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)        :: iv     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n, k, i

   IF ( iv%num_inst < 1 ) return
   DO i = 1, iv%num_inst

      IF ( iv%instid(i)%num_rad < 1 ) cycle

      n_obs_bad % rad % num = number(0, 0, 0)

      DO n=1, iv%instid(i)%num_rad

      if( iv%instid(i)%rad(n)%loc%proc_domain_with_halo ) then

      do k=1, iv%instid(i)%nchan

         np_available = np_available + iv%instid(i)%nchan

         re%instid(i)%rad(n)%tb(k) = residual( n, k, y%instid(i)%rad(n)%tb(k), &
                                      iv%instid(i)%rad(n)%tb(k), n_obs_bad % rad )
      end do

      endif

      END DO

    END DO

      np_missing  = np_missing  + n_obs_bad % rad % num % miss
      np_bad_data = np_bad_data + n_obs_bad % rad % num % bad
      np_obs_used = np_obs_used + n_obs_bad % rad % num % use 
   
END SUBROUTINE da_calculate_residual_rad

