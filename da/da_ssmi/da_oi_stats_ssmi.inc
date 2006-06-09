SUBROUTINE da_oi_stats_ssmi ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (IN)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (IN)      :: oi            ! OI

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.

   num_ssmi_tb = wrf_dm_sum_integer( oi%num_ssmi_tb )
   num_ssmi_retrieval = wrf_dm_sum_integer( oi%num_ssmi_retrieval )

   IF ( num_ssmi_tb .gt. 0 ) THEN

           CALL da_oi_stats_ssmi_tb ( stats_unit, oi )

   ELSE IF ( num_ssmi_retrieval .gt. 0) THEN

           CALL da_oi_stats_ssmi_rv ( stats_unit, oi )

   ENDIF

END SUBROUTINE da_oi_stats_ssmi
