SUBROUTINE DA_OI_Stats_SSMI ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (IN)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (IN)      :: oi            ! OI

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.

   num_ssmi_tb = wrf_dm_sum_integer( oi%num_ssmi_tb )
   num_ssmi_retrieval = wrf_dm_sum_integer( oi%num_ssmi_retrieval )

   IF ( num_ssmi_tb .gt. 0 ) THEN

           CALL DA_OI_Stats_SSMI_Tb ( stats_unit, oi )

   ELSE IF ( num_ssmi_retrieval .gt. 0) THEN

           CALL DA_OI_Stats_SSMI_Rv ( stats_unit, oi )

   ENDIF

END SUBROUTINE DA_OI_Stats_SSMI
