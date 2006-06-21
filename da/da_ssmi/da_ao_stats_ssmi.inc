subroutine da_ao_stats_ssmi ( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.

   num_ssmi_tb = wrf_dm_sum_integer( oi%num_ssmi_tb )
   num_ssmi_retrieval = wrf_dm_sum_integer( oi%num_ssmi_retrieval )

   IF ( num_ssmi_tb .gt. 0 ) THEN

      call da_ao_stats_ssmi_tb ( stats_unit, oi, re )
      num_ssmi_tot = num_ssmi_tb_tot

   ELSE IF ( num_ssmi_retrieval .gt. 0) THEN

      call da_ao_stats_ssmi_rv( stats_unit, oi, re )
      num_ssmi_tot = num_ssmi_rv_tot

   ENDIF

end subroutine da_ao_stats_ssmi

