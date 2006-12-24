subroutine da_ao_stats_ssmi  (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.

   num_ssmi_tot = 0
   num_ssmi_tb = wrf_dm_sum_integer (oi%num_ssmi_tb)
   num_ssmi_retrieval = wrf_dm_sum_integer (oi%num_ssmi_retrieval)

   if (num_ssmi_tb .gt. 0) then
      call da_ao_stats_ssmi_tb  (stats_unit, oi, re)
      num_ssmi_tot = num_ssmi_tb_tot
   else if (num_ssmi_retrieval .gt. 0) then
      call da_ao_stats_ssmi_rv (stats_unit, oi, re)
      num_ssmi_tot = num_ssmi_rv_tot
   end if

end subroutine da_ao_stats_ssmi


