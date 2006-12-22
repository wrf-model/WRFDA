subroutine da_oi_stats_ssmi ( stats_unit, oi )

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   integer  :: num_ssmi_tb, num_ssmi_retrieval       ! # obs over global domain.

   num_ssmi_tb = wrf_dm_sum_integer( oi%num_ssmi_tb )
   num_ssmi_retrieval = wrf_dm_sum_integer( oi%num_ssmi_retrieval )

   if ( num_ssmi_tb .gt. 0 ) then

           call da_oi_stats_ssmi_tb ( stats_unit, oi )

   else if ( num_ssmi_retrieval .gt. 0) then

           call da_oi_stats_ssmi_rv ( stats_unit, oi )

   end if

end subroutine da_oi_stats_ssmi


