subroutine da_wrfvar_finalize

   !-------------------------------------------------------------------------
   ! Purpose: Tidy up at the end
   !-------------------------------------------------------------------------

   use module_domain, only : domain, head_grid

   use da_control, only : trace_use, cost_unit, stderr, &
      check_max_iv_unit,grad_unit,ierr,num_alpha_corr_types, &
      rootproc,stats_unit, unit_start, rtm_option_crtm, &
      unit_end, jo_unit, unit_used
   use da_radiance1, only : num_tovs_before, tovs_recv_pe,tovs_copy_count, &
      tovs_send_pe,tovs_send_count,tovs_recv_start, num_tovs_after, &
      tovs_send_start
   use da_wrfvar_io, only : da_med_initialdata_output
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_tools1, only : da_free_unit
   use da_wrf_interfaces, only : wrf_error_fatal,med_shutdown_io
   use da_reporting, only : da_message

#ifdef CRTM
   use module_radiance, only : crtm_destroy
   use da_crtm, only : channelinfo, sensor_descriptor
   use da_control, only : rtm_option, use_rad
#endif

   implicit none

   integer :: i
   type(domain), pointer :: grid

   if (trace_use) call da_trace_entry ("da_wrfvar_finalize")

   ! output wrfvar analysis

   if ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_output (head_grid , config_flags)
      call med_shutdown_io (head_grid , config_flags)
   end if

   grid => head_grid

   deallocate (grid%parents)
   deallocate (grid%nests)
   deallocate (grid%domain_clock)
   deallocate (grid%alarms)
   deallocate (grid%alarms_created)

   deallocate (grid%i_start)
   deallocate (grid%i_end)
   deallocate (grid%j_start)
   deallocate (grid%j_end)

#include "em_deallocs.inc"

   deallocate (grid)

   if (allocated(num_tovs_before)) deallocate (num_tovs_before)
   if (allocated(num_tovs_after))  deallocate (num_tovs_after)
   if (allocated(tovs_copy_count)) deallocate (tovs_copy_count)
   if (allocated(tovs_send_pe))    deallocate (tovs_send_pe)
   if (allocated(tovs_recv_pe))    deallocate (tovs_recv_pe)
   if (allocated(tovs_send_start)) deallocate (tovs_send_start)
   if (allocated(tovs_send_count)) deallocate (tovs_send_count)
   if (allocated(tovs_recv_start)) deallocate (tovs_recv_start)

   if (rootproc) then
      close (cost_unit)
      close (grad_unit)
      close (stats_unit)
      close (jo_unit)
      close (check_max_iv_unit)
      call da_free_unit (cost_unit)
      call da_free_unit (grad_unit)
      call da_free_unit (stats_unit)
      call da_free_unit (jo_unit)
      call da_free_unit (check_max_iv_unit)
   end if

#ifdef CRTM
   if (use_rad .and. rtm_option == rtm_option_crtm) then
      ierr = CRTM_Destroy(ChannelInfo)
      deallocate(Sensor_Descriptor)
   end if
#endif

   do i=unit_start,unit_end
      if (unit_used(i)) then
         write(unit=stderr,FMT=*) "unit",i,"still used"
      end if
   end do

   call da_message ((/"SUCCESS COMPLETE WRFVAR"/))

   if (trace_use) call da_trace_exit ("da_wrfvar_finalize")

end subroutine da_wrfvar_finalize


