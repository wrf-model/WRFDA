subroutine da_med_initialdata_output (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(in)                   :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in),  optional       :: out_filename

   integer                :: fid
   character (len=80)     :: file_name

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_output")

   if (present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_output'
   end if

   call da_trace ("da_med_initialdata_ouput",Message="Writing wrfvar output")
   call open_w_dataset (fid, trim(file_name), grid , config_flags , &
                         output_model_input , "DATASET=INPUT", ierr)

   if (ierr /= 0) then
      write(unit=message(1),fmt=*) 'Error opening ', &
         trim(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call output_model_input (fid, grid , config_flags , ierr)

   call close_dataset (fid , config_flags, "DATASET=INPUT")

   if (trace_use) call da_trace_exit("da_med_initialdata_output")

end subroutine da_med_initialdata_output


 &
                         sqrt(sound%rms_err%t/real(nt)), &
                         sqrt(sound%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_sound")

end subroutine da_print_stats_sound


ts%minimum%tpw%value, stats%maximum%tpw%value, &
      stats%minimum%tpw%n, stats%maximum%tpw%n, &
      stats%minimum%tpw%l, stats%maximum%tpw%l)

   call da_proc_stats_combine(stats%average%speed, stats%rms_err%speed, &
      stats%minimum%speed%value, stats%maximum%speed%value, &
      stats%minimum%speed%n, stats%maximum%speed%n, &
      stats%minimum%speed%l, stats%maximum%speed%l)

   if (rootproc) then
      if (ntpw /= 0) then

         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           tpw(cm)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntpw
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tpw%value, stats%minimum%tpw%n    , &
            ' Maximum(n): ', stats%maximum%tpw%value, stats%maximum%tpw%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tpw/real(ntpw), &
            '    RMSE   : ', sqrt(stats%rms_err%tpw/real(ntpw))
      end if

      if (nspeed /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           Speed(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', nspeed
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%speed%value, stats%minimum%speed%n    , &
            ' Maximum(n): ', stats%maximum%speed%value, stats%maximum%speed%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%Speed/real(nspeed), &
            '    RMSE   : ', sqrt(stats%rms_err%Speed/real(nspeed))
      end if
   end if

   if (trace_use) call da_trace_exit("da_ao_stats_ssmi_rv")

end subroutine da_ao_stats_ssmi_rv


