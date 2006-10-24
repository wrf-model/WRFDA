subroutine da_wrfvar_finalize

   integer :: i

   ! output wrfvar analysis

   IF ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_output( head_grid , config_flags )
      call med_shutdown_io ( head_grid , config_flags )
   END IF

   if (allocated(num_tovs_before)) deallocate (num_tovs_before)
   if (allocated(num_tovs_after))  deallocate (num_tovs_after)
   if (allocated(tovs_copy_count)) deallocate (tovs_copy_count)
   if (allocated(tovs_send_pe))    deallocate (tovs_send_pe)
   if (allocated(tovs_recv_pe))    deallocate (tovs_recv_pe)
   if (allocated(tovs_send_start)) deallocate (tovs_send_start)
   if (allocated(tovs_send_count)) deallocate (tovs_send_count)
   if (allocated(tovs_recv_start)) deallocate (tovs_recv_start)

   if (rootproc) then
      close(cost_unit)
      close(grad_unit)
      close(stats_unit)
      close(jo_unit)
      close(check_max_iv_unit)
      call da_free_unit(cost_unit)
      call da_free_unit(grad_unit)
      call da_free_unit(stats_unit)
      call da_free_unit(jo_unit)
      call da_free_unit(check_max_iv_unit)

      do i=1,num_alpha_corr_types
         close(alpha_corr_unit1(i))
         close(alpha_corr_unit2(i))
         call da_free_unit(alpha_corr_unit1(i))
         call da_free_unit(alpha_corr_unit2(i))
      end do
   end if

   do i=unit_start,unit_end
      if (unit_used(i)) then
         write(0,*) "unit",i,"still used"
      end if
   end do

   call da_messages((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))

end subroutine da_wrfvar_finalize
