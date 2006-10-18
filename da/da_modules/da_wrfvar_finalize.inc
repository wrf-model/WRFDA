subroutine da_wrfvar_finalize

   implicit none

   integer :: i
   type(domain), pointer :: grid

   ! output wrfvar analysis

   IF ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_output( head_grid , config_flags )
      call med_shutdown_io ( head_grid , config_flags )
   END IF

   grid => head_grid

#include "em_deallocs.inc"


   do i=1,size(head_grid%alarms_created)
      if (head_grid%alarms_created(i)) then
         deallocate(head_grid%alarms(i)%alarmint)
      end if
   end do 

   if (associated(grid%i_start)) deallocate(grid%i_start)
   if (associated(grid%i_end))   deallocate(grid%i_end)
   if (associated(grid%j_start)) deallocate(grid%j_start)
   if (associated(grid%j_end))   deallocate(grid%j_end)

   call da_messages((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))

end subroutine da_wrfvar_finalize
