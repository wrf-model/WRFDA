subroutine da_store_obs_grid_info_new (i,j, proc_domain)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)  :: i(:)
   integer,           intent(in)  :: j(:)
   logical,           intent(out) :: proc_domain(:)

   if (trace_use) call da_trace_entry("da_store_obs_grid_info_new")

   proc_domain(:) = .false.
   
   where(i(:) >= its .and. i(:) <= ite .and. &
      j(:) >= jts .and. j(:) <= jte)
      proc_domain(:) = .true.
   end where

   if (trace_use) call da_trace_exit("da_store_obs_grid_info_new")

end subroutine da_store_obs_grid_info_new


