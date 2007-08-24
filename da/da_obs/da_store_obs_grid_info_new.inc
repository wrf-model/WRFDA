subroutine da_store_obs_grid_info_new (xp, i,j, proc_domain)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)  :: xp     ! Domain decomposition vars.
   integer,           intent(in)  :: i(:)
   integer,           intent(in)  :: j(:)
   logical,           intent(out) :: proc_domain(:)

   if (trace_use) call da_trace_entry("da_store_obs_grid_info_new")

   proc_domain(:) = .false.
   
   where(i(:) >= xp%its .and. i(:) <= xp%ite .and. &
      j(:) >= xp%jts .and. j(:) <= xp%jte)
      proc_domain(:) = .true.
   end where

   if (trace_use) call da_trace_exit("da_store_obs_grid_info_new")

end subroutine da_store_obs_grid_info_new


