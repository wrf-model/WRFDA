subroutine da_store_obs_grid_info (info)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(infa_type), intent(inout) :: info

   if (trace_use) call da_trace_entry("da_store_obs_grid_info")

   info%proc_domain(:,:) = .false.
   
   where(info%i(:,:) >= its .and. info%i(:,:) <= ite .and. info%j(:,:) >= jts .and. info%j(:,:) <= jte)
      info%proc_domain(:,:) = .true.
   end where

   if (trace_use) call da_trace_exit("da_store_obs_grid_info")

end subroutine da_store_obs_grid_info


