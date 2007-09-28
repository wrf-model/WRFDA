subroutine da_store_obs_grid_info (loc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (model_loc_type), intent(inout) :: loc    ! Obs location 

   if (trace_use_dull) call da_trace_entry("da_store_obs_grid_info")

   loc%proc_domain = .false.
   
   ! FIX?
   if (loc%i >= its .and. loc%i <= ite .and. &
      loc%j >= jts .and. loc%j <= jte) then
      loc%proc_domain = .true.
   end if

   ! if (loc%i >= its .and. loc%i < ite .and. &
   !    loc%j >= jts .and. loc%j < jte) then
   !    loc%proc_domain = .true.
   ! end if

   if (trace_use_dull) call da_trace_exit("da_store_obs_grid_info")

end subroutine da_store_obs_grid_info


