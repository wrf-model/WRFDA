subroutine da_store_obs_grid_info (xp, loc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type),     intent(in)    :: xp     ! Domain decomposition vars.
   type (model_loc_type), intent(inout) :: loc    ! Obs location 

   if (trace_use_dull) call da_trace_entry("da_store_obs_grid_info")

   loc%proc_domain = .false.
   
   ! FIX?
   if (loc%i >= xp%its .and. loc%i <= xp%ite .and. &
      loc%j >= xp%jts .and. loc%j <= xp%jte) then
      loc%proc_domain = .true.
   end if

   ! if (loc%i >= xp%its .and. loc%i < xp%ite .and. &
   !    loc%j >= xp%jts .and. loc%j < xp%jte) then
   !    loc%proc_domain = .true.
   ! end if

   if (trace_use_dull) call da_trace_exit("da_store_obs_grid_info")

end subroutine da_store_obs_grid_info


