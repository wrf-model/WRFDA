subroutine da_store_obs_grid_info (xp, loc)

   implicit none

   type (xpose_type),     intent(in)    :: xp     ! Domain decomposition vars.
   type (model_loc_type), intent(inout) :: loc    ! Obs location 

   loc%proc_domain = .false.
   
   ! JRB hack
   if (loc%i >= xp%its .and. loc%i <= xp%ite .and. &
      loc%j >= xp%jts .and. loc%j <= xp%jte) then
      loc%proc_domain = .true.
   end if

   ! if (loc%i >= xp%its .and. loc%i < xp%ite .and. &
   !    loc%j >= xp%jts .and. loc%j < xp%jte) then
   !    loc%proc_domain = .true.
   ! end if

end subroutine da_store_obs_grid_info


