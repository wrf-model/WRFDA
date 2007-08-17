subroutine da_store_obs_grid_info_new (xp, i,j, proc_domain)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)    :: xp     ! Domain decomposition vars.
   integer, intent(in)              :: i(:)
   integer, intent(in)              :: j(:)
   logical, intent(out)             :: proc_domain(:)

   proc_domain(:) = .false.
   
   where(i(:) >= xp%its .and. i(:) <= xp%ite .and. &
      j(:) >= xp%jts .and. j(:) <= xp%jte)
      proc_domain(:) = .true.
   end where

end subroutine da_store_obs_grid_info_new


