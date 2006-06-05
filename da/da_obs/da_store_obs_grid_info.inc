SUBROUTINE DA_Store_Obs_Grid_Info ( xp, loc )

   IMPLICIT NONE

   TYPE (xpose_type),     INTENT(IN)    :: xp     ! Domain decomposition vars.
   TYPE (model_loc_type), INTENT(INOUT) :: loc    ! Obs location 

   loc%proc_domain_with_halo = .true.
   loc%proc_domain = .false.
   
   if(loc%i >= xp%its .and. loc%i <= xp%ite .and. &
      loc%j >= xp%jts .and. loc%j <= xp%jte) then
        loc%proc_domain = .true.
   endif

END SUBROUTINE DA_Store_Obs_Grid_Info

