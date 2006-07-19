SUBROUTINE da_check_xtow_adjoint( grid, config_flags, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md

!---------------------------------------------------------------------------

#ifdef WRFVAR

!---------------------------------------------------------------------------
   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

#endif

END SUBROUTINE da_check_xtow_adjoint

