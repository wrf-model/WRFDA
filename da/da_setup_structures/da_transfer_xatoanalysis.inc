subroutine da_transfer_xatoanalysis( it, xbx, grid, config_flags ,&
#include "em_dummy_new_args.inc"
                           )

   !---------------------------------------------------------------------------
   ! PURPOSE: Transfer xb and xa (increments) to analysis.
   !---------------------------------------------------------------------------

   USE da_wrfvar_io

   IMPLICIT NONE

   INTEGER,         INTENT(IN)  :: it    ! outer-loop index
   TYPE (xbx_type), INTENT(OUT) :: xbx    ! Header & non-gridded vars.
   TYPE(domain) ,   TARGET      :: grid

   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   ! Definitions of dummy arguments
#include "em_dummy_new_decl.inc"

   character*4 filnam

   IF (trace_use) call da_trace_entry("da_transfer_xatoanalysis")

   !---------------------------------------------------------------------------
   ! Write out analysis in differing formats:
   !---------------------------------------------------------------------------      

   if (fg_format == fg_format_wrf) then
      if (write_increments .and. var4d ) then
         write(UNIT=filnam,FMT='(a3,i1)') 'inc',it
         call da_transfer_xatowrftl( grid, config_flags, filnam, &
#include "em_dummy_new_args.inc"
                                 )
      endif

      call da_transfer_xatowrf( xbx, grid, &
#include "em_dummy_new_args.inc"
                           )

      if (it < max_ext_its) then
         if (var4d) then
            call da_med_initialdata_output( grid , config_flags )
         endif

         call da_transfer_wrftoxb( xbx, grid, &
#include "em_dummy_new_args.inc"
                          )
      endif
   else if (fg_format == fg_format_kma) then
      call da_transfer_xatokma( xbx, grid, &
#include "em_dummy_new_args.inc"
                           )
      if (it < max_ext_its) then
         call da_transfer_kmatoxb( xbx, grid, &
#include "em_dummy_new_args.inc"
                           )
      end if
   end if

   IF (trace_use) call da_trace_exit("da_transfer_xatoanalysis")

end subroutine da_transfer_xatoanalysis


