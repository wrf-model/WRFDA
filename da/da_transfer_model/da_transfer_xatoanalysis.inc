subroutine da_transfer_xatoanalysis(it, xbx, grid, config_flags &
#include "em_dummy_new_args.inc"
                          )

   !---------------------------------------------------------------------------
   ! Purpose: Transfer xb and xa (increments) to analysis.
   !---------------------------------------------------------------------------

   implicit none

   integer,         intent(in)    :: it    ! outer-loop index
   type (xbx_type), intent(out)   :: xbx    ! Header & non-gridded vars.
   type(domain),    intent(inout) :: grid

   type (grid_config_rec_type), intent(inout) :: config_flags

   ! Definitions of dummy arguments
#include "em_dummy_new_decl.inc"

   character*4 filnam

   if (trace_use) call da_trace_entry("da_transfer_xatoanalysis")

   !---------------------------------------------------------------------------
   ! Write out analysis in differing formats:
   !---------------------------------------------------------------------------      

   if (fg_format == fg_format_wrf) then
      if (write_increments .and. var4d) then
         write(unit=filnam,fmt='(a3,i1)') 'inc',it
         call da_transfer_xatowrftl(grid, config_flags, filnam &
#include "em_dummy_new_args.inc"
                                )
      end if

      call da_transfer_xatowrf(xbx, grid &
#include "em_dummy_new_args.inc"
                          )

      if (it < max_ext_its) then
         if (var4d) then
            call da_med_initialdata_output(grid , config_flags)
         end if

         call da_transfer_wrftoxb(xbx, grid &
#include "em_dummy_new_args.inc"
                         )
      end if
   else if (fg_format == fg_format_kma) then
      call da_transfer_xatokma(xbx, grid &
#include "em_dummy_new_args.inc"
                          )
      if (it < max_ext_its) then
         call da_transfer_kmatoxb(xbx, grid &
#include "em_dummy_new_args.inc"
                          )
      end if
   end if

   if (trace_use) call da_trace_exit("da_transfer_xatoanalysis")

end subroutine da_transfer_xatoanalysis

