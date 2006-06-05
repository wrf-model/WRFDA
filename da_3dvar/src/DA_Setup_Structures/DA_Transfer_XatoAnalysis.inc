subroutine DA_Transfer_XatoAnalysis( it, xbx, grid, config_flags ,&
#include <em_dummy_args.inc>
                           )

!------------------------------------------------------------------------------
!  PURPOSE: Transfer xb and xa (increments) to analysis.
!
!  METHOD:  
!
!  HISTORY: 08/11/2004
!           Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   USE da_wrfvar_io

   IMPLICIT NONE

   INTEGER,         INTENT(IN)  :: it    ! outer-loop index
   TYPE (xbx_type), INTENT(OUT) :: xbx    ! Header & non-gridded vars.
   TYPE(domain) ,   TARGET      :: grid

   TYPE (grid_config_rec_type), intent(inout) :: config_flags

!--Definitions of dummy arguments
#include <em_dummy_decl.inc>

   character*4 filnam

   IF (trace_use) CALL da_trace_entry("da_transfer_xatoanalysis")

!------------------------------------------------------------------------------      
!  Write out analysis in differing formats:
!------------------------------------------------------------------------------      

   if(fg_format == 1) then
#ifdef VAR4DPOSS
      if (write_increments) then
         write(UNIT=filnam,FMT='(a3,i1)') 'inc',it
         call DA_Transfer_XatoWRFTL( grid, config_flags, filnam, &
#include <em_dummy_args.inc>
                                 )
      endif
#endif

      call DA_Transfer_XatoWRF( xbx, grid, &
#include <em_dummy_args.inc>
                           )

      if(it < max_ext_its) then
         if(lvar4d) then
            CALL da_med_initialdata_output( grid , config_flags )
         endif

         call DA_Transfer_WRFToXb( xbx, grid, &
#include <em_dummy_args.inc>
                          )
      endif
   else if(fg_format == 2) then
      if(it < max_ext_its) then
         call da_add_increments(it, xp, xa, xb, xbx)
      else
         call da_write_analysis_mm5(xb%mix, xb%mjy, xb%mkz, &
                                    xp, xb, xa, it)
      endif
   else if(fg_format == 3) then
      call DA_Transfer_XatoKMA( xbx, grid, &
#include <em_dummy_args.inc>
                           )
      if(it < max_ext_its) then
         CALL DA_Transfer_KMAToXb( xbx, grid, &
#include <em_dummy_args.inc>
                           )
      end if
   end if

   IF (trace_use) CALL da_trace_exit("da_transfer_xatoanalysis")


end subroutine DA_Transfer_XatoAnalysis

