subroutine DA_Transfer_XatoAnalysis( it, xbx, grid, &
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

   IMPLICIT NONE

   INTEGER,         INTENT(IN)  :: it    ! outer-loop index
   TYPE (xbx_type), INTENT(OUT) :: xbx    ! Header & non-gridded vars.
   TYPE(domain) ,   TARGET      :: grid

!--Definitions of dummy arguments
#include <em_dummy_decl.inc>

!------------------------------------------------------------------------------      
!  Write out analysis in differing formats:
!------------------------------------------------------------------------------      

   if(fg_format == 1) then
      call DA_Transfer_XatoWRF( xbx, grid, &
#include <em_dummy_args.inc>
                           )


      if(it < max_ext_its) then
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
   end if

end subroutine DA_Transfer_XatoAnalysis

