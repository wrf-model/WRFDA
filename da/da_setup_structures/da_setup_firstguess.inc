subroutine da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                              )
   !---------------------------------------------------------------------------
   ! PURPOSE: Allocate and read in components of first guess state.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xbx_type),INTENT(OUT)  :: xbx   ! Header & non-gridded vars.

   TYPE(domain) , TARGET        :: grid

   ! Definitions of dummy arguments to solve
#include "em_dummy_new_decl.inc"

   integer :: is, ie, js, je
   real    :: ddx , ddy    

   IF (trace_use) call da_trace_entry("da_setup_firstguess")

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte

   !---------------------------------------------------------------------------      
   ! [1.0] Setup and read in fields from first guess:
   !---------------------------------------------------------------------------      

   if ( fg_format == 1 ) then

      ! First guess is a WRF C-grid format file:

     call da_setup_firstguess_wrf( xbx, grid, &
#include "em_dummy_new_args.inc"
                                 )
   else if ( fg_format == 3 ) then

      ! First guess is an KMA format file:

     call da_setup_firstguess_kma( xbx, grid, &
#include "em_dummy_new_args.inc"
                                 )
   end if

   !---------------------------------------------------------------------------
   ! Exchange halo region for XB arrays.
   !---------------------------------------------------------------------------

   if ( fg_format == 1 .or. fg_format == 2) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      grid%xb%coefx(is:ie,js:je) = 0.5 * grid%xb%map_factor(is:ie,js:je)/grid%xb%ds
      grid%xb%coefy(is:ie,js:je) = grid%xb%coefx(is:ie,js:je)
      grid%xb%coefz(is:ie,js:je) = 0.5 / (grid%xb%map_factor(is:ie,js:je)*grid%xb%ds)
   else if (fg_format == 3) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      ddx =  earth_radius*1000 * 2.0 * pi / (grid%xb%ide-grid%xb%ids+1)
      ddy =  earth_radius*1000       * pi / (grid%xb%jde-grid%xb%jds  )
      grid%xb% coefx(is:ie,js:je) = 0.5 / (ddx * cos(grid%xlat(is:ie,js:je)*pi/180.))
      grid%xb% coefy(is:ie,js:je) = 0.5 /  ddy
   else
      WRITE(UNIT=errmsg(1),FMT='(A,I5)') &
         "Wrong choice for fg_format = ",fg_format
      call da_error(__FILE__,__LINE__,errmsg(1:1))
   end if

   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id0)
   periodic_x = grid%periodic_x

   if (global) then     
      ! Set East-West boundary for Xb-array 
      call da_set_boundary_xb(grid%xb)
   end if

   !---------------------------------------------------------------------------      
   ! [2.0] Setup grid-dependent constants used:
   !---------------------------------------------------------------------------

   ! [2.1] Set up fast Fourier & Legendre transform constants:

   call da_setup_runconstants( grid%xp, grid%xb, xbx)

   IF (trace_use) call da_trace_exit("da_setup_firstguess")

end subroutine da_setup_firstguess


