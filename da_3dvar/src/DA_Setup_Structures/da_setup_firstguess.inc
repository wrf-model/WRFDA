SUBROUTINE DA_Setup_FirstGuess( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate and read in components of first guess state.
!
!  METHOD:  
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xbx_type),INTENT(OUT)  :: xbx   ! Header & non-gridded vars.

   TYPE(domain) , TARGET        :: grid

!  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   write(unit=*, fmt='(/a)') &
        '-----------------------------------'
   write(unit=*, fmt='(/a)') &
        '[2.0] Set up first guess field (xb)'
   write(unit=*, fmt='(a/a)') &
        '-----------------------------------'

!------------------------------------------------------------------------------      
!  [1.0] Setup and read in fields from first guess:
!------------------------------------------------------------------------------      

   if ( fg_format == 1 ) then

!-----First guess is a WRF C-grid format file:

     call da_setup_firstguess_wrf( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                                 )

   else if ( fg_format == 2 ) then

!-----First guess is an MM5 format file:
      call da_setup_firstguess_mm5(xb, xbx, xp)

   else if ( fg_format == 3 ) then

!-----First guess is an KMA (netcdf format) file:
!     call da_setup_firstguess_kma(xb, xbx, xp)
   
      write(unit=*, fmt='(/a)') 'Input KMA FG.'

   end if

!------------------------------------------------------------------------------
!--Exchange halo region for XB arrays.
!------------------------------------------------------------------------------

   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id0)

!------------------------------------------------------------------------------

   write(unit=*, fmt='(4x,a,i4)') &
        'I Dimension grid points: ', xb % mix, &
        'J Dimension grid points: ', xb % mjy, &
        'K Dimension grid points: ', xb % mkz
   write(unit=*, fmt='(/)')

!------------------------------------------------------------------------------      
!  [2.0] Setup grid-dependent constants used:
!------------------------------------------------------------------------------

!  [2.1] Set up fast Fourier transform constants:

   CALL DA_Setup_RunConstants( xp, xbx, xb%ds)

END SUBROUTINE DA_Setup_FirstGuess

