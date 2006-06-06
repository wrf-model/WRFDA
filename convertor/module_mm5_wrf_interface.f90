MODULE module_mm5_wrf_interface

!--Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants

   use da_constants
   use da_define_structures
   use da_setup_structures
   use da_tools
   use da_par_util

   use da_solve_v3d
   use module_mm5_wrf

CONTAINS

SUBROUTINE mm5_wrf_interface(grid, config_flags)

   IMPLICIT NONE

   TYPE(domain),                intent(inout) :: grid
   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   TYPE (xbx_type)              :: xbx         ! For header & non-grid arrays.

   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   grid%itimestep = 1

#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif

!------------------------------------------------------------------------------

   write (6, '(/,A,/)') ' *** MM5 TO WRF CONVERTOR ***'

!------------------------------------------------------------------------------
!  Read namelist.input
!------------------------------------------------------------------------------

CALL initial_config

! Copy namelist variables to DA_Constants

#define SOURCE_RECORD model_config_rec%
#define DEST_RECORD

#include <config_assigns.inc>


!------------------------------------------------------------------------------
!  Initialise wrfvar parameters:
!------------------------------------------------------------------------------

   call da_init_wrfvar(grid, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte, &
#include <em_actual_args.inc>
                     )

!------------------------------------------------------------------------------
!  Set up first guess field (xb):
!------------------------------------------------------------------------------

   CALL da_setup_mm5(grid%xb, xbx, grid%xp)

   call da_interp_sigma2eta(grid%xb)

   call da_transfer_xbtowrf( xbx, grid, config_flags, &
#include <em_actual_args.inc>
                              )

END SUBROUTINE mm5_wrf_interface

END MODULE module_mm5_wrf_interface

