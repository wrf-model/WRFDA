MODULE module_wrf_mm5_interface

!--Driver layer modules
   USE module_domain
   use module_configure
   use module_driver_constants

   use da_constants
   use da_define_structures
   use da_setup_structures
   use da_tools
   use da_par_util

   use da_solve_v3d
   use module_wrf_mm5

   implicit none

CONTAINS

SUBROUTINE wrf_mm5_interface(grid, config_flags)

!--Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Test
   USE DA_Tools
   USE DA_Minimisation
   USE par_util

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

   write (6, '(/,A,/)') ' *** Convertor WRF to MM5 ***'


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
!  [3.0] Set up first guess field (xb):
!------------------------------------------------------------------------------

   CALL da_setup_firstguess( xbx, grid, &
#include <em_actual_args.inc>
                           )

   call da_setup_mm5_reference(grid%xb)

   call da_interp_eta2sigma(grid%xb)

   call da_transfer_xbtomm5( grid%xp, grid%xb )

END SUBROUTINE wrf_mm5_interface

END MODULE module_wrf_mm5_interface

