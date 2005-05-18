!MEDIATION_LAYER:STATS_BE
!

SUBROUTINE stats_be ( grid , config_flags , &
#include <em_dummy_args.inc>
                 )
!-------------------------------------------------------------------
!PURPOSE : To generate NMC BE statistics and Ensemble Statistics
!
!HISTORY : 08/15/2004                                Mi-Seon Lee
!-------------------------------------------------------------------

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Test
   USE DA_Tools
   USE DA_Gen_Be_Stats
   USE par_util
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   TYPE(domain),                intent(inout) :: grid
   TYPE(xbx_type)                             :: xbx

   TYPE(grid_config_rec_type), intent(inout)  :: config_flags
   TYPE(be_type)                              :: be    

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

!------------------------------------------------------------------------------
!
   write (6, '(/,A,/)') ' *** DA_Generate_Be_Stats ***'

!------------------------------------------------------------------------------
!  [1.0] Initialise STATS parameters:
!------------------------------------------------------------------------------

      call da_init_3dvar ( grid, xp, xb, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------

      call DA_Stats_Namelist


      call DA_Statistics_Step0 (grid, config_flags,       &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte, &
#include <em_dummy_args.inc>
                 )

   write(unit=*, fmt='(a,i2,2x,a)') &
        'DA_Gen_Be_Statistics step Finished normally.'

!-------------------------------------------------------------------------------
CONTAINS

#include "../da_solve_v3d/da_init_3dvar.inc"

END SUBROUTINE stats_be

