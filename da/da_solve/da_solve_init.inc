subroutine da_solve_init(grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)      :: grid

#ifdef DM_PARALLEL
   integer :: ii
#endif

   integer :: sm31,sm32,sm33,sm31x,sm32x,sm33x,sm31y,sm32y,sm33y

   ! if (dwordsize != rwordsize)
#define true_MSG_XPOSE add_msg_xpose_real
   ! else
   !    define true_MSG_XPOSE add_msg_xpose_doubleprecision
   ! end if

   if (trace_use) call da_trace_entry("da_solve_init")

   ! De-reference dimension information stored in the grid data structure.

   call da_copy_dims(grid)

   ! Compute these starting and stopping locations for each tile and number 
   ! of tiles.

   call set_tiles (grid , ids , ide , jds , jde , ips , ipe , jps , jpe)

   call da_copy_tile_dims(grid)

   sm31             = grid%sm31
   sm32             = grid%sm32
   sm33             = grid%sm33
   sm31x            = grid%sm31x
   sm32x            = grid%sm32x
   sm33x            = grid%sm33x
   sm31y            = grid%sm31y
   sm32y            = grid%sm32y
   sm33y            = grid%sm33y

#ifdef DM_PARALLEL
   if (trace_use) call da_trace("da_solve_init", &
      Message="Setup register xpose arrays")

   ! Register xpose arrays and build messages for processor communication.
#define REGISTER_I1
#include "em_data_calls.inc"
   ! XPOSE_V1
   if (grid%comms(XPOSE_V1) == invalid_message_value) then
      call setup_xpose_rsl(grid)
      call reset_msgs_xpose
      call true_MSG_XPOSE (grid%xp%v1z, grid%xp%v1x, grid%xp%v1y, 3)
      call define_xpose (grid%domdesc , grid%comms (XPOSE_V1))
   end if

   ! XPOSE_V2
   if (grid%comms(XPOSE_V2) == invalid_message_value) then
      call setup_xpose_rsl(grid)
      call reset_msgs_xpose
      call true_MSG_XPOSE (grid%xp%v2z, grid%xp%v2x, grid%xp%v2y, 3)
      call define_xpose (grid%domdesc , grid%comms (XPOSE_V2))
   end if

   if (trace_use) call da_trace("da_solve_init", &
      Message="Setup halo region communication")

   ! Define halo region communication.
   !-----------------------------------------------------------------------
   !  Stencils for patch communications
   !                           * * * * *
   !         *        * * *    * * * * *
   !       * + *      * + *    * * + * *
   !         *        * * *    * * * * *
   !                           * * * * *
   !ij vp%v1            x
   !ij xb%cori          x
   !ij xb%rho           x
   !ij xa%u             x
   !ij xa%v             x
   !--------------------------------------------------------------
#include "HALO_INIT.inc"
#include "HALO_PSICHI_UV.inc"
#include "HALO_BAL_EQN_ADJ.inc"
#include "HALO_PSICHI_UV_ADJ.inc"
#include "HALO_XA.inc"
#include "HALO_XB.inc"
#include "HALO_SFC_XA.inc"
#include "HALO_SFC_XB.inc"
#include "HALO_SSMI_XA.inc"
#include "HALO_SSMI_XB.inc"
#include "HALO_RADAR_XB.inc"
#include "HALO_RADAR_XB.inc"
#include "HALO_2D_WORK.inc"
#include "HALO_RADAR_XA_W.inc"

   if (trace_use) call da_trace("da_solve_init", &
      Message="Copy domain and transpose descriptors")

   ! Copy domain and transpose descriptors.

   grid%xp%domdesc = grid%domdesc
   do ii = 1, max_comms
     grid%xp%comms(ii) = grid%comms(ii)
   end do

   ! Copy halo and transpose IDs.

   grid%xp%halo_id0 = HALO_INIT
   grid%xp%halo_id1 = HALO_PSICHI_UV
   grid%xp%halo_id2 = HALO_BAL_EQN_ADJ
   grid%xp%halo_id3 = HALO_PSICHI_UV_ADJ
   grid%xp%halo_id4 = HALO_XA
   grid%xp%halo_id5 = HALO_XB
   grid%xp%halo_id6 = HALO_SFC_XA
   grid%xp%halo_id7 = HALO_SFC_XB
   grid%xp%halo_id8 = HALO_SSMI_XA
   grid%xp%halo_id9 = HALO_SSMI_XB
   grid%xp%halo_id10 = HALO_RADAR_XA
   grid%xp%halo_id11 = HALO_RADAR_XB
   grid%xp%halo_id12 = HALO_2D_WORK
   grid%xp%halo_id13 = HALO_RADAR_XA_W

   grid%xp%xpose_id1 = XPOSE_V1
   grid%xp%xpose_id2 = XPOSE_V2
#endif

   ! Fill background scalars:

   grid%xb%ids = grid%xp%ids 
   grid%xb%ide = grid%xp%ide
   grid%xb%jds = grid%xp%jds 
   grid%xb%jde = grid%xp%jde
   grid%xb%kds = grid%xp%kds 
   grid%xb%kde = grid%xp%kde 

   grid%xb%ims = grid%xp%ims 
   grid%xb%ime = grid%xp%ime
   grid%xb%jms = grid%xp%jms 
   grid%xb%jme = grid%xp%jme
   grid%xb%kms = grid%xp%kms 
   grid%xb%kme = grid%xp%kme 

   grid%xb%its = grid%xp%its 
   grid%xb%ite = grid%xp%ite
   grid%xb%jts = grid%xp%jts 
   grid%xb%jte = grid%xp%jte 
   grid%xb%kts = grid%xp%kts
   grid%xb%kte = grid%xp%kte 

   if (trace_use) call da_trace_exit("da_solve_init")

end subroutine da_solve_init


