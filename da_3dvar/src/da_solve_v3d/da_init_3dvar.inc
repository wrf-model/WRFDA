SUBROUTINE da_init_3dvar( grid, xp, xb, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

   IMPLICIT NONE

!  Input data.

   TYPE(domain) , TARGET            :: grid

   TYPE (xpose_type),INTENT(INOUT)  :: xp      ! Transpose variables.
   TYPE (xb_type), INTENT(INOUT)    :: xb      ! Background state

   INTEGER,      INTENT(OUT)        :: ids,ide, jds,jde, kds,kde   ! domain dims.
   INTEGER,      INTENT(OUT)        :: ims,ime, jms,jme, kms,kme   ! memory dims.
   INTEGER,      INTENT(OUT)        :: ips,ipe, jps,jpe, kps,kpe   ! patch  dims.

   INTEGER :: ii

   INTEGER :: sm31,sm32,sm33,sm31x,sm32x,sm33x,sm31y,sm32y,sm33y

!if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MSG_XPOSE add_msg_xpose_real
!else
!define TRUE_MSG_XPOSE add_msg_xpose_doubleprecision
!endif

!--De-reference dimension information stored in the grid data structure.

   call copy_dims( grid, xp, &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe  )

!--Compute these starting and stopping locations for each tile and number of tiles.

   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

   call copy_tile_dims( grid, xp, its, ite, jts, jte, kts, kte )

!**********************************************************************

   sm31             = grid%sm31
   sm32             = grid%sm32
   sm33             = grid%sm33
   sm31x            = grid%sm31x
   sm32x            = grid%sm32x
   sm33x            = grid%sm33x
   sm31y            = grid%sm31y
   sm32y            = grid%sm32y
   sm33y            = grid%sm33y

!**********************************************************************

!--Register xpose arrays and build messages for processor communication.

#ifdef DM_PARALLEL
#define REGISTER_I1
#include <em_data_calls.inc>
! XPOSE_V1
  IF ( grid%comms( XPOSE_V1 ) == invalid_message_value ) THEN
    CALL wrf_debug ( 50 , 'setting up xpose XPOSE_V1' )
    CALL setup_xpose_rsl( grid )
    CALL reset_msgs_xpose
#ifndef DEREF_KLUDGE
    CALL TRUE_MSG_XPOSE ( xp%v1z, xp%v1x, xp%v1y, 3 )
#else
    CALL TRUE_MSG_XPOSE ( xp%v1z(ims,jms,kms), &
                         xp%v1x(xp%imsx,xp%jmsx,xp%kmsx), &
                         xp%v1y(xp%imsy,xp%jmsy,xp%kmsy), 3 )
#endif
    CALL define_xpose ( grid%domdesc , grid%comms ( XPOSE_V1 ) )
  ENDIF

! XPOSE_V2
  IF ( grid%comms( XPOSE_V2 ) == invalid_message_value ) THEN
    CALL wrf_debug ( 50 , 'setting up xpose XPOSE_V2' )
    CALL setup_xpose_rsl( grid )
    CALL reset_msgs_xpose
#ifndef DEREF_KLUDGE
    CALL TRUE_MSG_XPOSE ( xp%v2z, xp%v2x, xp%v2y, 3 )
#else
    CALL TRUE_MSG_XPOSE ( xp%v2z(ims,jms,kms), &
                         xp%v2x(xp%imsx,xp%jmsx,xp%kmsx), &
                         xp%v2y(xp%imsy,xp%jmsy,xp%kmsy), 3 )
#endif
    CALL define_xpose ( grid%domdesc , grid%comms ( XPOSE_V2 ) )
  ENDIF

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
#include <HALO_INIT.inc>
#include <HALO_PSICHI_UV.inc>
#include <HALO_BAL_EQN_ADJ.inc>
#include <HALO_PSICHI_UV_ADJ.inc>
#include <HALO_XA.inc>
#include <HALO_XB.inc>
#include <HALO_SFC_XA.inc>
#include <HALO_SFC_XB.inc>
#include <HALO_SSMI_XA.inc>
#include <HALO_SSMI_XB.inc>
#include <HALO_RADAR_XB.inc>
#include <HALO_RADAR_XB.inc>
#include <HALO_2D_WORK.inc>
#include <HALO_RADAR_XA_W.inc>

! Copy domain and transpose descriptors.

   xp%domdesc = grid%domdesc
   do ii = 1, max_comms
     xp%comms(ii) = grid%comms(ii)
   enddo

!--Copy halo and transpose IDs.

   xp%halo_id0 = HALO_INIT
   xp%halo_id1 = HALO_PSICHI_UV
   xp%halo_id2 = HALO_BAL_EQN_ADJ
   xp%halo_id3 = HALO_PSICHI_UV_ADJ
   xp%halo_id4 = HALO_XA
   xp%halo_id5 = HALO_XB
   xp%halo_id6 = HALO_SFC_XA
   xp%halo_id7 = HALO_SFC_XB
   xp%halo_id8 = HALO_SSMI_XA
   xp%halo_id9 = HALO_SSMI_XB
   xp%halo_id10 = HALO_RADAR_XA
   xp%halo_id11 = HALO_RADAR_XB
   xp%halo_id12 = HALO_2D_WORK
   xp%halo_id13 = HALO_RADAR_XA_W

   xp%xpose_id1 = XPOSE_V1
   xp%xpose_id2 = XPOSE_V2
#endif

!  Fill background scalars:

   xb%ids = xp%ids 
   xb%ide = xp%ide
   xb%jds = xp%jds 
   xb%jde = xp%jde
   xb%kds = xp%kds 
   xb%kde = xp%kde 

   xb%ims = xp%ims 
   xb%ime = xp%ime
   xb%jms = xp%jms 
   xb%jme = xp%jme
   xb%kms = xp%kms 
   xb%kme = xp%kme 

   xb%its = xp%its 
   xb%ite = xp%ite
   xb%jts = xp%jts 
   xb%jte = xp%jte 
   xb%kts = xp%kts
   xb%kte = xp%kte 

END SUBROUTINE da_init_3dvar

