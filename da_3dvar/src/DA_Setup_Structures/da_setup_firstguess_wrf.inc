SUBROUTINE DA_Setup_FirstGuess_WRF( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Define/allocate components of WRF model state.
!
!  METHOD:
!
!  HISTORY: 04/16/2002 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xbx_type),INTENT(OUT)         :: xbx    ! Header & non-gridded vars.

   TYPE(domain) , TARGET               :: grid

!  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>
!---------------------------------------------------------------------------

   INTEGER           :: ier    ! error index

   integer           :: i, j, map_util_project
   integer           :: wrf_dim1, wrf_dim2, wrf_dim3
   REAL              :: x, y
  
   real              :: theta1, theta2, conv

   character(len=24) :: xb_date, an_date
   integer(kind=4)   :: flag
   integer           :: len, index, seconds

!---------------------------------------------------------------------------
!  [1.0] Read original WRF format first guess:
!---------------------------------------------------------------------------

   conv = 180.0 / pi
   
!---------------------------------------------------------------------------
!  [2.0] Copy header info:
!---------------------------------------------------------------------------

   if((xp%its == xp%ids) .and. (xp%jts == xp%jds)) then
      start_lat = xlat(xp%its, xp%jts)
      start_lon = xlong(xp%its, xp%jts)
   endif

#ifdef DM_PARALLEL
   call wrf_dm_bcast_real(start_lat, 1)
   call wrf_dm_bcast_real(start_lon, 1)
#endif

!---------------------------------------------------------------------------
!  Setup Map utility
!---------------------------------------------------------------------------

   call nl_get_map_proj ( grid%id , grid%map_proj )
   call nl_get_truelat1 ( grid%id , grid%truelat1 )
   call nl_get_truelat2 ( grid%id , grid%truelat2 )
   call nl_get_dx ( grid%id , grid%dx )
   call nl_get_cen_lat ( grid%id , grid%cen_lat )
   call nl_get_cen_lon ( grid%id , grid%cen_lon )
   call nl_get_moad_cen_lat ( grid%id , grid%moad_cen_lat )
   call nl_get_stand_lon ( grid%id , grid%stand_lon )

   phic = grid%moad_cen_lat
   xlonc = grid%stand_lon

   truelat1_3dv = grid%truelat1
   truelat2_3dv = grid%truelat2
   pole = 90.0
   dsm = 0.001 * grid%dx

   map_util_project = grid%map_proj

   if(print_detail > 0) then
      write(unit=*, fmt='(a, i6)') &
           'map_proj =', grid%map_proj

      write(unit=*, fmt='(a, e16.6)') &
           'cen_lat  =', grid%cen_lat,  &
           'cen_lon  =', grid%cen_lon,  &
           'truelat1 =', grid%truelat1, &
           'truelat2 =', grid%truelat2, &
           'start_lat =', start_lat, &
           'start_lon =', start_lon, &
           'dsm      =', dsm
   endif

!--Set map projection in WRFSI world.
   map_util_project = PROJ_LC

   if(grid%map_proj == 0) then
      map_util_project = PROJ_LATLON
   else if(grid%map_proj == 1) then
      map_util_project = PROJ_LC
   else if(grid%map_proj == 2) then
      map_util_project = PROJ_PS
   else if(grid%map_proj == 3) then
      map_util_project = PROJ_MERC
   endif

   call map_set(map_util_project,start_lat,start_lon,real(xp%ids),real(xp%jds), &
                grid%dx,grid%stand_lon,grid%truelat1,grid%truelat2,map_info)

!--Need to set map projection in WRF world.
   map_projection = grid%map_proj

   cone_factor = map_info%cone

!---------------------------------------------------------------------------
!  [3.0] Interpolate WRF C-grid winds to p points of 3DVAR grid (interpolate 
!  u to west, v to south?
!---------------------------------------------------------------------------

   xb % mix = xp%ide - xp%ids + 1
   xb % mjy = xp%jde - xp%jds + 1
   xb % mkz = xp%kde - xp%kds + 1

   xb % ds  = 0.001 * grid%dx

   mix = xb % mix
   mjy = xb % mjy
   mkz = xb % mkz
   
   CALL DA_Transfer_WRFToXb( xbx, grid, &
 
#include <em_dummy_args.inc>

                           )

END SUBROUTINE DA_Setup_FirstGuess_WRF

