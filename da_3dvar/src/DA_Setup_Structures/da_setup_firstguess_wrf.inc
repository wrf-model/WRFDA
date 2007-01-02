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
   REAL              :: x, y, xxc, yyc, xxx, yyy, lat_cen, lon_cen
  
   real              :: theta1, theta2, conv

   character(len=24) :: xb_date, an_date
   integer(kind=4)   :: flag
   integer           :: len, index, seconds, i_grid,  j_grid, m_expand

!--------------------------------------------------------------------------
!  [0.0] check the xb_date
!--------------------------------------------------------------------------

       write(xb_date, '(i4.4,2("-",i2.2),"_",i2.2,2(":",i2.2),".0000")')  &
            grid%start_year, grid%start_month, grid%start_day, &
            grid%start_hour, grid%start_minute,grid%start_second

       len = len_trim(ANALYSIS_DATE)
   
       write(an_date(1:len), fmt='(a)') trim(ANALYSIS_DATE)

       seconds = DA_Diff_Seconds(an_date, xb_date)

       if (seconds > ANALYSIS_ACCU) then
         print '("xb_date=",a," an_date=",a)', xb_date, an_date
         print '("diff=",i6,"   ANALYSIS_ACCU=",i6)',seconds, ANALYSIS_ACCU
         print '(/a)',"=======> Wrong xb time found???"
         stop 22222
       endif

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

   call map_set(map_util_project,grid%cen_lat,grid%cen_lon,   &
                real(xp%ide-xp%ids+2)/2., real(xp%jde-xp%jds+2)/2., &
                grid%dx,grid%stand_lon,grid%truelat1,grid%truelat2,map_info)

!--Need to set map projection in WRF world.
   map_projection = grid%map_proj

   cone_factor = map_info%cone

   if(print_detail == -1) then
     
!-------------------------------------------------------------------------
! Check the ll_to_ij:
!-------------------------------------------------------------------------

     print '(/a/)',"Check the map_set correctness::::::::::::::::::::::::"

!    Domain center:
     call  latlon_to_ij(map_info, grid%cen_lat, grid%cen_lon, start_x, start_y)
     print '(/"Center: latc,lonc,x,y, Xc, Yc:",6f10.3/)',   &
                 grid%cen_lat, grid%cen_lon, start_x, start_y, &
                 real(xp%ide-xp%ids+2)/2., real(xp%jde-xp%jds+2)/2.

     start_x = real(xp%ide-xp%ids+2)/2
     start_y = real(xp%jde-xp%jds+2)/2
     lat_cen = -999.9
     lon_cen = -999.9
     call  ij_to_latlon(map_info, start_x, start_y, lat_cen, lon_cen)
     print '(/"Center: X, Y, latc, lonc, phic, xlonc:",6f10.3/)',   &
                      start_x, start_y, lat_cen, lon_cen,   &
                      grid%cen_lat, grid%cen_lon

!    Low-left corner:
     call  latlon_to_ij(map_info, xlat(xp%ids,xp%jds), xlong(xp%ids,xp%jds), &
                                                            start_x, start_y)
       print '(/"Low-left: lat,lon,x,y,xp%ids,xp%jds:",4f10.3,2i6/)',   &
                xlat(xp%ids,xp%jds),  xlong(xp%ids,xp%jds), start_x, start_y, &
                                                             xp%ids, xp%jds
!   Upper-right corner:
       call  latlon_to_ij(map_info, xlat(xp%ide,xp%jde), xlong(xp%ide,xp%jde),&
                                                            start_x, start_y)
       print '(/"Upper-right: lat,lon,x,y,xp%ide,xp%jde:",4f10.3,2i6/)',   &
                xlat(xp%ide,xp%jde),  xlong(xp%ide,xp%jde), start_x, start_y, &
                                                             xp%ide, xp%jde
   endif

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Guo: the following lines to setup the domain definition for use of the MM5/GRAPH:

   coarse_ds = 0.001 * grid%dx
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1
   start_x = 1.0
   start_y = 1.0

   xxc = real(coarse_ix)/2.0
   yyc = real(coarse_jy)/2.0

   if (grid%cen_lon /= grid%stand_lon .or. &
       grid%cen_lat /= grid%moad_cen_lat) then
!
     print '(/a,2e20.12)','grid%cen_lon  , grid%cen_lat     :', &
                           grid%cen_lon, grid%cen_lat
     print '( a,2e20.12)','grid%stand_lon, grid%moad_cen_lat:', &
                           grid%stand_lon, grid%moad_cen_lat
     print '(a)',  &
       '## Compute the coarse_ix, coarse_jy, start_x, and start_y: ##'

     call latlon_to_ij(map_info, phic        , xlonc       , xxx, yyy)

     i_grid = nint(xxx-xxc)
     j_grid = nint(yyy-yyc)
     m_expand = 16
!     print '("i_grid, j_grid,  m_expand:",3I8)', i_grid, j_grid,  m_expand

     coarse_ix = coarse_ix + 2*abs(i_grid) + m_expand
     coarse_jy = coarse_jy + 2*abs(j_grid) + m_expand

     start_x = m_expand/2 + 1
     if (i_grid < 0) start_x = start_x - 2*i_grid
     start_y = m_expand/2 + 1
     if (j_grid < 0) start_y = start_y - 2*j_grid

     print '(a,2i5,2x,a,2f6.1)', &
           'Dimension of MOAD: ix, jy:',coarse_ix, coarse_jy, &
           'parent_x, parent_y:', start_x, start_y

   endif

   CALL Set_Map_Para ! set up the map background parameters

!   CALL llxy( PHIC,XLONC,X,Y )

   CALL llxy(grid%cen_lat,grid%cen_lon,x,y)
   print '(/a)',"Please check if the domain's central point (x,y) is correct:"
   print '(a,4f12.3/)', 'grid%cen_lat,grid%cen_lon,x,y:', &
                         grid%cen_lat,grid%cen_lon,x,y
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

