SUBROUTINE DA_Setup_FirstGuess_KMA( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Define/allocate components of WRF model state.
!
!  METHOD:
!
!  HISTORY: 08/06/2004 - Creation of F90 version.           Syed RH Rizvi
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

   integer           :: i, j
   integer           :: is, ie, js, je, ks, ke
   REAL              :: x, y
  
   real              :: theta1, theta2, conv

   character(len=24) :: xb_date, an_date
   integer(kind=4)   :: flag
   integer           :: len, index, seconds

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte
!---------------------------------------------------------------------------
!  [1.0] Read original WRF format first guess:
!---------------------------------------------------------------------------
   conv = 180.0 / pi

!  IF ( wrf_dm_on_monitor() ) THEN
!  ENDIF

!  call wrf_struct_bcast( xb )
   
!---------------------------------------------------------------------------
!  [2.0] Copy header info:
!---------------------------------------------------------------------------

! rizvi set it to 1 . Actually it should be decided by KMA 
   grid%map_proj = 0
   map_projection = grid%map_proj
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1
   coarse_ds = 0.001 * grid%dx

!  phic = grid%cen_lat
!  xlonc = grid%cen_lon
!  fix latitude and longitude
!  Note: staggering of grid is taken care of
    do j = js,je
    if( j > 1) then
    xlat(is:ie,j) = - 90.0  + (j-1)*180.0/(coarse_jy-2)
    else
!rizvi Avoid assigning -90 value                                           
    xlat(is:ie,j) = - 89.9                               
    end if
    if( xlat(ie,j) >  89.9 )then
    xlat(is:ie,j) = 89.9                               
    end if
    end do
    do i = is,ie
    xlong(i,js:je) = - 180.0 + (i-1)*360.0/(coarse_ix-1)
    end do
! fix map factor and coriol;is parameter
      
    f(is:ie,js:je) = 2.0 *earth_omega*sin(pi*xlat(is:ie,js:je)/180.0)

   truelat1_3dv = grid%truelat1
   truelat2_3dv = grid%truelat2
   pole = 90.0        !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   dsm = 0.001 * grid%dx

   if(print_detail > 0) then
   write(unit=*, fmt='(a, i6)') &
        'map_proj =', grid%map_proj, &
        'coarse_ix =', coarse_ix, &
        'coarse_jy =', coarse_jy

   write(unit=*, fmt='(a, e16.6)') &
        'cen_lat  =', grid%cen_lat,  &
        'cen_lon  =', grid%cen_lon,  &
        'truelat1 =', grid%truelat1, &
        'truelat2 =', grid%truelat2, &
        'dsm      =', dsm
   endif

   if (map_projection == 1) then

      if(abs(truelat1_3dv - truelat2_3dv) < 0.1) then

         write(unit=*, fmt='(a, f12.3)') &
              'truelat1_3dv = ', truelat1_3dv, &
              'truelat2_3dv = ', truelat2_3dv

         cone_factor = SIGN (1.,truelat1_3dv)*SIN (truelat1_3dv / conv)

         cone_factor = 1.0                                             

      else

         theta1 = (90.0 - truelat1_3dv)/CONV
         theta2 = (90.0 - truelat2_3dv)/CONV
 
         cone_factor = (log(sin(theta1)) - log(sin(theta2))) &
                     / (log(tan(theta1*0.5)) - log(tan(theta2*0.5)))

      endif

   else if (map_projection == 2) then

      cone_factor = 1.0

   else if (map_projection == 3) then

      cone_factor = 0.0

   endif
  
!  X,Y locations of starting point (1,1) of grid in coarse domain. Note: 0.5 
!  is added in WRF 3DVAR because original MM5 (1,1) is a dot point whereas in 
!  3DVAR there are only cross points!

   start_x = 1.0
   start_y = 1.0

   xb % mix = xp%ide - xp%ids + 1
   xb % mjy = xp%jde - xp%jds + 1
   xb % mkz = xp%kde - xp%kds + 1

   xb % ds  = coarse_ds

   mix = xb % mix
   mjy = xb % mjy
   mkz = xb % mkz


!---------------------------------------------------------------
   xbx% inc = 1
   xbx% ni  = grid%e_we - 1
   xbx% nj  = grid%e_sn - 1
   xbx%nk   = grid%e_vert - 1
!---------------------------------------------------------------
   xbx% lenwrk    = xbx%ni
   xbx% max_wavenumber = xbx%ni/2-1
   xbx% lenr           = xbx%inc * (xbx%ni - 1 ) + 1
   xbx% c_cv_size =(xbx%max_wavenumber+1)*(xbx%max_wavenumber+2)/2 
   xbx% alp_size = (xbx%nj+1)*(xbx%max_wavenumber+1)*(xbx%max_wavenumber+2)/4
!---------------------------------------------------------------

   CALL DA_Transfer_KMAToXb( xbx, grid, &
 
#include <em_dummy_args.inc>
                             )
END SUBROUTINE DA_Setup_FirstGuess_KMA

