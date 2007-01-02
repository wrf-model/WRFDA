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

   integer           :: i, j
   integer           :: is, ie, js, je, ks, ke

!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

!---------------------------------------------------------------------------
!  [2.0] Copy header info:
!---------------------------------------------------------------------------

! rizvi set it to 1 . Actually it should be decided by KMA 
   grid%map_proj = 0
   map_projection = grid%map_proj
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1

   xb % mix = xp%ide - xp%ids + 1
   xb % mjy = xp%jde - xp%jds + 1
   xb % mkz = xp%kde - xp%kds + 1

   mix = xb % mix
   mjy = xb % mjy
   mkz = xb % mkz

   xb % ds  = 0.001 * grid%dx

   start_x = 1.0
   start_y = 1.0
   start_lat = -90.0
   start_lon = -180.0
   delt_lat = 180.0/real(grid%e_sn - grid%s_sn - 1)
   delt_lon = 360.0/real(grid%e_we - grid%s_we)

    do j = js,je
    do i = is,ie
       xlat(i,j) = start_lat + real(j-1)*delt_lat
       xlong(i,j) = start_lon + real(i-1)*delt_lon
    end do
    end do

!--Avoid assigning -90,90 value                                           
    if(xb%jts == xb%jds) then
       xlat(is:ie,j) = -89.9                               
    end if

    if(xb%jte == xb%jde) then
       xlat(is:ie,j) = 89.9                               
    end if

! fix map factor and coriolis parameter
      
   f(is:ie,js:je) = 2.0 *earth_omega*sin(pi*xlat(is:ie,js:je)/180.0)

!---------------------------------------------------------------
   xbx%inc = 1
   xbx%ni  = grid%e_we - grid%s_we
   xbx%nj  = grid%e_sn - grid%s_sn
   xbx%nk  = grid%e_vert - 1
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

