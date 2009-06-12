SUBROUTINE PrecipType(nlat,nlon,nsig,t_bk,p_bk,q_bk,radar_3d,   &
                 wthr_type,cldpcp_type_3d)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  PrecipType  decide precipitation type
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate precipitation type
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     q_bk        - 3D moisture 
!     radar_3d    - 3D radar reflectivity in analysis grid (dBZ)
!     wthr_type   - weather type
!
!   output argument list:
!     cldpcp_type_3d - 3D precipitation type 
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use constants, only: rd_over_cp, h1000
  use kinds, only: r_single,i_kind,r_kind

  implicit none
  integer(i_kind),INTENT(IN):: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),INTENT(IN) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),INTENT(IN) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),INTENT(IN) :: q_bk(nlon,nlat,nsig)   ! moisture
!
! observation
!
  real(r_kind),INTENT(IN) :: radar_3d(nlon,nlat,nsig)   ! reflectivity
!
!
!  Variables for cloud analysis
!
  integer(i_kind),INTENT(out) :: cldpcp_type_3d(nlon,nlat,nsig)
  integer(i_kind),INTENT(in) :: wthr_type(nlon,nlat)
  LOGICAL :: l_mask(nlon,nlat)             ! "Potential" Precip Type
  
!
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,ilvl,nlvl
  real(r_single) :: temp_3d(nlon,nlat,nsig)   ! temperature
  real(r_single) :: rh_3d(nlon,nlat,nsig)   ! relative humidity
  real(r_single) :: p_pa_3d(nlon,nlat,nsig)   ! 
  REAL :: qvsat
  REAL :: f_qvsat
  INTEGER :: istatus
!
!====================================================================
!  Begin
!
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!

  DO j = 2,nlat-1
  DO i = 2,nlon-1
!
   DO k = 1,nsig                      ! Initialize
     temp_3d(i,j,k)=t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
     qvsat=f_qvsat(p_bk(i,j,k)*100.0,temp_3d(i,j,k))
     rh_3d(i,j,k)=100.*MIN(1.,MAX(0.,(q_bk(i,j,k)/qvsat)))
     p_pa_3d(i,j,k) = p_bk(i,j,k)*100.0
   END DO
!-----------------------------------------------------------------------

  ENDDO  ! i
  ENDDO  ! j

  l_mask = .false.

  call pcp_type_3d (nlon,nlat,nsig,temp_3d,rh_3d,p_pa_3d                  &
           ,radar_3d,l_mask,cldpcp_type_3d,istatus)


END SUBROUTINE precipType

