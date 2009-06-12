SUBROUTINE  save_cloudResults(mype,nlat,nlon,nsig,q,qr,qs,qg,qc,qi,tcld,pbk)
! 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  save_cloudResults write cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-27
!
! ABSTRACT: 
!  This subroutine write cloud analysis results to analysis variables to save the results
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     pbk         - 3D background pressure  (hPa)
!     q           - 3D moisture 
!     qr          - 3D rain mixing ratio (kg/kg)
!     qs          - 3D snow mixing ratio (kg/kg)
!     qg          - 3D hail mixing ratio (kg/kg)
!     qc          - 3D cloud water mixing ratio (kg/kg)
!     qi          - 3D cloud ice mixing ratio (kg/kg)
!     tcld        - 3D in-cloud temperature (K)
!
!   output argument list:
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

  use kinds, only: r_single,i_kind
  use guess_grids, only: ges_tv,ges_q
  use guess_grids, only: ges_qc,ges_qi,ges_qr,ges_qs,ges_qh
  use constants, only: rd_over_cp, h1000

  implicit none

  integer (i_kind),intent(in) :: nlat,nlon,nsig
  integer (i_kind),intent(in) :: mype

! background
!
! read in from WRF
!
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! mositure
  real(r_single),intent(in) :: qr(nlon,nlat,nsig)    ! rain
  real(r_single),intent(in) :: qs(nlon,nlat,nsig)    ! snow
  real(r_single),intent(in) :: qg(nlon,nlat,nsig)    ! hail
  real(r_single),intent(in) :: qc(nlon,nlat,nsig)    ! cloud water
  real(r_single),intent(in) :: qi(nlon,nlat,nsig)    ! cloud ice
  real(r_single),intent(inout) :: tcld(nlon,nlat,nsig)    ! cloud temperature

  real(r_single),intent(in) :: pbk(nlon,nlat,nsig)    ! pressure , pa
!
!  misc.
!
  INTEGER :: i,j,k
!
!================================================================
!

  do k=1,nsig
  do j=1,nlat
  do i=1,nlon
     tcld(i,j,k)=tcld(i,j,k)*(pbk(i,j,k)/h1000)**rd_over_cp
  ENDDO
  ENDDO
  ENDDO

  do k=1,nsig
  do j=1,nlat
  do i=1,nlon
     ges_q(j,i,k,1)=q(i,j,k)
!     ges_tv(j,i,k,1)=tcld(i,j,k)*(1.0+0.61*q(i,j,k))
     ges_qr(j,i,k,1)=qr(i,j,k)
     ges_qs(j,i,k,1)=qs(i,j,k)
     ges_qh(j,i,k,1)=qg(i,j,k)
     ges_qc(j,i,k,1)=qc(i,j,k)
     ges_qi(j,i,k,1)=qi(i,j,k)
  ENDDO
  ENDDO
  ENDDO
!

END SUBROUTINE save_cloudResults
