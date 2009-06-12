SUBROUTINE  BckgrndCC(nlon,nlat,nsig,    &
                      tbk,pbk,q,hbk,zh, &
                      cv_bk,t_k,z_lcl)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BckgrndCC  generate background cloud field
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine calculate cloud field based on background fields
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     tbk         - 3D background potentional temperature (K)
!     pbk         - 3D background pressure  (hPa)
!     q           - 3D moisture 
!     hbk         - 3D height
!     zh          - terrain
!
!   output argument list:
!     cv_bk       - 3D background cloud cover
!     t_k         - 3D temperature in K
!     z_lcl       - lifting condensation level
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
  use constants, only: rd_over_cp, h1000

  implicit none

  integer(i_kind) :: nlon,nlat,nsig
! background
!
! read in from WRF
!
  real(r_single),intent(in) :: tbk(nlon,nlat,nsig)   ! potential temperature
  real(r_single),intent(in) :: zh(nlon,nlat)           ! terrain
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! mositure
  real(r_single),intent(in) :: hbk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: pbk(nlon,nlat,nsig)   ! pressure

  real(r_single) :: rhbk(nlon,nlat,nsig)   ! rh
  real(r_single),intent(out) :: t_k(nlon,nlat,nsig)    ! temperature in k

  REAL :: z_ref_lcl
  PARAMETER(z_ref_lcl = 180.0)
!  CONSTANTS:
  REAL :: gamma_d   ! dry adiabatic lapse rate (K/m)
  PARAMETER (gamma_d = 9.8/1004.0)

  real(r_single),intent(out) :: z_lcl(nlon,nlat)   ! lifting condensation level
  real(r_single),intent(out) :: cv_bk(nlon,nlat,nsig)   !  cloud cover
!
!  misc.
!
  INTEGER :: i,j,k


  REAL :: f_qvsat
  REAL :: qvsat
  REAL :: dwpt
  REAL :: rh_to_cldcv

  REAL :: z_ref
  REAL :: arg, t_ref_c, td_ref_c
  REAL :: frac_z, t_ref_k,rh_ref

!
!================================================================
!
! get the RH

  do k=1,nsig
  do j=2,nlat-1
  do i=2,nlon-1
     t_k(i,j,k)=tbk(i,j,k)*(pbk(i,j,k)/h1000)**rd_over_cp
     qvsat=f_qvsat(pbk(i,j,k)*100.0,t_k(i,j,k))
     rhbk(i,j,k)=100.*MIN(1.,MAX(0.,(q(i,j,k)/qvsat)))
  enddo
  enddo
  enddo
!
!  Find the lifting condensation level
!
  z_lcl = -99999.0
  do j=2,nlat-1
  do i=2,nlon-1
      z_ref = z_ref_lcl + zh(i,j)
      IF (z_ref <= hbk(i,j,2) .OR. z_ref > hbk(i,j,nsig-1)) THEN
        PRINT*,'Error, ref.level is out of bounds at pt:',i,j,z_ref,hbk(i,j,2),hbk(i,j,nsig-1)
        call STOP2(114)
      END IF

      DO k = 3,nsig-1
        IF ( z_ref < hbk(i,j,k) .and. z_ref >= hbk(i,j,k-1)) THEN
          frac_z = (z_ref-hbk(i,j,k-1))/(hbk(i,j,k)-hbk(i,j,k-1))
          t_ref_k = t_k(i,j,k-1)+ frac_z*(t_k(i,j,k)-t_k(i,j,k-1))
          t_ref_c = t_ref_k - 273.15
!
          rh_ref = rhbk(i,j,k-1)+ frac_z*(rhbk(i,j,k)-rhbk(i,j,k-1))
          td_ref_c = dwpt(t_ref_c,rh_ref)
        END IF
      END DO  ! k = 2,nz-1
!
      z_lcl(i,j) = z_ref + (t_ref_c - td_ref_c)/gamma_d
      z_lcl(i,j) = min(hbk(i,j,nsig-1),max(z_lcl(i,j),hbk(i,j,2)))
  enddo
  enddo
!
!  get background cloud cover
!
  cv_bk=0.0
  do k=1,nsig
  do j=2,nlat-1
  do i=2,nlon-1
    IF (hbk(i,j,k) >= z_lcl(i,j)) THEN
       arg = hbk(i,j,k) - zh(i,j)
       cv_bk(i,j,k) = rh_to_cldcv(rhbk(i,j,k)*0.01,arg)
    ENDIF
  enddo
  enddo
  enddo
!

END SUBROUTINE BckgrndCC
