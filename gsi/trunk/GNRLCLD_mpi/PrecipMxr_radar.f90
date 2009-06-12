SUBROUTINE PrecipMxR_radar(mype,nlat,nlon,nsig,t_bk,p_bk,ref_mos_3d,cldpcp_type_3d, &
                 qr_cld,qs_cld,qh_cld,cldqropt)  
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  PrecipMxR_radar  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This is dirver to call subroutines that calculate liquid water content based on
!         radar reflectivity and precipitation type
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
!
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     ref_mos_3d  - 3D reflectiviy in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!     cldqropt    -  scheme used to retrieve precipitations
!                        1=Kessler 2=Lin 3=Thompson
!
!   output argument list:
!     qr_cld      - rain mixing ration (g/kg)
!     qs_cld      - snow mixing ration (g/kg)
!     qh_cld      - hail mixing ration (g/kg)
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
  integer(i_kind),intent(in):: nlat,nlon,nsig
  integer(i_kind),intent(in):: mype
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! height
!
  real(r_kind),intent(in)  :: ref_mos_3d(nlon,nlat,nsig)
!
!  Variables for cloud analysis
!
  integer(i_kind),intent(in) :: cldpcp_type_3d(nlon,nlat,nsig)
!
! hydrometeors
!
  REAL(r_single),intent(out) :: qr_cld(nlon,nlat,nsig)  ! rain
  REAL(r_single),intent(out) :: qs_cld(nlon,nlat,nsig)  ! snow
  REAL(r_single),intent(out) :: qh_cld(nlon,nlat,nsig)  ! hail

!-----------------------------------------------------------
!
! temp.
!

  REAL :: t_3d(nlon,nlat,nsig)  
  REAL :: p_3d(nlon,nlat,nsig)  

  INTEGER(i_kind) :: cldqropt
  INTEGER :: istatus_pcp
  INTEGER :: i,j,k
  INTEGER :: imax,jmax,kmax
  
  REAL ::  maxqr, maxqs, maxqh
!
!====================================================================
!  Begin
!
!  cldqropt = 2

  DO j = 2,nlat-1
  DO i = 2,nlon-1
    DO k = 1,nsig            
      t_3d(i,j,k) = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
      p_3d(i,j,k) = p_bk(i,j,k)*100.0
    END DO
  END DO
  END DO

!-----------------------------------------------------------------------
!
!  Calculate 3D Precipitation mixing ratio in g/kg.
!  Note that qr_cld, qs_cld, and qh_cld are diagnosed
!  qr, qs and qh in g/kg, respectively.
!
!-----------------------------------------------------------------------
!
    IF (cldqropt == 1) THEN
!
! Kessler's scheme
!
if(mype==0) then
      WRITE(6,'(a)') 'PrecipMxR_radar: Computing Precip mixing ratio.'
      WRITE(6,'(a)')                                                    &
          ' Using Kessler radar reflectivity equations...'
endif
      CALL pcp_mxr (nlon,nlat,nsig,t_3d,p_3d,ref_mos_3d,                &
                    cldpcp_type_3d,                                     &
                    qr_cld,qs_cld,qh_cld,                               &
                    istatus_pcp)

    ELSE IF (cldqropt == 2) THEN
!
! Ferrier's scheme
!
if(mype==0) then
      WRITE(6,'(a)') 'PrecipMxR_radar: Computing Precip mixing ratio.'
      WRITE(6,'(a)')                                                    &
          ' Using Ferrier radar reflectivity equations...'
endif
      CALL pcp_mxr_ferrier (nlon,nlat,nsig,t_3d,p_3d,ref_mos_3d,        &
                            cldpcp_type_3d,                             &
                            qr_cld,qs_cld,qh_cld,                       &
                            istatus_pcp)

    ELSE IF (cldqropt == 3) THEN
!
! Thompson's  scheme
!
if(mype==0) then
      WRITE(6,'(a)') ' PrecipMxR_radar: Computing Precip mixing ratio.'
      WRITE(6,'(a)')                                                    &
          ' Using Thompson RUC radar reflectivity equations...'
endif
      call pcp_mxr_thompsonRUC(qr_cld,qs_cld,qh_cld,       &
                               p_3d,t_3d,                  &
                               ref_mos_3d,nlon,nlat,nsig,cldpcp_type_3d)

    END IF   !cldqropt=1 or 2  or 3

  maxqr=-9999.0
  maxqs=-9999.0
  maxqh=-9999.0
  DO j = 2,nlat-1
  DO i = 2,nlon-1
    DO k = 1,nsig
      if(qr_cld(i,j,k) > maxqr ) maxqr = qr_cld(i,j,k)
      if(qs_cld(i,j,k) > maxqs ) maxqs = qs_cld(i,j,k)
      if(qh_cld(i,j,k) > maxqh ) maxqh = qh_cld(i,j,k)
    END DO
  END DO
  END DO
!  write(6,*) ' Maximum precipitation mixing ratio at process:', mype
!  write(6,*) 'rain=',maxqr,'snow=',maxqs,'hail=',maxqh

END SUBROUTINE PrecipMxR_radar

