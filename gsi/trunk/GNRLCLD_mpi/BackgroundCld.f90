SUBROUTINE  BackgroundCld(mype,tbk,pbk,psbk,wbk, &
             q,xlon,xlat,hbk,zh,xland,soil_tbk,z_lcl)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BackgroundCld  background ingestion for cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine reads in background fields for cloud analysis
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!
!
!   input argument list:
!     mype         - processor ID
!
!   output argument list:
!     tbk         - 3D background potentional temperature (K)
!     pbk         - 3D background pressure  (hPa)
!     psbk        - 2D background surface pressure (hPa)
!     wbk         - 3D vertical velocity
!     q           - 3D moisture
!     xlon        - 2D longitude in each grid
!     xlat        - 2D latitude in each grid
!     hbk         - 3D height
!     zh          - terrain
!     xland       - land
!     soil_tbk    - soil tmperature
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
  use gridmod, only: pt_ll,eta1_ll,aeta1_ll
  use gridmod, only: rlats,rlons
  use gridmod, only: regional, wrf_mass_regional
  use gridmod, only: regional_time
  use constants, only: rd_over_cp, h1000
  use constants, only: rd, grav, half, deg2rad, rad2deg
  use gridmod, only: lat2,lon2,nsig,nlat_regional,nlon_regional
  use guess_grids, only: ges_qc,ges_qi,ges_qr,ges_qs,ges_qh
  use guess_grids, only: soil_temp,isli,ges_xlon,ges_xlat
  use guess_grids, only: ges_z,ges_ps,ges_tv,ges_q,nfldsig,ifilesig

  implicit none

  integer(i_kind),intent(in):: mype

! background
!
! read in from WRF
!
  real(r_single),intent(out):: tbk(lon2,lat2,nsig)   ! temperature
  real(r_single),intent(out) :: psbk(lon2,lat2)         !  surface pressure
  real(r_single),intent(out) :: zh(lon2,lat2)           ! terrain
  real(r_single),intent(out) :: q(lon2,lat2,nsig)     ! mositure

  real(r_single),intent(out) :: xlon(lon2,lat2)         ! longitude
  real(r_single),intent(out) :: xlat(lon2,lat2)         ! latitude
!  real(r_single) :: gsfc(lon2,lat2,3)       ! soil 
!
! derived fields
!
  real(r_single),intent(out) :: wbk(lon2,lat2,nsig)   ! vertical velocity
  real(r_single),intent(out) :: hbk(lon2,lat2,nsig)   ! height
  real(r_single),intent(out) :: pbk(lon2,lat2,nsig)   ! pressure  hPa
!  real(r_single) :: thv(lon2,lat2,nsig)   ! virtul potential temperature
  real(r_single),intent(out) :: xland(lon2,lat2)        ! land
  real(r_single),intent(out) :: soil_tbk(lon2,lat2)    ! soil tmperature

!  real(r_single) :: rhbk(lon2,lat2,nsig)   ! rh
  real(r_single) :: t_k(lon2,lat2,nsig)    ! temperature in C

  real(r_single),intent(out) :: z_lcl(lon2,lat2)   ! lifting condensation level
  real(r_single) :: cv_bk(lon2,lat2,nsig)   !  cloud cover

!
!  misc.
!
  INTEGER :: i,j,k

  REAL :: rdog, h, dz, rl
  REAL :: height(nsig+1)
  real(r_single) :: q_integral(lon2,lat2)   
  real(r_single) :: deltasigma, psfc_this
  
!
!================================================================
!
!
!  call read_mass_cloud(tbkp1,psbk,zh,qp1,qrp1,qsp1,qgp1,qcp1,qip1,  &
!                       xlon,xlat,gsfc)

  wbk=1.0
  do j=1,lat2
  do i=1,lon2
     zh(i,j)=ges_z(j,i,1)                     ! m
     psbk(i,j)=ges_ps(j,i,1)*10.0              ! mb
!     psbk(i,j)=exp(ges_lnps(j,i,1))*10.0      ! mb
     xland(i,j)=isli(j,i,1)   !   0=water, 1=land, 2=ice
     soil_tbk(i,j)=soil_temp(j,i,1)   !   soil temperature
     xlon(i,j)=ges_xlon(j,i,1)
     xlat(i,j)=ges_xlat(j,i,1)
  ENDDO
  ENDDO

  do k=1,nsig
  do j=1,lat2
  do i=1,lon2
     q(i,j,k)=ges_q(j,i,k,1)
     tbk(i,j,k)=ges_tv(j,i,k,1)/(1.0+0.61*q(i,j,k))   ! virtual temp to temp
  ENDDO
  ENDDO
  ENDDO

  q_integral=1
  do k=1,nsig
    deltasigma=eta1_ll(k)-eta1_ll(k+1)
    do j=1,lat2
    do i=1,lon2
       q(i,j,k) = q(i,j,k)/(1.0-q(i,j,k))
       q_integral(i,j)=q_integral(i,j)+deltasigma*q(i,j,k)
    enddo
    enddo
  enddo
  do j=1,lat2
  do i=1,lon2
     psfc_this=pt_ll+(psbk(i,j)-pt_ll)/q_integral(i,j)
     psbk(i,j)= psfc_this
  enddo
  enddo

!
!  assign CAPE as 0, this part neen more work
!
!  gsfc(:,:,3)=0.0  ! CAPE, we need but not included in wrf_inout
!  1: land use;   2: sfc soil T;    3: CAPE
!
! get land use and convert latitude and longitude back to degree
!  xland=gsfc(:,:,1)
!  soil_tbk=gsfc(:,:,2)
  xlon=xlon*rad2deg
  xlat=xlat*rad2deg
!
! get virtual potential temperature (thv)
!
!  thv=0.0
!  do k=1,nsig
!  do j=1,nlat
!  do i=1,nlon
!     rl=qr(i,j,k)+qs(i,j,k)+qg(i,j,k)+qc(i,j,k)+qi(i,j,k)
!     thv(i,j,k)=tbk(i,j,k)*(1.0+0.61*q(i,j,k)-rl)
!  ENDDO
!  ENDDO
!  !ENDDO
!!
!
! now get pressure (pbk) and height (hbk) at each grid point
!
  if(regional .and. wrf_mass_regional ) then

!    do j=1,nlat
!    do i=1,nlon
!        psbk(i,j)=psbk(i,j)*0.01
!        zh(i,j)=zh(i,j)/grav
!    end do
!    end do

    do k=1,nsig
    do j=1,lat2
    do i=1,lon2
       pbk(i,j,k)=aeta1_ll(k)*(psbk(i,j)-pt_ll)+pt_ll
    end do
    end do
    end do

!   Compute geopotential height at midpoint of each layer
    rdog = rd/grav
    do j=1,lat2
    do i=1,lon2
      k  = 1
      h  = rdog * tbk(i,j,k)
      dz = h * log(psbk(i,j)/pbk(i,j,k))
      height(k) = zh(i,j) + dz
      
      do k=2,nsig
        h  = rdog * half * (tbk(i,j,k-1)+tbk(i,j,k))
        dz = h * log(pbk(i,j,k-1)/pbk(i,j,k))
        height(k) = height(k-1) + dz
      end do
      
      do k=1,nsig
        hbk(i,j,k)=height(k) - zh(i,j)
      end do
    end do
    end do
  else
    write(6,*) ' Only wrf mass grid is done for cloud analysis '
    write(6,*) ' You are choosing grid that is not recoginzed by cloud analysis'
    call stop2(114)
  endif

  do k=1,nsig
  do j=1,lat2
  do i=1,lon2
     tbk(i,j,k)=tbk(i,j,k)*(h1000/pbk(i,j,k))**rd_over_cp
  enddo
  enddo
  enddo
  call BckgrndCC(lon2,lat2,nsig,    &
                 tbk,pbk,q,hbk,zh,  &
                 cv_bk,t_k,z_lcl)    ! out

END SUBROUTINE BackgroundCld
