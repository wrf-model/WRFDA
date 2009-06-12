SUBROUTINE read_NESDIS(lunin,numsao,nlon,nlat,sat_ctp,sat_tem,w_frac,nlev_cld,mype)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NESDIS     read in NESDIS cloud products and map them into analysis grid
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine read in NESDIS cloud products and map them into analysis grid
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     lunin       - unit in which data are read in
!     numsao      - number of observation
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nlev_cld    - levels
!     mype        - processor ID
!
!   output argument list:
!     sat_ctp     - GOES cloud top pressure in analysis grid
!     sat_tem     - GOES cloud top temperature in analysis grid
!     w_frac      - GOES cloud coverage in analysis grid
!

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

  use kinds, only: r_single,i_kind,r_kind
  use gridmod, only: regional_time
  use gridmod, only: istart,jstart    ! jlon1,ilat1

  implicit none

  integer(i_kind),intent(in):: lunin
  integer(i_kind),intent(in):: mype
  INTEGER(i_kind),intent(in) :: nlon,nlat
  
  real(r_single),intent(out) :: sat_ctp(nlon,nlat)         ! cloud top pressure
  real(r_single),intent(out) :: sat_tem(nlon,nlat)         ! cloud top temperature
  real(r_single),intent(out) :: w_frac(nlon,nlat)          ! cloud fraction
  integer(i_kind),intent(in) :: nlev_cld(nlon,nlat)       ! levels
!
  INTEGER,intent(in) :: numsao
  INTEGER :: nn_obs
  real(r_kind),allocatable,dimension(:,:):: data_s
  logical,allocatable,dimension(:):: luse
!
! misc.
!
  character(10) :: obstype
  integer :: mm1
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis

  INTEGER :: nlon_ctp, nlat_ctp
  INTEGER :: i, j, itmp, jtmp
  INTEGER :: ib, jb
  character*12 :: adate
!        
! ===============================================================
!

  mm1=mype+1

  read(lunin) obstype,isis,nreal,nchanl,ilat1s,ilon1s
  nn_obs = nreal + nchanl
  allocate(luse(numsao),data_s(nn_obs,numsao))
  read(lunin) data_s, luse 
!
  ib=jstart(mm1)   ! begin i point of this domain
  jb=istart(mm1)   ! begin j point of this domain
  call mape_ctp (ib,jb,nlon,nlat,nn_obs,numsao,data_s,sat_ctp,sat_tem,w_frac)
!  read(lunin)  ((sat_ctp(i,j),i=2,ilon1s+1),j=2,ilat1s+1) ,  &
!               ((sat_tem(i,j),i=2,ilon1s+1),j=2,ilat1s+1) ,  &
!               ((w_frac(i,j),i=2,ilon1s+1),j=2,ilat1s+1) 
!!
!  filling boundarys
!
  DO i=2,nlon-1
    sat_ctp(i,1)=sat_ctp(i,2)
    sat_tem(i,1)=sat_tem(i,2)
    w_frac(i,1)=w_frac(i,2)
    sat_ctp(i,nlat)=sat_ctp(i,nlat-1)
    sat_tem(i,nlat)=sat_tem(i,nlat-1)
    w_frac(i,nlat)=w_frac(i,nlat-1)
  enddo
  DO j=2,nlat-1
    sat_ctp(1,j)=sat_ctp(2,j)
    sat_tem(1,j)=sat_tem(2,j)
    w_frac(1,j)=w_frac(2,j)
    sat_ctp(nlon,j)=sat_ctp(nlon-1,j)
    sat_tem(nlon,j)=sat_tem(nlon-1,j)
    w_frac(nlon,j)=w_frac(nlon-1,j)
  enddo
    sat_ctp(1,1)=sat_ctp(2,2)
    sat_tem(1,1)=sat_tem(2,2)
    w_frac(1,1)=w_frac(2,2)
    sat_ctp(1,nlat)=sat_ctp(2,nlat-1)
    sat_tem(1,nlat)=sat_tem(2,nlat-1)
    w_frac(1,nlat)=w_frac(2,nlat-1)
    sat_ctp(nlon,1)=sat_ctp(nlon-1,2)
    sat_tem(nlon,1)=sat_tem(nlon-1,2)
    w_frac(nlon,1)=w_frac(nlon-1,2)
    sat_ctp(nlon,nlat)=sat_ctp(nlon-1,nlat-1)
    sat_tem(nlon,nlat)=sat_tem(nlon-1,nlat-1)
    w_frac(nlon,nlat)=w_frac(nlon-1,nlat-1)

END SUBROUTINE read_NESDIS
