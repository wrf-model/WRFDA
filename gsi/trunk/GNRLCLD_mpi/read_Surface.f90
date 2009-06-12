SUBROUTINE read_Surface(lunin,mxst_p,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_Surface   read in cloud observations in surface observation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine read in cloud observations in surface observation
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mxst_p      -  maximum observation number
!     NVARCLD_P   -  first dimension of OLCD
!     lunin       - unit in which data are read in
!
!   output argument list:
!
!     numsao      -  observation number
!     OI          -  observation x location
!     OJ          -  observation y location
!     OLCD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation

!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!

  use kinds, only: r_single,i_kind,r_kind
  use gridmod, only: regional_time

  implicit none

  integer(i_kind), intent(in) :: lunin

  INTEGER(i_kind),intent(in) :: mxst_p,NVARCLD_P

  real(r_single),intent(out) :: OI(mxst_p)  ! x location
  real(r_single),intent(out) :: OJ(mxst_p)  ! y location
  INTEGER(i_kind),intent(out):: OCLD(NVARCLD_P,mxst_p)  ! cloud amount, cloud height,
                                            ! visibility
  CHARACTER*10,intent(out)   :: OWX(mxst_p)      ! weather
  real(r_single),intent(out) :: Oelvtn(mxst_p)  ! elevation

!
! temp.
!
  character*12   :: adate
  character*9    :: STANAM  ! stattion name
  real(r_single) :: LAT   ! latitude
  real(r_single) :: LON   ! longitude

  real(r_single) :: VIS   ! horizontal visibility
  real(r_single) :: CLD(3)! cloud base height
  character*10   :: WX      ! weather
  character*8    :: sky(3)  ! cloud cover or amount

!
!  misc.
!
  real(r_kind),allocatable,dimension(:,:):: data_s
  logical,allocatable,dimension(:):: luse
  character(10) :: obstype
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis

  INTEGER :: numsao, nn_obs
  real ::  cldamt,awx,cldhgt
  character*3 :: msky,mwx
  INTEGER :: i,j,k,k2
  integer :: start, end

  real       spval_p
  parameter (spval_p = 99999.)


!====================================================================
!  Begin
  OWX=''
  OCLD=-99999
!
  read(lunin) obstype,isis,nreal,nchanl,ilat1s,ilon1s

  nn_obs = nreal + nchanl
  allocate(luse(numsao),data_s(nn_obs,numsao))
  read(lunin) data_s, luse 
!
! read in ruface observations:
! station name, x location, y location, longitude, latitude, elevation
! visibility, cloud amount, cloud height, weather
!
    DO i=1,numsao
!       stanam=data_s(1,i)
       OI(i) = data_s(2,i)     
       OJ(i) = data_s(3,i)
       Oelvtn(i)  = data_s(4,i)
       VIS   = data_s(5,i)
! cloud amonut and base height
       DO j=1,3
          cldamt =  data_s(5+j,i)         ! cloud amount
          cldhgt =  int(data_s(11+j,i))   ! cloud bottom height
          if(cldamt < spval_p .and. cldhgt < spval_p) then
            if(abs(cldamt-0.) < 0.0001) then
              OCLD(j,i)=0                 !msky='CLR'
              cldhgt=spval_p
            elseif(abs(cldamt-13.) < 0.0001) then
              OCLD(j,i)=1                 !msky='FEW'
            elseif(abs(cldamt-11.) < 0.0001) then 
              OCLD(j,i)=2                 !msky='SCT'
            elseif(abs(cldamt-12.) < 0.0001) then
              OCLD(j,i)=3                 !msky='BKN'
            elseif((abs(cldamt-8.) < 0.0001) .or. (abs(cldamt-9.) < 0.0001)) then
              OCLD(j,i)=4                 !   msky='OVC'   msky='VV '
            endif
            OCLD(6+j,i) = cldhgt
          else
              OCLD(j,i) = 99
              OCLD(6+j,i) = spval_p
          endif
       enddo
! weather
       DO j=1,3
          awx    =  data_s(17+j,i)        ! weather
          mwx='   '
          if(awx.ge.10..and.awx.le.12.) mwx='BR '
          if(awx.ge.110..and.awx.le.112.) mwx='BR '
          if(awx.eq.5..or.awx.eq.105.) mwx='HZ '
          if(awx.ge.40..and.awx.le.49.) mwx='FG '
          if(awx.ge.130..and.awx.le.135.) mwx='FG '
          if(awx.ge.50..and.awx.le.59.) mwx='DZ '
          if(awx.ge.150..and.awx.le.159.) mwx='DZ '
          if(awx.ge.60..and.awx.le.69.) mwx='RA '
          if(awx.ge.160..and.awx.le.169.) mwx='RA '
          if(awx.ge.70..and.awx.le.78.) mwx='SN '
          if(awx.ge.170..and.awx.le.178.) mwx='SN '
          if(awx.eq.79..or.awx.eq.179.) mwx='PE '

          if(awx.ge.80..and.awx.le.90.) mwx='SH '
          if(awx.ge.180..and.awx.le.187.) mwx='SH '
          if(awx.ge.91..and.awx.le.99.) mwx='TH '
          if(awx.ge.190..and.awx.le.196.) mwx='TH '

          if (j.eq.1) start=1
          if (j.eq.2) start=4
          if (j.eq.3) start=7
          end=start+2
          OWX(i)(start:end)=mwx
       enddo
! visiblity
       IF(VIS .gt. spval_P) then
          OCLD(13,i)=spval_P
       else
          IF(VIS > 100.0 ) then
            OCLD(13,i)=int(VIS)
          else
            OCLD(13,i)=100
            write(6,*) 'read_Surface, Warning: change visibility to 100 m !!!'
          ENDIF
       endif
    ENDDO
    close(88)

END SUBROUTINE read_Surface

