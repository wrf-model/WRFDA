subroutine read_NSSL_mosaic(nread,ndata,infile,obstype,lunout)
!
!   PRGMMR: Ming Hu          ORG: NP22        DATE: 2006-03-27
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI grid
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!     infile   - unit from which to read mosaic information file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  GUST, EJET
!
!$$$
!
!_____________________________________________________________________
!
      use kinds, only: r_kind,r_double,i_kind
      use constants, only: zero,one_tenth,one,deg2rad,rad2deg
      use gridmod, only: regional,nlon,nlat,nsig,         &
                         tll2xy,txy2ll,                   &
                         regional_time,nhr_assimilation,  &
                         regional_fhr,    &
                         region_lat,region_lon

      implicit none
!
      real(r_kind),parameter:: r360 = 360.0_r_kind

      character(10),intent(in):: infile,obstype
      integer(i_kind),intent(in):: lunout
      integer(i_kind),intent(inout):: nread,ndata
!
      character*256 output_file
 
!
!  grid
      real,allocatable:: xlon(:,:)    !
      real,allocatable:: ylat(:,:)    !
      REAL, allocatable :: ref3d(:,:,:)   ! 3D reflectivity
!
!  For reflectiivty mosaic
!
!      INTEGER ::    ntiles
!      CHARACTER*180    mosaicPath(20)
!      CHARACTER*20    mosaictime
      CHARACTER*180   mosaicfile

      INTEGER ::   mscNlon   ! number of longitude of mosaic data
      INTEGER ::   mscNlat   ! number of latitude of mosaic data
      INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
      REAL, allocatable :: msclon(:)        ! longitude of mosaic data
      REAL, allocatable :: msclat(:)        ! latitude of mosaic data
      REAL, allocatable :: msclev(:)        ! level of mosaic data
      REAL, allocatable :: mscValue(:,:)   ! reflectivity

      INTEGER :: iVlu(126,126),ii,jj

      REAL :: lonMin,latMin,lonMax,latMax,dlon,dlat
!
!
!  ** misc
      
    real(r_kind)::rix  ! x-grid coordinate (grid units)
    real(r_kind)::riy  ! y-grid coordinate (grid units)
    logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

        integer i,j,k,itype,iymdh,ier,jret,ifn,itime
        integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

        integer typ_count(20)
        integer         iret
        CHARACTER       subset*8
        integer :: im
        integer :: status
        integer :: NCID

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4

  INTEGER  ::  maxlvl, tversion
  INTEGER  ::  ibegin,iend,jbegin,jend

  integer(i_kind) nreal,nchanl
  character(20):: sis


!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
write(*,*) 'deal with mosaic'
   nread=0
   ndata=0
   ifn = 91

!      OPEN(ifn,file=trim(infile))
!      read(ifn,*) ntiles, tversion
!      if(ntiles < 20 ) then
!        DO i=1,ntiles
!          read(ifn,'(a)')  mosaicPath(i)
!          write(*,*)  mosaicPath(i)
!        ENDDO
!        read(ifn,'(a)')  mosaictime
!     else
!       write(*,*) ' Too many tiles for reflectivity Mosaic!'
!       stop
!     endif
!     close(ifn)

!
!  get GSI horizontal grid in latitude and longitude
!
     allocate(xlon(nlon,nlat))
     allocate(ylat(nlon,nlat))
     DO j=1,nlat
     DO i=1,nlon
       xlon(i,j)=region_lon(j,i)*rad2deg
       ylat(i,j)=region_lat(j,i)*rad2deg
     ENDDO
     ENDDO
!     DO j=1,nlat
!       write(*,*) j
!       write(*,'(10f7.1)') (ylat(i,j),i=1,nlon)
!     ENDDO
!     DO j=1,nlat
!       write(*,*) j
!       write(*,'(10f7.1)') (xlon(i,j),i=1,nlon)
!     ENDDO
!      write(*,*) nread,ndata,infile,obstype,lunout
!      write(*,*) regional_time
!      write(*,*) nhr_assimilation,regional_fhr
      itime=regional_time(4)*100+regional_time(5)
!
!   deal with certain tile
!
   do im=1, 1
       tversion = 8
        mosaicfile=trim(infile)
        write(*,*) 'process tile:',trim(mosaicfile)

       IF( tversion == 14 ) then
         call GET_DIM_ATT_Mosaic(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
       ELSEIF( tversion == 8 ) then
         call GET_DIM_ATT_Mosaic8(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)

       ELSE
         write(*,*) ' unknown tile version !!!'
         stop 123
       ENDIF

       allocate(msclon(mscNlon))
       allocate(msclat(mscNlat))
       allocate(msclev(mscNlev))
       allocate(mscValue(mscNlon,mscNlat))

       msclon(1)=lonMin
       DO i=1,mscNlon-1
         msclon(i+1)=msclon(i)+dlon
       ENDDO
       msclat(1)=latMin
       DO i=1,mscNlat-1
         msclat(i+1)=msclat(i)+dlat
         msclat(i+1)=msclat(i)+dlat
       ENDDO
!
!  ingest mosaic file and interpolation
!
       call OPEN_Mosaic(mosaicfile, NCID)

       if(tversion == 14 ) then
         call Check_DIM_ATT_Mosaic(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
       elseif(tversion == 8 ) then
         call Check_DIM_ATT_Mosaic8(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
       endif
       write(*,*) mscNlon,mscNlat,mscNlev
       write(*,*) 'Area of tile=',lonMin,latMin,lonMax,latMax,dlon,dlat
       ibegin=0
       iend=nlon
       DO i=1,nlon-1
         if(lonMin < xlon(i+1,nlat/2) .and. lonMin >= xlon(i,nlat/2) ) ibegin=i
         if(lonMax < xlon(i+1,nlat/2) .and. lonMax >= xlon(i,nlat/2) ) iend=i
       enddo
       ibegin=min(max(ibegin,1),nlon)
       iend  =max(min(iend,nlon),1)
       jbegin=0
       jend=nlat
       DO j=1,nlat-1
         if(latMin < ylat(ibegin,j+1) .and. latMin >= ylat(ibegin,j) ) jbegin=j
         if(latMax < ylat(iend,j+1) .and. latMax >= ylat(iend,j) ) jend=j
       ENDDO
       jbegin=min(max(jbegin,1),nlat)
       jend  =max(min(jend,nlat),1)
       DO i=1,nlon-1
         if(lonMin < xlon(i+1,jbegin) .and. lonMin >= xlon(i,jbegin) ) ibegin=i
         if(lonMax < xlon(i+1,jend) .and. lonMax >= xlon(i,jend) ) iend=i
       enddo
       ibegin=min(max(ibegin-30,1),nlon)
       iend  =max(min(iend+30,nlon),1)
       jbegin=min(max(jbegin-30,1),nlat)
       jend  =max(min(jend+30,nlat),1)
!       write(*,*) 'Area of Mosaic tile in analysis domain:'
!       write(*,*) 'ibegin=',ibegin,' iend=',iend
!       write(*,*) 'jbegin=',jbegin,' jend=',jend
       maxlvl=mscNlev
       allocate(ref3d(ibegin:iend,jbegin:jend,maxlvl))
       ref3d=-99999.0

       DO k=1, mscNlev
          write(*,*) 'deal with level:', k,mscNlon,mscNlat
          call  GET_Mosaic_sngl_Mosaic(NCID,mscNlon,mscNlat,k,mscValue)
!          DO j=1,nlat
!          DO i=1,nlon
          DO j=jbegin,jend
          DO i=ibegin,iend
             rlat=ylat(i,j)
             rlon=xlon(i,j)

             if(tversion == 14 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(rlat-latMin)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             elseif(tversion == 8 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(latMax-rlat)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp 
             else
               write(*,*) ' Unknown Mosaic format !!'
               stop 123
             endif
             if( ip >= 1 .and. ip < mscNlon ) then
             if( jp >= 1 .and. jp < mscNlat ) then
! inside mosaic domain
               w1=(1.0-dip)*(1.0-djp)
               w2=dip*(1.0-djp)
               w3=dip*djp
               w4=(1.0-dip)*djp
               ref1=mscValue(ip,jp)
               ref2=mscValue(ip+1,jp)
               ref3=mscValue(ip+1,jp+1)
               ref4=mscValue(ip,jp+1)
               if(ref1 > -500.0 .and. ref2 > -500.0 .and.  &
                  ref3 > -500.0 .and. ref4 > -500.0 ) then
                  ref3d(i,j,k)=(ref1*w1+ref2*w2+ref3*w3+ref4*w4)/10.0
               elseif(ref1 > -5000.0 .and. ref2 > -5000.0 .and.  &
                  ref3 > -5000.0 .and. ref4 > -5000.0 ) then
                  ref3d(i,j,k)=-99.0
               endif
             endif
             endif
          ENDDO
          ENDDO
       ENDDO  ! mscNlev

       call CLOSE_Mosaic(NCID)

       deallocate(msclon)
       deallocate(msclat)
       deallocate(msclev)
       deallocate(mscValue)

      enddo    ! ntiles

!
       ndata=nlon*nlat
       nreal=0
       write(lunout) obstype,sis,nreal,maxlvl,nlon,nlat
       write(lunout) ibegin,iend,jbegin,jend,maxlvl
       write(lunout) ref3d

if (1==2) then
      OPEN(88,file='MosaicInGSI.'//trim(mosaicfile),form='unformatted')
      write(88) ibegin,iend,jbegin,jend,maxlvl,nlon,nlat
      write(88) ref3d
      close(88)
endif

      deallocate(ref3d)

!
end subroutine read_NSSL_mosaic
!
!
