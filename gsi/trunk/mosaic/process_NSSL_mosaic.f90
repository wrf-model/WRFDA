program process_NSSL_mosaic
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
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
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
!      use constants, only: zero,one_tenth,one,deg2rad,rad2deg
!      use gridmod, only: regional,nlon,nlat,nsig,         &
!                         tll2xy,txy2ll,                   &
!                         regional_time,nhr_assimilation,  &
!                         regional_fhr,    &
!                         ylat,xlon
  use mpi
  use kinds, only: r_kind,i_kind

  implicit none
!
  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  REAL, allocatable :: ref3d(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: ref0(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: maxref(:,:)   ! composite reflectivity
  integer , allocatable :: imaxref(:,:)   ! composite reflectivity
  REAL(r_kind), allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column
  CHARACTER*180   geofile
!
!  For reflectiivty mosaic
!
  INTEGER ::    ntiles
  CHARACTER*180   workPath
  CHARACTER*180   mosaicfile

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: msclev(:)        ! level of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity

  REAL :: lonMin,latMin,lonMax,latMax,dlon,dlat
!
!  lightning staff
!
  REAL, allocatable :: lightning(:,:)    ! lightning
  REAL, allocatable :: dbz_lightning(:,:)    ! lightning in 2d dbz
!
!
!  ** misc
      
  real        ::rix  ! x-grid coordinate (grid units)
  real        ::riy  ! y-grid coordinate (grid units)
  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

  integer i,j,k,itype,iymdh,ier,jret,ifn
  integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

  integer :: NCID

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4,refl_ltng

  INTEGER(i_kind)  ::  maxlvl, tversion
  INTEGER(i_kind)  ::  numlvl,numref
  integer :: status

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

write(*,*) mype, 'deal with mosaic'
!
! set geogrid fle name
!
  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)

!  if(mype==0) then
!    write(6,*)' max,min XLAT(:,1)=',maxval(ylat(:,1)),minval(ylat(:,1))
!    write(6,*)' max,min XLAT(1,:)=',maxval(ylat(1,:)),minval(ylat(1,:))
!    write(6,*)' xlat(1,1),xlat(nlon,1)=',ylat(1,1),ylat(nlon,1)
!    write(6,*)' xlat(1,nlat),xlat(nlon,nlat)=', &
!       ylat(1,nlat),ylat(nlon,nlat)
!
!    write(6,*)' max,min XLONG(:,1)=',maxval(xlon(:,1)),minval(xlon(:,1))
!    write(6,*)' max,min XLONG(1,:)=',maxval(xlon(1,:)),minval(xlon(1,:))
!    write(6,*)' xlong(1,1),xlong(nlon,1)=',xlon(1,1),xlon(nlon,1)
!    write(6,*)' xlong(1,nlat),xlong(nlon,nlat)=', &
!       xlon(1,nlat),xlon(nlon,nlat)
!
!    DO j=1,nlat,10
!       write(*,*) j
!!       write(*,'(10f7.1)') (ylat(i,j),i=1,nlon,10)
!     ENDDO
!    DO j=1,nlat,10
!       write(*,*) j
!       write(*,'(10f7.1)') (xlon(i,j),i=1,nlon,10)
!    ENDDO
!  endif
!
! set NSSL mosaic file name
!
  mypeLocal=mype+1
  workPath='./'
  write(mosaicfile,'(a,a,I1)') trim(workPath), 'mosaic_t',mypeLocal
!
!   deal with certain tile
!
   tversion = 8
   maxlvl = 31
   write(*,*) 'process tile:',trim(mosaicfile)
   call ifexist_file(mosaicfile,STATUS)
   IF (STATUS .NE. NF_NOERR) then 
      allocate(ref3d(nlon,nlat,maxlvl))
      ref3d=-999.0
      write(*,*) trim(mosaicfile), 'does not exist!!!'
   ELSE
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
!
   if( maxlvl == mscNlev ) then
      allocate(ref3d(nlon,nlat,maxlvl))
   else
       write(*,*) 'Wrong vertical layers:', maxlvl, mscNlev
       stop 1234
   endif
   ref3d=-999.0

   DO k=1, mscNlev
!          write(*,*) mype, 'deal with level:', k,mscNlon,mscNlat
          call  GET_Mosaic_sngl_Mosaic(NCID,mscNlon,mscNlat,k,mscValue)
          DO j=1,nlat
          DO i=1,nlon
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
                  ref3d(i,j,k)=-99.0   ! clear
               else
                  ref3d(i,j,k)=-999.0  ! no observation
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
   ENDIF

   call mpi_barrier(MPI_COMM_WORLD,ierror)
!
!  collect data from all processes to root (0)
!
   if(mype==0) then
     allocate( ref0(nlon,nlat,maxlvl) )
     allocate( maxref(nlon,nlat) )
     allocate( imaxref(nlon,nlat) )
   endif
   call MPI_REDUCE(ref3d, ref0, nlon*nlat*maxlvl, MPI_REAL, MPI_MAX, 0, &
                     MPI_COMM_WORLD, ierror)
   deallocate(ref3d)

!
!
  if(mype==0) then
!
    allocate(ref3d_column(maxlvl+2,nlon*nlat))
    ref3d_column=-999.0
    numref=0
    DO j=1,nlat
    DO i=1,nlon
      numlvl=0
      DO k=1,maxlvl
        if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
      ENDDO
      if(numlvl > 0 ) then
        numref=numref+1
        ref3d_column(1,numref)=float(i)
        ref3d_column(2,numref)=float(j)
        DO k=1,maxlvl
           ref3d_column(2+k,numref)=ref0(i,j,k)
        ENDDO
      endif
    ENDDO
    ENDDO

    write(*,*) 'Dump out results', numref, 'out of', nlon*nlat
    OPEN(10,file=trim(workPath)//'RefInGSI.dat',form='unformatted')
     write(10) maxlvl,nlon,nlat,numref,1,2
     write(10) ((ref3d_column(k,i),k=1,maxlvl+2),i=1,numref)
    close(10)
  
    write(*,*) 'Start write_bufr_nsslref'
    call write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column)
  endif

  call MPI_FINALIZE(ierror)
!
end program process_NSSL_mosaic
